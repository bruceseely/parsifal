;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/core/runtime/parse-loop.lisp
;;;
;;; The wait-and-see main loop from Marcus's parse.l 103-172, plus the
;;; rule-indexing machinery (rule-index, testrules) that the loop
;;; consults.
;;;
;;; What this file covers:
;;;
;;;   Rule indexing (parse.l 400-431)
;;;     *rule-table*, rule-index, rem-index, testrules
;;;
;;;   Main loop (parse.l 103-172)
;;;     parse-loop, nextrule label, runrule label,
;;;     *deriv* derivation trace
;;;
;;; Deviations from Marcus's source (worth knowing):
;;;
;;; - Rule storage. Marcus indexes rules by feature using nested cons
;;;   cells (a "type-plist" living in the cdr of `(ncons nil)') so that
;;;   `(fetchrules 'normal bpnt)' can do quick lookups via the buffer
;;;   head's feature list. That's a perf optimisation that pays off
;;;   when there are 100s of rules per packet. For the MVP we store
;;;   rules in *RULE-TABLE* -- a CL hash-table keyed by packet -- and
;;;   TESTRULES walks all rules of each active packet linearly,
;;;   sorting by priority. Same observable behaviour, slower for big
;;;   grammars. The fetchrules feature-indexing can be reintroduced
;;;   later without touching the loop or rule emissions.
;;;
;;; - AS/NR rules. The loop's NEXTRULE label calls `(set* 0)' first --
;;;   that function both advances the buffer AND tests AS rules (when
;;;   a new node enters the buffer) and NR rules (when an attached
;;;   node sits at the current position). We haven't ported `set*'
;;;   yet, so this version skips AS/NR firing during advance. Rules
;;;   compiled by glang-cl are flagged 'NORMAL by default; until we
;;;   port `set*' the AS/NR branches in TESTRULES will simply find
;;;   no candidates.
;;;
;;; - Input. The real `parse' calls `(sentin)' to read a sentence
;;;   into *wstring*, and `(nextword)' to pull words into the buffer.
;;;   Both live in com.l (the REPL); not yet ported. PARSE-LOOP
;;;   expects the buffer + initial rule to be set up by the caller.
;;;
;;; - Trace / break / display. Calls like `cursorpos', `drain',
;;;   `display-trace', `break beforerun', and the `say' chatter are
;;;   no-ops here. They affect Marcus's TTY parser experience, not
;;;   the parser's correctness.

(in-package :parsifal)


;;; ===========================================================
;;; Rule indexing (parse.l 400-431)
;;; ===========================================================

(defparameter *rule-table* (make-hash-table :test #'eq)
  "Packet symbol -> list of rule entries.
   Each entry is (PRIORITY PAT-FN RULE-NAME ACT-FN TYPE), where TYPE
   is one of NORMAL, AS, NR. Entries inside a packet's list are sorted
   by priority ascending (lower number = higher priority -- Marcus's
   convention).")

(defun reset-rule-table ()
  "Forget every rule. Useful between grammar reloads."
  (clrhash *rule-table*))

(defun priority-insert (entry rules)
  "Insert ENTRY into RULES, keeping ascending priority order."
  (cond ((null rules) (list entry))
        ((<= (first entry) (first (first rules)))
         (cons entry rules))
        (t (cons (first rules)
                 (priority-insert entry (rest rules))))))

(defun rule-index (type packets indexf item)
  "Register a rule. ITEM is (PRIORITY PAT-FN RULE-NAME ACT-FN). The
   rule will be found by TESTRULES whenever its TYPE matches and any
   packet in PACKETS is active. INDEXF is the feature-indexing key
   Marcus uses to speed up rule lookup; we ignore it for now (see
   header)."
  (declare (ignore indexf))
  (let ((rule-name (third item))
        (act-fn    (fourth item))
        (entry     (append item (list type))))
    ;; If a rule with this name was previously indexed, drop the
    ;; stale entries first so reloading a grammar doesn't double up.
    (rem-index rule-name)
    (dolist (pkt packets)
      (setf (gethash pkt *rule-table*)
            (priority-insert entry (gethash pkt *rule-table*))))
    ;; Cache the act-fn under the rule-name's plist. The loop reads it
    ;; via ACT-OF-RULE when chasing *nextrule*, so a follow-up rule
    ;; doesn't have to live in any particular package -- only the
    ;; rule-name symbol matters.
    (setf (get rule-name :act-fn) act-fn)
    ;; Remember where this rule lives, so REM-INDEX can find it.
    (setf (get rule-name :indexinfo)
          (list type packets item))))

(defun rem-index (rule-name)
  "Remove RULE-NAME from every packet it was indexed under. No-op if
   the rule was never indexed."
  (let ((info (get rule-name :indexinfo)))
    (when info
      (let ((packets (second info)))
        (dolist (pkt packets)
          (setf (gethash pkt *rule-table*)
                (remove rule-name (gethash pkt *rule-table*)
                        :key #'third))))
      (remprop rule-name :indexinfo)
      (remprop rule-name :act-fn))))


;;; ===========================================================
;;; Rule selection (parse.l 188-225)
;;; ===========================================================

(defun testrules (type bpnt)
  "Find the highest-priority rule of TYPE in any active packet
   whose pattern matches. On success, set *activerule* to
   (NAME ACT-FN) and return T. On failure, return NIL.

   BPNT is the buffer position the rule is being tested against;
   Marcus's full implementation uses it to pick the indexing
   feature, but our simpler hash-keyed table doesn't, so it is
   ignored here. Patterns access the buffer via the bound buffer
   registers (*1ST*, *2ND*, *3RD*) which the caller is responsible
   for setting up."
  (declare (ignore bpnt))
  (let ((candidates nil))
    (dolist (pkt *activepackets*)
      (dolist (rule (gethash pkt *rule-table*))
        (when (eq (fifth rule) type)
          (push rule candidates))))
    ;; Stable sort by priority so equal-priority rules retain
    ;; their relative order across packets.
    (setf candidates
          (stable-sort candidates #'< :key #'first))
    (dolist (rule candidates nil)
      (when (funcall (second rule))
        (setq *activerule* (list (third rule) (fourth rule)))
        (return t)))))


;;; ===========================================================
;;; Buffer advance (parse.l 253-300)
;;; ===========================================================
;;;
;;; SET* is the heart of "wait-and-see": it pulls new words into the
;;; buffer if needed, and (in Marcus's full version) tests AS rules
;;; on newly-entered nodes and NR rules on already-attached nodes
;;; before falling through. Our MVP version doesn't fire AS/NR yet;
;;; it pulls words and sets up feature vectors only.

(defun clear-buffer-position (index)
  "Reset INDEX's buffer register and feature vector so stale bits
   from a previously-evicted node don't survive past a position
   that SET* couldn't fill. Marcus's `parse' clears all three at
   startup; we run the equivalent per-position cleanup whenever
   NEXTWORD runs dry."
  (multiple-value-bind (feat fvec)
      (cond ((= index 0) (values *1stfeat* *1stfvec*))
            ((= index 1) (values *2ndfeat* *2ndfvec*))
            ((= index 2) (values *3rdfeat* *3rdfvec*)))
    (dolist (i feat) (setf (aref fvec i) 0)))
  (cond ((= index 0) (setq *1stfeat* nil |1ST| nil))
        ((= index 1) (setq *2ndfeat* nil |2ND| nil))
        ((= index 2) (setq *3rdfeat* nil |3RD| nil))))


(defun as-check (node abs-index)
  "After SET* sets up a fresh or unattached node, check whether the
   node's type triggers an attention-shift rule. Returns T if no AS
   rule fired (loop should consult NORMAL rules), NIL if one did
   (loop should run the freshly-set *activerule*).

   Mirrors the AS-CHECK / AS-RULE-SETUP labels in parse.l 299. The
   bufpntr-stack push lets the rule advance attention to ABS-INDEX
   non-destructively -- BUFRESTORE pops back later."
  (cond ((not (is-any-of node *as-types*)) t)
        ((not (testrules 'as abs-index))   t)
        (t
         (setq |1ST| node |2ND| nil |3RD| nil)
         (push *bufpntr* *bufpntrstak*)
         (setq *bufpntr* abs-index)
         nil)))

(defun set* (index)
  "Make buffer position (*bufpntr* + INDEX) the current focus,
   pulling words from *wstring* if the buffer is empty there and
   removing any already-attached node. Returns T when the buffer is
   set up (loop should consult TESTRULES), NIL when an AS rule
   already fired (loop should run *activerule* directly).

   Mirrors parse.l line 253. We still skip Marcus's NR-rule pre-
   check at the top of the function -- that branch fires when the
   *previous* buffer position has an attached NR-type node, and
   needs the bit-2 'NR-checked' flag bookkeeping we haven't ported
   yet."
  (let ((abs-index (+ index *bufpntr*)))
    (loop
      (let ((node (aref *buffer* abs-index)))
        (cond
          ;; Empty position -- pull the next word.
          ((null node)
           (let ((new (nextword)))
             (cond
               ((null new)
                ;; Input exhausted; explicitly clear this position's
                ;; fvec / register so a previous occupant's bits
                ;; don't survive into the next pattern test.
                (clear-buffer-position index)
                (return t))
               (t
                (insert-index-pos abs-index new)
                (setup* new index)
                (return (as-check new abs-index))))))
          ;; Not attached (flag bit 1 = 0) -- set up and check AS.
          ((zerop (logand 1 (flags node)))
           (setup* node index)
           (return (as-check node abs-index)))
          ;; Attached -- evict it and retry this position.
          (t
           (remove-index-pos abs-index)))))))


;;; ===========================================================
;;; Buffer garbage collection (parse.l 175-185)
;;; ===========================================================

(defun buffer-gc ()
  "Compact the buffer at the start of each NEXTRULE pass: walk forward
   from *bufpntr* dropping nodes that are already attached to the tree
   (flag bit 1 set) -- once attached, a node no longer needs a buffer
   slot. Stop and return immediately on an attached node that is an
   unchecked NR-type (flag bit 2 clear): it may still trigger a
   node-raising rule, so it must stay in place. Unattached nodes are
   left alone. Mirrors parse.l line 175.

   Removing a node shifts the rest down, so we revisit the same index
   after a removal (the (1- i) cancels the loop's (1+ i))."
  (do ((i *bufpntr* (1+ i))
       (node))
      ((> i *bufmax*) nil)
    (cond
      ;; Unattached node (bit 1 clear) -- still pending; leave it.
      ((zerop (logand 1 (flags (setq node (aref *buffer* i))))))
      ;; Attached but unchecked NR-type node -- stop; it may fire an
      ;; NR rule and must keep its slot.
      ((and (zerop (logand 2 (flags node)))
            (member (getr 'type node) *nr-types* :test #'eq))
       (return nil))
      ;; Attached, nothing left to do with it -- drop it and re-examine
      ;; this position (now occupied by the node that shifted down).
      ((remove-index-pos i) (setq i (1- i))))))


;;; ===========================================================
;;; Main loop (parse.l 103-172)
;;; ===========================================================
;;;
;;; The loop's structure: alternating between NEXTRULE (pick a rule
;;; to fire) and RUNRULE (fire it, check for follow-up). An action
;;; can set *nextrule* to chain into another rule, or *parsecomplete*
;;; to declare success. Otherwise the loop falls back to NEXTRULE.

(defun act-of-rule (rule-name)
  "Look up the action function (or symbol) for RULE-NAME. RULE-INDEX
   stashed it on the symbol's plist when the rule was registered."
  (or (get rule-name :act-fn)
      (error "no action registered for rule ~a" rule-name)))

(defun parse-loop ()
  "Run the wait-and-see loop until either *parsecomplete* becomes T
   (success, return T) or no rule fires (deadlock, return NIL).

   This expects the caller to have set up:
    * *activepackets*, *activerule* (the initial rule),
    * the buffer (via *buffer*, *bufpntr*, *bufmax*) including
      the nodes the patterns will match against, and
    * *deriv* (typically NIL).

   Mirrors parse.l line 103's PROG / GO structure."
  (prog ()
   runrule
     ;; Fire the current rule.
     (push (car *activerule*) *deriv*)
     (funcall (cadr *activerule*))
     ;; Did the action chain to another rule? Or complete the parse?
     (cond (*nextrule*
            (setq *activerule*
                  (list *nextrule* (act-of-rule *nextrule*)))
            (setq *nextrule* nil)
            (go runrule))
           (*parsecomplete*
            (return t)))
   nextrule
     ;; Compact the buffer (drop nodes already attached to the tree)
     ;; before picking the next rule. parse.l line 142.
     (when *buffer-gc* (buffer-gc))
     ;; Pick the next rule via pattern matching.
     (setq *activerule* nil
           |1ST|        nil
           |2ND|        nil
           |3RD|        nil)
     ;; Advance the buffer (pull words, fire AS/NR rules in future).
     ;; SET* returning NIL would mean an AS/NR rule was fired and
     ;; *activerule* was already set -- we'd go straight to runrule.
     ;; In the MVP SET* always returns T.
     (unless (set* 0) (go runrule))
     (cond ((testrules 'normal *bufpntr*) (go runrule))
           (t (warn "No rule applies.")
              (return nil)))))
