;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/core/runtime/parse-loop.lisp
;;;
;;; The wait-and-see main loop from Marcus's parse.l 103-172, plus the
;;; rule-indexing machinery (rule-index, testrules) that the loop
;;; consults.
;;;
;;; What this file covers:
;;;
;;;   Rule indexing (parse.l 400-443)
;;;     *rule-index*, rule-index, rem-index, fetchrules, testrules
;;;
;;;   Main loop (parse.l 103-172)
;;;     parse-loop, nextrule label, runrule label,
;;;     *deriv* derivation trace
;;;
;;; Deviations from Marcus's source (worth knowing):
;;;
;;; - Rule storage. Marcus indexes rules by feature so FETCHRULES can
;;;   prune the candidate set via the buffer node's feature list before
;;;   any pattern runs. We now do this too: *RULE-INDEX* is keyed by
;;;   (INDEXF TYPE PACKET) and FETCHRULES looks up only the buckets
;;;   whose INDEXF is among the node's features (plus the catch-all
;;;   NOINDEXF). Marcus keeps the buckets on each feature symbol's
;;;   plist; we keep them in one reboundable hash-table (see the
;;;   *RULE-INDEX* docstring). The glang-cl emission contract is
;;;   unchanged -- it already passes INDEXF to RULE-INDEX.
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
;;; Rule indexing (parse.l 400-443)
;;; ===========================================================

(defparameter *rule-index* (make-hash-table :test #'equal)
  "Feature-indexed rule store. The key is a list (INDEXF TYPE PACKET);
   the value is the priority-sorted list of ITEMs -- each ITEM being
   (PRIORITY PAT-FN RULE-NAME ACT-FN) -- registered under that
   index/type/packet. TYPE is NORMAL, AS, or NR. A rule is fetched
   only when its INDEXF appears among the buffer node's features (or
   is the catch-all NOINDEXF), so FETCHRULES prunes the candidate set
   by feature before any pattern is evaluated. Within a bucket, lower
   PRIORITY numbers come first (Marcus's convention).

   Marcus keeps these lists on the INDEXF symbol's plist, indexed by
   TYPE then PACKET (parse.l 400-431). We keep them in one reboundable
   special instead: tests can isolate a rule set with a fresh table,
   and the global feature symbols' plists stay clean. Same spirit as
   the gensym `daughters' plist and the :act-fn stash.")

(defun reset-rule-table ()
  "Forget every rule. Useful between grammar reloads."
  (clrhash *rule-index*))

(defun priority-insert (item items)
  "Insert ITEM into ITEMS keeping ascending priority order. Equal
   priorities land before existing ones, matching Marcus's `le'
   (<=) insertion at parse.l 421-430."
  (cond ((null items) (list item))
        ((<= (first item) (first (first items)))
         (cons item items))
        (t (cons (first items)
                 (priority-insert item (rest items))))))

(defun rule-index (type packets indexf item)
  "Register a rule. ITEM is (PRIORITY PAT-FN RULE-NAME ACT-FN). The
   rule becomes a candidate when TYPE matches, one of PACKETS is in
   the relevant active set, and INDEXF is among the tested node's
   features (INDEXF = NOINDEXF means `always'). Mirrors parse.l 400."
  (let ((name (third item)))
    ;; Re-registering a rule (grammar reload) drops the stale copies.
    (when (get name :indexinfo) (rem-index name))
    (setf (get name :indexinfo) (list indexf type packets item))
    ;; Stash the act-fn so ACT-OF-RULE can find it by name when the
    ;; loop chases *nextrule*, regardless of the rule's home package.
    (setf (get name :act-fn) (fourth item))
    (dolist (packet packets)
      (let ((key (list indexf type packet)))
        (setf (gethash key *rule-index*)
              (priority-insert item (gethash key *rule-index*)))))))

(defun rem-index (name)
  "Remove NAME from every (INDEXF TYPE PACKET) bucket it was indexed
   under. No-op if the rule was never indexed. Mirrors parse.l 432."
  (let ((info (get name :indexinfo)))
    (when info
      (destructuring-bind (indexf type packets item) info
        (dolist (packet packets)
          (let ((key (list indexf type packet)))
            (setf (gethash key *rule-index*)
                  (delete item (gethash key *rule-index*) :test #'eq)))))
      (remprop name :indexinfo)
      (remprop name :act-fn))))


;;; ===========================================================
;;; Rule selection (parse.l 188-244)
;;; ===========================================================

(defun rules-packets (type)
  "The active packets to search for rules of TYPE. Marcus additionally
   restricts AS rules to (cpool npool) and NR rules to (cpool) at
   parse.l 237-240; we don't, because (a) those packet names live in
   the grammar's package, not the runtime's, and (b) AS/NR rules are
   only ever registered in those packets, so filtering by active
   packet already yields the same set. Revisit when NR dispatch from
   set* lands."
  (declare (ignore type))
  *activepackets*)

(defun fetchrules (type bpnt)
  "Collect the priority-sorted rule buckets that could fire at buffer
   position BPNT: for each feature of the node there -- plus the
   catch-all NOINDEXF -- that indexes rules of TYPE, the bucket for
   each relevant active packet. Mirrors parse.l 227-244."
  (let ((features (cons 'noindexf
                        (let ((node (aref *buffer* bpnt)))
                          (and node (fe node)))))
        (packets  (rules-packets type))
        (buckets  nil))
    (dolist (f features buckets)
      (dolist (p packets)
        (let ((bucket (gethash (list f type p) *rule-index*)))
          (when bucket (push bucket buckets)))))))

(defun testrules (type bpnt)
  "Find the highest-priority rule of TYPE whose pattern matches at
   buffer position BPNT, considering only rules indexed under a
   feature the node there actually has (plus NOINDEXF). On success set
   *activerule* to (NAME ACT-FN) and return T; otherwise NIL.

   Mirrors parse.l 188-225 (testrules + testrules1/testrules2). Each
   bucket FETCHRULES returns is already priority-sorted, so merging
   them is a stable sort by priority: equal-priority rules keep their
   bucket order, which (with NOINDEXF and the feature buckets pushed
   in order) matches Marcus's earliest-bucket-wins tie-break. As
   Marcus notes, while testing a NORMAL rule's pattern an AS/NR rule
   may set *activerule* as a side effect; if so we stop and let the
   loop run it.

   Patterns read the buffer through the bound registers |1ST|/|2ND|/
   |3RD|, which the caller (the loop, via set*) sets up."
  (let ((candidates
          (stable-sort (apply #'append
                              (mapcar #'copy-list (fetchrules type bpnt)))
                       #'< :key #'first)))
    (dolist (rule candidates nil)
      (cond ((funcall (second rule))
             (setq *activerule* (list (third rule) (fourth rule)))
             (return t))
            ;; A NORMAL pattern test that fired an AS/NR rule already
            ;; set *activerule* -- short-circuit so the loop runs it.
            ((and (eq type 'normal) *activerule*)
             (return t))))))


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
