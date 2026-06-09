;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/core/runtime/case-frame.lisp
;;;
;;; The case-frame mechanism from case.l (Marcus's Appendix E), ported
;;; in increments. This first increment is the data type and its access
;;; / open-close caching -- the foundation the scoring, hypothesis
;;; generation, and slot-filling build on.
;;;
;;; A case-frame is a gensym (made by MAKESYM) whose plist holds:
;;;   case-frame   NORMAL or MOD (the frame type)
;;;   assoc-node   the node this frame is for
;;;   pred         the root word heading the frame
;;;   cases        the filled <slot>s
;;;   hypo-slots   the <slotframe>s -- ways to fill the remaining slots
;;;   objs-needed  count of obligatory slots still unfilled
;;; A node points at its frame through its `caseframe' register.
;;; (See Kurt Van Lehn's data-type comment at case.l lines 4-46.)
;;;
;;; PRED, HYPO-SLOTS, and OBJS-NEEDED are cached in like-named special
;;; variables while a frame is "open"; OPENFRAME/CLOSEFRAME/OPENCHEK
;;; swap them in and out of the frame's plist.
;;;
;;; Notes:
;;; - `openframe' is both a special var (the currently open frame) and a
;;;   function (open a frame); CL lets a symbol hold both, as Marcus
;;;   relies on.
;;; - `putc' (case.l 537) guards on the node's `caseframe' but writes to
;;;   its `case-frame' prop, while `getc' reads `caseframe' -- they don't
;;;   round-trip. Reproduced as-is; `putc's only caller (alt-fillslot)
;;;   stores ambiguity data nothing yet reads, and `getc' is unused.

(in-package :parsifal)


;;; Case specials not already declared in declr.lisp.
(defvar hypo-slots '((nil nil))
  "Open frame's <slotframe>s -- the ways left to fill its slots.")
(defvar objs-needed 0
  "Open frame's count of obligatory slots still unfilled.")


;;; ===========================================================
;;; Creation and access (case.l 52-82)
;;; ===========================================================

(defun newcf (type)
  "Make a fresh case-frame of TYPE (NORMAL/MOD/...), make it the open
   frame, and return it. Mirrors case.l 52."
  (clearcf)
  (let ((cf (makesym 'casefr)))
    (setq openframe cf)
    (setf (get cf 'case-frame) type)
    cf))

(defun case-frame (node)
  "NODE's associated case-frame, if any. Mirrors case.l 59."
  (getr 'caseframe node))

(defun associate-cf (cf node)
  "Attach CF to NODE (and, the first time, record NODE as CF's
   assoc-node and announce it to semantics). Mirrors case.l 61."
  (setr 'caseframe cf node)
  (unless (get cf 'assoc-node)
    (setf (get cf 'assoc-node) node)
    (semcall 'newframe cf node (get cf 'case-frame))))

(defun assoc-node (cf)
  "The node CF is a frame for. Mirrors case.l 67."
  (get cf 'assoc-node))

(defun open-obj-cases (slotfr)
  "How many object slots SLOTFR still leaves open: 0 if none, 2 for an
   *obj case, else 1. Mirrors case.l 114."
  (cond ((or (null (car slotfr)) (null (caar slotfr))) 0)
        ((eq (caaar slotfr) '*obj) 2)
        (t 1)))

(defun maxunls (node)
  "Most object slots any of NODE's frame's hypotheses leaves unfilled.
   Mirrors case.l 69."
  (openchek node)
  (do ((sfs hypo-slots (cdr sfs)) (i 0))
      ((null sfs) i)
    (setq i (max i (open-obj-cases (car sfs))))))

(defun minunls (node)
  "Fewest object slots any of NODE's frame's hypotheses leaves
   unfilled. Mirrors case.l 76."
  (openchek node)
  (do ((sfs hypo-slots (cdr sfs)) (i 64))
      ((null sfs) i)
    (setq i (min i (open-obj-cases (car sfs))))))


;;; ===========================================================
;;; Open / close caching (case.l 484-508)
;;; ===========================================================

(defun openchek (node)
  "Make NODE's frame the open one (closing the previous open frame
   first if it differs). Mirrors case.l 484."
  (let ((cf (getr 'caseframe node)))
    (unless (equal openframe cf)
      (when openframe (closeframe openframe))
      (openframe cf))))

(defun closeframe (cf)
  "Flush the cached PRED / HYPO-SLOTS / OBJS-NEEDED back onto CF's
   plist. Mirrors case.l 490."
  (when cf
    (setf (get cf 'hypo-slots)  hypo-slots)
    (setf (get cf 'pred)        pred)
    (setf (get cf 'objs-needed) objs-needed)))

(defun openframe (cf)
  "Make CF the open frame, loading its cached slots into the specials;
   with CF nil, clear them. Mirrors case.l 496. (NB: `openframe' is
   also the special var holding the open frame.)"
  (setq openframe cf)
  (cond (cf (setq hypo-slots  (get cf 'hypo-slots)
                  pred        (get cf 'pred)
                  objs-needed (get cf 'objs-needed)))
        (t (clearcf))))

(defun clearcf ()
  "Close the open frame and reset the cached slots to empty. Mirrors
   case.l 503."
  (closeframe openframe)
  (setq hypo-slots (list (list nil nil)))
  (setq objs-needed 0)
  (setq pred nil))


;;; ===========================================================
;;; Case-register access (case.l 537-544)
;;; ===========================================================

(defun putc (reg value node)
  "Put VALUE under REG on NODE's frame. Mirrors case.l 537 verbatim,
   including its `caseframe'-guard / `case-frame'-write mismatch (see
   file header)."
  (and (get (car node) 'caseframe)
       (setf (get (get (car node) 'case-frame) reg) value)))

(defun getc (reg node)
  "Get REG from NODE's frame (a dummy symbol if NODE has none, so GET
   just returns NIL). Mirrors case.l 542."
  (get (or (get (car node) 'caseframe) '***total-dummy***) reg))


;;; ===========================================================
;;; Semantics trace hook (case.l 547-560)
;;; ===========================================================
;;;
;;; In the delivered source the "call to semantics" is reduced to an
;;; optional trace -- there is no external semantic component.

(defun nodep (foo)
  "Is FOO a node, i.e. (SYMBOL . FLAGS)? Mirrors case.l 552."
  (and (listp foo) (atom (car foo)) (numberp (cdr foo))))

(defun node-w-feats (list)
  "Render LIST for tracing, expanding any node into (head \\ features).
   Mirrors case.l 554."
  (do ((l list (cdr l)) (result))
      ((null l) (nreverse result))
    (push (cond ((nodep (car l)) (list (caar l) '\\ (fe (car l))))
                ((car l)))
          result)))

(defun semcall (&rest l)
  "Trace-only `call to semantics' (the real call is gone in the
   delivered source). Mirrors case.l 547."
  (unless (member (get openframe 'case-frame) '(nil dummy-mod))
    (when ctrace
      (apply #'say-it (cons '|To semantics:| (node-w-feats l))))))


;;; ===========================================================
;;; Deferred parse.l hook now unblocked (parse.l 586)
;;; ===========================================================

(defun alt-fillslot (a b c)
  "Record an ambiguous slot-fill on the current sentence node S, rather
   than committing it. Mirrors parse.l 586; needed `putc', hence it
   lands with case.l. (Companion of alt-attach in buffer-ops.)"
  (putc 'ambig-fillslot (list a b c) s))


;;; ===========================================================
;;; Prepositional-group denotation (case.l 565)
;;; ===========================================================
;;;
;;; `pgof' is the denotation the grammar runs for a preposition + its
;;; object NP (glang.l 520: `(prefix prepositional 10. (denfun 'pgof
;;; phrase of right and right))'). It fabricates a throw-away `dummypg'
;;; node whose only content is its daughters -- the prep and the np --
;;; and hands it back as a one-element daughter list, so the case-frame
;;; machinery can later read its NP and PREP daughters.
;;;
;;; Marcus stores the daughters as a literal disembodied plist
;;; `(nil np (ncons np) prep (ncons prep))'. We follow the port's
;;; convention instead (see `attach', buffer-ops.lisp 154): the
;;; `daughters' register holds a fresh gensym whose real plist carries
;;; the type->list mappings, so the standard `(daughter 'np node)' /
;;; `(daughters 'prep node)' accessors retrieve them.

(defun pgof (prep np)
  "Fabricate a `dummypg' node holding PREP and NP as its prep/np
   daughters, returned as a one-element daughter list. Mirrors
   case.l 565."
  (let ((faknode   (makesym 'dummypg))
        (daughters (gensym "DAUGHTERS-")))
    (setf (get daughters 'np)   (list np)
          (get daughters 'prep) (list prep))
    (setf (get faknode 'daughters) daughters)
    (list faknode)))


;;; ===========================================================
;;; Surface-structure monitoring (case.l 574-598)
;;; ===========================================================
;;;
;;; `create' and `attachment' crules are the grammar's way to run a bit
;;; of code whenever a node of some type is created, or a node of some
;;; type is attached under a father of some type. They are registered by
;;; CRULE-INDEX and fired by CREATE-MONITOR (from NEWNODE) and
;;; ATTACH-MONITOR (from ATTACH).
;;;
;;; Storage: Marcus keeps creation crules on the variable :create-rules
;;; and attachment crules on the :attach-rules symbol's plist (keyed by
;;; father-node type). We use one reboundable list and one reboundable
;;; hash-table -- same shape, isolable in tests (cf. *rule-index*).
;;;
;;; NB: glang-cl does not yet compile the `{CREATE ...}' /
;;; `{ATTACHMENT CRULE ...}' rule forms into CRULE-INDEX calls; that is
;;; a separate glang-cl extension. The runtime monitor mechanism here
;;; works against crules registered directly (or by a future emission).

(defvar *create-rules* nil
  "Alist of creation crules: each entry (NODE-TYPE FN NAME).")
(defvar *attach-rules* (make-hash-table :test #'eq)
  "Father-node-type -> alist of attachment crules (ATTACH-TYPE FN NAME).")
(defvar fnode nil "Father node, bound by ATTACH-MONITOR for a crule body.")
(defvar snode nil "Attached node, bound by ATTACH-MONITOR for a crule body.")

(defun reset-crules ()
  "Forget all create/attach crules (between grammar reloads / tests)."
  (setq *create-rules* nil)
  (clrhash *attach-rules*))

(defun crule-index (type indexinfo)
  "Register a crule. For 'creation, INDEXINFO is (NODE-TYPE FN NAME).
   For 'attachment, INDEXINFO arrives as ((FATHER-TYPE . ATTACH-TYPE)
   FN NAME) and is rewritten in place to (ATTACH-TYPE FN NAME), filed
   under FATHER-TYPE. Mirrors case.l 574."
  (cond ((eq type 'attachment)
         (let* ((ftype  (caar indexinfo))
                (bucket (gethash ftype *attach-rules*)))
           (rplaca indexinfo (cdar indexinfo))
           (unless (member indexinfo bucket :test #'equal)
             (setf (gethash ftype *attach-rules*)
                   (cons indexinfo bucket)))))
        ((eq type 'creation)
         (push indexinfo *create-rules*))))

(defun create-monitor (type)
  "Run the creation crule for TYPE, if any. Called by NEWNODE. Mirrors
   case.l 585."
  (let ((crule (assoc type *create-rules* :test #'eq)))
    (when crule
      (when *crtrace* (say |Running create-rule| $ (caddr crule)))
      (funcall (cadr crule)))))

(defun attach-monitor (fn dn type)
  "When DN is attached under FN as TYPE, run the matching attachment
   crule (if FN's node-type has one for TYPE), with FNODE/SNODE bound to
   FN/DN. Called by ATTACH. Mirrors case.l 591."
  (let* ((attach-rules (gethash (getr 'type fn) *attach-rules*))
         (crule (and attach-rules (assoc type attach-rules :test #'eq))))
    (when crule
      (when *crtrace* (say |Running attach-rule| $ (caddr crule)))
      (setq fnode fn snode dn)
      (funcall (cadr crule))))
  t)


;;; ===========================================================
;;; Semantic markers (case.l 405-440)
;;; ===========================================================
;;;
;;; A case's marker requirement is a list of semantic markers split by
;;; `#' into an "ok" set (before #) and a "great" set (after #). SMQVAL
;;; scores a node against a case: 1 if a node marker is in the great
;;; set (or the case wants `all'), 0 if in the ok set (or the node has
;;; no markers), -2 if nothing matches.

(defun smarkers (node)
  "The semantic markers to score NODE by: for a PP, the markers of its
   NP's head; otherwise the markers of NODE's head, else NODE's own.
   Mirrors case.l 427 (`nead' there is a typo for `head')."
  (cond ((is node '(pp)) (getr 'markers (head (find-node 'np node))))
        ((getr 'markers (head node)))
        ((getr 'markers node))))

(defun smqval (case node)
  "Score NODE against CASE: 1 (great / `all'), 0 (ok / no markers),
   -2 (no marker matches). The marker set is the pred-specific markers
   (GET PRED case-name) or the case name's generic `markerset', split
   by `#' into ok | great. Mirrors case.l 405."
  (do ((marktail (or (get pred (car case)) (get (car case) 'markerset))
                 (cdr marktail))
       (nmarkers (smarkers node))
       (value 0)
       (marker)
       (great-sw))                       ; Marcus's #sw: past the `#' yet?
      ((null marktail) -2)
    (cond ((eq (car marktail) 'all) (return 1))
          ((null nmarkers)
           (when *carefulsw* (warn "NO markers for ~s." node))
           (return 0))
          ((member (setq marker (car marktail)) nmarkers) (return value))
          ((eq marker '|#|)
           (setq value (if great-sw 0 (progn (setq great-sw t) 1)))))))

(defun smqchek (case upper node)
  "Does NODE fit CASE at all (score >= 0)? Mirrors case.l 424."
  (declare (ignore upper))
  (>= (smqval case node) 0))

(defun maxsmqval (caseset node)
  "Best SMQVAL of NODE over CASESET. Mirrors case.l 433. (Errors on an
   empty CASESET, as in the original -- callers supply a non-empty set.)"
  (apply #'max (mapcar (lambda (case) (smqval case node)) caseset)))

(defun fit-of (lower gfunc upper)
  "How well LOWER fits as UPPER's GFUNC argument. Mirrors case.l 437."
  (fit-of-1 lower (cases upper nil gfunc)))

(defun fit-of-1 (node caseset)
  "Mirrors case.l 440."
  (maxsmqval caseset node))


;;; ===========================================================
;;; Hypothesis generation (case.l 268-394)
;;; ===========================================================
;;;
;;; Given the open frame's HYPO-SLOTS (the ways its slots might still be
;;; filled), these enumerate the <case>s a node could fill in a given
;;; grammatical function (subj / obj / pp). With MODE non-nil each
;;; result is a <hypoframe> (<case> <remaining-open-cases> <full-slots>);
;;; with MODE nil it's just the <case>.

(defun cases (node mode gfunc)
  "The cases NODE's frame offers for grammatical function GFUNC
   (obj / subj / a preposition). Mirrors case.l 268."
  (openchek node)
  (cond ((eq gfunc 'obj)  (objcases mode))
        ((eq gfunc 'subj) (subjcases mode))
        (t (ppcases gfunc mode))))

(defun subjcases (mode)
  (mapcan (lambda (fr) (subjcasegen fr mode)) hypo-slots))

(defun objcases (mode)
  (mapcan (lambda (fr) (objcasegen fr mode)) hypo-slots))

(defun ppcases (gfunc mode)
  (mapcan (lambda (fr) (ppcasegen fr gfunc mode)) hypo-slots))

(defun subjcasegen (slotfr mode)
  "Subject cases of SLOTFR, read in reverse, stopping after the first
   obligatory case. Mirrors case.l 286."
  (do ((open-cases (reverse (car slotfr)) (cdr open-cases))
       (result)
       (next))
      ((or (eq (cadr next) 'oblig) (null (car open-cases))) result)
    (setq next (car open-cases))
    (push (if mode
              (list next (reverse (cdr open-cases)) (cadr slotfr))
              next)
          result)))

(defun objcasegen (slotfr mode)
  "Object cases of SLOTFR -- usually just the first open case, with
   special handling of an `*obj' (indirect-object) marker. Mirrors
   case.l 299."
  (block objcasegen
    (let ((result nil)
          (open-cases (car slotfr)))
      (when (null (car open-cases)) (return-from objcasegen nil))
      (when (eq (caar open-cases) '*obj)
        (do ((l (cdr open-cases) (cdr l))
             (hd nil (cons (car l) hd))          ; Marcus's `head' local
             (obj-case (cadar open-cases)))      ; Marcus's `*objcase'
            ((or (null l)
                 (when (eq (caar l) obj-case)
                   (push (if mode
                             (list (car l)
                                   (append (nreverse hd) (cdr l))
                                   (cadr slotfr))
                             (car l))
                         result)
                   t))
             ;; flush the *obj entry whether or not it was matched
             (setq open-cases (cdr open-cases)))))
      (unless (< (open-obj-cases (list open-cases)) objs-needed)
        (push (if mode
                  (list (car open-cases) (cons nil (cdr open-cases)) (cadr slotfr))
                  (car open-cases))
              result))
      result)))

(defun ppcasegen (slotfr prep mode)
  "Cases of SLOTFR markable by preposition PREP, read forward; a
   refillable case stays available. Mirrors case.l 333."
  (do ((open-cases (car slotfr) (cdr open-cases))
       (csave nil (cons (car open-cases) csave))
       (result)
       (prep-cases (or (get (get pred 'preps) prep)   ; Marcus's `ppcases' local
                       (get prep 'cases-marked-by))))
      ((null open-cases) result)
    (when (member (caar open-cases) prep-cases)
      (push (if mode
                (list (car open-cases)
                      (append (reverse csave)
                              (if (member (caar open-cases) refillables)
                                  open-cases
                                  (cdr open-cases)))
                      (cadr slotfr))
                (car open-cases))
            result))))


;;; ===========================================================
;;; consolidate-frame (case.l 366-394)
;;; ===========================================================
;;;
;;; Keep the frame's CASES the intersection of the filled slots across
;;; all hypothetical slotframes -- a slot certain in every hypothesis.

(defun consolidate-frame ()
  "Mirrors case.l 366."
  (cond
   ((null hypo-slots)
    (warn "no consistent hypo-slot")
    (when *carefulsw* (break "no consistent hypo-slot")))
   ((null (cdr hypo-slots))
    (bind-slots (filter-out-filled (car hypo-slots))))
   (t (do ((pcercs (filter-out-filled (car hypo-slots)) (cdr pcercs))
           (cert-slots))
          ((null pcercs) (bind-slots cert-slots))
        (do ((otherslotfrs (cdr hypo-slots) (cdr otherslotfrs))
             (slot (car pcercs)))
            ((null otherslotfrs) (push slot cert-slots))
          (unless (member slot (cadar otherslotfrs)) (return nil)))))))

(defun filter-out-filled (hslotfr)
  "The full-slots of HSLOTFR not already in the open frame's CASES.
   Mirrors case.l 382."
  (do ((hslots (cadr hslotfr) (cdr hslots))
       (result)
       (filled-cases (get openframe 'cases)))   ; Marcus's `cases' local
      ((null hslots) result)
    (unless (member (car hslots) filled-cases) (push (car hslots) result))))

(defun bind-slots (cert-slots)
  "Add CERT-SLOTS to the open frame's CASES (and announce each to the
   trace). Mirrors case.l 390."
  (setf (get openframe 'cases) (append cert-slots (get openframe 'cases)))
  (mapc (lambda (slot) (semcall 'case slot openframe)) cert-slots))


;;; ===========================================================
;;; The major case-frame operations (case.l 99-252)
;;; ===========================================================
;;;
;;;   need-slots      tell the frame to expect N objects
;;;   fits            can the node fit somewhere in the frame?
;;;   fillslot        put the node into the frame in a given role
;;;   finalize-frame  check all obligatory cases are filled
;;;   passivize-cf    let an object case become obligatory (passive)

;; pp-cf-check is referenced by fillslot in Marcus's source but is not
;; defined in any delivered file; no-op stub so the pp-on-S branch of
;; fillslot stays whole. dp1 is util.l's case-frame display, only reached
;; under ctrace; stubbed for the same reason.
(defun pp-cf-check (lower) (declare (ignore lower)) nil)
(defun dp1 (cf) (declare (ignore cf)) nil)

(defun set-objs-needed (n node)
  "Record that NODE's frame expects N objects, pruning hypotheses that
   can't supply that many. Mirrors case.l 104."
  (openchek node)
  (setq objs-needed n)
  (when hypo-slots
    (do ((sfrs hypo-slots (cdr sfrs)) (result))
        ((null sfrs) (setq hypo-slots result))
      (unless (< (open-obj-cases (car sfrs)) n)
        (setq result (cons (car sfrs) result))))))

(defun need-slots (node n)
  "Tell NODE's frame to expect N objects, then consolidate. Called from
   the grammar. Mirrors case.l 99."
  (set-objs-needed n node)
  (consolidate-frame))

(defun fits* (lower gfunc upper)
  "Does LOWER fit any of UPPER's cases for GFUNC? Mirrors case.l 134."
  (openchek upper)
  (do ((hyposet (cases upper nil gfunc) (cdr hyposet)))
      ((null hyposet) nil)
    (when (smqchek (car hyposet) upper lower) (return t))))

(defun fits (lower gfunc upper)
  "Does LOWER fit UPPER as GFUNC (subj / obj / pp)? For a PP modifying
   an S, builds a throwaway frame to test it. Mirrors case.l 121."
  (cond ((and (eq gfunc 'pp) (eq (getr 'type upper) 's))
         (cond ((getr 'caseframe lower) (fits* upper 'subj lower))
               (t (associate-cf (newcf 'dummy-mod) '(dummy-node))
                  (fillslot (find-node 'prep lower) 'pred '(dummy-node))
                  (fillslot (find-node 'np lower) 'obj '(dummy-node))
                  (fits* upper 'subj '(dummy-node)))))
        ((eq gfunc 'pp)
         (fits* (find-node 'np lower)
                (word (find-node 'prep lower))
                upper))
        (t (fits* lower gfunc upper))))

(defun fillmod (lower upper)
  "Attach LOWER as a modifier of UPPER's frame. Mirrors case.l 156."
  (openchek upper)
  (consprop openframe lower 'mods)
  (setf (get (getr 'caseframe lower) 'mod-of) openframe)
  (semcall 'mod lower openframe))

(defun fillcase (lower gfunc upper)
  "Fill a GFUNC case of UPPER's frame with LOWER, keeping every
   hypothesis whose markers still agree. Mirrors case.l 162."
  (openchek upper)
  (do ((hypos (cases upper t gfunc) (cdr hypos)) (result) (thehypo))
      ((null hypos) (setq hypo-slots result))
    (when (smqchek (car (setq thehypo (car hypos))) upper lower)
      (push (list (cadr thehypo)
                  (cons (list (caar thehypo) lower gfunc) (caddr thehypo)))
            result)))
  (when (and (plusp objs-needed) (eq gfunc 'obj))
    (set-objs-needed (1- objs-needed) upper))
  (consolidate-frame)
  (when ctrace
    (closeframe openframe) (terpri) (print '|-----------|)
    (say |New frames for| $ upper)
    (dp1 (getr 'caseframe upper))
    (terpri) (print '|-----------|)))

(defun fillpred (lower upper)
  "Make LOWER (a verb) the predicate of UPPER's frame, seeding
   hypo-slots from the pred's lexical case-frame. Mirrors case.l 184."
  (openchek upper)
  (setq pred (or (get (getr 'word lower) 'root) (getr 'word lower))
        hypo-slots (list (list (get pred 'case-frame))))
  (semcall 'head lower openframe))

(defun fillspec (lower upper)
  "Record LOWER as UPPER's frame specifier (aux for S/VP, q-det for NP).
   Mirrors case.l 192."
  (openchek upper)
  (setf (get openframe 'spec)
        (list (cond ((is-any-of upper '(s vp)) 'aux)
                    ((is-any-of upper '(np)) 'q-det)
                    (t (warn "~s can't be a spec for ~s" lower upper)))
              lower))
  (semcall 'spec (get openframe 'spec) openframe))

(defun fillslot (lower gfunc upper)
  "Put LOWER into UPPER's frame in role GFUNC. Mirrors case.l 142."
  (cond ((member gfunc '(subj obj)) (fillcase lower gfunc upper))
        ((and (eq gfunc 'pp) (eq (getr 'type upper) 's))
         (pp-cf-check lower)
         (fillmod lower upper)
         (fillcase upper 'subj lower))
        ((eq gfunc 'pp)
         (fillcase (find-node 'np lower)
                   (word (find-node 'prep lower))
                   upper))
        ((eq gfunc 'pred) (fillpred lower upper))
        ((eq gfunc 'mod)  (fillmod lower upper))
        ((eq gfunc 'spec) (fillspec lower upper))))

(defun finalize-frame (node)
  "For an S node, drop any hypothesis that still leaves an obligatory
   case open (warning if none survive). Mirrors case.l 204."
  (openchek node)
  (when (is-any-of node '(s))
    (do ((hypos hypo-slots (cdr hypos))
         (badf nil)
         (result nil))
        ((null hypos)
         (cond (result (setq hypo-slots result))
               (t (warn "some oblig slots left unfilled in ~s -- finalize-frame. ~s"
                        openframe badf)
                  (when *carefulsw* (break "badf")))))
      (do ((casetail (caar hypos) (cdr casetail))
           (case))
          ((null casetail) (push (car hypos) result))
        (cond ((null (setq case (car casetail))))
              ((eq 'oblig (cadr case))
               (cond ((and (eq (car case) 'agt) (is c '(np-preposed))))
                     (t (push (car hypos) badf) (return nil))))
              ((atom (cadr case)))
              ((eq '|opt\\oblig| (caadr case))
               (push (car hypos) badf) (return nil)))))
    (consolidate-frame))
  (semcall 'finalize-the-frame openframe)
  (closeframe openframe))

(defun passivize-cf (node)
  "Passivize NODE's frame. Mirrors case.l 234."
  (openchek node)
  (passivize-cf1 nil))

(defun passivize-cf1 (mode)
  "Add, for each hypothesis, a variant whose object case is now
   obligatory. Mirrors case.l 238."
  (setq hypo-slots
        (nconc (when mode hypo-slots)
               (mapcan
                (lambda (slotfr)
                  (mapcar (lambda (hypo)
                            (list (append (cadr hypo)
                                          (list (list (caar hypo) 'oblig)))
                                  nil))
                          (objcasegen slotfr t)))
                hypo-slots))))


;;; ===========================================================
;;; Miscellaneous (case.l 514-528)
;;; ===========================================================

(defun prefer (value1 degree value2)
  "Is VALUE1 within DEGREE of VALUE2 (i.e. not worse by more than
   DEGREE)? Mirrors case.l 514."
  (not (> degree (- value1 value2))))

(defun domf (node)
  "Walk up fathers / higher-clauses from NODE to the nearest node that
   has a case-frame. Mirrors case.l 517."
  (closeframe openframe)
  (do ((tnode (or (getr 'father node) (getr 'highercl node))
              (or (getr 'father tnode) (getr 'highercl tnode))))
      ((or (null tnode) (getr 'case-frame tnode)) tnode)))

(defun dom-cf (node)
  "The nearest caseframe dominating NODE (walking up fathers). Mirrors
   case.l 523."
  (do ((n (getr 'father node) (getr 'father n)))
      ((null n))
    (let ((temp (getr 'caseframe n)))
      (when temp (return temp)))))

;; poss-pg-cases (the possessive prepositional-group case candidates for
;; a node) is referenced by real-caseset but is not defined in any
;; delivered file; no-op stub returning NIL so the genitive branch stays
;; whole. (cf. pp-cf-check / dp1 above.) NOTINLINE so the constant NIL
;; doesn't make real-caseset's genitive loop look like dead code.
(declaim (notinline poss-pg-cases))
(defun poss-pg-cases (sourcenode) (declare (ignore sourcenode)) nil)

(defun real-caseset (caseset sourcenode)
  "CASESET's cases, plus a `genlc' (genitive) case for every possessive
   PG case whose `genl-case-for' markers meet CASESET's markers. CASESET
   is (cases markers). Mirrors case.l 528.

   The genitive branch is dormant in this port: `poss-pg-cases' is not in
   any delivered file (stubbed to NIL), so REAL-CASESET currently returns
   just CASESET's own cases."
  (append (car caseset)
          (do ((possc-tail (poss-pg-cases sourcenode) (cdr possc-tail))
               (result)
               (markers (cadr caseset)))
              ((null possc-tail) result)
            (when (intersection (get (car possc-tail) 'genl-case-for) markers)
              (setq result (cons (list (car possc-tail) 'genlc) result))))))
