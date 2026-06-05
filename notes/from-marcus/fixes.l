(defun le (a b) (or (< a b) (= a b)))
(defun ge (a b) (or (> a b) (= a b)))
(defun consprop (atom value property)
  (putprop atom (cons value (get atom property)) property))

(putd 'cat (getd 'concat))
(setsyntax '\[ 2)
(setsyntax '\] 2)
(defun linel (port) linel) ;;hack, hack
(putd 'charpos (getd 'nwritn))
(putd 'sprinter (getd '$prpr))
(putd 'sprint (getd '$prdf)) ;;well, close

