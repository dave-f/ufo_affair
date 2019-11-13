;
; Functions for creating enemies

(defun dice-roll (type)
  (1+ (random type)))

(defmacro dice-roll-macro (arg)
  (if (string= arg "1d6")
      '(1+ (random 6))
    1))

(macroexpand-1 '(dice-roll-macro 1d6))

(defun test ()
  "Test macro"
  (dice-roll-macro 1d6))

; str con siz int pow dex
; hp 14; fighing 30% 1d3
(defun create-zombie-base ()
  "Return a list of (str con siz int pow dex) for a zombie"
  (let ((stats))
    (push (cons 'str (+ (dice-roll 6) (dice-roll 6) (dice-roll 6))) stats)
    (push (cons 'con (+ (dice-roll 6) (dice-roll 6) (dice-roll 6))) stats)
    (push (cons 'siz (+ (dice-roll 6) (dice-roll 6) 6)) stats)
    (push (cons 'int 1) stats)
    (push (cons 'pow 1) stats)
    (push (cons 'dex (+ (dice-roll 6) (dice-roll 6))) stats)
    stats))

(defun create-migo-base ()
  "Return a list of (str con siz int pow dex) for a mi-go"
  (let ((stats))
    (push (cons 'str (+ (dice-roll 6) (dice-roll 6) (dice-roll 6))) stats)
    (push (cons 'con (+ (dice-roll 6) (dice-roll 6) (dice-roll 6))) stats)
    (push (cons 'siz (+ (dice-roll 6) (dice-roll 6) (dice-roll 6))) stats)
    (push (cons 'int (+ (dice-roll 6) (dice-roll 6) 6)) stats)
    (push (cons 'pow (+ (dice-roll 6) (dice-roll 6) 6)) stats)
    (push (cons 'dex (+ (dice-roll 6) (dice-roll 6) (dice-roll 6) (dice-roll 6))) stats)
    stats))

; str con siz int pow dex
; hp 18; brawl 50% 1d4
(defun create-ape-base ()
  "Return a list of (str con siz int pow dex) for an ape"
  (let ((stats))
    (push (cons 'str (+ (dice-roll 6) (dice-roll 6) (dice-roll 6))) stats)
    (push (cons 'con (+ (dice-roll 6) (dice-roll 6) (dice-roll 6))) stats)
    (push (cons 'siz (+ (dice-roll 6) (dice-roll 6) 6)) stats)
    (push (cons 'int (dice-roll 6)) stats)
    (push (cons 'pow (dice-roll 6)) stats)
    (push (cons 'dex (+ (dice-roll 6) (dice-roll 6) (dice-roll 6))) stats)
    stats))

(defun create-enemy-triple (arg)
  "Create an enemy triple, i.e. percentage/percentage half/percentage fifth"
  (list (* arg 5) (floor (* arg 5) 2.0) (floor (* arg 5) 5.0)))

(defun create-enemy-percentages (enemy)
  "Create a list of percentages plus half and fifth for a list of enemy stats"
  (mapcar (lambda (i)
            (cond
             ((eq (car i) 'str)
              (list "Effort" (create-enemy-triple (cdr i))))
             ((eq (car i) 'con)
              (list "Stamina" (create-enemy-triple (cdr i))))
             ((eq (car i) 'siz)
              (list "Damage Bonus" "-"))
             ((eq (car i) 'int)
              (list "Idea" (create-enemy-triple (cdr i))))
             ((eq (car i) 'pow)
              (list "Luck" (create-enemy-triple (cdr i))))
             ((eq (car i) 'dex)
              (list "Agility" (create-enemy-triple (cdr i))))
             (t -1)))
          enemy))

(defun create-zombie ()
  "Create a zombie.  Returns a list of basic stats followed by a list of percentages."
  (let* ((zb (reverse (create-zombie-base)))
         (zf (create-enemy-percentages zb)))
    (list zb zf)))

(defun create-ape ()
  "Create an ape.  Return a list of basic stats followed by a list of percentages."
  (let* ((ab (reverse (create-ape-base)))
         (af (create-enemy-percentages ab)))
    (list ab af)))

(defun create-migo ()
  "Create a mi-go.  Return a list of basic stats followed by a list of percentages."
  (let* ((ab (reverse (create-migo-base)))
         (af (create-enemy-percentages ab)))
    (list ab af)))

; e.g.
(loop for i from 0 to 2 do
      (princ (create-migo))
      (princ "\n"))
