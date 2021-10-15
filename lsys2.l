(setf *rule-table* '())

;add a well formed production checker
;add it so that the input probability is cumulatively added when executing
;streamline defrule
;add assign loose symbols persistence to themselves flag,
;if there are loose productions which do not have a corresponding symbol to rewrite itself each generation create one
;fix 1L in symbol-lookup2


(defun defrule (symbol production)
  "well formed input like 'a (((a b) 1.000)) or 'b ( ((b a) 0.3) ((a b) 0.7) )"
  ; (setf *rule-table* (append (cond
  ;                              ((numberp (cdr production)) )
  ;                              )))
  (if (eq nil (member symbol (mapcar #'car *rule-table*)))
    (setf *rule-table* (append *rule-table* (list (list symbol production))))
    (mapcar #'(lambda (x) (if (eq symbol (car x))
                            (setf (second x) production)
                            )) *rule-table*)
    )
  )

(defun display-rules ()
  (mapcar #'(lambda (x) (format t "~S~%" x)) *rule-table*)
  (terpri)
  )

(defun clear-rules ()
  (setf *rule-table* '())
  )

(defun symbol-lookup (s)
  (car (cdr (car (member s *rule-table* :test #'(lambda (x y) (eq (car y) x))))))
  )

(defun exe-rule (x)
  ;add random state so I can modify the random seed
  (let ((r (random 1.000))
        (ps (sort (symbol-lookup x) #'< :key #'cadr)))
    ; (print r)
    (caar (member r ps :test #'< :key #'cadr))
    )
  )

(defun L-system (l)
  (let ((x '()))
    (dolist (item l x) (setf x (append x (exe-rule item))))
    )
  )

(defun generate-L (n s &key (g 0))
  (format t "G~a: ~a~%" g s)
  (if (>= g n)
    (L-system s)
    (generate-L n (L-system s) :g (+ g 1))
    )
  )

;context sensitive l system

(setf *sl2-flag* nil)


;

(defun symbol-lookup2 (left item right)
  "reads second parameter item, and looks it's productions up in the rule table with it's left and right context"
  ;twoL finds a three parameter rule, if none oneL finds one, if none then it must be a cfL
  ;when looking at a symbol it is unknown wether there may be a rule pertaining to the influence
  ;of it's neighbors and in the case of an individual symbol. look both ways before you cross the street
  (let* ((twoL (car (member item *rule-table* :test #'(lambda (x y) (and
                                                                    (equal (first (car y)) left)
                                                                    (equal (second (car y)) x)
                                                                    (equal (third (car y)) right))))))
         (oneL (if (null twoL) ;this needs to be checked against which way it is looking designed in the ruleset ex. (A B *) or (* B A)
                 (car (member item *rule-table* :test #'(lambda (x y) (and (= (length (car y)) 2)
                                                                           (or (and
                                                                                (equal (first (car y)) left)
                                                                                (equal (second (car y)) x))
                                                                               (and
                                                                                (equal (second (car y)) right)
                                                                                (equal (first (car y)) x))
                                                                               )))))))
         (cfL (if (and (null oneL) (null twoL))
                (car (member item *rule-table* :test #'(lambda (x y) (equal (first (car y)) x))))))
        )
    (cond
      ((not (null twoL)) (if *sl2-flag* (format t "found 2l rule: ~S at ~S ~%" twoL item))
                         (cadr twoL))
      ((not (null oneL)) (if *sl2-flag* (format t "found 1l rule: ~S at ~S ~%" oneL item))
                         (cadr oneL)
                         )
      ((not (null cfL))  (if *sl2-flag* (format t "found 0l rule: ~S at ~S ~S ~S  ~% " cfL left item right))
                         (cadr cfL))
      (t 'NO-RULE-FOUND)
      )
    )
  )

(defun demo-sl2 ()
  (setf *rule-table* '(
                       ((A) (((A B) 1.0)))
                       ((B) (((R) 1.0)))
                       ((R) (((A) 1.0)))
                       ((B R A) (((A C) 1.0)))
                       ((R A) ((() 1.0)))

                       ((C) (((A) 1.0)))
                       ((A C A) (((A D) 1.0)))
                       ((C A) ((() 1.0)))
                       ((D) (((A) 1.0)))
                       ((A D A) (((A B) 1.0)))
                       ((D A) ((() 1.0)))
                       ))
  (format t "ABRACADABRA RULE TABLE ~%")
  (display-rules)
  (setf *sl2-flag* nil)
  (generate-IL 20 '(a))
  (setf *sl2-flag* nil)
  )

(defun s-exe-rule (left item right)
  (let ((r (random 1.000))
        (ps (sort (symbol-lookup2 left item right);productions go here
                  #'< :key #'cadr)))
    ; (print r)
    (caar (member r ps :test #'< :key #'cadr))
    )
  )

; (setf *rule-table* '( ((A) (((A B) 1.0))) ((B) (((B R) 1.0))) ((B R) (((B R A) 1.0))) ((B R A) (((B R A C) 1.0))) ((C) (((C A) 1.0)))))
;( ((A) (((A B) 1.0))) ((B) (((B R) 1.0))) ((B R) (((B R A) 1.0))) ((B R A) (((B R A C) 1.0))) ((C) (((C A) 1.0))))

(defun IL-system (l)
  (let ((x '())
        (itemindex 0)
        (left '())
        (right '())
        )
    (dolist (item l x)
            (if (>= itemindex 1)
              (setf left (nth (- itemindex 1) l))
            )
            (if (< itemindex (length l))
              (setf right (nth (+ itemindex 1) l))
              (setf right '())
              )
            (let ((prod (s-exe-rule left item right)))
              (setf x (append x prod));for each item in the list, excute the related rule
              (incf itemindex))
            )
    )
  )

(defun generate-IL (n s &key (g 0))
  (format t "G~a: ~a~%" g s)
  (if (>= g n)
    (IL-system s)
    (generate-IL n (IL-system s) :g (+ g 1))
    )
  )

(defun abracadabra ()
  (setf *rule-table* '(
                       ((A) (((A B) 1.0)))
                       ((B) (((R) 1.0)))
                       ((R) (((A) 1.0)))
                       ((B R A) (((A C) 1.0)))
                       ((R A) ((() 1.0)))

                       ((C) (((A) 1.0)))
                       ((A C A) (((A D) 1.0)))
                       ((C A) ((() 1.0)))
                       ((D) (((A) 1.0)))
                       ((A D A) (((A B) 1.0)))
                       ((D A) ((() 1.0)))
                       ))
  )

(defun demo-IL1 ()
  (setf *rule-table* '(
                       ((A) (((A B) 1.0)))
                       ((B) (((R) 1.0)))
                       ((R) (((A) 1.0)))
                       ((B R A) (((A C) 1.0)))
                       ((R A) ((() 1.0)))

                       ((C) (((A) 1.0)))
                       ((A C A) (((A D) 1.0)))
                       ((C A) ((() 1.0)))
                       ((D) (((A) 1.0)))
                       ((A D A) (((A B) 1.0)))
                       ((D A) ((() 1.0)))
                       ))
  (format t "ABRACADABRA RULE TABLE ~%")
  (display-rules)
  (format t "ABRACADABRA~%")
  (generate-IL 20 '(a))
  )

;open L system

(defun algae-t ()
  (defrule 'a '(((a b) 0.5) ((b a) 1.000)))
  (defrule 'b '(((a) 1.000)))
  )

(defun algae-t2 ()
  (defrule 'a '(((a b) 0.3) ((b a) 1.000)))
  (defrule 'b '(((a) 0.5) ((b) 1.000)))
  )


(defun scloop (i)
  (cond
    ((>= i 7) 0)
    ((<= i -1) 6)
    (t i)
    )
  )

(defun algae-note-map (l)
  (let ((s '(C D E F G A B))
        (i -1))
    (mapcar #'(lambda (x) (cond
                            ((equal x 'a) (incf i)
                                          (setf i (scloop i))
                                          (nth i s)
                                          )
                            ((equal x 'b) (decf i)
                                          (setf i (scloop i))
                                          (nth i s))
                            )) l)
    )
  )
