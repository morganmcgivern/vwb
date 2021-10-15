; Morgan McGivern 2021
; brass instruments wash upon the seas of time, a copper mug which resonates, a nuanced belle nouveau
; a b r a c a d a b r a
; Start symbol: A
; Rule1: replace A with A B
; Rule2: replace B with B R
; Rule3: replace R with R A
; Rule4: replace R A with R A C
; Rule5: replace C with C A
; Rule6: replace C A with C A D
; Rule7: replace D with D A
; Rule8: replace D A with D A B
; Rule9: replace A B with A B R
; Rule10: replace B R with B R A

; (a b r a c a d a b r a)

(setq *rule-table* (make-hash-table))
;(setf (gethash '001 empList) '(Charlie Brown))

(defun t-defrule (a &rest b)
  "a being the input, b being the production(s)"
  (setf (gethash a *rule-table*) b)
  )

(defun t-display-rules ()
  (maphash #'(lambda (k v) (format t "~a -> ~a~%" k v)) *rule-table*)
  )

(defun t-delete-rule (a)
  (setf (gethash a *rule-table*) nil)
  )

(defun t-clear-rules ()
  (setq *rule-table* (make-hash-table))
  nil
  )

;stack overflow at 20 generations
(defun rgh (l)
  (if (null l)
    (gethash (car l) *rule-table*)
    (append (gethash (car l) *rule-table*) (rgh (cdr l)))
    )
  )

(defun rgh2 (l)
  (let ((x '()))
    (dolist (item l x) (setf x (append x (gethash item *rule-table*))))
    )
  )

(defun generate-l-system-display (n s &key (g 0))
  (format t "G~a: ~a~%" g s)
  (if (>= g n)
    ;(mapcar #'(lambda (x) (gethash x *rule-table*)) s)
    (rgh2 s)
    (generate-l-system-display n (rgh2 s) :g (+ g 1))
    )
  nil
  )

(defun algae-test ()
  (t-defrule 'a 'a 'b)
  (t-defrule 'b 'a)
  )

; algae
; 1. Alphabet: {A,B}
; 2. Start string: A
; 3. Productions:
; (1) A → AB
; (2) B → A

; (((D 0 1/2 NIL) (A 0 2 NIL) (F 0 1/2 NIL) (E 0 1/4 NIL))
;  ((E -1 1/4 NIL) (B -1 1/2 NIL) (D -1 2 NIL) (C -1 1/4 NIL))
;  ((A -1 1/2 NIL) (F -1 1/2 NIL) (E -1 1/2 NIL) (D -1 1/4 NIL)))

;-------------------------------------------------------------------------------------------------------------------
; Context Senstive L Systems (IL) -
; 1L Systems have one context left or right, 2L systems have both
; ??? Lsystems ???
; lcontext ancestor rcontext -> descendent
; These can be deterministic, or stochastic where each rule has an associated probability
;
; Bracketed L-Systems:
; Some kind of symbol like [] will start a new branch which operates independently
; (1) A → AB
; (2) B → A[A]
; n=0 : A
; n=1 : AB
; n=2 : ABA[A]
; n=3 : ABA[A]AB[AB] ; don't get confused here, remember the B before becomes A[A] and [A] becomes [AB]
; n=4 : ABA[A]AB[AB]ABA[A][ABA[A]]

; Non propagative L systems would be which there is only one symbol in the productions

;-------------------------------------------------------------------------------------------------------------------
;apparently, make-hash-table's default :test is #'eql not #'equal leading to returns of nil values
(setq *mus-rule-table* (make-hash-table :test #'equal))
(setq *mus-prob-table* (make-hash-table :test #'equal))

;make a system storage and changer
;defrule should check that the probabilities add up correctly

(defun m-defrule (a b &key (prob 1.00))
  "a being the input, b being the production(s)"
  (setf (gethash a *mus-rule-table*) b)
  (setf (gethash a *mus-prob-table*) prob)
  (m-display-rules)
  )

(defun m-display-rules ()
  (format t "Alpha -> Rewrite - Probability~%")
  (maphash #'(lambda (k v) (format t "~a -> ~a ~a ~%" k v (gethash k *mus-prob-table*))) *mus-rule-table*)
  )

(defun m-clear-rules ()
  (setq *mus-rule-table* (make-hash-table :test #'equal))
  (setq *mus-prob-table* (make-hash-table :test #'equal))
  )

;note--difference between accessors--
(defun algae-t ()
  (m-defrule 'a '(a b))
  (m-defrule 'b '(a))
  )

(defun algae-t2 ()
  (m-defrule '(a) '(a b))
  (m-defrule '(b) '(a))
  )
;------------------------------------

;((a b) (prob .3) (b a))

; (defun prob-test ()
;   (m-defrule 'a '(a b) :prob .70)
;   (m-defrule 'a '(b a) :prob .30)
;   (m-defrule 'b '(a))
;   )

(defun prob-test ()
  (m-defrule 'a '((a b) (b a)) :prob '(.70 .30))
  ; if we have sublists, then assign probability to corresponding locations that adds up to 1
  (m-defrule 'b '(a))
  )

(defun prob-test2 ()
  (m-defrule 'a '(a b))
  ; if we have sublists, then assign probability to corresponding locations that adds up to 1
  (m-defrule 'b '(a))
  )

(defun exe-rule (item)
  (let ((productions (gethash item *mus-rule-table*))
        (r (random 1.00))
        )
    (if (listp (car productions))
      (nth (position r (gethash item *mus-prob-table*) :test #'>) productions)
      ;I have a list of probilities and a list of items in corresponding slots, how do I roll and retrieve from the other one?
      productions
      )
    )
  )

(defun m-L-system (l)
  (let ((x '()))
    (dolist (item l x) (setf x (append x (exe-rule item))))
    )
  )

;context free w/ probability
(defun m-generate-L (n s &key (g 0))
  (format t "G~a: ~a~%" g s)
  (if (>= g n)
    (m-L-system s)
    (m-generate-L n (m-L-system s) :g (+ g 1))
    )
  nil
  )

;context sensitive
(defun m-IL-system ()
  ;pass in pairs/threes to the hash table, rewrite those symbols
  )

(defun m-generate-IL (n s &key (g 0))
  (format t "G~a: ~a~%" g s)
  (if (>= g n)
    (m-IL-system s)
    (m-generate-IL n (m-IL-system s) :g (+ g 1))
    )
  nil
  )


;generate context free
;generate context sensitive
;two l system mappers, how do I generate the whole note construct?
;one which maps notes and their octaves
;one which maps durations

(defun scloop (i)
  (cond
    ((= i 7) 0)
    ((= i -1) 6)
    (t i)
    )
  )

(defun algae-map (l)
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
