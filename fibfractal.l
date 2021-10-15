
; (note octave duration accent)
; ex. (G -4 1/2 SHARP) <=> ^G,,,1/2
;( ((1)(2)(3)(4)) ((1)(2)(3)(4)) )
(setf scale '(C D E F G A B))
(load 'lp.l)
; EUCLID(m, k)
; 1. if k = 0
; 2. then return m
; 3. else return EUCLID(k,m mod k)
;where m > k


; (defun random-note-duration (&optional u l)
;   "generates random note duration"
;   ;idea - implement euclidian geometry for rhythms
;   (let ((x (random 3)))
;     (cond
;       ((= x 0) '2)
;       ((= x 1) '1/4)
;       ((= x 2) '1/2)
;       )
;     )
;   )

(defun random-note-duration (&optional u l)
  "generates random note duration"
  ;??? how is a note duration decided? what determines an "attractive" duration?
  ;idea - implement euclidian geometry for rhythms
  (let ((x (random 100)))
    (cond
      ;these values can be messed with
      ((< x 10) '2)
      ((< x 70) '1/4)
      ((< x 100) '1/2)
      )
    )
  )

;https://www.howtobuildsoftware.com/index.php/how-do/c9aZ/recursion-lisp-clisp-generating-fibonacci-series-in-lisp-using-recursion
(defun fibonacci (n &optional (a 0) (b 1) (acc ()))
  "generates a list of fibonacci numbers up to N"
  (if (zerop n)
    (nreverse acc)
    (fibonacci (1- n) b (+ a b) (cons a acc)))
  )

; (defun rfibnote ()
;   "generates a random fibonacci note"
;   (list (nth (mod (car (last (fibonacci (+ 1 (random 25))))) 7) scale) 0 '1/4 NIL)
;   )

(defun rfibnote ()
  "generates a random fibonacci note"
  (list (nth (mod (car (last (fibonacci (+ 1 (random 25))))) 7) scale) 0 (random-note-duration) NIL)
  )

(defun inc-fib-note (o)
  "generates an incremental fibonacci note, starting at an octave of -2 to 2"
  (if inc-fib-index-p
    (decf inc-fib-index)
    (incf inc-fib-index)
    )
  (list (nth (mod (car (last (fibonacci inc-fib-index))) 7) scale) o (random-note-duration) NIL)
  )

(defun inc-fibonacci-notes (n)
  "generates N bars of fibonacci based notes a little more tightly together"
  (setf inc-fib-index-p (if (= 1 (random 2)) t nil))
  (setf inc-fib-index (+ 5 (random 25)))
  (let ((o (- (random 2) 1)))
    (if (> n 0)
      (append (list (list (inc-fib-note o) (inc-fib-note o) (inc-fib-note o) (inc-fib-note o))) (inc-fibonacci-notes (decf n)))
      (list (list (inc-fib-note o) (inc-fib-note o) (inc-fib-note o) (inc-fib-note o)))
      )
    )
  )

  ; generate bassline ->
  ; get the first note of each grouping
  ; create 1/4 notes or a 4 length to fill the bar

  ;generating chords ->
  ; take original code, modify the notes/octave etc by x method, then create an abc output method that
  ; takes each note from both lists and puts them inside []
