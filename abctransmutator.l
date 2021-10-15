; Morgan McGivern 2021
; ~Contains functions related to converting to and from EasyABC notation
;  and an internal lisp representation that is as follows~

; list depth zero contains lists each representing a bar grouping
; list depth one contains lists each representing an individual note
; list depth two contains four values each representing parts of a note as follows
; (note octave duration accent)
; ex. (G -4 1/2 SHARP) <=> ^G,,,1/2
; note being a whole tone,
; the octave of 0 relative to the uppercase notation in EasyABC, lower than that being negative, and lowercase being 1 and above
; duration being a fraction or whole number directly related to the note duration
; accent being sharp flat or natural

(load 'split-sequence.l) ;adapted from cl-utilities -> https://common-lisp.net/project/cl-utilities/
(load 'lp.l)
;  "C, D, E, F,|G, A, B, C|D E F G|A B c d|e f g a|b c' d' e'|f' g' a' b'|"
;  "C,,2 D,,4 E,1/2 F,|G, _A,1/2 B, C|D,,,,2 E F, G|A B,,, c d|e ^f'' =g a|b _c'''2 d' e'1/8|_f' g'1/2 a'1/2 b'2"
; ^G,,,/2

(setf string-scale-low '("c" "d" "e" "f" "g" "a" "b")) ;octave score of 1
(setf string-scale-up '("C" "D" "E" "F" "G" "A" "B")) ;octave score of 0
(setf scale '(C D E F G A B c))


(defun note-string-to-internal (x)
  "non-case-sensitive returning internal note against scale selection from string"
  (nth (position x string-scale-up :test #'string-equal) scale)
  )

(defun accent-string-to-internal (x)
  "converts accent string to internal representation ex. ^ -> sharp"
  (cond
    ((string= x "^") 'sharp)
    ((string= x "_") 'flat)
    ((string= x "=") 'natural)
    (t nil))
  )

(defun count-octave (x o)
  "returns octave count for , and ' abc"
  (cond
    ((= (length x) 0) o)
    ((string= "'" (subseq x 0 1)) (count-octave (subseq x 1) (incf o)))
    ((string= "," (subseq x 0 1)) (count-octave (subseq x 1) (decf o)))
    (t o)
    )
  )

(defun duration-to-internal (x)
  (if (string= x "")
    1/4
    (read-from-string x)
    )
  )

(defun indiv-abc-note-to-internal (x)
  "converts individual abc note strings to a list representation: (note octave duration accent)"
  ;TODO implement duration
  (let* (
         (accent (accent-string-to-internal (subseq x 0 1)))
         (index (if (not (eq accent nil))
                  1
                  0))
         (noteo (subseq x index (+ index 1)))
         (note (note-string-to-internal (subseq x index (incf index))))
         (octave (count-octave (subseq x index) (if (member noteo string-scale-low :test #'string=)
                                                  1
                                                  0)))
         ;increase index by absolute and adjusted octave value
         (index (if (member noteo string-scale-low :test #'string=)
                  (+ index (abs octave) -1)
                  (+ index (abs octave))
                  ))
         (duration (duration-to-internal (subseq x index)))
         )
    (list note octave duration accent)
    )
  )


(defun abc-to-internal (input-string)
  "converts whole abc strings to internal"
  (let* (
        (groupings (split-sequence #\| input-string))
        (indiv-notes (mapcar #'(lambda (x) (split-sequence #\  x)) groupings))
        )
    (mapcar #'(lambda (x) (mapcar (lambda (y)
                                         (indiv-abc-note-to-internal y)
                                          ) x)) indiv-notes)
    )
  )

(defun internal-to-accent-string (x)
  "converts the internal accent to it's corresponding character"
  (cond
    ((eq x 'sharp) "^")
    ((eq x 'flat) "_")
    ((eq x 'natural)  "=")
    (t ""))
  )

(defun find-note-string (n o)
  "gets the correct character for a note"
  (if (<= o 0)
    (nth (position n scale) string-scale-up)
    (nth (position n scale) string-scale-low)
    )
  )

(defun octave-to-string (x)
  "takes a number, returns n commas below 0, returns n apostrophes above 1"
  (cond
    ((< x 0) (concatenate 'string "," (octave-to-string (incf x))))
    ((> x 1) (concatenate 'string "'" (octave-to-string (decf x))))
    ((= x 0) "")
    )
  )

(defun duration-to-string (x)
  (if (= x 1/4)
    ""
    (write-to-string x)
    )
  )

(defun construct-note-string (x)
  "takes input in the form of (note octave duration accent) ex. (G -4 1/2 sharp) and outputs abc note string ex. ^G,,,,1/2"
  (concatenate 'string
               (internal-to-accent-string (fourth x))
               (find-note-string (first x) (second x))
               (octave-to-string (second x))
               (duration-to-string (third x))
               " "
               )
  )

; add new line after every four
(defun compile-groupings (x)
  "takes a list that has sublists representing groupings of note strings,
  adds the ABC | bar marker to each and compiles them into one string,
  then compiles each post processed string in the superlist into one string"
  (format nil "~{~a~}" (mapcar #'(lambda (y) (format nil "~{~a~}" (snoc " | " y))) x))
  )

(defun internal-to-abc (input)
  "takes input in the form of ( ((C -1 1/4 NIL) (D -1 1/4 NIL) (E -1 1/4 NIL) (F -1 1/4 NIL)) (...) ) and converts it to
  C, D, E, F,| ..."
  (compile-groupings (mapcar #'(lambda (x) (mapcar #'(lambda (y)
                                                             (construct-note-string y)
                                                             ) x)) input))
  )
