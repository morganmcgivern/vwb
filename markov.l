(load 'lp.l)

(defun mxcz (x)
  (let ((mx '())
        (unq (remove-duplicates x :test #'equal)))
    (mapcar #'(lambda (x) (mapcar #'(lambda (y) (setf mx (append mx (list (list x y 0)) ))) unq)) unq)
    mx
    )
  )


(defun count-mxcz (symbols)
  "given a list transfers the counts of each object transition into the count-mx variable"
  ;(mapcar #'(lambda (x y) (list x y)) x (cdr x)) - returns tuples of each transition
  ;take each of those, then lookup the respective object in count-mx and inc the third
  (let ((transitions (mapcar #'(lambda (x y) (list x y)) symbols (cdr symbols)))
        (count-mx (mxcz symbols))
        )

    (mapcar #'(lambda (x) (incf (third (car (member x count-mx :test #'equal :key #'(lambda (x) (list (car x) (cadr x)))))))) transitions)
    count-mx
    )
  )

(defun prob-mxcz (symbols)
  (let* ((unq (remove-duplicates symbols :test #'equal))
         (prob-mx (count-mxcz symbols))
         (totals (mapcar #'(lambda (x) (list x (let ((row-total 0))
                            (mapcar #'(lambda (y) (if (equal x (second y)) (incf row-total (third y)))) prob-mx)
                                row-total
                             ))) unq)))
    (mapcar #'(lambda (x) (setf (third x) (float (/ (third x) (second (car (member (second x) totals :test #'equal :key #'car))))))) prob-mx)
    ;for each in unq, map over prob-mx and find their cells,
    ;add them up for the total, then modify each of their values to the division
    prob-mx
    )
  )

(defun dist-mxcz (symbols)
  (let* ((unq (remove-duplicates symbols :test #'equal))
        (dist-mx (prob-mxcz symbols))
        (curr-sum 0)
        )
    (mapcar #'(lambda (symbol) (let* ((l (remove 'nil (mapcar #'(lambda (x) (if (equal (second x) symbol) x)) dist-mx)))
                                      (curr-sum (third (car l)))
                                      )
                                 (mapcar #'(lambda (y) (incf (third y) curr-sum)
                                                   (setf curr-sum (third y))) (cdr l))
                                 )) unq)
    dist-mx
    )
  )

(defun display-dist-mx (symbols)
  (format t "~S ~%" (mapcar #'(lambda (y) (remove 'nil (mapcar #'(lambda (x) (if (equal (second x) y) x)) (dist-mxcz symbols)))) (remove-duplicates symbols :test #'equal)))
  )

(defun tabelize-mxcz (symbols)
  (let ((out '()))
    (mapcar #'(lambda (y) (setf out (append out (remove 'nil (mapcar #'(lambda (x) (if (equal (second x) y) x)) (dist-mxcz symbols)))))) (remove-duplicates symbols :test #'equal))
    out)
  )

(setf *random-history* '())

(defun markov-next-lookup (symbol mx)
  (let ((r (random 1.000))
        (row (remove 'nil (mapcar #'(lambda (x) (if (equal (second x) symbol) x)) mx))))
    (setf *random-history* (append *random-history* (list r)))
    (caar (member r row :test #'< :key #'third))
    )
  )

(defun gen-markov (start-symbol len corpus)
  (setf *random-history* '())
  (let ((markov (list start-symbol)))
    (dotimes (n len)
             (setf markov (append markov (list (markov-next-lookup (car (last markov)) corpus))))
             )
    markov
    )
  )

(defun markovdemo1 ()
  (setf beet (car (abc-to-internal "E E F G G F E D C C D E")))
  (tabelize-mxcz beet)
  (internal-to-abc (list (gen-markov '(E 0 1/4 NIL) 32 (tabelize-mxcz beet))))
  )

(defun markovdemo2setup ()
  (setf hobbits (car (abc-to-internal "d e f f f a e d e A B c c c d B F2 A E2 A2 c2 d e f f a f e d e c d c B8 d3 c c3 B F3 G F E3 D F F G E4 c d e e e g d c d G A B B B c A E2 G D2 G2 B2 c d e e g e d c d B c B A8 c3 B B3 A E3 F E D3 C E E F D4 c d e e e gd c d G A B B B c A E2 G D2 G2 B2 c d e e g ed c d B c B A8 c3 B B3 A E3 F E D3 C E E F D4 d e f f f a e d e A B c c c d B F2 A E2 A2 c2 d e f f a f e d e c d c B8 d3 c c3 B F3 G F E3 D F F G E4 c d e e e g d c d G A B B B c A E2 G D2 G2 B2 c d e e g e d c d B c B A8 c3 B B3 A E3 F E D3 C E E F D4 c d e e e g d c d G A B B B c A E2 G D2 G2 B2 c d e e g e d c d B c B A8 c3 B B3 A E3 F E D3 C E E F D4 d e f f f a e d e A B c c c d B F2 A E2 A2 c2")))
  (setf ht (tabelize-mxcz hobbits))
  )

(defun markovdemo2 (length)
  (internal-to-abc (list (gen-markov '(D 1 1/4 NIL) length ht)))
  )
