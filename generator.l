;Morgan McGivern 2021
(load 'lp.l)
(load 'abctransmutator.l)
(load 'fibfractal.l)
(load 'lsys2.l)
(load 'markov.l)

(defun fibonaccinotes (n)
  "generates N bars of fibonacci based notes"
  (if (> n 0)
    (append (list (list (rfibnote) (rfibnote) (rfibnote) (rfibnote))) (fibonaccinotes (decf n)))
    (list (list (rfibnote) (rfibnote) (rfibnote) (rfibnote)))
    )
  )

; (defun Lsys-to-ABC (lsys #'encoding)
;
;   )

(defun lsys2demo1 (n)
  (setf *rule-table* '(
                       (((C 0 1/4 NIL)) ((((C 0 1/4 NIL) (A 0 1/4 NIL) (E 0 1/4 NIL)) 0.3)
                                         (((F 0 1/4 NIL) (D 0 1/4 NIL) (E 0 1/4 NIL)) 0.5)
                                         (((C 0 1/4 NIL) (A 0 1/4 NIL) (G 0 1/4 NIL) (C 0 1/4 NIL)) 1.000))
                                        )
                       (((A 0 1/4 NIL)) ((((A 0 1/4 NIL)) 1.0)))
                       (((E 0 1/4 NIL)) ((((E 0 1/4 NIL)) 1.0)))
                       (((G 0 1/4 NIL)) ((((G 0 1/4 NIL)) 1.0)))
                       (((D 0 1/4 NIL)) ((((D 0 1/4 NIL)) 1.0)))
                       (((F 0 1/4 NIL)) ((((F 0 1/2 NIL) (F 0 1/2 NIL)) 1.0)))
                       (((F 0 1/2 NIL)) ((((F 0 1/2 NIL)) 1.0)))
                       ))
  (setf lsystest (generate-IL n '((C 0 1/4 NIL))))
  (internal-to-abc (list lsystest))
  )


(defun lsys2demo2 (n)
  (setf *rule-table* '(
                       (((A 0 1/4 NIL)) ((((A 0 1/4 NIL) (E 0 1/4 NIL) (A 0 1/4 NIL)) 0.3)
                                         (((E 0 1/4 NIL) (A 0 1/4 NIL) (E 0 1/4 NIL)) 0.5)
                                         (((A 0 1/4 NIL) (A 0 1/4 NIL) (E 0 1/4 NIL)) 1.000))
                                        )
                       (((A 0 1/4 NIL) (A 0 1/4 NIL) (E 0 1/4 NIL)) ((((C 0 1/4 NIL)) 1.0)))
                       (((E 0 1/4 NIL) (A 0 1/4 NIL) (E 0 1/4 NIL)) ((((D 0 1/4 NIL)) 1.0)))
                       (((D 0 1/4 NIL)) ((((D 0 1/4 NIL)) 1.0)))
                       (((C 0 1/4 NIL)) ((((C 0 1/4 NIL)) 1.0)))
                       (((E 0 1/4 NIL)) ((((E 0 1/4 NIL)) 1.0)))
                       ))
  (setf lsystest (generate-IL n '((A 0 1/4 NIL))))
  (internal-to-abc (list lsystest))
  )
