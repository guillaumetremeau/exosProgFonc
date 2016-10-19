;;0
(define carre (lambda (x) (* x x)));
(carre 5);

(define quad (lambda (x) (* x (* x (* x x)))));
(quad 3);

;;1
(define cercle (lambda (r) (list 'Pour 'le 'cercle 'de 'rayon r'.
                                 'La 'circonference 'est (* 3.14 (carre r))'.
                                 'La 'surface 'est (* 2 (* 3.14 r)))));
(cercle 1);

;;2
(define LA '(1
               (2
                (6)
                (7))
               (3
                (8 12)
                (9))
               (4
                (10
                 (13)
                 (14 (17)))
                (11
                 (15 16))
                (12))
               (5)));
LA;
(car LA);
(cdr LA);

(car (cdr LA));

;;3
(define fact(lambda(n)
              (if (and (>= n 0) (integer? n))
                  (fact_r n)
                  'erreur)))
(define fact_r(lambda(n)
                (if(= n 0)
                   1
                   (* n (fact_r(- n 1))))))
(fact 2);

;;4
(define som_int(lambda(n)
                 (if (and (> n 0) (integer? n))
                     (som_int_r n)
                     'erreur)))
(define som_int_r(lambda(n)
                 (if(= n 0)
                    0
                    (+ n (som_int_r(- n 1))))))
(som_int 10)

;;5
(define long(lambda(L)
              (if (null? L)
                  0
                  (+ 1 (long(cdr L))))))
(long '(1 4 7 1 (1 2 3 4)))

;;6
(define renverse(lambda(L)
                  (if (null? L)
                      ()
                      (append (renverse (cdr L))
                              (list (car L))))))
(define miroir(lambda(L)
                (if (null? L)
                    ()
                    (append (miroir (cdr L))
                            (list (if (list? (car L))
                                      (miroir (car L))
                                      (car L)))))))
(renverse '(1 2 3))
(miroir '((1 2) ((4 3) 4)))

;;7
(define carre_l(lambda (L)
               (if (null? L)
                   ()
                   (list (if (integer? (car L))
                             (carre (car L))
                             (carre_l (car L)))
                         (if (integer? (cdr L))
                             (carre (cdr L))
                             (carre_l (car L)))))))
(carre_l (1 2 3))
                             
                       