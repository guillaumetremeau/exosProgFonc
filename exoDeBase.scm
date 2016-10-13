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
