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



;;miroir rt
(define (miroirrt L)
  (if (null? L)
      ()
      (miroir (cdr L




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
                             
;;mapkar

(define (mapkar f L)
  (if (null? L)
      ()
      (cons (f (car L))
            (mapkar f (cdr L)))))

(mapkar carre '(1 3 5 3 12))

(mapkar (lambda (x)(+ x 1)) '(1 5 6))


;; fibo
(define (fibo n)
  (if (or (= n 0) (= n 1))
      1
      (+ (fibo (- n 1)) (fibo (- n 2)))))

(fibo 5)
(fibo 10)
(fibo 20)

(define (fibol L)
  (if (= n 0)
      '(1 0)
      (let (res (fibol (- n 1)
  (const
   (+ (car L) (car (cdr L)))
   L))
(define (fibo2 n)
  (if (= n 0)
      '(1 0)
      (fibo1

(define (fibort n a b)
  (if (= n 0)
      b
      (fibort (- n 1) (+ a b) a)))
(define (fibo4 n)
  (fibort n 1 0))

(fibo4 100000)

;; niv0
(define (niv0 L)
  (cond ((null? L) ())
        ((list? (car L))
         (append (niv0 (car L))
                 (niv0 (cdr L)))
         (else (cons (car L)
                     (niv0 (cdr L)))))))

(niv0 '(1 (23 3) (4 (5 6)) 2))


;; tri_inser
(define (inserer x L)
  (if (null? L)
      (list x)
      (if (> x (car L))
          (cons (car L)
                (inserer x (cdr L)))
          (cons x L))))
(define (tri_ins L)
  (if (null? L)
      ()
      (inserer (car L)
               (tri_ins (cdr L)))))
(tri_ins '(2 86 5 4 12 33 5))


(define (inserer_nom x L)
  (if (null? L)
      (list x)
      (if (string>? (car x) (car (car L)))
          (cons (car L)
                (inserer_nom x (cdr L)))
          (cons x L))))
(define (tri_nom L)
  (if (null? L)
      ()
      (inserer_nom (car L)
                   (tri_nom(cdr L)))))
(tri_nom '((paul 150 60)
           (eric 170 50)
           (fabien 160 80)
           (jack 175 95)))