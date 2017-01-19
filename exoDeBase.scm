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

;;;4
(define som_int(lambda(n)
                 (if (and (> n 0) (integer? n))
                     (som_int_r n)
                     'erreur)))
(define som_int_r(lambda(n)
                 (if(= n 0)
                    0
                    (+ n (som_int_r(- n 1))))))
(som_int 10)

;;;5
(define long(lambda(L)
              (if (null? L)
                  0
                  (+ 1 (long(cdr L))))))
(long '(1 4 7 1 (1 2 3 4)))

;;;6
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



;;;miroir rt
(define (miroirrt L)
  (if (null? L)
      ()
      (miroir (cdr L




;;;7
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
                             
;;;mapkar

(define (mapkar f L)
  (if (null? L)
      ()
      (cons (f (car L))
            (mapkar f (cdr L)))))

(mapkar carre '(1 3 5 3 12))

(mapkar (lambda (x)(+ x 1)) '(1 5 6))


;;; fibo
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


;;; tri_inser
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

(define Selection (lambda (P L)
                    (if (null? L) #t
                        (and (P (car L))
                             (Selection P (cdr L)))
                        )))
(Selection integer? '(1 3 5 ))

(define (comp f g)
  (lambda (x)
    (f (g x))))
((comp fibo4 carre) 5)

(define (derive f)
  (lambda (h)
    (lambda (x)
      (/
       (-
        (f (+ x h))
        (f (- x h))
        (* 2 h))))))
(((derive carre) 0.1) 2)

(define (zip L1 L2)
  (map list L1 L2))
(zip '(1 5 3.2) '(2 4 7))

;;;Exercice 6
(define (trace M)
  (if (null? M)
      0
      (+
       (caar M)
       (trace (map cdr (cdr M)))
       )))

(trace '((1 2 3) (2 1 2) (3 2 1)))

(define (transp M)
  (if (null? (car M))
      ()
      (cons
       (map car M)
       (transp (map cdr M))
       )))
(transp '((11 21 31) (12 22 32) (13 23 33)))

(define (transp2 M)
  (apply map list M))

(transp2 '((1 2 3)(4 5 6)(7 8 9)))

(define (MV M V)
  (if (null?  M)
      ()
      (cons
       (apply + (map * (car M) V))
       (MV (cdr M) V))))

(MV '((1 2 3)(3 1 2)(2 3 1)) '(1 2 3))

(define (AL M)
  (lambda (V) (MV M V)))

((AL '((1 2 3)(4 5 6)(7 8 9))) '(1 1 1))


;;;Exercice 8
(define (P E)
  (if (null? E)
      '(())
      (append (P (cdr E))
            (map (lambda (x) (cons (car E) x)) (P (cdr E))))))

(P '(1 2 3))

;;;Exercice 4
(map list '(0 1))

(define (addFirst E1 E2)
  (if (null? E1)
  ()
  (append (map (lambda (x) (cons (car E1) x)) E2) (addFirst (cdr E1) E2))))
(addFirst '(0 1) '((0) (1)))
(define (PC E n)
  (if (= 0 n)
      '(())
      (addFirst E (PC E (- n 1)))))
(PC '(0 1) 3)

;;;Exercice 1
(define (quel-que-soit? L P)
  (if (null? L) #t
      (and (P (car L))
           (quel-que-soit? (cdr L) P))))
(quel-que-soit? '(1 5 6 4) integer?)

(define (existe? L P)
  (if (null? L) #f
      (or (P (car L))
          (existe? (cdr L) P))))
(existe? '(1.1 1.5) integer?)

(define (tous-egaux-u? L)
  (quel-que-soit? L (lambda (x) (= (car L) x))))
(tous-egaux-u? '(1 1 1))

(define (tous-egaux-e? L)
  (not (existe?  L (lambda (x) (not (= (car L) x))))))
(tous-egaux-e? '(1 1 0))
(tous-egaux-e? '())

(define (tous-diff-u? L)
  (if (or (null? L)(null? (cdr L))) #t
      (and (quel-que-soit? (cdr L) (lambda (x) (not (= (car L) x))))
       (tous-diff-u? (cdr L)))))
(tous-diff-u? '(1 2 2 3))

;;;Exerice 2

(define (rec f n B)
  (if (= 0 n)
      B
      (f (rec f n-1 B) n)))

;;;Exercice 10
;;Question 1
  ;;a)
(define fct
  (lambda (d a)
    (lambda (x)
      (annexe d a x))))

(define (annexe d a x)
  (if (equal? x (car d))
      (car a)
      (annexe (cdr d) (cdr a) x)))
(annexe '(a b c) '(1 2 3) 'a)
((fct '(a b c) '(1 2 3)) 'a)

  ;;b)
(define (LF E F)
  (map (lambda (z) (fct E z)) (PC F (long E))))

;;Question 2
  ;;a)
(define (SR L B C)
  (if (null? L)
      B
      (C (car L) (SR (cdr L) B C))))
(define (filtrer E P)
  (SR E () (lambda (x y) (if (P x) (cons x y) y))))
(filtrer '(1 a 2 b) integer?)

  ;;b)
(define (qqs? L P)
  (if (null? L)
      #t
      (and (P (car L)) (qqs? (cdr L) P))))
(define (stabilise? f P)
  (qqs? P (lambda (x) (member (f x) P))))
(stabilise? (lambda (x) (- 1 x)) '(0 1))
(stabilise? (lambda (x) (- 1 x)) '(0 1 2))

(define (LFS E P)
  (filtrer (LF E E) (lambda (x) (stabilise? x P))))
(map (lambda (app) (map (lambda (X) (app X)) '(0 1 2))) (LFS '(0 1 2) '(0 1)))
      
;;;Exercice 5
(define (chemins deb but G)
  (parcours deb but '() G))
(define (noeud s G)
  (if (= s (car (car G)))
      (car G)
      (noeud s (cdr G))))
(define (suivant noeud)
  (cdr noeud))
(define (parcours deb but dp G)
  (addFirst deb (append-map (lambda (x) (parcours x but (cons x dp) G))
                            (filtrer (suivant (noeud deb G)) (lambda (x) (not (member x dp)))))))
(addFirst '(d) '((a b) () (c)))

     

(chemins '(A) '(B) '((A (B C)) (C (B)) (B (A))))