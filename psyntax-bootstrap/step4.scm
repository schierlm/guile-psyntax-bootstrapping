;;; Bootstrapping psyntax.scm for Guile
;;;
;;; STEP 4: Provide syntax-case ported from psyntax.scm
;;;
;;; Copyright (c) 2001, 2003, 2006, 2009, 2010-2020
;;;   Free Software Foundation, Inc.
;;; Copyright (c) 2021 Michael Schierl <schierlm@gmx.de>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

(s1*-define-macro (s4*-seff-bind-synvar var value rank)
  (begin (set! s3*-current-syntax-bindings
               (cons
                (cons var (cons value rank))
                s3*-current-syntax-bindings))
         #f))

(s1*-define-macro (s4*-seff-pop-synvar)
  (begin (set! s3*-current-syntax-bindings (cdr s3*-current-syntax-bindings))
         #f))

(s1*-define-macro (s4*-with-bound-synvar var value rank body)
  `(s1*-expand-with-side-effects
    (s4*-seff-bind-synvar ,var ,value ,rank)
    ,body
    (s4*-seff-pop-synvar)))

(define (datum->syntax id datum)
  (make-syntax datum '((top)) '(bare bootstrapping)))

;;;;
;; [comment copied verbatim from psyntax.scm]

;; $sc-dispatch expects an expression and a pattern.  If the expression
;; matches the pattern a list of the matching expressions for each
;; "any" is returned.  Otherwise, #f is returned.  (This use of #f will
;; not work on r4rs implementations that violate the ieee requirement
;; that #f and () be distinct.)

;; The expression is matched with the pattern as follows:

;; pattern:                           matches:
;;   ()                                 empty list
;;   any                                anything
;;   (<pattern>1 . <pattern>2)          (<pattern>1 . <pattern>2)
;;   each-any                           (any*)
;;   #(free-id <key>)                   <key> with free-identifier=?
;;   #(each <pattern>)                  (<pattern>*)
;;   #(each+ p1 (p2_1 ... p2_n) p3)      (p1* (p2_n ... p2_1) . p3)
;;   #(vector <pattern>)                (list->vector <pattern>)
;;   #(atom <object>)                   <object> with "equal?"
;;;;
(define (s4*-$sc-dispatch e p)
  (letrec
      ((match-each
        (lambda (e p wrap?)
          (cond
           ((pair? e)
            (let ((first (match (car e) p '() wrap?)))
              (and first
                   (let ((rest (match-each (cdr e) p wrap?)))
                     (and rest (cons first rest))))))
           ((null? e) '())
           ((syntax? e)
            (match-each (syntax-expression e) p #t))
           (else #f))))

       (match-each+
        (lambda (e x-pat y-pat z-pat r wrap?)
          (let f ((e e))
            (cond
             ((pair? e)
              (call-with-values (lambda () (f (cdr e)))
                (lambda (xr* y-pat r)
                  (if r
                      (if (null? y-pat)
                          (let ((xr (match (car e) x-pat '() wrap?)))
                            (if xr
                                (values (cons xr xr*) y-pat r)
                                (values #f #f #f)))
                          (values
                           '()
                           (cdr y-pat)
                           (match (car e) (car y-pat) r wrap?)))
                      (values #f #f #f)))))
             ((syntax? e)
              (f (syntax-expression e)))
             (else
              (values '() y-pat (match e z-pat r wrap?)))))))

       (match-each-any
        (lambda (e wrap?)
          (cond
           ((pair? e)
            (let ((l (match-each-any (cdr e) wrap?)))
              (and l (cons (wrap (car e) wrap?) l))))
           ((null? e) '())
           ((syntax? e)
            (match-each-any (syntax-expression e) #t))
           (else #f))))

       (match-empty
        (lambda (p r)
          (cond
           ((null? p) r)
           ((eq? p '_) r)
           ((eq? p 'any) (cons '() r))
           ((pair? p) (match-empty (car p) (match-empty (cdr p) r)))
           ((eq? p 'each-any) (cons '() r))
           ((eq? (vector-ref p 0) 'each)
            (match-empty (vector-ref p 1) r))
           ((eq? (vector-ref p 0) 'each+)
            (match-empty (vector-ref p 1)
                         (match-empty
                          (reverse (vector-ref p 2))
                          (match-empty (vector-ref p 3) r))))
           ((member (vector-ref p 0) '(free-id atom)) r)
           ((eq? (vector-ref p 0) 'vector) (match-empty (vector-ref p 1) r)))))

       (combine
        (lambda (r* r)
          (if (null? (car r*))
              r
              (cons (map car r*) (combine (map cdr r*) r)))))

       (match*
        (lambda (e p r wrap?)
          (cond
           ((null? p) (and (null? e) r))
           ((pair? p)
            (and (pair? e) (match (car e) (car p)
                                  (match (cdr e) (cdr p) r wrap?) wrap?)))
           ((eq? p 'each-any)
            (let ((l (match-each-any e wrap?))) (and l (cons l r))))
           ((eq? (vector-ref p 0) 'each)
            (if (null? e)
                (match-empty (vector-ref p 1) r)
                (let ((l (match-each e (vector-ref p 1) wrap?)))
                  (and l
                       (let collect ((l l))
                         (if (null? (car l))
                             r
                             (cons (map car l) (collect (map cdr l)))))))))
           ((eq? (vector-ref p 0) 'each+)
            (call-with-values
                (lambda ()
                  (match-each+ e (vector-ref p 1) (vector-ref p 2) (vector-ref p 3) r wrap?))
              (lambda (xr* y-pat r)
                (and r
                     (null? y-pat)
                     (if (null? xr*)
                         (match-empty (vector-ref p 1) r)
                         (combine xr* r))))))
           ((eq? (vector-ref p 0) 'free-id) (and (symbol? e) (equal? e (vector-ref p 1)) r))
           ((eq? (vector-ref p 0) 'atom) (and (syntax? e) (equal? (vector-ref p 1) (syntax-expression e)) r))
           ((eq? (vector-ref p 0) 'vector)
            (and (vector? e)
                 (match (vector->list e) (vector-ref p 1) r wrap?))))))

       (wrap
        (lambda (e wrap?)
          (cond
           ((not wrap?) e)
           ((syntax? e) e)
           ((null? e) e)
           ((pair? e) (cons (wrap (car e) wrap?) (wrap (cdr e) wrap?)))
           ((vector? e) (list->vector (wrap (vector->list e) wrap?)))
           (#t (datum->syntax #f e)))))

       (match
        (lambda (e p r wrap?)
          (cond
           ((not r) #f)
           ((eq? p '_) r)
           ((eq? p 'any) (wrap (cons e r) wrap?))
           ((syntax? e)
            (match*
             (syntax-expression e) p r #t))
           (else (match* e p r wrap?))))))
    (cond
     ((eq? p 'any) (list e))
     ((eq? p '_) '())
     ((syntax? e)
      (match* (syntax-expression e)
              p '() #t))
     (else (match* e p '() #f)))))


(define (s4*-gen-syntax-case unpacked-x unpacked-keys unpacked-clauses)
  (letrec
      ((convert-pattern
        ;; accepts pattern & keys
        ;; returns $sc-dispatch pattern & ids
        (lambda (unpacked-pattern unpacked-keys)
          (letrec (
                   (cvt*
                    (lambda (unpacked-p* n ids)
                      (cond
                       ((pair? unpacked-p*)
                        (call-with-values
                            (lambda () (cvt* (cdr unpacked-p*) n ids))
                          (lambda (y ids)
                            (call-with-values
                                (lambda () (cvt (car unpacked-p*) n ids))
                              (lambda (x ids)
                                (values (cons x y) ids))))))
                       (#t (cvt unpacked-p* n ids)))))

                   (v-reverse
                    (lambda (x)
                      (let loop ((r '()) (x x))
                        (if (not (pair? x))
                            (values r x)
                            (loop (cons (car x) r) (cdr x))))))

                   (cvt
                    (lambda (p n ids)
                      (if (symbol? p)
                          (cond
                           ((member p unpacked-keys)
                            (values (vector 'free-id p) ids))
                           ((eq? p '_)
                            (values '_ ids))
                           (else
                            (values 'any (cons (cons p n) ids))))
                          (cond
                           ((and (pair? p) (pair? (cdr p)) (null? (cdr (cdr p))) (eq? '... (car (cdr p))))
                            (call-with-values
                                (lambda () (cvt (car p) (1+ n) ids))
                              (lambda (p ids)
                                (values (if (eq? p 'any) 'each-any (vector 'each p))
                                        ids))))
                           ((and (pair? p) (pair? (cdr p)) (eq? '... (car (cdr p))))
                            (call-with-values
                                (lambda () (cvt* (cdr (cdr p)) n ids))
                              (lambda (ys ids)
                                (call-with-values
                                    (lambda () (cvt (car p) (1+ n) ids))
                                  (lambda (x ids)
                                    (call-with-values
                                        (lambda () (v-reverse ys))
                                      (lambda (ys e)
                                        (values `#(each+ ,x ,ys ,e)
                                                ids))))))))
                           ((pair? p)
                            (call-with-values
                                (lambda () (cvt (cdr p) n ids))
                              (lambda (y ids)
                                (call-with-values
                                    (lambda () (cvt (car p) n ids))
                                  (lambda (x ids)
                                    (values (cons x y) ids))))))
                           ((null? p) (values '() ids))
                           ((vector? p)
                            (call-with-values
                                (lambda () (cvt (vector->list p) n ids))
                              (lambda (p ids) (values (vector 'vector p) ids))))
                           (#t (values (vector 'atom p) ids)))))))
            (cvt unpacked-pattern 0 '()))))

       (distinct-symbols?
        (lambda (ids)
          (let distinct? ((ids ids))
            (or (null? ids)
                (and (not (member (car ids) (cdr ids)))
                     (distinct? (cdr ids)))))))

       (build-dispatch-call
        (lambda (pvars unpacked-exp y)
          (if (null? pvars)
              unpacked-exp
              (list 's4*-with-bound-synvar (car (car pvars)) (list 'car y) (cdr (car pvars))
                    (build-dispatch-call (cdr pvars) unpacked-exp (list 'cdr y))))))

       (gen-clause
        (lambda (unpacked-x unpacked-keys unpacked-clauses unpacked-pat unpacked-fender unpacked-exp)
          (call-with-values
              (lambda () (convert-pattern unpacked-pat unpacked-keys))
            (lambda (p pvars)
              (cond
               ((not (and-map (lambda (x) (not (eq? (car x) '...))) pvars))
                (s0*-syntax-error "misplaced ellipsis"))
               ((not (distinct-symbols? (map car pvars)))
                (s0*-syntax-error "duplicate pattern variable"))
               (else
                (let ((y (module-gensym "tmp-")))
                  (list
                   (list 'lambda (list y)
                         (list 'if (if (eq? unpacked-fender #t) y
                                       (list 'if y (build-dispatch-call pvars unpacked-fender y) #f))
                               (build-dispatch-call pvars unpacked-exp y)
                               (gen-syntax-case unpacked-x unpacked-keys unpacked-clauses)))
                   (if (eq? p 'any)
                       (list 'list unpacked-x)
                       `(s4*-$sc-dispatch ,unpacked-x (quote ,p)))))))))))

       (gen-syntax-case
        (lambda (unpacked-x unpacked-keys unpacked-clauses)
          (if (null? unpacked-clauses)
              '(s0*-syntax-error "source expression failed to match any pattern")
              (cond
               ((and (list? (car unpacked-clauses)) (= 2 (length (car unpacked-clauses))))
                (let ((pat (car (car unpacked-clauses)))
                      (exp (car (cdr (car unpacked-clauses)))))
                  (if (and (symbol? pat)
                           (and-map (lambda (x) (not (eq? pat x)))
                                    (cons '... unpacked-keys)))
                      (if (eq? pat '_)
                          exp
                          `(s4*-with-bound-synvar ,pat ,unpacked-x 0 ,exp))
                      (gen-clause unpacked-x unpacked-keys (cdr unpacked-clauses) pat #t exp))))
               ((and (list? (car unpacked-clauses)) (= 3 (length (car unpacked-clauses))))
                (let ((pat (car (car unpacked-clauses)))
                      (fender (car (cdr (car unpacked-clauses))))
                      (exp (car (cdr (cdr (car unpacked-clauses))))))
                  (gen-clause unpacked-x unpacked-keys (cdr unpacked-clauses)
                              pat fender exp)))
               (#t (s0*-syntax-error "invalid clause")))))))
    (gen-syntax-case unpacked-x unpacked-keys unpacked-clauses)))

(s1*-define-macro (syntax-case val keys . clauses)
  (if (and-map (lambda (x) (and (symbol? x) (not (eq? x '...)))) keys)
      (let ((x (module-gensym "tmp-")))
        `((lambda (,x) ,(s4*-gen-syntax-case x keys clauses)) ,val))
      (s0*-syntax-error "invalid literals list")))
