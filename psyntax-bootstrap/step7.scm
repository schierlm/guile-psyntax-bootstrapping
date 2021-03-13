;;; Bootstrapping psyntax.scm for Guile
;;;
;;; STEP 7: load various functions from boot-9.scm
;;;
;;; Copyright (C) 1995-2014, 2016-2020  Free Software Foundation, Inc.
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

(define (symbol-append . args)
  (string->symbol (apply string-append (map symbol->string args))))

(define-syntax case
  (lambda (whole-expr)
    (s5*-letrec-defines ()
      (define (fold f seed xs)
        (let loop ((xs xs) (seed seed))
          (if (null? xs) seed
              (loop (cdr xs) (f (car xs) seed)))))
      (define (fold2 f a b xs)
        (let loop ((xs xs) (a a) (b b))
          (if (null? xs) (values a b)
              (call-with-values
                  (lambda () (f (car xs) a b))
                (lambda (a b)
                  (loop (cdr xs) a b))))))
      (define (reverse-map-with-seed f seed xs)
        (fold2 (lambda (x ys seed)
                 (call-with-values
                     (lambda () (f x seed))
                   (lambda (y seed)
                     (values (cons y ys) seed))))
               '() seed xs))
      (syntax-case whole-expr ()
        ((_ expr clause clauses ...)
         (with-syntax ((key #'key))
                      #`(let ((key expr))
                          #,@(fold
                              (lambda (clause-builder tail)
                                (clause-builder tail))
                              #'()
                              (reverse-map-with-seed
                               (lambda (clause seen)
                                 (s5*-letrec-defines ()
                                   (define (bad-clause msg)
                                     (s0*-syntax-error msg))
                                   (syntax-case clause ()
                                     ((test . rest)
                                      (with-syntax
                                       ((clause-expr
                                         (syntax-case #'rest (=>)
                                           ((=> receiver) #'(receiver key))
                                           ((=> receiver ...)
                                            (bad-clause
                                             "wrong number of receiver expressions"))
                                           ((e e* ...) #'(begin e e* ...))
                                           (_ (bad-clause "invalid clause")))))
                                       (syntax-case #'test (else)
                                         ((datums ...)
                                          (let ((seen
                                                 (fold
                                                  (lambda (datum seen)
                                                    (cons datum seen))
                                                  seen
                                                  (map syntax->datum #'(datums ...)))))
                                            (values (lambda (tail)
                                                      #`((if (memv key '(datums ...))
                                                             clause-expr
                                                             #,@tail))) seen)))
                                         (else (values (lambda (tail)
                                                         (if (null? tail)
                                                             #'(clause-expr)
                                                             (bad-clause
                                                              "else must be the last clause")))
                                                       seen))
                                         (_ (bad-clause "invalid clause")))))
                                     (_ (bad-clause "invalid clause")))))
                               '() #'(clause clauses ...))))))))))

(define (list-index l k)
  (let loop ((n 0)
             (l l))
    (and (not (null? l))
         (if (eq? (car l) k)
             n
             (loop (+ n 1) (cdr l))))))

(define-syntax-rule (when test stmt stmt* ...)
  (if test (begin stmt stmt* ...)))

(define-syntax-rule (unless test stmt stmt* ...)
  (if (not test) (begin stmt stmt* ...)))

(define-syntax with-fluids
  (lambda (stx)
   (s5*-letrec-defines ()
    (define (emit-with-fluids bindings body)
      (syntax-case bindings ()
        (()
         body)
        (((f v) . bindings)
         #`(with-fluid* f v
             (lambda ()
               #,(emit-with-fluids #'bindings body))))))
    (syntax-case stx ()
      ((_ ((fluid val) ...) exp exp* ...)
       (with-syntax (((fluid-tmp ...) (generate-temporaries #'(fluid ...)))
                     ((val-tmp ...) (generate-temporaries #'(val ...))))
         #`(let ((fluid-tmp fluid) ...)
             (let ((val-tmp val) ...)
               #,(emit-with-fluids #'((fluid-tmp val-tmp) ...)
                                   #'(begin exp exp* ...))))))))))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
         command ...)
     (letrec
       ((loop
         (lambda (var ...)
           (if test
               (begin
                 (if #f #f)
                 expr ...)
               (begin
                 command
                 ...
                 (loop (do "step" var step ...)
                       ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))
