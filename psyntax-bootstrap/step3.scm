;;; Bootstrapping psyntax.scm for Guile
;;;
;;; STEP 3: Provide 'syntax ported from psyntax.scm
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

;; will be dynamically changed while expanding syntax-case
(define s3*-current-syntax-bindings '())

;;; in this step, syntax variables #'foo in the original source are
;;; often replaced by unpacked normal symbols/expressions
;;; 'unpacked-foo, to solve the chicken-and-egg problem of creating
;;; those syntax forms without the syntax macro available.

(define (s3*-gen-syntax unpacked-e use-bindings)
  (letrec
      ((gen-syntax-unpacked
        (lambda (unpacked-e maps use-bindings)
          (cond
           ((symbol? unpacked-e)
            (let ((value (assq-ref (if use-bindings s3*-current-syntax-bindings '()) unpacked-e)))
              (if value
                  (call-with-values
                      (lambda () (gen-ref (car value) (cdr value) maps))
                    (lambda (var maps)
                      (values `(ref ,var) maps)))
                  (if (and use-bindings (eq? '... unpacked-e))
                      (s0*-syntax-error "misplaced ellipsis")
                      (values (gen-new-syntax unpacked-e) maps)))))
           ((and (pair? unpacked-e) use-bindings (eq? '... (car unpacked-e)) (pair? (cdr unpacked-e)) (eq? (cdr (cdr unpacked-e)) '()))
            (gen-syntax-unpacked (car (cdr unpacked-e)) maps #f))
           ((and (pair? unpacked-e) (pair? (cdr unpacked-e)) use-bindings (eq? '... (car (cdr unpacked-e))))
            (let f ((y (cdr (cdr unpacked-e)))
                    (k (lambda (maps)
                         (call-with-values
                             (lambda ()
                               (gen-syntax-unpacked (car unpacked-e) (cons '() maps) use-bindings))
                           (lambda (x maps)
                             (if (null? (car maps))
                                 (s0*-syntax-error "extra ellipsis")
                                 (values (gen-map x (car maps))
                                         (cdr maps))))))))
              (cond
               ((and (pair? y) use-bindings (eq? '... (car y)))
                (f (cdr y)
                   (lambda (maps)
                     (call-with-values
                         (lambda () (k (cons '() maps)))
                       (lambda (x maps)
                         (if (null? (car maps))
                             (s0*-syntax-error "extra ellipsis")
                             (values (gen-mappend x (car maps))
                                     (cdr maps))))))))
               (#t (call-with-values
                       (lambda () (gen-syntax-unpacked y maps use-bindings))
                     (lambda (y maps)
                       (call-with-values
                           (lambda () (k maps))
                         (lambda (x maps)
                           (values (list 'append x y) maps)))))))))
           ((pair? unpacked-e)
            (call-with-values
                (lambda () (gen-syntax-unpacked (car unpacked-e) maps use-bindings))
              (lambda (x maps)
                (call-with-values
                    (lambda () (gen-syntax-unpacked (cdr unpacked-e) maps use-bindings))
                  (lambda (y maps) (values (list 'cons x y) maps))))))
           ((vector? unpacked-e)
            (call-with-values
                (lambda ()
                  (gen-syntax-unpacked (vector->list unpacked-e) maps use-bindings))
              (lambda (e maps) (values (list 'list->vector e) maps))))
           ((null? unpacked-e)
            (values (list 'quote ()) maps))
           (#t (values (gen-new-syntax unpacked-e) maps)))))

       (gen-new-syntax
        (lambda (unpacked-e)
          `(make-syntax ',unpacked-e '((top)) '(bare bootstrapping))))

       (gen-ref
        (lambda (var level maps)
          (if (= level 0)
              (values var maps)
              (if (null? maps)
                  (begin (display (list var level maps)) (s0*-syntax-error "missing ellipsis"))
                  (call-with-values
                      (lambda () (gen-ref var (1- level) (cdr maps)))
                    (lambda (outer-var outer-maps)
                      (let ((b (assq outer-var (car maps))))
                        (if b
                            (values (cdr b) maps)
                            (let ((inner-var (module-gensym "tmp-")))
                              (values inner-var
                                      (cons (cons (cons outer-var inner-var)
                                                  (car maps))
                                            outer-maps)))))))))))

       (gen-mappend
        (lambda (e map-env)
          `(apply append ,(gen-map e map-env))))

       (remove-refs
        (lambda (e)
          (cond
           ((pair? e)
            (if (and (eq? (car e) 'ref) (pair? (cdr e)) (null? (cdr (cdr e))))
                (car (cdr e))
                (cons (remove-refs (car e)) (remove-refs (cdr e)))))
           ((vector? e)
            (list->vector (remove-refs (vector->list e))))
           (#t e))))

       (gen-map
        (lambda (e map-env)
          (let ((formals (map cdr map-env))
                (actuals (map (lambda (x) `(ref ,(car x))) map-env)))
            (cond
             ((eq? (car e) 'ref)
              ;; identity map equivalence:
              ;; (map (lambda (x) x) y) == y
              (car actuals))
             ((and-map
               (lambda (x) (and (eq? (car x) 'ref) (memq (cadr x) formals)))
               (cdr e))
              ;; eta map equivalence:
              ;; (map (lambda (x ...) (f x ...)) y ...) == (map f y ...)
              `(map ,(car e)
                    ,@(map (let ((r (map cons formals actuals)))
                             (lambda (x) (cdr (assq (cadr x) r))))
                           (cdr e))))
             (else `(map (lambda ,formals ,e) ,@actuals)))))))

    (remove-refs (gen-syntax-unpacked unpacked-e '() use-bindings))))

(s1*-define-macro (syntax x)
  (s3*-gen-syntax x #t))
