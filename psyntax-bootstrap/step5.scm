;;; Bootstrapping psyntax.scm for Guile
;;;
;;; STEP 5: Provide define-syntax definition
;;;
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
;;;
;;; Alternatively, at your option you may choose to treat this file (!)
;;; licensed under MIT License:
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.

(define (syntax->datum e)
  (cond
   ((vector? e)
    (list->vector (syntax->datum (vector->list e))))
   ((pair? e)
    (cons (syntax->datum (car e)) (syntax->datum (cdr e))))
   ((syntax? e)
    (syntax-expression e))
   (#t e)))

(define (throw . args)
  (raise-exception args))

(s1*-define-macro (s5*-build-syntax x)
  (s3*-gen-syntax x #f))

;; we need to special-case the construct
;; (define-syntax foo (identiier-syntax bar))
;; to mean (define foo bar)
;; as our defmacro-style expander will not expand identifiers

(s1*-define-macro (define-syntax name transformer)
  (if (and (pair? transformer) (pair? (cdr transformer)) (null? (cdr (cdr transformer))) (eq? (car transformer) 'identifier-syntax))
      `(define ,name ,(car (cdr transformer)))
      `(s1*-define-macro (,name . body)
         (eval (list 'syntax->datum (list (list 'quote ,transformer)
                                          (list 's5*-build-syntax (cons ',name body))))
               '(current-module)))))

;;; s5*-letrec-defines will convert nested defines inside letrec
;;; into letrec clauses. Defines inside a lambda are not supported
;;; by bootstrap guile.
(s1*-define-macro (s5*-letrec-defines lst . rest)
  (if (and (pair? rest) (pair? (car rest)) (eq? (car (car rest)) 'define))
      (if (and (pair? (cdr (car rest))) (pair? (car (cdr (car rest)))))
          `(s5*-letrec-defines ,(append lst `((,(car (car (cdr (car rest))))
                                               (lambda ,(cdr (car (cdr (car rest))))
                                                 ,@(cdr (cdr (car rest))))))) ,@(cdr rest))
          `(s5*-letrec-defines ,(append lst `(,(cdr (car rest)))) ,@(cdr rest)))
      `(letrec ,lst ,@rest)))
