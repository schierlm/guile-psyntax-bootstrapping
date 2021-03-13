;;; Bootstrapping psyntax.scm for Guile
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STEP 1: Define s1*-define-macro and with Guile primitives (and        ;;;
;;;         procedures that are available  before psyntax is loaded)      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(primitive-load-path "psyntax-bootstrap/step1")

;;; EXAMPLES of STEP 1

(s1*-define-macro (s1*-foo a b) (list '+ -2 (1+ a) (1+ b)))

(* 2 (s1*-foo 10 11))

's1*-foo

'(s1*-foo 1 2)

(define (s1*-bar x) (+ x (s1*-foo 16 16)))
(s1*-bar 10)

(apply (lambda (x) (+ x (s1*-foo 16 16))) '(10))

(define s1*-const (s1*-foo 20 21))
s1*-const

(let ((x 9) (s1*-foo 3)) s1*-foo)
(letrec* ((s1*-foo 3)) s1*-foo)

(let ((x 10)) (+ x (s1*-foo 20 12)))

(let ((y (s1*-foo 10 11))) (+ y y))

;; Experiments about evaluation order

(define (second2 a b c) b)

(s1*-define-macro (dump x y) (begin (display x) (newline) y))

(second2 (dump 1 (dump 2 #f)) (+ (dump 3 20) (dump 4 22)) (dump 5 #f))

(s1*-expand-with-side-effects (dump 1 #f) (dump 2 42) (dump 3 #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STEP 2: Provide quasiquote from GNU Mes                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(primitive-load-path "psyntax-bootstrap/step2")

;;; EXAMPLES for STEP 2:

(define s2*-a 2)
`(1 ,s2*-a 3 ,(+ 2 2) ,@(list 5 6) 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STEP 3: Provide 'syntax ported from psyntax.scm                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(primitive-load-path "psyntax-bootstrap/step3")

;;; EXAMPLES for STEP 3:

;; NOTE: print-syntax is not yet loaded, you'll need to patch print.c to be able to display those!

(set! s3*-current-syntax-bindings '((foo1 (syntax 99) . 0) (foo2 (syntax (11 22)) . 1) (foo3 (syntax ((11 12 13) (21 22 23) (31 32 33))) . 2)))
(syntax 99)
(syntax (a 99 b))
#'foo
#'foo1
#'(foo2 ...)
#'(... ...)
#'((foo2 . foo2) ...)
#'((((foo3 . foo3) ...) . (foo3 ...)) ...)
#'(foo 99 bar)
#'((2 22 222) (1 3 4) (11 33) (111))

(set! s3*-current-syntax-bindings '((a (syntax (1 11 111)) . 1) (b (syntax (2 22 222)) . 1) (c (syntax ((3 4) (33) ())) . 2)))
#'((a b c ...) ...)
#'((1 2 3 4) (11 22 33) (111 222))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STEP 4: Provide syntax-case ported from psyntax.scm                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(primitive-load-path "psyntax-bootstrap/step4")

;;; EXAMPLES for STEP 4:

(syntax-case #'(1 2 3) () ((b a ...) #'(a ...)))

(syntax-case #'(2 3 4) () (a #'(1 . a)))

(syntax-case #'((2 22 222) (1 3 4) (11 33) (111)) ()
 (((b ...) (a c ...) ...)
   #'((a b c ...) ...)))

;;; should be equal? to
#'((1 2 3 4) (11 22 33) (111 222))

(syntax-case #'(bar 99) (foo bar baz)
  ((foo x) #'(x x))
  ((bar x) #'(x x x))
  ((baz x) #'(x x x x))
  ((z x) #'(z z x)))

;;; should be equal? to
#'(99 99 99)

;;; test with multiple values
(call-with-values
    (lambda () (syntax-case #'(have 42 23) ()
                   ((have a b) (values #'(a b) #'a #'b))))
  (lambda args args))

;;; should be equal? to
#'((42 23) 42 23)

;;; test splitting expressions
(syntax-case (datum->syntax #'foo '(bar baz)) () ((x y) #'y))

;;; should be equal? to
#'baz

;;; test splitting expressions
(syntax-case '(bar baz) () ((x y) #'y))

;;; should be equal? to
'baz

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STEP 5: Provide define-syntax definition                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(primitive-load-path "psyntax-bootstrap/step5")

;;; EXAMPLES for STEP 5:

(syntax->datum #'(a b 3 "Hello" #(x y z)))

(define-syntax s5*-or
  (lambda (x)
  (syntax-case x ()
    ((s5*-or) #'#f)
    ((s5*-or e) #'e)
    ((s5*-or e1 e ...) #'(let ((temp e1))
                         (if temp temp (s5*-or e ...)))))))

(s5*-or)
(s5*-or #f #f #f #t #f)

(define-syntax zipfoo
  (lambda (x)
  (syntax-case x ()
    ((zipfoo (b ...) (a c ...) ...)
     #'(list (list a b c ...) ...)))))

(zipfoo (2 22 222) (1 3 4) (11 33) (111))


(s5*-letrec-defines ()
  (define value 42)
  (define (square x) (* x x))
  (square value))

(define-syntax the-answer (identifier-syntax '((4 2) 42 (4 2))))

the-answer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STEP 6: Provide with-syntax, syntax-rules and quasisyntax             ;;;
;;;         ported from psyntax.scm and quasisyntax.scm                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(primitive-load-path "psyntax-bootstrap/step6")

;;; EXAMPLES for STEP 6:

(with-syntax () #'(a b))
(with-syntax ((a 1) (b 2)) #'(a b))
(with-syntax ((a 1) ((b ...) '(2 3 4))) #'(a b ...))
(with-syntax (((b ...) '(2 3 4)) (a 1)) #'(a b ...))

(with-syntax ((a 1)) (with-syntax (((b ...) '(2 3 4))) #'(a b ...)))
(with-syntax (((b ...) '(2 3 4))) (with-syntax ((a 1)) #'(a b ...)))

(define-syntax foo
  (syntax-rules ()
    ((foo (a c) b)
     (list (list a b c)))))

(foo (1 3) 2)

(define-syntax s6*-or
  (syntax-rules ()
    ((s6*-or) #f)
    ((s6*-or e) e)
    ((s6*-or e1 e ...) (let ((temp e1))
                         (if temp temp (s6*-or e ...))))))

(s6*-or)
(s6*-or #f #f #f #t #f)

(define-syntax zipfoo
  (syntax-rules ()
    ((zipfoo (b ...) (a c ...) ...)
     (list (list a b c ...) ...))))

(zipfoo (2 22 222) (1 3 4) (11 33) (111))

(define-syntax-rule (zipfoo (b ...) (a c ...) ...)
  (list (list a b c ...) ...))

(zipfoo (2 22 222) (1 3 4) (11 33) (111))

#`(foo #,(+ 2 9) bar)

#`(foo #,@(list (+ 20 22) (+ 10 13)) bar)

(define-syntax test-ellipses-over-unsyntax
  (lambda (e)
    (let ((a (syntax a)))
      (with-syntax (((b ...) #'(1 2 3)))
                   #`(quote ((b #,a) ...))))))

(test-ellipses-over-unsyntax)

;; should be
'((1 a) (2 a) (3 a))

;; hygienic tests

(define-syntax prog1-foo
  (syntax-rules ()
    ((prog1-foo first rest ...)
     (let ((foo first))
      rest ... foo))))

(let ((foo 20) (bar 22) )
  (+ (prog1-foo bar (set! bar foo)) bar))

;; -> returns 44 instead of 42 as it is not hygienic yet

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STEP 7: load various functions from boot-9.scm                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(primitive-load-path "psyntax-bootstrap/step7")

;; NO EXAMPLES

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STEP 8: load patched version of psyntax.scm and load step 7 again     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; psyntax-patched.scm is built from psyntax.scm by applying stage2.patch

(primitive-load-path "ice-9/psyntax-patched")
(primitive-load-path "psyntax-bootstrap/step7")

;;; EXAMPLES for STEP 8:

(define s8*-a 2)
`(1 ,s8*-a 3 ,(+ 2 2) ,@(list 5 6) 7)

(syntax 99)
(syntax (a 99 b))
#'foo
#'foo1
#'(... ...)
#'(foo 99 bar)

(syntax-case #'(1 2 3) () ((b a ...) #'(a ...)))
(syntax-case #'(2 3 4) () (a #'(1 . a)))

(syntax-case #'((2 22 222) (1 3 4) (11 33) (111)) ()
 (((b ...) (a c ...) ...)
   #'((a b c ...) ...)))

;;; should be equal? to
#'((1 2 3 4) (11 22 33) (111 222))

(syntax-case #'(bar 99) (foo bar baz)
  ((foo x) #'(x x))
  ((bar x) #'(x x x))
  ((baz x) #'(x x x x))
  ((z x) #'(z z x)))

;;; should be equal? to
#'(99 99 99)

;;; test with multiple values
(call-with-values
    (lambda () (syntax-case #'(have 42 23) ()
                   ((have a b) (values #'(a b) #'a #'b))))
  (lambda args args))

;;; should be equal? to
#'((42 23) 42 23)

;;; test splitting expressions
(syntax-case (datum->syntax #'foo '(bar baz)) () ((x y) #'y))

;;; should be equal? to
#'baz

;;; test splitting expressions
(syntax-case '(bar baz) () ((x y) #'y))

;;; should be equal? to
'baz

(syntax->datum #'(a b 3 "Hello" #(x y z)))

(define-syntax s8*-or
  (lambda (x)
  (syntax-case x ()
    ((s8*-or) #'#f)
    ((s8*-or e) #'e)
    ((s8*-or e1 e ...) #'(let ((temp e1))
                         (if temp temp (s8*-or e ...)))))))

(s8*-or)
(s8*-or #f #f #f #t #f)

(define-syntax zipfoo
  (lambda (x)
  (syntax-case x ()
    ((zipfoo (b ...) (a c ...) ...)
     #'(list (list a b c ...) ...)))))

(zipfoo (2 22 222) (1 3 4) (11 33) (111))

(define-syntax the-answer (identifier-syntax '((4 2) 42 (4 2))))

the-answer

(with-syntax () #'(a b))
(with-syntax ((a 1) (b 2)) #'(a b))
(with-syntax ((a 1) ((b ...) '(2 3 4))) #'(a b ...))
(with-syntax (((b ...) '(2 3 4)) (a 1)) #'(a b ...))

(with-syntax ((a 1)) (with-syntax (((b ...) '(2 3 4))) #'(a b ...)))
(with-syntax (((b ...) '(2 3 4))) (with-syntax ((a 1)) #'(a b ...)))

(define-syntax foo
  (syntax-rules ()
    ((foo (a c) b)
     (list (list a b c)))))

(foo (1 3) 2)

(define-syntax s8*-or
  (syntax-rules ()
    ((s8*-or) #f)
    ((s8*-or e) e)
    ((s8*-or e1 e ...) (let ((temp e1))
                         (if temp temp (s8*-or e ...))))))

(s8*-or)
(s8*-or #f #f #f #t #f)

(define-syntax zipfoo
  (syntax-rules ()
    ((zipfoo (b ...) (a c ...) ...)
     (list (list a b c ...) ...))))

(zipfoo (2 22 222) (1 3 4) (11 33) (111))

(define-syntax-rule (zipfoo (b ...) (a c ...) ...)
  (list (list a b c ...) ...))

(zipfoo (2 22 222) (1 3 4) (11 33) (111))

#`(foo #,(+ 2 9) bar)

#`(foo #,@(list (+ 20 22) (+ 10 13)) bar)

(define-syntax test-ellipses-over-unsyntax
  (lambda (e)
    (let ((a (syntax a)))
      (with-syntax (((b ...) #'(1 2 3)))
                   #`(quote ((b #,a) ...))))))

(test-ellipses-over-unsyntax)

;; should be
'((1 a) (2 a) (3 a))

;; hygienic tests

(define-syntax prog1-foo
  (syntax-rules ()
    ((prog1-foo first rest ...)
     (let ((foo first))
      rest ... foo))))

(let ((foo 20) (bar 22) )
  (+ (prog1-foo bar (set! bar foo)) bar))

;;-> returns 42 now

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STEP 9: load unpatched version of psyntax.scm                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(primitive-load-path "ice-9/psyntax")

;;; EXAMPLES ARE SAME AS FOR STEP 7

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BOOTSTRAP COMPLETE                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
