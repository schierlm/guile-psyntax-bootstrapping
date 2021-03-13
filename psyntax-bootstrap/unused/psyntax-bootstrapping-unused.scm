;;; Bootstrapping psyntax.scm for Guile
;;;
;;; Unused steps
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
;;; These were steps I originally ported for bootstrapping, but which
;;; ended up to be not necessary. Keep them here in case we may need
;;; them later.
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
;;; STEP 2B: Provide syntax-rules from GNU Mes                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(primitive-load "psyntax-bootstrap/unused/step2b")

;;; EXAMPLES for STEP 2B:

(s2*-define-syntax s2*-or
  (s2*-syntax-rules ()
    ((s2*-or) #f)
    ((s2*-or e) e)
    ((s2*-or e1 e ...) (let ((temp e1))
                         (if temp temp (s2*-or e ...))))))

(s2*-or)
(s2*-or #f #f #f #t #f)

(s2*-define-syntax zipfoo
  (s2*-syntax-rules ()
    ((zipfoo (b ...) (a c ...) ...)
     (list (list a b c ...) ...))))

(zipfoo (2 22 222) (1 3 4) (11 33) (111))

;;; Text expanding:
(s2*-syntax-expander
 '(zipfoo ()
          ((zipfoo (b ...) (a c ...) ...)
           (list (list a b c ...) ...)))
 (lambda (x0) x0)
 'eq?)

;;; Text returning separate args
(s2*-syntax-expander
 '(zipfoo ()
          ((zipfoo (b ...) (a c ...) ...)
           (list (list b ...) (list a ...) (list c ...) ...)))
 (lambda (x0) x0)
 'eq?)


;; hygienic tests

(s2*-define-syntax s2*-prog1-foo
  (s2*-syntax-rules ()
    ((s2*-prog1-foo first rest ...)
     (let ((foo first))
      rest ... foo))))

(let ((foo 20) (bar 22) )
  (+ (s2*-prog1-foo bar (set! bar foo)) bar))

;-> returns 44 instead of 42

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STEP 3B: Generate s3*-make-foo symbols from %expanded-vtable          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(primitive-load "psyntax-bootstrap/unused/step3b")

;; EXAMPLES for STEP 3B:

(make-const #f '(1 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEST Stuff (compare with Guile)                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; at first confusing behaviour of syntax? (does not always create syntax
;; object), but now clear to me how it works and why:

,import (system syntax)
(syntax? #'a)
(syntax-case #'((1 2)) () ((b) (syntax? #'a)))
(syntax-case #'((1 2)) () ((a) (syntax? #'a)))
(syntax-case #'((1 2)) () ((a ...) (syntax? #'a)))

;; confusing interference with lambdas:

(syntax-case #'(42) () ((a) #'a))
(syntax-case #'(42) () ((a) ((lambda (b) #'a) 0)))
(syntax-case #'(42) () ((a) ((lambda (a) #'a) 0)))
