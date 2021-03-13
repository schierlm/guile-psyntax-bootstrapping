;;; Bootstrapping psyntax.scm for Guile
;;;
;;; STEP 1: Define s1*-define-macro and with Guile primitives (and
;;;         procedures that are available  before psyntax is loaded)
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


;;; s1*-define-macro is supposed to behave like Lisp define-macro does.
;;; Define it by creating a syntax transformer that creates syntax
;;; transformers for the newly defined macros
(define s1*-define-macro
  (make-syntax-transformer
   's1*-define-macro 's1*macro
   (lambda (_ arglist body)
     (list 'define (car arglist)
           (list 'make-syntax-transformer
                 (list 'quote (car arglist))
                 (list 'quote 's1*macro)
                 (list 'lambda (cons 's1*_ (cdr arglist)) body))))))

;;; s1*-subexpand is used to expand macros inside 'expr.
;;; 'where specifies the context: can be 'top 'args or 'let-args,
;;; everything that is not our macros will remain unchanged.
;;; This is defined in one function instead of multiple mutually recursively
;;; functions, as we cannot (reliably) reference functions defined later
;;; at this point, and letrec* would not make it easier to read.
(define (s1*-subexpand expr where)
  (cond
   ((not (pair? expr))
    ;;; no need to expand anything if not a pair
    expr)
   ((eq? where 'top)
    ;;; On toplevel, we have to examine the car in case it is a symbol
    ;;; also we need to expand arguments recursively
    (cond
     ;; we need to special case some forms (quote lambda define let letrec letrec*)
     ((eq? 'quote (car expr))
      expr)
     ((or (eq? 'lambda (car expr)) (eq? 'define (car expr)))
      (cons (car expr) (cons (car (cdr expr)) (s1*-subexpand (cdr (cdr expr)) 'args))))
     ((or (eq? 'let (car expr)) (eq? 'let* (car expr)) (eq? 'letrec (car expr)) (eq? 'letrec* (car expr)))
      (cons (car expr) (cons (s1*-subexpand (car (cdr expr)) 'let-args)
                             (s1*-subexpand (cdr (cdr expr)) 'args))))
     ;;; this is the general case
     ((and (symbol? (car expr)) (defined? (car expr)))
      (let ((proc (eval (car expr) (current-module))))
        (if (and (macro? proc) (eq? 's1*macro (macro-type proc)))
            (s1*-subexpand (apply (macro-transformer proc) expr) 'top)
            (cons (car expr)
                  (s1*-subexpand (cdr expr) 'args)))))
     (#t
      (cons (s1*-subexpand (car expr) 'top)
            (s1*-subexpand (cdr expr) 'args)))))
   ((eq? where 'args)
    ;;; when expanding args, we expand the car as toplevel, and the cdr as args
    (cons (s1*-subexpand (car expr) 'top)
          (s1*-subexpand (cdr expr) 'args)))
   ((eq? where 'let-args)
    ;;; each element is a list in the form (binding value)
    (cons
     (if (pair? (car expr))
         (list (car (car expr))
               (s1*-subexpand (car (cdr (car expr))) 'top))
         expr)
     (s1*-subexpand (cdr expr) 'let-args)))
   (#t
     ;;; should not happen
    expr)))

;;; Create a copy of the old macro expander hook
(define s1*-old-macroexpand macroexpand)

;;; Hook our own macro expander, which will first expand our macros
;;; and then call the original expander
(define (macroexpand expr) (s1*-old-macroexpand (s1*-subexpand expr 'top)))

;;;s1*-expand-with-side-effects is used to expand a body macro
;;; after another macro has been expanded (for its side effect),
;;; and before another macro will be expanded. The return
;;; value is the value of the body macro.
;;; Note that body may return multiple values which may not get lost.
;;; As our macro expander expands strictly from left to right,
;;; this does not need any special treatment:
(s1*-define-macro (s1*-expand-with-side-effects seff1 body seff2)
  (list '(lambda (a b . c) (apply values b))
        seff1
        (list 'call-with-values (list 'lambda '() body) '(lambda rest rest))
        seff2))
