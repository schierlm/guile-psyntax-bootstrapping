;;; Bootstrapping psyntax.scm for Guile
;;;
;;; STEP 2: Provide quasiquote from GNU Mes
;;;
;;; Copyright (c) 2016, 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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


(define (s2*-quasiquote-expand x)
  (cond ((vector? x) (list 'list->vector (s2*-quasiquote-expand (vector->list x))))
        ((not (pair? x)) (list 'quote x))
        ((eq? (car x) 'quasiquote)
         (s2*-quasiquote-expand (s2*-quasiquote-expand
                                 (if (null? (cddr x)) (cadr x)
                                     (cons 'list (cdr x))))))
        ((eq? (car x) 'unquote) (if (null? (cddr x)) (cadr x)
                                    (cons 'list (cdr x))))
        ((and (pair? (car x)) (eq? (caar x) 'unquote-splicing))
         ((lambda (d)
            (if (null? (cddar x)) (list 'append (cadar x) d)
                (list 'quote (append (cdar x) d))))
          (s2*-quasiquote-expand (cdr x))))
        (#t ((lambda (a d)
               (if (pair? d)
                   (if (eq? (car d) 'quote)
                       (if (and (pair? a) (eq? (car a) 'quote))
                           (list 'quote (cons (cadr a) (cadr d)))
                           (if (null? (cadr d))
                               (list 'list a)
                               (list 'cons* a d)))
                       (if (memq (car d) '(list cons*))
                           (cons (car d) (cons a (cdr d)))
                           (list 'cons* a d)))
                   (list 'cons* a d)))
             (s2*-quasiquote-expand (car x))
             (s2*-quasiquote-expand (cdr x))))))

(s1*-define-macro (quasiquote x) (s2*-quasiquote-expand x))
