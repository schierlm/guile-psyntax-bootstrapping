;;; Bootstrapping psyntax.scm for Guile
;;;
;;; STEP 6: Provide with-syntax, syntax-rules and quasisyntax
;;;         ported from psyntax.scm and quasisyntax.scm
;;;
;;; Copyright (c) 2001, 2003, 2006, 2009, 2010-2020
;;;   Free Software Foundation, Inc.
;;; Copyright (c) 2006 Andre van Tonder
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

(define-syntax with-syntax
  (lambda (x)
    (syntax-case x ()
      ((_ () e1 e2 ...)
       #'(let () e1 e2 ...))
     ((_ ((out in)) e1 e2 ...)
      #'(syntax-case in ()
          (out (let () e1 e2 ...))))
     ((_ ((out in) ...) e1 e2 ...)
      #'(syntax-case (list in ...) ()
          ((out ...) (let () e1 e2 ...)))))))

(define-syntax syntax-rules
  (lambda (xx)
    (letrec
        ((expand-clause
          (lambda (clause)
            ;; Convert a 'syntax-rules' clause into a 'syntax-case' clause.
            (syntax-case clause (syntax-error)
              (((keyword . pattern) template)
               #'((dummy . pattern) #'template)))))
         (expand-syntax-rules
          (lambda (dots keys docstrings clauses)
            (with-syntax
             (((k ...) keys)
              ((docstring ...) docstrings)
              ((((keyword . pattern) template) ...) clauses)
              ((clause ...) (map expand-clause clauses)))
             (with-syntax
              ((form #'(lambda (x)
                         docstring ...        ; optional docstring
                         #((macro-type . syntax-rules)
                           (patterns pattern ...)) ; embed patterns as procedure metadata
                         (syntax-case x (k ...)
                           clause ...))))
              #'form))
            )))
      (syntax-case xx ()
        ((_ (k ...) ((keyword . pattern) template) ...)
         (expand-syntax-rules #f #'(k ...) #'() #'(((keyword . pattern) template) ...)))
        ((_ (k ...) docstring ((keyword . pattern) template) ...)
         (string? (syntax->datum #'docstring))
         (expand-syntax-rules #f #'(k ...) #'(docstring) #'(((keyword . pattern) template) ...)))))))

(define-syntax define-syntax-rule
  (lambda (x)
    (syntax-case x ()
      ((_ (name . pattern) template)
       #'(define-syntax name
           (syntax-rules ()
             ((_ . pattern) template))))
      ((_ (name . pattern) docstring template)
       (string? (syntax->datum #'docstring))
       #'(define-syntax name
           (syntax-rules ()
             docstring
             ((_ . pattern) template)))))))

(define (generate-temporaries ls)
  (map (lambda (x) (datum->syntax #f (module-gensym "t"))) ls))

(define-syntax quasisyntax
  (lambda (e)
    (letrec
        ((expand
          (lambda (x level)
            (syntax-case x (quasisyntax unsyntax unsyntax-splicing)
              ((quasisyntax e)
               (with-syntax (((k _) x)
                             ((e* reps) (expand (syntax e) (+ level 1))))
                            (syntax ((k e*) reps))))
              ((unsyntax e)
               (= level 0)
               (with-syntax (((t) (generate-temporaries '(t))))
                            (syntax (t ((t e))))))
              (((unsyntax e ...) . r)
               (= level 0)
               (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                             ((t ...)        (generate-temporaries (syntax (e ...)))))
                            (syntax ((t ... . r*)
                                     ((t e) ... rep ...)))))
              (((unsyntax-splicing e ...) . r)
               (= level 0)
               (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                             ((t ...)        (generate-temporaries (syntax (e ...)))))
                            (with-syntax ((((tt ...) ...) (syntax ((t (... ...)) ...))))
                                         (syntax ((tt ... ... . r*)
                                                  (((tt ...) e) ... rep ...))))
                        ))
              ((k . r)
               (and (> level 0)
                    (identifier? (syntax k))
                    (or (free-identifier=? (syntax k) (syntax unsyntax))
                        (free-identifier=? (syntax k) (syntax unsyntax-splicing))))
               (with-syntax (((r* reps) (expand (syntax r) (- level 1))))
                            (syntax ((k . r*) reps))))
              ((h . t)
               (with-syntax (((h* (rep1 ...)) (expand (syntax h) level))
                             ((t* (rep2 ...)) (expand (syntax t) level)))
                            (syntax ((h* . t*)
                                     (rep1 ... rep2 ...)))))
              (#(e ...)
               (with-syntax ((((e* ...) reps)
                              (expand (vector->list (syntax #(e ...))) level)))
                            (syntax (#(e* ...) reps))))
              (other
               (syntax (other ())))))))

      (syntax-case e ()
        ((_ template)
         (with-syntax (((template* replacements) (expand (syntax template) 0)))
                      (syntax
                       (with-syntax replacements (syntax template*)))))))))

(define (identifier? x)
        (and (syntax? x)
             (symbol? (syntax-expression x))))
