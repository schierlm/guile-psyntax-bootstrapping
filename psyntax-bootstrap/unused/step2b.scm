;;; Bootstrapping psyntax.scm for Guile
;;;
;;; STEP 2B: Provide syntax-rules from GNU Mes
;;;
;;; Copyright (c) 1993-2004 by Richard Kelsey and Jonathan Rees.
;;; Copyright (c) 2016 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(s1*-define-macro (s2*-define-syntax macro-name transformer . stuff)
  `(s1*-define-macro (,macro-name . args)
     (,transformer (cons ',macro-name args)
                   (lambda (x0) x0)
                   eq?)))

(define s2*-indicators-for-zero-or-more (list '... '---))

(define (s2*-segment-pattern? pattern)
  (and (s2*-segment-template? pattern)
       (or (null? (cddr pattern))
           (s0*-syntax-error "segment matching not implemented" pattern))))

(define (s2*-segment-template? pattern)
  (and (pair? pattern)
       (pair? (cdr pattern))
       (memq (cadr pattern) s2*-indicators-for-zero-or-more)))

(s2*-define-syntax s2*-syntax-rules
        ;;; TESTING: (define s2*-syntax-expander
  (lambda (exp r c)
    (letrec ((%input (r '%input))       ;Gensym these, if you like.
             (%compare (r '%compare))
             (%rename (r '%rename))
             (%tail (r '%tail))
             (%temp (r '%temp))
             (rules (cddr exp))
             (subkeywords (cadr exp))
             (make-transformer
              (lambda (rules)
                `(lambda (,%input ,%rename ,%compare)
                   (let ((,%tail (cdr ,%input)))
                     (cond ,@(map process-rule rules)
                           (else
                            (s0*-syntax-error
                             "use of macro doesn't match definition"
                             ,%input)))))))
             (process-rule
              (lambda (rule)
                (if (and (pair? rule)
                         (pair? (cdr rule))
                         (null? (cddr rule)))
                    (let ((pattern (cdar rule))
                          (template (cadr rule)))
                      `((and ,@(process-match %tail pattern))
                        (let* ,(process-pattern pattern
                                                %tail
                                                (lambda (x) x))
                          ,(process-template template
                                             0
                                             (meta-variables pattern 0 '())))))
                    (s0*-syntax-error "ill-formed syntax rule" rule))))

             ;; Generate code to test whether input expression matches pattern

             (process-match
              (lambda (input pattern)
                (cond ((symbol? pattern)
                       (if (member pattern subkeywords)
                           `((,%compare ,input (,%rename ',pattern)))
                           `()))
                      ((s2*-segment-pattern? pattern)
                       (process-segment-match input (car pattern)))
                      ((pair? pattern)
                       `((let ((,%temp ,input))
                           (and (pair? ,%temp)
                                ,@(process-match `(car ,%temp) (car pattern))
                                ,@(process-match `(cdr ,%temp) (cdr pattern))))))
                      ((or (null? pattern) (boolean? pattern) (char? pattern))
                       `((eq? ,input ',pattern)))
                      (else
                       `((equal? ,input ',pattern))))))
             (process-segment-match
              (lambda (input pattern)
                (let ((conjuncts (process-match '(car l) pattern)))
                  (if (null? conjuncts)
                      `((list? ,input))
                      `((let loop ((l ,input))
                          (or (null? l)
                              (and (pair? l)
                                   ,@conjuncts
                                   (loop (cdr l))))))))))

             ;; Generate code to take apart the input expression
             ;; This is pretty bad, but it seems to work (can't say why).

             (process-pattern
              (lambda (pattern path mapit)
                (cond ((symbol? pattern)
                       (if (memq pattern subkeywords)
                           '()
                           (list (list pattern (mapit path)))))
                      ((s2*-segment-pattern? pattern)
                       (process-pattern (car pattern)
                                        %temp
                                        (lambda (x)        ;temp is free in x
                                          (mapit (if (eq? %temp x)
                                                     path
                                                     `(map (lambda (,%temp) ,x)
                                                           ,path))))))
                      ((pair? pattern)
                       (append (process-pattern (car pattern) `(car ,path) mapit)
                               (process-pattern (cdr pattern) `(cdr ,path) mapit)))
                      (else '()))))

             ;; Generate code to compose the output expression according to template

             (process-template
              (lambda (template rank env)
                (cond ((symbol? template)
                       (let ((probe (assq template env)))
                         (if probe
                             (if (<= (cdr probe) rank)
                                 template
                                 (s0*-syntax-error "template rank error (too few ...'s?)"))
                             `(,%rename ',template))))
                      ((s2*-segment-template? template)
                       (let ((vars
                              (free-meta-variables (car template) (+ rank 1) env '())))
                         (if (null? vars)
                             (s0*-silent-syntax-error "too many ...'s" template)
                             (let* ((x (process-template (car template)
                                                         (+ rank 1)
                                                         env))
                                    (gen (if (equal? (list x) vars)
                                             x ;+++
                                             `(map (lambda ,vars ,x)
                                                   ,@vars))))
                               (if (null? (cddr template))
                                   gen ;+++
                                   `(append ,gen ,(process-template (cddr template)
                                                                    rank env)))))))
                      ((pair? template)
                       `(cons ,(process-template (car template) rank env)
                              ,(process-template (cdr template) rank env)))
                      (else `(quote ,template)))))

             ;; Return an association list of (var . rank)

             (meta-variables
              (lambda (pattern rank vars)
                (cond ((symbol? pattern)
                       (if (memq pattern subkeywords)
                           vars
                           (cons (cons pattern rank) vars)))
                      ((s2*-segment-pattern? pattern)
                       (meta-variables (car pattern) (+ rank 1) vars))
                      ((pair? pattern)
                       (meta-variables (car pattern) rank
                                       (meta-variables (cdr pattern) rank vars)))
                      (else vars))))

             ;; Return a list of meta-variables of given higher rank

             (free-meta-variables
              (lambda (template rank env free)
                (cond ((symbol? template)
                       (if (and (not (memq template free))
                                (let ((probe (assq template env)))
                                  (and probe (>= (cdr probe) rank))))
                           (cons template free)
                           free))
                      ((s2*-segment-template? template)
                       (free-meta-variables (car template)
                                            rank env
                                            (free-meta-variables (cddr template)
                                                                 rank env free)))
                      ((pair? template)
                       (free-meta-variables (car template)
                                            rank env
                                            (free-meta-variables (cdr template)
                                                                 rank env free)))
                      (else free)))))
      (make-transformer rules))))
