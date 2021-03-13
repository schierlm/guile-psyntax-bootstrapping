;;; Bootstrapping psyntax.scm for Guile
;;;
;;; STEP 3B: Generate s3*-make-foo symbols from %expanded-vtable
;;;
;;; Copyright (C) 2001, 2003, 2006, 2009, 2010-2020
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

;;; based on (define-syntax define-expansion-constructors) in psyntax.scm

(define (s3*-constructor-list)
  (let lp ((n 0))
    (if (< n (vector-length %expanded-vtables))
        (cons
         (let* ((vtable (vector-ref %expanded-vtables n))
                (stem (struct-ref vtable (+ vtable-offset-user 0)))
                (fields (struct-ref vtable (+ vtable-offset-user 2)))
                (ctor (string->symbol (string-append "make-" (symbol->string stem)))))
           `(define ,(cons ctor fields)
              (make-struct/simple (vector-ref %expanded-vtables ,n) ,@fields)))
         (lp (1+ n)))
        '())))

(s1*-define-macro (s3*-define-constructors) (cons 'begin (s3*-constructor-list)))
(s3*-define-constructors)
