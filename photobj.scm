;;;Copyright (c) 2022 Kanin Shen.
;;;Copyright (c) 2005, 2009, 2011, 2016 Neil Van Dyke.
;;;This program is Free Software; you can redistribute it and/or modify it
;;;under the terms of the GNU Lesser General Public License as published by the ;;;Free Software Foundation; either version 3 of the License, or
;;;(at your option) any later version.  This program is distributed in the hope ;;;that it will be useful, but without any warranty; without even the implied
;;;warranty of merchantability or fitness for a particular purpose.
;;;See http://www.gnu.org/licenses/ for details.

#!chezscheme
(library (photobj)
    (export ! % ? @ ^
	    current-root-object
	    object?
	    object-apply
            object-apply/noslot-thunk
	    object-get
	    object-get/noslot-thunk
	    object-raw-clone/copy-all-slots
	    object-raw-clone/copy-immed-slots
	    object-raw-clone/no-slots-copy
	    object-set!)
    (import (rnrs) (rnrs mutable-pairs))
    
    (define make-parameter
      (case-lambda
	((init guard)
	 (let ((v (guard init)))
	   (case-lambda
	     (()  v)
	     ((u) (set! v (guard u))))))
	((init)
	 (make-parameter init (lambda (x) x)))))
    (define-record-type (object make-object object?)
      (fields
       (mutable parent object-parent set-parent!)
       (mutable slots slots set-slots!)))
    #;(define (slots-assq slot-symbol slots)
      (assq slot-symbol slots))
    #;(define (slots-assq slot-symbol slots)
     (let loop ((slots slots))
       (cond ((null? slots)                       #f)
	     ((eq? slot-symbol (car (car slots))) (car slots))
	     (else                                (loop (cdr slots))))))
    (define (find-slot obj slot-symbol proc noslot-thunk)
      (let loop ((o obj))
	(cond ((assq slot-symbol (slots o)) => proc)
	      (else (cond ((object-parent o) => loop)
			  (else (noslot-thunk)))))))
    (define (object-set! obj slot-symbol val)
      (let ((slots (slots obj)))
	(cond ((assq slot-symbol slots)
	       => (lambda (slot) (set-cdr! slot val)))
              (else (set-slots! obj (cons (cons slot-symbol val)
					  slots))))))
    (define (object-get obj slot-symbol)
      (find-slot
       obj
       slot-symbol
       cdr
       (lambda () (error obj "Object has no such slot:" slot-symbol))))
    (define (object-get/noslot-thunk obj slot-symbol noslot-thunk)
      (find-slot obj
		 slot-symbol
		 cdr
		 noslot-thunk))
    (define (object-apply obj slot-symbol . args)
      (apply (object-get obj slot-symbol) obj args))
    (define (object-apply/noslot-thunk obj slot-symbol noslot-thunk . args)
      (find-slot obj
		 slot-symbol
		 (lambda (slot) (apply (cdr slot) obj args))
		 noslot-thunk))
    (define (object-raw-clone/no-slots-copy obj)
      (make-object obj '()))
    (define (object-raw-clone/copy-immed-slots obj)
      (make-object obj
		   (map (lambda (pair)
			  (cons (car pair) (cdr pair)))
			(slots obj))))
    (define (object-raw-clone/copy-all-slots obj)
      (let loop-objs ((o    obj)
		      (seen '()))
	(if o
            (let loop-slots ((slots  (slots o))
			     (result seen))
              (if (null? slots)
		  (loop-objs (object-parent o) result)
		  (loop-slots (cdr slots)
			      (let ((name (car (car slots))))
				(if (assq name seen)
				    result
				    (cons (cons name (cdr (car slots)))
					  result))))))
	    (make-object obj seen))))
    (define current-root-object
      (make-parameter
       (make-object
	#f
	(list (cons 'clone object-raw-clone/no-slots-copy)))))
    (define-syntax ^ (syntax-rules () ((_ OBJ) (object-parent OBJ))))
    (define-syntax !
      (syntax-rules ()
	((_ OBJ (S0 V0) (S1 V1) ...) (let ((temp OBJ))
				       (! temp S0 V0)
                                       (! temp S1 V1) ...))
	((_ OBJ S V)                   (object-set! OBJ (quote S) V))))
    (define-syntax ?
      (syntax-rules ()
	((_ OBJ S)      (object-get OBJ (quote S)))
	((_ OBJ S0 ...) (let ((temp OBJ)) (values (? temp S0) ...)))))
    (define-syntax apply*
      (syntax-rules ()
	((_ (X0 X1 ...) S A0 ...) (let ((temp (X0 X1 ...)))
				    (apply* temp S A0 ...)))
	((_ OVAR        S A0 ...) ((object-get OVAR (quote S)) OVAR A0 ...))))
    (define-syntax @
      (syntax-rules ()
	((_ OBJ (S0 A0 ...) (S1 A1 ...) ...)
	 (let ((temp OBJ))
	   (apply* temp S0 A0 ...)
	   (apply* temp S1 A1 ...) ...))
	((_ OBJ S A ...)
	 (apply* OBJ S A ...))))
    (define-syntax %
      (syntax-rules ()
	((_)                         (% (current-root-object)))
	((_ OBJ)                     (@ OBJ clone))
        ((_ OBJ (S0 V0) (S1 V1) ...) (let ((temp (% OBJ)))
				       (! temp S0 V0)
                                       (! temp S1 V1) ...
                                       temp))))
    )
