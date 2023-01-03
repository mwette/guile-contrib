;; seqdiff.scm - generate sequence diff
;;
;; Copyright (C) 2020,2023 Matthew R. Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; This software is reserved for personal use by the copyright holder.

;; This code is derived from Python's difflib.

(define-module (seqdiff)
  #:export (sequence-diff string-diff make-seq-procs str-procs)
  #:use-module (srfi srfi-9))

(define-record-type seq-procs
  (make-seq-procs seq-len seq-ref elt-eq? seq-href seq-hset!)
  seq-procs?
  (seq-len seq-len-proc)
  (seq-ref seq-ref-proc)
  (elt-eq? elt-eq?-proc)
  (seq-href seq-href-proc)
  (seq-hset! seq-hset!-proc))

;; junk? is predicate taking elt
(define* (sequence-diff a b procs #:optional junk?)
  (letrec*
      ((seq-len (seq-len-proc procs))
       (seq-ref (seq-ref-proc procs))
       (elt-eq? (elt-eq?-proc procs))
       (seq-href (seq-href-proc procs))
       (seq-hset! (seq-hset!-proc procs))
       (la (seq-len a))
       (lb (seq-len b))
       (bx (let* ((bx (make-hash-table 97)))
	     (let loop ((ix (1- lb)))
	       (if (negative? ix) bx
		   (let ((el (seq-ref b ix)))
		     (seq-hset! bx el (cons ix (or (seq-href bx el) '())))
		     (loop (1- ix)))))))
       (longest-match
	(lambda (alo ahi blo bhi)
	  (let* ((lbp1 (1+ (seq-len b))))
	    (let loop1 ((ix alo)
			(next (make-vector lbp1 0))
			(prev (make-vector lbp1 0))
			(lix alo)
			(ljx blo)
			(lsz 0))
	      (if (= ix ahi) (values lix ljx lsz)
		  (let loop2 ((jxl (or (seq-href bx (seq-ref a ix)) '()))
			      (lix lix)
			      (ljx ljx)
			      (lsz lsz))
		    (cond
		     ((null? jxl)
		      (loop1 (1+ ix) (make-vector lbp1 0) next lix ljx lsz))
		     ((< (car jxl) blo)
		      (loop2 (cdr jxl) lix ljx lsz))
		     ((<= bhi (car jxl))
		      (loop1 (1+ ix) (make-vector lbp1 0) next lix ljx lsz))
		     (else
		      (let* ((jx (car jxl))
			     (k (1+ (vector-ref prev jx))))
			(vector-set! next (1+ jx) k)
			(if (> k lsz)
			    (loop2 (cdr jxl) (1+ (- ix k)) (1+ (- jx k)) k)
			    (loop2 (cdr jxl) lix ljx lsz)))))))))))
       (get-matches
	(lambda ()
	  (let loop ((res '()) (next '()) (curr `((0 ,la 0 ,lb))))
	    (cond
	     ((pair? curr)
	      (let ((alo (list-ref (car curr) 0)) (ahi (list-ref (car curr) 1))
		    (blo (list-ref (car curr) 2)) (bhi (list-ref (car curr) 3)))
		(call-with-values
		    (lambda () (longest-match alo ahi blo bhi))
		  (lambda (i j k)
		    (if (zero? k)
			(loop res next (cdr curr))
			(let* ((next (if (and (< alo i) (< blo j))
					 (cons (list alo i blo j) next)
					 next))
			       (next (if (and (< (+ i k) ahi) (< (+ j k) bhi))
					 (cons (list (+ i k) ahi (+ j k) bhi)
					       next)
					 next)))
			  (loop (cons (list i j k) res) next (cdr curr))))))))
	     ((pair? next)
	      (loop res '() next))
	     (else
	      (sort res (lambda (a b) (< (car a) (car b)))))))))
       (add-edits
	(lambda (i1 i2 j1 j2 res)
	  (cond
	   ((and (< i1 i2) (< j1 j2)) (cons* `(ins ,j1 ,j2) `(del ,i1 ,i2) res))
	   ((< i1 i2) (cons `(del ,i1 ,i2) res))
	   ((< j1 j2) (cons `(ins ,j1 ,j2) res))
	   (else res))))
       (gen-ops
	(lambda (matches)
	  (let loop ((res '()) (i1 0) (j1 0) (fxs matches))
	    (if (null? fxs)
		(let* ((res (if (< i1 la) (cons `(del ,i1 ,la) res) res))
		       (res (if (< j1 lb) (cons `(ins ,j1 ,lb) res) res)))
		  (reverse res))
		(let* ((fx (car fxs))
		       (i2 (list-ref fx 0))
		       (j2 (list-ref fx 1))
		       (sz (list-ref fx 2))
		       (res (add-edits i1 i2 j1 j2 res))
		       (res (cons `(dup ,i2 ,(+ i2 sz)) res)))
		  (unless (positive? sz) (error "fix for zero case!\n"))
		  (loop res (+ i2 sz) (+ j2 sz) (cdr fxs))))))))

    (gen-ops (get-matches))))

(define str-procs (make-seq-procs string-length string-ref char=?
				  hashq-ref hashq-set!))

(define* (string-diff a b #:optional junk?)
  (sequence-diff a b str-procs junk?))

;; --- last line ---
