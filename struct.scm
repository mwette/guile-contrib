;;; struct.scm - byte pack/unpack, like the Python struct module
;;;
;;; Copyright (C) 2016,2022,2023 Matthew R. Wette
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.

(define-module (struct)
  #:export (packed-size pack unpack)
  #:use-module (rnrs bytevectors))


;; character codes used to indicate endianness:
(define cs:md (string->char-set "=<>!"))

;; character codes used to indicate type:
(define cs:df (string->char-set "xcbB?hHiIlLqQfdsp")) ; type char

;; Return integer value of the character in string @var{str} at index @var{ix}.
(define (ctoi-at str ix) (- (char->integer (string-ref str ix)) 48))

;; Return endianness given the character code @var{code}.
(define (get-nd code)
  (case code
    ((#\!) (endianness big))		; network
    ((#\>) (endianness big))
    ((#\<) (endianness little))
    ((#\=) (native-endianness))
    ((#\@) (error "alignment not supported"))
    (else  (native-endianness))))

;; Return the size in bytes for data indicated by format count and type.
;; @example
;; (get-size 12 #\s) => 12
;; (get-size 12 #\i) => 4
;; @end example
(define (bv-size ct ch)
  (case ch
    ((#\x) 1) ((#\c) 1) ((#\b) 1) ((#\B) 1) ((#\?) 1)
    ((#\h) 2) ((#\H) 2) ((#\i) 4) ((#\I) 4) ((#\l) 4) ((#\L) 4)
    ((#\q) 8) ((#\Q) 8) ((#\f) 4) ((#\d) 8) ((#\s #\p) ct)
    (else (error "unknown code"))))

;; Return the number of datums indicated by the format count and type.
(define (fmt-cnt ct ch)
  (case ch
    ((#\s #\p) ct)
    (else 1)))
  

;; This is a helper for pack. Set the bv value, return number bytes written.
(define (set-value! bv ix nd ct ch val)
  (case ch
    ((#\x) (if #f #f))
    ((#\c) ;; todo: check for 8-bit char
     (bytevector-u8-set! bv ix (char->integer val)))
    ((#\b) (bytevector-s8-set! bv ix val))
    ((#\B) (bytevector-u8-set! bv ix val))
    ((#\?) (bytevector-u8-set! bv ix (if val 1 0)))
    ((#\h) (bytevector-s16-set! bv ix val nd))
    ((#\H) (bytevector-u16-set! bv ix val nd))
    ((#\i #\l) (bytevector-s32-set! bv ix val nd))
    ((#\I #\L) (bytevector-u32-set! bv ix val nd))
    ((#\q) (bytevector-s64-set! bv ix val nd))
    ((#\Q) (bytevector-u64-set! bv ix val nd))
    ((#\f) (bytevector-ieee-single-set! bv ix val nd))
    ((#\d) (bytevector-ieee-double-set! bv ix val nd))
    ((#\s #\p)
     (bytevector-copy!
      (u8-list->bytevector (map char->integer (string->list val)))
      0 bv ix (string-length val)))
    (else
     (scm-error 'misc-error "unpack"
		"bad type code: ~A" '(ch) #f))))

;; @deffn cons-value bv ix nd cd tail => list
;; Cons the datum indicated by the data with @var{tail}, where
;; @itemize
;; @item @var{bv} is the bytevector
;; @item @var{ix} is the index into the bytevector
;; @item @var{nd} is the endianness
;; @item @var{cd} is the code
;; @end itemize
;; This is a helper for @code{unpack}.
;; @end deffn
(define cons-value
  (let ((sbuf (make-bytevector 128)))
    (lambda (bv ix nd sz ch tail)
      (case ch
	((#\x) tail)
	((#\c) (cons (integer->char (bytevector-u8-ref bv ix)) tail))
	((#\b) (cons (bytevector-s8-ref bv ix) tail))
	((#\B) (cons (bytevector-u8-ref bv ix) tail))
	((#\?) (cons (if (zero? (bytevector-u8-ref bv ix)) #f #t) tail))
	((#\h) (cons (bytevector-s16-ref bv ix nd) tail))
	((#\H) (cons (bytevector-u16-ref bv ix nd) tail))
	((#\i) (cons (bytevector-s32-ref bv ix nd) tail))
	((#\I) (cons (bytevector-u32-ref bv ix nd) tail))
	((#\l) (cons (bytevector-s32-ref bv ix nd) tail))
	((#\L) (cons (bytevector-u32-ref bv ix nd) tail))
	((#\q) (cons (bytevector-s64-ref bv ix nd) tail))
	((#\Q) (cons (bytevector-u64-ref bv ix nd) tail))
	((#\f) (cons (bytevector-ieee-single-ref bv ix nd) tail))
	((#\d) (cons (bytevector-ieee-double-ref bv ix nd) tail))
	((#\s #\p)
	 (set! sbuf (make-bytevector sz))
	 (bytevector-copy! bv ix sbuf 0 sz)
	 (cons (utf8->string sbuf) tail))
	(else
	 (scm-error 'misc-error "unpack"
		    "bad type code: ~A" '(ch) #f))))))


;; @deffn packed-size format => size
;; In the Python struct module this is called "calcsize".
;; @end deffn
(define (packed-size format)
  (cond
   ((zero? (string-length format)) 0)
   (else
    (let* ((char-at (lambda (ix) (string-ref format ix)))
	   (f0 (if (char-set-contains? cs:md (char-at 0)) 1 0))
	   (ln (string-length format)))
      (let iter ((sz 0) (fx f0) (ct 0))	; sz: result, fx: inddx; ct: count
	(cond
	 ((= fx ln)
	  sz)
	 ((char-numeric? (string-ref format fx))
	  (iter sz (1+ fx) (+ (* 10 ct) (ctoi-at format fx))))
	 ((zero? ct)
	  (iter sz fx 1))
	 ((char-set-contains? cs:df (string-ref format fx))
	  (iter (+ sz (* ct (bv-size 1 (string-ref format fx)))) (1+ fx) 0))
	 (else
	  (scm-error 'misc-error "unpack" "format error" '() #f))))))))
  
;; @deffn {Scheme} pack format datum ... => bytevector
;; Like @code{struct.pack} from Python.  Pack @var{datum ...} into a
;; bytevector according to characters in @var{FORMAT}.  For example, 
;; @example
;; (pack "2Hd" 3 22 34.0) => #vu8(3 0 22 0 0 0 0 0 0 0 65 64)
;; @end example
;; In the above, the format characters indicate two unsigned short,
;; one double.  See the struct documentation for more detail.
;; @end deffn
(define (pack format . data)
  "- Scheme: pack format datum ... => bytevector
     Like 'struct.pack' from Python.  Pack DATUM ... into a bytevector
     according to characters in FORMAT.  For example,
          (pack \"2Hd\" 3 22 34.0) => #vu8(3 0 22 0 0 0 0 0 0 0 65 64)
     In the above, the format characters indicate two unsigned short,
     one double.  See the struct documentation for more detail."
  (cond
   ((zero? (string-length format)) (make-bytevector 0))
   (else
    (let* ((char-at (lambda (ix) (string-ref format ix)))
	   (f0 (if (char-set-contains? cs:md (char-at 0)) 1 0))
	   (nd (get-nd (char-at 0)))
	   (ln (string-length format))
	   (bvec (make-bytevector (packed-size format))))
      
      (let iter ((bx 0)		     ; index into resulting bytevector
		 (fx f0)	     ; index into format
		 (vals data)	     ; values to add
		 (ct 0)		     ; count from format
		 (ch #f))	     ; char from format
	(cond
	 ((positive? ct)		; encode a value
	  (set-value! bvec bx nd ct ch (car vals))
	  (iter (+ bx (bv-size ct ch)) fx (cdr vals) (- ct (fmt-cnt ct ch)) ch))
	 ((= fx ln)			; done
	  bvec)
	 ((null? vals)
	  (scm-error 'misc-error "pack"
		     "format size larger than input size" '() #f))
	 ((char-numeric? (string-ref format fx))
	  (iter bx (1+ fx) vals (- (* 10 ct) (ctoi-at format fx)) ch))
	 ((zero? ct)
	  (iter bx fx vals -1 ch))
	 ((char-set-contains? cs:df (string-ref format fx))
	  (iter bx (1+ fx) vals (- ct) (string-ref format fx)))
	 (else
	  (scm-error 'misc-error "pack"
		     "pack error" '() #f))))))))

;; @deffn {Scheme} unpack format bv => list
;; Like @code{struct.unpack} from Python.  Unpack the bytevector @var{bv} list
;; according to the characters in @var{FORMAT}.  For example, 
;; @example
;; (unpack "2Hd" #vu8(3 0 22 0 0 0 0 0 0 0 65 64)) => (3 22 34.0)
;; @end example
;; In the above, the format characters indicate two unsigned short,
;; one double.  See the struct documentation for more detail.
;; @end deffn
(define (unpack format bytevec)
  "- Scheme: unpack format bv => list
     Like 'struct.unpack' from Python.  Unpack the bytevector BV list
     according to the characters in FORMAT.  For example,
          (unpack \"2Hd\" #vu8(3 0 22 0 0 0 0 0 0 0 65 64)) => (3 22 34.0)
     In the above, the format characters indicate two unsigned short,
     one double.  See the struct documentation for more detail."
  (cond
   ((zero? (string-length format)) '())
   (else
    (let* ((char-at (lambda (ix) (string-ref format ix)))
	   (f0 (if (char-set-contains? cs:md (char-at 0)) 1 0))
	   (nd (get-nd (char-at 0)))
	   (ln (string-length format)))
      (let iter ((rz '())               ; result, list of bytevectors
		 (bx 0)			; index into input bytevector
		 (fx f0)                ; index into format string
		 (ct 0)			; format count
		 (ch #f))               ; format char
	(cond
	 ((> fx ln)
	  (error "format size larger than input bv size"))
	 ((positive? ct)
	  (iter (cons-value bytevec bx nd ct ch rz)
		(+ bx (bv-size ct ch)) fx (- ct (fmt-cnt ct ch)) ch))
	 ((= fx ln)
	  ;;(if (not (= bx (bytevector-length bytevec))) (error "error"))
	  (reverse rz))
	 ((char-numeric? (string-ref format fx))
	  (iter rz bx (1+ fx) (- (* 10 ct) (ctoi-at format fx)) ch))
	 ((zero? ct)
	  (iter rz bx fx -1 ch))
	 ((char-set-contains? cs:df (string-ref format fx))
	  (iter rz bx (1+ fx) (- ct) (string-ref format fx)))
	 (else
	  (scm-error 'misc-error "unpack" "format error" '() #f))))))))


;;; --- last line ---
