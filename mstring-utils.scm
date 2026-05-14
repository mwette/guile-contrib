;;; mstring-utils.scm - like python longstrings
;;;
;;; Copyright (C) 2023-2026 Matthew Wette
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.

;; see also string-diff in seqdiff.scm
(define-module (mstring-utils)
  #:export (string-search-rev
            enable-longstrings disable-longstrings read-longstring))

(define (string-search-rev pat str)
  (let ((chl (reverse (string->list pat))))
    (let loop ((want chl) (ex #f) (sx (string-length str)))
      (cond
       ((null? want) sx)
       ((zero? sx) #f)
       ((char=? (car want) (string-ref str (1- sx)))
        (loop (cdr want) (or ex sx) (1- sx)))
       (else (loop chl #f (1- (or ex sx))))))))
            
;; @deffn {procedure} read-longstring reader-char port
;; This reader macro procedure reads extended strings using the
;; delimiter @code{"""}.  Enable and disable its use via the
;; syntax @code{(enable-longstring)} and @code{(disable-longstring)}.
;; Example use:
;; @example
;; (define text #"""
;;   "Run. Matt. Run.", he said.
;; """)
;; @end example
;; @end deffn
(define (read-longstring reader-char port)
  "- procedure: read-longstring reader-char port
     This reader macro procedure reads extended strings using the
     delimiter ‘\"\"\"’.  Enable and disable its use via the syntax
     ‘(enable-longstring)’ and ‘(disable-longstring)’.  Example use:
          (define text #\"\"\"
            \"Run. Matt. Run.\", he said.
          \"\"\")"
  (define start-sq '(#\" #\" #\"))
  (define end-sq '(#\" #\" #\"))

  (define (skip-seq seq ch)
    (let loop ((bs seq) (ch ch))
      (cond
       ((null? bs) ch)
       ((eof-object? ch) (error "bad longstring expression"))
       ((char=? ch (car bs)) (loop (cdr bs) (read-char port)))
       (else (error "longstring: coding error")))))

  (let loop ((chl '()) (ex '()) (es end-sq)
             (ch (let ((ch (skip-seq start-sq reader-char)))
                   (if (char=? #\newline ch) (read-char port) ch))))
    (cond
     ((eof-object? ch) (error "bad longstring expression"))
     ((char=? ch (car es))
      (let ((es (cdr es)))
        (if (null? es)
            (reverse-list->string chl)
            (loop chl (cons ch ex) es (read-char port)))))
     ((pair? ex) (loop (append ex chl) '() end-sq ch))
     (else (loop (cons ch chl) ex es (read-char port))))))

(define-syntax-rule (enable-longstring)
  (eval-when (expand load eval)
    (read-hash-extend #\" read-longstring)
    (if #f #f)))

(define-syntax-rule (disable-longstring)
  (eval-when (expand load eval)
    (read-hash-extend #\" #f)
    (if #f #f)))

;; --- last line ---
