;;; hereis.scm - like python longstrings
;;;
;;; Copyright (C) 2023-2024 Matthew Wette
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.

(define-module (hereis)
  #:export (enable-hereis disable-hereis read-hereis-text))

(define (read-hereis-text reader-char port)
  (define beg-sq '(#\" #\" #\"))
  (define end-sq '(#\" #\" #\"))

  (define (skip-seq seq ch)
    (let loop ((bs seq) (ch ch))
      (cond
       ((null? bs) ch)
       ((eof-object? ch) (error "bad hereis expression"))
       ((char=? ch (car bs)) (loop (cdr bs) (read-char port)))
       (else (error "hereis: coding error")))))

  (define (skip-nl ch)
    (if (char=? #\newline ch) (read-char port) ch))

  ;;(assert (eq? reader-char (car beg-sq))
  (let loop ((chl '()) (ex '()) (es end-sq)
             (ch (skip-nl (skip-seq beg-sq reader-char))))
    (cond
     ((eof-object? ch) (error "bad hereis expression"))
     ((char=? ch (car es))
      (let ((es (cdr es)))
        (if (null? es)
            (reverse-list->string chl)
            (loop chl (cons ch ex) es (read-char port)))))
     ((pair? ex) (loop (append ex chl) '() end-sq ch))
     (else (loop (cons ch chl) ex es (read-char port))))))

(define-syntax-rule (enable-hereis)
  (eval-when (expand load eval)
    (read-hash-extend #\" read-hereis-text)
    (if #f #f)))

(define-syntax-rule (disable-hereis)
  (eval-when (expand load eval)
    (read-hash-extend #\" #f)
    (if #f #f)))

;; --- last line ---
