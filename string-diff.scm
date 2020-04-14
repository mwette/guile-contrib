;; string-diff

(use-modules (srfi srfi-11))

(define (string-diff a b)
  (define stref string-ref)
  (define ch-eq? char=?)
  (define subst substring)
  (define rlstr reverse-list->string)

  (define an (string-length a))
  (define bn (string-length b))

  (letrec
      ((lolev
	(lambda (rem add com ax bx)
	  (cond
	   ((or (= ax an) (= bx bn))
	    (values rem add com ax bx))
	   ((ch-eq? (stref a ax) (stref b bx))
	    (lolev rem add (cons (stref a ax) com) (1+ ax) (1+ bx)))
	   ((pair? com) (values rem add com ax bx))
	   (else
	    (let-values
		(((rem-1 add-1 com-1 ax-1 bx-1) ; case 1, rem from a
		  (lolev (cons (stref a ax) rem) add com (1+ ax) bx))
		 ((rem-2 add-2 com-2 ax-2 bx-2) ; case 2, add from b
		  (lolev rem (cons (stref b bx) add) com ax (1+ bx))))
	      (if (> (length com-1) (length com-2))
		  (values rem-1 add-1 com-1 ax-1 bx-1)
		  (values rem-2 add-2 com-2 ax-2 bx-2)))))))
       (hilev
	(lambda (ax bx)
	  (cond
	   ((and (= ax an) (= bx bn)) '())
	   ((or (= ax an) (= bx bn)) (list (list (subst a ax) (subst b bx) "")))
	   (else (call-with-values
		     (lambda () (lolev '() '() '() ax bx))
		   (lambda (rem add com ax bx)
		     (cons (list (rlstr rem) (rlstr add) (rlstr com))
			   (hilev ax bx)))))))))
    (hilev 0 0)))

;;(pp (string-diff "One 2 for." "One 33 for."))
;;(pp (string-diff "abc" "bcd"))

;; --- last line ---
