;;; scheme-texidoc.el --- minor mode to make scheme doc-strings from texinfo

;; Copyright (C) 2018 Matthew R. Wette

;; Author: Matt Wette <mwette@alumni.caltech.edu>
;; Keywords: scheme, texinfo

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;;; Notes:

;; M-x unload-feature RET scheme-texidoc RET

;; todo: remove old docstring, if present
;; todo: robustly find "(define ("
;; todo: handle (define foo (let (...) (lambda (...) "docstring"

;;; Code:

;;(require 'texinfmt)

(define-minor-mode scheme-texidoc
  "A minor-mode to create Scheme docstrings from texinfo comments.
Assume you have procedure that starts with
  (define (
and is preceeded by comments that provide documentation between 
  ;; @deffn ...
and 
  ;; @end deffn
Then set point to just before `(define (' and hit `C-cC->'.  
A texi2any formatted docstring will be inserted."
  nil
  " Tx"
  '(([(control c) (control d)] . scheme-texidoc-transfer-deffn))
  )

(defvar scheme-texidoc-version "v180902f")

(defvar scheme-texidoc-texi-buffer-name "*scmtxi texi*")
(defvar scheme-texidoc-text-buffer-name "*scmtxi text*")

;; moves point
(defun find-deffn-above ()
  "find deffn to this point; give up if hit blank before"
  (interactive)
  (beginning-of-line)
  (while (and (not (looking-at ";; @deffn "))
	      (not (looking-at "\\($\\| \\)")))
    (forward-line -1))
  (if (looking-at ";; @deffn ") (point) nil))

;; moves point
(defun find-end-deffn-below ()
  "find end deffn blow non ;;"
  (interactive)
  (beginning-of-line)
  (while (and (not (looking-at ";; @end deffn"))
	      (looking-at ";;\\($\\| \\)"))
    (forward-line 1))
  (if (looking-at ";; @end deffn") (progn (forward-line) (point)) nil))

;; we want to find range for deffn and point for docstring
(defun find-deffn-posns ()
  "Find points of interest."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (not (looking-at ";;")) (backward-line))
    (let ((txi-st (find-deffn-above))
	  (txi-nd (find-end-deffn-below))
	  (doc-pt nil)
	  )
      (while (not (looking-at "(")) (forward-line))
      (cond
       ((looking-at "(define *(")
	(skip-chars-forward 9)
	(forward-sexp)
	(set! doc-pt (point)))
       ((looking-at "(define *[^ (]")
	(re-search-forward "(lambda (" 100)
	(error "not implemented"))
       (t
	(error "couldn't find location for docstring")))
      (goto-char doc-pt)
      )))

;; run texi2any --plaintext on region
;; returns t or nil if success or failure to run command, respectively
(defun texi2any-on-region (start end)
  "run texi2any on region"
  (let ((text-buffer (get-buffer-create scheme-texidoc-text-buffer-name)))
    (save-excursion
      (set-buffer text-buffer)
      (erase-buffer))
    (zerop (call-process-region start end "texi2any" nil text-buffer nil
				"--plaintext"))
    (save-excursion
      (set-buffer text-buffer)
      ;; remove warning messages
      (goto-char (point-min))
      (while (looking-at "-:")
	(delete-region (point-min) (progn (forward-line) (point))))
      ;; clean up
      (goto-char (point-min))
      (if (looking-at " -") (delete-char 2))
      (while (> (forward-line) 0) (if (looking-at "  ") (delete-char 2)))
      (goto-char (point-max))
      (delete-trailing-whitespace)
      (delete-char -1))
    ))

(defun scheme-texidoc-transfer-deffn ()
  "Insert a docstring formatted from the leading texinfo-comments."
  (interactive)
  (save-excursion
    ;; Find the enclosing define.
    (beginning-of-line)
    (unless (looking-at "(define")
      (error "needs love to find (define"))
    (let* ((defpt (point))
	   (tx-st (find-deffn-above))	  ; texi start 
	   (tx-nd (find-end-deffn-below)) ; texi end
	   (scm-buf (current-buffer)))	  ; user buffer
      ;; Copy texi to working buffer and remove leading comment chars.
      (set-buffer (get-buffer-create scheme-texidoc-texi-buffer-name))
      (texinfo-mode)
      (erase-buffer)
      (insert-buffer-substring scm-buf tx-st tx-nd)
      (goto-char (point-min))
      (while (looking-at ";;")
	(delete-char 2)
	(if (looking-at " ") (delete-char 1))
	(forward-line)
	(beginning-of-line))
      ;; Generate info format.
      (goto-char (point-min)) (insert "\n")
      (goto-char (point-max)) (insert "\n")
      (texi2any-on-region (point-min) (point-max))
      ;; Copy into working Scheme buffer.
      (set-buffer (get-buffer scheme-texidoc-text-buffer-name))
      (let* ((pt-st (point-min))
	     (pt-nd (point-max))
	     (text (buffer-substring pt-st pt-nd)))
	(switch-to-buffer scm-buf)
	;; Position to insert point.
	(goto-char defpt)
	(forward-line)
	(beginning-of-line)
	;; Insert text from bufffer and indent.
	(let ((inspt (point)))
	  (insert "\"" text "\"\n")
	  (goto-char inspt)
	  (indent-for-tab-command))
	))))

(provide 'scheme-texidoc)

;; --- last line ---
