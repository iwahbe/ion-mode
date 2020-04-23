;;; package --- summary
;; Provides editing functionality for the ion-shell

;;; Commentary:
;; Currently provides syntax highlighting

;;; Code:


(define-derived-mode ion-mode prog-mode "ion"
  "major mode for editing ion scripts"

  (defvar ion-indent-spaces t "use spaces for indentation")
  (setq ion-mode-syntax-table
	(let ((syn-table (make-syntax-table prog-mode-syntax-table)))
					;single quotes are strings
	  (modify-syntax-entry ?\' "\"" syn-table)
	  (modify-syntax-entry ?\" "\"" syn-table)
	  (modify-syntax-entry ?# "<" syn-table)
	  (modify-syntax-entry ?\n ">" syn-table)
	  syn-table
	  ))
  (set-syntax-table ion-mode-syntax-table)
  (setq ion-mode-highlights
	(list (cons (regexp-opt
		     '("in" "if" "else" "let" "for" "end" "echo" "test")
		     'words) font-lock-keyword-face)
	      ))
  (defun ion-find-quote-variables (limit regexp)
    "Used to find variables in quotes"
    (let ((original-match-data nil))
      (save-match-data
	(while (and (null original-match-data)
		    (re-search-forward regexp limit t))
	  (if (and (nth 3 (syntax-ppss (match-beginning 0)))
		   (char-equal ?\" (nth 3 (syntax-ppss (match-beginning 0)))))
	      (setq original-match-data (match-data))
	    (forward-char 1)
	    )))
	  (when original-match-data
	    (set-match-data original-match-data)
	    (goto-char (match-end 0))
	    t)))

  (defun ion-is-command (pos)
    (let ((state (syntax-ppss pos)))
      (not (or
	    (nth 3 state) ; inside a string
	    (nth 4 state) ; inside a comment
	    ))))
    

  (defun ion-indentation-level (point)
    (let ((indent-end 0)(indent-begin 0)(end (regexp-opt '("end") 'word))
	  (begin (regexp-opt '("if" "fn" "for"))))
      (save-excursion
	(goto-char point)
	(end-of-line)
	(while (re-search-backward end (point-min) t)
	  (if (ion-is-command (point))
	  (setq indent-end (1+ indent-end)))))
      (save-excursion
	(goto-char point)
	(beginning-of-line)
	(while (re-search-backward begin (point-min) t)
	  (if (ion-is-command (point))
	  (setq indent-begin (1+ indent-begin)))))
      (max 0 (- indent-begin indent-end))
      ))
  
  (defun ion-replace-whitespace-begin-line (pos repl)
    (save-excursion
      (forward-line 0)
      (let ((end-point (re-search-forward "[\t ]*" (line-end-position) t)))
	(delete-region (line-beginning-position) (point))
	(insert repl)
	))
    )

  (defun ion-indent-line ()
    (ion-replace-whitespace-begin-line
     (point) (let ((indent (max (ion-indentation-level
				 (line-beginning-position)) 0)))
	       (if ion-indent-spaces
		   (make-string (* 4 indent) ? )
		 (make-string indent ?\t))))
    (if (save-excursion
	  (progn (forward-line 0) (= (re-search-forward
				      "[\t ]*" (line-end-position) t)
				     (line-end-position))))
	(end-of-line)))
  
  (setq font-lock-defaults '(ion-mode-highlights))
  (setq ion-mode-keywords "\\(if\\|else\\|let\\|for\\|in\\)")
  (font-lock-add-keywords 'ion-mode
			  '(
			    ("\\(fn\\) \\([a-z|A-Z|\\-]+\\)"
			     ;; fontify fn as keyword
			     (1 font-lock-keyword-face)
			     ;; fontify fn name as fn name
			     (2 font-lock-function-name-face))
			    ("\\([$@]\\)\\([A-z]+\\)" 
			     (1 font-lock-builtin-face)
			     (2 font-lock-variable-name-face))
			    ("\\([$@]{\\)\\([A-z| |]*\\)\\(}\\)"
			     (1 font-lock-builtin-face)
			     (2 font-lock-variable-name-face)
			     (3 font-lock-builtin-face))
			    ;; doesn't work
			    ("\\(\\[|\\]\\)" 0 font-lock-builtin-face)
			    ;; ("\\(\".*?\"\\)" 0 font-lock-string-face keep)
			    ((lambda (l)
			       (ion-find-quote-variables
				l "\\([$@]\\)\\([A-z]+\\)"))
			     1 font-lock-builtin-face t)
					
			    ((lambda (l)
			       (ion-find-quote-variables
				l "\\([$@]\\)\\([A-z]+\\)"))
			     2 font-lock-variable-name-face t)
			    ))

  (setq indent-line-function #'ion-indent-line)
  )

(provide 'ion-mode)
;;; ion-mode ends here


