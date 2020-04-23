;;; package --- summary
;; Provides editing functionality for the ion-shell

;;; Commentary:
;; Currently provides syntax highlighting

;;; Code:


(define-derived-mode ion-mode prog-mode "ion"
  "major mode for editing ion scripts"
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
  )

(provide 'ion-mode)
;;; ion-mode ends here


