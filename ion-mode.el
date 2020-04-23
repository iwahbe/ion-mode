;;; package --- summary
;; Provides editing functionality for the ion-shell

;;; Commentary:
;; Currently provides syntax highlighting and indentation

;;; Code:

(define-derived-mode ion-mode prog-mode "ion"
    "major mode for editing ion scripts"
  (defvar ion-indent-forward-keywords
    '("if" "fn" "for" "while")
    "ion-shell keywords that should cause an indent")
    
  (defvar ion-indent-backwards-keywords
    '("end")
    "ion-shell keywords that should decrease the indent")
  
  (defvar ion-indent-neutral-keywords
    '("alias" "and" "bg" "break" "calc" "case" "cd" "complete"
      "continue" "count" "dirs" "disown" "drop" "echo" "else"
      "eval" "exec" "exit" "false" "fg" "help"
      "history" "in" "jobs" "let" "match" "matches" "mkdir"
      "not" "or" "popd" "pushd" "pwd" "read" "set" "source"
      "status" "suspend" "test" "time" "true" "unalias" "wait")
    "ion-shell keywords that don't effect indentation")

  (defun ion-keywords ()
    "A list of all ion-shell keywords"
    (append ion-indent-forward-keywords
	    ion-indent-backwards-keywords
	    ion-indent-neutral-keywords))

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
		     (ion-keywords)
		     'words)
		    font-lock-keyword-face)))
  
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
    (let ((indent-end 0)(indent-begin 0)
	  (end (regexp-opt ion-indent-backwards-keywords 'word))
	  (begin (regexp-opt ion-indent-forward-keywords)))
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
	(insert repl))))

  (defun ion-replace-whitespace-end-line (pos repl)
    (save-excursion
      (end-of-line)
      (let ((begin-point (re-search-backward "[[:graph:]]" (line-beginning-position) t)))
	(delete-region (+ 1 (point)) (line-end-position))
	(insert repl))))

  (defun ion-indent-line ()
    (ion-replace-whitespace-begin-line
     (point) (let ((indent (max (ion-indentation-level
				 (line-beginning-position)) 0)))
	       (if ion-indent-spaces
		   (make-string (* 4 indent) ? )
		 (make-string indent ?\t))))
    (let ((non-whitespace
	   (save-excursion
	     (progn (forward-line 0)
		    (re-search-forward
		     "[\t ]*" (line-end-position) t)))))
    (if (= non-whitespace (line-end-position))
	(end-of-line)
      (if (> non-whitespace (point))
	  (goto-char non-whitespace))
      ))
    (ion-replace-whitespace-end-line (point) "")
    )
    
  
  (setq font-lock-defaults '(ion-mode-highlights))
  
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


