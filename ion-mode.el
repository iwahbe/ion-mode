;;; ion-mode.el --- A Major mode for ion-shell scripting
;; -*- lexical-binding: t -*-
;; Provides editing functionality for the ion-shell

;;; Commentary:
;; Currently provides syntax highlighting and indentation.
;; Syntax highlighting works on keywords returned with
;; 
;;                 (ion-keywords)
;;
;; and uses font-lock-keyword-face.  Comments are highlighted with
;; font-lock-comment-face.  Variables are highlighted with
;; font-lock-variable-name-face if defined outside of comments or single quoted
;; text.  Function names are highlighted with font-lock-function-name-face, but
;; only where they are defined.
;;
;; Indentation is calculated by sorting some keywords in to 3 categories.  One
;; category is indent-forward, which means there should be an indent following
;; this line.  This category is defined in the variable
;; 'ion-indent-forward-keywords'
;; Example:
;;
;;            if test 1 -le 0    # if is indent-forward
;;                echo indented  # so the next line is indented
;;
;; This works similarly to indent-backwards, except indent-backwards takes
;; effect immediately.
;; Example:
;;
;;                echo "done indenting" # this line is indented
;;            end                       # end is indent-backwards
;;
;; There is one more category, which doesn't effect indentation, but does change
;; how the line itself is displayed.  This category currently contains only
;; 'else' and 'if else'.
;; Example:
;;
;;            if test $x -le $y
;;                echo "$x > $y"
;;            else
;;                echo "$x < $y"
;;            end
;;
;; Syntax highlighting is surprisingly complicated.  This is due to the
;; difference between shell languages and normal languages.  Specifically there
;; are two different types of strings, and one type must show variable
;; highlighting, while the other must ignore the escape character.  The firs
;; problem is handled by doing a second run to highlight variables in double
;; quoted strings using the font-lock-add-keywords interface.  For the second, I
;; hack into the syntax highlighting function by defining my own
;; syntax-propertize-function and manually set otherwise escape character to be
;; a punctuation character within single quoted strings.



;; MIT License
;; 
;; Copyright (c) 2020 Ian Wahbe
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

;;;###autoload
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

  (declare-function ion-keywrods "ion-mode" ())
  
  (defun ion-keywords ()
    "A list of all ion-shell keywords"
    (append ion-indent-forward-keywords
	    ion-indent-backwards-keywords
	    ion-indent-neutral-keywords))

  
  (setq ion-mode-syntax-table
	(let ((syn-table (make-syntax-table prog-mode-syntax-table)))
	  ;; single quotes are strings that cannot be escaped
	  (modify-syntax-entry ?\' "\"" syn-table)
	  (modify-syntax-entry ?\" "\"" syn-table)
	  ;; comments start with # and end with a new line
	  (modify-syntax-entry ?# "<" syn-table)
	  (modify-syntax-entry ?\n ">" syn-table)
	  syn-table
	  ))

  (defvar ion-mode-single-quote-syntax-table
    (let ((syn-table (make-syntax-table ion-mode-syntax-table)))
      (modify-syntax-entry ?\\ "." syn-table) syn-table)
    "A syntax table for withing single quotes. It does not allow escapes")

  ;; Note: this function is run after the syntactic analyzer but before the
  ;; keyword step. It allows the user to override the syntax-table.
  (set (make-local-variable 'syntax-propertize-function)
       #'ion-mode-syntax-propertize-function)

  (defun ion-mode-syntax-propertize-function (start end)
	"Hook into the syntax propertize function to create non-standard text properties.
`START' and `END' define the limits of the search."
    ;Currently used to make '' strings inescapable
    (goto-char start)
    (while (re-search-forward "'" end t)
      (let ((begin-string (nth 3 (syntax-ppss (match-beginning 0)))))
      (if (and begin-string ; inside a string
	       (char-equal ?\' begin-string) ; its a ' string
	       (equal (char-after) ?\') ; we are at the end of the string
	       )
	  (put-text-property (nth 8 (syntax-ppss (point))) (point) 'syntax-table
			     ion-mode-single-quote-syntax-table)))
      (goto-char (1+ (point)))))

  (defvar ion-mode-variable-regex "[A-Z|a-z|_]"
    "The regex used to define what can appear in an ion variable")

  ;; Used to automate comment insertion
  (setq comment-start "#")
  (setq comment-padding " ")
  
  (set-syntax-table ion-mode-syntax-table)
  
  (defvar ion-mode-highlights
    (list (cons (regexp-opt
		 (ion-keywords)
		 'words)
		font-lock-keyword-face))
    "regexp optimal for of keywords to highlight")

  (declare-function ion-find-quote-variables "ion-mode" (limit regexp))
  
  (defun ion-find-quote-variables (limit regexp)
    "Used to find variables to highlight in string quotes.
    A variable is any string that matches `REGEXP'.
    It searches from the current point to `LIMIT'."
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

  (declare-function ion-is-command "ion-mode" (pos))
  
  (defun ion-is-command (pos)
    "Checks if `POS' is inside a string or comment"
    (let ((state (syntax-ppss pos)))
      (not (or
	    (nth 3 state) ; inside a string
	    (nth 4 state) ; inside a comment
	    ))))
  
  (declare-function ion-line-has "ion-mode" (point regx))
  
  (defun ion-line-has (pos regx)
    "Checks if the line containing `POS' contains `REGX'"
    (save-excursion
      (beginning-of-line)
      (re-search-forward regx (line-end-position) t)))
  
  (defun ion-indentation-level (point)
    "Returns a pair, the indentation level and the display offset at `POINT'"
    ;; This function works by recursing up to the top of the screen each call
    ;; finds the indentation change of the current line and adds it to the past
    ;; line. Some keywords have a special relation to indentation, like 'else'
    ;; which is what the second part of the cons returned is for. This is
    ;; calculated for a line only, and not used recursively.
    (let ((indent-level 0) (display-level 0))
      (save-excursion
	(beginning-of-line)
	(while (re-search-forward
		(regexp-opt ion-indent-forward-keywords 'word)
		(line-end-position) t)
	  (if (and (ion-is-command (point))
		   (not (ion-line-has (point) (regexp-opt '("else if") 'word))))
	      (setq indent-level (1+ indent-level)
		    display-level (1- display-level))))
	(beginning-of-line)
	(while (re-search-forward
		(regexp-opt ion-indent-backwards-keywords 'word)
		(line-end-position) t)
	  (if (ion-is-command (point)) (setq indent-level (1- indent-level))))
	(beginning-of-line)
	(while (re-search-forward (regexp-opt '("else" "else if") 'word)
				  (line-end-position) t)
	  (setq display-level (1- display-level))))
      (forward-line 0)
      (unless (= (point) (point-min))
	(forward-char -1))
      (cons
       ;; the max helps small indentation errors from disturbing an entire
       ;; project, but it does make debugging harder
       (max 0 (+ (if (= (point) (point-min)) 0 (car (ion-indentation-level (point))))
		 indent-level))
       display-level)))

  (defun ion-replace-whitespace-begin-line (pos repl)
    "Replaces whitespace before any character text on the line where `POS' is with `REPL'"
    (save-excursion
      (forward-line 0)
      (let ((end-point (re-search-forward "[\t ]*" (line-end-position) t)))
	(delete-region (line-beginning-position) (point))
	(insert repl))))

  (defun ion-replace-whitespace-end-line (pos repl)
    "Replaces whitespace at the end of the line where `POS' is with the string `REPL'"
    (save-excursion
      (end-of-line)
      (let ((begin-point (re-search-backward
			  "[[:graph:]]" (line-beginning-position) t)))
	(if (< (1+ (point)) (line-end-position))
	    (delete-region (1+ (point)) (line-end-position)))
	(insert repl))))

  (defun ion-indent-line ()
    "Indent's the current line according to ion-mode, removing excess whitespace"
    (ion-replace-whitespace-begin-line
     (point) (let*
		 ((indent (save-excursion (ion-indentation-level
					   (line-beginning-position))))
		  (display-indent (+ (car indent) (cdr indent))))
	     (if indent-tabs-mode
			 (make-string display-indent ?\t)
		   (make-string (* 4 display-indent) ? ) ; this is an escaped space
		   )))
    ;; cleanup whitespace
    (let ((non-whitespace
	   (save-excursion
	     (progn (forward-line 0)
		    (re-search-forward
		     "[\t ]*" (line-end-position) t)))))
      
      (if (= non-whitespace (line-end-position))
	  (end-of-line)
	(if (> non-whitespace (point))
	    (goto-char non-whitespace))))
    
    (ion-replace-whitespace-end-line (point) ""))

  (setq font-lock-defaults '(ion-mode-highlights))

  (defun ion-mode-match-assembly (before-frame var-needed &optional after-frame)
    "Regex matching (var)(frame)(ion-mode-variable-regex)(frame)"
    (concat "\\([$@]" before-frame "\\)\\(" ion-mode-variable-regex
	    (if (equal '+ var-needed) "+" (if (equal '* var-needed) "*")) "\\)"
	    (if after-frame (concat "\\(" after-frame "\\)") "")))

  
  (font-lock-add-keywords 'ion-mode
			  `(
			    (,(concat "\\(fn\\) \\(" ion-mode-variable-regex "+\\)")
			     ;; fontify fn as keyword
			     (1 font-lock-keyword-face)
			     ;; fontify fn name as fn name
			     (2 font-lock-function-name-face))
				(,(concat "\\(let\\) \\(" ion-mode-variable-regex "+\\)")
				 (1 font-lock-keyword-face)
				 (2 font-lock-variable-name-face))
			    (,(ion-mode-match-assembly "" '+)
			     (1 font-lock-builtin-face)
			     (2 font-lock-variable-name-face))
			    (,(ion-mode-match-assembly "{" '* "}")
			     (1 font-lock-builtin-face)
			     (2 font-lock-variable-name-face)
			     (3 font-lock-builtin-face))
			    (,(ion-mode-match-assembly "(" '* ")")
			     (1 font-lock-builtin-face)
			     (2 font-lock-variable-name-face)
			     (3 font-lock-builtin-face))
			    
			    ;; this highlights variables in quotes which are
			    ;; already highlighted by the syntactic highlighting
			    ;; pass

			    ((lambda (l)
			       (ion-find-quote-variables
				l ,(ion-mode-match-assembly "" '+)))
			     (1 font-lock-builtin-face t)
			     (2 font-lock-variable-name-face t))
			    
			    ((lambda (l)
			       (ion-find-quote-variables
				l ,(ion-mode-match-assembly "{" '+ "}")))
			     (1 font-lock-builtin-face t)
			     (2 font-lock-variable-name-face t)
			     (3 font-lock-builtin-face t))
			    
			    ((lambda (l)
			       (ion-find-quote-variables
				l ,(ion-mode-match-assembly "(" '+ ")")))
			     (1 font-lock-builtin-face t)
			     (2 font-lock-variable-name-face t)
			     (3 font-lock-builtin-face t))
			    ))

  (setq indent-line-function #'ion-indent-line)
  )


(defun ion-lexical-level (point)
  "Guess the lexical level of at `POINT' from indentation."
  (let ((level (save-excursion (ion-indentation-level (point)))))
	(+ (car level) (cdr level))))

(defun ion-variable-permutations (string)
  "Add possibel ways to call a variabele in ion-shell.
Variable is declared with name `STRING'"
  (list (concat "$" string)
		(concat "@" string)))


(defun ion-mode-company-completions ()
  "Find strings to feed to company-ion."
  ;; This strategy fails because it will only account for changes in indentation
  ;; when it sees a let binding. Instead, it must check each line.
  (save-excursion
  (let ((indent-scope (ion-lexical-level (point)))
		(vars ()))

	(while (not (= (point-min) (point)))
	  ;; Find all let statements, should be done each line
	  (let ((current-indent (ion-lexical-level (point))))
		  (while (re-search-forward
				  (concat "\\(let \\)\\("
						  ion-mode-variable-regex "+\\)\\(: "
						  ion-mode-variable-regex "+\\)?\\( [-+]?= \\)")
				  (line-end-position) t)
			(let ((mbeg2 (match-beginning 2))
				  (mend2 (match-end 2))
				  (mbeg0 (match-beginning 0))
				  (mend0 (match-end 0)))
			  (if (and (ion-is-command mbeg0)
					   (<= current-indent indent-scope))
				  (dolist (item (ion-variable-permutations
								 (buffer-substring-no-properties
						  mbeg2 mend2)))
					(add-to-list 'vars item)))
			  (goto-char mend0)
			  ))
		;; update indentation level, should be done each line
		(if (< current-indent indent-scope) (setq indent-scope current-indent))
		(forward-line -1)))
	vars)))

(defun company-ion (command &optional arg &rest ignored)
  "Company backend for ion-mode.
`COMMAND' gives the type of command.
`ARG' gives the arguments for that command.
`IGNORED' prevents errors when called."
  (interactive '(interactive))
  (cl-case command
	
	(interactive (company-begin-backend 'company-ion))

	(prefix (and (eq major-mode 'ion-mode) (company-grab-symbol)))

	(candidates
	 (cl-remove-if-not (lambda (s) (string-prefix-p arg s))
					(ion-mode-company-completions)))))

(if (featurep 'company) (add-to-list 'company-backends #'company-ion))


(provide 'ion-mode)
;;; ion-mode ends here
