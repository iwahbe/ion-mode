;;; ion-mode.el --- A Major mode for ion-shell scripting

;;; Commentary:
;; provides company-completion for ion-mode

;;; Code:
(require 'ion-mode)

(defun ion-mode-company-completions ()
  "Find strings to feed to company-ion-backend."
  ;; This strategy fails because it will only account for changes in indentation
  ;; when it sees a let binding. Instead, it must check each line.
  (let ((indent-scope (first (save-excursion (ion-indentation-level (point)))))
		(vars ()))
	(save-excursion
	  (while (re-search-backward
			  (concat "\\(let \\)\\(" ion-mode-variable-regex "+\\)\\( [-+]?= \\)")
			  (point-min) t)
		(let (
			  (mbeg2 (match-beginning 2))
			  (mend2 (match-end 2))
			  (mbeg0 (match-beginning 0))
			  (current-indent
			   (first (save-excursion (ion-indentation-level (point))))))
		  (message "current-index: ")
		  (if (and (ion-is-command mbeg0)
				   (<= current-indent indent-scope))
			  (add-to-list
			   'vars (buffer-substring-no-properties
					  mbeg2 mend2))
			)
		  (if (< current-indent indent-scope) (setq indent-scope current-indent))
		  ))
	  (mapcar(lambda (s) (concat "$" s)) vars)
	  )))

(defun company-ion-backend (command &optional arg &rest ignored)
  "Company backend for ion-mode."
  (interactive '(interactive))
  (case command
	
	(interactive (company-begin-backend 'company-ion-backend))

	(prefix (and (eq major-mode 'ion-mode) (company-grab-symbol)))

	(candidates
	 (remove-if-not (lambda (s) (string-prefix-p arg s))
					(ion-mode-company-completions)))))

(provide 'company-ion-backend)
;;; company-ion-backend ends here
