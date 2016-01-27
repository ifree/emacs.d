;;; minimal-env.el --- Most common environment

;;; Commentary:
;; 


;;; Code:

(custom-environment-define
 "minimal"
 :modules '(
            utils
            elpa
	    locales
	    editor-settings
	    fci
	    themes
	    fonts
	    company
	    uniquify
	    hippie-expand
	    isearch
	    ibuffer
	    spelling
	    dired
            ))


(provide 'minimal-env)

;;; minimal-env.el ends here
