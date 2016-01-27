;;; common-env.el --- Most common environment

;;; Commentary:
;; 


;;; Code:

(custom-environment-define
 "common"
 :modules '(
            utils
            elpa
	    locales
	    editor-settings
	    fci
	    themes
	    gui-frames
	    mode-line
	    fonts
	    company
	    ido
	    sessions
	    uniquify
	    hippie-expand
	    isearch
	    ibuffer
	    spelling
	    dired
	    windows
            ))

(provide 'common-env)

;;; common-env.el ends here
