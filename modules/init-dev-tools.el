;;; init-dev-tools.el --- various dev tools

;;; Commentary:
;; 


;;; Code:
;; code playgroud
(require 'playground)
;; leet-code tool
(require 'leet-code)

;;file template
(setq file-template-enabled t)
(autoload 'file-template-setup "filetemplate" nil t)
(file-template-setup)

;; (setq file-template-dir
;;       "~/Dev/projects/file-template"
;;       )

(provide 'init-dev-tools)

;;; init-dev-tools.el ends here
