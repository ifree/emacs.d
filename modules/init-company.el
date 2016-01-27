;;; init-company.el --- Company mode settings

;;; Commentary:
;; 


;;; Code:

(require-package 'company)

(add-hook 'after-init-hook 'global-company-mode)

(after-load 'company
  (diminish 'company-mode "CMP"))


(provide 'init-company)

;;; init-company.el ends here
