;;; org-config.el --- org settings

;;; Commentary:
;; 


;;; Code:
(setq org-src-fontify-natively t)	; syntax highlighting
(setq org-startup-with-inline-images t);; auto show images

(use-package tex
  :if (executable-find "/usr/bin/tex")
  :defer t
  :ensure auctex
  :config
  (use-package cdlatex :ensure t)
  (use-package org
    :ensure t
    :init
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex)))

(use-package org-onenote
  :defer t
  :ensure t)

(provide 'org-config)

;;; org-config.el ends here
