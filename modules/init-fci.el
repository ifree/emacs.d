;;; init-fci.el --- Fill column indicator

;;; Commentary:
;; 


;;; Code:

(require-package 'fill-column-indicator)

(setq fci-style 'shading)
(setq fci-rule-width 1)
(setq fci-rule-color "darkgrey")
(setq fci-rule-column 80)
(setq fci-handle-truncate-lines nil)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(defun auto-fci-mode (&optional unused)
    (if (> (window-width) fci-rule-column)
        (fci-mode 1)
      (fci-mode 0))
    )
(add-hook 'after-change-major-mode-hook 'auto-fci-mode)
(add-hook 'window-configuration-change-hook 'auto-fci-mode)

(provide 'init-fci)

;;; init-fci.el ends here
