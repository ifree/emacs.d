;;; rust-config.el --- rust config

;;; Commentary:
;; 


;;; Code:
(use-package rust-mode
  :ensure t
  :config
  (use-package cargo
    :ensure t)
  ;; racer
  (use-package racer
    :ensure t)
  (use-package flycheck-rust
    :ensure t)
  (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))




(provide 'rust-config)

;;; rust-config.el ends here
