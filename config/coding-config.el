;;; coding-config.el --- Conding config

;;; Commentary:
;; 


;;; Code:
;; lisp
(use-package paredit
  :ensure t
  :init
  (diminish 'paredit-mode "Par")
  (use-package paredit-everywhere
    :ensure t
    :init
    (require 'paredit)
    (define-key paredit-mode-map (read-kbd-macro "M-?") nil)
    (add-hook 'prog-mode-hook 'paredit-everywhere-mode)))

(use-package lively
  :ensure t)

;; Load .el if newer than corresponding .elc
(setq load-prefer-newer t)

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

(use-package hl-sexp
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package macrostep
  :ensure t)

(use-package lisp-mode
  :init
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)
  ;; eldoc
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(require 'derived)

(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

(defun frei/lisp-setup()
  (rainbow-delimiters-mode t)
  (enable-paredit-mode)
  (set-up-hippie-expand-for-elisp)
  (eldoc-mode))

(dolist (hook (mapcar #'derived-mode-hook-name
                      '(lisp-mode emacs-lisp-mode inferior-lisp-mode lisp-interaction-mode)))
  (add-hook hook 'frei/lisp-setup))


(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))


(use-package magit
  :ensure t)

(use-package ag
  :if (executable-find "ag")
  :ensure t
  :init
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-?") 'ag-project)
  :config
  (use-package wgrep-ag
    :ensure t))


(provide 'coding-config)

;;; coding-config.el ends here
