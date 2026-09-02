;;; coding-config.el --- Programming support  -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared programming tools that are independent of a language server.

;;; Code:
(setq load-prefer-newer t)

(use-package paredit
  :ensure t
  :hook
  ((emacs-lisp-mode . paredit-mode)
   (lisp-interaction-mode . paredit-mode)
   (lisp-mode . paredit-mode)
   (ielm-mode . paredit-mode)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package macrostep
  :ensure t
  :bind
  (:map emacs-lisp-mode-map
        ("C-c e" . macrostep-expand)))

(use-package magit
  :ensure t
  :commands magit-status
  :bind
  (("C-x g" . magit-status)))

(provide 'coding-config)

;;; coding-config.el ends here
