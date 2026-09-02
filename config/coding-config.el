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

;; Explain the region or buffer with tree-sitter and the pi agent.
(use-package code-insight
  :ensure nil
  :commands (frei/code-insight-history frei/code-insight-open-in-browser)
  :bind
  (("C-c a" . frei/code-insight)))

(use-package diagram-tool
  :ensure nil
  :commands (frei/diagram-render-buffer frei/diagram-toggle-images))

(use-package treesit-tool
  :ensure nil
  :commands frei/treesit-install-grammars)

(provide 'coding-config)

;;; coding-config.el ends here
