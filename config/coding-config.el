;;; coding-config.el --- Programming support  -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared programming tools that are independent of a language server.

;;; Code:
(require 'seq)

(defconst frei/treesit-disabled-modes
  '(js-ts-mode mhtml-ts-mode php-ts-mode go-work-ts-mode)
  "Tree-sitter modes that fail without the jsdoc, phpdoc or gowork grammar.
The prebuilt bundle of `frei/treesit-install-grammars' lacks these.")

;; Visit files in the tree-sitter major modes.  The option only takes
;; effect through its setter, so set it once treesit is loaded.  Grammars
;; come from `frei/treesit-install-grammars'; left at `ask', modes offer
;; on every visit to compile a missing grammar, which needs a compiler.
(use-package treesit
  :ensure nil
  :config
  (setopt treesit-auto-install-grammar 'never
          treesit-enabled-modes
          (seq-difference (seq-uniq (mapcar #'cdr treesit-major-mode-remap-alist))
                          frei/treesit-disabled-modes)))

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

;; Magit binds C-x g itself.
(use-package magit
  :ensure t
  :defer t)

;; Explain the region or buffer with tree-sitter and the pi agent.
(use-package code-insight
  :ensure nil
  :commands (frei/code-insight-history frei/code-insight-open-in-browser
             frei/code-insight-treesit-diagram)
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
