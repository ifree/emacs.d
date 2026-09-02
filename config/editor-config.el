;;; editor-config.el --- General editor settings  -*- lexical-binding: t; -*-

;;; Commentary:
;; Editing defaults, completion UI, navigation, and persistent state.

;;; Code:
(require 'utility)
(require 'search-tool)

(defvar frei/config-directory)
(defvar ispell-program-name)
(declare-function server-running-p "server")

(defconst frei/state-directory
  (expand-file-name "var/" frei/config-directory)
  "Directory for generated Emacs state.")

(defconst frei/backup-directory
  (expand-file-name "backups/" frei/state-directory)
  "Directory for backup files.")

(defconst frei/auto-save-directory
  (expand-file-name "auto-save/" frei/state-directory)
  "Directory for auto-save files.")

(dolist (directory
         (list frei/state-directory
               frei/backup-directory
               frei/auto-save-directory))
  (make-directory directory t))

(setq backup-by-copying t
      backup-directory-alist
      (list (cons "." frei/backup-directory))
      auto-save-file-name-transforms
      (list (list ".*" frei/auto-save-directory t))
      auto-save-list-file-prefix
      (expand-file-name ".saves-" frei/auto-save-directory)
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      delete-by-moving-to-trash t
      enable-recursive-minibuffers t
      sentence-end-double-space nil
      use-short-answers t
      read-extended-command-predicate
      #'command-completion-default-include-p)

(setq-default fill-column 120
              indent-tabs-mode nil)

(defun frei/prog-mode-visuals ()
  "Enable unobtrusive visual helpers in programming buffers."
  (setq-local show-trailing-whitespace t)
  (display-line-numbers-mode 1)
  (display-fill-column-indicator-mode 1))

(add-hook 'prog-mode-hook #'frei/prog-mode-visuals)

(electric-pair-mode 1)
(delete-selection-mode 1)

(when (fboundp 'context-menu-mode)
  (context-menu-mode 1))

(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-save-file
   (expand-file-name "recentf.eld" frei/state-directory))
  (recentf-max-saved-items 500)
  (recentf-auto-cleanup 300)
  (recentf-exclude
   '("/tmp/" "/ssh:" "/sudo:" "/var/folders/")))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1)
  :custom
  (savehist-file
   (expand-file-name "history.eld" frei/state-directory))
  (history-length 500))

(use-package saveplace
  :ensure nil
  :init
  (save-place-mode 1)
  :custom
  (save-place-file
   (expand-file-name "places.eld" frei/state-directory)))

(use-package which-key
  :ensure nil
  :demand t
  :init
  (which-key-mode 1)
  :config
  (which-key-setup-minibuffer))

(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

(when (boundp 'completion-pcm-leading-wildcard)
  (setq completion-pcm-leading-wildcard t))

(use-package marginalia
  :ensure t
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode 1))

(use-package consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer)
   ("C-c r" . consult-ripgrep)
   ("C-c l" . consult-line)
   ("C-c t" . consult-goto-line)
   ("M-g g" . consult-goto-line)
   ("M-s l" . consult-line)
   ("M-s r" . consult-ripgrep)
   ("M-y" . consult-yank-pop))
  :custom
  (consult-preview-key '(:debounce 0.15 any)))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode 1)
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (corfu-cycle t)
  (corfu-preselect 'prompt))

(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2)))

(use-package subword
  :ensure nil
  :hook (prog-mode . subword-mode))

(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode 1))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package dired
  :ensure nil
  :bind
  (("C-x C-j" . dired-jump))
  :custom
  (dired-dwim-target t))

(use-package dired-sidebar
  :ensure t
  :commands dired-sidebar-toggle-sidebar
  :bind
  ([f8] . dired-sidebar-toggle-sidebar))

(use-package project
  :ensure nil
  :bind
  (([f7] . project-compile))
  :custom
  (project-list-file
   (expand-file-name "projects.eld" frei/state-directory)))

(defun frei/project-eshell ()
  "Open an Eshell rooted at the current project."
  (interactive)
  (let ((display-buffer-alist
         (cons
          '("\\*.*eshell.*\\*"
            (display-buffer-reuse-window display-buffer-at-bottom)
            (window-height . 0.25))
          display-buffer-alist)))
    (if (project-current nil)
        (project-eshell)
      (eshell))))

(use-package eshell
  :ensure nil
  :bind
  (([f9] . frei/project-eshell)))

(use-package diff-hl
  :ensure t
  :hook
  ((prog-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)))

(use-package markdown-mode
  :ensure t
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(use-package image-mode
  :ensure nil
  :hook (image-mode . auto-revert-mode))

(let ((spell-checker
       (or (executable-find "hunspell")
           (executable-find "aspell"))))
  (when spell-checker
    (require 'ispell)
    (setq ispell-program-name spell-checker)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

(use-package server
  :ensure nil
  :if (not noninteractive)
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'editor-config)

;;; editor-config.el ends here
