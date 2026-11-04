;;; editor-config.el --- General editor settings  -*- lexical-binding: t; -*-

;;; Commentary:
;; Editing defaults, completion UI, navigation, and persistent state.

;;; Code:
(require 'utility)
(require 'search-tool)

(defvar frei/config-directory)
(defvar diff-hl-dired-process-buffer)
(defvar ispell-program-name)
(defvar w32-pipe-buffer-size)
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

;; The Windows locale would otherwise select Latin-1 and CRLF for new files.
(set-language-environment "UTF-8")
(setq-default buffer-file-coding-system 'utf-8-unix)

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
      confirm-kill-processes nil
      isearch-lazy-count t
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

(defun frei/file-line-numbers ()
  "Show line numbers in ordinary file buffers, including text and config files."
  (when (and buffer-file-name
             (not (derived-mode-p 'special-mode 'image-mode 'doc-view-mode
                                  'pdf-view-mode 'so-long-mode))
             (not (bound-and-true-p so-long-minor-mode)))
    (display-line-numbers-mode 1)))

(add-hook 'find-file-hook #'frei/file-line-numbers)
(add-hook 'after-change-major-mode-hook #'frei/file-line-numbers)

(electric-pair-mode 1)
(delete-selection-mode 1)
(repeat-mode 1)
(pixel-scroll-precision-mode 1)
(global-so-long-mode 1)

(context-menu-mode 1)

(defun frei/escape-quit ()
  "Quit like `keyboard-escape-quit', but keep the window layout."
  (interactive)
  (let ((buffer-quit-function (or buffer-quit-function #'ignore)))
    (keyboard-escape-quit)))

;; Remap rather than bind, so that modes remapping `keyboard-escape-quit'
;; themselves, such as Corfu, still take precedence.
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(global-set-key [remap keyboard-escape-quit] #'frei/escape-quit)

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
  (history-length 500)
  (savehist-additional-variables
   '(kill-ring search-ring regexp-search-ring)))

(use-package saveplace
  :ensure nil
  :init
  (save-place-mode 1)
  :custom
  (save-place-file
   (expand-file-name "places.eld" frei/state-directory)))

(use-package bookmark
  :ensure nil
  :defer t
  :custom
  (bookmark-default-file
   (expand-file-name "bookmarks.eld" frei/state-directory))
  (bookmark-save-flag 1))

(use-package transient
  :ensure nil
  :defer t
  :custom
  (transient-history-file
   (expand-file-name "transient/history.el" frei/state-directory))
  (transient-levels-file
   (expand-file-name "transient/levels.el" frei/state-directory))
  (transient-values-file
   (expand-file-name "transient/values.el" frei/state-directory)))

;; Tramp defines this variable in its autoloads with the default value,
;; which a Custom setting cannot override, so set it directly.
(defvar tramp-persistency-file-name)
(setq tramp-persistency-file-name
      (expand-file-name "tramp.eld" frei/state-directory))

(use-package eshell
  :ensure nil
  :defer t
  :custom
  (eshell-directory-name
   (expand-file-name "eshell/" frei/state-directory)))

(use-package nsm
  :ensure nil
  :defer t
  :custom
  (nsm-settings-file
   (expand-file-name "network-security.eld" frei/state-directory)))

(use-package url
  :ensure nil
  :defer t
  :custom
  (url-configuration-directory
   (expand-file-name "url/" frei/state-directory)))

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

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

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
   ("M-g i" . consult-imenu)
   ("M-g o" . consult-outline)
   ("C-c f" . consult-recent-file)
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
  (corfu-auto t)
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
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t))

(defun frei/dired-sidebar-follow-p (&rest _)
  "Return non-nil when the sidebar should follow the selected window.
Only file buffers count: for others `dired-sidebar' would move to their
`default-directory', such as the home directory for *Messages*."
  (buffer-file-name (window-buffer)))

(use-package dired-sidebar
  :ensure t
  :bind
  ([f8] . dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-should-follow-file t)
  :config
  (advice-add 'dired-sidebar-follow-file :before-while #'frei/dired-sidebar-follow-p))

;; Lets the sidebar expand folders in place with TAB.
(use-package dired-subtree
  :ensure t
  :defer t)

(defun frei/diff-hl-dired-cancel-pending-update ()
  "Cancel the previous status query before reusing its output buffer.
Merely signaling the process lets its sentinel start another Git command
in the reused buffer, whose cleanup can then prompt to kill that command."
  (when (buffer-live-p diff-hl-dired-process-buffer)
    (when-let* ((process (get-buffer-process diff-hl-dired-process-buffer)))
      (set-process-sentinel process #'ignore)
      (delete-process process))))

(use-package diff-hl
  :ensure t
  :hook
  ((prog-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode 1)
  (with-eval-after-load 'diff-hl-dired
    (advice-add 'diff-hl-dired-update :before
                #'frei/diff-hl-dired-cancel-pending-update)))

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package markdown-mermaid
  :ensure nil
  :hook (markdown-mode . frei/markdown-mermaid-mode))

(use-package autorevert
  :ensure nil
  :init
  (global-auto-revert-mode 1)
  :custom
  (auto-revert-avoid-polling t)
  (global-auto-revert-non-file-buffers t))

(let ((spell-checker
       (or (executable-find "hunspell")
           (executable-find "aspell"))))
  (when spell-checker
    (require 'ispell)
    (setq ispell-program-name spell-checker)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

;; Git prints non-ASCII file names as octal escapes unless told otherwise,
;; which shows up as \NNN in command output and completion candidates.
(setenv "GIT_CONFIG_COUNT" "1")
(setenv "GIT_CONFIG_KEY_0" "core.quotepath")
(setenv "GIT_CONFIG_VALUE_0" "false")

(when (eq system-type 'windows-nt)
  ;; Compacting font caches causes pauses with CJK fonts, and the default
  ;; pipe buffer makes subprocesses such as git and rg slow.
  (setq inhibit-compacting-font-caches t
        w32-pipe-buffer-size (* 64 1024)
        ;; Auto-detection would treat the NUL separated output of ripgrep
        ;; as binary, so decode process output as UTF-8.
        default-process-coding-system '(utf-8-dos . utf-8-unix)))

(use-package server
  :ensure nil
  :if (not noninteractive)
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'editor-config)

;;; editor-config.el ends here
