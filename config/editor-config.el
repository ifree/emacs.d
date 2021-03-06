;;; editor-config.el --- Genreal editor settings

;;; Commentary:
;; 


;;; Code:
;; copy paste whole line or region
(use-package utility)
(use-package search-tool)
(use-package whole-line-or-region
  :ensure t
  :init (whole-line-or-region-mode))


;; recentf
(use-package recentf
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 1000
	recentf-exclude '("^/tmp/pg*"
			  "^pg*"
			  "/tmp/"
			  "/ssh:")))


(fset 'yes-or-no-p 'y-or-n-p)
(global-linum-mode)
(setq-default fill-column 80)
;;; new line behavior
(global-set-key (kbd "RET") 'newline-and-indent)


;; backup
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist   '((".*" . "~/.saves"))    ; don't litter my fs tree
   auto-save-file-name-transforms   '((".*" "~/.saves"))
   delete-old-versions t
   kept-new-versions 4
   kept-old-versions 2
   version-control t)       ; use versioned backups




;;; hl
(use-package highlight-symbol
  :ensure t
  :config
  (diminish 'highlight-symbol-mode)
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
    (add-hook hook 'highlight-symbol-mode)
    (add-hook hook 'highlight-symbol-nav-mode))
  (add-hook 'org-mode-hook 'highlight-symbol-nav-mode))


;;; undo tree
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  (diminish 'undo-tree-mode))


;; guide-key
(use-package guide-key
  :ensure t
  :init
  (guide-key-mode 1)
  (diminish 'guide-key-mode)
  (setq guide-key/guide-key-sequence '("C-x" "C-c" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r" "C-x r" "C-c r"))
  (setq guide-key/recursive-key-sequence-flag t))

(use-package guide-key-tip
  :if window-system ;have some issue with X, x-show-tip have issue with HighDPI display....
  :ensure t
  :config
  (setq guide-key-tip/enabled t))

(use-package regex-tool :ensure t)
(use-package scratch :ensure t)

(use-package edit-server
  :if window-system
  :ensure t
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

;; dired
(use-package dired+
  :ensure t
  :init
  (setq-default diredp-hide-details-initially-flag nil
		dired-dwim-target t))

(use-package diff-hl
  :ensure t
  :defer t
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

;; projectile
;; http://projectile.readthedocs.io/en/latest/configuration/
(use-package projectile
  :ensure t
  :config
  (setq projectile-mode-line
        '(:eval (format " Projectile[%s]"
                        (projectile-project-name))))
  
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ([f8]        . treemacs)
        ("M-0"       . treemacs-select-window)
        ("C-c t 1"   . treemacs-delete-other-windows)
	("C-x t t"   . treemacs)
	("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; fonts
(use-package default-text-scale
  :ensure t
  :init
  (when (member "Inconsolata" (font-family-list nil))
    (set-face-attribute 'default nil :family "Inconsolata" :height 140))
  
  (bind-key "C-M-=" #'default-text-scale-increase)
  (bind-key "C-M--" #'default-text-scale-decrease))


;; spell checking
(use-package flyspell
  :config
  (use-package ispell)
  (when (executable-find ispell-program-name)
    (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)))


;; hipple expand
(use-package hipple-expand
  :bind
  ("M-/" . hippie-expand)

  :init  
  (setq hippie-expand-try-functions-list
	'(try-complete-file-name-partially
	  try-complete-file-name
	  try-expand-dabbrev
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill)))

;; company
(use-package company
  :ensure t
  :init
  (global-company-mode)
  (diminish 'company-mode "CMP"))

;; ido (helm next time
(use-package ido
  :init
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-use-virtual-buffers t)	;for history buffers
  
  (flx-ido-mode 1)			;override ido's flex matching
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  
  ;; disable search file in other dir if it not exists
  (setq ido-auto-merge-work-directories-length -1)
  :config
  (use-package flx-ido :ensure t)
  (use-package recentf)
  (add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element)))
  
  (use-package ido-completing-read+
    :ensure t
    :init (ido-ubiquitous-mode t))

  (use-package idomenu
    :ensure t))


;; Use smex to handle M-x
(use-package smex
  :ensure t
  :init
  ;; Change path for ~/.smex-items
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex))

;; revert buffer after image changed 
(use-package image-mode
  :init
  (add-hook 'image-mode-hook 'auto-revert-mode))

(provide 'editor-config)

;;; editor-config.el ends here
