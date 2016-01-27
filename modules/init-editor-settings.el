;;; init-editor-settings.el --- Genreal editor settings

;;; Commentary:
;; 


;;; Code:

(fset 'yes-or-no-p 'y-or-n-p)

(global-linum-mode)

(setq inhibit-startup-screen t)

(setq inhibit-startup-echo-area-message t)

(setq-default fill-column 80)
;;; new line behavior
(global-set-key (kbd "RET") 'newline-and-indent)

;; backup
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 4
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; copy paste whole line or region
(require-package 'whole-line-or-region)
(whole-line-or-region-mode)

;; recentf
(recentf-mode 1)

(setq recentf-max-saved-items 1000
      recentf-exclude '(
                        "^/tmp/pg*"
                        "^pg*"
			"/tmp/"
			"/ssh:"
                        ))

;;; hl
(require-package 'highlight-symbol)
(require-package 'diminish)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(add-hook 'org-mode-hook 'highlight-symbol-nav-mode)
(after-load 'highlight-symbol
  (diminish 'highlight-symbol-mode)
  (defadvice highlight-symbol-temp-highlight (around sanityinc/maybe-suppress activate)
    "Suppress symbol highlighting while isearching."
    (unless (or isearch-mode
                (and (boundp 'multiple-cursors-mode) multiple-cursors-mode))
      ad-do-it)))

;;; undo tree
(require-package 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; guide-key
(require-package 'guide-key-tip)
(require 'guide-key-tip)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r" "C-x r"))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)
(setq guide-key-tip/enabled t)
(diminish 'guide-key-mode)

(require-package 'regex-tool)
(require-package 'diminish)
(require-package 'scratch)

(provide 'init-editor-settings)

;;; init-editor-settings.el ends here
