;;; cpp-config.el --- C and C++ configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; C/C++ editing through built-in modes and Eglot with clangd.

;;; Code:
(require 'cpp-tool)
(require 'c-ts-google-style)

(defvar frei/state-directory
  (expand-file-name "var/" user-emacs-directory)
  "Directory for generated Emacs state.")

(defun frei/cpp-eglot-ensure ()
  "Start Eglot when clangd is available."
  (when (executable-find "clangd")
    (eglot-ensure)))

(use-package cc-mode
  :ensure nil
  :mode
  (("\\.h\\'" . c++-mode))
  :hook
  ((c-mode . frei/cpp-eglot-ensure)
   (c++-mode . frei/cpp-eglot-ensure)))

(when (and (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'c))
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-hook 'c-ts-mode-hook #'frei/cpp-eglot-ensure))

(when (and (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'cpp))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-hook 'c++-ts-mode-hook #'frei/cpp-eglot-ensure))

;; `google-c-style' only knows cc-mode, so give the tree-sitter modes an
;; equivalent indentation style.
(use-package c-ts-mode
  :ensure nil
  :hook (c-ts-base-mode . frei/c-ts-google-setup))

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :custom
  (eglot-autoshutdown t))

(use-package google-c-style
  :ensure t
  :hook (c-mode-common . google-set-c-style))

(use-package cmake-mode
  :ensure t
  :mode
  (("CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode)))

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
          '("\\*eshell.*\\*"
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

(use-package gdb-mi
  :ensure nil
  :custom
  (gdb-many-windows t)
  (gdb-show-main t))

(provide 'cpp-config)

;;; cpp-config.el ends here
