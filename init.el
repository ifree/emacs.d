;;; init.el --- Emacs config entry point

;;; Commentary:
;; multiple environment support
;; simplified config with `use-package`

;;; Code:
(when (< emacs-major-version 24)
  (message "Your emacs is too old, please upgrade it."))

;; global variables
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d"))

;; gc
(setq gc-cons-threshold (* 128 1024 1024))

;; package archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; init use-package
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(defvar use-package-verbose t)
;(setq use-package-debug t)

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (dolist (var '("EMACS_ENV_NAME" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))


;; load customize file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; customize stuff
;; custom load-path
(add-to-list 'load-path (concat user-emacs-directory "config/"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;; You can override your environment by set environment variable `EMACS_ENV_NAME'
(defconst EMACS_ENV_NAME (or (getenv "EMACS_ENV_NAME") "default"))

;; load environment config
(use-package env-config :demand :load-path (lambda () (list (concat "env/" EMACS_ENV_NAME "/"))))



(provide 'init)

;;; init.el ends here
