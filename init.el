;;; init.el --- Boostrap the configuration

;;; Commentary:
;; Configuration entry point.

;;; Code:
(when (< emacs-major-version 24)
  (message "Your emacs is too old, please upgrade it."))

;; global variables
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d"))

(defconst *is-win* (eq system-type 'windows-nt))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-mac* (eq system-type 'darwin))

;; gc
(setq gc-cons-threshold (* 128 1024 1024))

;; setup load path
(defun add-dir-to-load-path (parent-dir)
 "Add all subdirs from PARENT-DIR to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)))))

(add-to-list 'load-path (expand-file-name "environments" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-dir-to-load-path (expand-file-name "vendor" user-emacs-directory))

;; allow access from emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; load environment
(require 'custom-environment)
(require 'minimal-env)
(require 'common-env)
(require 'dev-env)
(require 'home-env)
(require 'work-env)

;; You can override your environment by set environment variable `EMACS-ENV-NAME'
(if-let ((envs (getenv "EMACS-ENV-NAME-LIST")))
    (dolist (env (split-string envs))
      (custom-environment-load env))
  (custom-environment-load "common"))


;; load customize file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
