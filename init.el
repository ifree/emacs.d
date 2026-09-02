;;; init.el --- Emacs configuration entry point  -*- lexical-binding: t; -*-

;;; Commentary:
;; A small entry point with environment-specific modules.

;;; Code:
(when (< emacs-major-version 30)
  (error "This configuration requires Emacs 30 or newer"))

(require 'package)

(defconst frei/config-directory
  (file-name-as-directory
   (file-name-directory
    (or load-file-name user-init-file user-emacs-directory)))
  "Root directory of this Emacs configuration.")

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("gnu" . 30)
        ("nongnu" . 20)
        ("melpa" . 10)))

(setq custom-file (expand-file-name "custom.el" frei/config-directory))
(load custom-file 'noerror 'nomessage)

(add-to-list 'load-path (expand-file-name "config/" frei/config-directory))
(add-to-list 'load-path (expand-file-name "lisp/" frei/config-directory))

(require 'use-package)
(setq use-package-verbose nil)

(unless (eq system-type 'windows-nt)
  (use-package exec-path-from-shell
    :ensure t
    :if (or (daemonp)
            (memq window-system '(mac ns x pgtk)))
    :custom
    (exec-path-from-shell-variables
     '("EMACS_ENV_NAME" "SSH_AUTH_SOCK" "SSH_AGENT_PID"
       "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    :config
    (exec-path-from-shell-initialize)))

(defconst frei/environment-name
  (or (getenv "EMACS_ENV_NAME") "default")
  "Name of the environment-specific configuration to load.")

(unless (member frei/environment-name '("default" "home" "work"))
  (error "Unknown Emacs environment: %s" frei/environment-name))

(let ((environment-directory
       (expand-file-name
        (format "env/%s/" frei/environment-name)
        frei/config-directory)))
  (unless (file-directory-p environment-directory)
    (error "Missing Emacs environment directory: %s" environment-directory))
  (add-to-list 'load-path environment-directory)
  (require 'env-config))

(provide 'init)

;;; init.el ends here
