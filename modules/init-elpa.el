;;; init-elpa.el --- Package mgmt


;;; Commentary:
;; 

;;; Code:

(require 'package)

;;; Standard package repositories

(when (< emacs-major-version 24)
  ;; Mainly for ruby-mode
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))

;; We include the org repository for completeness, but don't normally
;; use it.
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;; Also use Melpa for most packages
(add-to-list 'package-archives `("melpa" . ,(if (< emacs-major-version 24)
                                                "http://melpa.org/packages/"
                                              "https://melpa.org/packages/")))


;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install package `%s': %S" package err)
     nil)))


;;; Fire up package.el

(setq package-enable-at-startup nil)
(package-initialize)

;; init exec path
(require-package 'exec-path-from-shell)

(exec-path-from-shell-initialize)

(provide 'init-elpa)

;;; init-elpa.el ends here
