;;; early-init.el --- Early startup settings  -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep early startup small.  Normal editor configuration belongs in init.el.

;;; Code:
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      frame-inhibit-implied-resize t
      inhibit-startup-screen t
      inhibit-startup-message t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024)
                  gc-cons-percentage 0.1)))

(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

(provide 'early-init)

;;; early-init.el ends here
