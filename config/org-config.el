;;; org-config.el --- Org settings  -*- lexical-binding: t; -*-

;;; Commentary:
;; A small setup built on the Org version bundled with Emacs.

;;; Code:
(use-package org
  :ensure nil
  :hook (org-mode . visual-line-mode)
  :custom
  (org-startup-with-inline-images t)
  (org-edit-src-content-indentation 0))

(provide 'org-config)

;;; org-config.el ends here
