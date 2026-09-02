;;; search-tool.el --- Search conveniences  -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin wrappers around the public isearch API.

;;; Code:
(require 'isearch)
(require 'thingatpt)

(defun frei/isearch-backward-symbol-at-point ()
  "Start a backward regexp search for the symbol at point."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (unless symbol
      (user-error "No symbol at point"))
    (isearch-backward-regexp nil 1)
    (isearch-yank-string
     (concat "\\_<" (regexp-quote symbol) "\\_>"))))

(global-set-key (kbd "C-*") #'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-#") #'frei/isearch-backward-symbol-at-point)

(provide 'search-tool)

;;; search-tool.el ends here
