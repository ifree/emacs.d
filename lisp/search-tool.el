;;; search-tool.el --- Search conveniences  -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin wrappers around the public isearch API.

;;; Code:
(require 'isearch)

(defun frei/isearch-backward-symbol-at-point ()
  "Search backward for the symbol at point.
The backward counterpart of `isearch-forward-symbol-at-point'."
  (interactive)
  (let ((bounds (find-tag-default-bounds)))
    (unless bounds
      (user-error "No symbol at point"))
    (goto-char (car bounds))
    (isearch-mode nil nil nil nil #'isearch-symbol-regexp)
    (isearch-yank-string (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(global-set-key (kbd "C-*") #'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-#") #'frei/isearch-backward-symbol-at-point)

(provide 'search-tool)

;;; search-tool.el ends here
