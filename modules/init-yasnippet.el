;;; init-yasnippet.el --- yas

;;; Commentary:
;; 


;;; Code:

(require-package 'yasnippet)
(require 'yasnippet nil t)

(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)


(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
