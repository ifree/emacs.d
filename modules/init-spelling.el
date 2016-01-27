;;; init-spelling.el --- Spelling check

;;; Commentary:
;; 


;;; Code:
(require 'ispell)

(when (executable-find ispell-program-name)
  (require 'init-flyspell))

(provide 'init-spelling)

;;; init-spelling.el ends here
