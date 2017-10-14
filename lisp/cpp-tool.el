;;; cpp-tool.el --- cpp utility functions

;;; Commentary:
;; 

;;;###autoload
(defun compile-this-file (file)
  (interactive "fFile")
  (let ((cmd (format "%s -std=c++14 -o %s %s %s"
		     (or (getenv "CC") "clang++")
		     (file-name-sans-extension file)
		     (or (getenv "CPPFLAGS") "-Wall -g")
		     file
		     )))
    (compile cmd)))



(provide 'cpp-tool)

;;; cpp-tool.el ends here
