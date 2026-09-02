;;; cpp-tool.el --- C++ utility commands  -*- lexical-binding: t; -*-

;;; Commentary:
;; A small single-file compile command for quick experiments.

;;; Code:
(defun frei/compile-current-cpp-file (&optional file)
  "Compile FILE, or the current buffer file, as a C++20 program."
  (interactive)
  (let* ((source-file
          (expand-file-name
           (or file
               buffer-file-name
               (user-error "The current buffer is not visiting a file"))))
         (compiler (or (getenv "CXX") "clang++"))
         (flags
          (split-string-and-unquote
           (or (getenv "CXXFLAGS")
               "-std=c++20 -Wall -Wextra -g")))
         (output-file
          (concat
           (file-name-sans-extension source-file)
           (if (eq system-type 'windows-nt) ".exe" "")))
         (arguments
          (append
           (list compiler)
           flags
           (list "-o" output-file source-file)))
         (command
          (mapconcat #'shell-quote-argument arguments " ")))
    (compile command)))

(defalias 'compile-this-file #'frei/compile-current-cpp-file)

(provide 'cpp-tool)

;;; cpp-tool.el ends here
