;;; markdown-dnd-test.el --- File drop regression tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'dnd)
(require 'markdown-mode)
(require 'url-util)

(defconst frei/markdown-dnd-test--var-directory
  (expand-file-name "../var/" (file-name-directory (or load-file-name buffer-file-name))))

(defmacro frei/markdown-dnd-test--environment (&rest body)
  "Evaluate BODY with a Markdown drop target and temporary file fixtures."
  (declare (indent 0))
  `(progn
     (make-directory frei/markdown-dnd-test--var-directory t)
     (let ((directory (make-temp-file
                       (expand-file-name "markdown-dnd-test-"
                                         frei/markdown-dnd-test--var-directory) t)))
       (unwind-protect
           (save-window-excursion
             (with-temp-buffer
               (insert "# Original document\n")
               (markdown-mode)
               (let ((source (current-buffer)))
                 (switch-to-buffer source)
                 ,@body)))
         (dolist (buffer (buffer-list))
           (when-let* ((file (buffer-file-name buffer))
                       ((file-in-directory-p file directory)))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))
         (unless (file-in-directory-p directory frei/markdown-dnd-test--var-directory)
           (error "Test cleanup path escaped var/"))
         (delete-directory directory t)))))

(defun frei/markdown-dnd-test--file (directory name)
  "Create a file NAME in DIRECTORY and return its path."
  (let ((file (expand-file-name name directory)))
    (with-temp-file file (insert "Original file contents\n"))
    file))

(defun frei/markdown-dnd-test--url (file)
  "Return a local file URI for FILE on Windows or POSIX."
  (if (eq system-type 'windows-nt)
      (progn
        (unless (fboundp 'w32-dropped-file-to-url)
          (load "term/w32-win" nil t))
        (w32-dropped-file-to-url file))
    (url-encode-url (concat "file://" file))))

(ert-deftest frei/markdown-dnd-default-action-opens-file-without-changing-source ()
  (frei/markdown-dnd-test--environment
    (let ((file (frei/markdown-dnd-test--file directory "drop target.txt"))
          (markdown-yank-dnd-method 'file-link))
      (dnd-handle-multiple-urls (selected-window)
                                (list (frei/markdown-dnd-test--url file)) 'private)
      (should (get-file-buffer file))
      (should (eq (window-buffer) (get-file-buffer file)))
      (should (equal (with-current-buffer source (buffer-string)) "# Original document\n")))))

(ert-deftest frei/markdown-dnd-multiple-files-do-not-insert-spaces-into-files ()
  (frei/markdown-dnd-test--environment
    (let* ((first (frei/markdown-dnd-test--file directory "first.txt"))
           (second (frei/markdown-dnd-test--file directory "第二个 file.txt"))
           (markdown-yank-dnd-method 'open))
      (dnd-handle-multiple-urls (selected-window)
                                (mapcar #'frei/markdown-dnd-test--url (list first second))
                                'private)
      (dolist (file (list first second))
        (should (get-file-buffer file))
        (with-current-buffer (get-file-buffer file)
          (should-not (buffer-modified-p))
          (should (equal (buffer-string) "Original file contents\n"))))
      (should (equal (with-current-buffer source (buffer-string)) "# Original document\n")))))

;;; markdown-dnd-test.el ends here
