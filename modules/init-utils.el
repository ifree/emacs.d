;;; init-utils.el --- Utility functions

;;; Commentary:
;; 

;;; Code:

(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))



;; Delete the current file
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))



;; Rename the current file
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))


;; Browse current HTML file
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (browse-url (concat "file://" (buffer-file-name))))


;;Browse Current Folder
(defun browse-current-folder ()
  "Open the current buffer's folder."
  (interactive)
  (browse-url (concat "file://" (file-name-directory (buffer-file-name)))))

;auto create dir
(defun create-dir-when-not-exists()
  (let (
	(dir (file-name-directory buffer-file-name))
	)
    (when (and (not (file-exists-p dir))
	       (y-or-n-p (format "Directory %s does not exist, Create new one ? " dir)))
      (progn
	(make-directory dir t)
	(if buffer-read-only
	    (read-only-mode 0))))))
;; this function must at first, and return nil for process other functions
(add-to-ordered-list 'find-file-not-found-functions 'create-dir-when-not-exists)


(provide 'init-utils)

;;; init-utils.el ends here
