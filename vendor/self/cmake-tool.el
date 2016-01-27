;;; cmake-tool.el --- CMake utility functions
;;; Commentary:
;; 


;;; Code:
(require 'cmake-mode)

(defvar cmake-tool-target-var-name "cmake-tool-target-var"
  "Variable name that cmake-tool will look for to manipulate.")

(defun cmake-tool-collect-func (fun name)
  "Collect FUN(ction) content by NAME."
  (let ((reg (concat fun "[ \t]*([ \t]*" name "[ \t]+[^)]+)"))
        ret start end tmp)
    (save-excursion
      (while (re-search-forward reg nil t)
        (setq start (match-beginning 0)
              end (match-end 0)
              tmp '())
        (goto-char start)
        (while (re-search-forward cmake-regex-token end t)
          (setq tmp (cons (match-string-no-properties 0) tmp)))
        (setq ret (cons (reverse tmp) ret))))
    ret))


(defun cmake-tool-add-to-list (name &rest items)
  "Add ITEMS to variable."
  (let ((name (regexp-quote name))
        (reg (concat "set[ \t]*([ \t]*"
                     name "[ \t]+[^)]+)\\|list[ \t]*([ \t]*append[ \t]+"
                     name "[ \t]+[^)]+)"))
        start end should-insert)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward reg nil t)
        (setq start (match-beginning 0)
              end (match-end 0)
              should-insert nil)
        (goto-char start)
        ;; walk over token list
        (while (and (not should-insert) (re-search-forward cmake-regex-token end t))
          (let ((token (match-string-no-properties 0))
                (case-fold-search nil));turn on case sensitive search
            (when (string-match-p "CACHE\\|PARENT_SCOPE" token) ;insert before keywords
              (setq should-insert t))))
        (goto-char (match-beginning 0))
        (apply 'insert (mapcar (lambda (x) (format "%s " x)) items))))))

(defun cmake-tool-remove-from-list (name val)
  "Remove val from list."
  (let* ((name (regexp-quote name))
         (val (regexp-quote val))
         (reg (concat "set[ \t]*([ \t]*"
                      name "[ \t]+.*?\\("
                      val
                      "\\).*?)\\|list[ \t]*([ \t]*append[ \t]+"
                      name "[ \t]+.*?\\(" val "\\).*?)")))
    (re-search-forward reg)
    (when (match-beginning 1)
      (replace-match "" t nil nil 1))
    (when (match-beginning 2)
      (replace-match "" t nil nil 2))))

(defun cmake-tool-modify-list (addp)
  "You need to add this function to file save hook, to modify CMakeLists.txt."
  (interactive)
  ;; only care about c/cpp files
  ;; for insert need ensure file is newly created
  ;; for delete need ensure file was deleted
  (when (and
         (condition-case err
             (string-match-p "\.\\(cc\\|cpp\\|c\\)$" (buffer-file-name))
           (error nil))
         (not (file-exists-p (buffer-file-name)))
         (projectile-project-p))
    (message "orz")
    (let* ((file (buffer-file-name))
         (dir (file-name-directory file))
         (root (projectile-project-root)) ;you must in a project
         (cmake-file (concat dir "CMakeLists.txt"))
         (func (if addp
                   'cmake-tool-add-to-list
                 'cmake-tool-remove-from-list)))
        
    (while (and (not (file-exists-p cmake-file))
                (not (equal root
                            (file-name-directory cmake-file))))
      (setq cmake-file
            (concat
             (file-name-directory
              (directory-file-name (file-name-directory cmake-file)))
             "CMakeLists.txt")))
    
    (when (file-exists-p cmake-file)
      (message "cmake file found. %s" cmake-file)
      ;; search target variable
      (with-current-buffer (get-file-buffer cmake-file)
        (save-excursion
          (goto-char (point-min))
          ;; only search once
          (when (re-search-forward (concat "-\\*-.*\\("
                                           cmake-tool-target-var-name
                                           ":\s*\\([^\s]+\\)\s*\\)"))
            
            (funcall func 
                     (match-string-no-properties 2)
                     (file-relative-name
                      file
                      (file-name-directory cmake-file))))))))))


(defun cmake-tool-before-file-saved ()
  (interactive)
  (cmake-tool-modify-list t))

(defun cmake-tool-before-file-deleted ()
  (interactive)
  (cmake-tool-modify-list nil) nil)


(provide 'cmake-tool)

;;; cmake-tool.el ends here
