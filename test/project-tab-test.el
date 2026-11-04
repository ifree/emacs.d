;;; project-tab-test.el --- Project tab integration tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'dired)
(require 'project-tool)

(defconst frei/project-tab-test--var-directory
  (expand-file-name "../var/" (file-name-directory (or load-file-name buffer-file-name))))

(defvar frei/project-tab-test--directory)

(defmacro frei/project-tab-test--environment (&rest body)
  "Evaluate BODY with temporary projects and an isolated frame tab list."
  (declare (indent 0))
  `(progn
     (make-directory frei/project-tab-test--var-directory t)
     (let* ((frei/project-tab-test--directory
             (make-temp-file (expand-file-name "project-tab-test-"
                                              frei/project-tab-test--var-directory) t))
            (project-list-file (expand-file-name "projects.eld" frei/project-tab-test--directory))
            (project--list nil)
            (project-find-functions '(frei/project-try-configured))
            (project-switch-commands #'frei/project-switch-tab)
            ;; Git status jobs can keep fixture directories open on Windows.
            (dired-mode-hook (remove #'diff-hl-dired-mode dired-mode-hook))
            (frei/project-directories nil)
            (frei/project-favorites nil)
            (frei/project-tab-open-hook nil)
            (tab-bar-mode t)
            (tab-bar-history-mode nil)
            (tab-bar-closed-tabs nil)
            (saved-tabs (frame-parameter nil 'tabs))
            (home (generate-new-buffer " *project-tab-test-home*")))
       (unwind-protect
           (save-window-excursion
             (delete-other-windows)
             (switch-to-buffer home)
             (setq-local default-directory frei/project-tab-test--directory)
             (set-frame-parameter nil 'tabs nil)
             (tab-bar-tabs)
             ,@body)
         (set-frame-parameter nil 'tabs saved-tabs)
         (dolist (buffer (buffer-list))
           (when (or (eq buffer home)
                     (with-current-buffer buffer
                       (string-prefix-p frei/project-tab-test--directory default-directory)))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (when-let* ((process (get-buffer-process buffer)))
               (set-process-sentinel process #'ignore)
               (when (process-live-p process) (delete-process process)))
             (kill-buffer buffer)))
         ;; Only remove the test directory created under this checkout's var/.
         (unless (file-in-directory-p frei/project-tab-test--directory
                                      frei/project-tab-test--var-directory)
           (error "Test cleanup path escaped var/"))
         (delete-directory frei/project-tab-test--directory t)))))

(defun frei/project-tab-test--project (name)
  "Create and register a test project named NAME."
  (let ((root (file-name-as-directory (expand-file-name name frei/project-tab-test--directory))))
    (make-directory root t)
    (push root frei/project-directories)
    root))

(defun frei/project-tab-test--file (root &optional name)
  "Open a file under ROOT, using NAME or file.txt."
  (let ((file (expand-file-name (or name "file.txt") root)))
    (with-temp-file file (insert "Project tab test\n"))
    (find-file-noselect file)))

(defun frei/project-tab-test--root ()
  "Return the project root attached to the current tab."
  (alist-get 'frei/project-root (frei/project--current-tab)))

(ert-deftest frei/project-tab-switch-creates-and-reuses-tabs ()
  (frei/project-tab-test--environment
    (let ((a (frei/project-tab-test--project "alpha"))
          (b (frei/project-tab-test--project "beta")))
      (project-switch-project a)
      (should (equal (frei/project-tab-test--root) a))
      (should (derived-mode-p 'dired-mode))
      (should (equal default-directory a))
      (should-not (local-variable-p 'project-current-directory-override home))
      (project-switch-project b)
      (should (equal (frei/project-tab-test--root) b))
      (project-switch-project (expand-file-name "./" a))
      (should (equal (frei/project-tab-test--root) a))
      (should (= (length (tab-bar-tabs)) 3))
      (project-switch-project a)
      (should (= (length (tab-bar-tabs)) 3)))))

(ert-deftest frei/project-tab-switch-keeps-the-projects-window-layout ()
  (frei/project-tab-test--environment
    (let* ((a (frei/project-tab-test--project "alpha"))
           (b (frei/project-tab-test--project "beta"))
           (first (frei/project-tab-test--file a "one.txt"))
           (second (frei/project-tab-test--file a "two.txt")))
      (project-switch-project a)
      (switch-to-buffer first)
      (goto-char 4)
      (set-window-buffer (split-window-right) second)
      (project-switch-project b)
      (should (= (length (window-list)) 1))
      (project-switch-project a)
      (should (= (length (window-list)) 2))
      (should (eq (window-buffer) first))
      (should (= (point) 4))
      (should (memq second (mapcar #'window-buffer (window-list)))))))

(ert-deftest frei/project-tab-switch-adopts-an-existing-project-tab ()
  (frei/project-tab-test--environment
    (let* ((a (frei/project-tab-test--project "alpha"))
           (b (frei/project-tab-test--project "beta"))
           (file (frei/project-tab-test--file a)))
      (switch-to-buffer file)
      (project-switch-project b)
      (should (equal (frei/project-tab-test--root) b))
      (should (equal (alist-get 'frei/project-root (car (tab-bar-tabs))) a))
      (project-switch-project a)
      (should (eq (current-buffer) file))
      (should (= (length (tab-bar-tabs)) 2)))))

(ert-deftest frei/project-tab-switch-prefers-an-existing-project-file ()
  (frei/project-tab-test--environment
    (let* ((a (frei/project-tab-test--project "alpha"))
           (b (frei/project-tab-test--project "beta"))
           (file-a (frei/project-tab-test--file a))
           (file-b (frei/project-tab-test--file b)))
      (switch-to-buffer file-a)
      (project-switch-project b)
      (should (eq (current-buffer) file-b))
      (should-not (local-variable-p 'project-current-directory-override file-a)))))

(ert-deftest frei/project-tab-switch-disambiguates-same-named-roots ()
  (frei/project-tab-test--environment
    (let ((a (frei/project-tab-test--project "left/app"))
          (b (frei/project-tab-test--project "right/app")))
      (project-switch-project a)
      (let ((name (alist-get 'name (frei/project--current-tab))))
        (project-switch-project b)
        (should-not (equal name (alist-get 'name (frei/project--current-tab)))))
      (project-switch-project a)
      (should (equal default-directory a))
      (project-switch-project b)
      (should (equal default-directory b))
      (should (= (length (tab-bar-tabs)) 3)))))

(ert-deftest frei/project-tab-switch-reuses-renamed-tabs ()
  (frei/project-tab-test--environment
    (let ((a (frei/project-tab-test--project "alpha"))
          (b (frei/project-tab-test--project "beta")))
      (project-switch-project a)
      (tab-bar-rename-tab "My project")
      (project-switch-project b)
      (project-switch-project a)
      (should (equal (alist-get 'name (frei/project--current-tab)) "My project"))
      (should (= (length (tab-bar-tabs)) 3)))))

(ert-deftest frei/project-tab-previous-skips-unassigned-tabs ()
  (frei/project-tab-test--environment
    (let ((a (frei/project-tab-test--project "alpha"))
          (b (frei/project-tab-test--project "beta")))
      (project-switch-project a)
      (should-error (frei/project-previous-tab) :type 'user-error)
      (project-switch-project b)
      (tab-bar-select-tab 1)
      (frei/project-previous-tab)
      (should (equal (frei/project-tab-test--root) b))
      (frei/project-previous-tab)
      (should (equal (frei/project-tab-test--root) a))
      (frei/project-previous-tab)
      (should (equal (frei/project-tab-test--root) b)))))

(ert-deftest frei/project-tab-close-keeps-files-available-for-reopening ()
  (frei/project-tab-test--environment
    (let* ((a (frei/project-tab-test--project "alpha"))
           (file (frei/project-tab-test--file a)))
      (project-switch-project a)
      (tab-bar-close-tab)
      (should (buffer-live-p file))
      (project-switch-project a)
      (should (eq (current-buffer) file))
      (should (= (length (tab-bar-tabs)) 2)))))

(ert-deftest frei/project-tab-switch-keeps-the-command-menu-in-the-target-tab ()
  (frei/project-tab-test--environment
    (let* ((root (frei/project-tab-test--project "alpha"))
           (project-switch-commands '((ignore "Other" ?o)))
           (events nil)
           (frei/project-tab-open-hook
            (list (lambda () (push (list 'open (frei/project-tab-test--root)) events)))))
      (cl-letf (((symbol-function 'project--switch-project-command)
                 (lambda (directory)
                   (should (listp project-switch-commands))
                   (push (list 'menu directory (frei/project-tab-test--root)) events)
                   (lambda ()
                     (interactive)
                     (push (list 'command
                                 (expand-file-name (project-root (project-current t))))
                           events)))))
        (frei/project-switch-project root)
        (should (equal (nreverse events)
                       `((open ,root) (menu ,root ,root) (command ,root)))))
      (should-not (local-variable-p 'project-current-directory-override)))))

(ert-deftest frei/project-tab-new-project-opens-the-sidebar-and-keeps-main-focus ()
  (skip-unless (fboundp 'frei/project-tab-sidebar))
  (frei/project-tab-test--environment
    (let* ((a (frei/project-tab-test--project "alpha"))
           (b (frei/project-tab-test--project "beta"))
           (file-a (frei/project-tab-test--file a))
           (file-b (frei/project-tab-test--file b))
           (project-switch-commands #'ignore)
           (frei/project-tab-open-hook '(frei/project-tab-sidebar)))
      (dolist (entry (list (cons a file-a) (cons b file-b) (cons a file-a)))
        (frei/project-switch-project (car entry))
        (should (eq (current-buffer) (cdr entry)))
        (should (dired-sidebar-showing-sidebar-p))
        (let ((sidebar (dired-sidebar-buffer)))
          (should (equal (with-current-buffer sidebar (dired-current-directory)) (car entry)))
          (should-not (eq (selected-window) (get-buffer-window sidebar))))))))

(ert-deftest frei/project-tab-adoption-does-not-duplicate-an-existing-project ()
  (frei/project-tab-test--environment
    (let* ((a (frei/project-tab-test--project "alpha"))
           (b (frei/project-tab-test--project "beta"))
           (file (frei/project-tab-test--file a)))
      (project-switch-project a)
      (let ((tab-bar-new-tab-choice (lambda () file)))
        (tab-bar-new-tab))
      (project-switch-project b)
      (should (= (cl-count-if
                  (lambda (tab) (equal (alist-get 'frei/project-root tab) a))
                  (tab-bar-tabs))
                 1)))))

;;; project-tab-test.el ends here
