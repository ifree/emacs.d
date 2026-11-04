;;; project-tool.el --- Projects anywhere, and a list of them  -*- lexical-binding: t; -*-

;;; Commentary:
;; Extends the built-in project.el:
;;
;; - Any folder becomes a project with `frei/project-add-directory'.
;;   Added folders and favorites come before version control, so part of
;;   a large repository, such as one Unreal Engine module, can be a
;;   project of its own.
;; - Other folders outside version control are projects when they hold
;;   a build file such as CMakeLists.txt or *.uproject.  The outermost
;;   one is the root, because vendored libraries bring their own.
;; - The files of these projects are listed by ripgrep, without the
;;   output folders of the usual build tools and of Unreal Engine.
;; - `frei/project-list' shows the favorites and the recent projects,
;;   each with the time its newest file changed.  Scanning a project
;;   the size of Unreal Engine takes seconds, so the times are found in
;;   the background, cached, and looked up again only now and then for
;;   large projects.
;; - `frei/project-switch-tab' opens or selects a project tab, using
;;   project.el and tab-bar.  Tabs live only in the current session.
;;
;; Favorites and added folders live in `custom-file', so every machine
;; keeps its own and none of it reaches the repository.

;;; Code:
(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'tab-bar)

(defvar project-files-relative-names)
(declare-function project-root "project")
(declare-function project-known-project-roots "project")
(declare-function project-remember-project "project")
(declare-function project-forget-project "project")

(defgroup frei/project nil
  "Projects anywhere, and a list of them."
  :group 'project)

(defcustom frei/project-favorites nil
  "Projects to keep at hand, as lists of a directory and a description.
They head the project list and are always offered by
\\[frei/project-switch-project].  Like added folders, a favorite is a
project of its own even inside a larger one.
`frei/project-add-favorite' adds one."
  :type '(repeat (list (directory :tag "Directory")
                       (string :tag "Description"))))

(defcustom frei/project-directories nil
  "Folders made projects by `frei/project-add-directory'."
  :type '(repeat directory))

(defcustom frei/project-root-markers
  '("CMakeLists.txt" "package.json" "pyproject.toml" "Cargo.toml" "go.mod"
    "*.uproject")
  "File names, wildcards allowed, that mark a project outside version control."
  :type '(repeat string))

(defcustom frei/project-ignored-directories
  '("node_modules" "build" "out" "target" "dist" "__pycache__"
    "Intermediate" "Binaries" "Saved" "DerivedDataCache")
  "Directories left out of projects outside version control.
Build output that often has no ignore file saying so; the last four
are those of Unreal Engine."
  :type '(repeat string))

(defcustom frei/project-list-recent-count 10
  "Number of recent projects listed below the favorites."
  :type 'integer)

(defcustom frei/project-time-file
  (expand-file-name "var/project-times.eld" user-emacs-directory)
  "File caching the modification times shown in the project list."
  :type 'file)

(defcustom frei/project-time-max-age 3600
  "Seconds a time that took long to find is shown before a new lookup.
Projects scanned in under two seconds are looked up whenever the list
is shown, and saving a file from Emacs updates its project at once."
  :type 'integer)

(defconst frei/project--slow-lookup 2.0
  "Seconds after which a lookup counts as slow.")

(defconst frei/project--lookup-jobs 2
  "Number of lookups running at the same time.")

(defconst frei/project--lookup-timeout 120
  "Seconds after which a lookup is given up.")

;;;; Directories

(defun frei/project--key (directory)
  "Return DIRECTORY in the form used to compare and cache project roots."
  (downcase (file-name-as-directory (expand-file-name directory))))

(defun frei/project--same-directory-p (a b)
  "Return non-nil when the directory names A and B are the same."
  (equal (frei/project--key a) (frei/project--key b)))

(defun frei/project--root-name (directory)
  "Return DIRECTORY as a project root: abbreviated, with a final slash."
  (abbreviate-file-name (file-name-as-directory (expand-file-name directory))))

(defun frei/project--exists-p (directory)
  "Return non-nil when DIRECTORY exists; remote ones are assumed to.
Checking them would open a connection."
  (or (file-remote-p directory) (file-directory-p directory)))

(defun frei/project--configured-roots ()
  "Return the added folders and the favorites."
  (append frei/project-directories (mapcar #'car frei/project-favorites)))

(defun frei/project--innermost (dir roots)
  "Return the innermost of ROOTS that contains DIR, or nil."
  (let ((key (frei/project--key dir))
        (best nil)
        (best-length -1))
    (dolist (root roots best)
      (let ((root-key (frei/project--key root)))
        (when (and (string-prefix-p root-key key)
                   (> (length root-key) best-length))
          (setq best root
                best-length (length root-key)))))))

(defun frei/project--marker-root (dir)
  "Return the outermost directory from DIR up that holds a root marker.
The home directory and the file system root are not considered."
  (let ((home (frei/project--key "~"))
        (regexp (mapconcat #'wildcard-to-regexp frei/project-root-markers "\\|"))
        (dir (file-name-as-directory (expand-file-name dir)))
        (root nil))
    (while (not (or (equal (frei/project--key dir) home)
                    (equal dir (file-name-directory (directory-file-name dir)))))
      (when (ignore-errors (directory-files dir nil regexp t 1))
        (setq root dir))
      (setq dir (file-name-directory (directory-file-name dir))))
    root))

;;;; The project backend

(defun frei/project-try-configured (dir)
  "Return the project of DIR when it lies in an added folder or a favorite.
Meant to run before version control, so that such a folder is a
project of its own inside a repository.  A folder that is the root of
a Git repository is left to the VC backend."
  (unless (file-remote-p dir)
    (when-let* ((root (frei/project--innermost dir (frei/project--configured-roots)))
                ((not (file-exists-p (expand-file-name ".git" root)))))
      (cons 'frei/directory (frei/project--root-name root)))))

(defun frei/project-try-markers (dir)
  "Return the project of DIR from root markers, outside version control."
  (unless (file-remote-p dir)
    (when-let* ((root (frei/project--marker-root dir)))
      (cons 'frei/directory (frei/project--root-name root)))))

(cl-defmethod project-root ((project (head frei/directory)))
  (cdr project))

(defun frei/project--rg-files-command (&rest arguments)
  "Return the ripgrep command that lists project files, plus ARGUMENTS.
Ignore files count also outside Git, and the directories of
`frei/project-ignored-directories' are left out."
  (append (list "rg" "--files" "--no-require-git" "--no-messages" "--path-separator" "/")
          (mapcan (lambda (name) (list "--glob" (concat "!" name "/")))
                  frei/project-ignored-directories)
          arguments))

(cl-defmethod project-files ((project (head frei/directory)) &optional dirs)
  "List the files of PROJECT with ripgrep, which honors ignore files.
The names stay relative when `project-files-relative-names' allows it;
expanding them takes a second on a project the size of Unreal Engine."
  (if (not (executable-find "rg"))
      (cl-call-next-method)
    (let ((relative (and (bound-and-true-p project-files-relative-names)
                         (null (cdr dirs)))))
      (mapcan
       (lambda (dir)
         (let* ((default-directory (file-name-as-directory (expand-file-name dir)))
                (files (apply #'process-lines-ignore-status
                              (frei/project--rg-files-command))))
           (if relative files (mapcar #'expand-file-name files))))
       (or dirs (list (project-root project)))))))

(defun frei/project-remember-configured ()
  "Make the added folders and the favorites known projects."
  (dolist (directory (frei/project--configured-roots))
    (when (frei/project--exists-p directory)
      (project-remember-project
       (cons 'frei/directory (file-name-as-directory (expand-file-name directory)))
       t t))))

;;;; Project tabs

(defvar frei/project-tab-open-hook nil
  "Hook run in a new project tab after selecting its initial buffer.")

(defun frei/project--current-tab ()
  "Return the current tab in the selected frame."
  (seq-find (lambda (tab) (eq (car tab) 'current-tab)) (tab-bar-tabs)))

(defun frei/project--tab-index (root)
  "Return the zero-based index of ROOT's tab, or nil if none exists."
  (seq-position
   (tab-bar-tabs) root
   (lambda (tab directory)
     (when-let* ((other (alist-get 'frei/project-root tab)))
       (frei/project--same-directory-p other directory)))))

(defun frei/project--name-tab (root)
  "Associate the current tab with ROOT and give it a unique short name."
  (let* ((tab (frei/project--current-tab))
         (base (file-name-nondirectory (directory-file-name root)))
         (names (mapcar (lambda (other) (alist-get 'name other))
                        (remq tab (tab-bar-tabs))))
         (name base)
         (number 1))
    (while (member name names)
      (setq number (1+ number)
            name (format "%s<%d>" base number)))
    (setf (alist-get 'frei/project-root (cdr tab)) root)
    (tab-bar-rename-tab name)))

(defun frei/project--adopt-tab ()
  "Associate an unassigned tab with the project already being edited.
Ignore project.el's temporary target override during project switching.
Scratch and project-list buffers do not turn into project tabs just
because they inherited a project directory."
  (when (and (not (alist-get 'frei/project-root (frei/project--current-tab)))
             (or buffer-file-name
                 (derived-mode-p 'dired-mode 'eshell-mode 'compilation-mode 'magit-mode)))
    (let ((project-current-directory-override nil))
      (when-let* ((project (project-current nil)))
        (let ((root (file-name-as-directory (expand-file-name (project-root project)))))
          (unless (frei/project--tab-index root)
            (frei/project--name-tab root)))))))

;;;###autoload
(defun frei/project-switch-tab (&optional directory)
  "Select the tab for DIRECTORY's project, creating one when necessary.
DIRECTORY defaults to the current project.  A new tab shows an existing
project file buffer, another project buffer, or the root directory,
then runs `frei/project-tab-open-hook'.  No session file is written."
  (interactive)
  (let* ((project (if directory
                      (let ((project-current-directory-override nil))
                        (project-current t directory))
                    (project-current t)))
         (root (file-name-as-directory (expand-file-name (project-root project)))))
    (frei/project--adopt-tab)
    (let ((index (frei/project--tab-index root)))
      (if index
          (tab-bar-select-tab (1+ index))
        (let* ((buffers (project-buffers project))
               (buffer (or (seq-find #'buffer-file-name buffers)
                           (car buffers)
                           (dired-noselect root)))
               (tab-bar-new-tab-choice (lambda () buffer)))
          (tab-bar-new-tab)
          (frei/project--name-tab root)
          (run-hooks 'frei/project-tab-open-hook))))))

;;;###autoload
(defun frei/project-switch-project (directory)
  "Select DIRECTORY's project tab, then offer project.el's command menu."
  (interactive (list (funcall project-prompter)))
  (frei/project-switch-tab directory)
  (project-switch-project directory))

;;;###autoload
(defun frei/project-previous-tab ()
  "Switch to the most recently used other project tab in this frame."
  (interactive)
  (let* ((candidates
          (cl-loop for tab in (tab-bar-tabs)
                   for index from 1
                   when (and (not (eq (car tab) 'current-tab))
                             (alist-get 'frei/project-root tab))
                   collect (cons index (or (alist-get 'time tab) 0))))
         (recent (car (sort candidates (lambda (a b) (> (cdr a) (cdr b)))))))
    (unless recent (user-error "No other project tab"))
    (tab-bar-select-tab (car recent))))

;;;; Adding and removing

(defun frei/project--save (variable value)
  "Set VARIABLE to VALUE and save it in `custom-file', unless unchanged."
  (unless (equal (symbol-value variable) value)
    (customize-save-variable variable value)))

(defun frei/project-add-directory (directory)
  "Make DIRECTORY a project of its own, whatever it holds.
It wins over a repository or a build file around it, so part of a
large project can be one.  `frei/project-remove' undoes it."
  (interactive (list (read-directory-name "Add project: " nil nil t)))
  (require 'project)
  (let ((root (frei/project--root-name directory)))
    (unless (seq-some (lambda (entry) (frei/project--same-directory-p entry root))
                      frei/project-directories)
      (frei/project--save 'frei/project-directories
                          (append frei/project-directories (list root))))
    (project-remember-project (cons 'frei/directory (expand-file-name root)))
    (frei/project-list--refresh)
    (message "Added project %s" root)))

(defun frei/project-add-favorite (directory description)
  "Add DIRECTORY to `frei/project-favorites' with DESCRIPTION and save it.
Offers the project on the current line of the project list, or else
the current project."
  (interactive
   (let ((default (or (get-text-property (point) 'frei/project-directory)
                      (when-let* ((project (project-current)))
                        (project-root project)))))
     (list (read-directory-name "Favorite project: " default default t)
           (read-string "Description: "))))
  (require 'project)
  (let ((root (frei/project--root-name directory)))
    (frei/project--save
     'frei/project-favorites
     (append (seq-remove (lambda (entry) (frei/project--same-directory-p (car entry) root))
                         frei/project-favorites)
             (list (list root description))))
    (project-remember-project (cons 'frei/directory (expand-file-name root)))
    (frei/project-list--refresh)
    (message "Added %s to the favorite projects" root)))

(defun frei/project-remove (directory)
  "Drop DIRECTORY from the favorites, the added folders and the known projects.
On the project list this is the project at point."
  (interactive
   (progn
     (require 'project)
     (let ((directory (or (get-text-property (point) 'frei/project-directory)
                          (completing-read "Remove project: "
                                           (seq-uniq (append (frei/project--configured-roots)
                                                             (project-known-project-roots)))
                                           nil t))))
       (unless (y-or-n-p (format "Remove %s from the projects? "
                                 (abbreviate-file-name directory)))
         (user-error "Nothing removed"))
       (list directory))))
  (require 'project)
  (let ((other (lambda (entry) (not (frei/project--same-directory-p entry directory)))))
    (frei/project--save 'frei/project-directories
                        (seq-filter other frei/project-directories))
    (frei/project--save 'frei/project-favorites
                        (seq-filter (lambda (entry) (funcall other (car entry)))
                                    frei/project-favorites)))
  (dolist (root (project-known-project-roots))
    (when (frei/project--same-directory-p root directory)
      (project-forget-project root)))
  (frei/project-list--refresh))

;;;; Modification times

(defvar frei/project--times nil
  "Hash table of root keys and (TIME CHECKED SECONDS), or nil before reading.
TIME is when the newest file changed, CHECKED when that was found, and
SECONDS how long finding it took.")

(defvar frei/project--times-changed nil
  "Non-nil when the cached times differ from `frei/project-time-file'.")

(defvar frei/project--lookup-queue nil
  "Pairs of root keys and roots waiting for a lookup.")

(defvar frei/project--lookups nil
  "Running lookup processes.")

(defun frei/project--times ()
  "Return the cache of modification times, reading it on first use."
  (or frei/project--times
      (let ((table (make-hash-table :test #'equal)))
        (when (file-readable-p frei/project-time-file)
          (with-temp-buffer
            (insert-file-contents frei/project-time-file)
            (pcase-dolist (`(,key . ,entry) (ignore-errors (read (current-buffer))))
              (puthash key entry table))))
        (setq frei/project--times table))))

(defun frei/project-save-times ()
  "Write the cached modification times, if they changed."
  (when frei/project--times-changed
    (let ((entries nil)
          (print-length nil)
          (print-level nil))
      (maphash (lambda (key entry) (push (cons key entry) entries)) frei/project--times)
      (make-directory (file-name-directory frei/project-time-file) t)
      (with-temp-file frei/project-time-file
        (insert ";;; -*- lisp-data -*-\n")
        (prin1 entries (current-buffer))
        (insert "\n")))
    (setq frei/project--times-changed nil)))

(defun frei/project--format-time (time)
  "Return TIME, seconds since the epoch, for the project list."
  (if time (format-time-string "%Y-%m-%d %H:%M" time) ""))

(defun frei/project--stale-p (key)
  "Return non-nil when the cached time of the project KEY needs a lookup."
  (pcase (gethash key (frei/project--times))
    ('nil t)
    (`(,_ ,checked ,seconds)
     (> (- (float-time) checked)
        (if (< seconds frei/project--slow-lookup) 0 frei/project-time-max-age)))))

(defun frei/project--record-time (key time seconds)
  "Cache TIME for the project KEY, found in SECONDS, and show it.
A failed lookup, with TIME nil, keeps the time known before."
  (let ((time (or time (car (gethash key (frei/project--times))))))
    (puthash key (list time (float-time) seconds) (frei/project--times))
    (setq frei/project--times-changed t)
    (frei/project-list--show-time key time)))

(defun frei/project--start-lookup (key root)
  "Start finding when the newest file of ROOT changed; return the process.
ROOT has the root KEY.  The newest file is the first one ripgrep lists
when sorting by modification time."
  (let* ((output "")
         (started (float-time))
         (default-directory root)
         (process
          (make-process
           :name "project-time" :noquery t :connection-type 'pipe :coding 'utf-8
           :command (frei/project--rg-files-command "--sortr=modified")
           :filter (lambda (process chunk)
                     ;; The first line is all that is needed.
                     (setq output (concat output chunk))
                     (when (string-search "\n" output)
                       (delete-process process)))
           :sentinel (lambda (process _event)
                       (unless (or (process-live-p process) (process-get process 'done))
                         (process-put process 'done t)
                         (cancel-timer (process-get process 'timer))
                         (let* ((file (car (split-string output "[\r\n]+" t)))
                                (attributes (and file (file-attributes
                                                       (expand-file-name file root)))))
                           (frei/project--record-time
                            key
                            (and attributes
                                 (float-time (file-attribute-modification-time attributes)))
                            (- (float-time) started)))
                         (frei/project--run-lookups))))))
    (process-put process 'key key)
    (process-put process 'timer
                 (run-at-time frei/project--lookup-timeout nil #'delete-process process))
    process))

(defun frei/project--run-lookups ()
  "Start queued lookups while fewer than allowed are running.
The cache is written once nothing is left to do."
  (setq frei/project--lookups (seq-filter #'process-live-p frei/project--lookups))
  (while (and frei/project--lookup-queue
              (< (length frei/project--lookups) frei/project--lookup-jobs))
    (pcase-let ((`(,key . ,root) (pop frei/project--lookup-queue)))
      (push (frei/project--start-lookup key root) frei/project--lookups)))
  (unless (or frei/project--lookups frei/project--lookup-queue)
    (frei/project-save-times)))

(defun frei/project--queue-lookup (root)
  "Look up when the newest file of ROOT changed, unless that is under way."
  (let ((key (frei/project--key root)))
    (unless (or (assoc key frei/project--lookup-queue)
                (seq-some (lambda (process)
                            (and (process-live-p process)
                                 (equal (process-get process 'key) key)))
                          frei/project--lookups))
      (setq frei/project--lookup-queue
            (append frei/project--lookup-queue (list (cons key root))))
      (frei/project--run-lookups))))

(defun frei/project-note-save ()
  "Record the file just saved as the latest change of its project.
Only projects that already have a cached time are touched."
  (when (and buffer-file-name (not (file-remote-p buffer-file-name)))
    (when-let* ((project (project-current))
                (key (frei/project--key (project-root project)))
                (entry (gethash key (frei/project--times))))
      (setcar entry (float-time))
      (setq frei/project--times-changed t)
      (frei/project-list--show-time key (car entry)))))

;;;; The project list

(define-button-type 'frei/project
  'action (lambda (button)
            (let ((directory (button-get button 'frei/project-directory)))
              (unless (frei/project--exists-p directory)
                (user-error "%s no longer exists; d removes it"
                            (abbreviate-file-name directory)))
              (frei/project-switch-project directory)))
  'face nil
  'follow-link t
  'help-echo "Switch to this project")

(defvar-keymap frei/project-list-mode-map
  "n" #'forward-button
  "p" #'backward-button
  "a" #'frei/project-add-favorite
  "d" #'frei/project-remove)

(define-derived-mode frei/project-list-mode special-mode "Projects"
  "Major mode of the list of favorite and recent projects.
The time of a project is when its newest file changed, among the
files that \\[project-find-file] offers.  \\[revert-buffer] looks all
of them up again.

\\{frei/project-list-mode-map}"
  (setq-local revert-buffer-function (lambda (&rest _) (frei/project-list--render t))
              header-line-format
              " RET open · a add favorite · d remove · g refresh · q quit"
              truncate-lines t))

(defconst frei/project-list--time-width 18
  "Columns taken by a modification time and the space after it.")

(defun frei/project-list--show-time (key time)
  "Show TIME on the line of the project KEY in the project list."
  (when-let* ((buffer (get-buffer "*Projects*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (pos (point-min)))
        (while (setq pos (text-property-not-all pos (point-max) 'frei/project-time nil))
          (when (equal (get-text-property pos 'frei/project-time) key)
            (put-text-property pos (1+ pos) 'display (frei/project--format-time time)))
          (setq pos (1+ pos)))))))

(defun frei/project-list--insert (directory description name-end path-end force)
  "Insert the line of the project in DIRECTORY with DESCRIPTION.
The modification time starts at column NAME-END, the path after it,
and the description at PATH-END.  The time shown is the cached one; a
lookup starts when it is stale, or always with FORCE."
  (let* ((root (file-name-as-directory (expand-file-name directory)))
         (exists (frei/project--exists-p root))
         (start (point)))
    (insert "  " (propertize (file-name-nondirectory (directory-file-name root))
                             'face (if exists 'bold 'shadow))
            (propertize " " 'display `(space :align-to ,name-end)))
    (when (and exists (not (file-remote-p root)) (executable-find "rg"))
      (let ((key (frei/project--key root)))
        (insert (propertize " " 'frei/project-time key
                            'display (frei/project--format-time
                                      (car (gethash key (frei/project--times))))))
        (when (or force (frei/project--stale-p key))
          (frei/project--queue-lookup root))))
    (insert (propertize " " 'display
                        `(space :align-to ,(+ name-end frei/project-list--time-width)))
            (propertize (abbreviate-file-name root) 'face 'shadow))
    (when (or description (not exists))
      (insert (propertize " " 'display `(space :align-to ,path-end))
              (or description "")
              (if exists "" (propertize " (not found)" 'face 'warning))))
    (make-text-button start (point) :type 'frei/project 'frei/project-directory root)
    (insert "\n")))

(defun frei/project-list--render (&optional force)
  "Fill the project list buffer with the favorites and recent projects.
With FORCE, look up every modification time again."
  (require 'project)
  (let* ((favorites frei/project-favorites)
         (recent (seq-take
                  (seq-remove (lambda (root)
                                (seq-some (lambda (entry)
                                            (frei/project--same-directory-p (car entry) root))
                                          favorites))
                              (project-known-project-roots))
                  frei/project-list-recent-count))
         (directories (append (mapcar #'car favorites) recent))
         (name-end (+ 4 (apply #'max 0 (mapcar (lambda (directory)
                                                  (string-width
                                                   (file-name-nondirectory
                                                    (directory-file-name directory))))
                                                directories))))
         (path-end (+ name-end frei/project-list--time-width 2
                      (apply #'max 0 (mapcar (lambda (directory)
                                               (string-width
                                                (frei/project--root-name directory)))
                                             directories))))
         (inhibit-read-only t))
    (erase-buffer)
    (if (null directories)
        (insert "\n  No projects yet.  Press a to add a favorite; projects used\n"
                "  with C-x p are remembered on their own.\n")
      (when favorites
        (insert "\n" (propertize "Favorites" 'face 'font-lock-keyword-face) "\n")
        (pcase-dolist (`(,directory ,description) favorites)
          (frei/project-list--insert directory description name-end path-end force)))
      (when recent
        (insert "\n" (propertize "Recent" 'face 'font-lock-keyword-face) "\n")
        (dolist (directory recent)
          (frei/project-list--insert directory nil name-end path-end force))))
    (goto-char (point-min))
    (forward-button 1 nil nil t)))

(defun frei/project-list--refresh ()
  "Redraw the project list, if it is shown, from the cached times."
  (when-let* ((buffer (get-buffer "*Projects*")))
    (with-current-buffer buffer
      (frei/project-list--render))))

;;;###autoload
(defun frei/project-list ()
  "Show the favorite projects and the recent ones, and return the buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Projects*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'frei/project-list-mode)
        (frei/project-list-mode))
      (frei/project-list--render))
    (switch-to-buffer buffer)))

(defun frei/project-list-at-startup ()
  "Show the project list when Emacs starts without files to visit.
`initial-buffer-choice' would also show it next to such files."
  (when (and (not noninteractive)
             (not (daemonp))
             (equal (buffer-name (window-buffer)) "*scratch*"))
    (frei/project-list)))

(provide 'project-tool)

;;; project-tool.el ends here
