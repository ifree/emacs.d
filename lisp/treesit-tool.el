;;; treesit-tool.el --- Install tree-sitter grammars  -*- lexical-binding: t; -*-

;;; Commentary:
;; `frei/treesit-install-grammars' puts grammar libraries into the
;; tree-sitter directory of `user-emacs-directory'.  Windows and Linux on
;; x86-64 get the prebuilt bundle of casouri/tree-sitter-module, which
;; needs no compiler.  Other systems compile the grammars listed in
;; `frei/treesit-language-sources' with `treesit-install-language-grammar'.

;;; Code:
(require 'treesit)
(require 'url)

(defconst frei/treesit-prebuilt-url
  "https://github.com/casouri/tree-sitter-module/releases/latest/download/libs-%s.zip"
  "Download URL of the prebuilt grammar bundles, with a slot for the platform.")

(defconst frei/treesit-language-sources
  '((python "https://github.com/tree-sitter/tree-sitter-python")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (java "https://github.com/tree-sitter/tree-sitter-java")
    (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
    (bash "https://github.com/tree-sitter/tree-sitter-bash")
    (json "https://github.com/tree-sitter/tree-sitter-json"))
  "Grammars compiled from source when no prebuilt bundle exists.
Entries have the form of `treesit-language-source-alist'.")

(defun frei/treesit-grammar-directory ()
  "Return the directory where grammars are installed."
  (expand-file-name "tree-sitter/" user-emacs-directory))

(defun frei/treesit--prebuilt-bundle ()
  "Return the prebuilt bundle name for this machine, or nil."
  (let ((cpu (car (split-string system-configuration "-"))))
    (pcase system-type
      ('windows-nt (and (member cpu '("x86_64" "amd64")) "windows-x64"))
      ('gnu/linux (and (equal cpu "x86_64") "linux-x64"))
      ('darwin (and (equal cpu "x86_64") "macos-x64")))))

(defun frei/treesit--unzip (zip directory)
  "Extract ZIP into DIRECTORY."
  (let ((status
         (cond
          ((eq system-type 'windows-nt)
           (call-process "powershell" nil nil nil "-NoProfile" "-Command"
                         (format "Expand-Archive -LiteralPath '%s' -DestinationPath '%s' -Force"
                                 (convert-standard-filename zip)
                                 (convert-standard-filename directory))))
          ((executable-find "unzip")
           (call-process "unzip" nil nil nil "-o" "-q" zip "-d" directory))
          (t
           (user-error "The unzip command is needed to extract %s" zip)))))
    (unless (eql status 0)
      (error "Extracting %s failed with status %s" zip status))))

(defun frei/treesit--install-prebuilt (bundle directory)
  "Download the prebuilt BUNDLE and copy its grammars into DIRECTORY.
Return the number of installed grammars."
  (let ((zip (make-temp-file "tree-sitter-" nil ".zip"))
        (extracted (make-temp-file "tree-sitter-" t))
        (count 0))
    (unwind-protect
        (progn
          (message "Downloading the %s grammar bundle…" bundle)
          (url-copy-file (format frei/treesit-prebuilt-url bundle) zip t)
          (frei/treesit--unzip zip extracted)
          (dolist (file (directory-files-recursively
                         extracted "\\`libtree-sitter-.*\\.\\(dll\\|so\\|dylib\\)\\'"))
            (copy-file file (expand-file-name (file-name-nondirectory file) directory) t)
            (setq count (1+ count))))
      (ignore-errors (delete-file zip))
      (ignore-errors (delete-directory extracted t)))
    count))

(defun frei/treesit--compile-grammars (directory)
  "Compile `frei/treesit-language-sources' into DIRECTORY.
Return the number of installed grammars."
  (let ((count 0))
    (dolist (source frei/treesit-language-sources)
      (unless (assq (car source) treesit-language-source-alist)
        (push source treesit-language-source-alist)))
    (dolist (source frei/treesit-language-sources)
      (condition-case err
          (progn
            (message "Compiling the %s grammar…" (car source))
            (treesit-install-language-grammar (car source) directory)
            (setq count (1+ count)))
        (error (message "Skipping %s: %s" (car source) (error-message-string err)))))
    count))

;;;###autoload
(defun frei/treesit-install-grammars ()
  "Install tree-sitter grammars into `frei/treesit-grammar-directory'.
A prebuilt bundle is used where one exists, otherwise the grammars are
compiled, which needs git and a C compiler."
  (interactive)
  (unless (treesit-available-p)
    (user-error "This Emacs was built without tree-sitter support"))
  (let ((directory (frei/treesit-grammar-directory))
        (bundle (frei/treesit--prebuilt-bundle)))
    (make-directory directory t)
    (let ((count (if bundle
                     (frei/treesit--install-prebuilt bundle directory)
                   (frei/treesit--compile-grammars directory))))
      (message "Installed %d tree-sitter grammars in %s" count directory))))

(provide 'treesit-tool)

;;; treesit-tool.el ends here
