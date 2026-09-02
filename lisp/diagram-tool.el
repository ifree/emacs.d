;;; diagram-tool.el --- Show fenced diagram blocks as images  -*- lexical-binding: t; -*-

;;; Commentary:
;; `frei/diagram-render-buffer' turns the fenced diagram blocks of the
;; current buffer into inline images, and `frei/diagram-toggle-images'
;; switches between the images and the source.  Mermaid, Graphviz,
;; PlantUML, Vega, ECharts and drawio blocks are rendered to PNG by the
;; documd command line tool of docu.md, which needs Chrome; svg blocks
;; are shown directly.  `frei/diagram-available-formats' tells which
;; formats work on this machine.

;;; Code:
(require 'color)
(require 'seq)
(require 'subr-x)

(defgroup frei/diagram nil
  "Show fenced diagram blocks as images."
  :group 'tools)

(defcustom frei/diagram-program "documd"
  "The documd command of docu.md, installed with npm."
  :type 'string)

(defcustom frei/diagram-chrome nil
  "Chrome executable passed to documd, or nil to let it search."
  :type '(choice (const :tag "Let documd find Chrome" nil) file))

(defcustom frei/diagram-timeout 120
  "Seconds documd may spend on one diagram."
  :type 'integer)

(defcustom frei/diagram-png-scale 4
  "Device scale factor documd uses for PNG output."
  :type 'integer)

(defcustom frei/diagram-directory
  (expand-file-name "var/diagram/" user-emacs-directory)
  "Directory for the files exchanged with documd."
  :type 'directory)

(defconst frei/diagram-formats
  '((svg . "svg") (mermaid . "mmd") (dot . "dot") (plantuml . "puml")
    (vega . "vega") (vega-lite . "vl") (echarts . "echarts") (drawio . "drawio"))
  "Diagram formats and the file extension documd expects for each.")

(defconst frei/diagram--block-languages
  '(("svg" . svg) ("mermaid" . mermaid) ("dot" . dot) ("graphviz" . dot)
    ("plantuml" . plantuml) ("puml" . plantuml) ("vega" . vega)
    ("vega-lite" . vega-lite) ("echarts" . echarts) ("drawio" . drawio))
  "Fenced block languages and the diagram format they denote.")

(defconst frei/diagram--block-regexp
  (concat "^```\\(" (mapconcat #'car frei/diagram--block-languages "\\|")
          "\\)[ \t]*\n\\(\\(?:.*\n\\)*?\\)```[ \t]*$")
  "Regexp matching a fenced diagram block: group 1 language, group 2 source.")

;;;; Availability

(defun frei/diagram-format-available-p (format)
  "Return non-nil when diagrams in FORMAT can be shown here."
  (and (assq format frei/diagram-formats)
       (if (eq format 'svg)
           (image-type-available-p 'svg)
         (and (image-type-available-p 'png)
              (executable-find frei/diagram-program)
              t))))

(defun frei/diagram-available-formats ()
  "Return the diagram formats that can be shown on this machine."
  (seq-filter #'frei/diagram-format-available-p (mapcar #'car frei/diagram-formats)))

;;;; Rendering

(defun frei/diagram--background ()
  "Return the background color of the default face as a hex string."
  (let ((color (face-background 'default nil t)))
    (if (and (stringp color) (color-defined-p color))
        (apply #'color-rgb-to-hex (append (color-name-to-rgb color) '(2)))
      "#1e1e1e")))

(defun frei/diagram--work-file (extension)
  "Return a fresh file name with EXTENSION in `frei/diagram-directory'."
  (let ((directory (expand-file-name "work/" frei/diagram-directory)))
    (make-directory directory t)
    (make-temp-file (expand-file-name "diagram-" directory) nil (concat "." extension))))

(defun frei/diagram--file-bytes (file)
  "Return the contents of FILE as a unibyte string, or nil."
  (when (and (file-exists-p file) (> (file-attribute-size (file-attributes file)) 0))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (buffer-string))))

(defun frei/diagram-render (format source callback)
  "Render SOURCE in FORMAT and call CALLBACK with an image, or nil on failure.
The image is an image spec suitable for `insert-image'."
  (if (eq format 'svg)
      (funcall callback (frei/diagram--image source 'svg))
    (let* ((input (frei/diagram--work-file (cdr (assq format frei/diagram-formats))))
           (output (concat (file-name-sans-extension input) ".png"))
           (log (generate-new-buffer " *diagram*"))
           ;; Keep the browser out of the user's directories.
           (default-directory (file-name-directory input)))
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region source nil input nil 'silent))
      (make-process
       :name "documd" :buffer log :noquery t :connection-type 'pipe
       :command (append (list (executable-find frei/diagram-program) input output
                              "--timeout" (number-to-string frei/diagram-timeout))
                        (and frei/diagram-chrome (list "--chrome" frei/diagram-chrome)))
       :sentinel
       (lambda (process _event)
         (unless (process-live-p process)
           (let ((bytes (and (eql (process-exit-status process) 0)
                             (frei/diagram--file-bytes output))))
             (unless bytes
               (message "documd failed: %s"
                        (string-trim (with-current-buffer log (buffer-string)))))
             (kill-buffer log)
             (ignore-errors (delete-file input))
             (ignore-errors (delete-file output))
             (funcall callback (and bytes (frei/diagram--image bytes 'png))))))))))

;;;; Inline images

(defun frei/diagram--image (data type)
  "Return an image showing DATA of image TYPE, or nil when it cannot be shown."
  (when (and (display-images-p) (image-type-available-p type))
    (let* ((data (if (and (eq type 'svg) (not (string-match-p "xmlns=" data)))
                     (replace-regexp-in-string
                      "<svg\\b" "<svg xmlns=\"http://www.w3.org/2000/svg\"" data t t)
                   data))
           (scale (if (eq type 'png)
                      (/ (image-compute-scaling-factor image-scaling-factor)
                         (float frei/diagram-png-scale))
                    1.0)))
      (condition-case nil
          ;; Rendered diagrams assume a white page; hand written SVG is
          ;; asked to paint the editor background itself.
          (let ((image (create-image data type t
                                     :scale scale
                                     :background (if (eq type 'png) "white"
                                                   (frei/diagram--background))
                                     :max-width (- (window-body-width nil t) 40))))
            ;; Rendering now reveals data that the image library rejects.
            (image-size image t)
            image)
        (error nil)))))

(defun frei/diagram--blocks (&optional start end)
  "Return the diagram blocks between START and END as (START END FORMAT SOURCE).
The positions are markers.  START and END default to the whole buffer."
  (let ((blocks nil))
    (save-excursion
      (goto-char (or start (point-min)))
      (while (re-search-forward frei/diagram--block-regexp end t)
        (push (list (copy-marker (match-beginning 0))
                    (copy-marker (match-end 0))
                    (cdr (assoc (match-string 1) frei/diagram--block-languages))
                    (match-string-no-properties 2))
              blocks)))
    (nreverse blocks)))

(defun frei/diagram--shown-p (start end)
  "Return non-nil when a diagram image already covers START to END."
  (seq-some (lambda (overlay)
              (and (overlay-get overlay 'frei/diagram)
                   (<= (overlay-start overlay) start)
                   (>= (overlay-end overlay) end)))
            (overlays-in start end)))

(defun frei/diagram--overlays ()
  "Return the overlays that show diagrams as images in the current buffer."
  (seq-filter (lambda (overlay) (overlay-get overlay 'frei/diagram))
              (overlays-in (point-min) (point-max))))

(defun frei/diagram--show (buffer start end image)
  "Cover START to END of BUFFER with IMAGE."
  (when (and image (buffer-live-p buffer))
    (with-current-buffer buffer
      (dolist (old (overlays-in start end))
        (when (overlay-get old 'frei/diagram)
          (delete-overlay old)))
      (let ((overlay (make-overlay start end)))
        (overlay-put overlay 'frei/diagram t)
        (overlay-put overlay 'display image)
        (overlay-put overlay 'help-echo
                     "Diagram, M-x frei/diagram-toggle-images shows the source")))))

(defun frei/diagram-render-region (start end)
  "Show the diagram blocks between START and END that are not shown yet.
Rendering happens in the background; unsupported formats are left alone.
Return the number of blocks being rendered."
  (let ((buffer (current-buffer))
        (count 0))
    (pcase-dolist (`(,block-start ,block-end ,format ,source)
                   (frei/diagram--blocks start end))
      (when (and (frei/diagram-format-available-p format)
                 (not (frei/diagram--shown-p block-start block-end)))
        (setq count (1+ count))
        (frei/diagram-render
         format source
         (lambda (image) (frei/diagram--show buffer block-start block-end image)))))
    count))

;;;###autoload
(defun frei/diagram-render-buffer ()
  "Show every diagram block of the current buffer as an image.
Rendering happens in the background; unsupported formats are left alone.
Return the number of blocks being rendered."
  (interactive)
  (let ((count (frei/diagram-render-region (point-min) (point-max))))
    (when (called-interactively-p 'interactive)
      (message "Rendering %d diagram(s)" count))
    count))

;;;###autoload
(defun frei/diagram-toggle-images ()
  "Toggle between images and source text for the diagram blocks of the buffer."
  (interactive)
  (let ((overlays (frei/diagram--overlays)))
    (if overlays
        (progn (mapc #'delete-overlay overlays)
               (message "Showing diagram source"))
      (frei/diagram-render-buffer))))

(provide 'diagram-tool)

;;; diagram-tool.el ends here
