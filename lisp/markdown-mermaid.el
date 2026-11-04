;;; markdown-mermaid.el --- Inline Mermaid SVG in Markdown  -*- lexical-binding: t; -*-

;;; Commentary:
;; `frei/markdown-mermaid-mode' displays closed Mermaid fences as SVG.
;; Pele runs in a local Node process, once per batch of uncached diagrams.
;; C-c C-x m toggles the block at point; C-c C-x M toggles every block.
;; C-c C-x +, - and 0 resize the diagram at point relative to window fit.
;; Source text is preserved, including when a diagram cannot be rendered.

;;; Code:
(require 'cl-lib)
(require 'color)
(require 'json)
(require 'markdown-mode)
(require 'seq)
(require 'subr-x)

(defgroup frei/markdown-mermaid nil
  "Read Mermaid diagrams inside Markdown buffers."
  :group 'markdown)

(defcustom frei/markdown-mermaid-node-program "node"
  "Node executable used to run Pele (Node 20 or newer)."
  :type 'string)

(defcustom frei/markdown-mermaid-idle-delay 0.5
  "Seconds to wait after an edit before refreshing diagrams."
  :type 'number)

(defcustom frei/markdown-mermaid-timeout 15
  "Maximum seconds allowed for one batch of diagrams."
  :type 'number)

(defcustom frei/markdown-mermaid-font-family nil
  "Diagram font family, or nil to choose an installed CJK-capable font."
  :type '(choice (const :tag "Choose automatically" nil) string))

(defconst frei/markdown-mermaid--directory
  (expand-file-name "../tools/markdown-mermaid/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing the renderer and its local npm dependencies.")

(defvar-local frei/markdown-mermaid--overlays nil)
(defvar-local frei/markdown-mermaid--cache nil)
(defvar-local frei/markdown-mermaid--timer nil)
(defvar-local frei/markdown-mermaid--process nil)
(defvar-local frei/markdown-mermaid--show-images t)
(defvar-local frei/markdown-mermaid--width nil)
(defvar-local frei/markdown-mermaid--last-error nil)
(defvar frei/markdown-mermaid-mode)

(defun frei/markdown-mermaid--blocks ()
  "Return closed Mermaid fences as (START END SOURCE), in document order.
Use Markdown's syntax properties so fences inside other code blocks
are ignored and fence lengths and indentation follow `markdown-mode'."
  (save-excursion
    (save-restriction
      (widen)
      (syntax-propertize (point-max))
      (let (blocks)
        (dolist (property '(markdown-gfm-block-begin markdown-tilde-fence-begin))
          (goto-char (point-min))
          (let (location)
            (while (setq location (markdown-find-next-prop property))
              (let* ((start (car location))
                     (language (save-excursion (markdown-code-block-lang location)))
                     (bounds (markdown-code-block-at-pos start)))
                (when (and bounds (equal (downcase (or language "")) "mermaid"))
                  (goto-char start)
                  (let* ((prefix (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (progn (back-to-indentation) (point))))
                         (body-start (progn (forward-line 1) (point)))
                         (body-end (progn (goto-char (cadr bounds))
                                          (line-beginning-position)))
                         (source (buffer-substring-no-properties body-start body-end)))
                    (unless (string-empty-p prefix)
                      (setq source (mapconcat
                                    (lambda (line) (string-remove-prefix prefix line))
                                    (split-string source "\n") "\n")))
                    (push (list start (cadr bounds) source) blocks)))
                (goto-char (or (next-single-property-change start property)
                               (point-max)))))))
        (sort blocks (lambda (a b) (< (car a) (car b))))))))

(defun frei/markdown-mermaid--window-width ()
  "Return the available diagram width in pixels."
  (let ((windows (get-buffer-window-list (current-buffer) nil t)))
    (max 100 (- (if windows
                    (apply #'min (mapcar (lambda (window) (window-body-width window t))
                                        windows))
                  (window-body-width nil t))
                32))))

(defun frei/markdown-mermaid--color (face attribute fallback)
  "Read FACE's color ATTRIBUTE, using FALLBACK if unspecified."
  (let ((value (face-attribute face attribute nil t)))
    (if-let* ((rgb (and (stringp value) (color-name-to-rgb value))))
        (apply #'color-rgb-to-hex (append rgb '(2)))
      fallback)))

(defun frei/markdown-mermaid--settings ()
  "Return Pele options and concrete theme colors for this buffer."
  (let* ((background (frei/markdown-mermaid--color 'default :background "#272822"))
         (foreground (frei/markdown-mermaid--color 'default :foreground "#f8f8f2"))
         (surface (frei/markdown-mermaid--color 'mode-line-inactive :background background))
         (fonts (and (display-graphic-p) (font-family-list)))
         (family (or frei/markdown-mermaid-font-family
                     (seq-find (lambda (font) (member font fonts))
                               '("Sarasa Mono SC" "Microsoft YaHei" "PingFang SC"
                                 "Noto Sans CJK SC"))
                     "sans-serif")))
    `((options . ((maxWidth . ,(frei/markdown-mermaid--window-width))
                  (fontFamily . ,family)
                  (fontSize . ,(max 14 (frame-char-height)))))
      (colors . ((--pele-bg . ,background)
                 (--pele-fg . ,foreground)
                 (--pele-line . ,foreground)
                 (--pele-surface . ,surface)
                 (--pele-surface-alt . ,background)
                 (--pele-muted . ,(frei/markdown-mermaid--color
                                      'font-lock-comment-face :foreground foreground))
                 (--pele-border . ,(frei/markdown-mermaid--color
                                       'shadow :foreground foreground))
                 (--pele-accent . ,(frei/markdown-mermaid--color
                                       'font-lock-keyword-face :foreground foreground)))))))

(defun frei/markdown-mermaid--sync (settings)
  "Update block overlays and their cache keys for SETTINGS."
  (let ((old frei/markdown-mermaid--overlays)
        overlays)
    (pcase-dolist (`(,start ,end ,source) (frei/markdown-mermaid--blocks))
      (let* ((overlay (or (seq-find (lambda (item) (eq (overlay-start item) start)) old)
                          (make-overlay start end nil nil t)))
             (key (secure-hash 'sha256 (prin1-to-string (list source settings)))))
        (setq old (delq overlay old))
        (move-overlay overlay start end)
        (unless (equal key (overlay-get overlay 'key))
          (overlay-put overlay 'display nil)
          (overlay-put overlay 'help-echo nil))
        (overlay-put overlay 'frei/markdown-mermaid t)
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'priority 10)
        (overlay-put overlay 'invisible nil)
        (overlay-put overlay 'key key)
        (overlay-put overlay 'source source)
        (push overlay overlays)))
    (mapc #'delete-overlay old)
    (setq frei/markdown-mermaid--overlays (nreverse overlays))))

(defun frei/markdown-mermaid--report (error-message)
  "Report ERROR-MESSAGE once until a different problem occurs."
  (unless (equal error-message frei/markdown-mermaid--last-error)
    (setq frei/markdown-mermaid--last-error error-message)
    (message "Markdown Mermaid: %s" error-message)))

(defun frei/markdown-mermaid--display ()
  "Display cached SVGs on their current block overlays."
  (dolist (overlay frei/markdown-mermaid--overlays)
    (let* ((result (gethash (overlay-get overlay 'key) frei/markdown-mermaid--cache))
           (svg (alist-get 'svg result))
           (error-message (alist-get 'error result)))
      (cond
       ((or (not frei/markdown-mermaid--show-images)
            (overlay-get overlay 'source-visible))
        (overlay-put overlay 'display nil))
       (error-message
        (overlay-put overlay 'display nil)
        (overlay-put overlay 'help-echo error-message)
        (frei/markdown-mermaid--report error-message))
       ((and svg (not (overlay-get overlay 'display)))
        (condition-case error-data
            (let* ((width (max 1 (or (alist-get 'width result) 1)))
                   (fit (min 1.0 (/ (frei/markdown-mermaid--window-width) (float width))))
                   (scale (* fit (or (overlay-get overlay 'scale) 1.0)))
                   ;; A fixed :max-width would also cap manual zooming.
                   ;; Apply the initial fit through :scale instead.
                   (image (create-image (encode-coding-string svg 'utf-8-unix) 'svg t
                                        :scale scale :ascent 'center)))
              (image-size image t)
              (overlay-put overlay 'display image)
              (overlay-put overlay 'help-echo
                           "Mermaid: C-c C-x + / - / 0 resize; C-c C-x m shows source"))
          (error (frei/markdown-mermaid--report (error-message-string error-data))))))
      (let ((shown (overlay-get overlay 'display)))
        (overlay-put overlay 'face (and shown 'default))
        ;; A nil overlay property does not override hidden fence markers.
        ;; Use a symbol absent from Markdown's invisibility spec instead.
        (overlay-put overlay 'invisible (and shown 'frei/markdown-mermaid-visible))))))

(defun frei/markdown-mermaid--cleanup-process (process)
  "Dispose of PROCESS's timeout and private output buffers."
  (when-let* ((timer (process-get process 'timeout))) (cancel-timer timer))
  (dolist (buffer (list (process-buffer process) (process-get process 'stderr)))
    (when (buffer-live-p buffer) (kill-buffer buffer))))

(defun frei/markdown-mermaid--stop-process ()
  "Cancel this buffer's current render without running its callback."
  (when-let* ((process frei/markdown-mermaid--process))
    (setq frei/markdown-mermaid--process nil)
    (set-process-sentinel process #'ignore)
    (when (process-live-p process) (delete-process process))
    (frei/markdown-mermaid--cleanup-process process)))

(defun frei/markdown-mermaid--sentinel (process _event)
  "Cache PROCESS's output and show it only if its source is still current."
  (unless (process-live-p process)
    (unwind-protect
        (when-let* ((buffer (process-get process 'owner))
                    ((buffer-live-p buffer)))
          (with-current-buffer buffer
            (when (eq process frei/markdown-mermaid--process)
              (setq frei/markdown-mermaid--process nil)
              (when frei/markdown-mermaid-mode
                (condition-case error-data
                    (if (or (process-get process 'timed-out)
                            (not (eql (process-exit-status process) 0)))
                        (frei/markdown-mermaid--report
                         (if (process-get process 'timed-out) "Rendering timed out"
                           (with-current-buffer (process-get process 'stderr)
                             (string-trim (buffer-string)))))
                      (let ((results (with-current-buffer (process-buffer process)
                                       (goto-char (point-min))
                                       (alist-get 'results
                                                  (json-parse-buffer :object-type 'alist
                                                                     :array-type 'list)))))
                        (when (> (hash-table-count frei/markdown-mermaid--cache) 128)
                          (clrhash frei/markdown-mermaid--cache))
                        (dolist (result results)
                          (puthash (alist-get 'key result) result frei/markdown-mermaid--cache))
                        (when (eql (process-get process 'tick) (buffer-chars-modified-tick))
                          (frei/markdown-mermaid--display))))
                  (error (frei/markdown-mermaid--report (error-message-string error-data))))))))
      (frei/markdown-mermaid--cleanup-process process))))

(defun frei/markdown-mermaid--render (diagrams settings)
  "Render uncached DIAGRAMS in a local background process using SETTINGS."
  (let ((node (executable-find frei/markdown-mermaid-node-program))
        (script (expand-file-name "render.mjs" frei/markdown-mermaid--directory)))
    (cond
     ((not node) (frei/markdown-mermaid--report "Node 20 or newer is required"))
     ((not (file-exists-p (expand-file-name "node_modules/pele/package.json"
                                          frei/markdown-mermaid--directory)))
      (frei/markdown-mermaid--report
       (format "Install Pele with: npm ci --prefix %s" frei/markdown-mermaid--directory)))
     (t
      (let ((output (generate-new-buffer " *markdown-mermaid*"))
            (stderr (generate-new-buffer " *markdown-mermaid-errors*"))
            (default-directory frei/markdown-mermaid--directory))
        (condition-case error-data
            (let ((process (make-process
                            :name "markdown-mermaid" :buffer output :stderr stderr
                            :command (list node script) :noquery t :connection-type 'pipe
                            :coding '(utf-8-unix . utf-8-unix)
                            :sentinel #'frei/markdown-mermaid--sentinel)))
              (setq frei/markdown-mermaid--process process)
              (process-put process 'owner (current-buffer))
              (process-put process 'stderr stderr)
              (process-put process 'tick (buffer-chars-modified-tick))
              (process-put process 'timeout
                           (run-at-time frei/markdown-mermaid-timeout nil
                                        (lambda ()
                                          (when (process-live-p process)
                                            (process-put process 'timed-out t)
                                            (delete-process process)))))
              (process-send-string
               process (json-serialize (append `((diagrams . ,(vconcat diagrams))) settings)))
              (process-send-eof process))
          (error
           (frei/markdown-mermaid--stop-process)
           (dolist (buffer (list output stderr))
             (when (buffer-live-p buffer) (kill-buffer buffer)))
           (frei/markdown-mermaid--report (error-message-string error-data)))))))))

;;;###autoload
(defun frei/markdown-mermaid-refresh (&optional force)
  "Refresh Mermaid diagrams in this buffer.
With prefix argument FORCE, discard cached results and retry failed blocks."
  (interactive "P")
  (when frei/markdown-mermaid-mode
    (when (timerp frei/markdown-mermaid--timer) (cancel-timer frei/markdown-mermaid--timer))
    (setq frei/markdown-mermaid--timer nil)
    (frei/markdown-mermaid--stop-process)
    (when force
      (clrhash frei/markdown-mermaid--cache)
      (setq frei/markdown-mermaid--last-error nil)
      (dolist (overlay frei/markdown-mermaid--overlays) (overlay-put overlay 'display nil)))
    (when (and (display-images-p) (image-type-available-p 'svg))
      (let* ((settings (frei/markdown-mermaid--settings))
             (seen (make-hash-table :test 'equal))
             diagrams)
        (setq frei/markdown-mermaid--width (frei/markdown-mermaid--window-width))
        (frei/markdown-mermaid--sync settings)
        (frei/markdown-mermaid--display)
        (when frei/markdown-mermaid--show-images
          (dolist (overlay frei/markdown-mermaid--overlays)
            (let ((key (overlay-get overlay 'key)))
              (unless (or (overlay-get overlay 'source-visible)
                          (gethash key frei/markdown-mermaid--cache) (gethash key seen))
                (puthash key t seen)
                (push `((key . ,key) (source . ,(overlay-get overlay 'source))) diagrams))))
          (when diagrams (frei/markdown-mermaid--render (nreverse diagrams) settings)))))))

(defun frei/markdown-mermaid--schedule (&optional delay)
  "Schedule a refresh of this buffer after DELAY seconds of idle time."
  (when (timerp frei/markdown-mermaid--timer) (cancel-timer frei/markdown-mermaid--timer))
  (setq frei/markdown-mermaid--timer
        (run-with-idle-timer
         (or delay frei/markdown-mermaid-idle-delay) nil
         (lambda (buffer)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (frei/markdown-mermaid-refresh))))
         (current-buffer))))

(defun frei/markdown-mermaid--after-change (start end _old-length)
  "Reveal changed diagrams between START and END, then schedule a refresh."
  (dolist (overlay frei/markdown-mermaid--overlays)
    (when (and (overlay-buffer overlay)
               (<= (overlay-start overlay) end) (>= (overlay-end overlay) start))
      (overlay-put overlay 'display nil)))
  (frei/markdown-mermaid--schedule))

(defun frei/markdown-mermaid--window-change (_window)
  "Refresh when a window showing this buffer changes width."
  (unless (eql frei/markdown-mermaid--width (frei/markdown-mermaid--window-width))
    (frei/markdown-mermaid--schedule 0.1)))

(defun frei/markdown-mermaid--theme-change (&rest _)
  "Refresh Mermaid buffers after a theme change."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when frei/markdown-mermaid-mode (frei/markdown-mermaid--schedule 0.1)))))

(defun frei/markdown-mermaid--cleanup ()
  "Remove this buffer's timers, processes and diagram overlays."
  (when (timerp frei/markdown-mermaid--timer) (cancel-timer frei/markdown-mermaid--timer))
  (setq frei/markdown-mermaid--timer nil)
  (frei/markdown-mermaid--stop-process)
  (mapc #'delete-overlay frei/markdown-mermaid--overlays)
  (setq frei/markdown-mermaid--overlays nil
        frei/markdown-mermaid--cache nil))

(defun frei/markdown-mermaid--overlay-at-point ()
  "Return the Mermaid block overlay at point, or signal a user error."
  (or (seq-find (lambda (item)
                  (and (overlay-buffer item)
                       (<= (overlay-start item) (point))
                       (<= (point) (overlay-end item))))
                frei/markdown-mermaid--overlays)
      (user-error "No closed Mermaid block at point")))

;;;###autoload
(defun frei/markdown-mermaid-toggle-block ()
  "Toggle the Mermaid diagram at point between image and source."
  (interactive)
  (let ((overlay (frei/markdown-mermaid--overlay-at-point)))
    (overlay-put overlay 'source-visible (not (overlay-get overlay 'source-visible)))
    (frei/markdown-mermaid-refresh)))

;;;###autoload
(defun frei/markdown-mermaid-toggle-images ()
  "Toggle all Mermaid diagrams in this buffer between images and source."
  (interactive)
  (setq frei/markdown-mermaid--show-images (not frei/markdown-mermaid--show-images))
  (when frei/markdown-mermaid--show-images
    (dolist (overlay frei/markdown-mermaid--overlays)
      (overlay-put overlay 'source-visible nil)))
  (frei/markdown-mermaid-refresh))

(defun frei/markdown-mermaid--resize (factor)
  "Multiply the current diagram's zoom by FACTOR, or reset it when nil."
  (let ((overlay (frei/markdown-mermaid--overlay-at-point)))
    (unless (overlay-get overlay 'display)
      (user-error "Show the Mermaid image before resizing it"))
    (let ((scale (if factor
                     (max 0.1 (min 8.0 (* factor (or (overlay-get overlay 'scale) 1.0))))
                   1.0)))
      (overlay-put overlay 'scale scale)
      (overlay-put overlay 'display nil)
      (frei/markdown-mermaid--display)
      (message "Mermaid: %d%% of fitted size" (round (* 100 scale))))))

;;;###autoload
(defun frei/markdown-mermaid-increase-size (&optional steps)
  "Enlarge the Mermaid image at point by 20 percent per STEPS.
Zoom is relative to the image's fitted size, and is kept across refreshes."
  (interactive "p")
  (frei/markdown-mermaid--resize (expt 1.2 (or steps 1))))

;;;###autoload
(defun frei/markdown-mermaid-decrease-size (&optional steps)
  "Shrink the Mermaid image at point by one zoom step per STEPS."
  (interactive "p")
  (frei/markdown-mermaid--resize (expt 1.2 (- (or steps 1)))))

;;;###autoload
(defun frei/markdown-mermaid-reset-size ()
  "Restore the current Mermaid image to its fitted size."
  (interactive)
  (frei/markdown-mermaid--resize nil))

(defvar frei/markdown-mermaid-mode-map
  (make-sparse-keymap)
  "Keys for Mermaid images in Markdown buffers.")

;; Update the existing map as well when this file is reloaded.
(dolist (binding '(("C-c C-x m" . frei/markdown-mermaid-toggle-block)
                   ("C-c C-x M" . frei/markdown-mermaid-toggle-images)
                   ("C-c C-x r" . frei/markdown-mermaid-refresh)
                   ("C-c C-x +" . frei/markdown-mermaid-increase-size)
                   ("C-c C-x =" . frei/markdown-mermaid-increase-size)
                   ("C-c C-x -" . frei/markdown-mermaid-decrease-size)
                   ("C-c C-x 0" . frei/markdown-mermaid-reset-size)))
  (define-key frei/markdown-mermaid-mode-map (kbd (car binding)) (cdr binding)))

;;;###autoload
(define-minor-mode frei/markdown-mermaid-mode
  "Display Mermaid fences inline as SVG in a Markdown buffer.
Rendering happens asynchronously after opening or editing a document.
Use \\[frei/markdown-mermaid-toggle-block] to reveal a block's source,
\\[frei/markdown-mermaid-toggle-images] to toggle all diagrams, and
\\[frei/markdown-mermaid-refresh] to refresh (with a prefix, retry errors).
Resize the image at point with \\[frei/markdown-mermaid-increase-size],
\\[frei/markdown-mermaid-decrease-size] and \\[frei/markdown-mermaid-reset-size]."
  :lighter " Mermaid"
  (if frei/markdown-mermaid-mode
      (if (not (derived-mode-p 'markdown-mode))
          (progn (setq frei/markdown-mermaid-mode nil)
                 (user-error "This mode requires markdown-mode"))
        (setq frei/markdown-mermaid--cache (or frei/markdown-mermaid--cache
                                             (make-hash-table :test 'equal))
              frei/markdown-mermaid--show-images t)
        (add-hook 'after-change-functions #'frei/markdown-mermaid--after-change nil t)
        (add-hook 'window-size-change-functions #'frei/markdown-mermaid--window-change nil t)
        (add-hook 'kill-buffer-hook #'frei/markdown-mermaid--cleanup nil t)
        (add-hook 'change-major-mode-hook #'frei/markdown-mermaid--cleanup nil t)
        (frei/markdown-mermaid--schedule 0.1))
    (remove-hook 'after-change-functions #'frei/markdown-mermaid--after-change t)
    (remove-hook 'window-size-change-functions #'frei/markdown-mermaid--window-change t)
    (remove-hook 'kill-buffer-hook #'frei/markdown-mermaid--cleanup t)
    (remove-hook 'change-major-mode-hook #'frei/markdown-mermaid--cleanup t)
    (frei/markdown-mermaid--cleanup)))

(add-hook 'enable-theme-functions #'frei/markdown-mermaid--theme-change)
(add-hook 'disable-theme-functions #'frei/markdown-mermaid--theme-change)

(provide 'markdown-mermaid)

;;; markdown-mermaid.el ends here
