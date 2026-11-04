;;; markdown-mermaid-test.el --- Mermaid integration tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'markdown-mermaid)

(defmacro frei/markdown-mermaid-test--buffer (text &rest body)
  "Evaluate BODY in a Markdown buffer containing TEXT, with image display stubbed."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (markdown-mode)
     ;; Batch Emacs has no graphical frame.  Keep the real renderer and
     ;; image spec creation; only stub the display and pixel-size queries.
     (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
               ((symbol-function 'image-size) (lambda (&rest _) '(300 . 100))))
       (frei/markdown-mermaid-mode 1)
       (unwind-protect (progn ,@body)
         (frei/markdown-mermaid-mode -1)))))

(defun frei/markdown-mermaid-test--wait ()
  "Wait at most ten seconds for the current buffer's real Node process."
  (let ((deadline (+ (float-time) 10)))
    (while (and frei/markdown-mermaid--process (< (float-time) deadline))
      (accept-process-output frei/markdown-mermaid--process 0.05))
    (should-not frei/markdown-mermaid--process)))

(ert-deftest frei/markdown-mermaid-parses-closed-fences-only ()
  (with-temp-buffer
    (insert "# 文档\n\n```mermaid\nflowchart LR\n A --> B\n```\n\n"
            "````markdown\n```mermaid\nflowchart LR\n X --> Y\n```\n````\n\n"
            "  ````mermaid\n  mindmap\n    root((阅读))\n      图表\n  ````\n\n"
            "~~~mermaid\nsequenceDiagram\n A->>B: 中文\n~~~\n\n"
            "```mermaid\nflowchart TD\n A --> B\n")
    (markdown-mode)
    (let ((sources (mapcar #'caddr (frei/markdown-mermaid--blocks))))
      (should (= (length sources) 3))
      (should (equal (car sources) "flowchart LR\n A --> B\n"))
      (should (equal (cadr sources) "mindmap\n  root((阅读))\n    图表\n"))
      (should (equal (caddr sources) "sequenceDiagram\n A->>B: 中文\n")))))

(ert-deftest frei/markdown-mermaid-renders-caches-and-isolates-errors ()
  (frei/markdown-mermaid-test--buffer
      "```mermaid\nflowchart LR\n A[\"读取文档\"] --> B[\"显示\"]\n```\n\n```mermaid\nflowchart LR\n A[\"读取文档\"] --> B[\"显示\"]\n```\n\n```mermaid\nflowchart LR\n A[unfinished\n```\n"
    (let ((text (buffer-string)) (tick (buffer-chars-modified-tick)))
      (frei/markdown-mermaid-refresh)
      (frei/markdown-mermaid-test--wait)
      (should (= (length frei/markdown-mermaid--overlays) 3))
      (should (= (hash-table-count frei/markdown-mermaid--cache) 2))
      (should (overlay-get (nth 0 frei/markdown-mermaid--overlays) 'display))
      (should (overlay-get (nth 1 frei/markdown-mermaid--overlays) 'display))
      (should-not (overlay-get (nth 2 frei/markdown-mermaid--overlays) 'display))
      (should (overlay-get (nth 2 frei/markdown-mermaid--overlays) 'help-echo))
      (frei/markdown-mermaid-refresh)
      (should-not frei/markdown-mermaid--process)
      (should (equal text (buffer-string)))
      (should (= tick (buffer-chars-modified-tick))))))

(ert-deftest frei/markdown-mermaid-source-toggles-do-not-rewrite-text ()
  (frei/markdown-mermaid-test--buffer
      "```mermaid\nflowchart TD\n A --> B\n```\n"
    (let ((text (buffer-string)))
      (frei/markdown-mermaid-refresh)
      (frei/markdown-mermaid-test--wait)
      (goto-char (point-min))
      (frei/markdown-mermaid-toggle-block)
      (should-not (overlay-get (car frei/markdown-mermaid--overlays) 'display))
      (should-not frei/markdown-mermaid--process)
      (frei/markdown-mermaid-toggle-block)
      (should (overlay-get (car frei/markdown-mermaid--overlays) 'display))
      (frei/markdown-mermaid-toggle-images)
      (should-not (overlay-get (car frei/markdown-mermaid--overlays) 'display))
      (frei/markdown-mermaid-toggle-images)
      (should (overlay-get (car frei/markdown-mermaid--overlays) 'display))
      (should (equal text (buffer-string))))))

(ert-deftest frei/markdown-mermaid-discards-display-of-outdated-results ()
  (frei/markdown-mermaid-test--buffer
      "```mermaid\nflowchart TD\n A[\"旧内容\"] --> B\n```\n"
    (frei/markdown-mermaid-refresh)
    (should frei/markdown-mermaid--process)
    (cl-letf (((symbol-function 'frei/markdown-mermaid--schedule) #'ignore))
      (goto-char (point-min))
      (search-forward "旧内容")
      (replace-match "新内容" t t)
      (frei/markdown-mermaid-test--wait))
    (should-not (overlay-get (car frei/markdown-mermaid--overlays) 'display))
    (frei/markdown-mermaid-refresh)
    (frei/markdown-mermaid-test--wait)
    (let* ((overlay (car frei/markdown-mermaid--overlays))
           (svg (alist-get 'svg (gethash (overlay-get overlay 'key)
                                        frei/markdown-mermaid--cache))))
      (should (string-match-p "新内容" svg))
      (should (overlay-get overlay 'display)))))

(ert-deftest frei/markdown-mermaid-images-survive-hidden-markup-in-view-mode ()
  (frei/markdown-mermaid-test--buffer
      "```mermaid\nflowchart TD\n A --> B\n```\n"
    (markdown-view-mode)
    (frei/markdown-mermaid-mode 1)
    (font-lock-ensure)
    (should (invisible-p (point-min)))
    (frei/markdown-mermaid-refresh)
    (frei/markdown-mermaid-test--wait)
    (should (overlay-get (car frei/markdown-mermaid--overlays) 'display))
    (should-not (invisible-p (point-min)))
    (goto-char (point-min))
    (frei/markdown-mermaid-toggle-block)
    (should-not (overlay-get (car frei/markdown-mermaid--overlays) 'display))
    (should-not (overlay-get (car frei/markdown-mermaid--overlays) 'face))
    (should (invisible-p (point-min)))))

(ert-deftest frei/markdown-mermaid-removes-overlays-when-language-changes ()
  (frei/markdown-mermaid-test--buffer
      "```mermaid\nflowchart TD\n A --> B\n```\n"
    (frei/markdown-mermaid-refresh)
    (frei/markdown-mermaid-test--wait)
    (should (overlay-get (car frei/markdown-mermaid--overlays) 'display))
    (goto-char (point-min))
    (search-forward "mermaid")
    (replace-match "text" t t)
    (frei/markdown-mermaid-refresh)
    (should-not frei/markdown-mermaid--overlays)
    (should-not frei/markdown-mermaid--process)))

(ert-deftest frei/markdown-mermaid-zoom-is-local-and-survives-refresh ()
  (frei/markdown-mermaid-test--buffer
      "```mermaid\nflowchart TD\n A --> B\n```\n\n```mermaid\nflowchart TD\n A --> B\n```\n"
    (let ((text (buffer-string)))
      (frei/markdown-mermaid-refresh)
      (frei/markdown-mermaid-test--wait)
      (goto-char (point-min))
      (let* ((overlay (car frei/markdown-mermaid--overlays))
             (other (cadr frei/markdown-mermaid--overlays))
             (other-image (overlay-get other 'display))
             (original (plist-get (cdr (overlay-get overlay 'display)) :scale))
             (cache-count (hash-table-count frei/markdown-mermaid--cache)))
        (frei/markdown-mermaid-increase-size)
        (should (< (abs (- (plist-get (cdr (overlay-get overlay 'display)) :scale)
                           (* 1.2 original))) 0.00001))
        (should (eq other-image (overlay-get other 'display)))
        (should-not frei/markdown-mermaid--process)
        (should (= cache-count (hash-table-count frei/markdown-mermaid--cache)))
        (frei/markdown-mermaid-toggle-block)
        (frei/markdown-mermaid-toggle-block)
        (frei/markdown-mermaid-refresh t)
        (frei/markdown-mermaid-test--wait)
        (should (= (overlay-get overlay 'scale) 1.2))
        (should (< (abs (- (plist-get (cdr (overlay-get overlay 'display)) :scale)
                           (* 1.2 original))) 0.00001))
        (frei/markdown-mermaid-decrease-size)
        (should (< (abs (- (plist-get (cdr (overlay-get overlay 'display)) :scale)
                           original)) 0.00001))
        (should (equal text (buffer-string)))))))

(ert-deftest frei/markdown-mermaid-zoom-can-exceed-window-and-reset-to-fit ()
  (frei/markdown-mermaid-test--buffer
      "```mermaid\nflowchart TD\n A --> B\n```\n"
    (cl-letf (((symbol-function 'frei/markdown-mermaid--window-width) (lambda () 100)))
      (frei/markdown-mermaid--sync (frei/markdown-mermaid--settings))
      (let ((overlay (car frei/markdown-mermaid--overlays)))
        (puthash (overlay-get overlay 'key)
                 '((svg . "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"400\" height=\"100\"><rect width=\"400\" height=\"100\"/></svg>")
                   (width . 400) (height . 100))
                 frei/markdown-mermaid--cache)
        (frei/markdown-mermaid--display)
        (goto-char (point-min))
        (should (= (plist-get (cdr (overlay-get overlay 'display)) :scale) 0.25))
        (frei/markdown-mermaid-increase-size)
        (let ((properties (cdr (overlay-get overlay 'display))))
          (should (= (plist-get properties :scale) 0.3))
          (should-not (plist-get properties :max-width)))
        (frei/markdown-mermaid-reset-size)
        (should (= (plist-get (cdr (overlay-get overlay 'display)) :scale) 0.25))))))

(ert-deftest frei/markdown-mermaid-resize-needs-a-displayed-diagram ()
  (frei/markdown-mermaid-test--buffer
      "Text\n\n```mermaid\nflowchart TD\n A --> B\n```\n"
    (frei/markdown-mermaid-refresh)
    (frei/markdown-mermaid-test--wait)
    (goto-char (point-min))
    (should-error (frei/markdown-mermaid-increase-size) :type 'user-error)
    (goto-char (overlay-start (car frei/markdown-mermaid--overlays)))
    (frei/markdown-mermaid-toggle-block)
    (should-error (frei/markdown-mermaid-increase-size) :type 'user-error)))

(ert-deftest frei/markdown-mermaid-disable-cleans-up-in-flight-rendering ()
  (frei/markdown-mermaid-test--buffer
      "```mermaid\nflowchart TD\n A --> B\n```\n"
    (frei/markdown-mermaid-refresh)
    (let* ((process frei/markdown-mermaid--process)
           (output (process-buffer process))
           (stderr (process-get process 'stderr))
           (timer (process-get process 'timeout)))
      (should (process-live-p process))
      (frei/markdown-mermaid-mode -1)
      (should-not (process-live-p process))
      (should-not (buffer-live-p output))
      (should-not (buffer-live-p stderr))
      (should-not (memq timer timer-list))
      (should-not frei/markdown-mermaid--timer)
      (should-not frei/markdown-mermaid--overlays)
      (should-not frei/markdown-mermaid--cache))))

;;; markdown-mermaid-test.el ends here
