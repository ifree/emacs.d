;;; code-insight.el --- Explain code with tree-sitter and the pi agent  -*- lexical-binding: t; -*-

;;; Commentary:
;; `frei/code-insight' sends the region or the whole buffer to the pi
;; command line agent together with a structural outline extracted by
;; tree-sitter, and streams the answer into a transcript buffer next to
;; the code.  The built-in tasks are a short description, an
;; architecture diagram and a call overview; any other question can be
;; typed in.
;;
;; Answers about any number of files accumulate in one transcript; each
;; entry records its source, so names of definitions in an answer link
;; to the right file.  Running the command inside the transcript sends
;; the transcript itself, so questions about earlier answers just work.
;; Finished entries are also appended to a history file that
;; `frei/code-insight-history' brings back, diagrams included.
;;
;; The transcript has `frei/code-insight-mode' with keys for jumping
;; between entries, cancelling, saving, copying and visiting the source.
;;
;; Diagrams come back as Graphviz, Mermaid or PlantUML blocks and are
;; shown inline through diagram-tool.el, or as ASCII art for documents.
;; The prompt is assembled from templates/code-insight.md and the
;; drawing rules from templates/diagram-FORMAT.md, all editable.
;;
;; The outline only needs the tree-sitter grammar of the language, not a
;; tree-sitter major mode.  Without a grammar the plain code is sent.
;; `frei/treesit-install-grammars' from treesit-tool.el installs grammars.

;;; Code:
(require 'button)
(require 'cl-lib)
(require 'diagram-tool)
(require 'outline)
(require 'pulse)
(require 'seq)
(require 'subr-x)
(require 'text-property-search)
(require 'treesit)

(declare-function markdown-mode "markdown-mode")

(defgroup frei/code-insight nil
  "Explain code with tree-sitter and the pi agent."
  :group 'tools)

(defcustom frei/code-insight-program "pi"
  "Command line agent used to analyze code."
  :type 'string)

(defcustom frei/code-insight-arguments
  '("--no-tools" "--no-session" "--no-context-files")
  "Extra arguments passed to `frei/code-insight-program' for every request."
  :type '(repeat string))

(defcustom frei/code-insight-task-arguments
  '(("描述" "--thinking" "low")
    ("调用关系" "--thinking" "low"))
  "Extra agent arguments for particular tasks, keyed by task name.
Tasks that need careful layout, such as diagrams, are left at the
agent's default thinking level."
  :type '(alist :key-type string :value-type (repeat string)))

(defcustom frei/code-insight-question-arguments '("--thinking" "low")
  "Extra agent arguments for questions typed by hand."
  :type '(repeat string))

(defcustom frei/code-insight-model nil
  "Model pattern passed to the agent, or nil for its default."
  :type '(choice (const :tag "Agent default" nil) string))

(defcustom frei/code-insight-max-code-size 60000
  "Largest amount of code, in characters, attached in full.
Longer code is sent as the outline plus its beginning; a longer
transcript is sent from its end."
  :type 'integer)

(defcustom frei/code-insight-template-file
  (locate-user-emacs-file "templates/code-insight.md")
  "Markdown template of the prompt sent to the agent.
See `frei/code-insight-default-template' for the placeholders."
  :type 'file)

(defcustom frei/code-insight-diagram-format 'dot
  "Diagram language the agent is asked for when a task wants a picture.
Graphviz suits call graphs and module relationships best.  It falls
back to svg when the format cannot be rendered here; see
`frei/diagram-available-formats'.  The instructions for each format
live in templates/diagram-FORMAT.md next to the prompt template."
  :type '(choice (const dot) (const mermaid) (const plantuml) (const svg)))

(defcustom frei/code-insight-tasks
  '(("描述" . "用中文简要描述这段代码：先用一句话概括它的作用，再分点说明主要组成部分以及它们之间的关系，总长度控制在 300 字以内。只做描述，不做评价和修改建议。")
    ("架构图" . "画一张简单的架构图，展示这段代码的主要模块、类、函数以及它们之间的调用或依赖关系，节点不超过 20 个，图的格式按输出要求。图之后用两三句中文说明要点。")
    ("架构图（ASCII）" . "用纯 ASCII 字符画一张简单的架构图，用方框表示这段代码的主要模块、类、函数，用箭头表示它们之间的调用或依赖关系，节点不超过 20 个。输出一个 ```text 代码块，只用 + - | / \\ < > ^ v 和字母数字画框线与箭头，不用 Unicode 制表符，每行不超过 100 个字符，方框内的名字对齐整齐，方便直接粘贴到文档和代码注释里。代码块之后用两三句中文说明图的要点。")
    ("调用关系" . "用中文列出这段代码中主要函数和方法之间的调用关系，按从入口到底层的顺序整理成层级列表，并指出关键的数据流向。"))
  "Named analysis tasks offered by `frei/code-insight'."
  :type '(alist :key-type string :value-type string))

(defcustom frei/code-insight-shared-buffer t
  "Whether answers about all files go to one transcript buffer.
When nil, every source file gets a transcript of its own."
  :type 'boolean)

(defcustom frei/code-insight-directory
  (expand-file-name "var/code-insight/" user-emacs-directory)
  "Directory for the history file and answers exported to the browser."
  :type 'directory)

(defcustom frei/code-insight-display-action
  '((display-buffer-reuse-window display-buffer-in-direction)
    (direction . right)
    (window-width . 0.4))
  "How the transcript buffer is displayed; see `display-buffer'."
  :type 'sexp)

(defconst frei/code-insight-default-template
  "# 任务

下面的材料包含来源信息、用 tree-sitter 提取的结构概要（如果有）以及源码。请基于这些材料完成这个任务，直接给出结果，不要复述材料。

{{task}}

# 输出要求

- 回答用中文，使用 Markdown。
- 回答显示在背景色为 {{background}} 的编辑器里，如果自己选颜色，要与它对比清晰。
{{diagram-instructions}}

# 代码材料

- 来源：{{source}}
- 语言：{{language}}
- 范围：{{range}}
{{outline-section}}
{{code-section}}
"
  "Template used when `frei/code-insight-template-file' does not exist.
Placeholders: {{task}}, {{background}}, {{diagram-instructions}},
{{source}}, {{language}}, {{range}}, {{outline}}, {{outline-section}},
{{code}}, {{code-section}}.")

(defconst frei/code-insight-diagram-hints
  '((mermaid . "- 如需画图，输出一个 ```mermaid 代码块：用 flowchart LR 或 TD，节点写成 id[\"标签\"] 的形式，标签用双引号包住，中文可以直接写；用 --> 连线，可加 |说明|；可以用 subgraph 分组；不要用 click 和 HTML 标签。")
    (dot . "- 如需画图，输出一个 ```dot 代码块：digraph，rankdir=LR，node [shape=box]，节点标签用双引号包住，中文可以直接写，用 -> 连线并可加 [label=\"说明\"]，可以用 subgraph cluster_x 分组，子图名必须以 cluster_ 开头。")
    (plantuml . "- 如需画图，输出一个 ```plantuml 代码块，以 @startuml 开始、@enduml 结束，可以用组件图或类图，中文可以直接写。")
    (svg . "- 如需画图，输出一个 ```svg 代码块，里面是完整的 SVG 文档：根元素带 xmlns、width、height 和 viewBox，宽度 900 左右；只用 rect、circle、ellipse、line、path、polyline、polygon、text、g、defs、marker 这些基本元素，不要用 foreignObject、外部字体、图片、CSS class 或 style 标签；文字用 font-family=\"sans-serif\"；先画一个填满画布、颜色为 {{background}} 的背景矩形。"))
  "Fallback instructions for each diagram format.
The file templates/diagram-FORMAT.md next to the prompt template takes
precedence when it exists.")

(defconst frei/code-insight--instruction
  "The attached file starts with a task, followed by the material it is about. Complete the task. Answer in Chinese unless the task asks for another language."
  "ASCII message that accompanies the attached material.")

(defconst frei/code-insight--legend
  "C-c a 再问  j 跳转  n/p 上下条  t 图  s 保存  c 复制  o 源码  b 浏览器  h 历史  k 取消  x 清空  q 退出"
  "Key summary shown in the header line while idle.")

(defvar frei/code-insight-question-history nil
  "History of questions typed for `frei/code-insight'.")

(defvar frei/code-insight-mode)

(defvar-local frei/code-insight--sources nil
  "Alist of source keys and plists describing the sources of the transcript.
The plist has :buffer, :file, :definitions and :outline.")

(defvar-local frei/code-insight--process nil
  "The agent process working for the transcript, if any.")

;;;; Language detection

(defconst frei/code-insight--mode-languages
  '(("c++" . cpp) ("csharp" . c-sharp) ("js" . javascript) ("sh" . bash))
  "Major mode base names whose grammar has a different name.")

(defconst frei/code-insight--extension-languages
  '(("py" . python) ("pyi" . python)
    ("js" . javascript) ("mjs" . javascript) ("cjs" . javascript) ("jsx" . javascript)
    ("ts" . typescript) ("mts" . typescript) ("cts" . typescript) ("tsx" . tsx)
    ("c" . c) ("h" . c)
    ("cc" . cpp) ("cpp" . cpp) ("cxx" . cpp) ("hpp" . cpp) ("hh" . cpp) ("hxx" . cpp)
    ("rs" . rust) ("go" . go) ("java" . java) ("cs" . c-sharp)
    ("lua" . lua) ("sh" . bash) ("bash" . bash))
  "File name extensions and the grammar they belong to.")

(defun frei/code-insight--grammar-available-p (language)
  "Return non-nil when the grammar of LANGUAGE can be loaded."
  (and language
       (treesit-available-p)
       (treesit-language-available-p language)))

(defun frei/code-insight--language ()
  "Return the tree-sitter language of the current buffer, or nil.
The grammar is not necessarily installed; see
`frei/code-insight--grammar-available-p'."
  (let* ((base (string-remove-suffix
                "-mode" (string-remove-suffix "-ts-mode" (symbol-name major-mode))))
         (from-mode (or (cdr (assoc base frei/code-insight--mode-languages))
                        (and (not (member base '("fundamental" "text" "prog" "special")))
                             (intern base))))
         (extension (and buffer-file-name
                         (downcase (or (file-name-extension buffer-file-name) ""))))
         (from-extension (cdr (assoc extension frei/code-insight--extension-languages))))
    (cond ((frei/code-insight--grammar-available-p from-mode) from-mode)
          ((frei/code-insight--grammar-available-p from-extension) from-extension)
          (t (or from-extension from-mode)))))

;;;; Outline extraction

(defconst frei/code-insight--definition-types
  '("function_definition" "function_declaration" "function_item"
    "function_signature_item" "generator_function_declaration"
    "method_definition" "method_declaration" "constructor_declaration"
    "class_definition" "class_declaration" "abstract_class_declaration"
    "class_specifier" "struct_specifier" "struct_item" "struct_declaration"
    "union_specifier" "enum_specifier" "enum_item" "enum_declaration"
    "interface_declaration" "trait_item" "impl_item"
    "namespace_definition" "namespace_declaration" "internal_module" "mod_item"
    "type_alias_declaration" "type_declaration" "record_declaration")
  "Node types that define a named unit of code.")

(defconst frei/code-insight--assigned-function-types
  '("arrow_function" "function_expression")
  "Node types that define a function when assigned to a variable.")

(defconst frei/code-insight--call-types
  '("call" "call_expression" "method_invocation" "invocation_expression"
    "function_call" "macro_invocation")
  "Node types of function and method calls.")

(defconst frei/code-insight--import-types
  '("import_statement" "import_from_statement" "preproc_include"
    "use_declaration" "import_declaration" "using_directive")
  "Node types of imports and includes.")

(defun frei/code-insight--field-text (node field)
  "Return the text of the child of NODE in FIELD, or nil."
  (when-let* ((child (treesit-node-child-by-field-name node field)))
    (treesit-node-text child t)))

(defun frei/code-insight--first-line (node)
  "Return the first line of NODE's text, shortened to 100 characters."
  (let ((line (car (split-string (string-trim (treesit-node-text node t)) "\n"))))
    (truncate-string-to-width line 100 nil nil "…")))

(defun frei/code-insight--definition-name (node)
  "Return the name defined by NODE, or nil."
  (let ((type (treesit-node-type node)))
    (cond
     ((member type frei/code-insight--assigned-function-types)
      (frei/code-insight--field-text (treesit-node-parent node) "name"))
     ((equal type "type_declaration")
      (when-let* ((spec (treesit-node-child node 0 t)))
        (frei/code-insight--field-text spec "name")))
     ((equal type "impl_item")
      (let ((trait (frei/code-insight--field-text node "trait"))
            (target (frei/code-insight--field-text node "type")))
        (if trait (concat trait " for " target) target)))
     (t
      (or (frei/code-insight--field-text node "name")
          ;; C and C++ nest the name inside declarators.
          (let ((declarator node) (next nil))
            (while (setq next (treesit-node-child-by-field-name declarator "declarator"))
              (setq declarator next))
            (and (not (eq declarator node))
                 (treesit-node-text declarator t)))
          (frei/code-insight--field-text node "type"))))))

(defun frei/code-insight--callee (node)
  "Return the name called by the call NODE, or nil."
  (when-let* ((target (or (treesit-node-child-by-field-name node "function")
                          (treesit-node-child-by-field-name node "name")
                          (treesit-node-child-by-field-name node "macro")
                          (treesit-node-child node 0 t))))
    (let ((text (replace-regexp-in-string "[ \t\n]+" " " (treesit-node-text target t))))
      (and (< 0 (length text) 60) text))))

(defun frei/code-insight--definition-p (node)
  "Return non-nil when NODE defines a named unit of code."
  (let ((type (treesit-node-type node)))
    (or (member type frei/code-insight--definition-types)
        (and (member type frei/code-insight--assigned-function-types)
             (equal (treesit-node-type (treesit-node-parent node))
                    "variable_declarator")))))

(defun frei/code-insight--collect (root beg end)
  "Collect imports and definitions of ROOT overlapping BEG to END.
Return (IMPORTS . DEFINITIONS).  Each definition is a vector of level,
name, first line, start line, end line, called names and node type."
  (let ((imports nil)
        (definitions nil)
        (stack nil))
    (cl-labels
        ((walk (node level)
           (let ((type (treesit-node-type node)))
             (cond
              ((member type frei/code-insight--import-types)
               (when (< (length imports) 40)
                 (push (frei/code-insight--first-line node) imports)))
              ((frei/code-insight--definition-p node)
               (let ((entry (vector level
                                    (frei/code-insight--definition-name node)
                                    ;; Show `name = (...) => {' for assigned functions.
                                    (frei/code-insight--first-line
                                     (if (member type frei/code-insight--assigned-function-types)
                                         (treesit-node-parent node)
                                       node))
                                    (line-number-at-pos (treesit-node-start node) t)
                                    (line-number-at-pos (treesit-node-end node) t)
                                    nil
                                    type)))
                 (when (and (< (treesit-node-start node) end)
                            (> (treesit-node-end node) beg))
                   (push entry definitions))
                 (push entry stack)
                 (dolist (child (treesit-node-children node t))
                   (walk child (1+ level)))
                 (pop stack)))
              (t
               (when (and (member type frei/code-insight--call-types) stack)
                 (when-let* ((callee (frei/code-insight--callee node))
                             (current (car stack)))
                   (unless (or (member callee (aref current 5))
                               (>= (length (aref current 5)) 25))
                     (aset current 5 (append (aref current 5) (list callee))))))
               (dolist (child (treesit-node-children node t))
                 (walk child level)))))))
      (walk root 0))
    (cons (nreverse imports) (nreverse definitions))))

(defun frei/code-insight--parse (language beg end)
  "Collect the imports and definitions between BEG and END in LANGUAGE.
Return what `frei/code-insight--collect' returns."
  (let ((parser (treesit-parser-create language nil t)))
    (unwind-protect
        (frei/code-insight--collect (treesit-parser-root-node parser) beg end)
      (treesit-parser-delete parser))))

(defun frei/code-insight--analyze (language beg end)
  "Analyze the code between BEG and END in LANGUAGE with tree-sitter.
Return (OUTLINE . DEFINITIONS): a textual outline and an alist of
definition names and their start lines."
  (let ((parser (treesit-parser-create language nil t)))
    (unwind-protect
        (pcase-let* ((`(,imports . ,definitions)
                      (frei/code-insight--collect
                       (treesit-parser-root-node parser) beg end))
                     (base (if definitions
                               (seq-min (mapcar (lambda (entry) (aref entry 0)) definitions))
                             0))
                     (shown (seq-take definitions 300)))
          (cons
           (concat
            (when imports
              (concat "Imports:\n"
                      (mapconcat (lambda (line) (concat "  " line)) imports "\n")
                      "\n"))
            (if definitions "Definitions:\n" "Definitions: none found\n")
            (mapconcat
             (lambda (entry)
               (format "%s%s  [L%d-L%d]%s"
                       (make-string (* 2 (- (aref entry 0) base)) ?\s)
                       (or (aref entry 2) (aref entry 1) "?")
                       (aref entry 3) (aref entry 4)
                       (if (aref entry 5)
                           (concat "  -> calls: " (string-join (aref entry 5) ", "))
                         "")))
             shown "\n")
            (when (> (length definitions) (length shown))
              (format "\n  … %d more definitions omitted" (- (length definitions) (length shown)))))
           (let ((names nil))
             (dolist (entry definitions)
               (when (and (aref entry 1) (not (assoc (aref entry 1) names)))
                 (push (cons (aref entry 1) (aref entry 3)) names)))
             (nreverse names))))
      (treesit-parser-delete parser))))

;;;; Diagrams straight from tree-sitter

(defcustom frei/code-insight-treesit-max-nodes 80
  "Largest number of definitions drawn by `frei/code-insight-treesit-diagram'."
  :type 'integer)

(defconst frei/code-insight--treesit-task "架构图（tree-sitter）"
  "Name of the task that draws the call graph without the agent.")

(defconst frei/code-insight--function-type-regexp
  (rx (or "function" "method" "constructor"))
  "Regexp matching node types of functions and methods.")

(defconst frei/code-insight--entry-names
  '("main" "Main" "__main__" "WinMain" "wmain")
  "Names highlighted as entry points in generated call graphs.")

(defun frei/code-insight--dot-string (text)
  "Return TEXT quoted for a DOT attribute."
  (concat "\"" (replace-regexp-in-string "[\"\\]" "\\\\\\&" text) "\""))

(defun frei/code-insight--callee-name (callee)
  "Return the plain name a CALLEE expression refers to.
Argument lists are dropped, so Builder(env).run names run, and only
the last part of a qualified name is kept."
  (let ((text callee))
    (while (string-match-p "([^()]*)" text)
      (setq text (replace-regexp-in-string "([^()]*)" "" text)))
    (car (last (split-string text "[.:>]" t)))))

(defun frei/code-insight--call-graph (definitions)
  "Build the call graph of the DEFINITIONS vectors.
Return a list of the DOT source, the number of nodes drawn and the
number of edges."
  (let* ((entries (vconcat (seq-take definitions frei/code-insight-treesit-max-nodes)))
         (count (length entries))
         (parents (make-vector count nil))
         (children (make-vector count nil))
         (stack nil)
         (by-name nil)
         (edges nil))
    ;; Nesting from the levels, and an index of names.
    (dotimes (i count)
      (let ((level (aref (aref entries i) 0)))
        (while (and stack (>= (aref (aref entries (car stack)) 0) level))
          (pop stack))
        (when stack
          (aset parents i (car stack))
          (aset children (car stack) (append (aref children (car stack)) (list i))))
        (push i stack))
      (when-let* ((name (aref (aref entries i) 1)))
        (push (cons name i) by-name)))
    (setq by-name (nreverse by-name))
    ;; Edges: a call resolves to a definition of the same container first.
    (dotimes (i count)
      (dolist (callee (aref (aref entries i) 5))
        (let* ((name (frei/code-insight--callee-name callee))
               (candidates (mapcar #'cdr (seq-filter (lambda (pair) (equal (car pair) name))
                                                     by-name)))
               (target (or (seq-find (lambda (j) (eq (aref parents j) (aref parents i)))
                                     candidates)
                           (car candidates))))
          (when (and target (not (member (cons i target) edges)))
            (push (cons i target) edges)))))
    (setq edges (nreverse edges))
    ;; A call into a class next to a call of one of its methods adds
    ;; nothing; keep the edge to the method.
    (setq edges (seq-remove
                 (lambda (edge)
                   (and (aref children (cdr edge))
                        (seq-some (lambda (other)
                                    (and (eq (car other) (car edge))
                                         (eq (aref parents (cdr other)) (cdr edge))))
                                  edges)))
                 edges))
    (cl-labels
        ((function-p (i)
           (string-match-p frei/code-insight--function-type-regexp
                           (aref (aref entries i) 6)))
         (label (i)
           (let ((name (or (aref (aref entries i) 1) "?")))
             (if (function-p i) (concat name "()") name)))
         (node (i)
           (format "  n%d [label=%s, fillcolor=%s, fontcolor=%s];\n"
                   i (frei/code-insight--dot-string (label i))
                   (cond ((member (aref (aref entries i) 1) frei/code-insight--entry-names)
                          "\"#2ECC71\"")
                         ((function-p i) "\"#3498DB\"")
                         (t "\"#F1C40F\""))
                   (if (or (function-p i)
                           (member (aref (aref entries i) 1) frei/code-insight--entry-names))
                       "white" "black")))
         (emit (i)
           (if (aref children i)
               (concat (format "  subgraph cluster_%d {\n    label=%s; style=\"rounded,filled\"; fillcolor=\"#f5f5f5\"; color=\"#999999\"; fontcolor=black;\n"
                               i (frei/code-insight--dot-string (label i)))
                       (mapconcat #'emit (aref children i) "")
                       "  }\n")
             (node i)))
         (anchor (i)
           ;; Edges cannot end on a cluster; aim at its first leaf instead.
           (if (aref children i) (anchor (car (aref children i))) i)))
      (list
       (concat "digraph G {\n"
               "  graph [rankdir=LR, nodesep=0.4, ranksep=0.7, splines=ortho, compound=true, fontname=\"sans-serif\"];\n"
               "  node [shape=box, style=\"rounded,filled\", fontname=\"sans-serif\", fontsize=12];\n"
               "  edge [arrowhead=vee, color=\"#666666\"];\n"
               (mapconcat #'emit
                          (seq-filter (lambda (i) (null (aref parents i)))
                                      (number-sequence 0 (1- count)))
                          "")
               (mapconcat (lambda (edge)
                            (let ((from (anchor (car edge)))
                                  (to (anchor (cdr edge))))
                              (format "  n%d -> n%d%s;\n" from to
                                      (if (aref children (cdr edge))
                                          (format " [lhead=cluster_%d]" (cdr edge))
                                        ""))))
                          edges "")
               "}\n")
       count
       (length edges)))))

;;;; Prompt template

(defun frei/code-insight--read-file (file)
  "Return the contents of FILE decoded as UTF-8."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (buffer-string)))

(defun frei/code-insight--template ()
  "Return the prompt template text."
  (if (file-readable-p frei/code-insight-template-file)
      (frei/code-insight--read-file frei/code-insight-template-file)
    frei/code-insight-default-template))

(defun frei/code-insight--diagram-hint (format)
  "Return the instructions for diagrams in FORMAT."
  (let ((file (expand-file-name (format "diagram-%s.md" format)
                                (file-name-directory frei/code-insight-template-file))))
    (if (file-readable-p file)
        (string-trim-right (frei/code-insight--read-file file))
      (or (cdr (assq format frei/code-insight-diagram-hints)) ""))))

(defun frei/code-insight--expand (template values)
  "Replace the {{name}} placeholders of TEMPLATE with VALUES.
VALUES is an alist of placeholder names and strings; unknown
placeholders expand to nothing."
  (replace-regexp-in-string
   "{{\\([a-z-]+\\)}}"
   (lambda (match)
     (or (cdr (assoc (match-string 1 match) values)) ""))
   template t t))

(defun frei/code-insight--diagram-format ()
  "Return the diagram format to ask for, falling back to svg."
  (if (frei/diagram-format-available-p frei/code-insight-diagram-format)
      frei/code-insight-diagram-format
    'svg))

(defun frei/code-insight--wants-diagram-p (task)
  "Return non-nil when TASK asks for a rendered diagram.
Tasks that draw with ASCII characters get no drawing instructions."
  (and (string-match-p "图\\|画\\|diagram\\|chart\\|graph" task)
       (not (string-match-p "ASCII" task))))

(defun frei/code-insight--material (beg end task)
  "Return what the agent gets for TASK about the text between BEG and END.
The result is a list of the material text, a note about the outline
for the transcript, the outline text or nil, and an alist of definition
names and lines.  Everything the agent reads goes into the material,
because non-ASCII command line arguments do not survive on Windows."
  (let* ((transcript frei/code-insight-mode)
         (language (and (not transcript) (frei/code-insight--language)))
         (available (frei/code-insight--grammar-available-p language))
         (code (buffer-substring-no-properties beg end))
         (limit frei/code-insight-max-code-size)
         (truncated (> (length code) limit))
         (whole (and (= beg (point-min)) (= end (point-max))))
         (analysis (when available (frei/code-insight--analyze language beg end)))
         (outline (car analysis))
         (background (frei/diagram--background))
         (hint (if (frei/code-insight--wants-diagram-p task)
                   (frei/code-insight--expand
                    (frei/code-insight--diagram-hint (frei/code-insight--diagram-format))
                    (list (cons "background" background)))
                 ""))
         (note (cond (transcript "based on this transcript")
                     (available (format "tree-sitter outline (%s)" language))
                     (language (format "no %s grammar, code only" language))
                     (t "unknown language, code only"))))
    (list
     (frei/code-insight--expand
      (frei/code-insight--template)
      (list
       (cons "task" task)
       (cons "background" background)
       (cons "diagram-instructions" hint)
       (cons "source" (or buffer-file-name (buffer-name)))
       (cons "language" (cond (transcript "markdown, earlier analysis")
                              (language (symbol-name language))
                              (t "unknown")))
       (cons "range" (if whole
                         (format "全文，共 %d 行" (count-lines beg end))
                       (format "第 %d 到 %d 行，共 %d 行"
                               (line-number-at-pos beg t) (line-number-at-pos end t)
                               (line-number-at-pos (point-max) t))))
       (cons "outline" (or outline ""))
       (cons "outline-section"
             (if outline
                 (concat "\n## 结构概要（tree-sitter）\n\n```\n" outline "\n```\n")
               ""))
       (cons "code" code)
       (cons "code-section"
             (concat
              (when truncated
                (format "\n注意：内容超过 %d 个字符，只附上%s。\n"
                        limit (if transcript "最后的部分" "开头部分")))
              (format "\n## %s\n\n```%s\n%s\n```\n"
                      (if transcript "前面的分析记录" "源码")
                      (cond (transcript "markdown") (language language) (t ""))
                      (cond ((not truncated) code)
                            ;; Recent entries matter most in a transcript.
                            (transcript (substring code (- (length code) limit)))
                            (t (substring code 0 limit))))))))
     note
     outline
     (cdr analysis))))

;;;; Transcript buffer

(defvar frei/code-insight-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") #'frei/code-insight-goto)
    (define-key map (kbd "n") #'frei/code-insight-next-section)
    (define-key map (kbd "p") #'frei/code-insight-previous-section)
    (define-key map (kbd "t") #'frei/code-insight-toggle-images)
    (define-key map (kbd "s") #'frei/code-insight-save)
    (define-key map (kbd "c") #'frei/code-insight-copy)
    (define-key map (kbd "o") #'frei/code-insight-visit-source)
    (define-key map (kbd "b") #'frei/code-insight-open-in-browser)
    (define-key map (kbd "h") #'frei/code-insight-history)
    (define-key map (kbd "k") #'frei/code-insight-cancel)
    (define-key map (kbd "x") #'frei/code-insight-clear)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap of `frei/code-insight-mode'.")

(define-minor-mode frei/code-insight-mode
  "Minor mode of the transcript buffer of `frei/code-insight'.

\\{frei/code-insight-mode-map}"
  :lighter " Insight"
  :keymap frei/code-insight-mode-map)

(defun frei/code-insight--setup-buffer ()
  "Turn the current buffer into a transcript buffer."
  (unless (derived-mode-p 'markdown-mode 'text-mode)
    (if (fboundp 'markdown-mode) (markdown-mode) (text-mode)))
  (visual-line-mode 1)
  (frei/code-insight-mode 1)
  (add-hook 'kill-buffer-hook #'frei/code-insight--stop-process nil t)
  (setq buffer-read-only t)
  (frei/code-insight--set-header (current-buffer) frei/code-insight--legend))

(defun frei/code-insight--stop-process ()
  "Stop the agent of a transcript that is going away.
The process has no buffer of its own, so it would otherwise run on."
  (when (process-live-p frei/code-insight--process)
    (delete-process frei/code-insight--process)))

(defun frei/code-insight--source-key (buffer)
  "Return the key identifying the source BUFFER in a transcript."
  (or (buffer-file-name buffer) (buffer-name buffer)))

(defun frei/code-insight--register-source (buffer key &optional definitions)
  "Record KEY as a source of the transcript in BUFFER, with DEFINITIONS."
  (with-current-buffer buffer
    (let ((entry (assoc key frei/code-insight--sources)))
      (unless entry
        (setq entry (cons key (list :file (and (file-name-absolute-p key) key))))
        (push entry frei/code-insight--sources))
      (when-let* ((source (get-buffer key))
                  ((not (file-name-absolute-p key))))
        (plist-put (cdr entry) :buffer source))
      (when-let* ((source (get-file-buffer key)))
        (plist-put (cdr entry) :buffer source))
      (when definitions
        (plist-put (cdr entry) :definitions
                   (cl-remove-duplicates
                    (append definitions (plist-get (cdr entry) :definitions))
                    :key #'car :test #'equal :from-end t)))
      entry)))

(defun frei/code-insight--transcript (source)
  "Return the transcript buffer for the SOURCE buffer, creating it if needed."
  (let* ((name (if frei/code-insight-shared-buffer
                   "*code-insight*"
                 (format "*code-insight: %s*" (buffer-name source))))
         (buffer (get-buffer name)))
    (unless buffer
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
        (frei/code-insight--setup-buffer)
        (let ((inhibit-read-only t))
          (insert (format "# %s\n"
                          (if frei/code-insight-shared-buffer
                              "code-insight"
                            (buffer-name source)))))))
    buffer))

(defun frei/code-insight--set-header (buffer text)
  "Show TEXT in the header line of BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq header-line-format (concat " code-insight · " text))
      (force-mode-line-update))))

(defun frei/code-insight--insert (buffer text)
  "Append TEXT to BUFFER, scrolling only the windows that were at the end."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (following (seq-filter
                        (lambda (window) (>= (window-point window) (1- (point-max))))
                        (get-buffer-window-list buffer nil t))))
        (save-excursion
          (goto-char (point-max))
          (insert text))
        (dolist (window following)
          (set-window-point window (point-max)))))))

(defun frei/code-insight--heading-line (heading key task)
  "Return the section HEADING for source KEY and TASK, marked as a section.
Answers may contain Markdown headings of their own, so the sections
of the transcript are recognized by text properties instead."
  (propertize (concat "## " heading)
              'frei/code-insight-section t
              'frei/code-insight-source key
              'frei/code-insight-task task))

(defun frei/code-insight--section-heading (pos)
  "Return the start of the heading of the section containing POS, or nil."
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (when-let* ((match (text-property-search-backward 'frei/code-insight-section t t)))
      (prop-match-beginning match))))

(defun frei/code-insight--section-bounds ()
  "Return the start and end of the transcript section around point."
  (let* ((start (or (frei/code-insight--section-heading (point)) (point-min)))
         (end (or (save-excursion
                    (goto-char start)
                    (forward-line 1)
                    (when-let* ((match (text-property-search-forward
                                        'frei/code-insight-section t t)))
                      (prop-match-beginning match)))
                  (point-max))))
    (cons start end)))

(defun frei/code-insight--section-source ()
  "Return the source plist entry of the section around point, or nil."
  (when-let* ((heading (frei/code-insight--section-heading (point)))
              (key (get-text-property heading 'frei/code-insight-source)))
    (or (assoc key frei/code-insight--sources)
        (frei/code-insight--register-source (current-buffer) key))))

(defun frei/code-insight--sections ()
  "Return the sections of the transcript as (HEADING . POSITION) pairs."
  (let ((sections nil)
        (match nil))
    (save-excursion
      (goto-char (point-min))
      (while (setq match (text-property-search-forward 'frei/code-insight-section t t))
        (push (cons (replace-regexp-in-string
                     "  _[^_]*_\\'" ""
                     (string-remove-prefix
                      "## " (buffer-substring-no-properties
                             (prop-match-beginning match) (prop-match-end match))))
                    (prop-match-beginning match))
              sections)))
    (nreverse sections)))

(defun frei/code-insight--insert-outline (buffer outline)
  "Append OUTLINE to BUFFER as a folded subsection."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (let ((start (point)))
          (insert "### 结构概要\n\n```\n" outline "\n```\n\n")
          (goto-char start)
          (ignore-errors (outline-hide-subtree)))))))

;;;; Links to the source

(defun frei/code-insight--jump (entry line)
  "Show LINE of the source described by the plist ENTRY."
  (let* ((plist (cdr entry))
         (buffer (plist-get plist :buffer))
         (file (plist-get plist :file))
         (source (cond ((buffer-live-p buffer) buffer)
                       ((and file (file-exists-p file)) (find-file-noselect file)))))
    (unless source
      (user-error "找不到来源 %s" (car entry)))
    (pop-to-buffer source '((display-buffer-reuse-window display-buffer-use-some-window)))
    (widen)
    (goto-char (point-min))
    (forward-line (1- line))
    (pulse-momentary-highlight-one-line (point))))

(define-button-type 'frei/code-insight-definition
  'action (lambda (button)
            (frei/code-insight--jump (button-get button 'frei/code-insight-entry)
                                     (button-get button 'frei/code-insight-line)))
  'follow-link t
  'face 'link)

(defun frei/code-insight--definition-line (name definitions)
  "Return the line of NAME in the DEFINITIONS alist, or nil.
A qualified NAME such as Class.method is looked up by its last part."
  (save-match-data
    (or (cdr (assoc name definitions))
        (when (string-match "[.:]\\([A-Za-z_][A-Za-z0-9_]*\\)\\'" name)
          (cdr (assoc (match-string 1 name) definitions))))))

(defun frei/code-insight--linkify (start end entry)
  "Turn names of the definitions of ENTRY between START and END into buttons."
  (when-let* ((definitions (plist-get (cdr entry) :definitions)))
    (save-excursion
      (goto-char start)
      (let ((inhibit-read-only t))
        (while (re-search-forward
                "`\\([A-Za-z_][A-Za-z0-9_.:]*\\)\\(?:()\\)?`\\|\\_<\\([A-Za-z_][A-Za-z0-9_]*\\)()"
                end t)
          (let* ((from (match-beginning 0))
                 (to (match-end 0))
                 (name (or (match-string 1) (match-string 2)))
                 (line (frei/code-insight--definition-line name definitions)))
            (when line
              (make-text-button from to
                                :type 'frei/code-insight-definition
                                'frei/code-insight-entry entry
                                'frei/code-insight-line line
                                'help-echo (format "跳到源码第 %d 行" line)))))))))

;;;; History file

(defun frei/code-insight--history-file ()
  "Return the file that keeps finished entries."
  (expand-file-name "history.md" frei/code-insight-directory))

(defun frei/code-insight--append-history (text)
  "Append TEXT to the history file."
  (make-directory frei/code-insight-directory t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil (frei/code-insight--history-file) t 'silent)))

(defun frei/code-insight--mark-sections ()
  "Mark the entries of a transcript read from a file as sections."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (re-search-forward
              "^## \\(.+\\)\n<!-- code-insight source=\"\\([^\"]*\\)\" task=\"\\([^\"]*\\)\" -->$"
              nil t)
        (let ((key (match-string-no-properties 2))
              (task (match-string-no-properties 3)))
          (add-text-properties (match-beginning 0) (match-end 1)
                               (list 'frei/code-insight-section t
                                     'frei/code-insight-source key
                                     'frei/code-insight-task task))
          (frei/code-insight--register-source (current-buffer) key)))
      ;; Outlines are long; keep them folded like in a live transcript.
      (goto-char (point-min))
      (while (re-search-forward "^### 结构概要$" nil t)
        (ignore-errors (outline-hide-subtree))))))

;;;; Running the agent

(defun frei/code-insight--treesit-available-p ()
  "Return non-nil when the current buffer can be drawn without the agent."
  (and (not frei/code-insight-mode)
       (frei/code-insight--grammar-available-p (frei/code-insight--language))))

(defun frei/code-insight--read-task ()
  "Ask for an analysis task; return (NAME . PROMPT).
The PROMPT is the symbol `treesit' for the diagram drawn without the
agent, which is offered only when the buffer has a grammar."
  (let ((choice (completing-read "分析任务: "
                                 (append (and (frei/code-insight--treesit-available-p)
                                              (list frei/code-insight--treesit-task))
                                         (mapcar #'car frei/code-insight-tasks)
                                         '("自定义…"))
                                 nil t)))
    (cond
     ((equal choice "自定义…")
      (let ((question (read-string "问题: " nil 'frei/code-insight-question-history)))
        (cons question question)))
     ((equal choice frei/code-insight--treesit-task)
      (cons choice 'treesit))
     (t
      (assoc choice frei/code-insight-tasks)))))

(defun frei/code-insight--task-arguments (name)
  "Return the extra agent arguments for the task called NAME."
  (if (assoc name frei/code-insight-tasks)
      (cdr (assoc name frei/code-insight-task-arguments))
    frei/code-insight-question-arguments))

(defun frei/code-insight--one-line (text width)
  "Return TEXT on one line, shortened to WIDTH columns."
  (truncate-string-to-width
   (replace-regexp-in-string "[ \t\n]+" " " (string-trim text)) width nil nil "…"))

(defun frei/code-insight--tick (process)
  "Refresh the elapsed time shown for PROCESS."
  (if (process-live-p process)
      (frei/code-insight--set-header
       (process-get process 'buffer)
       (format "%s · %d 秒" (process-get process 'state)
               (float-time (time-since (process-get process 'start-time)))))
    (when-let* ((timer (process-get process 'timer)))
      (cancel-timer timer))))

(defun frei/code-insight--state (process state)
  "Record STATE for PROCESS and show it."
  (process-put process 'state state)
  (frei/code-insight--tick process))

(defun frei/code-insight--handle-event (process line)
  "Handle one LINE of JSON output from PROCESS."
  (unless (string-blank-p line)
    (let ((event (condition-case nil
                     (json-parse-string line :object-type 'alist :array-type 'list
                                        :null-object nil :false-object nil)
                   (error nil)))
          (buffer (process-get process 'buffer)))
      (if (not event)
          (frei/code-insight--insert buffer (concat line "\n"))
        (pcase (alist-get 'type event)
          ("message_start"
           ;; Keep what the agent received, which helps when debugging.
           (let ((message (alist-get 'message event)))
             (when (equal (alist-get 'role message) "user")
               (process-put process 'request
                            (alist-get 'text (car (alist-get 'content message)))))))
          ("message_update"
           (let ((update (alist-get 'assistantMessageEvent event)))
             (pcase (alist-get 'type update)
               ("thinking_start" (frei/code-insight--state process "思考中"))
               ("text_start" (frei/code-insight--state process "生成中"))
               ("text_delta" (frei/code-insight--insert buffer (alist-get 'delta update))))))
          ("message_end"
           (let ((message (alist-get 'message event)))
             (when (equal (alist-get 'role message) "assistant")
               (process-put process 'model (alist-get 'model message))
               (process-put process 'usage (alist-get 'usage message))))))))))

(defun frei/code-insight--filter (process output)
  "Split OUTPUT of PROCESS into lines and handle complete ones."
  (let* ((pending (concat (process-get process 'pending) output))
         (lines (split-string pending "\n")))
    (process-put process 'pending (car (last lines)))
    (dolist (line (butlast lines))
      (frei/code-insight--handle-event process line))))

(defun frei/code-insight--summary (process event)
  "Describe how PROCESS ended with EVENT."
  (let* ((status (process-exit-status process))
         (usage (process-get process 'usage))
         (cost (alist-get 'total (alist-get 'cost usage)))
         (elapsed (float-time (time-since (process-get process 'start-time)))))
    (cond
     ((string-prefix-p "killed" event)
      (format "已取消 · %.1f 秒" elapsed))
     ((zerop status)
      (format "完成 · %s · %s tokens%s · %.1f 秒"
              (or (process-get process 'model) "?")
              (or (alist-get 'totalTokens usage) "?")
              (if (and (numberp cost) (> cost 0)) (format " · $%.4f" cost) "")
              elapsed))
     (t
      (format "失败 · 退出码 %d · %s" status (string-trim event))))))

(defun frei/code-insight--sentinel (process event)
  "Finish the answer of PROCESS after EVENT ended it."
  (unless (process-live-p process)
    (let ((buffer (process-get process 'buffer))
          (heading (process-get process 'heading))
          (section (process-get process 'section))
          (entry (process-get process 'entry))
          (stderr (process-get process 'stderr))
          (pending (process-get process 'pending)))
      (process-put process 'pending "")
      (frei/code-insight--handle-event process pending)
      (when-let* ((timer (process-get process 'timer)))
        (cancel-timer timer))
      (when-let* ((file (process-get process 'file)))
        (ignore-errors (delete-file file)))
      (let ((summary (frei/code-insight--summary process event))
            (failed (not (zerop (process-exit-status process)))))
        (when (and failed (buffer-live-p stderr))
          (let ((errors (string-trim (with-current-buffer stderr (buffer-string)))))
            (unless (string-empty-p errors)
              (frei/code-insight--insert buffer (format "\n\n```\n%s\n```\n" errors)))))
        (frei/code-insight--insert buffer (format "\n\n---\n_%s_\n" summary))
        (when (buffer-live-p stderr)
          (kill-buffer stderr))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq frei/code-insight--process nil)
            (unless failed
              (frei/code-insight--linkify section (point-max) entry)
              (frei/diagram-render-region section (point-max))
              (frei/code-insight--append-history
               (concat "\n" (buffer-substring-no-properties heading (point-max)))))))
        (frei/code-insight--set-header buffer (concat summary " · " frei/code-insight--legend))))))

(defun frei/code-insight--request (buffer entry heading task material arguments)
  "Send MATERIAL to the agent and stream the answer into BUFFER.
ENTRY is the source entry of the answer, HEADING its title, TASK the
task name recorded with it, and ARGUMENTS extra agent arguments."
  (let ((program (executable-find frei/code-insight-program)))
    (unless program
      (user-error "Cannot find the %s command" frei/code-insight-program))
    (with-current-buffer buffer
      (when (process-live-p frei/code-insight--process)
        (user-error "上一个请求还在进行，先按 k 取消")))
    (let* ((file (let ((coding-system-for-write 'utf-8-unix))
                   (make-temp-file "code-insight-" nil ".md" material)))
           (stderr (generate-new-buffer " *code-insight stderr*"))
           (heading-start (with-current-buffer buffer
                            (frei/code-insight--insert buffer "\n")
                            (copy-marker (point-max))))
           (section (progn
                      (frei/code-insight--insert
                       buffer
                       (concat (frei/code-insight--heading-line heading (car entry) task)
                               (format "\n<!-- code-insight source=\"%s\" task=\"%s\" -->\n\n"
                                       (car entry) task)))
                      (with-current-buffer buffer (copy-marker (point-max)))))
           (process (make-process
                     :name "code-insight"
                     :command (append (list program "-p" "--mode" "json")
                                      frei/code-insight-arguments
                                      arguments
                                      (and frei/code-insight-model
                                           (list "--model" frei/code-insight-model))
                                      (list (concat "@" file)
                                            frei/code-insight--instruction))
                     :coding 'utf-8
                     :connection-type 'pipe
                     :noquery t
                     :stderr stderr
                     :filter #'frei/code-insight--filter
                     :sentinel #'frei/code-insight--sentinel)))
      (process-put process 'buffer buffer)
      (process-put process 'entry entry)
      (process-put process 'file file)
      (process-put process 'stderr stderr)
      (process-put process 'heading heading-start)
      (process-put process 'section section)
      (process-put process 'pending "")
      (process-put process 'start-time (current-time))
      (process-put process 'state "启动中")
      (process-put process 'timer (run-at-time 1 1 #'frei/code-insight--tick process))
      ;; The agent waits for standard input to end before exiting.
      (process-send-eof process)
      (with-current-buffer buffer
        (setq frei/code-insight--process process))
      (frei/code-insight--tick process)
      (display-buffer buffer frei/code-insight-display-action)
      process)))

;;;###autoload
(defun frei/code-insight (task)
  "Ask the pi agent to analyze the region or the buffer according to TASK.
TASK is (NAME . PROMPT), one of `frei/code-insight-tasks' or a
question of your own.  The answer streams into the transcript buffer
next to the code; inside a transcript the transcript itself is the
material, so questions about earlier answers can be asked there."
  (interactive (list (frei/code-insight--read-task)))
  (if (eq (cdr task) 'treesit)
      (frei/code-insight-treesit-diagram)
    (frei/code-insight--ask task)))

(defun frei/code-insight--ask (task)
  "Send the region or the buffer to the agent for TASK, a (NAME . PROMPT) pair."
  (let* ((region (use-region-p))
         (beg (if region (region-beginning) (point-min)))
         (end (if region (region-end) (point-max)))
         (transcript frei/code-insight-mode)
         (buffer (if transcript (current-buffer)
                   (frei/code-insight--transcript (current-buffer))))
         (key (if transcript
                  (or (car (frei/code-insight--section-source)) "transcript")
                (frei/code-insight--source-key (current-buffer))))
         (name (frei/code-insight--one-line (car task) 60))
         (heading (format "%s · %s%s · %s"
                          (file-name-nondirectory key)
                          name
                          (if region
                              (format "（第 %d 到 %d 行）"
                                      (line-number-at-pos beg t)
                                      (line-number-at-pos end t))
                            "")
                          (format-time-string "%m-%d %H:%M"))))
    (pcase-let* ((`(,material ,note ,outline ,definitions)
                  (frei/code-insight--material beg end (cdr task)))
                 (entry (frei/code-insight--register-source buffer key definitions)))
      (deactivate-mark)
      (let ((process (frei/code-insight--request
                      buffer entry (format "%s  _%s_" heading note) name material
                      (frei/code-insight--task-arguments (car task)))))
        (when (and outline (not (plist-get (cdr entry) :outline)))
          (plist-put (cdr entry) :outline t)
          (frei/code-insight--insert-outline buffer outline)
          (set-marker (process-get process 'section)
                      (with-current-buffer buffer (point-max))))
        process))))

;;;###autoload
(defun frei/code-insight-treesit-diagram ()
  "Draw the call graph of the region or the buffer from tree-sitter alone.
Definitions become nodes, classes and namespaces become clusters, and
calls between them become edges; no agent is involved.  The drawing
goes into the transcript like an answer."
  (interactive)
  (unless (frei/code-insight--treesit-available-p)
    (user-error "当前 buffer 没有可用的 tree-sitter 语法库"))
  (let* ((region (use-region-p))
         (beg (if region (region-beginning) (point-min)))
         (end (if region (region-end) (point-max)))
         (started (current-time))
         (source (current-buffer))
         (language (frei/code-insight--language))
         (definitions (cdr (frei/code-insight--parse language beg end)))
         (names (let ((alist nil))
                  (dolist (definition definitions)
                    (let ((name (aref definition 1)))
                      (when (and name (not (assoc name alist)))
                        (push (cons name (aref definition 3)) alist))))
                  (nreverse alist)))
         (buffer (frei/code-insight--transcript source))
         (key (frei/code-insight--source-key source))
         (entry (frei/code-insight--register-source buffer key names))
         (task frei/code-insight--treesit-task)
         (heading (format "%s · %s%s · %s  _tree-sitter (%s)_"
                          (file-name-nondirectory key) task
                          (if region
                              (format "（第 %d 到 %d 行）"
                                      (line-number-at-pos beg t)
                                      (line-number-at-pos end t))
                            "")
                          (format-time-string "%m-%d %H:%M")
                          language)))
    (with-current-buffer buffer
      (when (process-live-p frei/code-insight--process)
        (user-error "上一个请求还在进行，先按 k 取消")))
    (deactivate-mark)
    (pcase-let ((`(,dot ,nodes ,edges) (frei/code-insight--call-graph definitions)))
      (frei/code-insight--insert buffer "\n")
      (let ((heading-start (with-current-buffer buffer (copy-marker (point-max)))))
        (frei/code-insight--insert
         buffer
         (concat (frei/code-insight--heading-line heading key task)
                 (format "\n<!-- code-insight source=\"%s\" task=\"%s\" -->\n\n" key task)))
        (when (and (not (plist-get (cdr entry) :outline)) (not (seq-empty-p definitions)))
          (plist-put (cdr entry) :outline t)
          (frei/code-insight--insert-outline
           buffer (car (frei/code-insight--analyze language beg end))))
        (let ((section (with-current-buffer buffer (copy-marker (point-max)))))
          (frei/code-insight--insert
           buffer
           (concat
            (format "%d 个定义，%d 条调用关系%s。\n\n"
                    nodes edges
                    (if (> (length definitions) nodes)
                        (format "，只画了前 %d 个" nodes)
                      ""))
            "```dot\n" dot "```\n\n"
            (when names
              (concat "节点："
                      (mapconcat (lambda (pair) (format "`%s`" (car pair))) names " ")
                      "\n"))
            (format "\n---\n_完成 · tree-sitter · %d 节点 · %d 边 · %.2f 秒_\n"
                    nodes edges (float-time (time-since started)))))
          (with-current-buffer buffer
            (frei/code-insight--linkify section (point-max) entry)
            (unless (frei/diagram-format-available-p 'dot)
              (frei/code-insight--insert
               buffer "_没有 documd，图只能以 DOT 源码显示。_\n"))
            (frei/diagram-render-region section (point-max))
            (frei/code-insight--append-history
             (concat "\n" (buffer-substring-no-properties heading-start (point-max))))
            (frei/code-insight--set-header
             buffer (format "完成 · tree-sitter · %d 节点 · %d 边 · %s"
                            nodes edges frei/code-insight--legend))))))
    (display-buffer buffer frei/code-insight-display-action)))

;;;; Transcript commands

(defun frei/code-insight-goto ()
  "Jump to an entry of the transcript chosen by its heading."
  (interactive)
  (let ((sections (frei/code-insight--sections))
        (choices nil))
    (unless sections
      (user-error "还没有条目"))
    (dolist (section sections)
      (let ((label (car section)) (n 1))
        (while (assoc label choices)
          (setq n (1+ n)
                label (format "%s (%d)" (car section) n)))
        (push (cons label (cdr section)) choices)))
    (let ((choice (completing-read "跳到: " (reverse choices) nil t)))
      (goto-char (cdr (assoc choice choices)))
      (recenter 0))))

(defun frei/code-insight-next-section ()
  "Move to the next entry of the transcript."
  (interactive)
  (let ((match (save-excursion
                 (end-of-line)
                 (text-property-search-forward 'frei/code-insight-section t t))))
    (goto-char (if match (prop-match-beginning match) (point-max)))))

(defun frei/code-insight-previous-section ()
  "Move to the previous entry of the transcript."
  (interactive)
  (let ((match (save-excursion
                 (beginning-of-line)
                 (text-property-search-backward 'frei/code-insight-section t t))))
    (goto-char (if match (prop-match-beginning match) (point-min)))))

(defun frei/code-insight-toggle-images (&optional whole)
  "Show or hide the diagrams of the entry at point.
With prefix argument WHOLE, do it for the whole transcript."
  (interactive "P")
  (if whole
      (frei/diagram-toggle-images)
    (pcase-let ((`(,start . ,end) (frei/code-insight--section-bounds)))
      (let ((overlays (seq-filter (lambda (overlay) (overlay-get overlay 'frei/diagram))
                                  (overlays-in start end))))
        (if overlays
            (mapc #'delete-overlay overlays)
          (message "正在渲染 %d 张图" (frei/diagram-render-region start end)))))))

(defun frei/code-insight-cancel ()
  "Stop the request that is running for this transcript."
  (interactive)
  (if (process-live-p frei/code-insight--process)
      (delete-process frei/code-insight--process)
    (message "没有正在进行的请求")))

(defun frei/code-insight-copy ()
  "Copy the answer around point, without its heading and summary line."
  (interactive)
  (pcase-let ((`(,start . ,end) (frei/code-insight--section-bounds)))
    (let ((text (buffer-substring-no-properties start end)))
      (setq text (replace-regexp-in-string "\\`## .*\n\\(<!--.*-->\n\\)?" "" text))
      (setq text (replace-regexp-in-string
                  "### 结构概要\n\n```\n\\(?:.*\n\\)*?```\n\n?" "" text))
      (setq text (replace-regexp-in-string "\n---\n_[^\n]*_\n*\\'" "\n" text))
      (kill-new (string-trim text))
      (message "已复制 %d 个字符" (length (string-trim text))))))

(defun frei/code-insight--diagram-at-point ()
  "Return the diagram overlay at point, or nil."
  (seq-find (lambda (overlay) (overlay-get overlay 'frei/diagram))
            (overlays-at (point))))

(defun frei/code-insight-save (file)
  "Save the diagram at point as an image, or the whole transcript, to FILE.
A PNG or SVG file name saves the diagram at point; any other name saves
the transcript as Markdown."
  (interactive
   (let* ((diagram (frei/code-insight--diagram-at-point))
          (source (plist-get (cdr (frei/code-insight--section-source)) :file))
          (stem (file-name-base (or source (buffer-name))))
          (default (if diagram
                       (format "%s-diagram.%s" stem
                               (plist-get (cdr (overlay-get diagram 'display)) :type))
                     (format "%s-insight.md" stem))))
     (list (read-file-name "保存到: " (and source (file-name-directory source))
                           nil nil default))))
  (let ((diagram (frei/code-insight--diagram-at-point)))
    (if (and diagram (member (downcase (or (file-name-extension file) "")) '("png" "svg")))
        (let ((data (plist-get (cdr (overlay-get diagram 'display)) :data))
              (coding-system-for-write 'binary))
          ;; PNG data is already bytes; SVG source is text.
          (write-region (if (multibyte-string-p data) (encode-coding-string data 'utf-8) data)
                        nil file))
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max) file)))
    (message "已保存到 %s" (abbreviate-file-name file))))

(defun frei/code-insight-visit-source ()
  "Show the source of the entry at point."
  (interactive)
  (let ((entry (frei/code-insight--section-source)))
    (unless entry
      (user-error "这里没有条目"))
    (frei/code-insight--jump entry 1)))

(defun frei/code-insight-clear ()
  "Remove every entry of the transcript."
  (interactive)
  (when (process-live-p frei/code-insight--process)
    (user-error "请求还在进行，先按 k 取消"))
  (when (yes-or-no-p "清空这个 buffer 的所有条目？")
    (let ((inhibit-read-only t)
          (title (save-excursion
                   (goto-char (point-min))
                   (buffer-substring (point) (line-end-position)))))
      (remove-overlays)
      (erase-buffer)
      (insert title "\n")
      (setq frei/code-insight--sources nil)
      (frei/code-insight--set-header (current-buffer) frei/code-insight--legend))))

;;;###autoload
(defun frei/code-insight-history ()
  "Show the entries kept in the history file.
Diagrams are rendered on demand with t."
  (interactive)
  (let ((file (frei/code-insight--history-file)))
    (unless (file-exists-p file)
      (user-error "还没有历史记录"))
    (let ((buffer (get-buffer-create "*code-insight history*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (coding-system-for-read 'utf-8))
          (remove-overlays)
          (erase-buffer)
          (insert "# code-insight 历史\n")
          (insert-file-contents file nil nil nil)
          (goto-char (point-max))
          (setq frei/code-insight--sources nil))
        (frei/code-insight--setup-buffer)
        (frei/code-insight--mark-sections)
        (goto-char (point-max)))
      ;; From a transcript, take over its window instead of splitting it.
      (pop-to-buffer buffer (if frei/code-insight-mode
                                '(display-buffer-same-window)
                              frei/code-insight-display-action))
      (message "j 跳转到条目，t 渲染当前条目的图"))))

;;;###autoload
(defun frei/code-insight-open-in-browser ()
  "Open the transcript in the browser through documd, or as Markdown.
The file is kept under `frei/code-insight-directory'."
  (interactive)
  (let* ((stem (format-time-string "code-insight-%Y%m%d-%H%M%S"))
         (directory (expand-file-name "answers/" frei/code-insight-directory))
         (markdown (expand-file-name (concat stem ".md") directory))
         (html (expand-file-name (concat stem ".html") directory))
         (documd (executable-find frei/diagram-program)))
    (make-directory directory t)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (point-min) (point-max) markdown nil 'silent))
    (if (and documd (eql 0 (call-process documd nil nil nil markdown html)))
        (browse-url-of-file html)
      (browse-url-of-file markdown))))

(provide 'code-insight)

;;; code-insight.el ends here
