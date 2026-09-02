;;; c-ts-google-style.el --- Google indentation for c-ts-mode  -*- lexical-binding: t; -*-

;;; Commentary:
;; `google-c-style' only configures cc-mode, and the tree-sitter modes
;; never run `c-mode-common-hook'.  This file provides an equivalent
;; `c-ts-mode-indent-style' function for `c-ts-mode' and `c++-ts-mode',
;; layered on top of the built-in rules of Emacs 30 and newer.
;;
;; Covered: namespace bodies are not indented, access specifiers are
;; indented by one column, case labels are indented inside switch, and
;; constructor initializers, base class lists, wrapped arguments and
;; continued statements use a four column continuation indent.
;;
;; Add `frei/c-ts-google-setup' to `c-ts-base-mode-hook' to use it.

;;; Code:
(require 'treesit)

(declare-function c-ts-mode--simple-indent-rules "c-ts-mode" (mode style))
(declare-function c-ts-mode--indent-styles "c-ts-mode" (mode))
(declare-function c-ts-mode-set-style "c-ts-mode" (style))
(defvar c-ts-common-indent-offset)

(defconst frei/c-ts-google-continuation-offset 4
  "Indentation of continued statements and wrapped argument lists.")

(defconst frei/c-ts-google--bracket-regexp
  (rx bos
      (or "argument_list" "parameter_list" "parenthesized_expression"
          "condition_clause" "initializer_list" "template_argument_list"
          "template_parameter_list" "subscript_argument_list"
          "lambda_capture_specifier" "field_initializer_list"
          "compound_statement" "declaration_list" "field_declaration_list"
          "enumerator_list")
      eos)
  "Node types whose children are indented relative to the bracket.")

(defconst frei/c-ts-google--expression-regexp
  (rx bos (or (seq (+ (any "a-z_")) "_expression") "init_declarator") eos)
  "Node types that continue the statement they belong to.")

(defconst frei/c-ts-google--statement-regexp
  (rx bos
      (or (seq (+ (any "a-z_")) "_statement")
          "declaration" "field_declaration" "alias_declaration"
          "type_definition" "static_assert_declaration")
      eos)
  "Node types that can start a statement continued on later lines.")

(defconst frei/c-ts-google--binary-regexp
  (rx bos "binary_expression" eos)
  "Node type of binary expressions.")

(defconst frei/c-ts-google--condition-regexp
  (rx bos "condition_clause" eos)
  "Node type of the parenthesized condition of if, while and switch in C++.")

(defun frei/c-ts-google--standalone-start (node)
  "Return the start of the closest ancestor of NODE that begins its line.
NODE itself is considered first."
  (save-excursion
    (catch 'found
      (while node
        (goto-char (treesit-node-start node))
        (when (looking-back (rx bol (* blank)) (line-beginning-position))
          (throw 'found (point)))
        (setq node (treesit-node-parent node))))))

(defun frei/c-ts-google--first-named-sibling (_node parent &rest _)
  "Anchor on the first named child of PARENT, or on PARENT itself."
  (treesit-node-start (or (treesit-node-child parent 0 t) parent)))

(defun frei/c-ts-google--class-member-rule (node parent &rest _)
  "Indent members of a class body one level in from the class line.
Access specifiers are indented by a single column instead.  NODE and
PARENT are as in `treesit-simple-indent-rules'."
  (when (and node parent
             (treesit-node-match-p parent (rx bos "field_declaration_list" eos))
             (treesit-node-check node 'named)
             (not (treesit-node-match-p node (rx bos "preproc"))))
    (when-let* ((start (frei/c-ts-google--standalone-start parent)))
      (cons start
            (if (treesit-node-match-p node (rx bos "access_specifier" eos))
                1
              (symbol-value c-ts-common-indent-offset))))))

(defun frei/c-ts-google--enclosing (node parent stop-regexp)
  "Return the ancestor of NODE matching STOP-REGEXP through expressions only.
PARENT is the parent of NODE.  Return nil when a bracket or a
non-expression node is reached first."
  (when (and node parent
             (not (treesit-node-match-p node frei/c-ts-google--bracket-regexp)))
    (let ((ancestor parent)
          (found nil))
      (while (and ancestor (not found))
        (cond
         ((treesit-node-match-p ancestor stop-regexp)
          (setq found ancestor))
         ((treesit-node-match-p ancestor frei/c-ts-google--bracket-regexp)
          (setq ancestor nil))
         ((treesit-node-match-p ancestor frei/c-ts-google--expression-regexp)
          (setq ancestor (treesit-node-parent ancestor)))
         (t
          (setq ancestor nil))))
      found)))

(defun frei/c-ts-google--continued-statement (node parent)
  "Return the statement that NODE continues, or nil.
NODE starts a line inside an expression owned by the returned
statement, and PARENT is its parent node.  Lines inside brackets are
left to the built-in rules."
  (cond
   ((treesit-node-match-p parent frei/c-ts-google--expression-regexp)
    (frei/c-ts-google--enclosing node parent frei/c-ts-google--statement-regexp))
   ;; static const int kValue =
   ;;     42;
   ((and (treesit-node-match-p parent frei/c-ts-google--statement-regexp)
         (member (treesit-node-field-name node) '("default_value" "value")))
    parent)))

(defun frei/c-ts-google--first-operator (parent)
  "Return the start of the leftmost operator of the binary chain around PARENT.
Return nil when the chain is incomplete."
  (let ((top parent)
        (up nil))
    (while (and (setq up (treesit-node-parent top))
                (treesit-node-match-p up frei/c-ts-google--binary-regexp))
      (setq top up))
    (let ((left top)
          (child nil))
      (while (and (setq child (treesit-node-child-by-field-name left "left"))
                  (treesit-node-match-p child frei/c-ts-google--binary-regexp))
        (setq left child))
      (when-let* ((operator (treesit-node-child-by-field-name left "operator")))
        (treesit-node-start operator)))))

(defun frei/c-ts-google--condition-rule (node parent &rest _)
  "Align a continued if, while or switch condition with its first line.
NODE and PARENT are as in `treesit-simple-indent-rules'."
  (when (and node parent
             (not (treesit-node-match-p node (rx bos ")" eos)))
             (treesit-node-match-p parent frei/c-ts-google--expression-regexp))
    (when-let* ((clause (frei/c-ts-google--enclosing
                         node parent frei/c-ts-google--condition-regexp))
                (first (treesit-node-child clause 0 t)))
      (unless (treesit-node-eq first node)
        (cons (treesit-node-start first) 0)))))

(defun frei/c-ts-google--continuation-rule (node parent &rest _)
  "Indent a continued statement by `frei/c-ts-google-continuation-offset'.
Stream operators starting a line align with the first one, like
`c-lineup-streamop'.  NODE and PARENT are as in
`treesit-simple-indent-rules'."
  (when (and node parent)
    (when-let* ((statement (frei/c-ts-google--continued-statement node parent)))
      (let ((operator
             (and (treesit-node-match-p node (rx bos (or "<<" ">>") eos))
                  (treesit-node-match-p parent frei/c-ts-google--binary-regexp)
                  (frei/c-ts-google--first-operator parent))))
        (if operator
            (cons operator 0)
          (cons (treesit-node-start statement)
                frei/c-ts-google-continuation-offset))))))

(defun frei/c-ts-google--base-rules (language)
  "Return the built-in BSD indent rules of `c-ts-mode' for LANGUAGE.
BSD keeps braces aligned with the statement, which is what Google
style does when a brace ends up on its own line."
  (cond
   ((fboundp 'c-ts-mode--simple-indent-rules)
    (alist-get language (c-ts-mode--simple-indent-rules language 'bsd)))
   ((fboundp 'c-ts-mode--indent-styles)
    (alist-get 'bsd (c-ts-mode--indent-styles language)))
   ((fboundp 'c-ts-common-baseline-indent-rule)
    (list 'c-ts-common-baseline-indent-rule))))

(defun frei/c-ts-google-indent-style ()
  "Return `treesit-simple-indent-rules' rules approximating Google style.
The list does not contain the language symbol, as expected by
`c-ts-mode-set-style'."
  (let* ((language (if (derived-mode-p 'c++-ts-mode) 'cpp 'c))
         (offset (symbol-value c-ts-common-indent-offset))
         (continuation frei/c-ts-google-continuation-offset)
         (base (frei/c-ts-google--base-rules language))
         (google
          `(;; Namespace bodies stay at the namespace column.
            ((n-p-gp nil
                     ,(rx bos "declaration_list" eos)
                     ,(rx bos "namespace_definition" eos))
             standalone-parent 0)
            ;; class Foo
            ;;     : public Bar,
            ;;       public Baz {
            ((node-is ,(rx bos "base_class_clause" eos))
             standalone-parent ,continuation)
            ((parent-is ,(rx bos "base_class_clause" eos))
             frei/c-ts-google--first-named-sibling 0)
            frei/c-ts-google--class-member-rule
            ;; Foo::Foo()
            ;;     : member_(0) {}
            ((node-is ,(rx bos "field_initializer_list" eos))
             standalone-parent ,continuation)
            ;; Case labels are indented inside the switch.
            ((node-is ,(rx bos "case_statement" eos))
             standalone-parent ,offset)
            ;; A first argument or parameter on its own line.
            ((match ,(rx bos (not (any ")")))
                    ,(rx bos (or "argument_list" "parameter_list") eos)
                    nil 1 1)
             standalone-parent ,continuation)))
         (continued '(frei/c-ts-google--condition-rule
                      frei/c-ts-google--continuation-rule)))
    ;; The continuation rules only take over from the baseline rule, so
    ;; keep them behind every specialised built-in rule.
    (if (eq (car (last base)) 'c-ts-common-baseline-indent-rule)
        (append google (butlast base) continued (last base))
      (append google continued base))))

(defun frei/c-ts-google-setup ()
  "Apply Google style to the current `c-ts-mode' or `c++-ts-mode' buffer."
  (c-ts-mode-set-style #'frei/c-ts-google-indent-style)
  (setq-local comment-column 40))

(provide 'c-ts-google-style)

;;; c-ts-google-style.el ends here
