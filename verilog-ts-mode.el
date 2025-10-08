;;; verilog-ts-mode.el --- Verilog Tree-sitter major mode  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/verilog-ts-mode
;; Version: 0.4.0
;; Keywords: SystemVerilog, IDE, Tools
;; Package-Requires: ((emacs "29.1") (verilog-mode "2024.3.1.121933719"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode to navigate and edit SystemVerilog files with tree-sitter.

;; Provides tree-sitter based implementations for the following features:
;; - Syntax highlighting
;; - Indentation
;; - `imenu'
;; - `which-func'
;; - Navigation functions
;; - Prettify and beautify
;; - Completion at point

;; Contributions:
;;   This major mode is still under active development!
;;   Check contributing guidelines:
;;     - https://github.com/gmlarumbe/verilog-ts-mode#contributing

;; Troubleshooting:
;;   This version requires at least v0.3.1 of tree-sitter-systemverilog grammar:
;;     - https://github.com/gmlarumbe/tree-sitter-systemverilog
;;   To install it run the command below:
;;     - M-x verilog-ts-install-grammar RET

;;; Code:

;;; Requirements
(require 'treesit)
(require 'imenu)
(require 'verilog-mode)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")


;;; Customization
(defgroup verilog-ts nil
  "SystemVerilog tree-sitter mode."
  :group 'languages)

(defcustom verilog-ts-indent-level 4
  "Tree-sitter indentation of Verilog statements with respect to containing block."
  :group 'verilog-ts
  :type 'integer)

(defcustom verilog-ts-file-extension-re "\\.s?vh?\\'"
  "(SystemVerilog) file extensions.
Defaults to .v, .vh, .sv and .svh."
  :group 'verilog-ts
  :type 'string)

(defcustom verilog-ts-imenu-style 'tree
  "Style of generated Imenu index for current Verilog buffer.

- Simple: default basic grouping into categories.
- Tree: tree-like structure without further processing.
- Tree-group: tree-like structure with some processing to group same type
              elements into subcategories (useful for display with third party
              packages such as `imenu-list')."
  :type '(choice (const :tag "simple" simple)
                 (const :tag "tree" tree)
                 (const :tag "tree-group" tree-group))
  :group 'verilog-ts)

(defcustom verilog-ts-which-func-style 'custom
  "Style of `which-func' display for current Verilog buffer.

- Simple: show last element of current Imenu entry
- Breadcrumb: display hierarchy of current Imenu entry
- Custom: use custom `verilog-ts-mode' implementation for `which-func':
    - Format A:B
    - A represents the type of current node, B its name
    - For instances, A is the instance name and B the instance type."
  :type '(choice (const :tag "simple" simple)
                 (const :tag "breadcrumb" breadcrumb)
                 (const :tag "custom" custom))
  :group 'verilog-ts)

(defcustom verilog-ts-align-decl-expr-comments t
  "Non-nil means align declaration and expressions comments.

Alignment is performed after execution of `verilog-ts-pretty-declarations' and
`verilog-ts-pretty-expr'."
  :group 'verilog-ts
  :type 'boolean)

(defcustom verilog-ts-beautify-instance-extra nil
  "Set to non-nil to perform extra formatting on instances."
  :type 'boolean
  :group 'verilog-ts)

(defcustom verilog-ts-linter-enable t
  "Non-nil means enable tree-sitter based linting."
  :group 'verilog-ts
  :type 'boolean)


;;; Utils
;;;; Core
(defconst verilog-ts-instance-re "\\_<\\(module\\|interface\\|program\\|gate\\|udp\\|checker\\)_instantiation\\_>")
(defconst verilog-ts-port-header-ts-re "\\_<\\(variable\\|net\\|interface\\)_port_header\\_>")
(defconst verilog-ts-declaration-node-re
  (eval-when-compile
    (regexp-opt
     '("package_declaration"
       "class_declaration"
       "interface_class_declaration"
       "function_body_declaration"
       "task_body_declaration"
       "function_prototype"
       "task_prototype"
       "sequence_declaration"
       "property_declaration"
       "checker_declaration"
       "config_declaration")
     'symbols)))

(defun verilog-ts--node-at-point (&optional bound)
  "Return tree-sitter node at point.

If optional BOUND is non-nil, return nil if point is not over a symbol."
  (let* ((pos (point))
         (node (treesit-node-at pos 'verilog)))
    (if bound
        (when (and (>= pos (treesit-node-start node))
                   (<= pos (treesit-node-end node)))
          node)
      node)))

(defun verilog-ts--highest-node-at-pos (pos)
  "Return highest node starting at POS in the parsed tree.

Return nil if POS is at a whitespace character.

Snippet fetched from `treesit--indent-1'."
  (let* ((smallest-node (verilog-ts--node-at-point))
         (node (treesit-parent-while
                smallest-node
                (lambda (node)
                  (eq pos (treesit-node-start node))))))
    node))

(defun verilog-ts--highest-node-at-point (&optional bound)
  "Return highest node at point.

If optional BOUND is non-nil, return nil if point is not over a symbol."
  (verilog-ts--highest-node-at-pos (treesit-node-start (verilog-ts--node-at-point bound))))

(defun verilog-ts--node-at-bol (&optional skip-comment)
  "Return node at first non-blank character of current line.

Snippet fetched from `treesit--indent-1'.

If optional SKIP-COMMENT is non-nil return first node without taking comments
into account."
  (let* ((bol (save-excursion
                (forward-line 0)
                (skip-chars-forward " \t")
                (point)))
         (smallest-node
          (cond ((null (treesit-parser-list)) nil)
                ((eq 1 (length (treesit-parser-list)))
                 (treesit-node-at bol))
                ((treesit-language-at (point))
                 (treesit-node-at bol (treesit-language-at (point))))
                (t (treesit-node-at bol))))
         (node (treesit-parent-while
                smallest-node
                (lambda (node)
                  (eq bol (treesit-node-start node))))))
    (if (and skip-comment
             (string-match "\\_<\\(block\\|one_line\\)_comment\\_>" (treesit-node-type node))
             (> (line-end-position) (treesit-node-end node)))
        (progn
          (goto-char (treesit-node-end node))
          (skip-chars-forward " \t")
          (verilog-ts--highest-node-at-pos (point)))
      node)))

(defun verilog-ts--node-has-parent-recursive (node node-type)
  "Return non-nil if NODE is part of NODE-TYPE in the parsed tree."
  (treesit-parent-until
   node
   (lambda (node) ; Third argument must be a function
     (string-match node-type (treesit-node-type node)))))

(defun verilog-ts--node-has-child-recursive (node node-type)
  "Return first node of type NODE-TYPE that is a child of NODE in the parsed tree.
If none is found, return nil."
  (when node
    (treesit-search-subtree node node-type nil :all)))

(defun verilog-ts--node-parents-list (node node-type)
  "Return NODE parents that match NODE-TYPE as a list of nodes."
  (let (parent parent-list)
    (while (setq parent (verilog-ts--node-has-parent-recursive node node-type))
      (push parent parent-list)
      (setq node parent))
    (nreverse parent-list)))

(defun verilog-ts--node-identifier-name (node)
  "Return identifier name of NODE."
  (when node
    (cond (;; module/interface/program
           (string-match "\\_<\\(module\\|interface\\|program\\)_declaration\\_>" (treesit-node-type node))
           (or (treesit-node-text (treesit-node-child-by-field-name (treesit-search-subtree node "\\_<\\(module\\|interface\\|program\\)_\\(non\\)?ansi_header") "name") :no-props)
               (treesit-node-text (treesit-node-child-by-field-name node "name") :no-props))) ; extern module instantiation
          ;; package/class/function/task/checker/config/property/sequence
          ((string-match verilog-ts-declaration-node-re (treesit-node-type node))
           (treesit-node-text (treesit-node-child-by-field-name node "name") :no-props))
          ;; class constructor
          ((string-match "\\_<class_constructor_\\(prototype\\|declaration\\)\\_>" (treesit-node-type node))
           "new")
          ;; typedefs
          ((string-match "\\_<type_declaration\\_>" (treesit-node-type node))
           (let* ((named-nodes (treesit-node-children node :named))
                  (type-node (car (seq-filter (lambda (named-node)
                                                (string= (treesit-node-field-name named-node) "type_name"))
                                              named-nodes))))
             (treesit-node-text type-node :no-prop)))
          ;; function/task declaration (body/prototype in previous case)
          ((string-match "\\_<\\(task\\|function\\)_declaration\\_>" (treesit-node-type node))
           (verilog-ts--node-identifier-name (treesit-search-subtree node "\\_<\\(task\\|function\\)_body_declaration\\_>")))
          ;; class property
          ((string-match "\\_<class_property\\_>" (treesit-node-type node))
           (or (treesit-node-text (treesit-node-child-by-field-name (treesit-search-subtree node "\\_<variable_decl_assignment\\_>") "name") :no-props)
               (verilog-ts--node-identifier-name (treesit-search-subtree node "\\_<type_declaration\\_>"))))
          ;; instances
          ((string-match verilog-ts-instance-re (treesit-node-type node))
           (treesit-node-text (treesit-node-child-by-field-name node "instance_type") :no-props))
          ;; ports
          ((string-match "\\_<tf_port_item\\_>" (treesit-node-type node))
           (treesit-node-text (treesit-node-child-by-field-name node "name") :no-props))
          ((string-match "\\_<ansi_port_declaration\\_>" (treesit-node-type node))
           (treesit-node-text (treesit-node-child-by-field-name node "port_name") :no-props))
          ;; parameter/localparam
          ((string-match "\\_<\\(local_\\)?parameter_declaration\\_>" (treesit-node-type node))
           (let* ((param-name-node (treesit-search-subtree node "\\_<\\(param\\|type\\)_assignment\\_>"))
                  (node-identifier (treesit-search-subtree param-name-node "\\_<simple_identifier\\_>")))
             (treesit-node-text node-identifier :no-prop)))
          ;; struct/union member
          ((string-match "\\_<struct_union_member\\_>" (treesit-node-type node))
           (treesit-node-text (treesit-node-child-by-field-name (treesit-search-subtree node "\\_<variable_decl_assignment\\_>") "name") :no-prop))
          ;; Generic data_type (for structs/enums)
          ((string-match "\\_<data_type_or_\\(implicit\\|void\\)\\_>" (treesit-node-type node))
           (let ((var-name-node (treesit-node-next-sibling node)))
             (if var-name-node
                 (verilog-ts--node-identifier-name (treesit-search-subtree var-name-node "\\_<variable_decl_assignment\\_>"))
               (treesit-node-text (treesit-search-subtree node "\\_<simple_identifier\\_>") :no-prop)))) ; default
          ;; default
          (t
           (treesit-node-text (treesit-search-subtree node "\\_<simple_identifier\\_>") :no-prop)))))

(defun verilog-ts--node-identifier-type (node)
  "Return identifier type of NODE."
  (let ((type (treesit-node-type node)))
    (cond (;; Typedefs (+ typedef struct/enum)- INFO: Needs to be placed before "\\_<variable_decl_assignment\\_>" since it is a particular case of it
           (string-match "\\_<type_declaration\\_>" type)
           (let* ((start-node (verilog-ts--node-has-parent-recursive node "\\_<data_declaration\\_>"))
                  (end-node (verilog-ts--node-has-child-recursive start-node "\\_<\\(class\\|data\\)_type\\_>")))
             (if end-node
                 (treesit-node-text end-node :no-prop)
               ;; Check if it's a typedef class
               (when (and (verilog-ts--node-has-child-recursive start-node "typedef")
                          (verilog-ts--node-has-child-recursive start-node "class"))
                 "typedef class"))))
          (;; Variables (+ struct/enum)
           (string-match "\\_<variable_decl_assignment\\_>" type)
           (let* ((start-node (or (verilog-ts--node-has-parent-recursive node "\\_<struct_union_member\\_>") ; Order is important:
                                  (verilog-ts--node-has-parent-recursive node "\\_<class_property\\_>")      ; - From less generic to more generic
                                  (verilog-ts--node-has-parent-recursive node "\\_<data_declaration\\_>")))
                  (data-type-node (when start-node
                                    (treesit-search-subtree start-node "\\_<data_type_or_\\(implicit\\|void\\)\\_>")))
                  (array-indexes (mapconcat (lambda (node)
                                              (treesit-node-text node :no-prop))
                                            (verilog-ts-nodes "\\_<\\(unsized\\|unpacked\\|associative\\|queue\\)_dimension\\_>" node)))
                  (start-node-start-pos (treesit-node-start start-node))
                  (data-type-node-start-pos (treesit-node-start data-type-node)))
             (concat
              (when (and (or (string= (treesit-node-type start-node) "class_property")
                             (string= (treesit-node-type start-node) "data_declaration"))
                         (< start-node-start-pos data-type-node-start-pos))
                (concat (string-trim-right (buffer-substring-no-properties start-node-start-pos data-type-node-start-pos)) " "))
              (when data-type-node
                (treesit-node-text data-type-node :no-props))
              (when (not (string= array-indexes ""))
                (concat " / " array-indexes)))))
          (;; Nets
           (string-match "\\_<net_decl_assignment\\_>" type)
           (let* ((start-node (verilog-ts--node-has-parent-recursive node "\\_<net_declaration\\_>"))
                  (end-node (treesit-search-subtree start-node "\\_<net_decl_assignment\\_>"))
                  (array-indexes (mapconcat (lambda (node)
                                              (treesit-node-text node :no-prop))
                                            (verilog-ts-nodes "\\_<unpacked_dimension\\_>" node))))
             (concat
              (string-trim-right (buffer-substring-no-properties (treesit-node-start start-node) (treesit-node-start end-node)))
              (when (not (string= array-indexes ""))
                (concat " / " array-indexes)))))
          (;; Module/interface/program ports
           (string-match "\\_<ansi_port_declaration\\_>" type)
           (treesit-node-text (treesit-search-subtree node verilog-ts-port-header-ts-re) :no-prop))
          (;; Task/function arguments
           (string-match "\\_<tf_port_item\\_>" type)
           (let ((port-direction (treesit-node-text (treesit-search-subtree node "\\_<tf_port_direction\\_>") :no-prop))
                 (array-indexes (mapconcat (lambda (node)
                                             (treesit-node-text node :no-prop))
                                           (verilog-ts-nodes "\\_<\\(unsized\\|unpacked\\|associative\\|queue\\)_dimension\\_>" node))))
             (concat (when port-direction (concat port-direction " "))
                     (treesit-node-text (treesit-search-subtree node "\\_<data_type_or_implicit\\_>") :no-prop)
                     (when (not (string= array-indexes ""))
                       (concat " / " array-indexes)))))
          (t ;; Default
           type))))

(defun verilog-ts--node-instance-name (node)
  "Return identifier name of NODE.

Node must be of type `verilog-ts-instance-re'.  Otherwise return nil."
  (unless (and node (string-match verilog-ts-instance-re (treesit-node-type node)))
    (error "Wrong node type: %s" (treesit-node-type node)))
  (let ((child-node (treesit-search-subtree node "name_of_instance")))
    (treesit-node-text (treesit-node-child-by-field-name child-node "instance_name") :no-props)))

(defun verilog-ts--inside-module-or-interface-p ()
  "Return non-nil if point is inside a module or interface construct."
  (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) "\\_<\\(module\\|interface\\)_declaration\\_>"))

(defun verilog-ts--node-is-typedef-class (node)
  "Return declared class name if NODE is a typedef class declaration."
  (let ((type (treesit-node-type node)))
    (when (and node
               (string-match "\\_<type_declaration\\_>" type)
               (string= (treesit-node-text (treesit-node-child node 0) :no-prop) "typedef")
               (string= (treesit-node-text (treesit-node-child node 1) :no-prop) "class"))
      (treesit-node-text (treesit-node-child-by-field-name node "type_name") :no-prop))))

(defun verilog-ts--node-module-ports (node)
  "Return current NODE list of ports if a module or interface."
  (unless (and node (string-match "\\_<\\(module\\|interface\\)_declaration\\_>" (treesit-node-type node)))
    (error "Wrong node type: %s" (treesit-node-type node)))
  (let (header-node port-nodes)
    (cond (;; ANSI declaration
           (setq header-node (treesit-search-subtree node "\\_<module_ansi_header\\_>"))
           (setq port-nodes (mapcar #'car (cdr (treesit-induce-sparse-tree header-node "\\_<ansi_port_declaration\\_>"))))
           (mapcar (lambda (port-node)
                     `(,(treesit-node-text (treesit-node-child-by-field-name port-node "port_name") :no-props)
                       :data-type ,(or (treesit-node-text (treesit-search-subtree port-node "\\_<\\(net\\|variable\\)_port_type\\_>") :no-props)
                                       (treesit-node-text (treesit-search-subtree port-node "\\_<interface_port_header\\_>") :no-props))
                       :direction ,(treesit-node-text (treesit-search-subtree port-node "\\_<port_direction\\_>") :no-props)
                       :packed-dim ,(when (treesit-search-subtree port-node "\\_<packed_dimension\\_>")
                                      (mapconcat (lambda (node)
                                                   (treesit-node-text node :no-props))
                                                 (mapcar #'car (cdr (treesit-induce-sparse-tree port-node "\\_<packed_dimension\\_>")))))
                       :unpacked-dim ,(when (treesit-search-subtree port-node "\\_<unpacked_dimension\\_>")
                                        (mapconcat (lambda (node)
                                                     (treesit-node-text node :no-props))
                                                   (mapcar #'car (cdr (treesit-induce-sparse-tree port-node "\\_<unpacked_dimension\\_>")))))))
                   port-nodes))
          (;; non-ANSI declaration -> port_declaration
           (treesit-search-subtree node "\\_<module_nonansi_header\\_>")
           (setq port-nodes (mapcar #'car (cdr (treesit-induce-sparse-tree node "\\_<port_declaration\\_>"))))
           (mapcar (lambda (port-node)
                     `(,(treesit-node-text (treesit-search-subtree (treesit-search-subtree port-node "\\_<list_of_port_identifiers\\_>") "\\_<simple_identifier\\_>") :no-props)
                       :data-type ,(treesit-node-text (treesit-search-subtree port-node "\\_<\\(net\\|variable\\)_port_type\\_>") :no-props)
                       :direction ,(treesit-node-text (treesit-search-subtree port-node "\\_<\\(input\\|output\\|inout\\|ref\\)\\_>" nil t) :no-props)
                       :packed-dim ,(when (treesit-search-subtree port-node "\\_<packed_dimension\\_>")
                                      (mapconcat (lambda (node)
                                                   (treesit-node-text node :no-props))
                                                 (mapcar #'car (cdr (treesit-induce-sparse-tree port-node "\\_<packed_dimension\\_>")))))
                       :unpacked-dim ,(when (treesit-search-subtree port-node "\\_<unpacked_dimension\\_>")
                                        (mapconcat (lambda (node)
                                                     (treesit-node-text node :no-props))
                                                   (mapcar #'car (cdr (treesit-induce-sparse-tree port-node "\\_<unpacked_dimension\\_>")))))))
                   port-nodes))
          (t
           (error "Unexpected error")))))

(defun verilog-ts--node-module-parameters (node)
  "Return current NODE list of parameters if a module or interface."
  (unless (and node (string-match "\\_<\\(module\\|interface\\)_declaration\\_>" (treesit-node-type node)))
    (error "Wrong node type: %s" (treesit-node-type node)))
  (let (header-node param-nodes temp-node)
    (when (setq header-node (treesit-search-subtree node "\\_<module_ansi_header\\_>"))
      (setq param-nodes (mapcar #'car (cdr (treesit-induce-sparse-tree header-node "\\_<parameter_declaration\\_>"))))
      (mapcar (lambda (param-node)
                (cond ((setq temp-node (treesit-search-subtree param-node "\\_<param_assignment\\_>"))
                       `(,(treesit-node-text (treesit-search-subtree temp-node "\\_<simple_identifier\\_>") :no-props)
                         :data-type ,(treesit-node-text (treesit-search-subtree param-node "\\_<data_type_or_implicit\\_>") :no-props)
                         :default-value ,(treesit-node-text (treesit-search-subtree param-node "\\_<constant_param_expression\\_>") :no-props)
                         :is-parameter-type nil))
                      ((setq temp-node (treesit-search-subtree param-node "\\_<type_assignment\\_>"))
                       `(,(treesit-node-text (treesit-node-child-by-field-name temp-node "name") :no-props)
                         :data-type nil
                         :default-value ,(treesit-node-text (treesit-node-child-by-field-name temp-node "value") :no-props)
                         :is-parameter-type t))
                      (t
                       (user-error "Unexpected error"))))
              param-nodes))))


;;;; Context
(defconst verilog-ts-block-at-point-re
  (eval-when-compile
    (regexp-opt
     '("module_declaration"
       "interface_declaration"
       "program_declaration"
       "package_declaration"
       "class_declaration"
       "function_declaration"
       "task_declaration"
       "class_constructor_declaration"
       "function_prototype"
       "task_prototype"
       "class_constructor_prototype"
       "module_instantiation"
       "interface_instantiation"
       "always_construct"
       "initial_construct"
       "final_construct"
       "generate_region"
       "seq_block")
     'symbols)))

(defun verilog-ts-nodes (pred &optional start)
  "Return current buffer NODES that match PRED.

If optional arg START is non-nil, use it as the initial node to search in the
tree."
  (let ((root-node (or start (treesit-buffer-root-node))))
    (mapcar #'car (cdr (treesit-induce-sparse-tree root-node pred)))))

(defun verilog-ts-nodes-props (pred &optional start)
  "Return nodes and properties that satisfy PRED in current buffer.

If optional arg START is non-nil, use it as the initial node to search in the
tree.

Returned properties are a property list that include node name, start position
and end position."
  (mapcar (lambda (node)
            `(,node :name ,(verilog-ts--node-identifier-name node)
                    :start-pos ,(treesit-node-start node)
                    :end-pos ,(treesit-node-end node)))
          (verilog-ts-nodes pred start)))

(defun verilog-ts-module-at-point ()
  "Return node of module at point."
  (let ((module (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) "module_declaration"))
        (pos (point)))
    (when (and module
               (>= pos (treesit-node-start module))
               (<= pos (treesit-node-end module)))
      module)))

(defun verilog-ts-instance-at-point ()
  "Return node of instance at point."
  (let ((instance (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) verilog-ts-instance-re))
        (pos (point)))
    (when (and instance
               (>= pos (treesit-node-start instance))
               (<= pos (treesit-node-end instance)))
      instance)))

(defun verilog-ts--block-at-point (regex)
  "Return deepest node of block at point that matches REGEX."
  (let ((blocks (verilog-ts--node-parents-list (verilog-ts--node-at-point) regex))
        (pos (point)))
    (catch 'block
      (mapc (lambda (block)
              (when (and block
                         (>= pos (treesit-node-start block))
                         (<= pos (treesit-node-end block)))
                (throw 'block block)))
            blocks)
      nil)))

(defun verilog-ts-block-at-point ()
  "Return node of block at point."
  (verilog-ts--block-at-point verilog-ts-block-at-point-re))

(defun verilog-ts-nodes-block-at-point (pred)
  "Return block at point NODES that match PRED."
  (let ((block (verilog-ts-block-at-point)))
    (when block
      (mapcar #'car (cdr (treesit-induce-sparse-tree block pred))))))

(defun verilog-ts-children-block-at-point (pred)
  "Return block at point children NODES that match PRED."
  (let ((block (verilog-ts-block-at-point)))
    (when block
      (mapcar #'car (cdr (treesit-filter-child block pred))))))

(defun verilog-ts-search-node-block-at-point (pred &optional backward all)
  "Search forward for node matching PRED inside block at point.

By default, only search for named nodes, but if ALL is non-nil, search
for all nodes.  If BACKWARD is non-nil, search backwards."
  (treesit-search-forward (verilog-ts--node-at-point)
                          (lambda (node)
                            (and (string-match pred (treesit-node-type node))
                                 (< (treesit-node-end node) (treesit-node-end (verilog-ts-block-at-point)))))
                          backward
                          all))

;; Some examples using previous API
(defun verilog-ts-module-declarations-nodes-current-buffer ()
  "Return module declaration nodes of current file."
  (verilog-ts-nodes "module_declaration"))

(defun verilog-ts-module-declarations-current-buffer ()
  "Return module declaration names of current file."
  (mapcar (lambda (node-and-props)
            (plist-get (cdr node-and-props) :name))
          (verilog-ts-nodes-props "module_declaration")))

(defun verilog-ts-module-instances-nodes (module-node)
  "Return instance nodes of MODULE-NODE."
  (unless (and module-node (string= "module_declaration" (treesit-node-type module-node)))
    (error "Wrong module-node: %s" module-node))
  (verilog-ts-nodes verilog-ts-instance-re module-node))

(defun verilog-ts-module-instances (module-node)
  "Return instances of MODULE-NODE."
  (unless (and module-node (string= "module_declaration" (treesit-node-type module-node)))
    (error "Wrong module-node: %s" module-node))
  (mapcar (lambda (node-and-props)
            (plist-get (cdr node-and-props) :name))
          (verilog-ts-nodes-props verilog-ts-instance-re module-node)))

(defun verilog-ts-module-always-blocks (module-node)
  "Return always blocks of MODULE-NODE."
  (unless (and module-node (string= "module_declaration" (treesit-node-type module-node)))
    (error "Wrong module-node: %s" module-node))
  (mapcar (lambda (node-and-props)
            (plist-get (cdr node-and-props) :name))
          (verilog-ts-nodes-props "always_keyword" module-node)))

(defun verilog-ts-class-properties (class-node)
  "Return properties of CLASS-NODE."
  (unless (and class-node (string= "class_declaration" (treesit-node-type class-node)))
    (error "Wrong class-node: %s" class-node))
  (mapcar (lambda (node-and-props)
            (plist-get (cdr node-and-props) :name))
          (verilog-ts-nodes-props "class_property" class-node)))

(defun verilog-ts-class-methods (class-node)
  "Return methods of CLASS-NODE."
  (unless (and class-node (string= "class_declaration" (treesit-node-type class-node)))
    (error "Wrong class-node: %s" class-node))
  (mapcar (lambda (node-and-props)
            (plist-get (cdr node-and-props) :name))
          (verilog-ts-nodes-props "class_\\(constructor\\|method\\)" class-node)))

(defun verilog-ts-class-constraints (class-node)
  "Return constraints of CLASS-NODE."
  (unless (and class-node (string= "class_declaration" (treesit-node-type class-node)))
    (error "Wrong class-node: %s" class-node))
  (mapcar (lambda (node-and-props)
            (plist-get (cdr node-and-props) :name))
          (verilog-ts-nodes-props "constraint_declaration" class-node)))


;;; Font-lock
;;;; Faces
(defgroup verilog-ts-faces nil
  "Verilog-ts faces."
  :group 'verilog-ts)

(defvar verilog-ts-font-lock-grouping-keywords-face 'verilog-ts-font-lock-grouping-keywords-face)
(defface verilog-ts-font-lock-grouping-keywords-face
  '((t (:inherit font-lock-misc-punctuation-face)))
  "Face for grouping keywords: begin, end."
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-punctuation-face 'verilog-ts-font-lock-punctuation-face)
(defface verilog-ts-font-lock-punctuation-face
  '((t (:inherit font-lock-punctuation-face)))
  "Face for punctuation symbols, e.g:
!,;:?'=<>*"
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-operator-face 'verilog-ts-font-lock-operator-face)
(defface verilog-ts-font-lock-operator-face
  '((t (:inherit font-lock-operator-face)))
  "Face for operator symbols, such as &^~+-/|."
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-brackets-face 'verilog-ts-font-lock-brackets-face)
(defface verilog-ts-font-lock-brackets-face
  '((t (:inherit font-lock-bracket-face)))
  "Face for brackets []."
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-parenthesis-face 'verilog-ts-font-lock-parenthesis-face)
(defface verilog-ts-font-lock-parenthesis-face
  '((t (:inherit font-lock-bracket-face)))
  "Face for parenthesis ()."
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-curly-braces-face 'verilog-ts-font-lock-curly-braces-face)
(defface verilog-ts-font-lock-curly-braces-face
  '((t (:inherit font-lock-bracket-face)))
  "Face for curly braces {}."
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-port-connection-face 'verilog-ts-font-lock-port-connection-face)
(defface verilog-ts-font-lock-port-connection-face
  '((t (:inherit font-lock-constant-face)))
  "Face for port connections of instances.
.portA (signalA),
.portB (signalB)
);"
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-dot-name-face 'verilog-ts-font-lock-dot-name-face)
(defface verilog-ts-font-lock-dot-name-face
  '((t (:inherit font-lock-property-name-face)))
  "Face for dot-name regexps:
- Interface signals, classes attributes/methods and hierarchical refs.

axi_if.Ready <= 1'b1;
obj.method();"
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-brackets-content-face 'verilog-ts-font-lock-brackets-content-face)
(defface verilog-ts-font-lock-brackets-content-face
  '((t (:inherit font-lock-number-face)))
  "Face for content between brackets: arrays, bit vector width and indexing."
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-width-num-face 'verilog-ts-font-lock-width-num-face)
(defface verilog-ts-font-lock-width-num-face
  '((t (:inherit font-lock-number-face)))
  "Face for the bit width number expressions.
{1}'b1,
{4}'hF,
{3}'o7,"
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-width-type-face 'verilog-ts-font-lock-width-type-face)
(defface verilog-ts-font-lock-width-type-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for the bit width type expressions.
1'{b}1,
4'{h}F,
3'{o}7,"
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-module-face 'verilog-ts-font-lock-module-face)
(defface verilog-ts-font-lock-module-face
  '((t (:inherit font-lock-function-call-face)))
  "Face for module names."
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-instance-face 'verilog-ts-font-lock-instance-face)
(defface verilog-ts-font-lock-instance-face
  '((t (:inherit font-lock-variable-use-face)))
  "Face for instance names."
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-time-event-face 'verilog-ts-font-lock-time-event-face)
(defface verilog-ts-font-lock-time-event-face
  '((t (:inherit font-lock-property-name-face)))
  "Face for time-events: @ and #."
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-time-unit-face 'verilog-ts-font-lock-time-unit-face)
(defface verilog-ts-font-lock-time-unit-face
  '((t (:inherit font-lock-property-use-face)))
  "Face for time-units: ms, us, ns, ps, fs (delays and timescale/timeprecision)."
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-preprocessor-face 'verilog-ts-font-lock-preprocessor-face)
(defface verilog-ts-font-lock-preprocessor-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for preprocessor compiler directives (`include, `define, UVM macros...)."
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-modport-face 'verilog-ts-font-lock-modport-face)
(defface verilog-ts-font-lock-modport-face
  '((t (:inherit font-lock-type-face)))
  "Face for interface modports."
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-direction-face 'verilog-ts-font-lock-direction-face)
(defface verilog-ts-font-lock-direction-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for direction of ports/functions/tasks args."
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-translate-off-face 'verilog-ts-font-lock-translate-off-face)
(defface verilog-ts-font-lock-translate-off-face
  '((t (:slant italic)))
  "Face for pragmas between comments, e.g:
* translate_off / * translate_on"
  :group 'verilog-ts-faces)

(defvar verilog-ts-font-lock-attribute-face 'verilog-ts-font-lock-attribute-face)
(defface verilog-ts-font-lock-attribute-face
  '((t (:inherit font-lock-property-name-face)))
  "Face for RTL attributes."
  :group 'verilog-ts-faces)


;;;; Keywords
;; There are some keywords that are not recognized by tree-sitter grammar.
;; For these ones, use regexp matching patterns inside tree-sitter (:match "^foo$")
(defconst verilog-ts-keywords
  '("accept_on" "alias" "and" "assert" "assign" "assume" "before" "bind"
    "binsof" "break" "case" "checker" "class" "class" "clocking" "config" "const"
    "constraint" "cover" "covergroup" "coverpoint" "cross" "default" "defparam"
    "disable" "do" "else" "endcase" "endchecker" "endclass" "endclocking"
    "endconfig" "endfunction" "endgenerate" "endgroup" "endinterface" "endmodule"
    "endpackage" "endprogram" "endproperty" "endsequence" "endtask" "enum"
    "eventually" "export" "extends" "extern" "final" "first_match" "for" "force"
    "foreach" "forever" "fork" "forkjoin" "function" "generate" "genvar" "if" "iff"
    "illegal_bins" "implements" "implies" "import" "initial" "inside" "interconnect"
    "interface" "intersect" "join" "join_any" "join_none" "local" "localparam"
    "matches" "modport" "new" "nexttime" "null" "option" "or" "package" "packed"
    "parameter" "program" "property" "pure" "randcase" "randomize" "reject_on"
    "release" "repeat" "return" "s_always" "s_eventually" "s_nexttime" "s_until"
    "s_until_with" "sequence" "showcancelled" "soft" "solve" "struct" "super"
    "sync_accept_on" "sync_reject_on" "tagged" "task" "throughout" "timeprecision"
    "timeunit" "type" "typedef" "union" "unique" "until" "until_with" "virtual"
    "wait" "while" "with"
    (always_keyword)               ; always, always_comb, always_latch, always_ff
    (bins_keyword)                 ; bins, illegal_bins, ignore_bins
    (case_keyword)                 ; case, casez, casex
    (class_item_qualifier)         ; static, protected, local
    (dpi_function_import_property) ; dpi import
    (edge_identifier)              ; posedge, negedge, edge
    (lifetime)                     ; static, automatic
    (module_keyword)               ; module, macromodule
    (random_qualifier)             ; rand, randc
    (unique_priority)))            ; unique, unique0, priority

(defconst verilog-ts-operators-arithmetic
  '("+" "-" "*" "/" "%" "**"))

(defconst verilog-ts-operators-relational
  '("<" "<=" ">" ">="))

(defconst verilog-ts-operators-equality
  '("===" "!==" "==" "!="))

(defconst verilog-ts-operators-logical
  '("&&" "||" "!"))

(defconst verilog-ts-operators-bitwise
  '("~" "&" "~&" "|" "~|" "^" "~^"))

(defconst verilog-ts-operators-shift
  '("<<" ">>" "<<<" ">>>"))

(defconst verilog-ts-punctuation
  '(";" ":" "," "::"
    "=" "?" "|=" "&=" "^="
    "|->" "|=>" "->"
    ":=" ":/" "-:" "+:"))

(defconst verilog-ts-directives
  '("`include" "`define" "`ifdef" "`ifndef" "`timescale" "`default_nettype"
    "`elsif" "`undef" (resetall_compiler_directive) (undefineall_compiler_directive)
    "`endif" "`else" "`unconnected_drive" (celldefine_compiler_directive)
    (endcelldefine_compiler_directive) (endkeywords_directive) "`line"
    "`begin_keywords" "`pragma" "`__FILE__" "`__LINE__"))


;;;; Functions
(defun verilog-ts--fontify-error (node _override start end &rest _)
  "Fontify a syntax error with a red wavy underline.

For NODE,OVERRIDE, START, END, and ARGS, see `treesit-font-lock-rules'."
  (when verilog-ts-linter-enable
    (treesit-fontify-with-override (treesit-node-start node)
                                   (treesit-node-end node)
                                   '(:underline (:style wave :color "Red1"))
                                   'append
                                   start end)))

;;;; Treesit-settings
(defvar verilog-ts--font-lock-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'verilog
   '(((one_line_comment) @font-lock-comment-face)
     ((block_comment) @font-lock-comment-face))

   :feature 'string
   :language 'verilog
   '([(string_literal)
      (quoted_string) ; `include strings
      (system_lib_string)
      (dpi_spec_string)]
     @font-lock-string-face)

   :feature 'keyword
   :language 'verilog
   `((["begin" "end" "this"] @verilog-ts-font-lock-grouping-keywords-face)
     (["input" "output" "inout" "ref"] @verilog-ts-font-lock-direction-face)
     ([,@verilog-ts-keywords] @font-lock-keyword-face))

   :feature 'preprocessor
   :language 'verilog
   `(([,@verilog-ts-directives] @verilog-ts-font-lock-preprocessor-face)
     (text_macro_usage
      (simple_identifier) @verilog-ts-font-lock-preprocessor-face))

   :feature 'punctuation
   :language 'verilog
   `(([,@verilog-ts-punctuation] @verilog-ts-font-lock-punctuation-face)
     (["."] @verilog-ts-font-lock-operator-face)
     (["(" ")"] @verilog-ts-font-lock-parenthesis-face)
     (["[" "]"] @verilog-ts-font-lock-brackets-face)
     (["{" "}" "'{"] @verilog-ts-font-lock-curly-braces-face)
     (["@" "#" "##"] @verilog-ts-font-lock-time-event-face)
     (["[*" "[=" "[->"] @verilog-ts-font-lock-brackets-face)) ; SVA repetition


   :feature 'operator
   :language 'verilog
   `(;; INFO: Some of these might be redundant
     ([,@verilog-ts-operators-arithmetic] @verilog-ts-font-lock-operator-face)
     ([,@verilog-ts-operators-relational] @verilog-ts-font-lock-punctuation-face)
     ([,@verilog-ts-operators-equality] @verilog-ts-font-lock-punctuation-face)
     ([,@verilog-ts-operators-shift] @verilog-ts-font-lock-punctuation-face)
     ([,@verilog-ts-operators-bitwise] @verilog-ts-font-lock-operator-face)
     ([,@verilog-ts-operators-logical] @verilog-ts-font-lock-operator-face)
     ;; Operators (A.8.6):
     ((assignment_operator) @verilog-ts-font-lock-punctuation-face)
     ((unary_operator) @verilog-ts-font-lock-operator-face)
     ;; ((binary_operator) @verilog-ts-font-lock-operator-face) ; INFO: Unused in the grammar, left as a reference
     ((inc_or_dec_operator) @verilog-ts-font-lock-operator-face)
     ((stream_operator) @verilog-ts-font-lock-operator-face)
     ((event_trigger (["->" "->>"]) @verilog-ts-font-lock-operator-face)))

   :feature 'declaration
   :language 'verilog
   '(;; Module/interface/program/package/class/checker
     (module_nonansi_header
      name: (simple_identifier) @font-lock-function-name-face)
     (module_ansi_header
      name: (simple_identifier) @font-lock-function-name-face)
     (interface_nonansi_header
      name: (simple_identifier) @font-lock-function-name-face)
     (interface_ansi_header
      name: (simple_identifier) @font-lock-function-name-face)
     (program_nonansi_header
      name: (simple_identifier) @font-lock-function-name-face)
     (program_ansi_header
      name: (simple_identifier) @font-lock-function-name-face)
     (package_declaration
      name: (simple_identifier) @font-lock-function-name-face)
     (class_declaration
      name: (simple_identifier) @font-lock-function-name-face)
     (interface_class_declaration
      name: (simple_identifier) @font-lock-function-name-face)
     (checker_declaration
      name: (simple_identifier) @font-lock-function-name-face)
     (class_declaration
      (class_type
       (simple_identifier) @font-lock-type-face)) ; Parent class
     ;; Function/task/methods
     (function_body_declaration
      name: (simple_identifier) @font-lock-function-name-face)
     (task_body_declaration
      name: (simple_identifier) @font-lock-function-name-face)
     (function_prototype
      (data_type_or_void)
      name: (simple_identifier) @font-lock-function-name-face)
     (task_prototype
      name: (simple_identifier) @font-lock-function-name-face)
     (class_scope ; Definition of extern defined methods
      (class_type
       (simple_identifier) @verilog-ts-font-lock-dot-name-face)))

   :feature 'type
   :language 'verilog
   `(([(integer_vector_type) ; bit, logic, reg
       (integer_atom_type)   ; byte, shortint, int, longint, integer, time
       (non_integer_type)    ; shortreal, real, realtime
       (net_type)            ; supply0, supply1, tri, triand, trior, trireg, tri0, tri1, uwire, wire, wand, wor
       ["string" "event" "signed" "unsigned" "chandle"]]
      @font-lock-type-face)
     (data_type_or_implicit
      (data_type
       (simple_identifier) @font-lock-type-face))
     (data_type
      (class_type
       (simple_identifier) @font-lock-type-face
       (parameter_value_assignment)))
     (data_type
      (class_type
       (simple_identifier) @verilog-ts-font-lock-dot-name-face
       (simple_identifier) @font-lock-type-face))
     (net_port_header ; port with user net type
      (net_port_type
       (simple_identifier) @font-lock-type-face))
     (variable_port_header ; port with user variable type
      (variable_port_type
       (data_type
        (simple_identifier) @font-lock-type-face)))
     (["void'" (data_type_or_void)] @font-lock-type-face) ; void cast of task called as a function
     (interface_port_header ; Interfaces with modports
      interface_name: (simple_identifier) @verilog-ts-font-lock-dot-name-face
      modport_name: (simple_identifier) @verilog-ts-font-lock-modport-face)
     (type_assignment
      name: (simple_identifier) @font-lock-type-face)
     (net_declaration ; User type variable declaration
      (simple_identifier) @font-lock-type-face)
     (enum_base_type ; Enum base type with user type
      (simple_identifier) @font-lock-type-face))

   :feature 'instance
   :language 'verilog
   '(;; Module names
     (module_instantiation
      instance_type: (simple_identifier) @verilog-ts-font-lock-module-face)
     (interface_instantiation
      instance_type: (simple_identifier) @verilog-ts-font-lock-module-face)
     (program_instantiation
      instance_type: (simple_identifier) @verilog-ts-font-lock-module-face)
     (checker_instantiation
      instance_type: (simple_identifier) @verilog-ts-font-lock-module-face)
     (udp_instantiation
      instance_type: (simple_identifier) @verilog-ts-font-lock-module-face)
     (gate_instantiation
      [(cmos_switchtype)
       (mos_switchtype)
       (enable_gatetype)
       (n_input_gatetype)
       (n_output_gatetype)
       (pass_en_switchtype)
       (pass_switchtype)
       "pulldown" "pullup"]
      @verilog-ts-font-lock-module-face)
     ;; Instance names
     (name_of_instance
      instance_name: (simple_identifier) @verilog-ts-font-lock-instance-face)
     ;; Instance parameters
     (module_instantiation ; Assume module_instantiation has higher dynamic precedence than other instantiations
      (parameter_value_assignment
       (list_of_parameter_value_assignments
        (named_parameter_assignment
         (simple_identifier) @verilog-ts-font-lock-port-connection-face))))
     (module_instantiation ; Assume module_instantiation has higher dynamic precedence than other instantiations
      (parameter_value_assignment
       (list_of_parameter_value_assignments
        (ordered_parameter_assignment
         (param_expression
          (data_type
           (simple_identifier) @verilog-ts-font-lock-port-connection-face))))))
     ;; Port names
     (named_port_connection
      port_name: (simple_identifier) @verilog-ts-font-lock-port-connection-face)
     (named_parameter_assignment
      (simple_identifier) @verilog-ts-font-lock-port-connection-face)
     (named_checker_port_connection
      port_name: (simple_identifier) @verilog-ts-font-lock-port-connection-face)
     ;; Bind statements
     (bind_directive
      (bind_target_scope
       (simple_identifier) @font-lock-type-face)))

   :feature 'number
   :language 'verilog
   '((hex_number
      size: (unsigned_number) @verilog-ts-font-lock-width-num-face
      base: (hex_base) @verilog-ts-font-lock-width-type-face)
     (decimal_number
      size: (unsigned_number) @verilog-ts-font-lock-width-num-face
      base: (decimal_base) @verilog-ts-font-lock-width-type-face)
     (octal_number
      size: (unsigned_number) @verilog-ts-font-lock-width-num-face
      base: (octal_base) @verilog-ts-font-lock-width-type-face)
     (binary_number
      size: (unsigned_number) @verilog-ts-font-lock-width-num-face
      base: (binary_base) @verilog-ts-font-lock-width-type-face)
     ;; Same as before but without the width (width extension)
     (hex_number
      base: (hex_base) @verilog-ts-font-lock-width-type-face)
     (decimal_number
      base: (decimal_base) @verilog-ts-font-lock-width-type-face)
     (octal_number
      base: (octal_base) @verilog-ts-font-lock-width-type-face)
     (binary_number
      base: (binary_base) @verilog-ts-font-lock-width-type-face))

   :feature 'array
   :language 'verilog
   :override t
   '((unpacked_dimension
      [(constant_expression) (constant_range)] @verilog-ts-font-lock-brackets-content-face)
     (packed_dimension
      (constant_range) @verilog-ts-font-lock-brackets-content-face)
     (select
      (constant_range) @verilog-ts-font-lock-brackets-content-face)
     (constant_select
      (constant_range
       (constant_expression) @verilog-ts-font-lock-brackets-content-face))
     (constant_bit_select
      (constant_expression) @verilog-ts-font-lock-brackets-content-face)
     (bit_select
      (expression) @verilog-ts-font-lock-brackets-content-face)
     (indexed_range
      (expression) @verilog-ts-font-lock-brackets-content-face
      (constant_expression) @verilog-ts-font-lock-brackets-content-face)
     (constant_indexed_range
      (constant_expression) @verilog-ts-font-lock-brackets-content-face)
     (value_range ; inside {[min_range:max_range]}, place here to apply override
      (expression) @font-lock-constant-face)
     (cycle_delay_const_range_expression) @verilog-ts-font-lock-brackets-content-face
     (dynamic_array_new
      (expression) @font-lock-constant-face))

   :feature 'misc
   :language 'verilog
   '(;; Timeunit
     ((time_unit) @font-lock-constant-face)
     ;; Enum labels
     (enum_name_declaration
      (simple_identifier) @font-lock-constant-face)
     ;; Case item label (not radix)
     (case_item_expression
      (expression
       (primary
        (hierarchical_identifier
         (simple_identifier) @font-lock-constant-face))))
     ;; Hierarchical references, interface signals, class members, package scope
     (hierarchical_identifier
      (simple_identifier) @verilog-ts-font-lock-dot-name-face
      "."
      (simple_identifier))
     (method_call
      (primary) @verilog-ts-font-lock-dot-name-face
      (["." "::"])
      (method_call_body))
     (package_scope
      (simple_identifier) @verilog-ts-font-lock-dot-name-face)
     (method_call
      (primary
       (select
        (simple_identifier) @verilog-ts-font-lock-dot-name-face))
      (method_call_body))
     ;; Attributes
     (["(*" "*)"] @font-lock-constant-face)
     (attribute_instance
      (attr_spec (simple_identifier) @verilog-ts-font-lock-attribute-face))
     ;; Typedefs
     (type_declaration
      (class_type (simple_identifier) @font-lock-type-face)
      type_name: (simple_identifier) @font-lock-constant-face)
     (type_declaration
      type_name: (simple_identifier) @font-lock-constant-face)
     ("typedef" "class" (simple_identifier) @font-lock-constant-face)
     ;; Coverpoint & cross labels
     (cover_point
      name: (simple_identifier) @font-lock-constant-face)
     (cover_cross
      name: (simple_identifier) @font-lock-constant-face)
     ;; Loop variables (foreach[i])
     (loop_variables
      (simple_identifier) @font-lock-constant-face)
     ;; Bins values
     (bins_or_options
      (expression
       (primary
        (concatenation
         (expression) @font-lock-constant-face))))
     ;; Bins ranges
     (covergroup_value_range
      (expression) @font-lock-constant-face)
     ;; Queue dimension
     (("$") @font-lock-constant-face)
     ;; Parameterized classes (e.g: uvm_config_db #(axi_stream_agent_config))
     (class_type
      (parameter_value_assignment
       (list_of_parameter_value_assignments) @verilog-ts-font-lock-dot-name-face)))

   :feature 'system-tf
   :language 'verilog
   :override t ; Highlight system_tf calls inside array ranges
   '([(system_tf_identifier)               ; System task/function
      "$fatal" "$error" "$warning" "$info" ; (severity_system_task)
      "$stop" "$finish" "$exit"]           ; (simulation_control_task)
     @font-lock-builtin-face)

   :feature 'error
   :language 'verilog
   :override t
   '((ERROR) @verilog-ts--fontify-error)))


;;; Indent
;;;; Matchers
(defun verilog-ts--matcher-unit-scope (node parent &rest _)
  "A tree-sitter simple indent matcher for NODE and PARENT.
Matches if point is at $unit scope."
  (let* ((root (treesit-buffer-root-node)))
    (or (treesit-node-eq node root)
        (treesit-node-eq parent root))))

(defun verilog-ts--matcher-blank-line (node &rest _)
  "A tree-sitter simple indent matcher for NODE.
Matches if point is at a blank line."
  (unless node
    t))

(defun verilog-ts--matcher-continued-string (&rest _)
  "A tree-sitter simple indent matcher for NODE.
Matches if point is at a continued quoted string."
  (equal (treesit-node-type (treesit-node-parent (verilog-ts--node-at-point))) "quoted_string"))

(defun verilog-ts--matcher-uvm-field-macro (&rest _)
  "A tree-sitter simple indent matcher.
Matches if point is at a uvm_field_* macro.
Snippet fetched from `treesit--indent-1'."
  (let* ((bol (save-excursion
                (forward-line 0)
                (skip-chars-forward " \t")
                (point)))
         (leaf-node (treesit-node-at bol))
         (node (verilog-ts--node-has-parent-recursive leaf-node "text_macro_usage"))
         (node-text (when node
                      (treesit-node-text node :no-props))))
    (when (and node-text
               (eq 0 (string-match "`[ou]vm_field_" node-text)))
      node-text)))

(defun verilog-ts--matcher-default-indent (&rest _)
  "A tree-sitter simple indent matcher.
Always return non-nil."
  t)

(defun verilog-ts--matcher-ansi-port-after-paren (node &rest _)
  "A tree-sitter simple indent matcher for NODE.
Return non-nil if the first ansi-port is in the same line as the opening
parenthesis."
  (let* ((indent-node (verilog-ts--node-has-parent-recursive node "list_of_port_declarations"))
         (indent-node-line (line-number-at-pos (treesit-node-start indent-node)))
         (first-port-node (treesit-node-child indent-node 1)) ; ansi_port_declaration
         (first-port-node-line (line-number-at-pos (treesit-node-start first-port-node))))
    (eq indent-node-line first-port-node-line)))

(defun verilog-ts--matcher-parameter-port-after-paren (node &rest _)
  "A tree-sitter simple indent matcher for NODE.
Return non-nil if the first parameter is in the same line as the opening
parenthesis."
  (let* ((indent-node (verilog-ts--node-has-parent-recursive node "parameter_port_list"))
         (indent-node-line (line-number-at-pos (treesit-node-start indent-node)))
         (first-port-node (treesit-node-child indent-node 2)) ; parameter_port_declaration
         (first-port-node-line (line-number-at-pos (treesit-node-start first-port-node))))
    (eq indent-node-line first-port-node-line)))

(defun verilog-ts--matcher-continued-parameter-port (node &rest _)
  "A tree-sitter simple indent matcher for NODE.
Return non-nil if matches continued declaration of parameter ports.
parameter A = 0,
          B = 1,
          C = 2"
  (let ((child-node (treesit-node-child node 0)))
    (string= (treesit-node-type child-node) "data_type")))

(defun verilog-ts--matcher-unpacked-array-same-line (_node parent &rest _)
  "A tree-sitter simple indent matcher for PARENT.
Return non-nil if the first unpacked array element is in the same line as the
opening brace."
  (let* ((indent-node-line (line-number-at-pos (treesit-node-start parent)))
         (first-elm-node (treesit-node-child parent 1))
         (first-elm-node-line (line-number-at-pos (treesit-node-start first-elm-node))))
    (eq indent-node-line first-elm-node-line)))

(defun verilog-ts--matcher-import-package-ansi-header (node parent &rest _)
  "A tree-sitter simple indent matcher for NODE and PARENT.
Return non-nil if matches import package on ANSI header:
   module foo
       import bar_pkg::*;
       import baz_pkg::*;
           (
       input wire clk,
       input wire rst
   );"
  (let ((node-type (treesit-node-type node))
        (child-node-type (treesit-node-type (treesit-node-child node 0)))
        (parent-node-type (treesit-node-type parent)))
    (or (and (string= node-type "module_ansi_header") ; First import declaration
             (string= child-node-type "package_import_declaration"))
        (and (string= node-type "package_import_declaration") ; Subsequent import declarations
             (string= parent-node-type "module_ansi_header")))))

(defun verilog-ts--matcher-expression-args (node &rest _)
  "A tree-sitter simple indent matcher for NODE.
Return non-nil if matches continued declaration of list of arguments.

    ret_value = funcall(a, b, c,
                        d, e, f);"
  (when (and node (string-match "expression" (treesit-node-type node)))
    (verilog-ts--node-has-parent-recursive node "\\(list_of\\(_actual\\)?_arguments\\|cond_predicate\\|concatenation\\)")))

(defun verilog-ts--matcher-expression-decl (node &rest _)
  "A tree-sitter simple indent matcher for NODE.
Return non-nil if matches continued declaration of list of net or variable
declaration:

    wire [1:0] mux_output0 =
                   select0[0] ? mux_input0 :
                   select0[1] ? mux_input1 :
                   mux_input2;"
  (when (and node (string-match "expression" (treesit-node-type node)))
    (verilog-ts--node-has-parent-recursive node "list_of_\\(net\\|variable\\)_decl_assignments")))

(defun verilog-ts--matcher-expression-loop (node &rest _)
  "A tree-sitter simple indent matcher for NODE.
Return non-nil if matches continued expression of a loop statement condition.

    while (a && b &&
           !c) begin"
  (when (and node (string-match "expression" (treesit-node-type node)))
    (let ((loop-node (verilog-ts--node-has-parent-recursive node "loop_statement")))
      (and loop-node
           (string= (treesit-node-type (treesit-node-child loop-node 2)) "expression"))))) ; 2 accounts for while (

(defun verilog-ts--matcher-expression-assignment (node &rest _)
  "A tree-sitter simple indent matcher for NODE.
Return non-nil if matches continued expression of a loop statement condition.

    valid_c <= (valid &&
                (state != A) &&
                (state != B));"
  (and node ; prevent matching blank lines and continued quoted strings
       (string-match "expression" (treesit-node-type node)) ; also match mintypmax and constant expressions
       (verilog-ts--node-has-parent-recursive node "\\(continuous_assign\\|\\(non\\)?blocking_assignment\\)")
       (not (verilog-ts--node-has-parent-recursive node "constraint_block")))) ; Prevent matching in randomize with { blocking assignments


;;;; Anchors
(defun verilog-ts--anchor-end-indent (node parent &rest _)
  "A tree-sitter simple indent anchor for NODE and PARENT.
Handle indentation of block end keywords."
  (save-excursion
    (pcase (treesit-node-text node :no-props)
      ("endtask"
       (goto-char (treesit-node-start (or (verilog-ts--node-has-parent-recursive node "class_method") ; First check if it's a class method to align to modifiers ...
                                          (verilog-ts--node-has-parent-recursive node "task_declaration"))))) ; ... e.g: protected/static/etc...
      ("endfunction"
       (goto-char (treesit-node-start (or (verilog-ts--node-has-parent-recursive node "class_method")
                                          (verilog-ts--node-has-parent-recursive node "\\(function\\|class_constructor\\)_declaration")))))
      ;; default is parent-bol 0
      (_
       (goto-char (treesit-node-start parent))
       (back-to-indentation)
       (point)))))

(defun verilog-ts--anchor-expression-args (node parent &rest _)
  "A tree-sitter simple indent anchor for NODE and PARENT."
  (let (indent-node)
    (save-excursion
      (cond ((setq indent-node (verilog-ts--node-has-parent-recursive node "\\(list_of\\(_actual\\)?_arguments\\|cond_predicate\\)"))
             (goto-char (treesit-node-start indent-node))
             (point))
            ((setq indent-node (verilog-ts--node-has-parent-recursive node "concatenation"))
             (goto-char (treesit-node-start (treesit-search-subtree indent-node "expression")))
             (point))
            (t
             (goto-char (treesit-node-start parent))
             (back-to-indentation)
             (point))))))

(defun verilog-ts--anchor-expression-decl (node &rest _)
  "A tree-sitter simple indent anchor for NODE and PARENT."
  (let (indent-node)
    (save-excursion
      (cond ((setq indent-node (verilog-ts--node-has-parent-recursive node "assignment_pattern"))
             (goto-char (treesit-node-start (treesit-search-subtree indent-node "constant_expression")))
             (point))
            (t
             (setq indent-node (verilog-ts--node-has-parent-recursive node "list_of_\\(net\\|variable\\)_decl_assignments"))
             (goto-char (treesit-node-start indent-node))
             (point))))))

(defun verilog-ts--anchor-expression-loop (node &rest _)
  "A tree-sitter simple indent anchor for NODE and PARENT."
  (let* ((loop-node (verilog-ts--node-has-parent-recursive node "loop_statement"))
         (expr-node (treesit-search-subtree loop-node "expression")))
    (save-excursion
      (goto-char (treesit-node-start expr-node))
      (point))))

(defun verilog-ts--anchor-expression-assignment (node parent &rest _)
  "A tree-sitter simple indent anchor for NODE and PARENT."
  (let* ((temp-node (treesit-parent-until
                     node
                     (lambda (node) ; Third argument must be a function
                       (not (string= (treesit-node-type node) "expression")))))
         (first-node (treesit-search-subtree temp-node "\\(expression\\|primary\\)")))
    (cond ((eq (treesit-node-start node) (treesit-node-start first-node))
           (save-excursion
             (goto-char (treesit-node-start parent))
             (forward-char verilog-ts-indent-level) ; DANGER: This didn't seem a good option!, but it's for a corner case so leave it here (when right after = there is a blank space)
             (point)))
          (t
           (save-excursion
             (goto-char (treesit-node-start first-node))
             (point))))))

(defun verilog-ts--anchor-grandparent-bol (_node parent &rest _)
  "A tree-sitter simple indent anchor for NODE and PARENT.
Find the beginning of line of node's grandparent.
INFO: Might be present in future Emacs releases.
Check `treesit' and `treesit-simple-indent-presets'."
  (save-excursion
    (goto-char (treesit-node-start (treesit-node-parent parent)))
    (back-to-indentation)
    (point)))

(defun verilog-ts--anchor-tf-port-list (node &rest _)
  "A tree-sitter simple indent anchor for NODE.
Indent task/function arguments."
  (let ((indent-node (or (verilog-ts--node-has-parent-recursive node "class_method") ; First check if it's a class method to align to modifiers ...
                         (verilog-ts--node-has-parent-recursive node "\\(task\\|function\\)_declaration"))))
    (save-excursion
      (if indent-node
          (goto-char (treesit-node-start indent-node))
        (point)))))

(defun verilog-ts--anchor-tf-port-item (node &rest _)
  "A tree-sitter simple indent anchor for NODE.
Indent task/function arguments."
  (let ((indent-node (verilog-ts--node-has-parent-recursive node "\\(tf_port\\|class_constructor_arg\\)_list")))
    (save-excursion
      (if indent-node
          (goto-char (treesit-node-start indent-node))
        (point)))))

(defun verilog-ts--anchor-first-ansi-port (node &rest _)
  "A tree-sitter simple indent anchor for NODE.
Indent ansi_ports depending on first port:

 - module foo (input a
    -> Will indent the rest of the ports right below the first one.

 - module foo (
     input a
    -> Will indent the rest of the ports with respect to parent-bol (module)."
  (let ((indent-node (verilog-ts--node-has-parent-recursive node "list_of_port_declarations")))
    (save-excursion
      (goto-char (treesit-node-start indent-node))
      (skip-chars-forward "( \t")
      (point))))

(defun verilog-ts--anchor-ansi-port (node &rest _)
  "A tree-sitter simple indent anchor for NODE.
Indent ansi_ports according to module definition."
  (let ((indent-node (or (verilog-ts--node-has-parent-recursive node "module_declaration")
                         (verilog-ts--node-has-parent-recursive node "interface_declaration"))))
    (when indent-node
      (save-excursion
        (goto-char (treesit-node-start indent-node))
        (point)))))

(defun verilog-ts--anchor-coverpoint-bins (node &rest _)
  "A tree-sitter simple indent anchor for NODE.
Indent bins with respect to label of coverpoint."
  (let ((indent-node (verilog-ts--node-has-parent-recursive node "cover_point")))
    (save-excursion
      (goto-char (treesit-node-start indent-node)))))

(defun verilog-ts--anchor-cross-bins (node &rest _)
  "A tree-sitter simple indent anchor for NODE.
Indent cross bins with respect to label of coverpoint."
  (let ((indent-node (verilog-ts--node-has-parent-recursive node "cover_cross")))
    (save-excursion
      (goto-char (treesit-node-start indent-node)))))

(defun verilog-ts--anchor-continued-parameter (node &rest _)
  "A tree-sitter simple indent anchor for NODE.
Indent continued line parameters in port declarations."
  (let* ((param-decl-node (treesit-search-forward node
                                                  (lambda (node)
                                                    (string= (treesit-node-type node) "parameter_declaration"))
                                                  :backward))
         (param-decl-start-node (treesit-search-subtree param-decl-node
                                                        (lambda (node)
                                                          (string= (treesit-node-type node) "list_of_param_assignments"))))
         (param-decl-start-node (treesit-node-start param-decl-start-node)))
    (when param-decl-start-node
      (save-excursion
        (goto-char param-decl-start-node)))))

(defun verilog-ts--anchor-parameter-port (node &rest _)
  "A tree-sitter simple indent anchor for NODE and PARENT.
Indent parameters depending on first parameter:
 - module foo # (parameter int a = 0
    -> Will indent the rest of the ports right below the first one.
 - module foo #(
     parameter int a = 0,
    -> Will indent the rest of the ports with respect to parent-bol (module)."
  (let ((indent-node (treesit-search-subtree (verilog-ts--node-has-parent-recursive node "parameter_port_list")
                                             (lambda (node)
                                               (string= (treesit-node-type node) "parameter_port_declaration")))))
    (save-excursion
      (goto-char (treesit-node-start indent-node))
      (skip-chars-forward "( \t")
      (point))))

(defun verilog-ts--anchor-import-package-ansi-header (node &rest _)
  "A tree-sitter simple indent anchor for NODE and PARENT.
Indent package imports on ANSI headers, used in conjunction with
`verilog-ts--matcher-import-package-ansi-header'."
  (let ((indent-node (verilog-ts--node-has-parent-recursive node "module_declaration")))
    (save-excursion
      (goto-char (treesit-node-start indent-node)))))

(defun verilog-ts--anchor-point-min (&rest _)
  "A tree-sitter simple indent anchor."
  (save-excursion
    (point-min)))

(defun verilog-ts--anchor-continued-string (&rest _)
  "A tree-sitter simple indent anchor."
  (let ((indent-node (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) "quoted_string")))
    (save-excursion
      (goto-char (treesit-node-start indent-node))
      (1+ (point))))) ; Take " into account

(defun verilog-ts--anchor-assignment-pattern (_node parent &rest _)
  "A tree-sitter simple indent matcher for PARENT.
- int a = '{0, 1, 2,
    -> Will indent the rest of the elements right below the first one.
- int a = '{
    -> Will indent the rest of the elements with respect to parent-bol
       (assignment_pattern)."
  (let ((indent-node (treesit-node-child parent 1)))
    (save-excursion
      (goto-char (treesit-node-start indent-node))
      (point))))


;;;; Rules
(defconst verilog-ts--indent-procedural
  (eval-when-compile
    (regexp-opt
     '("continuous_assign"
       "always_construct"
       "if_generate_construct"
       "loop_generate_construct"
       "initial_construct"
       "statement_or_null"
       "case_item"
       "block_item_declaration"               ; Procedural local variables declaration
       "tf_item_declaration"                  ; Procedural local variables in tasks declaration
       "function_statement_or_null"           ; Procedural statement in a function
       "checker_or_generate_item_declaration" ; default disable iff (!rst_ni);
       "concurrent_assertion_item"            ; default disable iff (!rst_ni);
       "super")
     'symbols)))

(defvar verilog-ts--treesit-indent-rules
  `((verilog
     ;; Unit scope
     (verilog-ts--matcher-unit-scope verilog-ts--anchor-point-min 0) ; Place first for highest precedence
     ;; Comments
     ((and (node-is "\\_<\\(block\\|one_line\\)_comment\\_>")
           verilog-ts--matcher-unit-scope)
      verilog-ts--anchor-point-min 0)
     ((and (node-is "\\_<\\(block\\|one_line\\)_comment\\_>")
           (parent-is "\\_<\\(conditional_statement\\|list_of_port_connections\\)\\_>"))
      parent-bol 0)
     ((and (node-is "\\_<\\(block\\|one_line\\)_comment\\_>")
           (parent-is "\\_<list_of_port_declarations\\_>"))
      verilog-ts--anchor-grandparent-bol verilog-ts-indent-level)
     ((and (node-is "\\_<\\(block\\|one_line\\)_comment\\_>")
           (parent-is "\\_<if_generate_construct\\_>"))
      parent-bol 0)
     ((node-is "\\_<\\(block\\|one_line\\)_comment\\_>") parent-bol verilog-ts-indent-level)
     ;; Procedural
     ((node-is ,verilog-ts--indent-procedural) parent-bol verilog-ts-indent-level)
     ;; ANSI Port/parameter declaration
     ((and (node-is "ansi_port_declaration")
           verilog-ts--matcher-ansi-port-after-paren)
      verilog-ts--anchor-first-ansi-port 0)
     ((node-is "ansi_port_declaration") verilog-ts--anchor-ansi-port verilog-ts-indent-level) ; Previous rule fallback
     ((and (node-is "parameter_port_declaration")
           verilog-ts--matcher-continued-parameter-port)
      verilog-ts--anchor-continued-parameter 0)
     ((and (node-is "parameter_port_declaration")
           verilog-ts--matcher-parameter-port-after-paren)
      verilog-ts--anchor-parameter-port 0)
     ((node-is "parameter_port_declaration") parent-bol verilog-ts-indent-level) ; First instance parameter (without parameter keyword)
     ((node-is "parameter_port_list") parent-bol 0) ; Open parenthesis in new line (old verilog-mode style)
     ((node-is "list_of_\\(port_declarations\\|param_assignments\\)") parent-bol verilog-ts-indent-level) ; Open parenthesis in newline/level, first instance parameter wo/ parameter keyword (old verilog-mode style)
     ((node-is "\\(module\\|interface\\)_or_generate_item") parent-bol verilog-ts-indent-level)
     ;; Import packages
     (verilog-ts--matcher-import-package-ansi-header verilog-ts--anchor-import-package-ansi-header verilog-ts-indent-level)
     ((node-is "package_import_declaration") parent-bol verilog-ts-indent-level)
     ((and (node-is "package_or_generate_item_declaration")
           (parent-is "package_declaration"))
      parent-bol verilog-ts-indent-level)
     ;; Instance port/parameters
     ((node-is "\\(named\\|ordered\\)_port_connection") parent-bol 0)
     ((node-is "\\(named\\|ordered\\)_parameter_assignment") parent-bol 0)
     ((and (node-is ".")
           (parent-is "named_port_connection")) ; E.g. ports with `ifdefs
      verilog-ts--anchor-grandparent-bol 0)
     ;; Enclosing
     ((node-is "end") verilog-ts--anchor-end-indent 0)
     ((node-is "\\(else\\|join_keyword\\|{\\|}\\|(\\|)\\|]\\)") parent-bol 0)
     ;; Macros
     ((and (node-is "class_item") ; Place before (node-is "class_item") to match with higher precedence
           verilog-ts--matcher-uvm-field-macro)
      parent-bol ,(* verilog-ts-indent-level 2))
     ;; Others
     ((node-is "class_item") parent-bol verilog-ts-indent-level)
     ((node-is "timeunits_declaration") parent-bol verilog-ts-indent-level)
     ((or (node-is "timeunit")
          (node-is "timeprecision"))
      parent 0)
     ((node-is "\\(class_constructor_arg\\|tf_port\\)_list") verilog-ts--anchor-tf-port-list verilog-ts-indent-level) ; Task ports in multiple lines (first line)
     ((node-is "\\(tf_port_item\\|class_constructor_arg\\)") verilog-ts--anchor-tf-port-item 0) ; Task ports in multiple lines
     ((node-is "\\(constraint_block_item\\|enum_name_declaration\\|generate_region\\|constraint_expression\\|dist_list\\)") parent-bol verilog-ts-indent-level)
     ((node-is "hierarchical_instance") parent-bol 0) ; Instance name in separate line
     ((node-is "bins_or_options") verilog-ts--anchor-coverpoint-bins verilog-ts-indent-level)
     ((node-is "cross_body_item") verilog-ts--anchor-cross-bins verilog-ts-indent-level)
     ((node-is "dist_item") verilog-ts--anchor-grandparent-bol verilog-ts-indent-level)
     ((and (parent-is "assignment_pattern")
           verilog-ts--matcher-unpacked-array-same-line)
      verilog-ts--anchor-assignment-pattern 0)
     ((parent-is "assignment_pattern") parent-bol verilog-ts-indent-level)
     ;; Continued lines
     (verilog-ts--matcher-expression-args verilog-ts--anchor-expression-args 0) ; expression 1
     (verilog-ts--matcher-expression-decl verilog-ts--anchor-expression-decl 0) ; expression 2
     (verilog-ts--matcher-expression-loop verilog-ts--anchor-expression-loop 0) ; expression 3
     (verilog-ts--matcher-expression-assignment verilog-ts--anchor-expression-assignment 0) ; expression 4
     ((node-is "expression") parent-bol verilog-ts-indent-level) ; expression generic
     ((node-is "constant_expression") parent-bol 0)
     ((node-is "module_ansi_header") parent-bol 0) ; Opening bracket of module ports/parmeters
     ((node-is "\\(param\\|variable_decl\\)_assignment") parent 0)
     ;; Arguments
     ((node-is "actual_argument") parent 0)
     ;; Blank lines and continued strings
     (verilog-ts--matcher-continued-string verilog-ts--anchor-continued-string 0)
     ((and (parent-is "list_of_\\(port_connections\\|parameter_value_assignments\\)")
           verilog-ts--matcher-blank-line)
      parent-bol 0)
     (verilog-ts--matcher-blank-line parent-bol verilog-ts-indent-level)
     ;; Default indent
     (verilog-ts--matcher-default-indent parent-bol verilog-ts-indent-level))))


;;; Imenu
(defconst verilog-ts-imenu-create-index-re
  (eval-when-compile
    (regexp-opt
     '("module_declaration"
       "interface_declaration"
       "program_declaration"
       "package_declaration"
       "class_declaration"
       "class_property"
       "function_declaration"
       "task_declaration"
       "class_constructor_declaration"
       "function_prototype"
       "task_prototype"
       "class_constructor_prototype"
       "module_instantiation"
       "interface_instantiation"
       "always_construct"
       "initial_construct"
       "final_construct"
       "generate_region")
     'symbols)))

(defvar verilog-ts-imenu-format-item-label-function
  'verilog-ts-imenu-format-item-label
  "Imenu function used to format an item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar verilog-ts-imenu-format-parent-item-label-function
  'verilog-ts-imenu-format-parent-item-label
  "Imenu function used to format a parent item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar verilog-ts-imenu-format-parent-item-jump-label-function
  'verilog-ts-imenu-format-parent-item-jump-label
  "Imenu function used to format a parent jump item label.
It must be a function with two arguments: TYPE and NAME.")

(defun verilog-ts-imenu-format-item-label (_type name)
  "Return Imenu label for single node using TYPE and NAME."
  (format "%s" name))

(defun verilog-ts-imenu-format-parent-item-label (_type name)
  "Return Imenu label for parent node using TYPE and NAME."
  (format "%s" name))

(defun verilog-ts-imenu-format-parent-item-jump-label (type _name)
  "Return Imenu label for parent node jump using TYPE and NAME."
  (format "*%s*" type))

(defun verilog-ts-imenu-treesit-create-index-tree (node)
  "Given a sparse tree, create an imenu alist.

NODE is the root node of the tree returned by
`treesit-induce-sparse-tree' (not a tree-sitter node, its car is
a tree-sitter node).  Walk that tree and return an imenu alist.

Return a list of ENTRY where

ENTRY := (NAME . MARKER)
       | (NAME . ((JUMP-LABEL . MARKER)
                  ENTRY
                  ...)

NAME is the function/class's name, JUMP-LABEL is like \"*function
definition*\".

Copied from Python's `python--imenu-treesit-create-index-1' and adapted to
SystemVerilog parser."
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'verilog-ts-imenu-treesit-create-index-tree
                           children))
         (type (treesit-node-type ts-node))
         ;; The root of the tree could have a nil ts-node.
         (name (when ts-node
                 (or (verilog-ts--node-identifier-name ts-node)
                     "Anonymous")))
         (pos (treesit-node-start ts-node))
         (marker (when ts-node
                   (if imenu-use-markers
                       (set-marker (make-marker) pos)
                     pos))))
    (cond
     ;; Root node
     ((null ts-node)
      subtrees)
     ;; Non-leaf node
     (subtrees
      (let ((parent-label (funcall verilog-ts-imenu-format-parent-item-label-function type name))
            (jump-label (funcall verilog-ts-imenu-format-parent-item-jump-label-function type name)))
        `((,parent-label
           ,(cons jump-label marker)
           ,@subtrees))))
     ;; Leaf node
     (t (let ((label (funcall verilog-ts-imenu-format-item-label-function type name)))
          (list (cons label marker)))))))

(defun verilog-ts--imenu-treesit-create-index-tree-group-process (subtree)
  "Utility function to process SUBTREE and group leaves into categories."
  (let (instances always initial tasks functions properties default)
    (mapc
     (lambda (elm)
       (if (and (listp elm) (listp (cdr elm)) (listp (cddr elm)) ; Basic checks due to custom imenu entry format for grouping
                (or (numberp (cadr elm)) (markerp (cadr elm)))   ; Element can be grouped because it was added ...
                (stringp (caddr elm))) ; ... a third field, indicating tree-sitter type
           (let ((type (caddr elm))
                 (entry (cons (car elm) (cadr elm))))
             (pcase type
               ((or "module_instantiation" "interface_instantiation") (push entry instances))
               ("always_construct" (push entry always))
               ("initial_construct" (push entry initial))
               ((or "task_declaration" "task_prototype") (push entry tasks))
               ((or "function_declaration" "function_prototype" "class_constructor_declaration" "class_constructor_prototype") (push entry functions))
               ("class_property" (push entry properties))
               (_ (push entry default))))
         ;; Otherwise entry cannot be grouped because it already was, or because it was a leaf node
         (push elm default)))
     subtree)
    ;; Populate return value
    (if (or always instances initial tasks functions properties) ; Avoid processing when no grouping is required
        (progn
          (when instances
            (setq instances (nreverse instances))
            (setq default `(("Instances" ,@instances) ,@default)))
          (when always
            (setq always (nreverse always))
            (setq default `(("Always" ,@always) ,@default)))
          (when initial
            (setq initial (nreverse initial))
            (setq default `(("Initial" ,@initial) ,@default)))
          (when tasks
            (setq tasks (nreverse tasks))
            (setq default `(("Tasks" ,@tasks) ,@default)))
          (when functions
            (setq functions (nreverse functions))
            (setq default `(("Functions" ,@functions) ,@default)))
          (when properties
            (setq properties (nreverse properties))
            (setq default `(("Properties" ,@properties) ,@default)))
          default)
      ;; Else it might be processing of the leaf nodes of top subtree and reordering is required
      (nreverse default))))

(defun verilog-ts-imenu-treesit-create-index-tree-group (node)
  "Given a sparse tree, create an imenu alist.

NODE is the root node of the tree returned by
`treesit-induce-sparse-tree' (not a tree-sitter node, its car is
a tree-sitter node).  Walk that tree and return an imenu alist.

Return a list of ENTRY where

ENTRY := (NAME . MARKER)
       | (NAME . ((JUMP-LABEL . MARKER)
                  ENTRY
                  ...)

NAME is the function/class's name, JUMP-LABEL is like \"*function
definition*\".

Copied from Python's `python--imenu-treesit-create-index-1' and adapted to
SystemVerilog parser."
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'verilog-ts-imenu-treesit-create-index-tree-group
                           children))
         (type (treesit-node-type ts-node))
         ;; The root of the tree could have a nil ts-node.
         (name (when ts-node
                 (or (verilog-ts--node-identifier-name ts-node)
                     "Anonymous")))
         (pos (treesit-node-start ts-node))
         (marker (when ts-node
                   (if imenu-use-markers
                       (set-marker (make-marker) pos)
                     pos))))
    (cond
     ;; Root node
     ((null ts-node)
      (verilog-ts--imenu-treesit-create-index-tree-group-process subtrees))
     ;; Non-leaf node
     (subtrees
      (let ((parent-label (funcall verilog-ts-imenu-format-parent-item-label-function type name))
            (jump-label (funcall verilog-ts-imenu-format-parent-item-jump-label-function type name)))
        `((,parent-label
           ,(cons jump-label marker)
           ,@(verilog-ts--imenu-treesit-create-index-tree-group-process subtrees)))))
     ;; Leaf node
     (t (let ((label (funcall verilog-ts-imenu-format-item-label-function type name)))
          (if (member type '("module_instantiation"
                             "interface_instantiation"
                             "always_construct"
                             "initial_construct"
                             "function_declaration"
                             "task_declaration"
                             "class_constructor_declaration"
                             "function_prototype"
                             "task_prototype"
                             "class_constructor_prototype"
                             "class_property"))
              (list (list label marker type))
            (list (cons label marker))))))))

(defun verilog-ts--imenu-create-index (func &optional node)
  "Imenu index builder function for `verilog-ts-mode'.

FUNC is the function that traverses the syntax tree and builds the index suited
for Imenu.

NODE is the root node of the subtree you want to build an index
of.  If nil, use the root node of the whole parse tree.

Copied from Python's `python-imenu-treesit-create-index' and adapted
to SystemVerilog parser."
  (let* ((node (or node (treesit-buffer-root-node 'verilog)))
         (tree (treesit-induce-sparse-tree
                node
                verilog-ts-imenu-create-index-re
                nil 1000)))
    (funcall func tree)))

(defun verilog-ts-imenu-create-index-tree ()
  "Create `imenu' index alist with a tree structure."
  (verilog-ts--imenu-create-index #'verilog-ts-imenu-treesit-create-index-tree))

(defun verilog-ts-imenu-create-index-tree-group ()
  "Create `imenu' index alist with a tree structure with subgroups."
  (verilog-ts--imenu-create-index #'verilog-ts-imenu-treesit-create-index-tree-group))

(defun verilog-ts-imenu-setup ()
  "Setup `imenu' based on value of `verilog-ts-imenu-style'."
  (pcase verilog-ts-imenu-style
    ('simple
     (setq-local treesit-simple-imenu-settings
                 `(("Module" "\\`entity_declaration\\'")
                   ("Interface" "\\`interface_declaration\\'")
                   ("Program" "\\`program_declaration\\'")
                   ("Package" "\\`package_declaration\\'")
                   ("Class" "\\`class_declaration\\'")
                   ("Function" "\\`\\(function\\|class_constructor\\)_declaration\\'")
                   ("Task" "\\`task_declaration\\'")
                   ("Function prototype" "\\`\\(function\\|class_constructor\\)_prototype\\'")
                   ("Task prototype" "\\`task_prototype\\'")
                   ("Property" "\\`class_property\\'")
                   ("Always" "\\`always_construct\\'")
                   ("Initial" "\\`initial_construct\\'")
                   ("Final" "\\`final_construct\\'")
                   ("Generate" "\\`generate_region\\'")
                   ("Instance" "\\`\\(module\\|interface\\)_instantiation\\'")))
     (setq-local treesit-defun-name-function #'verilog-ts--node-identifier-name))
    ('tree
     (setq-local imenu-create-index-function #'verilog-ts-imenu-create-index-tree))
    ('tree-group
     (setq-local imenu-create-index-function #'verilog-ts-imenu-create-index-tree-group))
    (_ (error "Wrong value for `verilog-ts-imenu-style': set to simple/tree/tree-group"))))


;;; Which-func
(defvar-local verilog-ts-which-func-extra nil
  "Variable to hold extra information for `which-func'.")

(defvar verilog-ts-which-func-format-function 'verilog-ts-which-func-format-shorten
  "Variable of the function to be called to return the type in `which-func'.

It must have one argument (tree-sitter node) and must return a string with the
type.")

(defun verilog-ts-which-func-format-simple (node)
  "Return tree-sitter type of current NODE."
  (treesit-node-type node))

(defun verilog-ts-which-func-format-shorten (node)
  "Return shortened name of NODE if possible."
  (pcase (treesit-node-type node)
    ("module_declaration"            "mod")
    ("interface_declaration"         "itf")
    ("program_declaration"           "pgm")
    ("package_declaration"           "pkg")
    ("class_declaration"             "cls")
    ("function_declaration"          "fun")
    ("task_declaration"              "task")
    ("class_constructor_declaration" "new")
    ("function_prototype"            "fun")
    ("task_prototype"                "task")
    ("class_constructor_prototype"   "new")
    ("generate_region"               "gen")
    ("module_instantiation"          (verilog-ts--node-instance-name node))
    ("interface_instantiation"       (verilog-ts--node-instance-name node))
    (_                               (treesit-node-type node))))

(defun verilog-ts-which-func-function ()
  "Retrieve `which-func' candidates."
  (let ((node (verilog-ts--block-at-point verilog-ts-imenu-create-index-re)))
    (if node
        (progn
          (setq verilog-ts-which-func-extra (verilog-ts--node-identifier-name node))
          (funcall verilog-ts-which-func-format-function node))
      (setq verilog-ts-which-func-extra nil)
      "n/a")))

(defun verilog-ts-which-func-setup ()
  "Hook for `verilog-ts-mode' to enable `which-func'."
  (pcase verilog-ts-which-func-style
    ('simple
     (setq-local which-func-functions nil)
     (setq-local which-func-imenu-joiner-function (lambda (x) (car (last x)))))
    ('breadcrumb
     (setq-local which-func-functions nil)
     (setq-local which-func-imenu-joiner-function (lambda (x) (string-join x "."))))
    ('custom
     (setq-local which-func-functions '(verilog-ts-which-func-function))
     (setq-local which-func-format
                 `("["
                   (:propertize which-func-current
                    face (which-func :weight bold)
                    mouse-face mode-line-highlight)
                   ":"
                   (:propertize verilog-ts-which-func-extra
                    face which-func
                    mouse-face mode-line-highlight)
                   "]")))
    (_ (error "Wrong value for `verilog-ts-which-func-style': set to default/breadcrumb/custom"))))


;;; Navigation
(defconst verilog-ts-defun-re
  (eval-when-compile
    (regexp-opt
     '("module_declaration"
       "interface_declaration"
       "package_declaration"
       "program_declaration"
       "class_declaration"
       "function_declaration"
       "task_declaration"
       "class_method"
       "class_constructor_declaration")
     'symbols)))

(defconst verilog-ts-backward-sexp-re
  (eval-when-compile
    (regexp-opt
     '("endmodule"
       "endinterface"
       "endprogram"
       "endpackage"
       "endclass"
       "endfunction"
       "endtask")
     'symbols)))

(defconst verilog-ts-function-task-re
  (eval-when-compile
    (regexp-opt
     '("task_declaration"
       "function_declaration"
       "class_method" ; "class_method" includes constructor and extern prototypes
       "class_constructor_declaration")
     'symbols)))

(defconst verilog-ts-function-task-class-re
  (eval-when-compile
    (regexp-opt
     '("class_declaration"
       "task_declaration"
       "function_declaration"
       "class_method"
       "class_constructor_declaration")
     'symbols)))

(defconst verilog-ts-goto-begin-down-re
  (eval-when-compile
    (regexp-opt
     '("task_declaration"
       "function_declaration"
       "class_method" ; "class_method" includes constructor and extern prototypes
       "class_constructor_declaration"
       "seq_block")
     'symbols)))

(defvar-local verilog-ts--begin-level-down-parent-node nil)
(defvar-local verilog-ts--defun-level-down-parent-node nil)


(defun verilog-ts-forward-sexp (&optional arg)
  "Move forward across S-expressions.

With `prefix-arg', move ARG expressions."
  (interactive "p")
  (let* ((node (verilog-ts--node-at-point))
         (node-type (treesit-node-type node))
         (highest-node (treesit-parent-while ; Same as `verilog-ts--highest-node-at-point' but ignoring package_item and class_item
                        node
                        (lambda (loop-node)
                          (and (eq (treesit-node-start node) (treesit-node-start loop-node))
                               (not (string-match "\\_<\\(package\\|class\\)_item\\_>" (treesit-node-type loop-node)))))))
         (highest-node-type (treesit-node-type highest-node)))
    (cond (;; Defuns
           (or (string-match verilog-ts-defun-re highest-node-type)
               (string= "extern" node-type))
           (if (and arg (< arg 0))
               (goto-char (treesit-node-start highest-node))
             (goto-char (treesit-node-end highest-node))))
          ;; begin/end
          ((string= "begin" node-type)
           (let ((seq-block-node (verilog-ts--node-has-parent-recursive node "\\(seq\\|generate\\)_block")))
             (goto-char (treesit-node-end seq-block-node))))
          ;; Default
          (t
           (if (and arg (< arg 0))
               (backward-sexp arg)
             (forward-sexp arg))))))

(defun verilog-ts-backward-sexp (&optional arg)
  "Move backward across S-expressions.

With `prefix-arg', move ARG expressions."
  (interactive "p")
  (let* ((node (verilog-ts--node-at-point))
         (node-type (treesit-node-type node))
         (prev-sibling (treesit-node-prev-sibling node))
         (prev-sibling-2 (treesit-node-prev-sibling prev-sibling))
         (prev-sibling-type (treesit-node-type prev-sibling))
         (prev-sibling-2-type (treesit-node-type prev-sibling-2)))
    (cond (;; Defuns
           (or (string-match verilog-ts-backward-sexp-re node-type)
               (and (string= ":" node-type)
                    (string-match verilog-ts-backward-sexp-re prev-sibling-type))
               (and (string= "simple_identifier" node-type)
                    (string= ":" prev-sibling-type)
                    (string-match verilog-ts-backward-sexp-re prev-sibling-2-type)))
           (let ((parent-node (verilog-ts--node-has-parent-recursive node verilog-ts-defun-re)))
             (if (and arg (< arg 0))
                 (goto-char (treesit-node-end parent-node))
               (goto-char (treesit-node-start parent-node)))))
          ;; begin/end
          ((string= "end" node-type)
           (let ((seq-block-node (verilog-ts--node-has-parent-recursive node "\\(seq\\|generate\\)_block")))
             (goto-char (treesit-node-start seq-block-node))))
          ;; Default
          (t
           (if (and arg (< arg 0))
               (forward-sexp arg)
             (backward-sexp arg))))))

(defun verilog-ts-find-function-task (&optional bwd)
  "Search for a Verilog function/task declaration or definition.

If optional arg BWD is non-nil, search backwards."
  (treesit-search-forward-goto (verilog-ts--node-at-point) verilog-ts-function-task-re t bwd))

(defun verilog-ts-find-function-task-fwd ()
  "Search forward for a Verilog function/task declaration or definition."
  (verilog-ts-find-function-task))

(defun verilog-ts-find-function-task-bwd ()
  "Search backward for a Verilog function/task declaration or definition."
  (verilog-ts-find-function-task :bwd))

(defun verilog-ts-find-class (&optional bwd)
  "Search for a class declaration.

If optional arg BWD is non-nil, search backwards."
  (treesit-search-forward-goto (verilog-ts--node-at-point) "class_declaration" t bwd))

(defun verilog-ts-find-class-fwd ()
  "Search forward for a Verilog class declaration."
  (verilog-ts-find-class))

(defun verilog-ts-find-class-bwd ()
  "Search backward for a Verilog class declaration."
  (verilog-ts-find-class :bwd))

(defun verilog-ts-find-function-task-class (&optional bwd)
  "Find closest declaration of a function/task/class.

If optional arg BWD is non-nil, search backwards."
  (treesit-search-forward-goto (verilog-ts--node-at-point) verilog-ts-function-task-class-re t bwd))

(defun verilog-ts-find-function-task-class-fwd ()
  "Search forward for a Verilog function/task/class declaration."
  (verilog-ts-find-function-task-class))

(defun verilog-ts-find-function-task-class-bwd ()
  "Search backward for a Verilog function/task/class declaration."
  (verilog-ts-find-function-task-class :bwd))

(defun verilog-ts-find-block (&optional bwd)
  "Search for a Verilog block regexp.

If optional arg BWD is non-nil, search backwards."
  (treesit-search-forward-goto (verilog-ts--node-at-point) verilog-ts-block-at-point-re t bwd))

(defun verilog-ts-find-block-fwd ()
  "Search forward for a Verilog block regexp."
  (verilog-ts-find-block))

(defun verilog-ts-find-block-bwd ()
  "Search backwards for a Verilog block regexp."
  (verilog-ts-find-block :bwd))

(defun verilog-ts-find-module-instance (&optional bwd)
  "Search for a Verilog module/instance.

If optional arg BWD is non-nil, search backwards."
  (treesit-search-forward-goto (verilog-ts--node-at-point) verilog-ts-instance-re t bwd))

(defun verilog-ts-find-module-instance-fwd ()
  "Search forwards for a Verilog module/instance."
  (verilog-ts-find-module-instance))

(defun verilog-ts-find-module-instance-bwd ()
  "Search backwards for a Verilog module/instance."
  (verilog-ts-find-module-instance :bwd))

(defun verilog-ts--begin-level-down-fn (node)
  "Aux function to search nodes in the subtree for `verilog-ts-goto-begin-down'."
  (and (string-match verilog-ts-goto-begin-down-re (treesit-node-type node)) ; Match against begin-down regexp (default behavior)
       (> (treesit-node-start node) (point))                                      ; Downwards navigating should always move point forward
       (not (equal node verilog-ts--begin-level-down-parent-node))))              ; Exclude current node to avoid deadlocks while navigating down

(defun verilog-ts-goto-begin-down ()
  "Move point to start position of next nested begin."
  (let* ((parent-node (or (verilog-ts--node-has-parent-recursive (verilog-ts--highest-node-at-point) verilog-ts-goto-begin-down-re)
                          (verilog-ts--highest-node-at-point)))
         (node (when parent-node
                 (setq verilog-ts--begin-level-down-parent-node parent-node)
                 (treesit-search-subtree parent-node 'verilog-ts--begin-level-down-fn)))
         (pos (treesit-node-start node)))
    (if (and pos
             (<= pos (treesit-node-end parent-node)))
        (goto-char pos)
      (when (called-interactively-p 'any)
        (message "No more begin downwards")))))

(defun verilog-ts-goto-begin-up ()
  "Move point to start position of current begin."
  (let* ((node-at-point (verilog-ts--node-at-point))
         (begin-node (treesit-parent-until
                      node-at-point
                      (lambda (node)
                        (and (string-match "\\_<seq_block\\_>" (treesit-node-type node))
                             (not (equal (treesit-node-start node-at-point)
                                         (treesit-node-start node)))))))
         (begin-pos (treesit-node-start begin-node)))
    (when begin-pos
      (goto-char begin-pos))))

(defun verilog-ts--defun-level-down-fn (node)
  "Aux function to search nodes in the subtree for `verilog-ts-defun-level-down'."
  (and (string-match verilog-ts-defun-re (treesit-node-type node)) ; Match against defun regexp (default behavior)
       (> (treesit-node-start node) (point))                       ; Downwards navigating should always move point forward
       (not (equal node verilog-ts--defun-level-down-parent-node)) ; Exclude current node to avoid deadlocks while navigating down
       (not (member (treesit-node-type (verilog-ts--node-at-point :bound)) '("extern" "task" "function" "new"))))) ; Prevent navigation if point is already on a task/function

(defun verilog-ts-defun-level-down ()
  "Move down one defun-level."
  (interactive)
  (let* ((parent-node (or (verilog-ts--node-has-parent-recursive (verilog-ts--highest-node-at-point) verilog-ts-defun-re)
                          (verilog-ts--highest-node-at-point)))
         (node (when parent-node
                 (setq verilog-ts--defun-level-down-parent-node parent-node)
                 (treesit-search-subtree parent-node 'verilog-ts--defun-level-down-fn)))
         (pos (treesit-node-start node)))
    (if (and pos
             (<= pos (treesit-node-end parent-node)))
        (goto-char pos)
      (when (called-interactively-p 'any)
        (message "No more defuns downwards")))))

(defun verilog-ts-defun-level-up ()
  "Move up one defun-level."
  (interactive)
  (let* ((cur-node (verilog-ts--highest-node-at-point))
         (node (verilog-ts--node-has-parent-recursive cur-node verilog-ts-defun-re))
         (pos (treesit-node-start node)))
    (if pos
        (goto-char pos)
      (when (called-interactively-p 'any)
        (message "No more defuns upwards")))))

(defun verilog-ts-nav-up-dwim ()
  "Contextual based search upwards.

- If inside a module or interface, navigate instances backwards.
- Otherwise if in a begin/end block move to corresponding begin.
- In any other case move one defun level up."
  (interactive)
  (if (verilog-ts--inside-module-or-interface-p)
      (verilog-ts-find-module-instance-bwd)
    ;; Else
    (if (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) "seq_block")
        (verilog-ts-goto-begin-up)
      (call-interactively #'verilog-ts-defun-level-up))))

(defun verilog-ts-nav-down-dwim ()
  "Contextual based search downwards.

- If inside a module or interface, navigate instances forward.
- Otherwise try to find nested begin.
- In any other case move one defun level down."
  (interactive)
  (if (verilog-ts--inside-module-or-interface-p)
      (verilog-ts-find-module-instance-fwd)
    ;; Else
    (if (verilog-ts--node-has-parent-recursive (verilog-ts--highest-node-at-point) verilog-ts-goto-begin-down-re)
        (verilog-ts-goto-begin-down)
      (call-interactively #'verilog-ts-defun-level-down))))

(defun verilog-ts-nav-prev-dwim ()
  "Context based search previous.

If in a parenthesis, go to opening parenthesis (Elisp like).
Otherwise move through relevant language constructs."
  (interactive)
  (if (or (member (preceding-char) '(?\) ?\] ?\}))
          (string= (symbol-at-point) "end"))
      (verilog-ts-backward-sexp)
    (verilog-ts-find-block-bwd)))

(defun verilog-ts-nav-next-dwim ()
  "Context based search next.

If in a parenthesis, go to closing parenthesis (Elisp like).
Otherwise move through relevant language constructs."
  (interactive)
  (if (or (member (following-char) '(?\( ?\[ ?\{ ?\) ?\] ?\}))
          (string= (symbol-at-point) "begin"))
      (verilog-ts-forward-sexp)
    (verilog-ts-find-block-fwd)))

(defun verilog-ts-goto-next-error ()
  "Move point to next error in the parse tree."
  (interactive)
  (or (treesit-search-forward-goto (verilog-ts--node-at-point) "ERROR" t)
      (when (called-interactively-p 'any)
        (message "No more errors"))))

(defun verilog-ts-goto-prev-error ()
  "Move point to previous error in the parse tree."
  (interactive)
  (or (treesit-search-forward-goto (verilog-ts--node-at-point) "ERROR" t :bwd)
      (when (called-interactively-p 'any)
        (message "No more errors"))))


;;; Prettify
;;;; Common aux functions
(defconst verilog-ts-pretty-decl-nodes-re
  (eval-when-compile
    (regexp-opt
     '("list_of_net_decl_assignments"
       "list_of_variable_decl_assignments"
       "ansi_port_declaration"
       "parameter_port_declaration"
       "local_parameter_declaration"
       "parameter_declaration"
       "parameter_override")
     'symbols)))

(defconst verilog-ts-pretty-expr-nodes-re
  (eval-when-compile
    (regexp-opt
     '("nonblocking_assignment"
       "blocking_assignment"
       "list_of_net_decl_assignments"
       "list_of_variable_decl_assignments"
       "ansi_port_declaration"
       "parameter_port_declaration"
       "local_parameter_declaration"
       "parameter_declaration"
       "parameter_override"
       "continuous_assign")
     'symbols)))

(defun verilog-ts-pretty--contiguous-elms (pred elm seq)
  "Return sublist of contiguous elements that match PRED in SEQ.
Search around matching ELM in SEQ."
  (let* ((idx (when elm (seq-position seq elm)))
         (elms-after (when idx (seq-take-while pred (seq-subseq seq idx))))
         (elms-before (when idx (seq-take-while pred (nreverse (seq-subseq seq 0 idx))))))
    `(,@(nreverse elms-before) ,@elms-after)))

(defun verilog-ts-pretty--block-siblings (node-re &optional process-fn)
  "Return syntax tree preprocessed to get siblings that match NODE-RE.

Includes the blocks at point nodes so that there is a parent node for all
desired siblings.

If provided optional arg PROCESS-FN run it on each element of the induced
sparse tree."
  (cadr
   (treesit-induce-sparse-tree
    (or (verilog-ts-block-at-point)
        (treesit-buffer-root-node))
    (concat "\\(\\(" verilog-ts-block-at-point-re "\\)\\|\\(" node-re "\\)\\)")
    process-fn)))

(defun verilog-ts-pretty--nodes (type &optional no-filter-expr)
  "Syntactic sugar to retrieve nodes to be prettified for declaration at point.

TYPE could be either \='decl or \='expr depending on the thing to be aligned.

If optional arg NO-FILTER-EXPR is non-nil, retrieve all siblings of
expression-like nodes even if there is no assignment."
  (let* ((nodes-re (pcase type
                     ('decl verilog-ts-pretty-decl-nodes-re)
                     ('expr verilog-ts-pretty-expr-nodes-re)
                     (_ (error "Unexpected type: %s" type))))
         (cur-decl-node (verilog-ts--node-has-child-recursive
                         (if (region-active-p)
                             (save-excursion
                               (goto-char (region-beginning))
                               (skip-chars-forward " \t")
                               (verilog-ts--highest-node-at-pos (point)))
                           (verilog-ts--node-at-bol))
                         nodes-re))
         (cur-decl-node-type (treesit-node-type cur-decl-node))
         (all-siblings
          (mapcar #'car
                  (cdr (pcase cur-decl-node-type
                         ((or "list_of_net_decl_assignments"
                              "list_of_variable_decl_assignments")
                          (if (verilog-ts--node-has-parent-recursive cur-decl-node "\\_<struct_union_member\\_>")
                              (treesit-induce-sparse-tree (verilog-ts--node-has-parent-recursive cur-decl-node "data_declaration") "\\(\\_<struct_union\\_>\\|list_of_variable_decl_assignments\\)")
                            (verilog-ts-pretty--block-siblings "list_of_\\(net\\|variable\\)_decl_assignments")))
                         ("ansi_port_declaration"
                          (treesit-induce-sparse-tree (verilog-ts--node-has-parent-recursive cur-decl-node "list_of_port_declarations") cur-decl-node-type))
                         ("parameter_port_declaration"
                          (treesit-induce-sparse-tree (verilog-ts--node-has-parent-recursive cur-decl-node "parameter_port_list") cur-decl-node-type))
                         ((or "local_parameter_declaration"
                              "parameter_declaration"
                              "parameter_override")
                          (seq-remove (lambda (elm)
                                        (and (listp elm)
                                             (eq (car elm) nil)))
                                      (verilog-ts-pretty--block-siblings
                                       "\\(\\(local_\\)?parameter_declaration\\|parameter_override\\)"
                                       (lambda (node)
                                         (when (not (verilog-ts--node-has-parent-recursive node "parameter_port_list"))
                                           node)))))
                         ((or "blocking_assignment"
                              "nonblocking_assignment")
                          (verilog-ts-pretty--block-siblings "\\(non\\)?blocking_assignment"))
                         ("continuous_assign"
                          (verilog-ts-pretty--block-siblings "continuous_assign"))
                         (_ nil)))))
         (nodes (verilog-ts-pretty--contiguous-elms (lambda (node)
                                                      (string-match nodes-re (treesit-node-type node)))
                                                    cur-decl-node
                                                    all-siblings)))
    ;; Filter nodes if region is active
    (when (region-active-p)
      (setq nodes (seq-filter (lambda (node)
                                (and (>= (treesit-node-start node) (region-beginning))
                                     (<= (treesit-node-end node) (region-end))))
                              nodes)))
    ;; Return nodes
    (pcase type
      ('decl nodes)
      ('expr (if no-filter-expr
                 nodes
               (seq-filter (lambda (node)
                             (verilog-ts--node-has-child-recursive node "<?="))
                           nodes)))
      (_ (error "Unexpected type: %s" type)))))

(defun verilog-ts-align-comments (nodes)
  "Align inline comments of NODES."
  (let* ((node-lines-with-comments (save-excursion
                                     (seq-filter (lambda (node)
                                                   (goto-char (treesit-node-start node))
                                                   (goto-char (line-beginning-position))
                                                   (re-search-forward "//" (line-end-position) t))
                                                 nodes)))
         (node-lines (mapcar (lambda (node)
                               (line-number-at-pos (treesit-node-start node)))
                             node-lines-with-comments))
         (indent-levels (mapcar (lambda (node)
                                  (save-excursion
                                    (goto-char (treesit-node-start node))
                                    (goto-char (line-beginning-position))
                                    (re-search-forward "//" (line-end-position) t)
                                    (backward-char 2)
                                    (skip-chars-backward " \t\n\r")
                                    (forward-char)
                                    (current-column)))
                                node-lines-with-comments))
         (indent-level-max (when indent-levels
                             (apply #'max indent-levels))))
    (when node-lines-with-comments
      (save-excursion
        (dolist (line node-lines)
          (goto-char (point-min))
          (forward-line (1- line))
          (re-search-forward "//" (line-end-position) t)
          (backward-char 2)
          (just-one-space)
          (indent-to indent-level-max))))))

(defun verilog-ts--pretty (type)
  "Line up declarations and expressions around point.

TYPE could be either \='decl or \='expr depending on the thing to be aligned."
  (let* ((nodes-re (pcase type
                     ('decl verilog-ts-pretty-decl-nodes-re)
                     ('expr verilog-ts-pretty-expr-nodes-re)
                     (_ (error "Unexpected type: %s" type))))
         (nodes (verilog-ts-pretty--nodes type))
         (node-lines (mapcar (lambda (node)
                               (line-number-at-pos (treesit-node-start node)))
                             nodes))
         (indent-levels
          (mapcar (lambda (node)
                    (save-excursion
                      (pcase type
                        ('decl (verilog-ts-pretty-decl--goto-node-start node))
                        ('expr (verilog-ts-pretty-expr--goto-node-start node))
                        (_ (error "Unexpected type: %s" type)))
                      (skip-chars-backward " \t\n\r")
                      (forward-char)
                      (current-column)))
                  nodes))
         (indent-level-max (when indent-levels
                             (apply #'max indent-levels)))
         cur-node)
    ;; Start processing
    (if nodes
        (save-excursion
          (dolist (line node-lines)
            (goto-char (point-min))
            (forward-line (1- line))
            (when (setq cur-node (verilog-ts--node-has-child-recursive (verilog-ts--node-at-bol :skip-comment) nodes-re))
              (pcase type
                ('decl (verilog-ts-pretty-decl--goto-node-start cur-node))
                ('expr (verilog-ts-pretty-expr--goto-node-start cur-node))
                (_ (error "Unexpected type: %s" type)))
              (just-one-space)
              (indent-to indent-level-max)))
          ;; Align comments if enabled
          (when verilog-ts-align-decl-expr-comments
            (verilog-ts-align-comments (verilog-ts-pretty--nodes type :no-filter-expr)))
          (message "Aligned to col %s" indent-level-max))
      (message "Point not at a declaration"))))

;;;; Declarations
(defun verilog-ts-pretty-decl--goto-node-start (node)
  "Syntactic sugar to move point to NODE start position."
  (let ((node-pos (treesit-node-start
                   (pcase (treesit-node-type node)
                     ((or "list_of_net_decl_assignments"
                          "list_of_variable_decl_assignments")
                      node)
                     ("ansi_port_declaration" (treesit-node-child-by-field-name node "port_name"))
                     ("parameter_port_declaration" (verilog-ts--node-has-child-recursive node "list_of_\\(param\\|type\\)_assignments"))
                     ((or "local_parameter_declaration"
                          "parameter_declaration"
                          "parameter_override")
                      (verilog-ts--node-has-child-recursive node "list_of_\\(def\\)?param_assignments"))
                     (_ nil)))))
    (when node-pos
      (goto-char node-pos))))

(defun verilog-ts-pretty-decl--goto-node-end (node-type)
  "Syntactic sugar to move point to current node end pos depending on NODE-TYPE."
  (let* ((node-at-point (verilog-ts--node-at-point))
         (node (if (string-match node-type (treesit-node-type node-at-point))
                   node-at-point
                 (or (verilog-ts--node-has-parent-recursive node-at-point node-type)
                     (verilog-ts--node-has-child-recursive node-at-point node-type))))
         node-pos)
    (pcase (treesit-node-type node)
      ((or "list_of_net_decl_assignments"
           "list_of_variable_decl_assignments")
       (setq node-pos (treesit-node-end (verilog-ts--node-has-parent-recursive node "\\_<\\(net\\|data\\)_declaration\\_>")))
       (when node-pos
         (goto-char node-pos))
       (forward-char)) ; Avoid getting stuck
      ((or "ansi_port_declaration"
           "parameter_port_declaration"
           "local_parameter_declaration"
           "parameter_declaration"
           "parameter_override")
       (setq node-pos (treesit-node-end node))
       (when node-pos
         (goto-char node-pos))
       (forward-line 1)) ; Avoid getting stuck
      (_
       (error "Unexpected node-type: %s" node-type)))))

(defun verilog-ts-pretty-declarations ()
  "Line up declarations around point."
  (interactive)
  (verilog-ts--pretty 'decl))

;;;; Expressions
(defun verilog-ts-pretty-expr--goto-node-start (node)
  "Syntactic sugar to move point to NODE start position."
  (let ((node-pos (treesit-node-start (verilog-ts--node-has-child-recursive node "<?="))))
    (when node-pos
      (goto-char node-pos))))

(defun verilog-ts-pretty-expr--goto-node-end ()
  "Syntactic sugar to move point to NODE end position."
  (let* ((node (verilog-ts--node-at-point))
         node-pos)
    (pcase (treesit-node-type node)
      ((or "ansi_port_declaration"
           "parameter_port_declaration"
           "local_parameter_declaration"
           "parameter_declaration"
           "parameter_override")
       (setq node-pos (treesit-node-end node))
       (when node-pos
         (goto-char node-pos))
       (forward-line 1))
      (_
       (setq node-pos (treesit-node-end (verilog-ts--node-has-parent-recursive node verilog-ts-pretty-expr-nodes-re)))
       (when node-pos
         (goto-char node-pos))
       (forward-char)))))

(defun verilog-ts-pretty-expr ()
  "Line up expressions around point."
  (interactive)
  (verilog-ts--pretty 'expr))

;;;; File
(defun verilog-ts-pretty--search-forward (type)
  "Search forward next prettifiable node.

TYPE could be either \='decl or \='expr depending on the thing to be aligned.

Take into account that due to `treesit-search-forward' behavior with
`verilog-ts-pretty-decl-nodes-re' and `verilog-ts-pretty-expr-nodes-re',
parameter_port_declaration might be shadowed by parameter_declaration nodes."
  (let* ((nodes-re (pcase type
                     ('decl verilog-ts-pretty-decl-nodes-re)
                     ('expr verilog-ts-pretty-expr-nodes-re)
                     (_ (error "Unexpected type: %s" type))))
         (node (treesit-search-forward (verilog-ts--node-at-point) nodes-re)))
    (when (and node
               (eq type 'expr)
               (not (verilog-ts--node-has-child-recursive node "<?=")))
      (goto-char (treesit-node-end node))
      (forward-line 1)
      (verilog-ts-pretty--search-forward 'expr))
    (cond (;; Parameter port
           (and (string= (treesit-node-type node) "parameter_declaration")
                (verilog-ts--node-has-parent-recursive node "parameter_port_declaration"))
           (verilog-ts--node-has-parent-recursive node "parameter_port_declaration"))
          ;; Default
          (t node))))

(defun verilog-ts-pretty-current-buffer ()
  "Align all declarations and expressions of current buffer."
  (let (node node-type)
    ;; Declarations
    (save-excursion
      (goto-char (point-min))
      (while (setq node (verilog-ts-pretty--search-forward 'decl))
        (setq node-type (treesit-node-type node))
        (goto-char (treesit-node-start node))
        (verilog-ts-pretty-declarations)
        (verilog-ts-pretty-decl--goto-node-end node-type))) ; Move to next declaration
    ;; Expressions
    (save-excursion
      (goto-char (point-min))
      (while (setq node (verilog-ts-pretty--search-forward 'expr))
        (setq node-type (treesit-node-type node))
        (goto-char (treesit-node-start node))
        (verilog-ts-pretty-expr)
        (verilog-ts-pretty-expr--goto-node-end))))) ; Move to next expression

;;; Beautify
(defun verilog-ts-beautify-instance-at-point--align-node-lines (thing-node-type &optional f-ts-nodes)
  "Return cons cell with line numbers of nodes to be aligned and indent level.

Expected THING-NODE-TYPE is either `named_parameter_assignment' or
`named_port_connection'.

F-TS-NODES is a function with one argument which is a tree-sitter node, used to
calculate the indentation level to be returned.  If not provided calculate
according to found parenthesis."
  (let ((f-parens (lambda (node)
                    (and (treesit-search-subtree node "(" nil :all)
                         (treesit-search-subtree node ")" nil :all))))
        instance-node align-nodes align-nodes-cur-indent-level align-nodes-lines indent-level)
    (setq instance-node (verilog-ts-block-at-point)) ; Refresh outdated instance-node after `indent-region'
    (setq align-nodes (seq-filter (lambda (node)
                                    (funcall f-parens node))
                                  (verilog-ts-nodes thing-node-type instance-node)))
    (when align-nodes (setq align-nodes-lines ; Needed because of outdated nodes if trying to use align-nodes directly
                            (delete-dups (mapcar (lambda (node)
                                                   (line-number-at-pos (treesit-node-start node)))
                                                 align-nodes)))
          (when (> (length align-nodes-lines) 1) ; Ignore one-liners
            (setq align-nodes-cur-indent-level (mapcar (lambda (node)
                                                         (goto-char (treesit-node-end (funcall (or f-ts-nodes f-parens) node)))
                                                         (current-column))
                                                       align-nodes))
            (setq indent-level (1+ (apply #'max align-nodes-cur-indent-level)))
            (cons align-nodes-lines indent-level)))))

(defun verilog-ts-beautify-instance-at-point--align (thing)
  "Align THING of current module at point (ports/parameters)."
  (let ((f-sibling-at-point (lambda (parent-pred search-node)
                              (let ((parent-node (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) parent-pred)))
                                (when parent-node
                                  (treesit-search-subtree parent-node search-node nil :all)))))
        (f-identifier (lambda (node)
                        (cond ((eq thing 'parameters) (treesit-search-subtree node "\\_<simple_identifier\\_>"))
                              ((eq thing 'ports) (treesit-node-child-by-field-name node "port_name"))
                              (t (error "Invalid thing to align")))))
        (f-space (if verilog-ts-beautify-instance-extra
                     #'just-one-space
                   #'delete-horizontal-space))
        (thing-node-type (cond ((eq thing 'parameters) "\\_<named_parameter_assignment\\_>")
                               ((eq thing 'ports) "\\_<named_port_connection\\_>")
                               (t (error "Invalid thing to align"))))
        temp-node temp-var align-nodes-lines indent-level)
    ;; Implementation
    (setq temp-var (verilog-ts-beautify-instance-at-point--align-node-lines thing-node-type f-identifier))
    (setq align-nodes-lines (car temp-var))
    (setq indent-level (cdr temp-var))
    (mapc (lambda (node-line)
            (goto-char (point-min))
            (forward-line (1- node-line))
            (when (setq temp-node (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) thing-node-type))
              (goto-char (treesit-node-end (funcall f-identifier temp-node)))
              (just-one-space)
              (indent-to indent-level)
              (goto-char (treesit-node-end (funcall f-sibling-at-point thing-node-type "(")))
              (funcall f-space)
              (goto-char (treesit-node-start (funcall f-sibling-at-point thing-node-type ")")))
              (funcall f-space)))
          align-nodes-lines)
    ;; Extra alignment processing for closing port/parameter parenthesis
    (when verilog-ts-beautify-instance-extra
      (setq temp-var (verilog-ts-beautify-instance-at-point--align-node-lines thing-node-type))
      (setq align-nodes-lines (car temp-var))
      (setq indent-level (cdr temp-var))
      (when indent-level
        (setq indent-level (- indent-level 2)))
      (mapc (lambda (node-line)
              (goto-char (point-min))
              (forward-line (1- node-line))
              (goto-char (treesit-node-start (funcall f-sibling-at-point thing-node-type ")")))
              (just-one-space)
              (indent-to indent-level))
            align-nodes-lines))
    ;; Align comments
    (verilog-ts-align-comments (verilog-ts-nodes thing-node-type (verilog-ts-block-at-point)))))

(defun verilog-ts-beautify-block-at-point ()
  "Beautify/indent block at point.
If block is an instance, also align parameters and ports."
  (interactive)
  (let ((node (verilog-ts-block-at-point))
        type name)
    (unless node
      (user-error "Not inside a block"))
    (save-excursion
      (setq type (treesit-node-type node))
      (setq name (verilog-ts--node-identifier-name node))
      (indent-region (treesit-node-start node) (treesit-node-end node))
      (when (string-match verilog-ts-instance-re type)
        (verilog-ts-beautify-instance-at-point--align 'parameters)
        (verilog-ts-beautify-instance-at-point--align 'ports)))
    (message "%s : %s" type name)))

(defun verilog-ts-beautify-instances-current-buffer ()
  "Beautify all the instances in current buffer."
  (let (node)
    (save-excursion
      (goto-char (point-min))
      (while (setq node (treesit-search-forward (verilog-ts--node-at-point) verilog-ts-instance-re))
        (goto-char (treesit-node-start node))
        (verilog-ts-beautify-block-at-point)
        (setq node (treesit-search-forward (verilog-ts--node-at-point) verilog-ts-instance-re))
        (goto-char (treesit-node-end node))
        (when (not (eobp))
          (forward-char))))))

(defun verilog-ts-beautify-current-buffer ()
  "Beautify current buffer:
- Indent whole buffer
- Beautify every instantiated module
- Prettify declarations and expressions
- Untabify and delete trailing whitespace"
  (interactive)
  (indent-region (point-min) (point-max))
  (verilog-ts-beautify-instances-current-buffer)
  (verilog-ts-pretty-current-buffer)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace (point-min) (point-max))
  (message "Beautified %s" (or (and (buffer-file-name)
                                    (file-name-nondirectory (buffer-file-name)))
                               (buffer-name))))

(defun verilog-ts-beautify-files (files)
  "Beautify SystemVerilog FILES.
FILES is a list of strings containing the filepaths."
  (dolist (file files)
    (unless (file-exists-p file)
      (error "File %s does not exist! Aborting!" file)))
  (save-window-excursion
    (dolist (file files)
      (with-temp-file file
        (insert-file-contents file)
        (verilog-ts-mode)
        (verilog-ts-beautify-current-buffer)))))

(defun verilog-ts-beautify-dir-files (dir &optional follow-symlinks)
  "Beautify Verilog files on DIR.

If FOLLOW-SYMLINKS is non-nil, symbolic links that point to
directories are followed.  Note that this can lead to infinite
recursion."
  (interactive "DDirectory: ")
  (let ((files (directory-files-recursively dir
                                            verilog-ts-file-extension-re
                                            nil nil
                                            follow-symlinks)))
    (verilog-ts-beautify-files files)))


;;; Completion
(defun verilog-ts-completion-at-point ()
  "Verilog tree-sitter powered completion at point.

Complete with keywords and current buffer identifiers."
  (interactive)
  (let* ((bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds))
         candidates)
    (setq candidates (remove (thing-at-point 'symbol :no-props)
                             (append (mapcar (lambda (node-and-props)
                                               (plist-get (cdr node-and-props) :name))
                                             (verilog-ts-nodes-props "simple_identifier"))
                                     verilog-keywords)))
    (list start end candidates . nil)))


;;; Auto
(defun verilog-ts-auto (&optional inject)
  "Expand AUTO statements in `verilog-ts-mode'.

Workaround to run `verilog-auto' in `verilog-ts-mode' avoiding `syntax-ppss'
errors:
- https://github.com/gmlarumbe/verilog-ext/issues/12

Injection if appropriate if INJECT is non-nil."
  (interactive)
  (unwind-protect
      (progn
        (verilog-mode)
        (verilog-auto inject)
        (verilog-ts-mode))
    (verilog-ts-mode)))

(defun verilog-ts-delete-auto ()
  "Delete the automatic outputs, regs, and wires created by \\[verilog-ts-auto].
Use \\[verilog-ts-auto] to re-insert the updated AUTOs.

Workaround to run `verilog-delete-auto' in `verilog-ts-mode' avoiding
`syntax-ppss' errors:
- https://github.com/gmlarumbe/verilog-ext/issues/12

See `verilog-delete-auto' for more information."
  (interactive)
  (unwind-protect
      (progn
        (verilog-mode)
        (verilog-delete-auto)
        (verilog-ts-mode))
    (verilog-ts-mode)))


;;; Major-mode
;;;; Setup
;;;###autoload
(defun verilog-ts-install-grammar ()
  "Install SystemVerilog tree-sitter grammar.

This command requires Git, a C compiler and (sometimes) a C++ compiler,
and the linker to be installed and on PATH."
  (interactive)
  (let ((url "https://github.com/gmlarumbe/tree-sitter-systemverilog"))
    (add-to-list 'treesit-language-source-alist `(systemverilog ,url))
    (treesit-install-language-grammar 'systemverilog)))


;;;; Features
(defvar-keymap verilog-ts-mode-map
  :doc "Keymap for SystemVerilog language with tree-sitter"
  :parent verilog-mode-map
  "TAB"     #'indent-for-tab-command
  "C-M-f"   #'verilog-ts-forward-sexp
  "C-M-b"   #'verilog-ts-backward-sexp
  "C-M-a"   #'beginning-of-defun ; Unmap `verilog-beg-of-defun' from `verilog-mode'
  "C-M-e"   #'end-of-defun       ; Unmap `verilog-end-of-defun' from `verilog-mode'
  "C-M-h"   #'mark-defun         ; Unmap `verilog-mark-defun' from `verilog-mode'
  "C-M-d"   #'verilog-ts-nav-down-dwim
  "C-M-u"   #'verilog-ts-nav-up-dwim
  "C-M-n"   #'verilog-ts-nav-next-dwim
  "C-M-p"   #'verilog-ts-nav-prev-dwim
  "C-c TAB" #'verilog-ts-pretty-declarations
  "C-c C-o" #'verilog-ts-pretty-expr
  "C-c e n" #'verilog-ts-goto-next-error
  "C-c e p" #'verilog-ts-goto-prev-error
  "C-c C-a" #'verilog-ts-auto
  "C-c C-k" #'verilog-ts-delete-auto)

(defvar verilog-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\"     table)
    (modify-syntax-entry ?+  "."      table)
    (modify-syntax-entry ?-  "."      table)
    (modify-syntax-entry ?=  "."      table)
    (modify-syntax-entry ?%  "."      table)
    (modify-syntax-entry ?<  "."      table)
    (modify-syntax-entry ?>  "."      table)
    (modify-syntax-entry ?&  "."      table)
    (modify-syntax-entry ?|  "."      table)
    (modify-syntax-entry ?`  "."      table)
    (modify-syntax-entry ?_  "_"      table)
    (modify-syntax-entry ?\' "."      table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    table)
  "Syntax table used in Verilog mode buffers.")

;;;###autoload
(define-derived-mode verilog-ts-mode verilog-mode "SystemVerilog"
  "Major mode for editing SystemVerilog files, using tree-sitter library."
  :syntax-table verilog-ts-mode-syntax-table
  ;; Emacs convention expects 'verilog' instead of 'systemverilog' for `verilog-ts-mode':
  ;;  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Language-Grammar.html
  (add-to-list 'treesit-load-name-override-list '(verilog "libtree-sitter-systemverilog" "tree_sitter_systemverilog"))
  ;; Treesit
  (when (treesit-ready-p 'verilog)
    (treesit-parser-create 'verilog)
    ;; Font-lock.
    (setq font-lock-defaults nil) ; Disable `verilog-mode' font-lock/indent config
    (setq-local treesit-font-lock-feature-list
                '((comment string)
                  (keyword operator)
                  (preprocessor type declaration instance system-tf misc error)
                  (punctuation number array)))
    (setq-local treesit-font-lock-settings verilog-ts--font-lock-settings)
    ;; Indent.
    (setq-local indent-line-function nil)
    (setq-local comment-indent-function nil)
    (setq-local treesit-simple-indent-rules verilog-ts--treesit-indent-rules)
    ;; Navigation.
    (setq-local treesit-defun-type-regexp verilog-ts-defun-re)
    ;; Imenu.
    (verilog-ts-imenu-setup)
    ;; Which-func
    (verilog-ts-which-func-setup)
    ;; Completion
    (add-hook 'completion-at-point-functions #'verilog-ts-completion-at-point nil 'local)
    ;; Setup.
    (treesit-major-mode-setup)))


;;; Provide
(provide 'verilog-ts-mode)

;;; verilog-ts-mode.el ends here

