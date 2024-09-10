;;; verilog-ts-mode.el --- Verilog Tree-sitter major mode  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/verilog-ts-mode
;; Version: 0.2.0
;; Keywords: Verilog, IDE, Tools
;; Package-Requires: ((emacs "29.1"))

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
;;
;; Provides tree-sitter based implementations for the following features:
;; - Syntax highlighting
;; - Indentation
;; - `imenu'
;; - `which-func'
;; - Navigation functions
;; - Prettify and beautify
;; - Completion at point
;;
;;
;; Contributions:
;;   This major mode is still under active development!
;;   Check contributing guidelines:
;;     - https://github.com/gmlarumbe/verilog-ts-mode#contributing

;;; Code:

;;; Requirements

(require 'treesit)
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


;;; Utils
;;;; Core
(defconst verilog-ts-identifier-re "[a-zA-Z_][a-zA-Z_0-9]*")
(defconst verilog-ts-identifier-sym-re (concat "\\_<" verilog-ts-identifier-re "\\_>"))
(defconst verilog-ts-instance-re "\\_<\\(module\\|interface\\|program\\|gate\\|udp\\|checker\\)_instantiation\\_>")
(defconst verilog-ts-port-header-ts-re "\\_<\\(variable\\|net\\|interface\\)_port_header\\_>")

(defconst verilog-ts--declaration-node-re
  (eval-when-compile
    (regexp-opt
     '("package_declaration"
       "class_declaration"
       "interface_class_declaration"
       "function_body_declaration"
       "task_body_declaration"
       "function_prototype"
       "task_prototype"
       "checker_declaration"
       "config_declaration")
     'symbols)))


(defun verilog-ts--node-at-point ()
  "Return tree-sitter node at point."
  (treesit-node-at (point) 'verilog))

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
    (treesit-search-subtree node node-type)))

(defun verilog-ts--node-identifier-name (node)
  "Return identifier name of NODE."
  (when node
    (cond (;; module/interface/program
           (string-match "\\_<\\(module\\|interface\\|program\\)_declaration\\_>" (treesit-node-type node))
           (or (treesit-node-text (treesit-node-child-by-field-name node "name") :no-props) ; extern module instantiation
               (treesit-node-text (treesit-node-child-by-field-name (treesit-search-subtree node "\\_<module_\\(non\\)?ansi_header") "name") :no-props)))
          ;; package/class/function/task/checker/config
          ((string-match verilog-ts--declaration-node-re (treesit-node-type node))
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
          ;; default
          (t
           (treesit-node-text (treesit-search-subtree node "\\_<simple_identifier\\_>") :no-prop)))))

(defun verilog-ts--node-identifier-type (node)
  "Return identifier type of NODE."
  (let ((type (treesit-node-type node)))
    (cond (;; Variables
           (string-match "\\_<variable_decl_assignment\\_>" type)
           (treesit-node-text (verilog-ts--node-has-child-recursive (verilog-ts--node-has-parent-recursive node "\\_<data_declaration\\_>") "\\_<data_type\\_>") :no-prop))
          ;; TODO: Still not taking (random_qualifier) sibling of (data_type) into account for rand/randc attributes
          ;; TODO: Still not taking unpacked/queue/array dimensions into account
          (;; Nets
           (string-match "\\_<net_decl_assignment\\_>" type)
           (treesit-node-text (verilog-ts--node-has-child-recursive (verilog-ts--node-has-parent-recursive node "\\_<net_declaration\\_>") "\\_<\\(net_type\\|simple_identifier\\)\\_>") :no-prop))
          (;; Module/interface/program ports
           (string-match "\\_<ansi_port_declaration\\_>" type)
           (treesit-node-text (treesit-search-subtree node verilog-ts-port-header-ts-re) :no-prop))
          (;; Task/function arguments
           (string-match "\\_<tf_port_item\\_>" type)
           (let ((port-direction (treesit-node-text (treesit-search-subtree node "\\_<tf_port_direction\\_>") :no-prop)))
             (concat (when port-direction (concat port-direction " "))
                     (treesit-node-text (treesit-search-subtree node "\\_<data_type_or_implicit\\_>") :no-prop))))
          (;; Typedefs
           (string-match "\\_<type_declaration\\_>" type)
           (treesit-node-text (verilog-ts--node-has-child-recursive (verilog-ts--node-has-parent-recursive node "\\_<data_declaration\\_>") "\\_<\\(class\\|data\\)_type\\_>") :no-prop))
          (t ;; Default
           type))))

(defun verilog-ts--node-instance-name (node)
  "Return identifier name of NODE.

Node must be of type `verilog-ts-instance-re'.  Otherwise return nil."
  (unless (and node (string-match verilog-ts-instance-re (treesit-node-type node)))
    (error "Wrong node type: %s" (treesit-node-type node)))
  (let ((child-node (treesit-search-subtree node "name_of_instance")))
    (treesit-node-text (treesit-node-child-by-field-name child-node "instance_name") :no-props)))

(defun verilog-ts--highest-node-at-pos (pos)
  "Return highest node starting at POS in the parsed tree.

Only might work as expected if point is at the beginning of a symbol.

Snippet fetched from `treesit--indent-1'."
  (let* ((smallest-node (verilog-ts--node-at-point))
         (node (treesit-parent-while
                smallest-node
                (lambda (node)
                  (eq pos (treesit-node-start node))))))
    node))

(defun verilog-ts--highest-node-at-symbol ()
  "Return highest node in the hierarchy for symbol at point.
Check also `treesit-thing-at-point' for similar functionality."
  (verilog-ts--highest-node-at-pos (car (bounds-of-thing-at-point 'symbol))))

(defun verilog-ts--node-at-bol ()
  "Return node at first non-blank character of current line.
Snippet fetched from `treesit--indent-1'."
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
    node))

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

(defun verilog-ts--inside-module-or-interface-p ()
  "Return non-nil if point is inside a module or interface construct."
  (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) "\\_<\\(module\\|interface\\)_declaration\\_>"))

(defun verilog-ts--node-is-typedef-class-p (node)
  "Return non-nil if NODE is a typedef class declaration."
  (let ((type (treesit-node-type node)))
    (and node
         (string-match "\\_<type_declaration\\_>" type)
         (string-match (concat "typedef\\s-+class\\s-+" verilog-ts-identifier-re "\\s-*;")
                       (treesit-node-text node :no-prop)))))

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
       "module_instantiation"
       "interface_instantiation"
       "always_construct"
       "initial_construct"
       "final_construct"
       "generate_region"
       "seq_block")
     'symbols)))


(defun verilog-ts-module-at-point ()
  "Return node of module at point."
  (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) "module_declaration"))

(defun verilog-ts-instance-at-point ()
  "Return node of instance at point."
  (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) verilog-ts-instance-re))

(defun verilog-ts-block-at-point ()
  "Return node of block at point."
  (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) verilog-ts-block-at-point-re))

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



;;;; Navigation
(defun verilog-ts-forward-sexp (&optional arg)
  "Move forward across S-expressions.

With `prefix-arg', move ARG expressions."
  (interactive "p")
  (if (member (following-char) '(?\( ?\{ ?\[))
      (if (and arg (< arg 0))
          (backward-sexp arg)
        (forward-sexp arg))
    (let* ((node (or (verilog-ts--highest-node-at-symbol)
                     (verilog-ts--node-at-point)))
           (beg (treesit-node-start node))
           (end (treesit-node-end node)))
      (if (and arg (< arg 0))
          (goto-char beg)
        (goto-char end)))))

(defun verilog-ts-backward-sexp (&optional arg)
  "Move backward across S-expressions.

With `prefix-arg', move ARG expressions."
  (interactive "p")
  (if (member (preceding-char) '(?\) ?\} ?\]))
      (if (and arg (< arg 0))
          (forward-sexp arg)
        (backward-sexp arg))
    (let* ((node (treesit-node-parent (verilog-ts--node-at-point)))
           (beg (treesit-node-start node))
           (end (treesit-node-end node)))
      (if (and arg (< arg 0))
          (goto-char end)
        (goto-char beg)))))


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

(defvar verilog-ts-font-lock-error-face 'verilog-ts-font-lock-error-face)
(defface verilog-ts-font-lock-error-face
  '((t (:underline (:style wave :color "Red1"))))
  "Face for tree-sitter parsing errors."
  :group 'verilog-ts-faces)


;;;; Keywords
;; There are some keywords that are not recognized by tree-sitter grammar.
;; For these ones, use regexp matching patterns inside tree-sitter (:match "^foo$")
(defconst verilog-ts-keywords
  '("alias" "and" "assert" "assign" "assume" "before" "bind" "binsof" "break"
    "case" "checker" "class" "class" "clocking" "config" "const" "constraint"
    "cover" "covergroup" "coverpoint" "cross" "default" "defparam" "disable"
    "do" "else" "endcase" "endchecker" "endclass" "endclocking" "endconfig"
    "endfunction" "endgenerate" "endgroup" "endinterface" "endmodule"
    "endpackage" "endprogram" "endproperty" "endsequence" "endtask" "enum"
    "extends" "extern" "final" "first_match" "for" "foreach" "forever" "fork"
    "forkjoin" "function" "generate" "genvar" "if" "iff" "illegal_bins"
    "implements" "import" "initial" "inside" "interconnect" "interface"
    "intersect" "join" "join_any" "join_none" "local" "localparam" "modport"
    "new" "null" "option" "or" "package" "packed" "parameter" "program"
    "property" "pure" "randcase" "randomize" "repeat" "return" "sequence"
    "showcancelled" "soft" "solve" "struct" "super" "tagged" "task"
    "timeprecision" "timeunit" "type" "typedef" "union" "unique" "virtual"
    "wait" "while" "with"
    (always_keyword)       ; always, always_comb, always_latch, always_ff
    (bins_keyword)         ; bins, illegal_bins, ignore_bins
    (case_keyword)         ; case, casez, casex
    (class_item_qualifier) ; static, protected, local
    (edge_identifier)      ; posedge, negedge, edge
    (lifetime)             ; static, automatic
    (module_keyword)       ; module, macromodule
    (random_qualifier)     ; rand, randc
    (unique_priority)))    ; unique, unique0, priority

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
  (eval-when-compile
    (mapcar (lambda (elm)
              (concat "directive_" elm))
            '("include" "define" "ifdef" "ifndef" "timescale" "default_nettype"
              "elsif" "undef" "resetall" "undefineall" "endif" "else"
              "unconnected_drive" "celldefine" "endcelldefine" "end_keywords"
              "unconnected_drive" "line" "begin_keywords" "pragma" "__FILE__"
              "__LINE__"))))


;;;; Treesit-settings
(defvar verilog-ts--treesit-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'verilog
   '((comment) @font-lock-comment-face)

   :feature 'string
   :language 'verilog
   '([(string_literal)
      (quoted_string) ; `include strings
      (system_lib_string)]
     @font-lock-string-face)

   :feature 'keyword
   :language 'verilog
   `((["begin" "end" "this"] @verilog-ts-font-lock-grouping-keywords-face)
     ([(port_direction) "ref"] @verilog-ts-font-lock-direction-face) ; Explicitly add "ref" to consider tf_port_direction "const ref"
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
     (["{" "}"] @verilog-ts-font-lock-curly-braces-face)
     (["@" "#" "##"] @verilog-ts-font-lock-time-event-face))

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
     (udp_instantiation ; INFO: UDPs: Adding field to udp_identifier is still buggy in the grammar
      (simple_identifier) @verilog-ts-font-lock-module-face)
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
     (parameter_value_assignment
      (list_of_parameter_value_assignments
       (named_parameter_assignment
        (simple_identifier) @verilog-ts-font-lock-port-connection-face)))
     (ordered_parameter_assignment
      (param_expression
       (data_type
        (simple_identifier) @verilog-ts-font-lock-port-connection-face)))
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
      (select "." (simple_identifier) @verilog-ts-font-lock-dot-name-face))
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
     (("$") @font-lock-constant-face))

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
   '((ERROR) @verilog-ts-font-lock-error-face)))


;;; Indent
;;;; Matchers
(defun verilog-ts--matcher-unit-scope (node parent &rest _)
  "A tree-sitter simple indent matcher.
Matches if point is at $unit scope."
  (let* ((root (treesit-buffer-root-node)))
    (or (treesit-node-eq node root)
        (treesit-node-eq parent root))))

(defun verilog-ts--matcher-blank-line (node &rest _)
  "A tree-sitter simple indent matcher.
Matches if point is at a blank line."
  (unless node
    t))

(defun verilog-ts--matcher-continued-string (node &rest _)
  "A tree-sitter simple indent matcher.
Matches if point is at a continued quoted string."
  (let ((node-at-point (verilog-ts--node-at-point))) ; There is no node-at-bol in this case
    (and (not node)
         (equal (treesit-node-type (treesit-node-parent node-at-point)) "quoted_string"))))

(defun verilog-ts--matcher-uvm-field-macro (&rest _)
  "A tree-sitter simple indent matcher.
Matches if point is at uvm_field_* macro.
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
               (or (eq 0 (string-match "`[ou]vm_field_" node-text))))
      node-text)))

(defun verilog-ts--matcher-default-indent (&rest _)
  "A tree-sitter simple indent matcher.
Always return non-nil."
  t)

(defun verilog-ts--matcher-ansi-port-after-paren (node &rest _)
  "A tree-sitter simple indent matcher.
Return non-nil if the first ansi-port is in the same line as the opening
parenthesis."
  (let* ((indent-node (verilog-ts--node-has-parent-recursive node "list_of_port_declarations"))
         (indent-node-line (line-number-at-pos (treesit-node-start indent-node)))
         (first-port-node (treesit-node-child indent-node 1)) ; ansi_port_declaration
         (first-port-node-line (line-number-at-pos (treesit-node-start first-port-node))))
    (eq indent-node-line first-port-node-line)))

(defun verilog-ts--matcher-parameter-port-after-paren (node &rest _)
  "A tree-sitter simple indent matcher.
Return non-nil if the first parameter is in the same line as the opening
parenthesis."
  (let* ((indent-node (verilog-ts--node-has-parent-recursive node "parameter_port_list"))
         (indent-node-line (line-number-at-pos (treesit-node-start indent-node)))
         (first-port-node (treesit-node-child indent-node 2)) ; parameter_port_declaration
         (first-port-node-line (line-number-at-pos (treesit-node-start first-port-node))))
    (eq indent-node-line first-port-node-line)))

(defun verilog-ts--matcher-continued-parameter-port (node &rest _)
  "A tree-sitter simple indent matcher.
Return non-nil if matches continued declaration of parameter ports.
parameter A = 0,
          B = 1,
          C = 2"
  (let ((child-node (treesit-node-child node 0)))
    (string= (treesit-node-type child-node) "data_type")))

(defun verilog-ts--matcher-unpacked-array-same-line (_node parent &rest _)
  "A tree-sitter simple indent matcher.
Return non-nil if the first unpacked array element is in the same line as the
opening brace."
  (let* ((indent-node-line (line-number-at-pos (treesit-node-start parent)))
         (first-elm-node (treesit-node-child parent 1))
         (first-elm-node-line (line-number-at-pos (treesit-node-start first-elm-node))))
    (eq indent-node-line first-elm-node-line)))

(defun verilog-ts--matcher-import-package-ansi-header (node parent &rest _)
  "A tree-sitter simple indent matcher.
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
  "A tree-sitter simple indent matcher.
Return non-nil if matches continued declaration of list of arguments.

    ret_value = funcall(a, b, c,
                        d, e, f);
"
  (when (and node (string-match "expression" (treesit-node-type node)))
    ;; TODO: Place them in the proper order? Maybe create compound RE?
    (or (verilog-ts--node-has-parent-recursive node "list_of\\(_actual\\)?_arguments")
        (verilog-ts--node-has-parent-recursive node "cond_predicate")
        (verilog-ts--node-has-parent-recursive node "concatenation"))))

(defun verilog-ts--matcher-expression-decl (node &rest _)
  "A tree-sitter simple indent matcher.
Return non-nil if matches continued declaration of list of net or variable
declaration:

    wire [1:0] mux_output0 =
                   select0[0] ? mux_input0 :
                   select0[1] ? mux_input1 :
                   mux_input2;
"
  (when (and node (string-match "expression" (treesit-node-type node)))
    (verilog-ts--node-has-parent-recursive node "list_of_\\(net\\|variable\\)_decl_assignments")))

(defun verilog-ts--matcher-expression-loop (node &rest _)
  "A tree-sitter simple indent matcher.
Return non-nil if matches continued expression of a loop statement condition.

    while (a && b &&
           !c) begin
"
  (when (and node (string-match "expression" (treesit-node-type node)))
    (let ((loop-node (verilog-ts--node-has-parent-recursive node "loop_statement")))
      (and loop-node
           (string= (treesit-node-type (treesit-node-child loop-node 2)) "expression"))))) ; 2 accounts for while (

(defun verilog-ts--matcher-expression-assignment (node &rest _)
  "A tree-sitter simple indent matcher.
Return non-nil if matches continued expression of a loop statement condition.

    valid_c <= (valid &&
                (state != A) &&
                (state != B));
"
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
       (goto-char (treesit-node-start (or (verilog-ts--node-has-parent-recursive node "class_method")
                                          (verilog-ts--node-has-parent-recursive node "task_declaration")))))
      ("endfunction"
       (goto-char (treesit-node-start (or (verilog-ts--node-has-parent-recursive node "class_method")
                                          (verilog-ts--node-has-parent-recursive node "function_declaration")
                                          (verilog-ts--node-has-parent-recursive node "class_constructor_declaration")))))
      ;; default is parent-bol 0
      (_
       (goto-char (treesit-node-start parent))
       (back-to-indentation)
       (point)))))

(defun verilog-ts--anchor-expression-args (node parent &rest _)
  "A tree-sitter simple indent anchor for NODE and PARENT."
  (let (indent-node)
    (save-excursion
      (cond ((setq indent-node (verilog-ts--node-has-parent-recursive node "list_of\\(_actual\\)?_arguments"))
             (goto-char (treesit-node-start indent-node))
             (point))
            ((setq indent-node (verilog-ts--node-has-parent-recursive node "cond_predicate"))
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

(defun verilog-ts--anchor-grandparent (_node parent &rest _)
  "A tree-sitter simple indent anchor for NODE and PARENT.
Find the beginning of node's grandparent.
INFO: Might be present in future Emacs releases.
Check `treesit' and `treesit-simple-indent-presets'."
  (save-excursion
    (goto-char (treesit-node-start (treesit-node-parent parent)))))

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
  (let ((indent-node (or (verilog-ts--node-has-parent-recursive node "class_method")
                         (verilog-ts--node-has-parent-recursive node "task_declaration")
                         (verilog-ts--node-has-parent-recursive node "function_declaration"))))
    (save-excursion
      (if indent-node
          (goto-char (treesit-node-start indent-node))
        (point)))))

(defun verilog-ts--anchor-tf-port-item (node &rest _)
  "A tree-sitter simple indent anchor for NODE.
Indent task/function arguments."
  (let ((indent-node (or (verilog-ts--node-has-parent-recursive node "tf_port_list")
                         (verilog-ts--node-has-parent-recursive node "class_constructor_arg_list"))))
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
  (let* ((node-at-point (verilog-ts--node-at-point))
         (indent-node (verilog-ts--node-has-parent-recursive node-at-point "quoted_string")))
    (save-excursion
      (goto-char (treesit-node-start indent-node))
      (1+ (point))))) ; Take " into account

(defun verilog-ts--anchor-assignment-pattern (_node parent &rest _)
  "A tree-sitter simple indent anchor.
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
;; INFO: Do not use siblings as anchors, since comments could be wrongly detected as siblings!
(defvar verilog-ts--indent-rules
  `((verilog
     ;; Unit scope
     (verilog-ts--matcher-unit-scope verilog-ts--anchor-point-min 0) ; Place first for highest precedence
     ;; Comments
     ((and (node-is "comment")
           verilog-ts--matcher-unit-scope)
      verilog-ts--anchor-point-min 0)
     ((and (node-is "comment")
           (or (parent-is "conditional_statement")
               (parent-is "list_of_port_connections")))
      parent-bol 0)
     ((and (node-is "comment")
           (parent-is "list_of_port_declarations"))
      verilog-ts--anchor-grandparent-bol verilog-ts-indent-level)
     ((node-is "comment") parent-bol verilog-ts-indent-level)
     ;; Procedural
     ((node-is "continuous_assign") parent-bol verilog-ts-indent-level)
     ((node-is "always_construct") parent-bol verilog-ts-indent-level)
     ((node-is "if_generate_construct") parent-bol verilog-ts-indent-level)
     ((node-is "loop_generate_construct") parent-bol verilog-ts-indent-level)
     ((node-is "initial_construct") parent-bol verilog-ts-indent-level)
     ((node-is "statement_or_null") parent-bol verilog-ts-indent-level)
     ((node-is "case_item") parent-bol verilog-ts-indent-level)
     ((node-is "block_item_declaration") parent-bol verilog-ts-indent-level)     ; Procedural local variables declaration
     ((node-is "tf_item_declaration") parent-bol verilog-ts-indent-level)        ; Procedural local variables in tasks declaration
     ((node-is "function_statement_or_null") parent-bol verilog-ts-indent-level) ; Procedural statement in a function
     ((node-is "checker_or_generate_item_declaration") parent-bol verilog-ts-indent-level) ; default disable iff (!rst_ni);
     ((node-is "concurrent_assertion_item") parent-bol verilog-ts-indent-level) ; default disable iff (!rst_ni);
     ((node-is "super") parent-bol verilog-ts-indent-level)
     ;; ANSI Port/parameter declaration
     ((node-is "parameter_port_list") parent-bol 0) ; Open parenthesis in new line (old verilog-mode style)
     ((node-is "list_of_port_declarations") parent-bol verilog-ts-indent-level) ; Open parenthesis in new line (old verilog-mode style)
     ((and (node-is "ansi_port_declaration")
           verilog-ts--matcher-ansi-port-after-paren)
      verilog-ts--anchor-first-ansi-port 0)
     ((node-is "ansi_port_declaration") verilog-ts--anchor-ansi-port verilog-ts-indent-level) ; Fallback of previous rule
     ((node-is "module_or_generate_item") parent-bol verilog-ts-indent-level)
     ((node-is "interface_or_generate_item") parent-bol verilog-ts-indent-level)
     ((node-is "list_of_param_assignments") parent-bol verilog-ts-indent-level) ; First instance parameter (without parameter keyword)
     ((and (node-is "parameter_port_declaration")
           verilog-ts--matcher-continued-parameter-port)
      verilog-ts--anchor-continued-parameter 0)
     ((and (node-is "parameter_port_declaration")
           verilog-ts--matcher-parameter-port-after-paren)
      verilog-ts--anchor-parameter-port 0)
     ((node-is "parameter_port_declaration") parent-bol verilog-ts-indent-level) ; First instance parameter (without parameter keyword)
     ;; import packages
     (verilog-ts--matcher-import-package-ansi-header verilog-ts--anchor-import-package-ansi-header verilog-ts-indent-level)
     ((node-is "package_import_declaration") parent-bol verilog-ts-indent-level)
     ((and (node-is "package_or_generate_item_declaration")
           (parent-is "package_declaration"))
      parent-bol verilog-ts-indent-level)
     ;; Instance port/parameters
     ((node-is "named_port_connection") parent-bol 0)
     ((node-is "ordered_port_connection") parent-bol 0)
     ((node-is "named_parameter_assignment") parent-bol 0)
     ((node-is "ordered_parameter_assignment") parent-bol 0)
     ((and (node-is ".")
           (parent-is "named_port_connection")) ; E.g. ports with `ifdefs
      verilog-ts--anchor-grandparent-bol 0)
     ;; Block end
     ((node-is "end") verilog-ts--anchor-end-indent 0)
     ;; Closing
     ((or (node-is "else")         ; Parent is 'if
          (node-is "join_keyword") ; Parent is 'fork
          (node-is "}")
          (node-is ")")
          (node-is "]"))
      parent-bol 0)
     ;; Opening.
     ((or (node-is "{")
          (node-is "("))
      parent-bol 0)
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
     ((node-is "tf_port_list") verilog-ts--anchor-tf-port-list verilog-ts-indent-level) ; Task ports in multiple lines (first line)
     ((node-is "tf_port_item") verilog-ts--anchor-tf-port-item 0) ; Task ports in multiple lines
     ((node-is "class_constructor_arg_list") verilog-ts--anchor-tf-port-list verilog-ts-indent-level) ; INFO: Similar to "tf_port_list"
     ((node-is "class_constructor_arg") verilog-ts--anchor-tf-port-item 0) ; INFO: Similar to  "tf_port_item"
     ((node-is "constraint_block_item") parent-bol verilog-ts-indent-level)
     ((node-is "enum_name_declaration") parent-bol verilog-ts-indent-level)
     ((node-is "generate_region") parent-bol verilog-ts-indent-level)
     ((node-is "hierarchical_instance") parent-bol 0) ; Instance name in separate line
     ((node-is "constraint_expression") parent-bol verilog-ts-indent-level) ; Instance name in separate line
     ((node-is "bins_or_options") verilog-ts--anchor-coverpoint-bins verilog-ts-indent-level) ; Instance name in separate line
     ((node-is "cross_body_item") verilog-ts--anchor-cross-bins verilog-ts-indent-level) ; Instance name in separate line
     ((node-is "dist_list") parent-bol verilog-ts-indent-level) ; Instance name in separate line
     ((node-is "dist_item") verilog-ts--anchor-grandparent-bol verilog-ts-indent-level) ; Instance name in separate line
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
     ((node-is "variable_decl_assignment") parent 0)
     ((node-is "param_assignment") parent 0)
     ((node-is "module_ansi_header") parent-bol 0) ; Opening bracket of module ports/parmeters
     ;; Arguments
     ((node-is "actual_argument") parent 0)
     ;; Blank lines and continued strings
     (verilog-ts--matcher-continued-string verilog-ts--anchor-continued-string 0)
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

(defun verilog-ts-imenu-format-item-label (type name)
  "Return Imenu label for single node using TYPE and NAME."
  (format "%s (%s)" name type))

(defun verilog-ts-imenu-format-parent-item-label (type name)
  "Return Imenu label for parent node using TYPE and NAME."
  (format "%s..." (verilog-ts-imenu-format-item-label type name)))

(defun verilog-ts-imenu-format-parent-item-jump-label (type _name)
  "Return Imenu label for parent node jump using TYPE and NAME."
  (cond ((eq type 'cls)
         "*class definition*")
        ((eq type 'fun)
         "*function definition*")
        ((eq type 'task)
         "*task definition*")
        (t
         (format "%s" type))))

(defun verilog-ts-imenu-treesit-create-index-1 (node)
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
         (subtrees (mapcan #'verilog-ts-imenu-treesit-create-index-1
                           children))
         (type (pcase (treesit-node-type ts-node)
                 ("module_declaration"            'mod)
                 ("interface_declaration"         'itf)
                 ("program_declaration"           'pgm)
                 ("package_declaration"           'pkg)
                 ("class_declaration"             'cls)
                 ("class_property"                'prop)
                 ("function_declaration"          'fun)
                 ("task_declaration"              'task)
                 ("class_constructor_declaration" 'new)
                 ("function_prototype"            'fun)
                 ("task_prototype"                'task)
                 ("class_constructor_prototype"   'new)
                 ("module_instantiation"          'inst)
                 ("always_construct"              'always)
                 ("generate_region"               'gen)))
         ;; The root of the tree could have a nil ts-node.
         (name (when ts-node
                 (verilog-ts--node-identifier-name ts-node)))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond
     ((null ts-node)
      subtrees)
     (subtrees
      (let ((parent-label (funcall verilog-ts-imenu-format-parent-item-label-function
                                   type
                                   name))
            (jump-label (funcall verilog-ts-imenu-format-parent-item-jump-label-function
                                 type
                                 name)))
        `((,parent-label
           ,(cons jump-label marker)
           ,@subtrees))))
     (t (let ((label (funcall verilog-ts-imenu-format-item-label-function
                              type
                              name)))
          (list (cons label marker)))))))

(defun verilog-ts-imenu-create-index (&optional node)
  "Imenu index builder function for `verilog-ts-mode'.

NODE is the root node of the subtree you want to build an index
of.  If nil, use the root node of the whole parse tree.

Copied from Python's `python-imenu-treesit-create-index' and adapted
to SystemVerilog parser."
  (let* ((node (or node (treesit-buffer-root-node 'verilog)))
         (tree (treesit-induce-sparse-tree
                node
                verilog-ts-imenu-create-index-re
                nil 1000)))
    (verilog-ts-imenu-treesit-create-index-1 tree)))

(defun verilog-ts--defun-name (node)
  "Return the defun name of NODE.

Return nil if there is no name or if NODE is not a defun node."
  (verilog-ts--node-identifier-name node))


;;; Which-func
(defvar-local verilog-ts-which-func-extra nil
  "Variable to hold extra information for `which-func'.")

(defun verilog-ts-which-func-shorten-block (block-type)
  "Return shortened name of BLOCK-TYPE, if possible."
  (cond ((string= "function_declaration"          block-type) "func")
        ((string= "class_constructor_declaration" block-type) "func")
        ((string= "task_declaration"              block-type) "task")
        ((string= "class_declaration"             block-type) "cls")
        ((string= "module_declaration"            block-type) "mod")
        ((string= "interface_declaration"         block-type) "itf")
        ((string= "package_declaration"           block-type) "pkg")
        ((string= "program_declaration"           block-type) "pgm")
        ((string= "generate_region"               block-type) "gen")
        ((string= "class_property"                block-type) "prop")
        (t block-type)))

(defun verilog-ts-which-func-function ()
  "Retrieve `which-func' candidates."
  (let ((node (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) verilog-ts-imenu-create-index-re)))
    (when node
      (setq verilog-ts-which-func-extra (verilog-ts--node-identifier-name node))
      (verilog-ts-which-func-shorten-block (treesit-node-type node)))))

(defun verilog-ts-which-func ()
  "Hook for `verilog-ts-mode' to enable `which-func'."
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


;;; Navigation
(defconst verilog-ts--defun-type-regexp
  (eval-when-compile
    (regexp-opt
     '("module_declaration"
       "interface_declaration"
       "program_declaration"
       "package_declaration"
       "class_declaration"
       "class_method"
       "class_constructor_declaration"
       "function_declaration"
       "task_declaration")
     'symbols)))

(defconst verilog-ts--function-task-regexp
  (eval-when-compile
    (regexp-opt
     '("task_declaration"
       "function_declaration"
       "class_method" ; "class_method" includes constructor and extern prototypes
       "class_constructor_declaration")
     'symbols)))

(defconst verilog-ts--function-task-class-regexp
  (eval-when-compile
    (regexp-opt
     '("class_declaration"
       "task_declaration"
       "function_declaration"
       "class_method"
       "class_constructor_declaration")
     'symbols)))

(defun verilog-ts-find-function-task (&optional bwd)
  "Search for a Verilog function/task declaration or definition.

If optional arg BWD is non-nil, search backwards."
  (treesit-search-forward-goto (verilog-ts--node-at-point) verilog-ts--function-task-regexp t bwd))

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
  (treesit-search-forward-goto (verilog-ts--node-at-point) verilog-ts--function-task-class-regexp t bwd))

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

(defun verilog-ts-goto-begin-up ()
  "Move point to start position of current begin."
  (let* ((begin-node (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) "seq_block"))
         (begin-pos (treesit-node-start begin-node)))
    (when begin-pos
      (goto-char begin-pos))))

(defun verilog-ts-goto-begin-down ()
  "Move point to start position of next nested begin."
  (let* ((begin-node (verilog-ts--node-has-child-recursive (verilog-ts--node-at-point) "seq_block"))
         (begin-pos (treesit-node-start begin-node)))
    (when begin-pos
      (goto-char begin-pos))))

(defun verilog-ts-defun-level-up ()
  "Move up one defun-level."
  (let* ((node (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) verilog-ts--defun-type-regexp))
         (pos (treesit-node-start node)))
    (when pos
      (goto-char pos))))

(defun verilog-ts-defun-level-down ()
  "Move down one defun-level."
  (let* ((node (verilog-ts--node-has-child-recursive (verilog-ts--node-at-point) verilog-ts--defun-type-regexp))
         (pos (treesit-node-start node)))
    (when pos
      (goto-char pos))))

(defun verilog-ts-goto-next-error ()
  "Move point to next error in the parse tree."
  (interactive)
  (treesit-search-forward-goto (verilog-ts--node-at-point) "ERROR" t))

(defun verilog-ts-goto-prev-error ()
  "Move point to previous error in the parse tree."
  (interactive)
  (treesit-search-forward-goto (verilog-ts--node-at-point) "ERROR" t :bwd))


;;;; Dwim
(defconst verilog-ts-beg-of-defun-dwim-inside-module-re
  (eval-when-compile
    (regexp-opt
     '("module_declaration"
       "interface_declaration"
       "always_construct"
       "initial_construct"
       "final_construct"
       "generate_region")
     'symbols)))
(defconst verilog-ts-beg-of-defun-dwim-outside-module-re
  (eval-when-compile
    (regexp-opt
     '("function_declaration"
       "task_declaration"
       "class_declaration"
       "package_declaration"
       "program_declaration")
     'symbols)))

(defun verilog-ts-beg-of-defun-dwim ()
  "Context based search upwards.
If in a module/interface look for instantiations.
Otherwise look for functions/tasks."
  (interactive)
  (if (verilog-ts--inside-module-or-interface-p)
      (treesit-search-forward-goto (verilog-ts--node-at-point) verilog-ts-beg-of-defun-dwim-inside-module-re t :bwd)
    (treesit-search-forward-goto (verilog-ts--node-at-point) verilog-ts-beg-of-defun-dwim-outside-module-re t :bwd)))

(defun verilog-ts-end-of-defun-dwim ()
  "Context based search upwards.
If in a module/interface look for instantiations.
Otherwise look for functions/tasks."
  (interactive)
  (if (verilog-ts--inside-module-or-interface-p)
      (treesit-search-forward-goto (verilog-ts--node-at-point) verilog-ts-beg-of-defun-dwim-inside-module-re t)
    (treesit-search-forward-goto (verilog-ts--node-at-point) verilog-ts-beg-of-defun-dwim-outside-module-re t)))

(defun verilog-ts-nav-down-dwim ()
  "Context based search downwards.
If in a module/interface look for instantiations.
Otherwise look for functions, tasks and classes."
  (interactive)
  (if (verilog-ts--inside-module-or-interface-p)
      (verilog-ts-find-module-instance-fwd)
    (verilog-ts-find-function-task-class-fwd)))

(defun verilog-ts-nav-up-dwim ()
  "Context based search upwards.
If in a module/interface look for instantiations.
Otherwise look for functions, tasks and classes."
  (interactive)
  (if (verilog-ts--inside-module-or-interface-p)
      (verilog-ts-find-module-instance-bwd)
    (verilog-ts-find-function-task-class-bwd)))

(defun verilog-ts-nav-next-dwim ()
  "Context based search next.
If in a parenthesis, go to closing parenthesis (Elisp like).
Otherwise move to next paragraph."
  (interactive)
  (if (or (member (following-char) '(?\( ?\[ ?\{ ?\) ?\] ?\}))
          (member (preceding-char) '(?\) ?\] ?\}))
          (string= (symbol-at-point) "begin"))
      (verilog-ts-forward-sexp)
    (forward-paragraph)))

(defun verilog-ts-nav-prev-dwim ()
  "Context based search previous.
If in a parenthesis, go to opening parenthesis (Elisp like).
Otherwise move to previous paragraph."
  (interactive)
  (if (or (member (following-char) '(?\( ?\[ ?\{ ?\) ?\] ?\}))
          (member (preceding-char) '(?\) ?\] ?\}))
          (string= (symbol-at-point) "end"))
      (verilog-ts-backward-sexp)
    (backward-paragraph)))



;;; Prettify
(defconst verilog-ts-pretty-declarations-node-re "list_of_\\(net\\|variable\\)_decl_assignments")
(defconst verilog-ts-pretty-expr-node-re "\\(non\\)?blocking_assignment")

(defun verilog-ts-pretty-declarations ()
  "Line up declarations around point."
  (interactive)
  (let* ((decl-node-re verilog-ts-pretty-declarations-node-re)
         (nodes (verilog-ts-nodes-block-at-point decl-node-re))
         ;; TODO: Fix this!
         ;; (nodes (verilog-ts-children-block-at-point (lambda (child)
         ;;                                              (string= (treesit-node-type child) decl-node-re))))
         (node-lines (mapcar (lambda (node)
                               (line-number-at-pos (treesit-node-start node)))
                             nodes))
         (indent-levels (mapcar (lambda (node)
                                  (save-excursion
                                    (goto-char (treesit-node-start node))
                                    (skip-chars-backward " \t\n\r")
                                    (forward-char)
                                    (current-column)))
                                nodes))
         (indent-level-max (when indent-levels
                             (apply #'max indent-levels)))
         current-node)
    ;; Start processing
    (when nodes
      (save-excursion
        (dolist (line node-lines )
          (goto-char (point-min))
          (forward-line (1- line))
          (setq current-node (verilog-ts-search-node-block-at-point decl-node-re))
          (goto-char (treesit-node-start current-node))
          (just-one-space)
          (indent-to indent-level-max))))))

(defun verilog-ts-pretty-expr ()
  "Line up expressions around point."
  (interactive)
  (let* ((decl-node-re verilog-ts-pretty-expr-node-re)
         (align-node-re "variable_lvalue")
         ;; TODO: Fix this!
         (nodes (verilog-ts-nodes-block-at-point decl-node-re))
         (node-lines (mapcar (lambda (node)
                               (line-number-at-pos (treesit-node-start node)))
                             nodes))
         (indent-levels (mapcar (lambda (node)
                                  (let ((lhs-node (verilog-ts--node-has-child-recursive node align-node-re)))
                                    (save-excursion
                                      (goto-char (treesit-node-end lhs-node))
                                      (forward-char)
                                      (current-column))))
                                nodes))
         (indent-level-max (when indent-levels
                             (apply #'max indent-levels)))
         current-node)
    ;; Start processing
    (when nodes
      (save-excursion
        (dolist (line node-lines)
          (goto-char (point-min))
          (forward-line (1- line))
          (setq current-node (verilog-ts-search-node-block-at-point align-node-re))
          (goto-char (treesit-node-end current-node))
          (just-one-space)
          (indent-to indent-level-max))))))

;;; Beautify
(defun verilog-ts-beautify-block-at-point ()
  "Beautify/indent block at point.

If block is an instance, also align parameters and ports."
  (interactive)
  (let ((node (verilog-ts-block-at-point))
        start end type name)
    (unless node
      (user-error "Not inside a block"))
    (setq start (treesit-node-start node))
    (setq end (treesit-node-end node))
    (setq type (treesit-node-type node))
    (setq name (verilog-ts--node-identifier-name node))
    (indent-region start end)
    ;; Instance: also align ports and params
    (when (string-match verilog-ts-instance-re type)
      (let ((re "\\(\\s-*\\)(")
            params-node ports-node)
        (setq node (verilog-ts-block-at-point)) ; Refresh outdated node after `indent-region'
        (when (setq params-node (verilog-ts--node-has-child-recursive node "list_of_parameter_value_assignments"))
          (align-regexp (treesit-node-start params-node) (treesit-node-end params-node) re 1 1 nil))
        (setq node (verilog-ts-block-at-point)) ; Refresh outdated node after `align-regexp' for parameter list
        (when (setq ports-node (verilog-ts--node-has-child-recursive node "list_of_port_connections"))
          (align-regexp (treesit-node-start ports-node) (treesit-node-end ports-node) re 1 1 nil))))
    (message "%s : %s" type name)))

(defun verilog-ts-beautify-current-buffer ()
  "Beautify current buffer:
- Indent whole buffer
- Beautify every instantiated module
- Untabify and delete trailing whitespace"
  (interactive)
  (let (node)
    (indent-region (point-min) (point-max))
    (save-excursion
      (goto-char (point-min))
      (while (setq node (treesit-search-forward (verilog-ts--node-at-point) verilog-ts-instance-re))
        (goto-char (treesit-node-start node))
        (verilog-ts-beautify-block-at-point)
        (setq node (treesit-search-forward (verilog-ts--node-at-point) verilog-ts-instance-re))
        (goto-char (treesit-node-end node))
        (when (not (eobp))
          (forward-char))))
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace (point-min) (point-max))))

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
  (verilog-mode)
  (verilog-auto inject)
  (verilog-ts-mode))


;;; Major-mode
;;;; Setup
;;;###autoload
(defun verilog-ts-install-grammar ()
  "Install Verilog tree-sitter grammar.

This command requires Git, a C compiler and (sometimes) a C++ compiler,
and the linker to be installed and on PATH."
  (interactive)
  (let ((url "https://github.com/gmlarumbe/tree-sitter-systemverilog"))
    (add-to-list 'treesit-language-source-alist `(verilog ,url))
    (treesit-install-language-grammar 'verilog)))


;;;; Features
(defvar-keymap verilog-ts-mode-map
  :doc "Keymap for SystemVerilog language with tree-sitter"
  :parent verilog-mode-map
  "TAB"     #'indent-for-tab-command
  "C-M-f"   #'verilog-ts-forward-sexp
  "C-M-b"   #'verilog-ts-backward-sexp
  "C-M-a"   #'verilog-ts-beg-of-defun-dwim
  "C-M-e"   #'verilog-ts-end-of-defun-dwim
  "C-M-d"   #'verilog-ts-nav-down-dwim
  "C-M-u"   #'verilog-ts-nav-up-dwim
  "C-M-n"   #'verilog-ts-nav-next-dwim
  "C-M-p"   #'verilog-ts-nav-prev-dwim
  "C-c TAB" #'verilog-ts-pretty-declarations
  "C-c C-o" #'verilog-ts-pretty-expr
  "C-c e n" #'verilog-ts-goto-next-error
  "C-c e p" #'verilog-ts-goto-prev-error
  "C-c C-a" #'verilog-ts-auto)

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
  ;; Treesit
  (when (treesit-ready-p 'verilog)
    (treesit-parser-create 'verilog)
    ;; Font-lock.
    (setq font-lock-defaults nil) ; Disable `verilog-mode' font-lock/indent config
    (setq-local treesit-font-lock-feature-list
                '((comment string)
                  (keyword operator)
                  (preprocessor punctuation type declaration instance number array system-tf misc)
                  (error)))
    (setq-local treesit-font-lock-settings verilog-ts--treesit-settings)
    ;; Indent.
    (setq-local indent-line-function nil)
    (setq-local comment-indent-function nil)
    (setq-local treesit-simple-indent-rules verilog-ts--indent-rules)
    ;; Navigation.
    (setq-local treesit-defun-type-regexp verilog-ts--defun-type-regexp)
    ;; Imenu.
    (setq-local imenu-create-index-function #'verilog-ts-imenu-create-index)
    ;; Which-func
    (verilog-ts-which-func)
    ;; Completion
    (add-hook 'completion-at-point-functions #'verilog-ts-completion-at-point nil 'local)
    ;; Setup.
    (treesit-major-mode-setup)))


;;; Provide
(provide 'verilog-ts-mode)

;;; verilog-ts-mode.el ends here

