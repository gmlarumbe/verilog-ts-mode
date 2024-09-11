;;; verilog-ts-mode-test-beautify.el --- verilog-ts-mode ERT beautify tests  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/test-hdl

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
;;
;; verilog-ts-mode ERT beautify tests
;;
;;; Code:


(defconst verilog-ts-mode-test-ref-dir-beautify (file-name-concat verilog-ts-mode-test-ref-dir "beautify"))
(defconst verilog-ts-mode-test-dump-dir-beautify (file-name-concat verilog-ts-mode-test-dump-dir "beautify"))


(defconst verilog-ts-mode-test-beautify-file-list (mapcar (lambda (file)
                                                            (file-name-concat verilog-ts-mode-test-files-common-dir file))
                                                          '("axi_demux.sv" "instances.sv" "ucontroller.sv")))

;; TODO: At some point replace with `verilog-ts-mode-test-common-file-list'
;; - When axi_test.sv does not give errors
(defconst verilog-ts-mode-test-prettify-file-list
  (append (mapcar (lambda (file)
                    (file-name-concat verilog-ts-mode-test-files-common-dir file))
                  '("axi_demux.sv" "instances.sv" "ucontroller.sv" "tb_program.sv"))
          ;; verilog-ts-mode-test-common-file-list
          (test-hdl-directory-files (file-name-concat verilog-ts-mode-test-files-dir "prettify")
                                    verilog-ts-file-extension-re)))



(defun verilog-ts-mode-test-beautify-file ()
  (verilog-ts-mode)
  (let ((beautify-re (concat "\\(?1:^\\s-*\\." verilog-ts-identifier-re "\\)\\(?2:\\s-*\\)(")))
    ;; Clean blanks in ports (similar to `verilog-ext-replace-regexp-whole-buffer')
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward beautify-re nil t)
        (replace-match "\\1(")))
    ;; Run beautify function
    (test-hdl-no-messages
      (verilog-ts-beautify-current-buffer))))

(defun verilog-ts-mode-test-prettify--remove ()
  (let ((debug nil)
        node)
    ;; Declarations
    (save-excursion
      (goto-char (point-min))
      (while (setq node (treesit-search-forward (verilog-ts--node-at-point) verilog-ts-pretty-declarations-node-re))
        (goto-char (treesit-node-start node))
        (when debug
          (message "Removing decl @ line %s" (line-number-at-pos (point))))
        (just-one-space)
        ;; Move to next declaration
        (goto-char (treesit-node-end (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) "\\_<\\(net\\|data\\)_declaration\\_>")))))
    ;; Expressions
    (save-excursion
      (goto-char (point-min))
      (while (setq node (treesit-search-forward (verilog-ts--node-at-point) verilog-ts-pretty-expr-node-re))
        (goto-char (treesit-node-end (verilog-ts--node-has-child-recursive node "\\_<variable_lvalue\\_>")))
        (when debug
          (message "Removing expr @ line %s" (line-number-at-pos (point))))
        (just-one-space)
        ;; Move to next expression
        (goto-char (treesit-node-end (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) "\\(statement_or_null\\|\\(non\\)?blocking_assignment\\)")))))))

(defun verilog-ts-mode-test-prettify-file ()
  (let ((debug nil)
        node)
    (verilog-ts-mode)
    (verilog-ts-mode-test-prettify--remove)
    ;; Declarations
    (save-excursion
      (goto-char (point-min))
      (while (setq node (treesit-search-forward (verilog-ts--node-at-point) verilog-ts-pretty-declarations-node-re))
        (goto-char (treesit-node-start node))
        (when debug
          (message "Prettifying decl @ line %s" (line-number-at-pos (point))))
        (verilog-ts-pretty-declarations)
        ;; Move to next declaration
        (goto-char (treesit-node-end (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) "\\_<\\(net\\|data\\)_declaration\\_>")))))
    ;; Expressions
    (save-excursion
      (goto-char (point-min))
      (while (setq node (treesit-search-forward (verilog-ts--node-at-point) verilog-ts-pretty-expr-node-re))
        (goto-char (treesit-node-start node))
        (when debug
          (message "Prettifying expr @ line %s" (line-number-at-pos (point))))
        (verilog-ts-pretty-expr)
        ;; Move to next expression
        (goto-char (treesit-node-end (verilog-ts--node-has-parent-recursive (verilog-ts--node-at-point) "\\(statement_or_null\\|\\(non\\)?blocking_assignment\\)")))))))


(defun verilog-ts-mode-test-beautify-gen-expected-files ()
  ;; Beautify
  (test-hdl-gen-expected-files :file-list verilog-ts-mode-test-beautify-file-list
                               :dest-dir verilog-ts-mode-test-ref-dir-beautify
                               :out-file-ext "beauty.sv"
                               :fn #'verilog-ts-mode-test-beautify-file)
  ;; Prettify
  (test-hdl-gen-expected-files :file-list verilog-ts-mode-test-prettify-file-list
                               :dest-dir verilog-ts-mode-test-ref-dir-beautify
                               :out-file-ext "pretty.sv"
                               :fn #'verilog-ts-mode-test-prettify-file))


(ert-deftest beautify ()
  (dolist (file verilog-ts-mode-test-beautify-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-beautify (test-hdl-basename file "beauty.sv"))
                                                         :fn #'verilog-ts-mode-test-beautify-file)
                                  (file-name-concat verilog-ts-mode-test-ref-dir-beautify (test-hdl-basename file "beauty.sv"))))))

(ert-deftest prettify ()
  (dolist (file verilog-ts-mode-test-prettify-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-beautify (test-hdl-basename file "pretty.sv"))
                                                         :fn #'verilog-ts-mode-test-prettify-file)
                                  (file-name-concat verilog-ts-mode-test-ref-dir-beautify (test-hdl-basename file "pretty.sv"))))))


(provide 'verilog-ts-mode-test-beautify)

;;; verilog-ts-mode-test-beautify.el ends here
