;;; verilog-ts-mode-test-beautify.el --- verilog-ts-mode ERT beautify tests  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Gonzalo Larumbe

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


(defconst verilog-ts-mode-test-beautify-file-list verilog-ts-mode-test-common-file-list)

(defconst verilog-ts-mode-test-prettify-file-list
  (append verilog-ts-mode-test-common-file-list
          (test-hdl-directory-files (file-name-concat verilog-ts-mode-test-files-dir "prettify")
                                    verilog-ts-file-extension-re)
          (remove (lambda (file)
                    (file-name-concat verilog-ts-mode-test-files-dir "veripool")
                    ("indent_analog.v"))
                  (test-hdl-directory-files (file-name-concat verilog-ts-mode-test-files-dir "veripool")
                                            verilog-ts-file-extension-re))))



(defun verilog-ts-mode-test-beautify-file ()
  (verilog-ts-mode)
  (let ((beautify-re (concat "\\(?1:^\\s-*\\." verilog-identifier-re "\\)\\(?2:\\s-*\\)(")))
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
        node node-type)
    ;; Declarations
    (save-excursion
      (goto-char (point-min))
      (while (setq node (verilog-ts-pretty--search-forward 'decl))
        (setq node-type (treesit-node-type node))
        (verilog-ts-pretty-decl--goto-node-start node)
        (when debug
          (message "Removing decl @ line %s / type: %s" (line-number-at-pos (point)) node-type))
        (just-one-space)
        (verilog-ts-pretty-decl--goto-node-end node-type))) ; Move to next declaration
    ;; Expressions
    (save-excursion
      (goto-char (point-min))
      (while (setq node (verilog-ts-pretty--search-forward 'expr))
        (setq node-type (treesit-node-type node))
        (if (verilog-ts--node-has-child-recursive node "<?=")
            (progn
              (verilog-ts-pretty-expr--goto-node-start node)
              (when debug
                (message "Removing expr @ line %s / type: %s" (line-number-at-pos (point)) node-type))
              (just-one-space)
              (verilog-ts-pretty-expr--goto-node-end))
          (forward-line)))))) ; Move to next expression

(defun verilog-ts-mode-test-prettify-file ()
  (verilog-ts-mode)
  (verilog-ts-mode-test-prettify--remove)
  (test-hdl-no-messages
    (verilog-ts-pretty-current-buffer)))


(defun verilog-ts-mode-test-beautify-gen-expected-files ()
  ;; Beautify
  (test-hdl-gen-expected-files :file-list verilog-ts-mode-test-beautify-file-list
                               :dest-dir verilog-ts-mode-test-ref-dir-beautify
                               :out-file-ext "beauty.sv"
                               :fn #'verilog-ts-mode-test-beautify-file)
  (let ((verilog-ts-beautify-instance-extra t))
    (test-hdl-gen-expected-files :file-list verilog-ts-mode-test-beautify-file-list
                                 :dest-dir verilog-ts-mode-test-ref-dir-beautify
                                 :out-file-ext "beauty.extra.sv"
                                 :fn #'verilog-ts-mode-test-beautify-file))
  ;; Prettify
  (test-hdl-gen-expected-files :file-list verilog-ts-mode-test-prettify-file-list
                               :dest-dir verilog-ts-mode-test-ref-dir-beautify
                               :out-file-ext "pretty.sv"
                               :fn #'verilog-ts-mode-test-prettify-file)
  ;; Prettify (without comment aligment)
  (let ((verilog-ts-align-decl-expr-comments nil))
    (test-hdl-gen-expected-files :file-list verilog-ts-mode-test-prettify-file-list
                                 :dest-dir verilog-ts-mode-test-ref-dir-beautify
                                 :out-file-ext "no_comm_align.pretty.sv"
                                 :fn #'verilog-ts-mode-test-prettify-file)))


(ert-deftest beautify ()
  (dolist (file verilog-ts-mode-test-beautify-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-beautify (test-hdl-basename file "beauty.sv"))
                                                         :fn #'verilog-ts-mode-test-beautify-file)
                                  (file-name-concat verilog-ts-mode-test-ref-dir-beautify (test-hdl-basename file "beauty.sv"))))))

(ert-deftest beautify-instance-extra ()
  (let ((verilog-ts-beautify-instance-extra t))
    (dolist (file verilog-ts-mode-test-beautify-file-list)
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-beautify (test-hdl-basename file "beauty.extra.sv"))
                                                           :fn #'verilog-ts-mode-test-beautify-file)
                                    (file-name-concat verilog-ts-mode-test-ref-dir-beautify (test-hdl-basename file "beauty.extra.sv")))))))

(ert-deftest prettify ()
  (dolist (file verilog-ts-mode-test-prettify-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-beautify (test-hdl-basename file "pretty.sv"))
                                                         :fn #'verilog-ts-mode-test-prettify-file)
                                  (file-name-concat verilog-ts-mode-test-ref-dir-beautify (test-hdl-basename file "pretty.sv"))))))

(ert-deftest prettify::no-comment-alignment ()
  (let ((verilog-ts-align-decl-expr-comments nil))
    (dolist (file verilog-ts-mode-test-prettify-file-list)
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-beautify (test-hdl-basename file "no_comm_align.pretty.sv"))
                                                           :fn #'verilog-ts-mode-test-prettify-file)
                                    (file-name-concat verilog-ts-mode-test-ref-dir-beautify (test-hdl-basename file "no_comm_align.pretty.sv")))))))


(provide 'verilog-ts-mode-test-beautify)

;;; verilog-ts-mode-test-beautify.el ends here
