;;; verilog-ts-mode-test-utils.el --- verilog-ts-mode ERT utils tests  -*- lexical-binding: t -*-

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
;; verilog-ts-mode ERT utils tests
;;
;;; Code:


(defconst verilog-ts-mode-test-ref-dir-utils (file-name-concat verilog-ts-mode-test-ref-dir "utils"))
(defconst verilog-ts-mode-test-dump-dir-utils (file-name-concat verilog-ts-mode-test-dump-dir "utils"))

(defconst verilog-ts-mode-test-utils-block-at-point-file-and-pos
  `((,(file-name-concat verilog-ts-mode-test-files-common-dir "ucontroller.sv") 839 840 988 1288 2699 4873 4874 4888)
    (,(file-name-concat verilog-ts-mode-test-files-common-dir "instances.sv") 820 826 906 1423 1623 1627 1634 1635 1764 1984 1995 1996 2632 2810 2819 2820)
    (,(file-name-concat verilog-ts-mode-test-files-common-dir "tb_program.sv") 855 975 1287 1619 1781 1866 1889 2029 3436 3459 3495 3515 3541 3643 4343 4556 4634 4635 4645)
    (,(file-name-concat verilog-ts-mode-test-files-common-dir "uvm_component.svh") 1790 1840 1841 2747 7601 7602 29886 30879 31499 32030 58460 58668 59430 63161 76134 76584 76623 76638 100707 100752 100839)
    (,(file-name-concat verilog-ts-mode-test-files-common-dir "axi_test.sv") 954 1074 1245 1386 1433 77731 77760 77772 78713 79365 79753 82960 86883 86893)))

(defconst verilog-ts-mode-test-utils-module-at-point-file-and-pos
  `((,(file-name-concat verilog-ts-mode-test-files-common-dir "ucontroller.sv") 335 833 834 856 857 1345 2331 2608 3129 3483 3939 4169 4639 4863 4865 4887)
    (,(file-name-concat verilog-ts-mode-test-files-common-dir "instances.sv") 1 690 819 820 837 838 1182 1355 1636 1692 1911 1999 2809 2811)
    (,(file-name-concat verilog-ts-mode-test-files-common-dir "tb_program.sv") 465 855 856 885 1219 4607 4635 4657 4658)
    (,(file-name-concat verilog-ts-mode-test-files-common-dir "uvm_component.svh") 526 1128 1790 3025 28721 86490 93485 98428 100752)
    (,(file-name-concat verilog-ts-mode-test-files-common-dir "axi_test.sv") 62 882 883 936 1074 2597 5924 7945 9876 14175 18417 25888 32247 38877 41513 77708 77812 78095 78401 78941 79881 85928 86861 86884 86893 86894)
    (,(file-name-concat verilog-ts-mode-test-files-common-dir "axi_demux.sv") 1833 1869 1888 2231 3066 4183 5183 13228 27844 28709 28710 28838 29284 29879 31415 32706 32814 32820 32829 32830 32878 32905 32930 34046 34562 36282 36664)))

(defconst verilog-ts-mode-test-utils-instance-at-point-file-and-pos
  `((,(file-name-concat verilog-ts-mode-test-files-common-dir "instances.sv") 819 838 906 960 1076 1130 1208 1300 1355 1462 1552 1607 1692 1692 1705 1955 1956 2017 2021 2065 2066 2103 2254 2314 2368 2405 2515 2516 2602 2730 2808 2821)
    (,(file-name-concat verilog-ts-mode-test-files-common-dir "ucontroller.sv") 833 1072 1530 2334 2335 2346 2539 2975 2999 3000 3112 3204 3337 3768 3939 4122 4399 4592 4722 4862 4888)))

(defconst verilog-ts-mode-test-utils-identifier-file-list (mapcar (lambda (file)
                                                                    (file-name-concat verilog-ts-mode-test-files-common-dir file))
                                                                  '("instances.sv"
                                                                    "ucontroller.sv"
                                                                    "axi_demux.sv"
                                                                    "tb_program.sv"
                                                                    "uvm_component.svh")))

(defconst verilog-ts-mode-test-utils-identifier-ts-re
  (regexp-opt
   '(;; Declarations
     "module_declaration"
     "interface_declaration"
     "program_declaration"
     "package_declaration"
     "class_declaration"
     "interface_class_declaration"
     "class_constructor_declaration"
     "checker_declaration"
     "config_declaration"
     "task_declaration"
     "function_declaration"
     "function_body_declaration"
     "task_body_declaration"
     "parameter_declaration"
     "local_parameter_declaration"
     "ansi_port_declaration"
     "type_declaration"
     ;; Instantiation
     "module_instantiation"
     "interface_instantiation"
     "program_instantiation"
     "gate_instantiation"
     "udp_instantiation"
     "checker_instantiation"
     ;; Others
     "variable_decl_assignment"
     "net_decl_assignment"
     "class_property"
     "class_constructor_prototype"
     "tf_port_item")
   'symbols))


(defun verilog-ts-mode-test-utils-block-at-point-fn ()
  (treesit-node-type (verilog-ts-block-at-point)))

(defun verilog-ts-mode-test-utils-instance-at-point-fn ()
  (let ((node (verilog-ts-instance-at-point)))
    (when node
      `(,(verilog-ts--node-identifier-name node)
        ,(verilog-ts--node-instance-name node)))))

(defun verilog-ts-mode-test-utils-module-at-point-fn ()
  (let ((node (verilog-ts-module-at-point)))
    (when node
      (verilog-ts--node-identifier-name node))))

(defun verilog-ts-mode-test-utils-identifier-fn ()
  (let (node ret-value)
    (goto-char (point-min))
    (while (setq node (treesit-search-forward-goto (verilog-ts--node-at-point) verilog-ts-mode-test-utils-identifier-ts-re))
      (push `(,(verilog-ts--node-identifier-name node)
              ,(verilog-ts--node-identifier-type node))
            ret-value))
    (reverse ret-value)))


(defun verilog-ts-mode-test-utils-gen-expected-files ()
  ;; Block at point
  (dolist (file-and-pos verilog-ts-mode-test-utils-block-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir verilog-ts-mode-test-ref-dir-utils
                                   :out-file-ext "block.at.point.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode verilog-ts-mode
                                           :fn verilog-ts-mode-test-utils-block-at-point-fn
                                           :pos-list ,pos-list))))
  ;; Instance at point
  (dolist (file-and-pos verilog-ts-mode-test-utils-instance-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir verilog-ts-mode-test-ref-dir-utils
                                   :out-file-ext "inst.point.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode verilog-mode
                                           :fn verilog-ts-mode-test-utils-instance-at-point-fn
                                           :pos-list ,pos-list))))
  ;; Module at point
  (dolist (file-and-pos verilog-ts-mode-test-utils-module-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir verilog-ts-mode-test-ref-dir-utils
                                   :out-file-ext "mod.point.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode verilog-mode
                                           :fn verilog-ts-mode-test-utils-module-at-point-fn
                                           :pos-list ,pos-list))))
  ;; Identifier name and type
  (dolist (file verilog-ts-mode-test-utils-identifier-file-list)
    (test-hdl-gen-expected-files :file-list `(,file)
                                 :dest-dir verilog-ts-mode-test-ref-dir-utils
                                 :out-file-ext "identifier.el"
                                 :process-fn 'eval
                                 :fn #'verilog-ts-mode-test-utils-identifier-fn)))


(ert-deftest utils::block-at-point ()
  (dolist (file-and-pos verilog-ts-mode-test-utils-block-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-utils (test-hdl-basename file "block.at.point.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode verilog-ts-mode
                                                                   :fn verilog-ts-mode-test-utils-block-at-point-fn
                                                                   :pos-list ,pos-list))
                                    (file-name-concat verilog-ts-mode-test-ref-dir-utils (test-hdl-basename file "block.at.point.el")))))))

(ert-deftest utils::instance-at-point ()
  (dolist (file-and-pos verilog-ts-mode-test-utils-instance-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-utils (test-hdl-basename file "inst.point.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode verilog-ts-mode
                                                                   :fn verilog-ts-mode-test-utils-instance-at-point-fn
                                                                   :pos-list ,pos-list))
                                    (file-name-concat verilog-ts-mode-test-ref-dir-utils (test-hdl-basename file "inst.point.el")))))))


(ert-deftest utils::module-at-point ()
  (dolist (file-and-pos verilog-ts-mode-test-utils-module-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-utils (test-hdl-basename file "mod.point.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode verilog-ts-mode
                                                                   :fn verilog-ts-mode-test-utils-module-at-point-fn
                                                                   :pos-list ,pos-list))
                                    (file-name-concat verilog-ts-mode-test-ref-dir-utils (test-hdl-basename file "mod.point.el")))))))


(ert-deftest utils::identifier ()
  (dolist (file verilog-ts-mode-test-utils-identifier-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-utils (test-hdl-basename file "identifier.el"))
                                                         :process-fn 'eval
                                                         :fn #'verilog-ts-mode-test-utils-identifier-fn)
                                  (file-name-concat verilog-ts-mode-test-ref-dir-utils (test-hdl-basename file "identifier.el"))))))




(provide 'verilog-ts-mode-test-utils)

;;; verilog-ts-mode-test-utils.el ends here
