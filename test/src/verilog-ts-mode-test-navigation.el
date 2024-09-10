;;; verilog-ts-mode-test-navigation.el --- verilog-ts-mode ERT navigation tests  -*- lexical-binding: t -*-

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
;; verilog-ts-mode ERT navigation tests
;;
;;; Code:


(defconst verilog-ts-mode-test-ref-dir-navigation (file-name-concat verilog-ts-mode-test-ref-dir "navigation"))
(defconst verilog-ts-mode-test-dump-dir-navigation (file-name-concat verilog-ts-mode-test-dump-dir "navigation"))

(defconst verilog-ts-mode-test-navigation-rtl-file-list (mapcar (lambda (file)
                                                              (file-name-concat verilog-ts-mode-test-files-common-dir file))
                                                            '("instances.sv"
                                                              "ucontroller.sv"
                                                              "axi_demux.sv")))
(defconst verilog-ts-mode-test-navigation-tb-file-list (mapcar (lambda (file)
                                                             (file-name-concat verilog-ts-mode-test-files-common-dir file))
                                                           '("axi_test.sv"
                                                             "tb_program.sv"
                                                             "uvm_component.svh")))

(defconst verilog-ts-mode-test-navigation-block-nav-file-list verilog-ts-mode-test-common-file-list)

(defconst verilog-ts-mode-test-navigation-defun-up-file-and-pos
  `((,(file-name-concat verilog-ts-mode-test-files-common-dir "tb_program.sv") 855 1068 1143 1684 1829 2589 3495 4413 4635 4658)
    (,(file-name-concat verilog-ts-mode-test-files-common-dir "axi_test.sv") 883 936 954 1074 1218 1272 1433 1471 1636 5977 21939 22413 86894)
    (,(file-name-concat verilog-ts-mode-test-files-common-dir "uvm_component.svh") 1357 1516 1883 2595 58464 58659 58685 59192 59412 59984 59908 59897 59869 59546 100840)))

(defconst verilog-ts-mode-test-navigation-defun-down-file-and-pos
  `((,(file-name-concat verilog-ts-mode-test-files-common-dir "tb_program.sv") 855 1004 1189 1680 2029 3459 3602 3885 4007)
    (,(file-name-concat verilog-ts-mode-test-files-common-dir "axi_test.sv") 883 936 954 1074 1245 1309 1328 1356 1381 1433 1471 1490 2337 2456 2337 26583 26589 26699 27501 27586 86894)
    (,(file-name-concat verilog-ts-mode-test-files-common-dir "uvm_component.svh") 1261 1357 1883 2703 48756 59192 59464 59551 59874 60416 60417 60436 100840)))


(defun verilog-ts-mode-test-navigation-gen-expected-files ()
  ;; Instances fwd
  (test-hdl-gen-expected-files :file-list verilog-ts-mode-test-navigation-rtl-file-list
                               :dest-dir verilog-ts-mode-test-ref-dir-navigation
                               :out-file-ext "inst.fwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode verilog-ts-mode
                                       :fn verilog-ts-find-module-instance-fwd))
  ;; Instances bwd
  (test-hdl-gen-expected-files :file-list verilog-ts-mode-test-navigation-rtl-file-list
                               :dest-dir verilog-ts-mode-test-ref-dir-navigation
                               :out-file-ext "inst.bwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode verilog-ts-mode
                                       :fn verilog-ts-find-module-instance-bwd
                                       :start-pos-max t))
  ;; Classes fwd
  (test-hdl-gen-expected-files :file-list verilog-ts-mode-test-navigation-tb-file-list
                               :dest-dir verilog-ts-mode-test-ref-dir-navigation
                               :out-file-ext "class.fwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode verilog-ts-mode
                                       :fn verilog-ts-find-class-fwd))
  ;; Classes bwd
  (test-hdl-gen-expected-files :file-list verilog-ts-mode-test-navigation-tb-file-list
                               :dest-dir verilog-ts-mode-test-ref-dir-navigation
                               :out-file-ext "class.bwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode verilog-ts-mode
                                       :fn verilog-ts-find-class-bwd
                                       :start-pos-max t))
  ;; Task-functions fwd
  (test-hdl-gen-expected-files :file-list verilog-ts-mode-test-navigation-tb-file-list
                               :dest-dir verilog-ts-mode-test-ref-dir-navigation
                               :out-file-ext "tf.fwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode verilog-ts-mode
                                       :fn verilog-ts-find-function-task-fwd))
  ;; Task-functions bwd
  (test-hdl-gen-expected-files :file-list verilog-ts-mode-test-navigation-tb-file-list
                               :dest-dir verilog-ts-mode-test-ref-dir-navigation
                               :out-file-ext "tf.bwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode verilog-ts-mode
                                       :fn verilog-ts-find-function-task-bwd
                                       :start-pos-max t))
  ;; Block fwd
  (test-hdl-gen-expected-files :file-list verilog-ts-mode-test-navigation-block-nav-file-list
                               :dest-dir verilog-ts-mode-test-ref-dir-navigation
                               :out-file-ext "block.fwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode verilog-ts-mode
                                       :fn verilog-ts-find-block-fwd))
  ;; Block bwd
  (test-hdl-gen-expected-files :file-list verilog-ts-mode-test-navigation-block-nav-file-list
                               :dest-dir verilog-ts-mode-test-ref-dir-navigation
                               :out-file-ext "block.bwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode verilog-ts-mode
                                       :fn verilog-ts-find-block-bwd
                                       :start-pos-max t))
  ;; Defun level up
  (dolist (file-and-pos verilog-ts-mode-test-navigation-defun-up-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir verilog-ts-mode-test-ref-dir-navigation
                                   :out-file-ext "defun.up.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode verilog-ts-mode
                                           :fn verilog-ts-defun-level-up
                                           :pos-list ,pos-list))))
  ;; Defun level down
  (dolist (file-and-pos verilog-ts-mode-test-navigation-defun-down-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir verilog-ts-mode-test-ref-dir-navigation
                                   :out-file-ext "defun.down.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode verilog-ts-mode
                                           :fn verilog-ts-defun-level-down
                                           :pos-list ,pos-list)))))


(ert-deftest verilog-ts-mode::navigation::instances ()
  (dolist (file verilog-ts-mode-test-navigation-rtl-file-list)
    ;; Forward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-navigation (test-hdl-basename file "inst.fwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode verilog-ts-mode
                                                                 :fn verilog-ts-find-module-instance-fwd))
                                  (file-name-concat verilog-ts-mode-test-ref-dir-navigation (test-hdl-basename file "inst.fwd.el"))))
    ;; Backward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-navigation (test-hdl-basename file "inst.bwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode verilog-ts-mode
                                                                 :fn verilog-ts-find-module-instance-bwd
                                                                 :start-pos-max t))
                                  (file-name-concat verilog-ts-mode-test-ref-dir-navigation (test-hdl-basename file "inst.bwd.el"))))))


(ert-deftest verilog-ts-mode::navigation::classes ()
  (dolist (file verilog-ts-mode-test-navigation-tb-file-list)
    ;; Forward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-navigation (test-hdl-basename file "class.fwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode verilog-ts-mode
                                                                 :fn verilog-ts-find-class-fwd))
                                  (file-name-concat verilog-ts-mode-test-ref-dir-navigation (test-hdl-basename file "class.fwd.el"))))
    ;; Backward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-navigation (test-hdl-basename file "class.bwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode verilog-ts-mode
                                                                 :fn verilog-ts-find-class-bwd
                                                                 :start-pos-max t))
                                  (file-name-concat verilog-ts-mode-test-ref-dir-navigation (test-hdl-basename file "class.bwd.el"))))))


(ert-deftest verilog-ts-mode::navigation::task-functions ()
  (dolist (file verilog-ts-mode-test-navigation-tb-file-list)
    ;; Forward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-navigation (test-hdl-basename file "tf.fwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode verilog-ts-mode
                                                                 :fn verilog-ts-find-function-task-fwd))
                                  (file-name-concat verilog-ts-mode-test-ref-dir-navigation (test-hdl-basename file "tf.fwd.el"))))
    ;; Backward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-navigation (test-hdl-basename file "tf.bwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode verilog-ts-mode
                                                                 :fn verilog-ts-find-function-task-bwd
                                                                 :start-pos-max t))
                                  (file-name-concat verilog-ts-mode-test-ref-dir-navigation (test-hdl-basename file "tf.bwd.el"))))))


(ert-deftest verilog-ts-mode::navigation::blocks ()
  (dolist (file verilog-ts-mode-test-navigation-block-nav-file-list)
    ;; Forward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-navigation (test-hdl-basename file "block.fwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode verilog-ts-mode
                                                                 :fn verilog-ts-find-block-fwd))
                                  (file-name-concat verilog-ts-mode-test-ref-dir-navigation (test-hdl-basename file "block.fwd.el"))))
    ;; Backward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-navigation (test-hdl-basename file "block.bwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode verilog-ts-mode
                                                                 :fn verilog-ts-find-block-bwd
                                                                 :start-pos-max t))
                                  (file-name-concat verilog-ts-mode-test-ref-dir-navigation (test-hdl-basename file "block.bwd.el"))))))


(ert-deftest verilog-ts-mode::navigation::defun-level-up ()
  (dolist (file-and-pos verilog-ts-mode-test-navigation-defun-up-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-navigation (test-hdl-basename file "defun.up.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode verilog-ts-mode
                                                                   :fn verilog-ts-defun-level-up
                                                                   :pos-list ,pos-list))
                                    (file-name-concat verilog-ts-mode-test-ref-dir-navigation (test-hdl-basename file "defun.up.el")))))))


(ert-deftest verilog-ts-mode::navigation::defun-level-down ()
  (dolist (file-and-pos verilog-ts-mode-test-navigation-defun-down-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-navigation (test-hdl-basename file "defun.down.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode verilog-ts-mode
                                                                   :fn verilog-ts-defun-level-down
                                                                   :pos-list ,pos-list))
                                    (file-name-concat verilog-ts-mode-test-ref-dir-navigation (test-hdl-basename file "defun.down.el")))))))



(provide 'verilog-ts-mode-test-navigation)

;;; verilog-ts-mode-test-navigation.el ends here
