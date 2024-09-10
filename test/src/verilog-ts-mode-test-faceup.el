;;; verilog-ts-mode-test-faceup.el --- verilog-ts-mode ERT faceup tests  -*- lexical-binding: t -*-

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

;; verilog-ts-mode ERT faceup tests

;;; Code:

(defconst verilog-ts-mode-test-faceup-file-list verilog-ts-mode-test-common-file-list)

(defconst verilog-ts-mode-test-ref-dir-faceup (file-name-concat verilog-ts-mode-test-ref-dir "faceup"))
(defconst verilog-ts-mode-test-dump-dir-faceup (file-name-concat verilog-ts-mode-test-dump-dir "faceup"))


(defun verilog-ts-mode-test-faceup-file ()
  (verilog-ts-mode)
  (let ((verilog-align-typedef-regexp nil))
    (test-hdl-no-messages
      (test-hdl-faceup-test-file 'verilog-ts-mode))))


(defun verilog-ts-mode-test-faceup-gen-expected-files ()
  (test-hdl-gen-expected-files :file-list verilog-ts-mode-test-faceup-file-list
                               :dest-dir verilog-ts-mode-test-ref-dir-faceup
                               :out-file-ext "faceup"
                               :fn #'verilog-ts-mode-test-faceup-file))

(ert-deftest verilog-ts-mode::faceup ()
  (dolist (file verilog-ts-mode-test-faceup-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat verilog-ts-mode-test-dump-dir-faceup (test-hdl-basename file "faceup"))
                                                         :fn #'verilog-ts-mode-test-faceup-file)
                                  (file-name-concat verilog-ts-mode-test-ref-dir-faceup (test-hdl-basename file "faceup"))))))


(provide 'verilog-ts-mode-test-faceup)


;;; verilog-ts-mode-test-faceup.el ends here
