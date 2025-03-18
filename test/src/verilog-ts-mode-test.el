;;; verilog-ts-mode-test.el --- verilog-ts-mode ERT tests  -*- lexical-binding: t -*-

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
;; verilog-ts-mode ERT tests
;;
;;; Code:

;; Allow loading of packages in Emacs interactive session
(defconst verilog-ts-mode-test-dir (file-name-parent-directory (file-name-directory (or load-file-name (buffer-file-name)))))
(defconst verilog-ts-mode-test-hdl-dir (file-name-concat verilog-ts-mode-test-dir "test-hdl"))

(unless noninteractive
  (dolist (dir `(,(file-name-concat verilog-ts-mode-test-dir "src")
                 ,verilog-ts-mode-test-hdl-dir))
    (unless (member dir load-path)
      (add-to-list 'load-path dir))))

(require 'test-hdl)
(require 'verilog-ts-mode)


;;;; Directories
(defconst verilog-ts-mode-test-ref-dir (file-name-concat verilog-ts-mode-test-dir "ref"))
(defconst verilog-ts-mode-test-dump-dir (file-name-concat verilog-ts-mode-test-dir "dump"))
(defconst verilog-ts-mode-test-files-dir (file-name-concat verilog-ts-mode-test-dir "files"))
(defconst verilog-ts-mode-test-files-common-dir (file-name-concat verilog-ts-mode-test-files-dir "common"))
(defconst verilog-ts-mode-test-ucontroller-dir (file-name-concat verilog-ts-mode-test-files-dir "ucontroller"))
(defconst verilog-ts-mode-test-ucontroller-rtl-dir (file-name-concat verilog-ts-mode-test-ucontroller-dir "rtl"))
(defconst verilog-ts-mode-test-ucontroller-tb-dir (file-name-concat verilog-ts-mode-test-ucontroller-dir "tb"))

(defconst verilog-ts-mode-test-common-file-list (test-hdl-directory-files verilog-ts-mode-test-files-common-dir
                                                                          verilog-ts-file-extension-re))


;;;; Tests
(require 'verilog-ts-mode-test-faceup)
(require 'verilog-ts-mode-test-indent)
(require 'verilog-ts-mode-test-utils)
(require 'verilog-ts-mode-test-imenu)
(require 'verilog-ts-mode-test-navigation)
(require 'verilog-ts-mode-test-beautify)


;;;; Aux funcs
(defun verilog-ts-mode-test-gen-expected-files ()
  (verilog-ts-mode-test-faceup-gen-expected-files)
  (verilog-ts-mode-test-indent-gen-expected-files)
  (verilog-ts-mode-test-utils-gen-expected-files)
  (verilog-ts-mode-test-imenu-gen-expected-files)
  (verilog-ts-mode-test-navigation-gen-expected-files)
  (verilog-ts-mode-test-beautify-gen-expected-files))


(provide 'verilog-ts-mode-test)

;;; verilog-ts-mode-test.el ends here
