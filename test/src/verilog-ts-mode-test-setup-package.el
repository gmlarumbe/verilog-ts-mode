;;; verilog-ts-mode-test-setup-package.el --- verilog-ts-mode ERT Tests Setup with package.el  -*- lexical-binding: t -*-

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
;; verilog-ts-mode ERT Tests Setup with package.el
;;
;; INFO: Packages downloaded from MELPA (not MELPA Stable) will fetch the
;; snapshot of the latest commit in the corresponding Git repo and its
;; dependencies. It would therefore have the same effect as testing with
;; straight but with the issue that test/ code in the repo would not be in sync
;; with the code of the downloaded package until the snapshot is updated
;; (various times per day).
;;
;; For MELPA Stable this is different since package.el will download the tagged
;; version of the repo and all its dependencies.
;;
;;; Code:

;;;; Setup package.el
;; INFO: Perform tests in package.el only in MELPA Stable:
;;  - For bleeding-edge versions use straight and package.el basic test
(setq test-hdl-setup-package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")))
(require 'test-hdl-setup-package)

;;;; Install/setup package
(message "Installing and setting up verilog-ts-mode")
(package-install 'verilog-ts-mode)
(require 'verilog-ts-mode)
(add-to-list 'auto-mode-alist '("\\.s?vh?\\'" . verilog-ts-mode))

;;;; Customization
;;;;; align
(require 'align)
(setq align-default-spacing 1)
(setq align-to-tab-stop nil)

;;;;; tree-sitter
(setq treesit-font-lock-level 4)


(provide 'verilog-ts-mode-test-setup-package)

;;; verilog-ts-mode-test-setup-package.el ends here
