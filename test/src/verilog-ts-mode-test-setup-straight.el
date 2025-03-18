;;; verilog-ts-mode-test-setup-straight.el --- verilog-ts-mode ERT Tests Setup with straight.el -*- lexical-binding: t -*-

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
;; verilog-ts-mode ERT Tests Setup with straight.el
;;
;;; Code:


(require 'test-hdl-setup-straight)


;;;; Setup built-in dependencies
(use-package align
  :straight nil
  :config
  (setq align-default-spacing 1)
  (setq align-to-tab-stop nil))


;;;; Install/setup package
;; Always needed since straight.el does not download dependencies of local packages
(use-package verilog-ts-mode
  :mode (("\\.s?vh?\\'" . verilog-ts-mode))
  :config
  (setq treesit-font-lock-level 4))

;; Shadow/override with actions/checkout repo, instead of the one downloaded by straight.el
(test-hdl-when-github-action
  (use-package verilog-ts-mode
    :straight nil))


(provide 'verilog-ts-mode-test-setup-straight)

;;; verilog-ts-mode-test-setup-straight.el ends here
