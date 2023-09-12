[![MELPA](https://melpa.org/packages/verilog-ts-mode-badge.svg)](https://melpa.org/#/verilog-ts-mode)
[![MELPA Stable](https://stable.melpa.org/packages/verilog-ts-mode-badge.svg)](https://stable.melpa.org/#/verilog-ts-mode)
[![Build Status](https://github.com/gmlarumbe/verilog-ts-mode/workflows/ERT-straight/badge.svg)](https://github.com/gmlarumbe/verilog-ts-mode/actions/workflows/build_straight.yml)
[![Build Status](https://github.com/gmlarumbe/verilog-ts-mode/workflows/package-el-basic/badge.svg)](https://github.com/gmlarumbe/verilog-ts-mode/actions/workflows/build_package_melpa_basic.yml)
[![Build Status](https://github.com/gmlarumbe/verilog-ts-mode/workflows/ERT-MELPA-Stable/badge.svg)](https://github.com/gmlarumbe/verilog-ts-mode/actions/workflows/build_package_melpa_stable.yml)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# verilog-ts-mode.el - SystemVerilog Tree-sitter mode for Emacs #

The package `verilog-ts-mode` provides syntax highlighting,
indentation, `imenu`, `which-func`, navigation and basic beautify and completion features.

`verilog-ts-mode` is derived from `verilog-mode` making AUTOs and other utilities still available.


## Requirements ##

- Emacs 29.1+
- Verilog tree-sitter grammar


## Installation ##

### MELPA ###

`verilog-ts-mode` is available on MELPA.

### straight.el ###

To install it via [straight](https://github.com/radian-software/straight.el) with `use-package`:

```emacs-lisp
(straight-use-package 'use-package)
(use-package verilog-ts-mode)
```

### Tree-sitter grammar ###

The package provides an interactive command to simplify the installation of the grammar:

- `M-x RET verilog-ts-install-grammar RET`

This command requires Git, a C compiler and (sometimes) a C++ compiler,
and the linker to be installed and on the PATH.

Once run successfully it will install the
[forked](https://github.com/gmlarumbe/tree-sitter-verilog), maintained
version of [tree-sitter-verilog](https://github.com/tree-sitter/tree-sitter-verilog)
that `verilog-ts-mode` relies on.


## Setup ##

To open Verilog and SystemVerilog files with `verilog-ts-mode` simply
add this line to your init file:

``` elisp
(add-to-list 'auto-mode-alist '("\\.s?vh?\\'" . verilog-ts-mode))
```

# Contributing #

Contributions are welcome! Just stick to common Elisp conventions and
run the ERT suite after testing your changes and before submitting a
new PR.

For new functionality add new ERT tests if possible.

Consider [sponsoring](https://github.com/sponsors/gmlarumbe) to help
maintaining the project and for the development of new features. *Thank you!*

## ERT Tests setup ###

To run the whole ERT test suite change directory to the
`verilog-ts-mode` root and run the default target:

```shell
$ make
```

To run a subset of tests (e.g. navigation):

```shell
$ make TESTS=navigation
```

To regenerate all the expected outputs for the tests:

```shell
$ make gen
```

To regenerate the expected outputs for a group of tests (e.g. navigation):

```shell
$ make gen TESTS=navigation
```

## Other Emacs packages
* [vhdl-ts-mode](https://github.com/gmlarumbe/vhdl-ts-mode): VHDL Tree-sitter mode
* [verilog-ext](https://github.com/gmlarumbe/verilog-ext): SystemVerilog Extensions
* [vhdl-ext](https://github.com/gmlarumbe/vhdl-ext): VHDL Extensions
* [fpga](https://github.com/gmlarumbe/fpga): FPGA & ASIC Utilities for tools of major vendors and open source
* [wavedrom-mode](https://github.com/gmlarumbe/wavedrom-mode): edit and render WaveJSON files to create timing diagrams
* [vunit-mode](https://github.com/embed-me/vunit-mode.git): Integration of [VUnit](https://github.com/VUnit/vunit) workflow
