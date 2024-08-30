[![MELPA](https://melpa.org/packages/verilog-ts-mode-badge.svg)](https://melpa.org/#/verilog-ts-mode)
[![MELPA Stable](https://stable.melpa.org/packages/verilog-ts-mode-badge.svg)](https://stable.melpa.org/#/verilog-ts-mode)
[![Build Status](https://github.com/gmlarumbe/verilog-ts-mode/actions/workflows/build_straight.yml/badge.svg)](https://github.com/gmlarumbe/verilog-ts-mode/actions/workflows/build_straight.yml)
[![Build Status](https://github.com/gmlarumbe/verilog-ts-mode/actions/workflows/build_package_melpa_basic.yml/badge.svg)](https://github.com/gmlarumbe/verilog-ts-mode/actions/workflows/build_package_melpa_basic.yml)
[![Build Status](https://github.com/gmlarumbe/verilog-ts-mode/actions/workflows/build_package_melpa_stable.yml/badge.svg)](https://github.com/gmlarumbe/verilog-ts-mode/actions/workflows/build_package_melpa_stable.yml)


# verilog-ts-mode.el - SystemVerilog Tree-sitter mode for Emacs #

The package `verilog-ts-mode` provides syntax highlighting,
indentation, `imenu`, `which-func`, navigation and basic beautify and completion features.

`verilog-ts-mode` is derived from `verilog-mode` making AUTOs and other utilities still available.


## Requirements ##

- Emacs 29.1+ with tree-sitter support
- Verilog tree-sitter grammar
  - WARNING: This package relies on the new [tree-sitter-systemverilog](https://github.com/gmlarumbe/tree-sitter-systemverilog) grammar which is much more robust than the original [tree-sitter-verilog](https://github.com/tree-sitter/tree-sitter-verilog)

Before installing/building Emacs make sure that tree-sitter is available:

* Ubuntu:
``` shell
$ sudo apt-get install tree-sitter
```
* Arch:
``` shell
$ sudo pacman -S tree-sitter
```
* Manually:
```shell
$ git clone https://github.com/tree-sitter/tree-sitter.git
$ cd tree-sitter
$ make && sudo make install
```

If Emacs has been built with tree-sitter support the following command should return `t`:
```elisp
(treesit-available-p)
```

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

Once run successfully it will install the new
[tree-sitter-systemverilog](https://github.com/gmlarumbe/tree-sitter-systemverilog)
grammar that `verilog-ts-mode` relies on.

At this point, the following command should return `t`:

``` elisp
(treesit-language-available-p 'verilog)
```


## Setup ##

To open Verilog and SystemVerilog files with `verilog-ts-mode` simply
add this line to your init file:

``` elisp
(add-to-list 'auto-mode-alist '("\\.s?vh?\\'" . verilog-ts-mode))
```

### Syntax highlighting ###

To change the faces default values there are two methods:

* Via <kbd>M-x</kbd> `customize-group` <kbd>RET</kbd> `verilog-ts-faces`.
   - Customize faces and save configuration once you get the desired result

* Through elisp code
   - Below there is a snippet with a configuration example that works well with a dark background:
    ``` elisp
  (set-face-attribute 'verilog-ts-font-lock-grouping-keywords-face nil :foreground "dark olive green")
  (set-face-attribute 'verilog-ts-font-lock-punctuation-face nil       :foreground "burlywood")
  (set-face-attribute 'verilog-ts-font-lock-operator-face nil          :foreground "burlywood" :weight 'extra-bold)
  (set-face-attribute 'verilog-ts-font-lock-brackets-face nil          :foreground "goldenrod")
  (set-face-attribute 'verilog-ts-font-lock-parenthesis-face nil       :foreground "dark goldenrod")
  (set-face-attribute 'verilog-ts-font-lock-curly-braces-face nil      :foreground "DarkGoldenrod2")
  (set-face-attribute 'verilog-ts-font-lock-port-connection-face nil   :foreground "bisque2")
  (set-face-attribute 'verilog-ts-font-lock-dot-name-face nil          :foreground "gray70")
  (set-face-attribute 'verilog-ts-font-lock-brackets-content-face nil  :foreground "yellow green")
  (set-face-attribute 'verilog-ts-font-lock-width-num-face nil         :foreground "chartreuse2")
  (set-face-attribute 'verilog-ts-font-lock-width-type-face nil        :foreground "sea green" :weight 'bold)
  (set-face-attribute 'verilog-ts-font-lock-module-face nil            :foreground "green1")
  (set-face-attribute 'verilog-ts-font-lock-instance-face nil          :foreground "medium spring green")
  (set-face-attribute 'verilog-ts-font-lock-time-event-face nil        :foreground "deep sky blue" :weight 'bold)
  (set-face-attribute 'verilog-ts-font-lock-time-unit-face nil         :foreground "light steel blue")
  (set-face-attribute 'verilog-ts-font-lock-preprocessor-face nil      :foreground "pale goldenrod")
  (set-face-attribute 'verilog-ts-font-lock-modport-face nil           :foreground "light blue")
  (set-face-attribute 'verilog-ts-font-lock-direction-face nil         :foreground "RosyBrown3")
  (set-face-attribute 'verilog-ts-font-lock-translate-off-face nil     :background "gray20" :slant 'italic)
  (set-face-attribute 'verilog-ts-font-lock-attribute-face nil         :foreground "orange1")
    ```

# Contributing #

Contributions are welcome! Just stick to common Elisp conventions and
run the ERT suite after testing your changes and before submitting a
new PR.

For new functionality add new ERT tests if possible.

Consider [sponsoring](https://github.com/sponsors/gmlarumbe) to help
maintaining the project and for the development of new features. *Thank you!*

## ERT Tests setup ###

### Setup ###

To run the whole ERT test suite change directory to the `verilog-ts-mode`
root and make sure `test-hdl` Git submodule has been loaded:

```shell
git submodule update --init
```

### Targets ###

Then run the default target:

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
