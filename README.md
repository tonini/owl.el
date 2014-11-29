# OWL

> Produces HTML and online documentation for Emacs projects

OWL is in `alpha` stage, feedback and reporting issues are appreciated.

## Installation

```
$ cd into/emacs/project/
$ git clone https://github.com/tonini/owl.el.git doc
```

## Setup

Edit setup variables in `owl-setup.el`

```el
(setq owl-documentation-packages '(your-package))
(setq owl-documentation-prefix 'your-package-)
```

## Libary dependencies

If you have libary dependencies in your project, you need to use
[Cask](https://github.com/cask/cask) to generate the documention. That's because
you will run emacs in mode and via `cask exec` the dependencies will be loaded
via cask.

```
$ cd into/emacs/project
$ cask exec emacs -batch -Q -l doc/owl.el
$ open doc/index.html
```
