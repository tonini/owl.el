# OWL

> Produces HTML and online documentation for Emacs projects

***

![Produces HTML and online documentation for Emacs projects](http://i.imgur.com/QumUjDY.png)

***

OWL is in `alpha` stage, feedback and reporting issues are appreciated.

Working example can be found [here](http://www.samueltonini.com/owl-example/).

## Installation

```
$ cd into/emacs/project/
$ git clone https://github.com/tonini/owl.el.git doc
```

## Setup

Change the `owl-setup.el.example` file to `owl-setup.el` and add your specific
documentation setup.

Example:

```el
(setq owl-documentation-packages '(your-package))
(setq owl-documentation-prefix 'your-package-)
```

`owl-documentation-packages` holds the packages of the project who should be loaded.

`owl-documentation-prefix` is the prefix which is be used to fetch `functions`
and `variables` from the project codebase.

## Library dependencies

If you have libary dependencies which doesn't come with emacs in your project, you need to use
[Cask](https://github.com/cask/cask) to generate the documention. That's because
you will run emacs in mode and via `cask exec` the dependencies will be loaded
via cask.

## Usage

With `cask` and third-party dependencies:

```
$ cd into/emacs/project
$ cask exec emacs -batch -Q -l doc/owl.el
$ open doc/index.html
```

Without `cask` and third-party dependencies.
```
$ cd into/emacs/project
$ emacs -batch -Q -l doc/owl.el
$ open doc/index.html
```
