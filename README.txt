# ocp-annot

`ocp-annot` is a simple editor helper tool to use `.annot` files when
other tools are not available, for example if you are working with
an unsupported version of OCaml (trunk).

It provides the following features:
* Show the type of an expression, and the enclosing expressions
* Jump to the definition of an identifier, in the same file or in another file

## Installation

First, install the `ocp-annot` binary in a generic binary location
(not a switch if possible).

### Emacs configuration

Edit your .emacs and add the following:
==================================
(with-temp-buffer (insert (shell-command-to-string
  "ocp-annot --emacs --output-config")) (eval-buffer))
==================================

Depending on your OCaml mode, pick one of the two following lines:
==================================
(add-hook 'tuareg-mode-hook 'ocp-annot-mode t)
(add-hook 'caml-mode-hook 'ocp-annot-mode t)
===================================

You might also want to replace in the previous lines 'ocp-annot-mode
  with one of the following:
* 'ocp-annot-when-no-merlin : `ocp-annot` will be used when `merlin` is not
    found in the PATH when starting Emacs.
* 'ocp-annot-when-no-ocamlspot : `ocp-annot` will be used when
    `ocamlspot` is not found in the PATH when starting Emacs.

You can also define your own keymap for `ocp-annot-mode`:
===================================
(setq ocp-annot-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") 'ocp-annot-print-info-at-point)
    (define-key map (kbd "C-c ;") 'ocp-annot-jump-to-definition-at-point)
    (define-key map (kbd "C-c C-a") 'ocp-annot-find-alternate-file)
    (define-key map (kbd "C-c C-m") 'ocp-annot-find-file-symbol-at-point)
    map))
===================================


## Usage

From the command line, the following arguments are available:

```
ocp-annot --help
  --emacs  Output for Emacs
  --json  Output in JSON
  --query-info-file-pos FILE:LINE:LINEPOS Query info at pos
  --query-jump-file-pos FILE:LINE:LINEPOS Query jump info at pos
  --query-jump-long-ident LONG_IDENT Query jump info at pos
  --query-alternate-file FILE Query corresponding interface/implementation
  --output-config  Output config for the selected editor (Emacs)
  -help  Display this list of options
  --help  Display this list of options
```

`ocp-annot` will output results for Emacs by default, but it is possible
to use the `--json` argument to implement a mode for an editor supporting
JSON results.

### Emacs mode

The following functions are available:
* ocp-annot-mode: toggle `ocp-annot` minor mode
* ocp-annot-print-info-at-point (C-c C-t)
* ocp-annot-print-info-at-point-and-copy
* ocp-annot-jump-backward
* ocp-annot-jump-to-definition-at-point (C-c ;)
* ocp-annot-jump-to-definition-at-point-other-window
* ocp-annot-find-alternate-file (C-C C-a)

