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
The other tool, `ocp-genannot` is tied to a particular OCaml version,
so it should be installed in the current switch, used to compile
`ocp-annot`.

### Emacs configuration

Edit your .emacs and add the following:

```
(let*
    ((ocp-annot-binary (executable-find "ocp-annot")))
  (if ocp-annot-binary
    (with-temp-buffer (insert (shell-command-to-string
     "ocp-annot --emacs --output-config")) (eval-buffer))))
```

Depending on your OCaml mode, pick one of the two following lines:

```
(add-hook 'tuareg-mode-hook 'ocp-annot-mode t)
(add-hook 'caml-mode-hook 'ocp-annot-mode t)
```

You might also want to replace in the previous lines 'ocp-annot-mode
  with one of the following:
* 'ocp-annot-when-no-merlin : `ocp-annot` will be used when `merlin` is not
    found in the PATH when starting Emacs.
* 'ocp-annot-when-no-ocamlspot : `ocp-annot` will be used when
    `ocamlspot` is not found in the PATH when starting Emacs.

You can also define your own keymap for `ocp-annot-mode`:
```
(setq ocp-annot-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") 'ocp-annot-print-info-at-point)
    (define-key map (kbd "C-c ;") 'ocp-annot-jump-to-definition-at-point)
    (define-key map (kbd "C-c C-a") 'ocp-annot-find-alternate-file)
    (define-key map (kbd "C-c C-m") 'ocp-annot-find-file-symbol-at-point)
    (define-key map (kbd "C-c C-u") 'ocp-annot-find-ident-at-point)
    (define-key map (kbd "C-c C-o") 'ocp-annot-local-occur-at-point)
    map))
```


## Usage

From the command line, the following arguments are available:

```
ocp-annot --help
  --emacs  Output for Emacs
  --json  Output in JSON
  --query-info-file-pos POSITION      Query type at pos {action}
  --query-jump-file-pos POSITION      Query jump info at pos {action}
  --query-jump-long-ident LIDENT      Query jump info on LIDENT {action}
  --query-file-long-ident LIDENT      Query file info on LIDENT {action}
  --query-uses-long-ident LIDENT      Query uses of LIDENT {action}
  --query-occur-long-ident LIDENT     Query locations of uses of LIDENT {action}
  --query-local-uses-file-pos POSITION  Query uses of ident at POSITION {action}
  --query-local-occur-file-pos POSITION Query locations of uses of ident at POSITION {action}
  --query-alternate-file FILE         Find interface/implementation {action}
   --output-config  Output config for the selected editor (Emacs)
  -help  Display this list of options
  --help  Display this list of options
```

`ocp-annot` will output results for Emacs by default, but it is possible
to use the `--json` argument to implement a mode for an editor supporting
JSON results.

### Emacs mode

The following functions are available:
* `ocp-annot-mode`: toggle `ocp-annot` minor mode
* `ocp-annot-print-info-at-point` (`C-c C-t`): print type of expression in
    mini-buffer. Repeat for enclosing expressions
* `ocp-annot-print-info-at-point-and-copy`: print type of expressions and
    copy it in clipboard
* `ocp-annot-jump-to-definition-at-point` (`C-c ;`): jump to the definition of
    the identifier under point
* `ocp-annot-jump-backward`: after jumping, jump backward
* `ocp-annot-jump-to-definition-at-point-other-window`: jump to the definition
    of the identifier, by opening a new window
* `ocp-annot-find-alternate-file` (`C-C C-a`): find the file defining the
    (module/long) identifier under point
* `ocp-annot-find-ident-at-point` (`C-c C-u`): display occurrences of the 
    identifier at point in the current project
* `ocp-annot-local-occur-at-point` (`C-c C-o`): display _exact_ occurrences
    of the identifier at point in the same file

## Watch mode

`ocp-annot --watch` can be used to monitor a project, and translate
any .cmt file into a .annot file.

To translate .cmt files into .annot files, ocp-annot will try the
following cases:
* If running at the top of the OCaml distribution, ocp-annot will
  use `tools/read_cmt`.
* If `ocaml_cmt` is available, it will use it. There is an OCaml PR on
  Github to install `read_cmt` as `ocaml_cmt` with OCaml.
* Otherwise, it will use `ocp-gennanot`, that is available from the
  `ocp-annot` package.

