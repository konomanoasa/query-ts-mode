# query-ts-mode

[![CI](https://github.com/konomanoasa/query-ts-mode/actions/workflows/ci.yaml/badge.svg)](https://github.com/konomanoasa/query-ts-mode/actions/workflows/ci.yaml)

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/)-based
[Emacs](https://www.gnu.org/software/emacs/) major mode for
Tree-sitter Query Language.

## Requirement

Emacs 31.1 or later.

## Installation

```elisp
(package-vc-install "https://github.com/konomanoasa/query-ts-mode")
```

## Automatic Activation

Enabled for `.scm` files under a `queries` directory.

## Features

- Comment Commands
- Electric Pair
- Font Lock
- Indentation
- Navigation
- Syntax Table

## Font Lock

Supports `treesit-font-lock-level`.

| Level | Font Lock                                                                    |
| ----- | ---------------------------------------------------------------------------- |
| 1     | Comments                                                                     |
| 2     | Node, field, capture, predicate, and directive names, `MISSING`, and strings |
| 3     | Wildcards and escapes                                                        |
| 4     | Operators, punctuation, and brackets                                         |

## Grammar

[tree-sitter-query](https://github.com/konomanoasa/tree-sitter-query)

## License

[MIT](LICENSE)
