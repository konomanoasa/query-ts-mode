;;; query-ts-mode-test.el --- Tests for query-ts-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 konomanoasa
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(require 'ert)
(require 'loaddefs-gen)
(require 'newcomment)
(require 'query-ts-mode)

(dolist (language '(query))
  (unless (treesit-ready-p language t)
    (error "The %s grammar is required to run the tests" language)))

;;;; Helpers

(defun query-ts-mode-test--position (fragment &optional line)
  (save-excursion
    (goto-char (point-min))
    (when line
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (if (equal line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
              (setq found t)
            (forward-line 1)))
        (unless found (ert-fail (format "Missing fixture line: %S" line)))))
    (unless (search-forward fragment (and line (line-end-position)) t)
      (ert-fail (format "Missing fixture fragment: %S" fragment)))
    (- (point) (length fragment))))

(defun query-ts-mode-test--face (fragment &optional offset line)
  (get-text-property (+ (query-ts-mode-test--position fragment line)
                        (or offset 0)) 'face))

(defun query-ts-mode-test--syntax-class (fragment &optional offset line)
  (syntax-propertize (point-max))
  (syntax-class (syntax-after (+ (query-ts-mode-test--position fragment line)
                                 (or offset 0)))))

(defun query-ts-mode-test--indent (source &optional offset)
  (with-temp-buffer
    (insert source)
    (query-ts-mode)
    (setq-local indent-tabs-mode nil)
    (when offset
      (setq-local query-ts-mode-indent-offset offset))
    (indent-region (point-min) (point-max))
    (let ((indented (buffer-string)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) indented))
      indented)))

(defun query-ts-mode-test--buffer-state ()
  (font-lock-ensure)
  (syntax-propertize (point-max))
  (let (state)
    (dotimes (offset (- (point-max) (point-min)))
      (let ((position (+ (point-min) offset)))
        (push (list (get-text-property position 'face) (syntax-after position)) state)))
    (nreverse state)))

(defun query-ts-mode-test--should-match-fresh-buffer (level)
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (state (query-ts-mode-test--buffer-state))
        (file buffer-file-name))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (let ((treesit-font-lock-level level)) (query-ts-mode))
      (should (equal state (query-ts-mode-test--buffer-state))))))

;;;; Grammar

(ert-deftest query-ts-mode-respects-grammar-sources ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)) received)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (language)
                  (setq received (assq language treesit-language-source-alist))
                  t))
          (dolist (source query-ts-mode--grammar-sources)
            (let* ((language (car source))
                   (custom (list language "/local/grammar" :revision "custom")))
              (dolist (configured (list nil (list custom)))
                (let ((treesit-language-source-alist configured))
                  (should (query-ts-mode--ensure-grammar language))
                  (should (equal received (if configured custom source)))
                  (should (eq treesit-language-source-alist configured)))))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest query-ts-mode-reports-unavailable-grammar ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)))
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed (lambda (_language) nil))
          (with-temp-buffer
            (should-error (query-ts-mode) :type 'user-error)
            (should-not (treesit-parser-list))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest query-ts-mode-starts-and-reuses-parser ()
  (with-temp-buffer
    (insert "(node) @capture\n")
    (query-ts-mode)
    (should (eq major-mode 'query-ts-mode))
    (should (eq (treesit-parser-language treesit-primary-parser) 'query))
    (should (equal (treesit-node-type (treesit-parser-root-node treesit-primary-parser))
                   "query"))
    (query-ts-mode)
    (should (equal (treesit-parser-list) (list treesit-primary-parser)))))

;;;; Mode Selection

(ert-deftest query-ts-mode-selects-files ()
  (pcase-dolist (`(,file . ,expected)
                 '(("/tmp/queries/highlights.scm" . t)
                   ("/tmp/queries/python/highlights.scm" . t)
                   ("queries/injections.scm" . t)
                   ("/tmp/highlights.scm" . nil)
                   ("/tmp/queries.scm" . nil)
                   ("/tmp/myqueries/highlights.scm" . nil)
                   ("/tmp/queries/highlights.txt" . nil)))
    (with-temp-buffer
      (setq buffer-file-name file)
      (set-auto-mode)
      (should (eq (eq major-mode 'query-ts-mode) expected)))))

(ert-deftest query-ts-mode-generates-autoloads ()
  (let ((output (make-temp-file "query-ts-mode-loaddefs-"))
        (directory (file-name-directory (locate-library "query-ts-mode"))))
    (unwind-protect
        (progn
          (loaddefs-generate directory output nil nil nil t)
          (with-temp-buffer
            (insert-file-contents output)
            (dolist (form '("(autoload 'query-ts-mode" "(add-to-list 'auto-mode-alist"))
              (goto-char (point-min))
              (should (search-forward form nil t)))))
      (delete-file output))))

;;;; Syntax

(ert-deftest query-ts-mode-classifies-owned-delimiters-and-strings ()
  (with-temp-buffer
    (insert "(node \"(\" [(a) \"[\"] ((b) (#eq? @b \")\")) (#set! k \"{}\"))\n")
    (query-ts-mode)
    (pcase-dolist (`(,fragment ,offset ,class)
                   '(("(node" 0 4) ("\"(\"" 0 15) ("\"(\"" 1 1) ("\"(\"" 2 15)
                     ("[(a)" 0 4) ("(a)" 0 4) ("(a)" 2 5) ("\"[\"" 1 1) ("] ((" 0 5)
                     ("((b)" 0 4) ("((b)" 1 4) ("(#eq" 0 4) ("\")\"" 1 1)
                     ("(#set" 0 4) ("{}" 0 1) ("{}" 1 1) ("))\n" 0 5) ("))\n" 1 5)))
      (ert-info ((format "%S at %d" fragment offset))
        (should (= (query-ts-mode-test--syntax-class fragment offset) class))))
    (should (= (scan-sexps 1 1) (query-ts-mode-test--position "\n")))
    (should (nth 3 (syntax-ppss (1+ (query-ts-mode-test--position "{}")))))))

(ert-deftest query-ts-mode-keeps-unowned-and-unterminated-delimiters-as-punctuation ()
  (with-temp-buffer
    (insert ")]\n(#eq? @x \"unfinished\n")
    (let ((treesit-font-lock-level 4)) (query-ts-mode))
    (font-lock-ensure)
    (should (treesit-node-check (treesit-parser-root-node treesit-primary-parser) 'has-error))
    (dolist (fragment '(")]" "]\n" "\"unfinished"))
      (ert-info ((format "%S" fragment))
        (should (= (query-ts-mode-test--syntax-class fragment) 1))))
    (dolist (fragment '(")]" "]\n"))
      (should-not (query-ts-mode-test--face fragment)))))

(ert-deftest query-ts-mode-keeps-string-interior-quotes-and-escapes-as-punctuation ()
  (with-temp-buffer
    (insert "(#eq? @x \"'`\\\"\\\\[](){}\")\n")
    (query-ts-mode)
    (dolist (fragment '("'" "`" "\\\"" "\\\\" "[" "]" "(" ")" "{" "}"))
      (let ((start (query-ts-mode-test--position "'")))
        (goto-char start)
        (search-forward fragment)
        (syntax-propertize (point-max))
        (should (= (syntax-class (syntax-after (- (point) (length fragment)))) 1))))))

(ert-deftest query-ts-mode-classifies-comments ()
  (with-temp-buffer
    (insert "; first\n(node \"; text\") ;; second\n(#eq? @x \";\")\n")
    (query-ts-mode)
    (syntax-propertize (point-max))
    (pcase-dolist (`(,fragment . ,expected)
                   '(("first" . t) ("second" . t) ("text" . nil) ("eq" . nil)))
      (ert-info ((format "%S" fragment))
        (should (eq (not (null (nth 4 (syntax-ppss
                                       (query-ts-mode-test--position fragment)))))
                    expected))))
    (should (nth 3 (syntax-ppss (query-ts-mode-test--position "text"))))))

;;;; Comment Commands

(ert-deftest query-ts-mode-comments-and-uncomments ()
  (with-temp-buffer
    (insert "(node)\n;; note\n")
    (query-ts-mode)
    (comment-region 1 7)
    (should (equal (buffer-string) "; (node)\n;; note\n"))
    (uncomment-region (point-min) (point-max))
    (should (equal (buffer-string) "(node)\nnote\n"))))

(ert-deftest query-ts-mode-uncomments-multiline-source ()
  (dolist (case
           '(("(#eq? @x \"one\\\n; two\")\n" "; (#eq? @x \"one\\\n; ; two\")\n")))
    (with-temp-buffer
      (insert (car case))
      (query-ts-mode)
      (let ((state (query-ts-mode-test--buffer-state)))
        (comment-or-uncomment-region (point-min) (point-max))
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       (cadr case)))
        (comment-or-uncomment-region (point-min) (point-max))
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       (car case)))
        (should (equal (query-ts-mode-test--buffer-state) state))))))

;;;; Electric Pair

(ert-deftest query-ts-mode-supplies-electric-pairs ()
  (let ((electric-pair-pairs '((?% . ?%)))
        (electric-pair-mode nil))
    (pcase-dolist (`(,prefix ,opening ,expected)
                   '(("" ?\( "()")
                     ("" ?\[ "[]")))
      (ert-info ((format "%S / %c" prefix opening))
        (with-temp-buffer
          (insert prefix)
          (query-ts-mode)
          (should-not electric-pair-mode)
          (should (local-variable-p 'electric-pair-pairs))
          (should (equal (assq ?% electric-pair-pairs) '(?% . ?%)))
          (electric-pair-local-mode 1)
          (let ((last-command-event opening)) (self-insert-command 1))
          (should (equal (buffer-string) expected))
          (should (= (point) (1- (point-max)))))))
    (should (equal electric-pair-pairs '((?% . ?%)))))
  (let ((electric-pair-pairs '((?\( . ?!) (?\[ . ?@))))
    (pcase-dolist (`(,prefix ,opening ,expected)
                   '(("" ?\( "(!")
                     ("" ?\[ "[@")))
      (with-temp-buffer
        (insert prefix)
        (query-ts-mode)
        (electric-pair-local-mode 1)
        (let ((last-command-event opening)) (self-insert-command 1))
        (should (equal (buffer-string) expected))
        (should (= (point) (1- (point-max))))))
    (should (equal electric-pair-pairs '((?\( . ?!) (?\[ . ?@))))))

(ert-deftest query-ts-mode-respects-pair-newline-preferences ()
  (let ((calls 0))
    (pcase-dolist (`(,setting ,expected)
                   (list (list nil "(\n)")
                         (list t "(\n\n)")
                         (list (lambda () (setq calls (1+ calls)) nil) "(\n)")
                         (list (lambda () (setq calls (1+ calls)) t) "(\n\n)")))
      (setq calls 0)
      (let ((electric-pair-open-newline-between-pairs setting))
        (with-temp-buffer
          (insert "()")
          (query-ts-mode)
          (electric-indent-local-mode -1)
          (electric-pair-local-mode 1)
          (goto-char 2)
          (call-interactively (key-binding (kbd "RET")))
          (should (equal (buffer-string) expected)))
        (should (eq (> calls 0) (functionp setting)))
        (should (eq electric-pair-open-newline-between-pairs setting))))))

(ert-deftest query-ts-mode-pairs-delimiters-and-indents-on-return ()
  (pcase-dolist (`(,prefix ,opening ,expected ,column)
                 '(("" ?\( "(\n  \n)" 2)
                   ("" ?\[ "[\n  \n]" 2)))
    (let ((electric-pair-pairs nil)
          (electric-pair-open-newline-between-pairs t))
      (with-temp-buffer
        (query-ts-mode)
        (setq-local indent-tabs-mode nil)
        (electric-indent-local-mode 1)
        (electric-pair-local-mode 1)
        (insert prefix)
        (let ((last-command-event opening))
          (self-insert-command 1))
        (call-interactively (key-binding (kbd "RET")))
        (should (equal (buffer-string) expected))
        (should (= (current-column) column))))))

(ert-deftest query-ts-mode-restricts-pair-newlines-to-cst-contexts ()
  (let ((electric-pair-open-newline-between-pairs t))
    (pcase-dolist (`(,before ,after ,expected)
                   '(("(" ")" "(\n\n)")
                     ("[" "]" "[\n\n]")
                     ("\"(" ")\"" "\"(\n)\"")
                     ("\"[" "]\"" "\"[\n]\"")
                     ("; (" ")" "; (\n)")
                     ("; [" "]" "; [\n]")))
      (ert-info ((format "%S / %S" before after))
        (with-temp-buffer
          (insert before after)
          (query-ts-mode)
          (electric-indent-local-mode -1)
          (electric-pair-local-mode 1)
          (goto-char (1+ (length before)))
          (call-interactively (key-binding (kbd "RET")))
          (should (equal (buffer-string) expected)))))))

(ert-deftest query-ts-mode-pairs-language-quotes ()
  (let ((electric-pair-pairs nil)
        (electric-pair-text-pairs nil))
    (pcase-dolist (`(,prefix ,quote ,opened ,closed)
                   '(("(#eq? @x " ?\" "(#eq? @x \"\"" "(#eq? @x \"x\"")))
      (with-temp-buffer
        (insert prefix)
        (query-ts-mode)
        (electric-pair-local-mode 1)
        (let ((last-command-event quote)) (self-insert-command 1))
        (should (equal (buffer-string) opened))
        (should (= (point) (1- (point-max))))
        (let ((last-command-event ?x)) (self-insert-command 1))
        (let ((last-command-event quote)) (self-insert-command 1))
        (should (equal (buffer-string) closed))
        (should (eobp))))
    (should-not electric-pair-pairs)))

;;;; Font Lock

(ert-deftest query-ts-mode-fontifies-by-level ()
  (dolist (level '(1 2 3 4))
    (let ((treesit-font-lock-level level))
      (with-temp-buffer
        (insert "; note\n((node field: (_) !other . (child)+ @cap) (#eq? @cap \"a\\n\" local))\n"
                "(MISSING)\n")
        (query-ts-mode)
        (font-lock-ensure)
        (pcase-dolist (`(,fragment ,offset ,minimum ,face)
                       '(("note" 0 1 font-lock-comment-face)
                         ("node" 0 2 font-lock-type-face)
                         ("field" 0 2 font-lock-property-use-face)
                         ("other" 0 2 font-lock-property-use-face)
                         ("@cap)" 1 2 font-lock-variable-name-face)
                         ("@cap \"" 1 2 font-lock-variable-use-face)
                         ("eq" 0 2 font-lock-function-call-face)
                         ("\"a" 0 2 font-lock-string-face)
                         ("\"a" 1 2 font-lock-string-face)
                         ("local" 0 2 font-lock-string-face)
                         ("MISSING" 0 2 font-lock-keyword-face)
                         ("_)" 0 3 font-lock-constant-face)
                         ("\\n" 0 3 font-lock-escape-face)
                         ("+" 0 4 font-lock-operator-face)
                         (". (" 0 4 font-lock-operator-face)
                         ("!other" 0 4 font-lock-negation-char-face)
                         (": (" 0 4 font-lock-punctuation-face)
                         ("@cap)" 0 4 font-lock-punctuation-face)
                         ("#eq" 0 4 font-lock-punctuation-face)
                         ("? @" 0 4 font-lock-punctuation-face)
                         ("((" 0 4 font-lock-bracket-face)))
          (ert-info ((format "Level %s: %S at %d" level fragment offset))
            (should (eq (query-ts-mode-test--face fragment offset)
                        (and (>= level minimum) face)))))))))

(ert-deftest query-ts-mode-fontifies-names-by-owning-context ()
  (with-temp-buffer
    (insert "(ERROR) (MISSING identifier) (MISSING _/\"token\") (expression/call) (_/node)\n"
            "(node _field: (_)* @a.b) (#set! injection.language \"x\\\"y\")\n")
    (let ((treesit-font-lock-level 4)) (query-ts-mode))
    (font-lock-ensure)
    (pcase-dolist (`(,fragment ,offset ,face)
                   '(("ERROR" 0 font-lock-type-face)
                     ("MISSING identifier" 0 font-lock-keyword-face)
                     ("identifier" 0 font-lock-type-face)
                     ("_/\"token" 0 font-lock-type-face)
                     ("_/\"token" 1 font-lock-punctuation-face)
                     ("_/\"token" 2 font-lock-string-face)
                     ("token" 0 font-lock-string-face)
                     ("expression" 0 font-lock-type-face)
                     ("/call" 0 font-lock-punctuation-face)
                     ("call" 0 font-lock-type-face)
                     ("_/node" 0 font-lock-type-face)
                     ("node)" 0 font-lock-type-face)
                     ("_field: (_)" 0 font-lock-constant-face)
                     ("_field: (_)" 1 font-lock-property-use-face)
                     ("_)*" 0 font-lock-constant-face)
                     ("*" 0 font-lock-operator-face)
                     ("a.b" 0 font-lock-variable-name-face)
                     ("a.b" 1 font-lock-variable-name-face)
                     ("#set" 0 font-lock-punctuation-face)
                     ("set" 0 font-lock-function-call-face)
                     ("! injection" 0 font-lock-punctuation-face)
                     ("injection.language" 0 font-lock-string-face)
                     ("injection.language" 9 font-lock-string-face)
                     ("x\\" 0 font-lock-string-face)
                     ("\\\"" 0 font-lock-escape-face)
                     ("\\\"" 1 font-lock-escape-face)
                     ("y\"" 0 font-lock-string-face)
                     ("y\"" 1 font-lock-string-face)
                     ("\n(node" 0 nil)
                     ("(node _" 0 font-lock-bracket-face)))
      (ert-info ((format "%S at %d" fragment offset))
        (should (eq (query-ts-mode-test--face fragment offset) face))))))

(ert-deftest query-ts-mode-highlights-field-chains-and-call-anchors ()
  (with-temp-buffer
    (insert "outer: inner: (node . (#custom?)) @x\n")
    (let ((treesit-font-lock-level 4)) (query-ts-mode))
    (font-lock-ensure)
    (pcase-dolist (`(,fragment ,face)
                   '(("outer" font-lock-property-use-face)
                     ("inner" font-lock-property-use-face)
                     ("node" font-lock-type-face)
                     ("." font-lock-operator-face)
                     ("custom" font-lock-function-call-face)
                     ("x\n" font-lock-variable-name-face)))
      (should (eq (query-ts-mode-test--face fragment) face)))))

;;;; Navigation

(ert-deftest query-ts-mode-navigates-top-level-field-chains ()
  (with-temp-buffer
    (insert "outer:\n  inner:\n    (node . (#custom?)) @x\n\n[(other) next: _]\n")
    (query-ts-mode)
    (dolist (fragment '("inner:" "node" "custom" "@x"))
      (goto-char (query-ts-mode-test--position fragment))
      (beginning-of-defun)
      (should (= (point) (point-min)))
      (end-of-defun)
      (should (= (point) (1+ (query-ts-mode-test--position "\n\n[")))))
    (goto-char (query-ts-mode-test--position "next:"))
    (beginning-of-defun)
    (should (= (point) (query-ts-mode-test--position "[")))))

(ert-deftest query-ts-mode-navigates-strings-and-top-level-defuns ()
  (with-temp-buffer
    (insert "; head\n(first\n  (child) @c)\n\n((second) (#eq? @x \"a b\"))\n(#set! k v)\n")
    (query-ts-mode)
    (should-not treesit-simple-imenu-settings)
    (should (equal (mapcar #'car (cdr (assq 'query treesit-thing-settings)))
                   '(sexp defun)))
    (goto-char (query-ts-mode-test--position "(first"))
    (forward-sexp)
    (should (= (point) (query-ts-mode-test--position "\n\n((second")))
    (goto-char (query-ts-mode-test--position "\"a b\""))
    (forward-sexp)
    (should (= (point) (query-ts-mode-test--position "))\n(#set")))
    (goto-char (query-ts-mode-test--position "child"))
    (beginning-of-defun)
    (should (= (point) (query-ts-mode-test--position "(first")))
    (end-of-defun)
    (should (= (point) (1+ (query-ts-mode-test--position "\n\n((second"))))
    (goto-char (query-ts-mode-test--position "eq"))
    (beginning-of-defun)
    (should (= (point) (query-ts-mode-test--position "((second")))
    (end-of-defun)
    (should (= (point) (query-ts-mode-test--position "(#set")))
    (goto-char (query-ts-mode-test--position "k v"))
    (beginning-of-defun)
    (should (= (point) (query-ts-mode-test--position "(#set")))))

(ert-deftest query-ts-mode-navigates-atoms-and-prefixed-specifications ()
  (with-temp-buffer
    (insert "(node child: (super/sub) @cap !absent . (MISSING \"tok\") (_)) @outer\n"
            "(#eq? @outer \"a b\" local)\n")
    (query-ts-mode)
    (pcase-dolist (`(,fragment ,offset ,count ,target ,target-offset)
                   '(("node" 0 1 "node" 4)
                     ("node" 4 -1 "node" 0)
                     ("child:" 0 1 "@cap" 4)
                     ("child:" 5 -1 "child:" 0)
                     ("super/sub" 0 1 "super/sub" 9)
                     ("super/sub" 6 1 "super/sub" 9)
                     ("super/sub" 9 -1 "super/sub" 0)
                     ("@cap" 0 1 "@cap" 4)
                     ("@cap" 4 -1 "child:" 0)
                     ("!absent" 0 1 "!absent" 7)
                     ("MISSING" 0 1 "\"tok\"" 5)
                     ("\"tok\"" 0 1 "\"tok\"" 5)
                     ("(_)" 1 1 "(_)" 2)
                     ("eq?" 0 1 "eq?" 2)
                     ("\"a b\"" 1 1 "\"a b\"" 5)
                     ("local" 0 1 "local" 5)))
      (ert-info ((format "%S" (list fragment offset count target target-offset)))
        (goto-char (+ (query-ts-mode-test--position fragment) offset))
        (forward-sexp count)
        (should (= (point) (+ (query-ts-mode-test--position target)
                              target-offset)))))))

(ert-deftest query-ts-mode-navigates-postfixed-patterns-and-edits ()
  (with-temp-buffer
    (insert "(a)+ @first\n(b) @second\n(#custom?)\n")
    (query-ts-mode)
    (goto-char (point-min))
    (forward-sexp 2)
    (should (= (point) (query-ts-mode-test--position "\n(#custom")))
    (backward-sexp 2)
    (should (= (point) (point-min)))
    (goto-char (query-ts-mode-test--position "(b)"))
    (insert "field: ")
    (forward-sexp)
    (should (= (point) (query-ts-mode-test--position "\n(#custom")))
    (backward-sexp)
    (should (= (point) (query-ts-mode-test--position "field:")))
    (goto-char (point-max))
    (beginning-of-defun)
    (should (= (point) (query-ts-mode-test--position "(#custom")))
    (beginning-of-defun)
    (should (= (point) (query-ts-mode-test--position "field:")))))

;;;; Indentation

(ert-deftest query-ts-mode-indents-top-level-and-alternant-field-chains ()
  (should (equal (query-ts-mode-test--indent
                  "  outer:\ninner:\n(node)\n@x\n  [(left)\nright:\n(child)]\n"
                  2)
                 "outer:\n  inner:\n    (node)\n      @x\n[(left)\n  right:\n    (child)]\n")))

(ert-deftest query-ts-mode-indents-structures ()
  (should (equal (query-ts-mode-test--indent
                  (concat "  ; head\n  (node\n(child\nfield:\n(_))\n@cap\n)\n"
                          "[\n\"(\"\n; inside\n] @x\n((a) @y\n(#match? @y\n\"^A\"))\n")
                  3)
                 (concat "; head\n(node\n   (child\n      field:\n         (_))\n      @cap\n)\n"
                         "[\n   \"(\"\n   ; inside\n] @x\n((a) @y\n   (#match? @y\n      \"^A\"))\n"))))

(ert-deftest query-ts-mode-preserves-string-continuation-whitespace ()
  (dolist (source '("(#eq? @x \"a\\\n   b\\\n c\")\n"
                    "(node\n  \"x\\\n\")\n"))
    (should (equal (query-ts-mode-test--indent source 2) source))))

(ert-deftest query-ts-mode-indents-blank-lines ()
  (with-temp-buffer
    (insert "(node\n\n)\n  \n(other)\n")
    (query-ts-mode)
    (goto-char (point-min))
    (forward-line 1)
    (indent-according-to-mode)
    (should (= (current-column) query-ts-mode-indent-offset))
    (forward-line 2)
    (indent-according-to-mode)
    (should (= (line-beginning-position) (line-end-position)))))

(ert-deftest query-ts-mode-indents-enter-after-nested-closings ()
  (pcase-dolist (`(,source ,offset ,column)
                 '(("(outer\n  (inner)|\n)\n" 2 2)
                   ("(outer\n  (middle\n    (inner)|\n  )\n)\n" 2 4)
                   ("(outer\n    (middle\n        (inner) @x|\n    )\n)\n" 4 8)
                   ("(outer\n  field: (inner)|\n)\n" 2 2)
                   ("[(inner)|\n]\n" 3 3)
                   ("(node)|\n(other)\n" 2 0)))
    (ert-info ((format "%S" (list source offset column)))
      (with-temp-buffer
        (insert source)
        (query-ts-mode)
        (setq-local query-ts-mode-indent-offset offset)
        (setq-local indent-tabs-mode nil)
        (electric-indent-local-mode 1)
        (goto-char (query-ts-mode-test--position "|"))
        (delete-char 1)
        (call-interactively (key-binding (kbd "RET")))
        (should (= (current-column) column))))))

;;;; Updates

(ert-deftest query-ts-mode-refreshes-syntax-before-multiline-errors ()
  (dolist (narrow '(nil t))
    (with-temp-buffer
      (insert "((node)\n (#eq? @x \"foo\"))\n")
      (let ((treesit-font-lock-level 4)) (query-ts-mode))
      (syntax-propertize (point-max))
      (should (= (syntax-class (syntax-after 1)) 4))
      (goto-char (point-min))
      (search-forward "\"))")
      (backward-char 3)
      (let ((position (point)) (removed (char-after)))
        (save-restriction
          (when narrow (narrow-to-region (line-beginning-position) (point-max)))
          (delete-char 1)
          (syntax-propertize (point-max)))
        (query-ts-mode-test--should-match-fresh-buffer 4)
        (should (= (syntax-class (syntax-after 1)) 1))
        (goto-char position)
        (insert removed)
        (query-ts-mode-test--should-match-fresh-buffer 4)
        (should (= (syntax-class (syntax-after 1)) 4))))))

(ert-deftest query-ts-mode-updates-like-fresh-buffer ()
  (pcase-dolist (`(,source ,old ,new ,fragment ,face)
                 '(("(node) ; (a)\n" "(node)" "\"(node)\"" "node" font-lock-string-face)
                   ("(#eq? @x \"unfinished)\n" "unfinished" "finished\"" "finished"
                    font-lock-string-face)
                   ("(a \"b\")\n" "\"b\"" "\"b" "b" nil)
                   ("(node (child))\n" "(child)" "[(child)]" "[" font-lock-bracket-face)
                   ("(node field: (x))\n" "field" "_field" "_field"
                    font-lock-constant-face)
                   ("(node _field: (x))\n" "_field" "field" "field"
                    font-lock-property-use-face)
                   ("(x) @_cap\n" "(x)" "(_x)" "_x" font-lock-type-face)
                   ("_ _ @second\n" "_ _" "__" "__" font-lock-constant-face)
                   ("(node) @x\n" "(node) @x" "((node) @x (#eq? @x \"a\"))" "eq"
                    font-lock-function-call-face)))
    (ert-info ((format "%S: %S -> %S" source old new))
      (with-temp-buffer
        (insert source)
        (let ((treesit-font-lock-level 4)) (query-ts-mode))
        (query-ts-mode-test--buffer-state)
        (goto-char (query-ts-mode-test--position old))
        (delete-char (length old))
        (insert new)
        (font-lock-ensure)
        (should (eq (query-ts-mode-test--face fragment) face))
        (query-ts-mode-test--should-match-fresh-buffer 4)))))

(ert-deftest query-ts-mode-preserves-syntax-when-narrowed ()
  (with-temp-buffer
    (insert "; head\n(node \"; text\")\n; tail\n")
    (query-ts-mode)
    (narrow-to-region (query-ts-mode-test--position "(node") (point-max))
    (syntax-propertize (point-max))
    (should (nth 3 (syntax-ppss (query-ts-mode-test--position "text"))))
    (should-not (nth 4 (syntax-ppss (query-ts-mode-test--position "text"))))
    (should (nth 4 (syntax-ppss (query-ts-mode-test--position "tail"))))
    (widen)
    (should (nth 4 (syntax-ppss (query-ts-mode-test--position "head")))))
  (with-temp-buffer
    (insert "; head\n(node)\n")
    (query-ts-mode)
    (syntax-propertize (point-max))
    (goto-char (point-min))
    (insert "\"")
    (goto-char (line-end-position))
    (insert "\"")
    (narrow-to-region (query-ts-mode-test--position "(node") (point-max))
    (syntax-propertize (point-max))
    (widen)
    (should (nth 3 (syntax-ppss 3)))
    (should-not (nth 4 (syntax-ppss 3)))))

(provide 'query-ts-mode-test)

;;; query-ts-mode-test.el ends here
