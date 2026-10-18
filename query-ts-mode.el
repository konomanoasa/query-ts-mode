;;; query-ts-mode.el --- Tree-sitter mode for Tree-sitter queries  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 konomanoasa
;;
;; Author: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Maintainer: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: languages
;; URL: https://github.com/konomanoasa/query-ts-mode
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

;;; Commentary:
;;
;; Tree-sitter major mode for Tree-sitter queries.

;;; Code:

(require 'elec-pair)
(require 'newcomment)
(require 'treesit)

(defgroup query-ts nil
  "Tree-sitter mode for Tree-sitter queries."
  :group 'languages)

;;;; Grammar

(defconst query-ts-mode--grammar-sources
  '((query "https://github.com/konomanoasa/tree-sitter-query"
           :revision "v0.3.2"))
  "Tree-sitter grammar sources for Tree-sitter queries.")

(defun query-ts-mode--ensure-grammar (language)
  "Ensure that the grammar for LANGUAGE is installed."
  (let ((treesit-language-source-alist
         (if (assq language treesit-language-source-alist)
             treesit-language-source-alist
           (cons (assq language query-ts-mode--grammar-sources)
                 treesit-language-source-alist))))
    (or (treesit-ensure-installed language)
        (user-error "Tree-sitter grammar `%s' is unavailable" language))))

;;;; Incremental Updates

(defconst query-ts-mode-update--containers
  nil
  "Container nodes traversed to find an edited source unit.")

(defvar-local query-ts-mode-update--state nil
  "Bounds and recovery state captured before a buffer edit.")

(defvar-local query-ts-mode-update--pending-range nil
  "Edited owner range awaiting syntax propertization.")

(defun query-ts-mode-update--bounds (beg end)
  "Return enclosing source-unit bounds around BEG and END."
  (let ((start beg)
        (finish end)
        (root (treesit-parser-root-node treesit-primary-parser)))
    (dolist (position (list (max (point-min) (1- beg))
                            (min (point-max) end)))
      (let ((node (treesit-node-at position treesit-primary-parser))
            owner)
        (while (and node (not (treesit-node-eq node root)))
          (unless (member (treesit-node-type node)
                          query-ts-mode-update--containers)
            (setq owner node))
          (setq node (treesit-node-parent node)))
        (when owner
          (setq start (min start (treesit-node-start owner))
                finish (max finish (treesit-node-end owner))))))
    (cons start finish)))

(defun query-ts-mode-update--before-change (beg end)
  "Save owner bounds and old-tree recovery state before changing BEG to END."
  (when treesit-primary-parser
    (save-restriction
      (widen)
      (setq query-ts-mode-update--state
            (cons (query-ts-mode-update--bounds beg end)
                  (treesit-node-check
                   (treesit-parser-root-node treesit-primary-parser)
                   'has-error))))))

(defun query-ts-mode-update--reset-parser ()
  "Replace the primary parser without reusing its old tree."
  (let* ((parser treesit-primary-parser)
         (language (treesit-parser-language parser))
         (tag (treesit-parser-tag parser))
         (ranges (treesit-parser-included-ranges parser))
         (notifiers (treesit-parser-notifiers parser))
         (level (treesit-parser-embed-level parser)))
    (treesit-parser-delete parser)
    (setq treesit-primary-parser (treesit-parser-create language nil nil tag))
    (treesit-parser-set-included-ranges treesit-primary-parser ranges)
    (treesit-parser-set-embed-level treesit-primary-parser level)
    (dolist (notifier notifiers)
      (treesit-parser-add-notifier treesit-primary-parser notifier))))

(defun query-ts-mode-update--after-change (beg end old-length)
  "Refresh edited owners and avoid stale trees after BEG, END and OLD-LENGTH."
  (when treesit-primary-parser
    (let ((narrowed (buffer-narrowed-p)))
      (save-restriction
        (widen)
        (let* ((state query-ts-mode-update--state)
               (reset (or (cdr state)
                          (and (= beg 1) (> old-length 0)
                               (eq (char-after 1) #xfeff)))))
          (setq query-ts-mode-update--state nil)
          (when reset (query-ts-mode-update--reset-parser))
          (let* ((bounds (if reset (cons (point-min) (point-max))
                           (query-ts-mode-update--bounds beg end)))
                 (start (min (car bounds) (or (caar state) beg)))
                 (finish (min (point-max)
                              (max (cdr bounds)
                                   (+ (or (cdar state) (+ beg old-length))
                                      (- end beg old-length)))))
                 (pending query-ts-mode-update--pending-range)
                 (pending-end (cdr pending)))
            (when (and pending-end (>= pending-end beg))
              (setq pending-end (max end (+ pending-end (- end beg old-length)))))
            (setq query-ts-mode-update--pending-range
                  (cons (min start (or (car pending) start))
                        (min (point-max) (max finish (or pending-end finish)))))
            (syntax-ppss-flush-cache start)
            (font-lock-flush start finish)
            (when narrowed (syntax-propertize finish))))))))

(defun query-ts-mode-update--extend-region (start end)
  "Include edited owners when propertizing START to END."
  (when query-ts-mode-update--pending-range
    (let ((bounds query-ts-mode-update--pending-range))
      (setq query-ts-mode-update--pending-range nil)
      (cons (max (point-min) (min start (car bounds)))
            (min (point-max) (max end (cdr bounds)))))))

(defun query-ts-mode-update--setup ()
  "Configure incremental updates for the current buffer."
  (add-hook 'before-change-functions #'query-ts-mode-update--before-change nil t)
  (add-hook 'after-change-functions #'query-ts-mode-update--after-change nil t)
  (add-hook 'syntax-propertize-extend-region-functions
            #'query-ts-mode-update--extend-region nil t))

;;;; Syntax

(defvar query-ts-mode-syntax--text-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (dolist (character '(?\; ?\" ?\' ?` ?\\ ?\( ?\) ?\[ ?\] ?{ ?}))
      (modify-syntax-entry character "." table))
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for text without a CST syntax classification.")

(defvar query-ts-mode-syntax-table
  (let ((table (copy-syntax-table query-ts-mode-syntax--text-table)))
    (dolist (entry '((?\( . "()") (?\) . ")(") (?\[ . "(]") (?\] . ")[")))
      (modify-syntax-entry (car entry) (cdr entry) table))
    table)
  "Syntax table for `query-ts-mode'.")

;;;;; Syntax Queries

(defconst query-ts-mode-syntax--query
  (treesit-query-compile
   'query
   '((comment) @comment
     (string) @string
     (node_pattern ["(" ")"] @delimiter)
     (group ["(" ")"] @delimiter)
     (predicate ["(" ")"] @delimiter)
     (directive ["(" ")"] @delimiter)
     (alternation ["[" "]"] @delimiter)))
  "Compiled syntax query for Tree-sitter queries.")

;;;;; Propertization

(defvar-local query-ts-mode-syntax--changed-start nil
  "Earliest pending syntax change reported by the parser.")

(defun query-ts-mode-syntax--changed (ranges _parser)
  "Invalidate syntax properties for the parser's changed RANGES."
  (dolist (range ranges)
    (setq query-ts-mode-syntax--changed-start
          (min (or query-ts-mode-syntax--changed-start (car range))
               (car range))))
  (when query-ts-mode-syntax--changed-start
    (syntax-ppss-flush-cache query-ts-mode-syntax--changed-start)))

(defun query-ts-mode-syntax--extend-region (start end)
  "Extend START and END to include pending structural changes."
  (treesit-parser-root-node treesit-primary-parser)
  (let ((begin query-ts-mode-syntax--changed-start))
    (setq query-ts-mode-syntax--changed-start nil)
    (when (and begin (< begin start))
      (cons (max (point-min) begin) end))))

(defun query-ts-mode-syntax--propertize (start end)
  "Apply syntax properties between START and END."
  (let ((accessible-start (point-min)))
    (save-restriction
      (widen)
      (when (and (= start accessible-start)
                 (> accessible-start (point-min)))
        (setq start (point-min))
        (syntax-ppss-flush-cache start))
      (put-text-property start end 'syntax-table query-ts-mode-syntax--text-table)
      (dolist (capture (treesit-query-capture
                        (treesit-parser-root-node treesit-primary-parser)
                        query-ts-mode-syntax--query start end))
        (let* ((node (cdr capture))
               (begin (treesit-node-start node))
               (finish (treesit-node-end node)))
          (unless (or (= begin finish) (treesit-node-check node 'missing))
            (pcase (car capture)
              ('comment
               (put-text-property begin (1+ begin) 'syntax-table
                                  (string-to-syntax "<")))
              ('string
               (put-text-property begin finish 'syntax-table
                                  query-ts-mode-syntax--text-table)
               (let ((opening (treesit-node-child node 0))
                     (closing (treesit-node-child node -1)))
                 (when (and (not (treesit-node-check node 'has-error))
                            (equal (treesit-node-type opening) "\"")
                            (equal (treesit-node-type closing) "\"")
                            (< (treesit-node-start opening)
                               (treesit-node-start closing)))
                   (put-text-property (treesit-node-start opening)
                                      (treesit-node-end opening)
                                      'syntax-table (string-to-syntax "|"))
                   (put-text-property (treesit-node-start closing)
                                      (treesit-node-end closing)
                                      'syntax-table (string-to-syntax "|")))))
              ('delimiter
               (put-text-property begin finish 'syntax-table
                                  (string-to-syntax
                                   (pcase (treesit-node-type node)
                                     ("(" "()")
                                     (")" ")(")
                                     ("[" "(]")
                                     ("]" ")["))))))))))))

;;;;; Setup

(defun query-ts-mode-syntax--setup ()
  "Configure syntax handling for the current buffer."
  (treesit-parser-add-notifier treesit-primary-parser #'query-ts-mode-syntax--changed)
  (setq-local syntax-propertize-function
              #'query-ts-mode-syntax--propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t)
  (add-hook 'syntax-propertize-extend-region-functions
            #'query-ts-mode-syntax--extend-region t t))

;;;; Comment Commands

(defun query-ts-mode-comment--uncomment-region (beg end &optional arg)
  "Uncomment BEG through END using syntax classified before editing.
Pass ARG to `uncomment-region-default'."
  (syntax-propertize end)
  (unwind-protect
      (let ((syntax-propertize-function nil))
        (uncomment-region-default beg end arg))
    (syntax-ppss-flush-cache beg)))

(defun query-ts-mode-comment--setup ()
  "Configure comment commands for the current buffer."
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";+[ \t\v\f]*")
  (setq-local comment-use-syntax t)
  (setq-local uncomment-region-function #'query-ts-mode-comment--uncomment-region))

;;;; Electric Pair

(defun query-ts-mode-electric-pair--newline-context-p ()
  "Return non-nil between adjacent delimiters of a Query structure."
  (when (and (eq (char-before) ?\n)
             (>= (- (point) 2) (point-min))
             (< (point) (point-max)))
    (let* ((opening (treesit-node-at (- (point) 2) treesit-primary-parser))
           (closing (treesit-node-at (point) treesit-primary-parser))
           (owner (treesit-node-parent opening)))
      (and (= (treesit-node-start opening) (- (point) 2))
           (= (treesit-node-end opening) (1- (point)))
           (= (treesit-node-start closing) (point))
           (= (treesit-node-end closing) (1+ (point)))
           (treesit-node-eq owner (treesit-node-parent closing))
           (member (list (treesit-node-type owner)
                         (treesit-node-type opening)
                         (treesit-node-type closing))
                   '(("node_pattern" "(" ")") ("group" "(" ")")
                     ("predicate" "(" ")") ("directive" "(" ")")
                     ("alternation" "[" "]")))))))

(defun query-ts-mode-electric-pair--setup ()
  "Configure electric pairing for the current buffer."
  (let ((pairs '((?\( . ?\)) (?\[ . ?\]) (?\" . ?\")))
        (table (copy-syntax-table (syntax-table))))
    (setq-local electric-pair-pairs (append electric-pair-pairs pairs))
    (dolist (pair pairs)
      (unless (eq (cdr (assq (car pair) electric-pair-pairs)) (cdr pair))
        (modify-syntax-entry (car pair) "." table)))
    (set-syntax-table table))
  (let ((setting electric-pair-open-newline-between-pairs))
    (setq-local electric-pair-open-newline-between-pairs
                (lambda ()
                  (and (if (functionp setting) (funcall setting) setting)
                       (query-ts-mode-electric-pair--newline-context-p))))))

;;;; Font Lock

;;;;; Features

(defconst query-ts-mode-font-lock--feature-list
  '((comment)
    (type property variable function keyword string)
    (constant escape)
    (operator punctuation bracket))
  "Font-lock features by decoration level.")

;;;;; Settings

(defun query-ts-mode-font-lock--settings ()
  "Return font-lock settings for the current buffer."
  (treesit-font-lock-rules
   :default-language 'query

   :feature 'comment
   '((comment) @font-lock-comment-face)

   :feature 'type
   '((node_pattern node: (identifier) @font-lock-type-face)
     (supertype (identifier) @font-lock-type-face)
     (missing_node type: (identifier) @font-lock-type-face))

   :feature 'property
   '((field_constraint name: (identifier) @font-lock-property-use-face)
     (negated_field name: (identifier) @font-lock-property-use-face))

   :feature 'variable
   '((pattern capture: (capture name: (identifier) @font-lock-variable-name-face))
     (predicate argument: (capture name: (identifier) @font-lock-variable-use-face))
     (directive argument: (capture name: (identifier) @font-lock-variable-use-face)))

   :feature 'function
   '((predicate name: (identifier) @font-lock-function-call-face)
     (directive name: (identifier) @font-lock-function-call-face))

   :feature 'keyword
   '((missing_node "MISSING" @font-lock-keyword-face))

   :feature 'string
   '((string ["\"" (string_content)] @font-lock-string-face)
     (predicate argument: (identifier) @font-lock-string-face)
     (directive argument: (identifier) @font-lock-string-face))

   :feature 'constant
   '((wildcard) @font-lock-constant-face)

   :feature 'escape
   '((escape_sequence) @font-lock-escape-face)

   :feature 'operator
   '([(quantifier) (anchor)] @font-lock-operator-face
     (negated_field "!" @font-lock-negation-char-face))

   :feature 'punctuation
   '((capture "@" @font-lock-punctuation-face)
     (predicate ["#" "?"] @font-lock-punctuation-face)
     (directive ["#" "!"] @font-lock-punctuation-face)
     (field_constraint ":" @font-lock-punctuation-face)
     (supertype "/" @font-lock-punctuation-face))

   :feature 'bracket
   '((node_pattern ["(" ")"] @font-lock-bracket-face)
     (group ["(" ")"] @font-lock-bracket-face)
     (alternation ["[" "]"] @font-lock-bracket-face)
     (predicate ["(" ")"] @font-lock-bracket-face)
     (directive ["(" ")"] @font-lock-bracket-face))))

;;;;; Setup

(defun query-ts-mode-font-lock--setup ()
  "Configure font lock for the current buffer."
  (setq-local treesit-font-lock-feature-list
              query-ts-mode-font-lock--feature-list)
  (setq-local treesit-font-lock-settings
              (query-ts-mode-font-lock--settings)))

;;;; Navigation

(defun query-ts-mode-navigation--sexp-p (node)
  "Return non-nil if NODE is a representative editing unit."
  (let ((type (treesit-node-type node))
        (parent (treesit-node-parent node)))
    (and (< (treesit-node-start node) (treesit-node-end node))
         (or (member type '("identifier" "wildcard" "string" "capture"
                            "supertype" "missing_node" "pattern"
                            "field_constraint" "negated_field"))
             (and (member type '("node_pattern" "group" "alternation"
                                 "predicate" "directive"))
                  (not (and (equal (treesit-node-type parent) "pattern")
                            (= (treesit-node-start parent) (treesit-node-start node))
                            (= (treesit-node-end parent) (treesit-node-end node)))))))))

(defun query-ts-mode-navigation--defun-p (node)
  "Return non-nil if NODE is a top-level Query item."
  (and (member (treesit-node-type node)
               '("pattern" "field_constraint" "predicate" "directive"))
       (equal (treesit-node-type (treesit-node-parent node)) "query")))

(defconst query-ts-mode-navigation--settings
  '((query
     (sexp query-ts-mode-navigation--sexp-p)
     (defun query-ts-mode-navigation--defun-p)))
  "Tree-sitter thing definitions for Tree-sitter queries.")

(defun query-ts-mode-navigation--setup ()
  "Configure navigation for the current buffer."
  (setq-local treesit-thing-settings
              query-ts-mode-navigation--settings)
  (setq-local treesit-defun-tactic 'top-level)
  (setq-local treesit-defun-skipper nil))

;;;; Indentation

(defcustom query-ts-mode-indent-offset 2
  "Number of spaces for each indentation level."
  :type 'natnum
  :group 'query-ts)

(defun query-ts-mode-indent--inside-string-p (node parent bol)
  "Return non-nil if BOL, at NODE or in PARENT, is inside a string."
  (let ((string (treesit-parent-until (or node parent) "^string$" t)))
    (and string (< (treesit-node-start string) bol))))

(defun query-ts-mode-indent--keep (_node _parent bol)
  "Return BOL to preserve the current indentation."
  bol)

(defconst query-ts-mode-indent--rules
  `((query
     (query-ts-mode-indent--inside-string-p query-ts-mode-indent--keep 0)
     ((node-is "^[])]$") parent-bol 0)
     ((parent-is "^query$") column-0 0)
     ((parent-is ,(rx string-start
                      (or "node_pattern" "group" "alternation" "predicate"
                          "directive" "pattern" "field_constraint")
                      string-end))
      parent-bol query-ts-mode-indent-offset)))
  "Tree-sitter indentation rules for Tree-sitter queries.")

(defun query-ts-mode-indent--setup ()
  "Configure indentation for the current buffer."
  (setq-local treesit-simple-indent-rules
              query-ts-mode-indent--rules))

;;;; Mode

(defun query-ts-mode--setup ()
  "Configure `query-ts-mode' in the current buffer."
  (query-ts-mode--ensure-grammar 'query)
  (setq-local treesit-primary-parser (treesit-parser-create 'query))
  (query-ts-mode-syntax--setup)
  (query-ts-mode-comment--setup)
  (query-ts-mode-electric-pair--setup)
  (query-ts-mode-font-lock--setup)
  (query-ts-mode-navigation--setup)
  (query-ts-mode-indent--setup)
  (treesit-major-mode-setup)
  (query-ts-mode-update--setup))

;;;###autoload
(define-derived-mode query-ts-mode prog-mode "Query-TS"
  "Major mode for editing Tree-sitter queries."
  :syntax-table query-ts-mode-syntax-table
  :group 'query-ts
  (query-ts-mode--setup))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\(?:\\`\\|/\\)queries/\\(?:[^/]+/\\)*[^/]+\\.scm\\'" . query-ts-mode))

(provide 'query-ts-mode)

;;; query-ts-mode.el ends here
