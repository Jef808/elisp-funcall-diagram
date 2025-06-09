;;; efd.el --- Generate function call diagram -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Jean-François Arbour
;;
;; Author: Jean-François Arbour <jf.arbour@gmail.com>
;; Maintainer: Jean-François Arbour <jf.arbour@gmail.com>
;; Created: May 30, 2025
;; Modified: May 30, 2025
;; Version: 0.0.1
;; Keywords: convenience docs extensions files help internal lisp local matching outlines processes tex text tools
;; Homepage: https://github.com/jfa/efd
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Very simple tool to generate function call diagram.
;;
;;; Code:

(defun efd-compile-function-calls (file-paths)
  "Generate a list of function call for functions in FILE-PATHS.
FILE-PATHS can be a single file path string or a list of file paths.
Returns an alist of (FILE-PATH . FUNCTION-CALLS) where FUNCTION-CALLS
is a list of (FUNC-NAME . (CALLEE1 CALLEE2 ...)) cons cells."
  (let ((functions (efd-extract-functions file-paths))
        (file-function-calls '()))
    (dolist (func-info functions)
      (let* ((file-path (car func-info))
             (func-name (cadr func-info))
             (calls (efd-find-function-calls func-info functions))
             (file-entry (assoc file-path file-function-calls)))
        (if file-entry
            (setcdr file-entry (cons (cons func-name calls) (cdr file-entry)))
          (push (cons file-path (list (cons func-name calls))) file-function-calls))))
    file-function-calls))


(defun efd-extract-functions (file-paths)
  "Extract all function definitions from FILE-PATHS.
FILE-PATHS can be a single file path string or a list of file paths.
Returns a list of (FILE-PATH . (FUNC-NAME . FUNC-START)) tuples."
  (with-temp-buffer
    (let ((functions '())
          (files (if (listp file-paths) file-paths (list file-paths))))
      (dolist (file-path files)
        (let ((file-start (point-max)))
          (goto-char (point-max))
          (insert-file-contents file-path)
          (goto-char file-start)
          (while (re-search-forward "^\\s-*(defun\\s-+\\([^[:space:]()]+\\)" nil t)
            (let ((func-name (match-string 1))
                  (func-start (+ (match-beginning 0) 1)))
              (push (cons file-path (cons func-name func-start)) functions)))))
      (nreverse functions))))

(defun efd-find-function-calls (func-info functions)
  "Find all functions called within FUNC-INFO's definition.
FUNC-INFO is a tuple (FILE-PATH . (FUNC-NAME . FUNC-START)) as the entries
in the list returned by `efd-extract-functions1'.
FUNCTIONS is a list of such tuples."
  (let ((file-path (car func-info))
        (func-name (cadr func-info))
        (func-start (cddr func-info))
        (func-names (mapcar (lambda (entry) (cadr entry)) functions)))
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char func-start)
      (let ((func-end (efd-find-function-end))
            (calls '()))
        (while (and (< (point) func-end)
                    (re-search-forward "(\\([^[:space:]()]+\\)" func-end t))
          (let ((potential-call (match-string 1)))
            (when (and (member potential-call func-names)
                       (not (string= potential-call func-name))
                       (not (member potential-call calls)))
              (push potential-call calls))))
        (nreverse calls)))))

(defun efd-find-function-end ()
  "Find the end position of the current function definition."
  (save-excursion
    (end-of-defun)
    (- (point) 1)))

(defun efd-function-calls-to-dot (function-calls &optional options)
  "Convert FUNCTION-CALLS to a dot graph with styling options.
OPTIONS is a plist that can contain:
  :graph-name - Name of the digraph
  :node-color - Color for nodes
  :edge-color - Color for edges
  :layout - Layout direction (TB, LR, BT, RL)"
  (let* ((graph-name (or (plist-get options :graph-name) "CallGraph"))
         (node-color (or (plist-get options :node-color) "lightgreen"))
         (edge-color (or (plist-get options :edge-color) "black"))
         (layout (or (plist-get options :layout) "LR"))
         (dot-lines '()))
    (push (format "digraph %s {" graph-name) dot-lines)
    (push (format "  rankdir=%s;" layout) dot-lines)
    (push (format "  node [shape=box, style=\"rounded,filled\", fillcolor=%s];"
                  node-color) dot-lines)
    (push (format "  edge [color=%s];" edge-color) dot-lines)
    (push "" dot-lines)
    (dolist (entry function-calls)
      (let ((caller (car entry))
            (callees (cdr entry)))
        (if callees
            (dolist (callee callees)
              (push (format "  \"%s\" -> \"%s\";" caller callee) dot-lines))
          (push (format "  \"%s\";" caller) dot-lines))))
    (push "" dot-lines)
    (push "}" dot-lines)
    (string-join (nreverse dot-lines) "\n")))

(defun efd-write-call-graph-dot (function-calls filename &optional options)
  "Write call graph DOT output to a file.
FUNCTION-CALLS is the function call data.
FILENAME is the output file path.
OPTIONS are passed to call-graph-to-dot-styled."
  (with-temp-file filename
    (insert (efd-function-calls-to-dot function-calls options))))

(provide 'efd)
;;; efd.el ends here
