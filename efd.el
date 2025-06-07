;;; efd.el --- Generate function call diagram -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Jean-François Arbour
;;
;; Author: Jean-François Arbour <jf.arbour@gmail.com>
;; Maintainer: Jean-François Arbour <jf.arbour@gmail.com>
;; Created: May 30, 2025
;; Modified: May 30, 2025
;; Version: 0.0.1
;; Keywords: convenience data docs extensions files help internal lisp local matching outlines processes tex text tools unix
;; Homepage: https://github.com/jfa/efd
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Generate function call diagram
;;
;;; Code:

(defun efd-generate (file-path)
  "Generate a hierarchical call graph for functions in FILE-PATH.
Returns an alist where each entry is (FUNCTION . CALLED-FUNCTIONS)."
  (let ((functions (efd-extract-functions file-path))
        (call-graph '()))
    (dolist (func functions)
      (let ((calls (efd-find-function-calls func file-path functions)))
        (push (cons (car func) calls) call-graph)))
      call-graph))

(defun efd-extract-functions (file-path)
  "Extract all function definitions from FILE-PATH.
Returns a list of (FUNCTION-NAME . START-POS) pairs."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (let ((functions '()))
      (while (re-search-forward "^\\s-*(defun\\s-+\\([^[:space:]()]+\\)" nil t)
        (let ((func-name (match-string 1))
              (func-start (+ (match-beginning 0) 1)))
          (push (cons func-name func-start) functions)))
      (nreverse functions))))

(defun efd-find-function-calls (func-info file-path functions)
  "Find all functions called within FUNC-INFO's definition in FILE-PATH.
FUNC-INFO is a cons cell (FUNCTION-NAME . START-POS) as returned
by `efd-extract-functions'.
FUNCTIONS is a list of such cons cells for all function definitions in the file."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (cdr func-info))
    (let ((func-end (efd-find-function-end))
          (calls '()))
      (while (and (< (point) func-end)
                  (re-search-forward "(\\([^[:space:]()]+\\)" func-end t))
          (let ((potential-call (match-string 1)))
            (when (and (assoc potential-call functions)
                       (not (string= potential-call (car func-info)))
                       (not (member potential-call calls)))
              (push potential-call calls))))
      (nreverse calls))))

(defun efd-find-function-end ()
  "Find the end position of the current function definition."
  (save-excursion
    (end-of-defun)
    (- (point) 1)))

(defun efd-call-graph-to-dot (call-alist &optional options)
  "Convert call graph to DOT with styling options.
OPTIONS is a plist that can contain:
  :graph-name - Name of the digraph
  :node-color - Color for nodes
  :edge-color - Color for edges
  :layout - Layout direction (TB, LR, BT, RL)"
  (let* ((graph-name (or (plist-get options :graph-name) "CallGraph"))
         (node-color (or (plist-get options :node-color) "lightblue"))
         (edge-color (or (plist-get options :edge-color) "black"))
         (layout (or (plist-get options :layout) "TB"))
         (dot-lines '()))

    ;; Start the digraph with styling
    (push (format "digraph %s {" graph-name) dot-lines)
    (push (format "  rankdir=%s;" layout) dot-lines)
    (push (format "  node [shape=box, style=\"rounded,filled\", fillcolor=%s];"
                  node-color) dot-lines)
    (push (format "  edge [color=%s];" edge-color) dot-lines)
    (push "" dot-lines)

    ;; Process entries
    (dolist (entry call-alist)
      (let ((caller (car entry))
            (callees (cdr entry)))
        (if callees
            (dolist (callee callees)
              (push (format "  \"%s\" -> \"%s\";" caller callee) dot-lines))
          (push (format "  \"%s\";" caller) dot-lines))))

    (push "" dot-lines)
    (push "}" dot-lines)

    (string-join (nreverse dot-lines) "\n")))

;; Utility function to write DOT to file
(defun efd-write-call-graph-dot (call-alist filename &optional options)
  "Write call graph DOT output to a file.
CALL-ALIST is the function call data.
FILENAME is the output file path.
OPTIONS are passed to call-graph-to-dot-styled."
  (with-temp-file filename
    (insert (efd-call-graph-to-dot call-alist options))))

(provide 'efd)
;;; efd.el ends here
