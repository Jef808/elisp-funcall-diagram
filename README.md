## EFD - Emacs Function Diagram Generator

Generate function call diagrams from Emacs Lisp files.

### What it does

EFD analyzes Emacs Lisp files to extract function definitions and their
call relationships, then generates DOT format diagrams that can be
rendered with Graphviz.

### Usage

``` elisp
;; Generate call graph from a file
(setq call-graph (efd-generate "path/to/your-file.el"))

;; Convert to dot format and write the graph to some file 
(efd--write-call-graph-dot call-graph "output.dot"
                           '(:graph-name "MyProject"
                             :node-color "lightgreen"))
```

### Generate diagram

``` bash
# Convert DOT to PNG using Graphviz
dot -Tpng output.dot -o diagram.png
```

### Requirements

- Emacs 24.3+
- Graphviz (for rendering diagrams)
