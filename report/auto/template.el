(TeX-add-style-hook
 "template"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("jpaper" "pageno")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("ulem" "normalem")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "latex2e"
    "jpaper"
    "jpaper10"
    "ulem"
    "amsfonts"
    "amssymb"
    "amsmath"
    "bussproofs"
    "syntax"
    "textgreek"
    "listings"
    "color")
   (TeX-add-symbols
    "IWreport"
    "step"
    "bstep")
   (LaTeX-add-bibliographies
    "references")))

