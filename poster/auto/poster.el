(TeX-add-style-hook
 "poster"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("baposter" "portrait" "final" "paperwidth=40in" "paperheight=40in")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("beramono" "scaled") ("fontenc" "T1")))
   (TeX-run-style-hooks
    "latex2e"
    "baposter"
    "baposter10"
    "calc"
    "graphicx"
    "amsmath"
    "amssymb"
    "relsize"
    "multirow"
    "multicol"
    "pgfbaselayers"
    "bussproofs"
    "listings"
    "beramono"
    "fontenc")
   (TeX-add-symbols
    '("colouredcircle" 1)
    "captionfont"
    "compresslist"
    "lsti")
   (LaTeX-add-lengths
    "leftimgwidth")))

