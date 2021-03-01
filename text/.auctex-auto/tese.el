(TeX-add-style-hook
 "tese"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("book" "12pt" "twoside" "brazil" "english")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "a4paper") ("biblatex" "natbib=true" "style=extras/plainnat-ime" "")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url")
   (TeX-run-style-hooks
    "latex2e"
    "extras/basics"
    "extras/fonts"
    "extras/floats"
    "extras/thesis-formatting"
    "extras/index"
    "extras/hyperlinks"
    "extras/source-code"
    "extras/utils"
    "extras/bibconfig"
    "conteudo/folhas-de-rosto"
    "conteudo/capitulos"
    "book"
    "bk12"
    "geometry"
    "biblatex"
    "imeusp-capa")
   (LaTeX-add-labels
    "bibliografia")
   (LaTeX-add-bibliographies
    "bibliografia"))
 :latex)

