(TeX-add-style-hook
 "04-exemplos"
 (lambda ()
   (TeX-run-style-hooks
    "figuras/gnuplot"
    "figuras/matplotlib")
   (LaTeX-add-labels
    "chap:exemplos"
    "orphanchar"
    "eq:2grau"
    "eq:bhaskara"
    "eq:delta"
    "fig:gantt"
    "prog:java"
    "fig:gnuplot"
    "fig:matplotlib"
    "fig:graficos"
    "tab:amino_acidos"
    "tab:numeros"
    "tab:ficha")
   (LaTeX-add-index-entries
    "biblatex"
    "Modo Matemático"
    "Floats"
    "Legendas"
    "Subfiguras"
    "Java"))
 :latex)

