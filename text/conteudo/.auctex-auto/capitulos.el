(TeX-add-style-hook
 "capitulos"
 (lambda ()
   (TeX-run-style-hooks
    "conteudo/01-introducao"
    "conteudo/02-bibliografia"
    "conteudo/03-desenvolvimento"
    "conteudo/04-exemplos"))
 :latex)
