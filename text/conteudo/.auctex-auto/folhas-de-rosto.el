(TeX-add-style-hook
 "folhas-de-rosto"
 (lambda ()
   (TeX-run-style-hooks
    "conteudo/resumo-abstract")
   (TeX-add-symbols
    '("disablenewpage" 1)
    "clearpage"
    "cleardoublepage"))
 :latex)

