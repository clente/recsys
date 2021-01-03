(TeX-add-style-hook
 "01-introducao"
 (lambda ()
   (LaTeX-add-labels
    "cap:introduction"
    "sec:social_networks"
    "sec:recommender_systems"
    "sec:radicalization"
    "sec:hypothesis"
    "sec:consideracoes_preliminares")
   (LaTeX-add-index-entries
    "Notas de rodapé"
    "natbib"
    "Língua estrangeira"
    "Legendas"
    "Floats"
    "bibtex"
    "biblatex"
    "biber"
    "Google Scholar"
    "Web of Science"
    "Scopus"
    "Zotero"
    "Mendeley"
    "CiteULike"
    "Tese/Dissertação!versões"
    "Formatação"
    "Tese/Dissertação!itens obrigatórios"
    "Tese/Dissertação!itens opcionais"))
 :latex)

