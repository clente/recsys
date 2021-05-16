# Experimentos Inescapable Bias

## Continuação do que já foi feito

01. Quais são os filmes mais recomendados?
  - Usar os modelos já prontos e criar uma lista com os top 10-100

02. Os conjuntos de filmes mais recomendados são sempre parecidos?
  - Usar os modelos já prontos e ver a intersecção entre os top 10-100
  
03. Quais características os mais recomendados têm em comum?
  - Calcular onde as representações vetoriais batem

04. Os modelos treinados até agora podem ser considerados enviesados?
  - Usar o fairmodels ou as definições usadas no artigo

05. Como é o grafo das recomendações?
  - Desenhar um grafo conectando filmes que se recomendam

## Novos experimentos (depois de implementar o TF-R)

01. Para uma mesma pessoa e um modelo estático, as recomendações variam quanto?
  - Rodar o algoritmo várias vezes e capturar as variações naturais

02. Quão diferentes podem ser as recomendações de duas pessoas em um modelo estático?
  - Criar vários usuários aleatórios e capturar as variações

03. Quanto mudam as recomendações se o modelo é retreinado depois de cada escolha?
  - Fazer um usuário escolher filmes aleatoriamente e retreinar o modelo

04. Quantos usuários são necessários para criar mudanças sensíveis?
  - Usar um número cada vez maior de usuários escolhendo os mesmos filmes

05. Se o modelo é retreinado sempre, como mudam as recomendações se o usuário sempre segue?
  - Fazer o usuário sempre seguir a recomendação e ver como mudam as próximas

06. Quantos usuários são necessários para mudar a próxima recomendação?
  - Fazer um usuário tentar desviar dos demais e ver como mudam as recomendações

07. Como ficam as recomendações se o usuário não obedece?
  - Fazer um usuário escolher filmes aleatórios e ver o que acontece no retreino

08. Como ficam as recomendações se um usuário desvia?
  - Fazer um usuário desviar da maioria e ver o que acontece no retreino

09. Como ficam as recomendações com dois conjuntos de usuários competindo?
  - Analisar um usuário novo se há dois conjuntos escolhendo filmes distintos

10. O que acontece se soltarmos vários usuários obedientes?
  - Entender a interação entre vários usuários que seguem as recomendações
  
11. O que acontece se soltarmos vários usuários desobedientes?
  - Entender a interação entre vários usuários que não seguem as recomendações

12. O que acontece com as recomendações se um usuário insiste no mesmo vídeo?
  - Mapear a variação nas recomendações se o usuário não muda de comportamento

13. O que faz mais diferença: muitos usuários ou poucos usuários engajados?
  - Testar vários usuários vendo um conjunto por pouco tempo e poucos vendo por muito
