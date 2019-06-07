# Victor Miranda (vmm)

######1-Descarregue o arquivo .csv da planilha e imprima o dataframe obtido 
#exatamente do jeito que ele se encontra.
gotdf <- read.csv(file="got.csv", header=TRUE, sep=",", encoding = "UTF-8")
print(gotdf)
######2-Encontre a média, o desvio padrão e a moda das notas dos episódios.

My.dp <- function(column) {
  # a variância é o somatório das diferenças entre o valor e a média elevado ao quadrado
  # dividido pela quantidade de notas
  variancia = 0
  media = mean(gotdf[[column]])
  for(i in gotdf[[column]]){
    variancia = variancia + ((i - media) ^ 2)
  }
  variancia <- variancia / length(gotdf[[column]])
  # o desvio padrão é a raiz quadrada da variância
  dp = sqrt(variancia)
  return(dp)
}
My.moda <- function(column) {
  # salva os valores unicos da lista na variavel ux
  ux <- unique(gotdf[[column]])
  # conta as ocorrências de cada valor unico na lista
  tab <- tabulate(match(gotdf[[column]], ux))
  # retorna o valor unico que tem mais ocorrencias
  return(ux[tab == max(tab)])
}
print(mean(gotdf[["Nota"]]))
print(My.dp("Nota"))
print(My.moda("Nota"))

######3-Encontre a média, o desvio padrão e a mediana da audiência dos episódios.
print(mean(gotdf[["Audiencia.Em.milhoes."]]))
print(My.dp("Audiencia.Em.milhoes."))
print(median(gotdf[["Audiencia.Em.milhoes."]]))

######4-Faça uma função que retorna apenas os nomes dos episódios que possuem notas
#maiores ou iguais a nove (9).
notasmaiornove <- function() {
  return (gotdf[gotdf$Nota >= 9,]["Episodio"])
}
print(notasmaiornove())

######5-Faça uma função que retorna o nome dos episódios com menor e maior notas,
#nessa ordem para cada uma das temporadas. Por fim, faça um dataframe com cada episódio encontrado com as colunas 
#TÍTULO, NOTA, TEMPORADA ordenados de forma crescente por temporada
#(de 1 até 8).
menoremaior <- function() {

}

######6-Faça uma função que retorne qual a temporada com o menor desvio padrão na
#audiência.
menordesvio <- function() {
  desviosTemporadas = aggregate(Audiencia.Em.milhoes. ~ Temporada, gotdf, sd)
  return (head(desviosTemporadas[order(desviosTemporadas$Audiencia.Em.milhoes.),], 1))
}
print(menordesvio())


######7-Faça uma função que retorne a  média das notas dos episódios em que Brienne
#of Tarth(Gwendoline Christie) participa.

#filtra se contem Brienne e calcula a media
mediaBrienne <- function() {
  return (mean(gotdf[grepl("Brienne", gotdf$Personagens),][["Nota"]]))
}
print(mediaBrienne())

######8-Faça uma função que retorne uma lista com os personagens que só apareceram 
#em um único episódio na quarta (4ª) temporada.
unicoquarta <- function() {

}

######9-Faça uma função que dado o nome de um personagem, cria um histograma onde 
#mostra a frequência de aparição desse personagem a cada temporada. Não esqueça de dar um título e fazer ele 
#de forma colorida, facilitando a visualização. Um exemplo para o 
#personagem Bran Stark(Isaac Hempstead) seria:
histograma<- function(personagem) {
  
}
