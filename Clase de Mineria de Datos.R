install.packages('tm', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('wordcloud', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('ggplot2', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('dplyr', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('readr', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('cluster', dependencies=TRUE, repos='http://cran.rstudio.com/')

library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(readr)
library(cluster)

libro <- read_lines("C:/Users/orlando/curso/Datos/LasMil.txt", skip = 790, n_max = 6631-790)
#dado que estoy trabajando en una maquina virtual cargo el archvio y en el codigo
#lo llamo simplemente, en el R Studio de la maquina si de debe nombrar la ubicacion 

summary(libro)
str(libro)


View(libro)
length(libro)
libro[0:length(libro)]
max(nchar(libro))

diez <- rep(1:ceiling(length(libro)/10), each = 10)
length(diez)
diez <- diez[1:length(libro)]
estructuraLibro <- cbind(diez, libro) %>% data.frame()
str(estructuraLibro)

texto <- aggregate(formula = libro~ diez, data = estructuraLibro , FUN = paste, collapse = " ")

dim(texto)
texto <- texto %>% select(libro) %>% as.matrix
dim(texto)

texto <- gsub("[[:cntrl:]]", " ", texto)
texto <- tolower(texto)
texto <- removeWords(texto, words = stopwords("spanish"))
texto <- removePunctuation(texto)
texto <- removeNumbers(texto)
texto <- stripWhitespace(texto)
texto <- gsub("!"," ",texto)
texto <- gsub("[¿|«|»]"," ", texto)
mil_corpus <- Corpus(VectorSource(texto))
class(mil_corpus)
mil_mapeo <- tm_map(mil_corpus, PlainTextDocument)
wordcloud(mil_mapeo$content$content, 
          max.words = 50, random.order = F, 
          colors = brewer.pal(name = "Dark2", n = 8))
texto <- removeWords(texto, words = c("entonces","tal", "tan", "así", "aquí", "dijo", "pues"))
mil_corpus <- texto %>% VectorSource() %>% Corpus()
mil_mapeo <- mil_corpus %>% tm_map(PlainTextDocument)
wordcloud(
  mil_mapeo$content$content, max.words = 100, 
  random.order = F, 
  colors=brewer.pal(name = "Dark2", n = 8)
)

tdm = TermDocumentMatrix(mil_corpus)
tdm

mil_matriz = as.matrix(tdm)

mil_matriz <- mil_matriz %>% rowSums() %>% sort(decreasing = TRUE)
mil_matriz <- data.frame(palabra = names(mil_matriz), frec = mil_matriz)
mil_matriz

wordcloud(
  words = mil_matriz$palabra, 
  freq = mil_matriz$frec, 
  max.words = 100, 
  random.order = F, 
  colors=brewer.pal(name = "Dark2", n = 8)
)

mil_matriz[1:12, ] %>%
  ggplot(aes(palabra, frec)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = frec)) + 
  coord_flip() + 
  labs(title = "Doce palabras mas frecuentes",  x = "Palabras", y = "Número de uso")

mil_matriz %>%
  mutate(perc = (frec/sum(frec))*100) %>%
  .[1:10, ] %>%
  ggplot(aes(palabra, perc)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = round(perc, 2))) + 
  coord_flip() +
  labs(title = "Las Diez palabras más frecuentes", x = "Palabras", y = "Porcentaje de uso")

mil_nuevo <- removeSparseTerms(tdm, sparse = .95)
mil_nuevo
mil_nuevo <- mil_nuevo %>% as.matrix()
mil_nuevo <- mil_nuevo / rowSums(mil_nuevo)
mil_distancia <- dist(mil_nuevo, method = "euclidian")
mil_hclust <-  hclust(mil_distancia, method = "ward.D")
plot(mil_hclust, main = "Dendrograma de Niebla - hclust", sub = "", xlab = "")