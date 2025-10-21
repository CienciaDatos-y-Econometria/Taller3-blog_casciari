# =============================================================
# Pipeline: este archivo es la primera parte de proyecto de 
#            desarrollo para un recomendador de cuentos para
#            el escritor Hernán Casciari. Aquí desarrollamos
#            el pipeline de los datos, obteniéndolos de la DB
#            para su procesamiento correcto en pos de emplearlos
#            como insumo para el recomendador.
#
# 1) Buenas prácticas, librerías y descarga de datos 
# 2) Análisis cuentos pre-procesamiento
# 3) Procesamiento cuentos a tokens
# 
# =============================================================

# =============================================================
# 1) Buenas prácticas, librerías y descarga de datos
# =============================================================

# Buenas prácticas
rm(list = ls())

# NOTA: CAMBIAR DIRECTORIO
# setwd("C:/Users/Asuar/OneDrive/Escritorio/Libros Clases/Economía/Ciencia Datos y Econometria/Taller3-blog_casciari")

# Librerías
require(pacman)
p_load(tidyverse, stringr, dplyr, stringi, tm, stopwords) #TODO

# Datos y primera visualización
db <- read.csv("stores/blog_casciari.csv")
head(db)


# =============================================================
# 2) Análisis  cuentos pre-procesamiento
# =============================================================

# Ver final de cada cuento (últimas 5)
sapply(str_split(db$cuento, "\\s+"), function(x) paste(tail(x, 5), collapse = " "))


# Ver distribución tamaño string
# TODO ponerlo más lindo
db %>%
  mutate(largo = nchar(cuento)) %>%
  ggplot(aes(x = largo)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribución del tamaño de los cuentos (caracteres)",
       x = "Número de caracteres", y = "Frecuencia")

# Cambiar formato fecha
db <- db %>%
  mutate(fecha = as.Date(fecha, format = "%m/%d/%y"))

# Ver distribución por fecha
db %>%
  ggplot(aes(x = fecha)) +
  geom_histogram(binwidth = 30, fill = "orange", color = "black") +
  labs(title = "Distribución de cuentos por fecha",
       x = "Fecha", y = "Número de cuentos")

# Son todos cuentos independientes? Hoy hay secuelas o continuaciones?

# =============================================================
# Procesamiento cuentos a tokens
# =============================================================

# Eliminamos tildes y caracteres especiales del español
cuentos <- stri_trans_general(str = db$cuento, id = "Latin-ASCII")

# TODO 
cuentos <- Corpus(VectorSource(cuentos))

# Puntuacion y todo minuscula
cuentos <- tm_map(cuentos,content_transformer(removePunctuation))
cuentos <- tm_map(cuentos, content_transformer(tolower)) 

# Quiitar números y espacios dobles
cuentos <- tm_map(cuentos,content_transformer(removeNumbers))
cuentos <- tm_map(cuentos,content_transformer(stripWhitespace))

# Lista stopwords español de dos fuentes diferentes y combinamos
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)

# TODO: podría quitarse nombres o cosas recurrentes
# (Nombres buscar palabras empiezan mayúsculas y revisar) (revisar todos terminan con Hérnan Casciari)
# También lenguaje argento
# Agregamos palabras específica
lista_palabras <- c(lista_palabras,"mientras")
cuentos<-tm_map(cuentos, removeWords, lista_palabras)

# proximo miércoles hay actividad

# TODO cambiar esta parte
# Expresiones regulares para cambiar "miniseriedeTV" por "miniserie tv"
toTV <- content_transformer(function(x, pattern) {return (gsub(pattern, 'tv', x))})

sinopsis<-tm_map(sinopsis,toTV,"miniseriedetv")
sinopsis<-tm_map(sinopsis,toTV,"miniserie tv")
sinopsis[[101]]$content

# Lematizar


# Tokenizar (crear matriz documentos)
dtm_cuentos<-DocumentTermMatrix(cuentos)

# Quitar sparsity; quitar palabras que no están en 95% docs
dtm_cuentos <- removeSparseTerms(dtm_cuentos, sparse = 0.95)







