# =============================================================
# Pipeline: este archivo es la primera parte de proyecto de 
#            desarrollo para un recomendador de cuentos del
#            escritor Hernán Casciari. Aquí desarrollamos el
#            pipeline de los datos, obteniéndolos de la DB
#            para su procesamiento correcto en pos de emplearlos
#            como insumo para el recomendador.
#
# 1) Buenas prácticas, librerías y descarga de datos 
# 2) Análisis cuentos pre-procesamiento
# 3) Web scrapping stopwords argentinas
# 4) Procesamiento cuentos a tokens
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
p_load(tidyverse, stringr, dplyr, stringi, tm, stopwords, tokenizers) #TODO para procesar data
p_load(rvest, udpipe) # para web scrapping

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
# Web scrapping stopwords argentinas
# =============================================================

# Leer la página web
url <- "https://www.lifeder.com/frases-palabras-argentinas/"

# Si esta URL falla, podrías usar otra fuente de expresiones argentinas:
# url <- "https://languagetool.org/insights/es/publicacion/palabras-argentinas/"

page <- read_html(url)

# Extraer los nodos que contienen las palabras/expresiones
palabras_raw <- page %>%
  html_nodes("strong, b") %>%      # selecciona etiquetas strong o b
  html_text() %>%
  str_trim()

# Filtrar y procesar; minuscula, duplicacdos, puntuación, 
palabras_filt <- palabras_raw %>%
  tolower() %>%
  unique() %>%
  # quitar puntuación
  str_replace_all("[[:punct:]]", "")
  

# Lematización usando udpipe
# Descargar y cargar modelo español
ud_model_file <- udpipe_download_model(language = "spanish")
ud_model <- udpipe_load_model(ud_model_file$file_model)

# Crear data frame para lematizar
df_arg <- data.frame(token = palabras_filt, stringsAsFactors = FALSE)

anot_arg <- udpipe_annotate(ud_model, x = df_arg$token)
anot_arg <- as.data.frame(anot_arg)

# Tomar el lemma de cada token
palabras_lemma <- anot_arg %>%
  select(token, lemma) %>%
  distinct() %>%
  mutate(lemma = ifelse(is.na(lemma) | lemma == "", token, lemma)) %>%
  pull(lemma) %>%
  unique()

# Resultado final en una lista
stopwords_arg <- palabras_lemma

# Ver primeras 20
head(stopwords_argentinismos, 20)


# =============================================================
# Procesamiento cuentos a tokens
# =============================================================

# Falta:
# - Lematización (revisar, no cambia mucho)
# - Quitar nombres (revisar importantes, como Chirri->amigo, Nina->hija, 
#     Mercedes->hogar/casa, Totin->perro, Cristina->esposa)
# - Quitar palabras argentinas (puede o no, depende si al final hay suficientes palabras)
#   - Web scapping de página (procesar minusculas, acentos y eso)
# - Preguntar Ignacio si cuentos muy largos afectan
# - Web scrapping para etiquetas y así sacar temas
#   - Esto podría llevar a enfoque k-means

# Eliminamos tildes y caracteres especiales del español
cuentos <- stri_trans_general(str = db$cuento, id = "Latin-ASCII")

# Quitar "/n" y reemplazar por espacio
cuentos <- str_replace_all(cuentos, "\n"," ")

# Quitar últimas 5 palabras, pues son nombre autor y fecha
cuentos <- sapply(str_split(cuentos, "\\s+"), function(x) paste(head(x, -5), collapse = " "))

# Volver strings en objetos tipo Corpus para funciones con la librería
cuentos <- Corpus(VectorSource(cuentos))

# Quitar puntuacion, números, espacios dobles, y pasar todo a minuscula
cuentos <- tm_map(cuentos,content_transformer(removePunctuation))
cuentos <- tm_map(cuentos,content_transformer(removeNumbers))
cuentos <- tm_map(cuentos,content_transformer(stripWhitespace))
cuentos <- tm_map(cuentos, content_transformer(tolower)) 

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

# TODO cambiar esta parte
# Expresiones regulares para cambiar "miniseriedeTV" por "miniserie tv"
toTV <- content_transformer(function(x, pattern) {return (gsub(pattern, 'tv', x))})

sinopsis<-tm_map(sinopsis,toTV,"miniseriedetv")
sinopsis<-tm_map(sinopsis,toTV,"miniserie tv")
sinopsis[[101]]$content

# Lematizar (TODO: modelo de udp ya descargado sección previa)
# Convertir corpus a data.frame temporal
texto_df <- data.frame(texto = sapply(cuentos, as.character), stringsAsFactors = FALSE)

# Procesar con udpipe
anotaciones <- udpipe_annotate(ud_model, x = texto_df$texto)
anotaciones <- as.data.frame(anotaciones)

# Usar solo las lemas (en minúsculas)
cuentos_lematizados <- anotaciones %>%
  group_by(doc_id) %>%
  summarise(texto = paste(lemma, collapse = " ")) %>%
  pull(texto)

# Volver a objeto Corpus
cuentos <- Corpus(VectorSource(cuentos_lematizados))

# Convertimos el corpus lematizado a vector de texto
textos_lem <- sapply(cuentos, as.character)

# Generamos n-gramas de 1, 2 y 3 palabras
tokens_unigramas <- tokenize_words(textos_lem)
tokens_bigramas  <- tokenize_ngrams(textos_lem, n = 2)
tokens_trigramas <- tokenize_ngrams(textos_lem, n = 3)

# Combinamos todos los tokens en un solo texto por documento
tokens_combinados <- mapply(function(u, b, t) {
  paste(c(u, b, t), collapse = " ")
}, tokens_unigramas, tokens_bigramas, tokens_trigramas)

# Volvemos al formato Corpus para continuar el pipeline
cuentos <- Corpus(VectorSource(tokens_combinados))

# Tokenizar (crear matriz documentos)
dtm_cuentos<-DocumentTermMatrix(cuentos)

# Quitar sparsity; quitar palabras que no están en 90% docs
dtm_cuentos <- removeSparseTerms(dtm_cuentos, sparse = 0.90)

# Exportar a stores
saveRDS(dtm_cuentos, file = "stores/dtm_cuentos.rds")





