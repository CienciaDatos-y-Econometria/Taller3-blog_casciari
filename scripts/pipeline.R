# =============================================================
# Pipeline: Recomendador Casciari (Parte 1 - Pipeline de datos)
# =============================================================

# -------------------------------------------------------------
# 1) Buenas prácticas, librerías y helpers
# -------------------------------------------------------------
rm(list = ls())
# setwd("~/Desktop/Taller 1 - BigData")  # <- ajusta si lo necesitas

# Librerías
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse, stringr, dplyr, stringi, tm, stopwords, tokenizers,
  rvest, udpipe
)

# ---- Helper de normalización: minúsculas + sin tildes + limpio ----
normalize_plain <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("[^a-z0-9\\s]", " ", x, perl = TRUE)  # quita signos no alfanuméricos
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# -------------------------------------------------------------
# 2) Carga de datos y primeras miradas
# -------------------------------------------------------------
db <- read.csv("blog_casciari.csv", stringsAsFactors = FALSE)
head(db)

# Ver últimas 5 palabras de cada cuento (pueden ser firma/fecha)
sapply(str_split(db$cuento, "\\s+"), function(x) paste(tail(x, 5), collapse = " "))

# Histograma de largo (caracteres)
db %>%
  mutate(largo = nchar(cuento)) %>%
  ggplot(aes(x = largo)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribución del tamaño de los cuentos (caracteres)",
    x = "Número de caracteres", y = "Frecuencia"
  )

# Fechas a Date
db <- db %>%
  mutate(fecha = as.Date(fecha, format = "%m/%d/%y"))

db %>%
  ggplot(aes(x = fecha)) +
  geom_histogram(binwidth = 30, fill = "orange", color = "black") +
  labs(
    title = "Distribución de cuentos por fecha",
    x = "Fecha", y = "Número de cuentos"
  )

# -------------------------------------------------------------
# 3) Web scraping de argentinismos -> lista normalizada
# -------------------------------------------------------------
tryCatch({
  url <- "https://www.lifeder.com/frases-palabras-argentinas/"
  page <- read_html(url)
  palabras_raw <- page %>%
    html_nodes("strong, b") %>%
    html_text() %>%
    str_trim()

  palabras_filt <- palabras_raw %>%
    tolower() %>%
    unique() %>%
    str_replace_all("[[:punct:]]", "") %>%
    .[. != ""]
  # Normaliza (sin tildes)
  palabras_filt <- normalize_plain(palabras_filt)

  cat("Palabras argentinas extraídas:", length(palabras_filt), "\n")
}, error = function(e) {
  cat("Error en web scraping, usando lista predefinida\n")
  palabras_filt <- c(
    "che","boludo","pibe","mina","laburo","guita","quilombo",
    "bondi","fiaca","chabon","mango","faso","morfi","birra"
  )
  palabras_filt <- normalize_plain(palabras_filt)
})

# -------------------------------------------------------------
# 4) Modelo de udpipe (español)
# -------------------------------------------------------------
if(!file.exists("spanish-gsd-ud-2.5-191206.udpipe")) {
  cat("Descargando modelo udpipe español...\n")
  ud_model_file <- udpipe_download_model(language = "spanish")
  ud_model <- udpipe_load_model(ud_model_file$file_model)
} else {
  cat("Cargando modelo udpipe existente...\n")
  ud_model <- udpipe_load_model("spanish-gsd-ud-2.5-191206.udpipe")
}

# -------------------------------------------------------------
# 5) Preprocesamiento de texto previo a udpipe
#     - quitar firma/fecha del final (últimas 5 palabras)
#     - limpiar saltos de línea
#     - (NO quitamos stopwords aquí; lo haremos después de lematizar)
# -------------------------------------------------------------
# Quitar saltos de línea
cuentos_raw <- str_replace_all(db$cuento, "\n", " ")

# Quitar últimas 5 palabras (firma/fecha)
cuentos_raw <- sapply(str_split(cuentos_raw, "\\s+"), function(x) {
  paste(head(x, max(0, length(x) - 5)), collapse = " ")
})

# Corpus básico (aún sin remover stopwords)
corpus_inicial <- Corpus(VectorSource(cuentos_raw))
corpus_inicial <- tm_map(corpus_inicial, content_transformer(removePunctuation))
corpus_inicial <- tm_map(corpus_inicial, content_transformer(removeNumbers))
corpus_inicial <- tm_map(corpus_inicial, content_transformer(stripWhitespace))
corpus_inicial <- tm_map(corpus_inicial, content_transformer(tolower))

# Convierte a data.frame para anotar
texto_df <- data.frame(
  texto = sapply(corpus_inicial, as.character),
  stringsAsFactors = FALSE
)

# -------------------------------------------------------------
# 6) Lematización con udpipe + filtrado POS + normalización
#     + stopwords normalizadas + argentinismos
# -------------------------------------------------------------

# Lista stopwords español de dos fuentes diferentes y combinamos
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)

cat("Procesando", nrow(texto_df), "documentos con udpipe...\n")
anotaciones <- udpipe_annotate(ud_model, x = texto_df$texto)
anotaciones <- as.data.frame(anotaciones)

# Filtra categorías poco informativas
filtro_upos <- c("PRON","DET","AUX","PART","ADP","CCONJ","SCONJ","PUNCT","SYM","NUM","X")
anotaciones <- anotaciones %>%
  filter(!(upos %in% filtro_upos))

# Usa lemma si existe; si no, token
anotaciones <- anotaciones %>%
  mutate(lemma = ifelse(is.na(lemma) | lemma == "", token, lemma))

# Normaliza lemmas (sin tildes, minúsculas, limpio)
anotaciones$lemma <- normalize_plain(anotaciones$lemma)

# Stopwords (unión de dos fuentes) normalizadas
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)
lista_palabras <- normalize_plain(lista_palabras)

# Añade propias/funcionales que puedan colarse
lista_palabras <- unique(c(
  lista_palabras, "el", "la", "los", "las", "decir", "hacer", "tener"
  , "vez", "ver"
))

# También quita argentinismos si quieres neutralizarlos
lista_stop_extra <- unique(c(lista_palabras))

# Aplica stopwords y borra vacíos
anotaciones <- anotaciones %>%
  filter(!(lemma %in% lista_stop_extra),
         lemma != "")


# Reconstruye textos lematizados y normalizados por doc
cuentos_lematizados <- anotaciones %>%
  group_by(doc_id) %>%
  summarise(texto = paste(lemma, collapse = " "), .groups = "drop") %>%
  pull(texto)

# -------------------------------------------------------------
# 7) N-gramas (1-3) a partir de textos ya normalizados
# -------------------------------------------------------------
cat("Generando n-gramas...\n")
tokens_unigramas <- tokenize_words(cuentos_lematizados)
tokens_bigramas  <- tokenize_ngrams(cuentos_lematizados, n = 2)
tokens_trigramas <- tokenize_ngrams(cuentos_lematizados, n = 3)

tokens_combinados <- mapply(function(u, b, t) {
  paste(c(u, b, t), collapse = " ")
}, tokens_unigramas, tokens_bigramas, tokens_trigramas)

# Corpus final (normalizado, sin tildes, con n-gramas)
cuentos <- Corpus(VectorSource(tokens_combinados))


cuentos<-tm_map(cuentos, removeWords, lista_palabras)


# Quitar puntuacion, números, espacios dobles, y pasar todo a minuscula
cuentos <- tm_map(cuentos,content_transformer(removePunctuation))
cuentos <- tm_map(cuentos,content_transformer(removeNumbers))
cuentos <- tm_map(cuentos,content_transformer(stripWhitespace))
cuentos <- tm_map(cuentos, content_transformer(tolower)) 


# Definir transformador general para reemplazar texto
toWord <- content_transformer(function(x, pattern, replacement) {
  gsub(pattern, replacement, x, ignore.case = TRUE)
})

# Aplicar transformaciones con expresiones regulares
cuentos <- tm_map(cuentos, toWord, "\\bChirri\\b", "amigo")
cuentos <- tm_map(cuentos, toWord, "\\bNina\\b", "hija")
cuentos <- tm_map(cuentos, toWord, "\\bMercedes\\b", "hogar")
cuentos <- tm_map(cuentos, toWord, "\\bTotin\\b", "perro")
cuentos <- tm_map(cuentos, toWord, "\\bCristina\\b", "esposa")


# Limpieza adicional de casos específicos (ejemplo miniserie de TV)
toTV <- content_transformer(function(x, pattern) gsub(pattern, "tv", x))
cuentos <- tm_map(cuentos, toTV, "miniseriedetv")
cuentos <- tm_map(cuentos, toTV, "miniserie tv")

# (Opcional) inspección
if(length(cuentos) >= 1) {
  cat("Ejemplo documento 1 (1000 chars):\n",
      substr(cuentos[[1]]$content, 1, 1000), "\n\n")
}

# -------------------------------------------------------------
# 8) Matriz Documento-Término (DTM) y sparsity
# -------------------------------------------------------------
cat("Creando matriz documento-término...\n")
dtm_cuentos <- DocumentTermMatrix(cuentos)
cat("Dimensiones DTM original:", paste(dim(dtm_cuentos), collapse = " x "), "\n")

# Quitar términos muy escasos (sparsity <= 0.90 -> presentes ≥10% docs)
dtm_cuentos <- removeSparseTerms(dtm_cuentos, sparse = 0.90)

# Releer original para metadata de títulos
db_raw <- read.csv("blog_casciari.csv", stringsAsFactors = FALSE)

# Asegura que #docs coincida
stopifnot(nrow(dtm_cuentos) == nrow(db_raw))

# Títulos únicos para docnames
titles <- make.unique(as.character(db_raw$titulo))
dtm_cuentos$dimnames$Docs <- titles

# Guarda metadatos
dir.create("stores", showWarnings = FALSE)
saveRDS(tibble(titulo = titles), "stores/meta_cuentos.rds")

cat("Dimensiones DTM después de sparse removal:",
    paste(dim(dtm_cuentos), collapse = " x "), "\n")

# Exporta DTM
saveRDS(dtm_cuentos, file = "stores/dtm_cuentos.rds")
cat("Proceso completado. DTM guardada en stores/dtm_cuentos.rds\n")

# -------------------------------------------------------------
# 9) Métricas y resumen para reporte
# -------------------------------------------------------------
cat("\n===============================================\n")
cat("           MÉTRICAS DEL PIPELINE\n")
cat("===============================================\n")

# DTM original (sobre el mismo 'cuentos' final sin sparsity)
dtm_original <- DocumentTermMatrix(cuentos)

docs_finales        <- nrow(dtm_cuentos)
terminos_finales     <- ncol(dtm_cuentos)
terminos_originales  <- ncol(dtm_original)
reduccion_absoluta   <- terminos_originales - terminos_finales
reduccion_pct        <- round((reduccion_absoluta / terminos_originales) * 100, 1)

# Frecuencias de términos en la DTM final
frecuencias <- colSums(as.matrix(dtm_cuentos))
top_terminos <- sort(frecuencias, decreasing = TRUE)

# Sparsity real (porcentaje de ceros)
mat_bin <- as.matrix(dtm_cuentos) > 0
sparsity_real <- round((1 - sum(mat_bin) / (docs_finales * terminos_finales)) * 100, 1)

cat("DIMENSIONES Y REDUCCIÓN:\n")
cat("========================\n")
cat("Documentos finales:              ", docs_finales, "\n")
cat("Términos originales:             ", terminos_originales, "\n")
cat("Términos finales:                ", terminos_finales, "\n")
cat("Términos eliminados:             ", reduccion_absoluta, "\n")
cat("Reducción vocabulario:           ", reduccion_pct, "%\n")
cat("Sparsity aplicada:               90%\n")
cat("Sparsity real resultante:        ", sparsity_real, "%\n")

cat("\nTOP 10 TÉRMINOS MÁS FRECUENTES:\n")
cat("===============================\n")
n_top <- min(10, length(top_terminos))
for(i in 1:n_top) {
  cat(sprintf("%2d. %-20s %8d\n", i, names(top_terminos)[i], top_terminos[i]))
}

cat("\n===============================================\n")
cat("           TEXTO PARA TU REPORTE\n")
cat("===============================================\n")

texto_reporte <- sprintf(
  "La matriz documento-término final presenta dimensiones de %d documentos × %d términos tras aplicar un umbral de sparsity del 90%%. El vocabulario se redujo de %d a %d términos únicos (−%s%%). Los términos más frecuentes post-procesamiento son: %s.",
  docs_finales,
  terminos_finales,
  terminos_originales,
  terminos_finales,
  reduccion_pct,
  paste(names(top_terminos)[1:min(5, length(top_terminos))], collapse = ", ")
)

cat(texto_reporte, "\n")
cat("\n===============================================\n")
