# =============================================================
# Recomendadores: este archivo es la segunda parte de proyecto.
#            Ahora se desarrollan 2 metodologías de recomendación
#            de cuentos del escritor Hernán Casciari. Primero se 
#            implementa uno basado en TF-IDF y similitud del coseno, 
#            luego uno basado en LDA.
#
# 1) Buenas prácticas, librerías y descarga de datos 
# 2) Recomendador TF-IDF y similitud del coseno
# 3) Recomendador LDA
#   3.1) Matrices de probabilidades
#   3.2) Probabilidades documento-tópico
#   3.3) Distribución de probabilidades γ por tópico
#   3.4) Matriz documento-tópico y similitud
#   3.5) Función recomendadora LDA
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
p_load(tidyverse, stringr, dplyr, tm, proxy, maptpx, tidytext) #TODO para procesar data

# Datos y primera visualización
cuentos <- readRDS("stores/dtm_cuentos.rds") # cuentos procesados
db <- read.csv("stores/blog_casciari.csv") # db original para cuentos 

# A los cuentos origionales quitar salto página; no aporta nada
# Quitar "/n" y reemplazar por espacio
db$cuento <- str_replace_all(db$cuento, "\n"," ")


# =============================================================
# 2) Recomendador TF-IDF y similitud del coseno
# =============================================================

# Crear matriz TF-IDF a partir del corpus de cuentos
tfidf <- weightTfIdf(cuentos)

# Matriz TF-IDF
tfidf_matrix <- as.matrix(tfidf)

# TODO Se podría normalizar los vectores antes de similitud para controlar efecto por longitud
tfidf_matrix <- tfidf_matrix / sqrt(rowSums(tfidf_matrix^2))

# Calcular similitud de coseno entre documentos
cosine_sim <- 1 - proxy::dist(tfidf_matrix, tfidf_matrix, method = "cosine")

# TODO Revisar un ejemplo rápido (opcional)
cosine_sim[1, 1:10]

# Asegurar que 'db' tenga una columna de índice (para vincular títulos y filas del DTM)
db$index <- 1:nrow(db)

# Crear función recomendadora
# TODO revisar que orden en dtm sea el mismo que en db
recomendador_tfidf <- function(title, distancias = cosine_sim, df = db) {
  
  # Obtener el índice del cuento
  idx <- df$index[df$titulo == title]
  
  if (length(idx) == 0) {
    stop("El título ingresado no existe en la base de datos.")
  }
  
  # Crear un data.frame con las distancias y los títulos
  recomendaciones <- data.frame(
    dist = as.numeric(distancias[idx, ]),
    titulo = df$titulo
  )
  
  # Ordenar de mayor a menor similitud
  recomendaciones <- recomendaciones %>% arrange(desc(-dist))
  
  # Retornar los 10 cuentos más similares (excluyendo el mismo)
  return(recomendaciones$titulo[2:11])
}

# Ejemplo de uso
recomendador_tfidf("Messi es un perro")

# Revisar recomendación (para "Messi es un perro" habla de infancia y periodismo)
c1 <- db %>% filter(titulo == "Messi es un perro") %>% pull(cuento)
c2 <- db %>% filter(titulo == "Matar la crisis a volantazos") %>% pull(cuento)
c1
c2


# =============================================================
# 3) Recomendador LDA
# =============================================================

# Definir valores de K a probar
grid <- c(3, 5, 7, 10)

# Crear lista vacía para guardar los modelos
modelos_lda <- list() # Se guardarán estilo "K3, K5, K7, K10"

# Fijar semilla
set.seed(123)

# Entrenar modelos y guardarlos en la lista
for (k in grid) {
  cat("Entrenando modelo con K =", k, "...\n")
  modelo <- topics(cuentos, K = k)
  modelos_lda[[paste0("K", k)]] <- modelo
  cat("Modelo con", k, "tópicos completado.\n\n")
}

# TODO: cambiar el # de la "K" para cambiar el modelo
cuentos_lda <- modelos_lda$K3


# =============================================================
# 3.1) Matrices de probabilidades
# =============================================================

# En maptpx:
#  - theta: probabilidad palabra-tópico
#  - omega: probabilidad documento-tópico

# Extraer theta (palabra-tópico)
theta_matrix <- cuentos_lda$theta

# Convertir a formato tidy
cuentos_topics <- as.data.frame(theta_matrix) %>%
  mutate(term = rownames(theta_matrix)) %>%
  pivot_longer(cols = -term,
               names_to = "topic",
               values_to = "beta") %>%
  mutate(topic = as.numeric(gsub("V", "", topic)))

# Obtener los 10 términos más representativos por tópico
cuentos_top_terms <- cuentos_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Visualización de tópicos
cuentos_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(title = "Top 10 términos por tópico",
       x = "Beta (probabilidad palabra-tópico)",
       y = "Término")


# =============================================================
# 3.2) Probabilidades documento-tópico
# =============================================================

# Extraer omega
omega_matrix <- cuentos_lda$omega

cuentos_documents <- as.data.frame(omega_matrix) %>%
  mutate(document = row_number()) %>%
  pivot_longer(cols = -document,
               names_to = "topic",
               values_to = "gamma") %>%
  mutate(topic = as.numeric(gsub("V", "", topic)))

# =============================================================
# 3.3) Distribución de probabilidades γ por tópico
# =============================================================

cuentos_documents %>%
  ggplot(aes(x = factor(topic), y = gamma, fill = factor(topic))) +
  geom_boxplot() +
  labs(title = "Distribución de probabilidades γ por tópico",
       x = "Tópico",
       y = "Probabilidad documento-tópico",
       fill = "Tópico")

# =============================================================
# 3.4) Matriz documento-tópico y similitud
# =============================================================

gamma_wide <- cuentos_documents %>%
  pivot_wider(names_from = topic,
              values_from = gamma,
              names_prefix = "topic_")

gamma_matrix <- as.matrix(gamma_wide[, -1])
rownames(gamma_matrix) <- gamma_wide$document

# Calcular similitud de coseno entre documentos
cosine_similarity <- function(matrix) {
  norm_matrix <- matrix / sqrt(rowSums(matrix^2))
  sim <- norm_matrix %*% t(norm_matrix)
  return(sim)
}

similarity_matrix <- cosine_similarity(gamma_matrix)

# =============================================================
# Función recomendadora LDA
# =============================================================

recomendador_lda <- function(title, distancias = similarity_matrix, df = db) {
  
  # Obtener el índice del cuento
  idx <- df$index[df$titulo == title]
  
  if (length(idx) == 0) {
    stop("El título ingresado no existe en la base de datos.")
  }
  
  # Crear un data.frame con las distancias y los títulos
  recomendaciones <- data.frame(
    dist = distancias[idx, ],
    titulo = df$titulo
  )
  
  # Ordenar de mayor a menor similitud
  recomendaciones <- recomendaciones %>% arrange(-dist)
  
  # Retornar los 10 cuentos más similares (excluyendo el mismo)
  return(recomendaciones$titulo[2:11])
}

# Ejemplo de uso
recomendador_lda("Messi es un perro")
