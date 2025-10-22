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
# 4) 
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
p_load(tidyverse, stringr, dplyr, tm, proxy) #TODO para procesar data

# Datos y primera visualización
db <- readRDS("stores/dtm_cuentos.rds")


# =============================================================
# 2) Recomendador TF-IDF y similitud del coseno
# =============================================================

# Crear matriz TF-IDF a partir del corpus de cuentos
tfidf <- weightTfIdf(db)

# Matriz TF-IDF
tfidf_matrix <- as.matrix(tfidf)

# TODO Se podría normalizar los vectores antes de similitud para controlar efecto por longitud
tfidf_matrix <- tfidf_matrix / sqrt(rowSums(tfidf_matrix^2))

# Calcular similitud de coseno entre documentos
cosine_sim <- 1 - proxy::dist(tfidf_matrix, tfidf_matrix, method = "cosine")

# TODO Revisar un ejemplo rápido (opcional)
cosine_sim[1, 1:10]

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
    dist = distancias[idx, ],
    titulo = df$titulo
  )
  
  # Ordenar de mayor a menor similitud
  recomendaciones <- recomendaciones %>% arrange(-dist)
  
  # Retornar los 10 cuentos más similares (excluyendo el mismo)
  return(recomendaciones$titulo[2:11])
}

# Ejemplo de uso
recomendador_tfidf("Messi es un perro")


