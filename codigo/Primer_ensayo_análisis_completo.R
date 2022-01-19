#####################################
# PRIMER ENSAYO ANÁLISIS COMPLETO
#https://rpubs.com/Joaquin_AR/334526
#https://www.cienciadedatos.net/documentos/38_text_minig_con_r_ejemplo_practico_twitter#introducción

## CONTENIDOS
# 1. Cargar textos y convertirlos en tablas
# 2. Creación de tablas a partir del léxico y establecimiento de variables
# 3. Limpiar el texto y tokenizar
# 4. Total de palabras y palabras únicas en cada autor
# 5. Uso Stopword
# 6. Palabras más usadas por cada autor, con gráfico
# 7. Correlación entre usuarios por palabras utilizadas
# 8. Número de palabras comunes entre dos autores
# 9. Palabras que se utilizan de forma más diferenciada por cada autor
# 10. Logaritmo de odds mayor que cero marcan la probabilidad de que una palabra sea de un autor concreto
# 11. Representamos ahora las 30  voces más diferenciadas
# 12. Bigramas y trigramas
# 13. Idf y tf_idf
# 14. SVM
####################################
direccion <- ("~/Desktop/ORDAS/analisis_a_fondo/ordas/")
#Carga las librería necesarias
library(tidyverse)
library(tidytext)
library(tm)
library(topicmodels)
library(scales)
options(scipen=999)
# Carga la lista de palabras vacías
vacias <- read_csv("~/Desktop/ORDAS/vacias.txt",
                   locale = default_locale())
# Los textos los lees desde un repositorio externo
direccion <- ("~/Desktop/ORDAS/analisis_a_fondo/ordas/")
# Localiza los textos
titulos <- c("Articulos de Aparicio",
             "Articulos de Luis Mateo",
             "Articulos de Merino")
ficheros <- c("aparicio.txt",
              "mateo.txt",
              "merino.txt")
autores <- c("Aparicio",
             "Mateo",
             "Merino")

#Lo siguiente es crear la tabla ensayos en la que se guardarán los cuatro textos divididos en páginas:
ensayos <- tibble(autor = character(),
                  texto = character(),
                  titulo = character(),
                  pagina = numeric())

#Lo que sigue es el proceso de carga de los  textos y su 
# subdivisión en páginas de 375 palabras
for (j in 1:length(ficheros)){
  texto.entrada <- read_lines(paste(direccion,
                                    ficheros[j],
                                    sep = ""),
                              locale = default_locale())
  texto.todo <- paste(texto.entrada, collapse = " ")
  por.palabras <- strsplit(texto.todo, " ")
  texto.palabras <- por.palabras[[1]]
  trozos <- split(texto.palabras,
                  ceiling(seq_along(texto.palabras)/375))
  for (i in 1:length(trozos)){
    fragmento <- trozos[i]
    fragmento.unido <- tibble(texto = paste(unlist(fragmento),
                                            collapse = " "),
                              titulo = titulos[j],
                              autor = autores [j],
                              pagina = i)
    ensayos <- bind_rows(ensayos, fragmento.unido)
  }
}
texto.palabras
length(ficheros)

# borrar todos los objetos intermedios que has creado para dividir los textos en páginas.
rm(ficheros, titulos, trozos, fragmento, fragmento.unido, texto.entrada, texto.palabras, texto.todo, por.palabras, i, j)

#De entre toda la información disponible, en este análisis únicamente 
#se emplea: autor del texto, etc., y contenido
colnames(ensayos)

# Selección de variables
ensayos <- ensayos %>% select(autor, texto, titulo, pagina)

head(ensayos)

#Limpiar y tokenizar el texto
limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

#hacemos un test
test = "Esto es 1 ejemplo de l'limpieza de6 TEXTO  https://t.co/rnHPgyhx4Z @JoaquinAmatRodrigo #textmining"
limpiar_tokenizar(texto = test)

# Se aplica la función de limpieza y tokenización a cada texto
ensayos <- ensayos %>% mutate(texto_tokenizado = map(.x = texto,
                                                     .f = limpiar_tokenizar))
ensayos %>% select(texto_tokenizado) %>% head()

# almacenar el texto tokenizado
ensayos %>% slice(1) %>% select(texto_tokenizado) %>% pull()

#Proceso de expansión
ensayos_tidy <- ensayos %>% select(-texto) %>% unnest()
ensayos_tidy <- ensayos_tidy %>% rename(token = texto_tokenizado)
head(ensayos_tidy)

#Total de palabras utilizadas por cada usuario
ensayos_tidy %>% group_by(autor) %>% summarise(n = n())

#Palabras distintas utilizadas por cada usuario
ensayos_tidy %>% select(autor, token) %>% distinct() %>%  group_by(autor) %>%
  summarise(palabras_distintas = n())

ensayos_tidy %>% select(autor, token) %>% distinct() %>%
  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()

#Palabras más utilizadas por usuario
ensayos_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>% print(n=30)

#Aplicar stopword
lista_stopwords <- c('de', 'el', 'la', 'que', 'en', 'los', 'se',
                     'del','no', 'las', 'un', 'por', 'una', 'su','al',
                     'con', 'me', 'es', 'mas', 'más', 'sus', 'lo', 'para',
                     'como', 'había', 'y', 'ya', 'e', 'en', 'entre', 'esa', 
                     'esas', 'ese', 'eso','esos', 'esta', 'estas',
                     'este', 'esto', 'les', 'mi', 'mis', 'mía', 'mías', 'mío', 
                     'míos', 'ni', 'nos', 'o', 'os', 'otra', 'otras', 'otro',
                     'otros', 'pero', 'por', 'qué', 'sin', 'su', 'sus', 'suya',
                     'suyas', 'suyo', 'sí', 'te', 'ti', 'una', 'uno', 'unos',
                     'le', 'yo', 'era', 'él', 'estaba', 'cuando', 'hacia', 'hasta',
                     'después', 'sobre', 'le', 'él', 'muy', 'dos', 'usted',
                     'tan', 'ella', 'todo', 'allí', 'fue', 'entonces', 'ella', 'dónde', 
                     'donde', 'porque', 'pues', 'también', 'desde', 'ha', 'han', 'siempre', 'hay') 

# Se añade el término si al listado de stopwords
lista_stopwords <- c(lista_stopwords, "si")

# Se filtran las stopwords
ensayos_tidy <- ensayos_tidy %>% filter(!(token %in% lista_stopwords))

#Representación gráfica
ensayos_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)

#Word Clouds
library(wordcloud)
library(RColorBrewer)

wordcloud_custom <- function(grupo, df){
  print(grupo)
  wordcloud(words = df$token, freq = df$frecuencia,
            max.words = 400, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(4, "Dark2"))
}

df_grouped <- ensayos_tidy %>% group_by(autor, token) %>% count(token) %>%
  group_by(autor) %>% mutate(frecuencia = n / n()) %>%
  arrange(autor, desc(frecuencia)) %>% nest() 

walk2(.x = df_grouped$autor, .y = df_grouped$data, .f = wordcloud_custom)

#Correlación entre usuarios por palabras utilizadas: Una forma 
# de cuantificar la similitud entre los perfiles de dos usuarios 
# es calculando la correlación en el uso de palabras. 
# La idea es que, si dos usuarios escriben de forma similar, 
# tenderán a utilizar las mismas palabras y con frecuencias similares
library(gridExtra)
library(scales)

ensayos_spread <- ensayos_tidy %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = NA, drop = TRUE)

cor.test(~ Aparicio + Mateo, method = "pearson", data = ensayos_spread)
cor.test(~ Merino + Mateo, data = ensayos_spread)
cor.test(~ Merino + Aparicio, data = ensayos_spread)
# cor expresa la correlación: a mayor cifra mayor correlación entre autores

#Graficamos
p1 <- ggplot(ensayos_spread, aes(Aparicio, Mateo)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

p2 <- ggplot(ensayos_spread, aes(Mateo, Merino)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

p3 <- ggplot(ensayos_spread, aes(Aparicio, Merino)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

grid.arrange(p1, p2, p3, nrow = 1)


#número de palabras comunes entre cada par de autores.
#APARICIO / MATEO
palabras_comunes <- dplyr::intersect(ensayos_tidy %>% filter(autor=="Aparicio") %>%
                                       select(token), ensayos_tidy %>% filter(autor=="Mateo") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre Aparicio y Mateo", palabras_comunes)

#MATEO / MERINO
palabras_comunes <- dplyr::intersect(ensayos_tidy %>% filter(autor=="Mateo") %>%
                                       select(token), ensayos_tidy %>% filter(autor=="Merino") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre Mateo y Merino", palabras_comunes)



#MERINO / APARICIO
palabras_comunes <- dplyr::intersect(ensayos_tidy %>% filter(autor=="Aparicio") %>%
                                       select(token), ensayos_tidy %>% filter(autor=="Merino") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre Aparicio y Merino", palabras_comunes)


#Qué palabras se utilizan de forma más diferenciada por cada autor, 
#Es decir, palabras que utiliza mucho un autor y que no utiliza el otro. 
#Se compara por pares

# Pivotaje y despivotaje
ensayos_spread <- ensayos_tidy %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = 0, drop = TRUE)
ensayos_unpivot <- ensayos_spread %>% gather(key = "autor", value = "n", -token)

# Selección de los autores Aparicio y Mateo
ensayos_unpivot <- ensayos_unpivot %>% filter(autor %in% c("Aparicio",
                                                           "Mateo"))
# Se añade el total de palabras de cada autor
ensayos_unpivot <- ensayos_unpivot %>% left_join(ensayos_tidy %>%
                                                   group_by(autor) %>%
                                                   summarise(N = n()),
                                                 by = "autor")
# Cálculo de odds y log of odds de cada palabra en APARICIO/MATEO
ensayos_logOdds <- ensayos_unpivot %>%  mutate(odds = (n + 1) / (N + 1))
ensayos_logOdds <- ensayos_logOdds %>% select(autor, token, odds) %>% 
  spread(key = autor, value = odds)
ensayos_logOdds <- ensayos_logOdds %>%  mutate(log_odds = log(Aparicio/Mateo),
                                               abs_log_odds = abs(log_odds))
# Si el logaritmo de odds es mayor que cero, significa que es una palabra con
# mayor probabilidad de ser de Aparicio. Esto es así porque el ratio sea ha
# calculado como Aparicio/Mateo.
ensayos_logOdds <- ensayos_logOdds %>%
  mutate(autor_frecuente = if_else(log_odds > 0,
                                   "aparicio.txt",
                                   "mateo.txt"))
ensayos_logOdds %>% arrange(desc(abs_log_odds)) %>% head()

#CAMBIAMOS LOS AUTORES

# Pivotaje y despivotaje
ensayos_spread <- ensayos_tidy %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = 0, drop = TRUE)
ensayos_unpivot <- ensayos_spread %>% gather(key = "autor", value = "n", -token)

# Selección de los autores Merino y Mateo
ensayos_unpivot <- ensayos_unpivot %>% filter(autor %in% c("Merino",
                                                           "Mateo"))
# Se añade el total de palabras de cada autor
ensayos_unpivot <- ensayos_unpivot %>% left_join(ensayos_tidy %>%
                                                   group_by(autor) %>%
                                                   summarise(N = n()),
                                                 by = "autor")

# Cálculo de odds y log of odds de cada palabra en MERINO/MATEO
ensayos_logOdds <- ensayos_unpivot %>%  mutate(odds = (n + 1) / (N + 1))
ensayos_logOdds <- ensayos_logOdds %>% select(autor, token, odds) %>% 
  spread(key = autor, value = odds)
ensayos_logOdds <- ensayos_logOdds %>%  mutate(log_odds = log(Merino/Mateo),
                                               abs_log_odds = abs(log_odds))
# Si el logaritmo de odds es mayor que cero, significa que es una palabra con
# mayor probabilidad de ser de Aparicio. Esto es así porque el ratio sea ha
# calculado como Merino/Mateo.
ensayos_logOdds <- ensayos_logOdds %>%
  mutate(autor_frecuente = if_else(log_odds > 0,
                                   "merino.txt",
                                   "mateo.txt"))
ensayos_logOdds %>% arrange(desc(abs_log_odds)) %>% head()


#Representamos ahora las 30 más diferenciadas
ensayos_logOdds %>% group_by(autor_frecuente) %>% top_n(15, abs_log_odds) %>%
  ggplot(aes(x = reorder(token, log_odds), y = log_odds, fill = autor_frecuente)) +
  geom_col() +
  labs(x = "palabra", y = "log odds ratio (Aparicio / Mateo)") +
  coord_flip() + 
  theme_bw()

# Análisis de palabras en relación (bigramas o trigramas)
library(tidytext)
head(ensayos)
limpiar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  return(nuevo_texto)
}

bigramas <- ensayos %>% mutate(texto = limpiar(texto)) %>%
  select(texto) %>%
  unnest_tokens(input = texto, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE)

# Contaje de ocurrencias de cada bigrama
bigramas  %>% count(bigrama, sort = TRUE)


#Eliminamos bigramas que contienen alguna stopword
# Separación de los bigramas 
bigrams_separados <- bigramas %>% separate(bigrama, c("palabra1", "palabra2"),
                                           sep = " ")
head(bigrams_separados)

# Filtrado de los bigramas que contienen alguna stopword
bigrams_separados <- bigrams_separados  %>%
  filter(!palabra1 %in% lista_stopwords) %>%
  filter(!palabra2 %in% lista_stopwords)

# Unión de las palabras para formar de nuevo los bigramas
bigramas <- bigrams_separados %>%
  unite(bigrama, palabra1, palabra2, sep = " ")

# Nuevo contaje para identificar los bigramas más frecuentes
bigramas  %>% count(bigrama, sort = TRUE) %>% print(n = 20)

#hacer gráfico
library(igraph)
library(ggraph)
graph <- bigramas %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>% 
  count(palabra1, palabra2, sort = TRUE) %>%
  filter(n > 3) %>% graph_from_data_frame(directed = FALSE)
set.seed(123)

plot(graph, vertex.label.font = 2,
     vertex.label.color = "black",
     vertex.label.cex = 0.7, edge.color = "gray85")

# Análisis de palabras en relación (bigramas o trigramas)
library(tidytext)
head(ensayos)
limpiar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  return(nuevo_texto)
}

trigramas <- ensayos %>% mutate(texto = limpiar(texto)) %>%
  select(texto) %>%
  unnest_tokens(input = texto, output = "trigrama",
                token = "ngrams",n = 3, drop = TRUE)

# Contaje de ocurrencias de cada bigrama
trigramas  %>% count(trigrama, sort = TRUE)


#Eliminamos bigramas que contienen alguna stopword
# Separación de los bigramas 
trigrams_separados <- trigramas %>% separate(trigrama, c("palabra1", "palabra2", "palabra3"),
                                             sep = " ")
head(trigrams_separados)

# Filtrado de los bigramas que contienen alguna stopword
trigrams_separados <- trigrams_separados  %>%
  filter(!palabra1 %in% lista_stopwords) %>%
  filter(!palabra2 %in% lista_stopwords) %>%
  filter(!palabra3 %in% lista_stopwords)

# Unión de las palabras para formar de nuevo los bigramas
trigramas <- trigrams_separados %>%
  unite(trigrama, palabra1, palabra2, palabra3, sep = " ")

# Nuevo contaje para identificar los bigramas más frecuentes
trigramas  %>% count(trigrama, sort = TRUE) %>% print(n = 20)

#hacer gráfico
library(igraph)
library(ggraph)
graph <- trigramas %>%
  separate(trigrama, c("palabra1", "palabra2", "palabra3"), sep = " ") %>% 
  count(palabra1, palabra2, palabra3, sort = TRUE) %>%
  filter(n > 1) %>% graph_from_data_frame(directed = FALSE)
set.seed(123)

plot(graph, vertex.label.font = 2,
     vertex.label.color = "black",
     vertex.label.cex = 0.7, edge.color = "gray85")



# Cuantificación temática de un texto
#Frecuencia de los términos
# Número de veces que aparece cada término por texto
ensayos_tf <- ensayos_tidy %>% group_by(autor, token) %>% summarise(n = n())

# Se añade una columna con el total de términos por texto
ensayos_tf <- ensayos_tf %>% mutate(total_n = sum(n))

# Se calcula el tf. El tf de cada término se obtiene dividiendo el númeor 
# de apariciones de ese término por el número total de palabras 
# del texto
ensayos_tf <- ensayos_tf %>% mutate(tf = n / total_n )
head(ensayos_tf)

#Frecuencia inversa
total_documentos = ensayos_tidy$autor %>% unique() %>% length()
total_documentos

# Número de documentos en los que aparece cada término
ensayos_idf <- ensayos_tidy %>% distinct(token, autor) %>% group_by(token) %>%
  summarise(n_documentos = n())

# Cálculo del idf
ensayos_idf <- ensayos_idf %>% mutate(idf = n_documentos/ total_documentos) %>%
  arrange(desc(idf))
head(ensayos_idf)

#Relación de frecuencia y frecuencia inversa
ensayos_tf_idf <- left_join(x = ensayos_tf, y = ensayos_idf, by = "token") %>% ungroup()
ensayos_tf_idf <- ensayos_tf_idf %>% mutate(tf_idf = tf * idf)
ensayos_tf_idf %>% select(-autor) %>% head() 

# Algoritmos de clasificación a un texto, basado en un modelo de 
# aprendizaje estadístico basado en máquinas de vector soporte (SVM) 
# con el objetivo de predecir la autoría
# 1. repartir las observaciones en un set de entrenamiento y otro de 
#test. Esto permite evaluar la capacidad del modelo. Para este 
#ejercicio se selecciona como test un 20% aleatorio

ensayos_Aparicio_ed <- ensayos %>% filter(autor %in% c("Aparicio", "Mateo"))
set.seed(123)
train <- sample(x = 1:nrow(ensayos_Aparicio_ed), size = 0.8 * nrow(ensayos_Aparicio_ed))
ensayos_train <- ensayos_Aparicio_ed[train, ]
ensayos_test  <- ensayos_Aparicio_ed[-train, ]


#2. verificar que la proporción de cada grupo es similar en el set 
#de entrenamiento y en el de test.

table(ensayos_train$autor) / length(ensayos_train$autor)

table(ensayos_test$autor) / length(ensayos_test$autor)

#3. se crea un matriz tf-idf en la que cada columna es un término, 
# cada fila un documento y el valor de intersección el tf-idf 
#correspondiente.

library(quanteda)
texto <- paste0("Esto es 1 ejemplo de l'limpieza de6 TEXTO",
                "https://t.co/rnHPgyhx4Z @JoaquinAmatRodrigo #textmining")
matriz_tfidf <- dfm(x = texto, what = "word", remove_numbers = TRUE,
                    remove_punct = TRUE, remove_symbols = TRUE,
                    remove_separators = TRUE, remove_twitter = FALSE,
                    remove_hyphens = TRUE, remove_url = FALSE)
colnames(matriz_tfidf)
limpiar_tokenizar(texto = texto)

# Volver a fundir
paste(limpiar_tokenizar(texto = texto), collapse = " ")

# Limpieza y tokenización de los documentos de entrenamiento
ensayos_train$texto <- ensayos_train$texto %>% map(.f = limpiar_tokenizar) %>%
  map(.f = paste, collapse = " ") %>% unlist()

# Creación de la matriz documento-término
matriz_tfidf_train <- dfm(x = ensayos_train$texto, remove = lista_stopwords)

# Se reduce la dimensión de la matriz eliminando aquellos términos que 
# aparecen en menos de 2 documentos. Con esto se consigue eliminar ruido.
matriz_tfidf_train <- dfm_trim(x = matriz_tfidf_train, min_docfreq = 2)

# Conversión de los valores de la matriz a tf-idf
matriz_tfidf_train <- dfm_tfidf(matriz_tfidf_train, scheme_tf = "prop",
                            scheme_df = "inverse")

matriz_tfidf_train

#4.  A la hora de trasformar los documentos de test
# Limpieza y tokenización de los documentos de test
ensayos_test$texto <- ensayos_test$texto %>% map(.f = limpiar_tokenizar) %>%
  map(.f = paste, collapse = " ") %>% unlist()
# Identificación de las dimensiones de la matriz de entrenamiento
# Los objetos dm() son de clase S4, se accede a sus elementos mediante @
dimensiones_matriz_train <- matriz_tfidf_train@Dimnames$features
# Conversión de vector a diccionario pasando por lista
dimensiones_matriz_train <- as.list(dimensiones_matriz_train)
names(dimensiones_matriz_train) <- unlist(dimensiones_matriz_train)
dimensiones_matriz_train <- dictionary(dimensiones_matriz_train)

# Proyección de los documentos de test
matriz_tfidf_test <- dfm(x = ensayos_test$texto,
                         dfm_lookup(dimensiones_matriz_train))
matriz_tfidf_test <- dfm_tfidf(matriz_tfidf_test, scheme_tf = "prop",
                           scheme_df = "inverse")
matriz_tfidf_test

#5. Se comprueba que las dimensiones de ambas matrices son iguales.
all(colnames(matriz_tfidf_test) == colnames(matriz_tfidf_train))

#6. Clasificamos con SVM
library(e1071)
modelo_svm <- svm(x = matriz_tfidf_train, y = as.factor(ensayos_train$autor),
                  kernel = "linear", cost = 1, scale = TRUE,
                  type = "C-classification")
modelo_svm

#7. Predicciones: Empleando el modelo entrenado en el paso 
# anterior se predice la autoría de los textos de test.
predicciones <- predict(object = modelo_svm, newdata = matriz_tfidf_test)

# Matriz de confusión: Error de predicción
table(observado = ensayos_test$autor, predicho = predicciones)

#Comprobar los errores de clasificación
clasificaciones_erroneas <- sum(ensayos_test$autor != predicciones)
error <- 100 * mean(ensayos_test$autor != predicciones)
paste("Número de clasificaciones incorrectas =", clasificaciones_erroneas)

#Porcentaje de error
paste("Porcentaje de error =", round(error, 2), "%")

#Validación cruzada
set.seed(369)
svm_cv <- tune("svm", train.x =  matriz_tfidf_train,
               train.y = as.factor(ensayos_train$autor),
               kernel = "linear", 
               ranges = list(cost = c(0.1, 0.5, 1, 2.5, 5)))
summary(svm_cv)

#8 Graficación
ggplot(data = svm_cv$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = error - dispersion, ymax = error + dispersion)) +
  theme_bw()

svm_cv$best.parameters

#Se reajusta el modelo con el índice de error que nos da el anterior script: 0.1
modelo_svm <- svm(x = matriz_tfidf_train, y = as.factor(ensayos_train$autor),
                  kernel = "linear", cost = 0.1, scale = TRUE)

predicciones <- predict(object = modelo_svm, newdata = matriz_tfidf_test)
table(observado = ensayos_test$autor, predicho = predicciones)

# Nueva clasificción
clasificaciones_erroneas <- sum(ensayos_test$autor != predicciones)
error <- 100 * mean(ensayos_test$autor != predicciones)
paste("Número de clasificaciones incorrectas =", clasificaciones_erroneas)
paste("Porcentaje de error =", round(error,2), "%")
