
# Establecemos directorio de trabajo
setwd("~/Desktop/ORDAS/corpus_oppose")

#Cargamos 'stylo'
library(stylo)

# Ejecutamos la función
oppose(gui = TRUE, training.frequencies = NULL, 
       test.frequencies = NULL,
       training.corpus.dir = "primary_set", 
       test.corpus.dir = "secondary_set", 
       features = NULL, 
       path = NULL)
