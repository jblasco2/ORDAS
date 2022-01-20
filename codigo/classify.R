
## Creamos el corpus de la siguiente manera: dos carpetas ('primary_set' y 'secondary_set')
## En la primera (primary_set), de entrenamiento, colocamos todos los textos de los candidatos.
## En la segunda (secondary_set), de testado, colocamos los textos objeto de nuestro análisis

# Establecemos directorio de trabajo
setwd("~/Desktop/ORDAS/")

#Cargamos 'stylo'
library(stylo)

# Ejecutamos la función
classify(gui = TRUE, training.frequencies = NULL, 
         test.frequencies = NULL,
         training.corpus.dir = "primary_set", 
         test.corpus.dir = "secondary_set", 
         features = NULL, 
         path = NULL)
