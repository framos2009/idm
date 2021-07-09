################################
## Introduccion al Data Mining
## Script de practica 1
################################


# Rutas de acceso a datos
dataPath <- "../data"

fileName <- "consumos_2018.csv" # Archivo con los datos

# Cargo los datos en mi ds
datos <- read.csv(
  file.path(dataPath, fileName),
  sep    = ";",
  dec    = ",",
  header = TRUE
)
