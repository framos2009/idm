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

head(datos)  # Chequeo de las primeras observaciones del conjunto de datos

knitr::kable(head(datos))  # 

str(datos)  # Estructura de los datos leidos

# Veo la dimensión de los datos cargados
dim(datos)

# Descripción de datos
summary(datos)

# Asigno el tipo de datos factor a las variables binarias
datos$zona <- as.factor(datos$zona)  

datos[12 : 23] <- data.frame(
  apply(datos[12 : 23],
        2,
        function(x) as.factor(x))
  )

# for (x in 12:23) {
#   datos[x] <- as.factor(datos[x])
# }
# 

str(datos)  # Estructura de los datos leidos

### Repito el análisis solo para zona 5

datos.z5 <- datos[ datos$zona == 5, ]
head(datos.z5)

dim(datos.z5)
nrow(datos.z5)
summary(datos.z5)


### A partir de la variable continua Porcentaje de propietarios en el bloque 
# (prop_b), construya una variable dicotomica que tome el valor 1 para las 
# observaciones con valores mayor a la mediana y el valor 0 en cualquier otro caso.

datos$mayor.prop <- as.factor(
  ifelse(datos$prop_b > median(datos$prop_b), 1, 0)
)

summary(datos$mayor.prop)


# Descripcion del procentaje de propietarios segun la variable creada

summary(datos$prop_b[datos$mayor.prop == 1])
summary(datos$prop_b[datos$mayor.prop == 0])


# Utilizanco la funcion “summaryBy()” del paquete “psych”.

library(doBy)
summaryBy(prop_b ~ mayor.prop,
          data = datos)

summaryBy(prop_b ~ mayor.prop,
          data = datos, 
          FUN = c(length, 
                  min, 
                  max, 
                  mean, 
                  sd, 
                  median))

# d. Categorizar la misma variable (prop_b) en los intervalos definidos por los 
# cuartiles. [Ayuda: la funci?n “quantile()” permite obtener los percentiles].
# Alternativa 1 : utilizando la funcion “case_when” del paquete dplyr.

library(dplyr)

datos <- mutate(
  datos,
  categ1 = case_when(
    prop_b <= quantile(prop_b, 0.25) ~ "<= Q1",
    prop_b <= quantile(prop_b, 0.5)  ~ "Q1 - Q2",
    prop_b <= quantile(prop_b, 0.75) ~ "Q2 - Q3",
    TRUE ~ "> Q3"
  )
)

summary(datos$categ1)
table(datos$categ1)

datos$categ1 <- factor(
  datos$categ1, 
  levels  = c("<= Q1", "Q1 - Q2", "Q2 - Q3", "> Q3"),
  ordered = TRUE
)

summary(datos$categ1)


# e. Seleccione una muestra aleatoria del 10% de las observaciones totales y 
# calcule el promedio del Porcentaje de hogares con ninios en el radio censal (kids_r)

n.tot     <- nrow(datos)
n.muestra <- round(n.tot * 0.1) 
n.tot
n.muestra

# Genero los id de la muestra en forma aleatoria, sin reemplazo
id.muestra <- sample(
  1 : n.tot, 
  size = n.muestra,
  replace = FALSE
)

# Extraigo el sub set de datos de la muestra
train  <- datos[id.muestra, ]

mean(train$kids_r)

# f. Seleccione 1000 muestras aleatorias del 10% de las observaciones totales 
# guardando el promedio del Porcentaje de hogares con ninios en el radio censal 
# (kids_r) en un vector
# Seteamos una semilla para poder reproducir los mismos resultados

set.seed(1974)

# Creamos un vector para ir guardando los resultados de cada iteracion
promedios  <- c()

for (i in 1 : 50) {
  id.muestra <- sample(
    1 : n.tot, 
    size = n.muestra,
    replace = FALSE
  )
  
  train  <- datos[id.muestra, ]
  
  result.it <- mean(train$kids_r)
  
  promedios <- c(promedios, result.it)
  
}

dim(promedios)
length(promedios)

# g. Realice un histograma con los 1000 promedios calculados en el punto anterior
hist(promedios)

library(ggplot2)

data.plot <- data.frame(promedios)
head(data.plot)

ggplot(data = data.plot, aes(x = promedios)) + # Se define el conjunto de datos y la/s variables a utilizar.
  geom_histogram() # Se define el tipo de dato a utilizar.

# Los graficos con el paquete ggplot2 permiten trabajar por capaz

g0 <- ggplot(data = data.plot, aes(x = promedios)) + # Se define el conjunto de datos y la/s variables a utilizar.
  geom_histogram() # Se define el tipo de dato a utilizar.
g0

nint <- 6 # se define el numero de intervalos
amplitud  <- (max(data.plot$promedios) - min(data.plot$promedios)) / nint

g1 <- g0 +  
  geom_histogram(binwidth = amplitud) # Se define la amplitud de los intervalos.
g1  

g2 <- g1 +
  geom_line(stat = "bin", color = "red", binwidth = amplitud) + # se agrega el poligono de frecuencia.
  xlab("Promedio del porcentaje de propietarios") # Se agrega la etiqueta al eje x.
g2

