###############################################
# Estudio de los turnos IMC de 2019-01 a 2021-06
# Autor: Fernando Ramos
# Fecha: 22/06/2021
###############################################
#Cargo librerias

knitr::opts_chunk$set(echo = TRUE)

listofpackages <- c( "psych",     "ggplot2",    "knitr",
                     "pastecs",   "FactoMineR", "grid",
                     "gridExtra", "ggfortify",  "factoextra",
                     "corrplot",  "dplyr",      "readr",
                     "tidyverse", "lubridate",  "cowplot")
newPackages <- listofpackages[ !(listofpackages %in%
                                   installed.packages()[, "Package"])]
if(length(newPackages)) install.packages(newPackages)
for (paquete in listofpackages) {
  suppressMessages(library(paquete, character.only = TRUE))
}


# Cargo el set de datos - Pacientes con turno ambulatorio de 01-01-2019
#a 30-06-21
# Turnos <- read_csv("Turnos_2019_2021_06.csv")
Turnos <- read_delim("Turnos_2019_2021_06.csv", 
                                  "\t",
                     escape_double = FALSE,
                     trim_ws = TRUE)

summary(Turnos)
names(Turnos)

#Elimino filas vacías & posteriores al 30/06 
Turnos <- Turnos %>% 
  filter(!is.na(PACIENTE) &
           FEC_TURNO < "2021/07/01" &
           FEC_TURNO >= "2019/01/01" &
           FEC_ASIGNA >= "2019/01/01" &
           ESTADO != "LIBRE" &
           ESTADO != "ANULADO" &
           between(DIAS_TURNO, 0, 90) &
           SER != "KI" &
           SER != "UCR")

Turnos$ESTADO <- as.factor(Turnos$ESTADO)
Turnos$CANAL <- as.factor(Turnos$CANAL)
Turnos$TIPO <- as.factor(Turnos$TIPO)

Turnos$condicion <- factor(Turnos$ESTADO,
                           levels = c("ASIGNADO", "TOMADO"),
                           labels = c("Ausente", "Presente"))

#Separo el horario del turno, en horas y minutos
Turnos <- separate(Turnos, HORA, into = c("hora_turno", "minuto_turno"), 
                   convert = TRUE)
dim(Turnos)
summary(Turnos)
options(scipen = 999)

####
total_ausentes <- sum(Turnos$ESTADO == "ASIGNADO")
total_presentes <- sum(Turnos$ESTADO == "TOMADO")
total_turnos <- total_ausentes + total_presentes

#Evolución de asignación de turnos 1°semestre años 2019 a 2021
Turnos %>% 
  filter(DIAS_TURNO < 90,
         MES < 7) %>% 
  ggplot() +
  aes(x = AÑO,
      fill = condicion,
      label = ..count..) +
  geom_bar(position = "dodge", width = 0.75) +
  labs(title = "Evolución de turnos 1° semestre de 2019 a 2021",
       y = "Turnos",
       x = "Año")
# Evolución turnos por profesional
profe <- c("CIANCA",
           "CROCEJU",
           "CRUZ",
           "ZANAT")
turnos_profe <- filter(Turnos, DIAS_TURNO < 90,
                 MES < 7)

paso <- NULL
g <- NULL

for (paso in profe) {
  subtitulo <- paste0("Profesional: ", paso)
  g <- ggplot( data = filter(turnos_profe, profesional == paso)) +
    aes(x = AÑO,
        fill = condicion,
        label = ..count..) +
    geom_bar(position = "dodge", width = 0.75) +
    scale_x_continuous(limits = c(2018.5, 2021.5),
                       breaks = seq(2019, 2022, 1)) +
    labs(title = "Evolución de turnos 1° semestre de 2019 a 2021",
         subtitle = subtitulo,
         y = "Turnos",
         x = "Año")
  print(g)
}

#Evolución de turnos 1° sem 2021 / profesionales piloto
for (paso in profe) {
  subtitulo <- paste0("Profesional: ", paso)
  g <- ggplot( data = filter(turnos_profe,
                             profesional == paso &
                               AÑO == 2021)) +
    aes(x = MES,
        fill = condicion,
        label = ..count..) +
    geom_bar(position = "dodge", width = 0.75) +
    # scale_x_continuous(limits = c(2018.5, 2021.5),
    #                    breaks = seq(2019, 2022, 1)) +
    labs(title = "Evolución de turnos 1° semestre 2021",
         subtitle = subtitulo,
         y = "Turnos",
         x = "Mes",
         xlim(1, 6))
  print(g)
}

#Evolución turnos para los prf Cianca, Crocce S, Cruy y Zanatta en 2021
Turnos %>% 
  filter(DIAS_TURNO < 90,
         AÑO == 2021,
         profesional %in% c(
           "CIANCA",
           "CROCEJU",
           "CRUZ",
           "ZANAT"
         )) %>% 
  ggplot() +
  aes(x = reorder(profesional, profesional,
              function(x) - length(x)),
      fill = condicion,
      label = ..count..) +
  geom_bar(position = "dodge") +
  labs(title = "Evolución de turnos 1° semestre 2021",
       y = "Turnos",
       x = "Profesional")

# Cantidad de turnos ausentes y presentes por día y horario
Turnos %>% 
  filter(DIAS_TURNO < 90,
         AÑO == 2021,
         TIPO != "9",
         profesional == "ZANAT") %>% 
  ggplot() +
  aes(x = hora_turno,
      fill = condicion,
      label = ..count..) +
  geom_bar(position = "dodge") +
  facet_wrap(~dia) +
  labs(title = "Cantidad de turnos 1° semestre 2021",
       subtitle = "Profesional: ZANATTA",
       y = "Turnos",
       x = "Hora Turno")


Turnos %>% 
  filter(DIAS_TURNO < 90,
         MES < 7) %>% 
ggplot() +
  aes(x = DIAS_TURNO,
      y = ..count.. / sum(..count..) * 100,
      fill = condicion) +
  geom_bar() +
  labs(title = "Días para asignación de turnos s/estado ",
       subtitle = "1° semestre - Años: 2019 a 2021",
       y = "% Turnos asignados",
       x = "Días asignación turnos")


t19 <- Turnos %>% 
  filter(DIAS_TURNO < 90,
         AÑO == 2019,
         MES < 7) %>% 
  ggplot() +
  aes(x = MES,
      y = ..count..,
      fill = condicion) +
  geom_bar() +
  labs(title = "Evolución de turnos 1° semestre del año 2019",
       y = "Turnos",
       x = "Mes")

t20 <- Turnos %>% 
  filter(DIAS_TURNO < 90,
         AÑO == 2020,
         MES < 7) %>% 
  ggplot() +
  aes(x = MES,
      y = ..count..,
      fill = condicion) +
  geom_bar() +
  labs(title = "Evolución de turnos 1° semestre del año 2020",
       y = "Turnos",
       x = "Mes")

t21 <- Turnos %>% 
  filter(DIAS_TURNO < 90,
         AÑO == 2021) %>% 
  ggplot() +
  aes(x = MES,
      y = ..count..,
      fill = ESTADO) +
  geom_bar() +
  labs(title = "Evolución de turnos 1° semestre del año 2021",
       y = "Turnos",
       x = "Mes")

plot_grid(t19, t20, t21, ncol = 2)

Turnos %>% 
  filter(DIAS_TURNO < 90) %>% 
  ggplot() +
  aes(sample = DIAS_TURNO) +
  stat_qq(distribution = qunif, geom = "line") 


Turnos %>% 
  filter(DIAS_TURNO < 90,
         MES < 7) %>% 
  ggplot() +
  aes(x = DIAS_TURNO, fill = condicion) +
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent)+
  labs(title = "% ausentismo vs presentismo de pacientes",
       subtitle = "1° semestre de los años 2019 a 2021",
       x = "Días asignación turno",
       y = "% Turnos")


#Turnos por horario
turnos_horario <- Turnos %>% 
  group_by(DIAS_TURNO) %>% 
  summarise(
    presentes = sum(ESTADO == "TOMADO"),
    ausentes = sum(ESTADO == "ASIGNADO"),
    total = n(),
    ausentismo = ausentes / total,
    densidad = total / (total_presentes + total_ausentes))

summary(turnos_horario)



ggplot(data = filter(turnos_horario,
                     between(ausentismo, 0.0, 0.9),
                     DIAS_TURNO <90)) +
  aes(x = DIAS_TURNO, y = ausentismo * 100,
      color = densidad) +
  geom_point(size = 2) +
  ggtitle("Ausentismo de pacientes / días asignación turnos
          Período: 01/01/2019 al 30/06/2021") +
  scale_x_continuous("Días Asignación Turnos") +
  scale_y_continuous("% Ausentismo",
                     limits = c(0,60),
                     breaks = seq(0, 60, 10)) +
  scale_color_continuous(low = "salmon", high = "darkred") 

#Turnos por servicio
turnos_servicio <- Turnos %>% 
  group_by(SER) %>% 
  summarise(
    presentes = sum(ESTADO == "TOMADO"),
    ausentes = sum(ESTADO == "ASIGNADO"),
    total = n(),
    ausentismo = ausentes / total,
    densidad = total / (total_presentes + total_ausentes)) %>% 
  arrange(desc(ausentismo))

ggplot(data = filter(turnos_servicio,
                     between(ausentismo,
                             0.0, 0.8))) +
  aes(x = SER, weight = ausentismo,
      color = densidad) +
  geom_bar() +
#  geom_point(size = 2) +
  ggtitle("Ausentismo por servicio
          Período: 01/01/2019 al 30/06/2021") +
  scale_color_continuous(low = "pink", high = "darkred") 

# reducimos los datos para los prf Cianca, Crocce, Cruz y Zanatta
Turnos <- Turnos %>% 
  filter(!is.na(PACIENTE) &
           ESTADO != "LIBRE" &
           ESTADO != "ANULADO" &
           DIAS_TURNO >= 0 &
           profesional %in%(c(
             "CROCEJU",
             "CRUZ",
             "CIANCA",
             "ZANAT")) &
           SER != "KI" &
           SER != "UCR")

total_ausentes <- sum(Turnos$ESTADO == "ASIGNADO")
total_presentes <- sum(Turnos$ESTADO == "TOMADO")

turnos_horario <- Turnos %>% 
  group_by(DIAS_TURNO) %>% 
  summarise(
    presentes = sum(ESTADO == "TOMADO"),
    ausentes = sum(ESTADO == "ASIGNADO"),
    total = n(),
    ausentismo = ausentes / total,
    densidad = total / (total_presentes + total_ausentes))

summary(turnos_horario)

ggplot(data = filter(turnos_horario,
                     between(ausentismo, 0.0, 0.9),
                     DIAS_TURNO <90)) +
  aes(x = DIAS_TURNO, y = ausentismo * 100,
      color = densidad) +
  geom_point(size = 2) +
  ggtitle("Ausentismo de pacientes / días asignación turnos
          Período: 01/01/2019 al 30/06/2021
          Profesionales: Ciancaglini, Crocce S, Cruz y Zanatta") +
  scale_x_continuous("Días Asignación Turnos") +
  scale_y_continuous("% Ausentismo",
                     limits = c(0,60),
                     breaks = seq(0, 60, 10)) +
  scale_color_continuous(low = "salmon", high = "darkred") 


## A fin de desestacionalizar los datos se toma de enero a junio de 2019
# a 2021 y repetimos análisis
total_ausentes <- sum(Turnos$ESTADO == "ASIGNADO" &
                        between(Turnos$MES, 1, 6))
total_presentes <- sum(Turnos$ESTADO == "TOMADO" &
                         between(Turnos$MES, 1, 6))

turnos_horario <- Turnos %>% 
  group_by(DIAS_TURNO) %>% 
  filter(between(MES, 1, 6)) %>% 
  summarise(
    presentes = sum(ESTADO == "TOMADO"),
    ausentes = sum(ESTADO == "ASIGNADO"),
    total = n(),
    ausentismo = ausentes / total,
    densidad = total / (total_presentes + total_ausentes))

summary(turnos_horario)

ggplot(data = filter(turnos_horario,
                     between(ausentismo, 0.0, 0.9),
                     DIAS_TURNO <90)) +
  aes(x = DIAS_TURNO, y = ausentismo * 100,
      color = densidad) +
  geom_point(size = 2) +
  ggtitle("Ausentismo de pacientes / días asignación turnos
          Período: Enero a Junio de 2019 a 2021
          Profesionales: Ciancaglini, Crocce S, Cruz y Zanatta") +
  scale_x_continuous("Días Asignación Turnos") +
  scale_y_continuous("% Ausentismo",
                     limits = c(0,60),
                     breaks = seq(0, 60, 10)) +
  scale_color_continuous(low = "salmon", high = "darkred") 



# ordeno los datos por ausentismo de mayor a menor
turnos_servicio <-  arrange(turnos_servicio, desc(ausentismo))
summary(turnos_servicio)



write_delim(turnos_servicio, 
            "datos/turnos_servicio.txt",
            delim = "\t")


### Analizo ausentismo y presentismo por profesional
#Obtengo cantidad de turnos por profesional / estado, excluyendo
#los turnos libres
turnos_prf <- Turnos %>% 
  filter(ESTADO != "LIBRE" & ESTADO != "ANULADO" &
           SER != "KI" &
           between(MES, 1, 6)) %>% 
  group_by(profesional, ESTADO) %>% 
  summarise(total = n())

turnos_prf <- filter(turnos_prf,
                     profesional %in% c(
                       "CIANCA",
                       "CROCEJU",
                       "CRUZ",
                       "ZANAT"
                     ))

#Ordeno los datos - Usando pivot a lo ancho
turnos_prf <- turnos_prf %>% 
  pivot_wider(names_from = ESTADO, values_from = total)

# si un registro tiene na en asignado o tamado; indica que no tuvo
#ningún turno --> = 0
turnos_prf <-  replace_na(turnos_prf, list(ASIGNADO = 0, TOMADO = 0))


#agrego columna de total de turnos, ausentismo y presentismo
turnos_prf <-  turnos_prf %>% 
  mutate(total = ASIGNADO + TOMADO) %>% 
  mutate(presentismo = TOMADO / total) %>% 
  mutate(ausentismo = ASIGNADO / total)

# ordeno los datos por ausentismo de mayor a menor
arrange(turnos_prf, desc(ausentismo))

ggplot(data = turnos_prf) +
  geom_point(mapping = aes(x = profesional, y = ausentismo))

hist(turnos_prf$ausentismo, 
     main = "Ausentismo de pacientes por profesional",
     xlab = "Indice ausentismo",
     ylab = "Frecuencia")

summary(turnos_prf$ausentismo)

#Grabo ausentismo por profesional
write_delim(turnos_prf, 
            "datos/ausente_profesional.txt",
            delim = "\t")


###

#calculo la contidad de turnos datos por operador / mes
turnos_operador <- Turnos %>%
  filter(ESTADO != "LIBRE") %>% 
  group_by(AÑO, MES, DIA, OPERADOR) %>% 
  summarise(asignados = n()) %>% 
  arrange(AÑO, MES, DIA,desc(asignados))

#Agrupo por pacientes, la cantidad de turnos asignados y tomados. 
#La suma de ambos es la cantidad de turnos que saco el paciente
rnk_presentismo <- Turnos %>% 
  filter(ESTADO %in% c("ASIGNADO", "TOMADO") & SER !="KI") %>% 
  group_by(HC) %>% 
  summarise(ausentes = sum(ESTADO == "ASIGNADO", na.rm = TRUE),
            total = n(),
            ausentismo = ausentes / total)


ggplot(data = filter(rnk_presentismo, ausentismo > 0.0), 
       mapping = aes(x = ausentismo * 100)) +
  geom_bar() +
  scale_x_continuous(name="% ausentismo")


hist(rnk_presentismo$ausentismo * 100,
     main = "Ausentismo",
     xlab = "% Ausentismo",
     ylab = "Turnos",
     xlim = c(1, 100))

# Ausentismo por día de la semana
Turnos <- Turnos %>%
  mutate(dia = paste0(
    wday(FEC_TURNO, week_start = 1),
    "-",
    weekdays(FEC_TURNO)))

turnos_dia <- Turnos %>% 
  group_by(dia,
           hora_turno) %>% 
  summarise(
    presentes = sum(ESTADO == "TOMADO"),
    ausentes = sum(ESTADO == "ASIGNADO"),
    total = n(),
    ausentismo = ausentes / total,
    densidad = total / (total_presentes + total_ausentes))

ggplot(data = filter(turnos_dia,
                     dia != "7-domingo")) +
  aes(x = hora_turno, y = ausentismo,
      color = densidad,
      fill = densidad) +
  geom_bar(stat = "identity") +
  facet_wrap(~dia) +
  scale_x_continuous(name = "Hora del turno",
                     limits = c(7,21)) +
  scale_y_continuous(name = "Turnos ausente") +
  labs(title = "Ausentismo por día de la semana",
       subtitle = "Período: 01/01/2019 al 30/06/2021") 


turnos_dia <- Turnos %>% 
  filter(profesional %in% c(
    "CIANCA",
    "CROCEJU",
    "CRUZ",
    "ZANAT"),
    AÑO == 2021) %>% 
  group_by(dia,
           hora_turno,
           profesional) %>% 
  summarise(
    presentes = sum(ESTADO == "TOMADO"),
    ausentes = sum(ESTADO == "ASIGNADO"),
    total = n(),
    ausentismo = ausentes / total,
    densidad = total / (total_presentes + total_ausentes))

ggplot(data = filter(turnos_dia,
                     dia != "7-domingo",
                     total>2)) +
  aes(x = dia, y = ausentismo,
      color = profesional,
      fill = profesional) +
  geom_bar(stat = "identity") 


Turnos %>% 
  filter(profesional %in% c(
    "CIANCA",
    "CROCEJU",
    "CRUZ",
    "ZANAT"),
    AÑO == 2021,
    ESTADO == "ASIGNADO") %>% 
  ggplot() +
  aes(x = dia, y = ..count.. / sum(..count..) * 100,
      fill = profesional) +
  geom_bar(width = 0.75) +
  labs(title = "Ausentismo por profesional por día",
       subtitle = "1° semestre de 2021",
       x = "Día",
       y = "% Ausentismo")


ggplot(data = filter(turnos_dia,
                     dia != "7-domingo",
                     total>2)) +
  aes(x = dia, y = sum())

  # scale_x_continuous(name = "Hora del turno",
  #                    limits = c(7,21)) +
  # scale_y_continuous(name = "Turnos ausente") +
  # ggtitle("Ausentismo por día de la semana por profesional
  #                   Período: 01/01/2021 al 30/06/2021") 


#análisis de ausentismo por día/horario y profesional

# Para la prueba piloto trabajo con el sub set de turnos de los siguientes
# médicos: Ciancaglini, Crocce S, Cruz M, Zanatta
Turnos <- filter(Turnos,
                 profesional %in% 
                   c("CIANCA",
                     "CROCEJU",
                     "CRUZ",
                     "ZANAT"))

#busco correlación entre tiempo de espera del turno y el ausentismo
ggplot(data = filter(Turnos, ESTADO == "ASIGNADO" &
                       DIAS_TURNO <= 60 &
                       DIAS_TURNO >= 0),
       mapping = aes(x = DIAS_TURNO, color = profesional,
                     fill = profesional)) +
  geom_bar() +
  facet_wrap(~weekdays(FEC_TURNO)) +
  scale_x_continuous(name = "Días espera turno", limits = c(-1,25)) +
  scale_y_continuous(name = "Turnos ausente") +
  ggtitle("Turnos no asistidos vs días de espera por profesional")

# Ahora busco si hay estacionalidad, analizando de enero a junio p/c/año
Turnos <- filter(Turnos, between(MES, 1, 6))

#busco correlación entre tiempo de espera del turno y el ausentismo
ggplot(data = filter(Turnos, ESTADO == "ASIGNADO" & 
                       DIAS_TURNO <= 60 & DIAS_TURNO >= 0), 
       mapping = aes(x = DIAS_TURNO)) +
  geom_bar() +
  scale_x_continuous(name = "Días espera turno", limits = c(-1,62)) +
  scale_y_continuous(name = "Turnos ausente") +
  ggtitle("Turnos no asistidos vs días de espera para el turno")

#busco correlación entre tiempo de espera del turno y el ausentismo
ggplot(data = filter(Turnos, ESTADO == "ASIGNADO" &
                       DIAS_TURNO <= 60 &
                       DIAS_TURNO >= 0),
       mapping = aes(x = wday(FEC_TURNO), color = dia,
                     fill = dia)) +
  geom_bar() +
  facet_wrap(~profesional) +
  scale_x_continuous(name = "Días de la semana", limits = c(1,7)) +
  scale_y_continuous(name = "Turnos ausente") +
  ggtitle("Turnos no asistidos vs días de espera por profesional")

ggplot(data = filter(Turnos, ESTADO == "ASIGNADO" &
                       DIAS_TURNO <= 60 &
                       DIAS_TURNO >= 0 &
                       dia == 'lunes'),
       mapping = aes(x = DIAS_TURNO, color = profesional,
                     fill = profesional)) +
  geom_bar() +
  facet_wrap(~weekdays(FEC_TURNO)) +
  scale_x_continuous(name = "Días espera turno", limits = c(-1,25)) +
  scale_y_continuous(name = "Turnos ausente") +
  ggtitle("Turnos no asistidos vs días de espera por profesional")

Turnos <- Turnos %>% 
  mutate(profesional = as.factor(profesional),
         ESTADO = as.factor(ESTADO),
         CANAL = as.factor(CANAL),
         SER = as.factor(SER),
         TIPO = as.factor(TIPO))

summary(Turnos)
# Ahora busco si hay estacionalidad, analizando de enero a junio p/c/año
Turnos <- filter(Turnos, AÑO == 2021)
###################################333
#Reglas de asociación
library(tidyr)
library(arules)
library(knitr)
library(arulesViz)

reglas_turnos <- select(Turnos,
                        DIAS_TURNO,
                        profesional,
                        CANAL,
                        SER,
                        EDAD,
                        ESTADO)

reglas <- apriori(reglas_turnos)

inspect(reglas)

reglas2 <- as.(reglas, "data.frame") ## no funciona!!

reglas <- as.data.frame(reglas) #tampoco me funciona...

#knitr::kable( head(reglas), digits = 3 )

## Análisis PCA
Turnos_pca <- Turnos %>%
  filter(AÑO == 2021) %>% 
  group_by(MES,
           DIAS_TURNO) %>% 
  summarise(ausentismo = sum(condicion == "Ausente") /
              sum(n()))
  
summary(Turnos_pca)

res<-pastecs::stat.desc(Turnos_pca) %>%
  as.matrix %>%
  as.data.frame %>%
  round(2)

pairs.panels(Turnos_pca, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = F # show correlation ellipses
            )

dfInd <- stack(Turnos_pca)
names(dfInd) <- c("valor","variable")
ggplot(dfInd, aes(x = variable, y = valor)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1))

#remover la última columna de los nombres de provincias
X <- as.matrix(Turnos_pca[,]) 

#trabajamos con logaritomo dada la asimetría de algunas variables
X <- log(X) 

S <- cov(X) #matriz de varianzas y covarianzas
R <- cor(X) # matriz de correlaciones

eig <- eigen(S)
eig$values

