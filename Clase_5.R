library(tidyverse)   #Librería de análisis de datos
library(wesanderson) #Colores como las películas de este señor
library(ggformula)   #Splines
library(lubridate)   #Fechas

# Lectura de la base desde Internet
direccion.web    <- "~/CursoR/ile_completo.csv"
ile.data         <- read_csv(direccion.web)

# Lectura del diccionario de datos desde su dirección web
direccion.web    <- "~/CursoR/diccionario-de-datos-ile.csv"
diccionario.data <- read_csv(direccion.web)

