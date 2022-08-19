library(tidyverse)

defunciones <- read_csv("CursoR/datasets/conjunto_de_datos_defunciones_generales_2017.CSV")

defunciones %>%
  group_by(sexo) %>%
  tally()

defunciones %>%
  filter(ent_regis == "09") %>%
  group_by(mun_regis) %>%
  tally()
#Equivalente a primero contar y luego filtrar
defunciones %>%
  group_by(ent_regis, mun_regis) %>%
  tally() %>%
  filter(ent_regis == "09") 

defunciones %>%
  filter(ent_regis == ent_resid) %>%
  tally()

# Calcular la media con summarise (para agregar cosas)
defunciones %>%
  summarise(Promedio = mean(dia_ocurr))

defunciones %>%
  summarise(Promedio = mean(dia_ocurr),
            Dia_nac  = mean(dia_nacim))

defunciones %>%
  group_by(ent_regis) %>%
  summarise(Median = median(dia_nacim))

install.packages("rmarkdown")
install.packages("kableExtra")
install.packages("prettydoc")
install.packages("knitr")
#DESCARGAR DE DROPBOX IME_2020.xls Y CATALOGO DATOS INEGI (CARPETA)
