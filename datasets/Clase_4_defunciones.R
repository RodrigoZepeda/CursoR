library(tidyverse)
name <- "~/CursoR/datasets/conjunto_de_datos_defunciones_generales_2017.CSV"
def2017 <- read_csv(name)

name <- "~/CursoR/datasets/conjunto_de_datos_defunciones_generales_2016.CSV"
def2016 <- read_csv(name)

deedadagrup <- read_csv("CursoR/datasets/Catalogo_datos_inegi/deedadagrup.csv", 
                        locale = locale(encoding = "WINDOWS-1252"))

#La de grupo de edad en def2017 y def2016 està en caracter
#pero en dedadgrup es numero voy a convertirla
def2016 <- def2016 %>%
  mutate(CVE_EDAD = as.numeric(edad_agru)) #Nueva columna

def2017 <- def2017 %>%
  mutate(edad_agru = as.numeric(edad_agru)) #Sobreescribe columna

def2016 <- def2016 %>% left_join(deedadagrup, 
                                 by = c("CVE_EDAD" = "CVE"))
def2017 <- deedadagrup %>% right_join(def2017,
                                      by = c("CVE" = "edad_agru"))
#ESTO DEBERIA DAR ERROR:
defunciones <- def2016 %>% bind_rows(def2017)
#Cambiar a caracter
def2017    <- def2017 %>% mutate(ent_ocules = as.character(ent_ocules))
defunciones <- def2016 %>% bind_rows(def2017)

#Veamos los grupos de edad
grupos_edad <- defunciones %>% 
  distinct(DESCRIP)

defunciones_24_34 <- defunciones %>%
  filter(DESCRIP == "De 25 a 29 años" | 
           DESCRIP == "De 30 a 34 años")

defunciones_sexo <- defunciones_24_34 %>%
  group_by(sexo) %>%
  tally()

#Vamos a juntar la fecha en fecha
library(lubridate)
defunciones_24_34 <- defunciones_24_34 %>%
  mutate(fecha = paste0(dia_ocurr,"/",mes_ocurr,"/",anio_ocur))

defunciones_24_34 <- defunciones_24_34 %>%
  mutate(fecha = dmy(fecha)) #dmy = day-month-year

defunciones_24_34 <- defunciones_24_34 %>%
  mutate(sem_epi = epiweek(fecha)) %>% #sem epi 
  mutate(año = year(fecha)) %>% #año
  mutate(dia_sem = wday(fecha)) %>% #domin, lun, mart
  mutate(mes = month(fecha)) #mes del año

defunciones_24_34 %>%
  group_by(sem_epi, sexo, anio_ocur) %>%
  tally() %>%
  write_excel_csv("Defunciones_25_34_por_sexo_semana.csv")
#ile-completo
#diccionario-datos-ile
#ejemploanalisis.pdf
