rm(list = ls()) #Clear all
library(tidymodels)
library(tidyverse)

#Esto le dice a R que por default use tidymodels en lugar
#de lo que sea que hace por default
tidymodels_prefer()

#Copiado de los apuntes
emb_pob <- read_delim("https://online.stat.psu.edu/stat462/sites/onlinecourses.science.psu.edu.stat462/files/data/poverty/index.txt")

ggplot(emb_pob) +
  geom_point(aes(x = PovPct, y = Brth15to17)) +
  geom_smooth(aes(x = PovPct, y = Brth15to17),
              method = "lm", formula = y ~ x, se = F) +
  theme_bw()
#y ~ x es notación para y = b0 + b1 * x

ggplot(emb_pob) +
  geom_point(aes(x = PovPct, y = Brth15to17)) +
  geom_smooth(aes(x = PovPct, y = Brth15to17, 
                  color = "y = b0 + b1 * x"),
              method = "lm", formula = y ~ x, se = F) +
  geom_smooth(aes(x = PovPct, y = Brth15to17,
                  color = "y = b1 * x"),
              method = "lm", formula = y ~ -1 + x, se = F) +
  theme_bw()
#y ~ -1 + x es notación para y =  b1 * x

#En tidymodels
#  modelo %>%
#     set_engine("como_ajustar") %>%
#     fit(y ~ x, data = datos) #Formula

# Regresión lineal (básico) ----
modelo_1 <- linear_reg() %>%
  set_engine("lm") %>%
  fit(Brth15to17 ~ PovPct, data = emb_pob) #Brth = b0 + b1*PovPct 

#En tidymodels
# modelo_ajustado %>%
#   extract_fit_engine() %>%
#   summary() ó tidy() ó predict() 

modelo_1 %>%
  extract_fit_engine() %>%
  summary()

modelo_1 %>%
  extract_fit_engine() %>%
  tidy(conf.int = TRUE, conf.level = 0.9) %>%
  write_excel_csv("Datos.csv")

#Obtener de los observados la predicción
NatalidadPredicha <- modelo_1 %>%
  extract_fit_engine() %>%
  predict(new_data = emb_pob)

tabla_con_predichos <- emb_pob %>% cbind(NatalidadPredicha)

#Obtener de los observados el IC
natalidad_ic <- modelo_1 %>%
  extract_fit_engine() %>%
  predict(new_data = emb_pob, interval = "confidence")

tabla_con_predichos <- emb_pob %>% cbind(natalidad_ic)

ggplot(tabla_con_predichos) +
  geom_ribbon(aes(x = PovPct, ymin = lwr, ymax = upr),
              alpha = 0.5, fill = "green") +
  geom_point(aes(x = PovPct, y = Brth15to17), color = "pink") +
  geom_line(aes(x = PovPct, y = fit), color = "blue") +
  theme_bw()

