rm(list = ls())
library(markovchain) #install.packages(markovchain)
estados <- c("Sano","Diabetes","Renal")
transicion <- matrix(
  c(0.75, 0.25, 0, 0, 0.5, 0.5, 0, 0, 1), 
  byrow = T, ncol = 3)

modelo <- new("markovchain", states = estados, 
              transitionMatrix = transicion)
plot(modelo)
summary(modelo)
set.seed(100)
rmarkovchain(4, modelo, "Sano")

library(foreach) #para repetir y juntar
simulaciones <- foreach(i = 1:20, .combine = rbind) %do% {
  #NOTA: HAY UNA FORMA DE GARANTIZAR EMPEZAR EN SANO ROD LA CHECA
  rmarkovchain(4, modelo)
}

objeto1 <- simulaciones %>%
  as_tibble() %>%
  group_by(V4) %>%
  tally()
#Ejercicio: Cambiar que la proba de renal dado diabetes sea 0.3 y
#la de seguir xen diabetes sea 0.7 y ver como cambia
