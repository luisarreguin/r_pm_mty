# Prueba de lectura de datos

library(tidyverse)
library(janitor)

prueba <- 
  read_rds("./datos/base.rds") %>% 
  clean_names() %>% 
  print()

prueba <- 
  prueba %>% 
  filter(fecha == 2005) %>% 
  print()

plot(prueba$longitud, prueba$latitud)

