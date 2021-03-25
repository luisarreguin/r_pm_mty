# ============================================================================ #
# Point Patter Analysis
# Fecha: 
# Autor: 
# ============================================================================ #

# Librerias

library(tidyverse)
library(janitor)
library(sf)


# Base de datos de los casos

base <- read_rds("./Datos/base.rds") %>% 
  select(fecha, nombre, registro, longitud, latitud, x, y, diagnostico) %>% 
  print()

# Capa de la Zona Metropolitana de Monterrey

zmm <-
  st_read("./Capas/19mun.shp") %>% 
  clean_names() %>% 
  filter(cvegeo == "19039" | cvegeo == "19006" | cvegeo == "19018" | cvegeo == "19019"
         | cvegeo == "19021" | cvegeo == "19026" | cvegeo == "19031" 
         | cvegeo == "19041" | cvegeo == "19046" | cvegeo == "19048"
         | cvegeo == "19049" | cvegeo == "19009" | cvegeo == "19025" | cvegeo == "19001"
         | cvegeo == "19010" | cvegeo == "19012" | cvegeo == "19047" | cvegeo == "19045") %>%
  print()


