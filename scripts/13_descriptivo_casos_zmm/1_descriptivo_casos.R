# ============================================================================ #
# Descriptivo de los casos por medio de mapas
# Fecha: 
# Autor: Luis Enrique Reyes Arreguin
# ============================================================================ #


library(tidyverse)
library(janitor)
library(sf)
library(leaflet)
library(gganimate)
library(cowplot)

# Base de datos

base <- read_rds("./Datos/base.rds") %>% 
  select(fecha, nombre, x, y, diagnostico) %>% 
  print()

# Capa de la Zona Metropolitana de Monterrey

zmm <- st_read("./capas/sun_2018.shp", options = "ENCODING=ISO-8859-1") %>% 
  clean_names() %>% 
  filter(cve_sun == "M19.01") %>% 
  select(!c(cve_loc:nom_loc, pob_2018)) %>% 
  print()

# Transformo la proyeccion a crs = 4326

zmm <- 
  st_transform(zmm, crs = 4326)

# Convierto la malla de datos a una capa con proyeccion crs = 4326

base <- st_as_sf(base, coords = c("x", "y"), crs = 4326) %>% 
  print()

# Genero un mapa con los casos ubicados en la ZMM

# Cargo la capa del estado de Nuevo leon

nl_mun <-
  st_read("Capas/19_nuevoleon/conjunto_de_datos/19mun.shp") %>% 
  clean_names() %>% 
  print()

plot(st_geometry(nl_mun))

# genero un cuadro con la informacion de la ZMM

zmm_box <- st_as_sfc(st_bbox(zmm))
plot(zmm_box)

# genero un mapa que señale dentro del estado la ubicacion de la ZMM

g1 <-
  ggplot() +
  geom_sf(data = nl_mun, fill = NA) +
  geom_sf(data = zmm_box, fill = NA, color = "red", size = 1) +
  theme_bw()
g1

# genero un mapa con los puntos de los casos de cancer en la ZMM

g2 <-
  ggplot() +
  geom_sf(data = base, colour = "dodgerblue3", size = 0.5) +
  geom_sf(data = zmm, fill = NA, colour = "grey40", size = .4) +
  theme_bw()
g2  

# uno los mapas 

g3 <-
  ggdraw() +
  draw_plot(g2) +
  draw_plot(g1, x = 0.2, y = 0.05, width = 0.20, height = 0.20) +
  labs(title = "Casos de cáncer de vías respiratorias",
       subtitle = "Zona Metropolitana de Monterrey (2005 - 2019)") +
  theme(plot.title = element_text(hjust = 1, size = 12)) +
  theme_set(theme_cowplot()) 

g3  

# Genero un mapa con leaflet 

leaflet_zmm <-
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>% 
  addPolygons(data = zmm,
              fillOpacity = 0.05,
              weight = 1) %>% 
  addCircles(data = base,
             color = "#008B8B",
             opacity = 0.8,
             weight = 0.5)

leaflet_zmm

# genero un grafico animado con los casos de cancer por año (2005-2019)

anim_zmm <-
  ggplot() +
  geom_sf(data = base, colour = "red", size = 0.5) +
  geom_sf(data = zmm, fill = "transparent", colour = "grey30", size = 0.4) +
  transition_manual(as.factor(fecha)) + 
  labs(title = "Casos de cáncer de vías respiratorias",
       subtitle = 'ZMM Año: {current_frame}') +
  theme_bw()

anim_zmm

anim_save("./Salidas/gif_casos.gif", anim_zmm)

