
library(tidyverse)
library(janitor)
library(sf)

casos <- 
  read_rds("./Datos/base.rds") %>% 
  select(registro, fecha, diagnostico, x, y) %>% 
  print()

casos <- 
  st_as_sf(casos, coords = c("x", "y"), crs = 4326) %>% 
  print()

mun <- c("19019", "19021", "19039", "19018", "19048", "19025", "19041", "19009", 
         "19012", "19046", "19010", "19001", "19047", "19031", "19049", "19006", 
         "19026", "19045")

zmm_mun <- 
  st_read("./Capas/19mun.shp") %>% 
  clean_names() %>% 
  filter(cvegeo %in% mun) %>% 
  print()

zmm_ageb <- 
  st_read("./Capas/19a.shp") %>% 
  clean_names() %>% 
  filter(str_c(cve_ent, cve_mun) %in% mun) %>% 
  print()

zmm_area <- 
  st_union(zmm_mun) %>% 
  st_sf() %>% 
  mutate(cve_sun = "M19.01", 
         nom_sun = "Monterrey") %>% 
  relocate(geometry, .after = last_col()) %>% 
  print()

# st_write(zmm_area, "./Capas/zmm_area.shp")

rm(mun)

# http://insus.gob.mx/archivos/ProgramasSociales/PRAH/Anexos/Anexo_14_Anexo_Tecnico.pdf
casos <- st_transform(casos, crs = 32614)
zmm_area <- st_transform(zmm_area, crs = 32614)
zmm_ageb <- st_transform(zmm_ageb, crs = 32614)
zmm_mun <- st_transform(zmm_mun, crs = 32614)

identical(st_crs(casos), st_crs(zmm_area))
identical(st_crs(casos), st_crs(zmm_ageb))
identical(st_crs(casos), st_crs(zmm_mun))

# st_write(casos, "./Capas/casos_utm.shp")
# st_write(zmm_ageb, "./Capas/zmm_ageb_utm.shp")
# st_write(zmm_area, "./Capas/zmm_area_utm.shp")
# st_write(zmm_mun, "./Capas/zmm_mun_utm.shp")

