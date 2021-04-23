library(openair)
library(tidyverse)
library(haven)
library(janitor)
library(sf)
library(tmap)
library(RColorBrewer)
library(viridis)
library(viridisLite)
library(jcolors)
library(lubridate)
library(gganimate)
library(ggTimeSeries)
library(ggthemes)
library(scales)
library(gridExtra)
library(grid)

pm25_meananual_ageb <-
  read_rds("./Datos/idw_anual.rds")

cont_ageb <- 
  pm25_meananual_ageb%>%
  group_by(cvegeo) %>% 
  summarise(pm25 = mean(pm25)) %>% 
  print()

#### Mapas


mun <- c("19019", "19021", "19039", "19018", "19048", "19025", "19041", "19009", 
         "19012", "19046", "19010", "19001", "19047", "19031", "19049", "19006", 
         "19026", "19045")

ageb <- 
  st_read("./Capas/19a.shp") %>% 
  clean_names() %>% 
  filter(str_c(cve_ent, cve_mun) %in% mun) %>% 
  print()

zmm_mun <- 
  st_read("./Capas/19mun.shp") %>%  
  clean_names() %>% 
  filter(str_c(cve_ent, cve_mun) %in% mun) %>% 
  print()

cont_ageb <- 
  inner_join(ageb, cont_ageb, by = "cvegeo") 


library(viridis)

ggplot() +
  geom_sf(data = cont_ageb, aes(fill = pm25), colour = "transparent", size = .02) +
  scale_fill_gradient2(low = "#FFF7BC", mid = "#FEC44F", midpoint = 25, high = "#D95F0E",
                       breaks = c(20,22,24,26,28, 30)) +
  labs(title = (expression(paste("Distribución de ", PM[2.5], " en la ZMM"))),
       fill = expression(paste( mu, g/m^3))) +
  coord_sf(datum = NA) + 
  theme_gray() +
  theme(legend.position = c(0.1, 0.15),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20),
        legend.key.width = unit(1.3,"cm"), 
        legend.margin=margin(t = -0.2, unit='cm'),
        legend.background = element_rect(fill = "gray90", linetype="solid", 
                                         colour ="darkgray"))


