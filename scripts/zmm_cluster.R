
# https://rpubs.com/Joaquin_AR/310338

library(tidyverse)
library(janitor)
library(sf)

casos <- 
  st_read("./Capas/casos_utm.shp") %>% 
  rename(registro = registr, diagnostico = dignstc) %>% 
  print()

zmm_mun <- 
  st_read("./Capas/zmm_mun_utm.shp") %>% 
  clean_names() %>% 
  print()

zmm_ageb <- 
  st_read("./Capas/zmm_ageb_utm.shp") %>% 
  clean_names() %>% 
  print()

zmm_area <- 
  st_read("./Capas/zmm_area_utm.shp") %>% 
  clean_names() %>% 
  print()

identical(st_crs(casos), st_crs(zmm_area))
identical(st_crs(casos), st_crs(zmm_ageb))
identical(st_crs(casos), st_crs(zmm_mun))

ggplot() + 
  geom_sf(data = zmm_mun, fill = "transparent") + 
  geom_sf(data = casos, colour = "dodgerblue4", alpha = 0.4) + 
  theme_bw()

casos %>% 
  st_join(zmm_mun, join = st_intersects) %>% 
  st_set_geometry(NULL) %>% 
  count(cvegeo, name = "tot_casos") %>% 
  arrange(desc(tot_casos)) %>% 
  print()

casos_fuera_ageb <- 
  casos %>% 
  st_join(zmm_ageb, join = st_intersects) %>% 
  filter(is.na(cvegeo)) %>% 
  pull(registro)

casos <- 
  casos %>% 
  mutate(caso_ageb = case_when(registro %in% casos_fuera_ageb ~ 0, TRUE ~ 1)) %>% 
  relocate(geometry, .after = last_col()) %>% 
  print()

rm(casos_fuera_ageb)

ggplot() + 
  geom_sf(data = zmm_ageb, fill = "white", colour = "grey70", lwd = 0.2) + 
  geom_sf(data = casos, aes(colour = factor(caso_ageb)), size = 0.7) + 
  scale_color_manual(values = c("red", "dodgerblue4")) + 
  coord_sf(datum = NA) + 
  theme_bw()

casos_por_ageb <- 
  st_contains(zmm_ageb, casos, sparse = F) %>% 
  as_tibble(.name_repair = "unique") %>% 
  mutate(tot_casos = rowSums(.)) %>% 
  select(tot_casos) %>% 
  bind_cols(st_set_geometry(zmm_ageb, NULL)) %>% 
  relocate(tot_casos, .after = last_col()) %>% 
  print()

zmm_ageb_casos <- 
  inner_join(zmm_ageb, casos_por_ageb, 
             by = c("cvegeo", "cve_ent", "cve_mun", "cve_loc", "cve_ageb")) %>% 
  print()

library(RColorBrewer)
ggplot() + 
  geom_sf(data = zmm_area, fill = 'transparent', colour = "grey80", lwd = 0.8) + 
  geom_sf(data = zmm_mun, fill = 'grey95', colour = "grey80", lwd = 0.3) + 
  geom_sf(data = zmm_ageb_casos, aes(fill = factor(tot_casos)), lwd = 0.05) +
  scale_fill_brewer(palette = "YlOrBr") + 
  scale_color_brewer(palette = "YlOrBr") + 
  coord_sf(datum = NA) + 
  theme_bw()

scale_casos_por_ageb <- 
  casos_por_ageb %>% 
  mutate(id_ageb = str_c("ageb_", str_pad(1:nrow(casos_por_ageb), width = 4, pad = "0"))) %>% 
  select(id_ageb, tot_casos) %>% 
  column_to_rownames("id_ageb") %>% 
  scale()

scale_casos_por_ageb

rm(casos_por_ageb)


library(factoextra)
fviz_nbclust(x = scale_casos_por_ageb, FUNcluster = kmeans, method = "wss", k.max = 6) + 
  labs(subtitle = "Elbow method")

fviz_nbclust(x = scale_casos_por_ageb, FUNcluster = kmeans, method = "silhouette", k.max = 6) + 
  labs(subtitle = "Silhouette method")

fviz_nbclust(scale_casos_por_ageb, kmeans, nstart = 25,  method = "gap_stat", k.max = 6, nboot = 50) + 
  labs(subtitle = "Gap statistic method")

clust_ageb <- kmeans(scale_casos_por_ageb, centers = 3, nstart = 25)
print(clust_ageb)

fviz_cluster(clust_ageb, data = scale_casos_por_ageb)

zmm_ageb <- 
  zmm_ageb %>%
  mutate(cluster = clust_ageb$cluster) %>% 
  relocate(cluster, .after = cve_ageb) %>% 
  print()

ggplot() + 
  geom_sf(data = zmm_area, fill = 'transparent', colour = "grey80", lwd = 0.8) + 
  geom_sf(data = zmm_mun, fill = 'grey95', colour = "grey80", lwd = 0.3) + 
  geom_sf(data = zmm_ageb, aes(fill = factor(cluster)), lwd = 0.05) +
  scale_fill_brewer(palette = "YlOrBr") + 
  scale_color_brewer(palette = "YlOrBr") + 
  coord_sf(datum = NA) + 
  theme_bw()



