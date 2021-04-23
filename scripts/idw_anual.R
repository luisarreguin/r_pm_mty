library(tidyverse)
library(haven)
library(janitor)
library(sf)

pm25 <- 
  read_rds("./Datos/tab_2000_2019.rds") %>% 
  select(year, site, pm25) %>% 
  print()


mun <- c("19019", "19021", "19039", "19018", "19048", "19025", "19041", "19009", 
         "19012", "19046", "19010", "19001", "19047", "19031", "19049", "19006", 
         "19026", "19045")

ageb <- 
  st_read("./Capas/19a.shp") %>% 
  clean_names() %>% 
  filter(str_c(cve_ent, cve_mun) %in% mun) %>% 
  print()

# calcular centroides AGEB

c_ageb <- 
  ageb %>% 
  st_centroid() %>% 
  print()

plot(c_ageb)


# estaciones

sites <-
  st_read("./Capas/zmm_sites_pm.shp") %>% 
  select(site, geometry) %>% 
  print()

identical(st_crs(c_ageb), st_crs(sites))

prj <- st_crs(c_ageb)

sites <- st_transform(sites, prj)

identical(st_crs(c_ageb), st_crs(sites))

library(tmap)
tm_shape(ageb) +
  tm_polygons() +
  tm_shape(c_ageb) +
  tm_dots() +
  tm_shape(sites) +
  tm_dots(col = "red", size = 1)

#write_sf(sites, "./Capas/sites_prj.shp")
#write_sf(c_ageb, "./Capas/c_ageb.shp")


library(gstat)
library(sp)
library(rgdal)

c_ageb <-
  st_read("./Capas/c_ageb.shp") %>% 
  print()

sites <-
  st_read("./Capas/zmm_sites_pm.shp") %>% 
  select(site, geometry) %>% 
  print()

### 2000 ===========================
pm25_2000 <- 
  pm25 %>%
  filter(year == "2000" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2000$year)

est_ano_1 <-
  inner_join(sites, pm25_2000, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2000 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

### 2001 ===========================
pm25_2001 <- 
  pm25 %>%
  filter(year == "2001" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2001$year)

est_ano_1 <-
  inner_join(sites, pm25_2001, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2001 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(ano_2000, ano_2001) %>% 
  print()


### 2002 ===========================
pm25_2002 <- 
  pm25 %>%
  filter(year == "2002" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2002$year)

est_ano_1 <-
  inner_join(sites, pm25_2002, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2002 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2002) %>% 
  print()


### 2003 ===========================
pm25_2003 <- 
  pm25 %>%
  filter(year == "2003" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2003$year)

est_ano_1 <-
  inner_join(sites, pm25_2003, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2003 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2003) %>% 
  print()

tail(pm25_meananual_ageb)

### 2004 ===========================
pm25_2004 <- 
  pm25 %>%
  filter(year == "2004" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2004$year)

est_ano_1 <-
  inner_join(sites, pm25_2004, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2004 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2004) %>% 
  print()

tail(pm25_meananual_ageb)

### 2005 ===========================
pm25_2005 <- 
  pm25 %>%
  filter(year == "2005" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2005$year)

est_ano_1 <-
  inner_join(sites, pm25_2005, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2005 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2005) %>% 
  print()

tail(pm25_meananual_ageb)

### 2006 ===========================
pm25_2006 <- 
  pm25 %>%
  filter(year == "2006" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2006$year)

est_ano_1 <-
  inner_join(sites, pm25_2006, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2006 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2006) %>% 
  print()

tail(pm25_meananual_ageb)

### 2007 ===========================
pm25_2007 <- 
  pm25 %>%
  filter(year == "2007" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2007$year)

est_ano_1 <-
  inner_join(sites, pm25_2007, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2007 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2007) %>% 
  print()

tail(pm25_meananual_ageb)

### 2008 ===========================
pm25_2008 <- 
  pm25 %>%
  filter(year == "2008" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2008$year)

est_ano_1 <-
  inner_join(sites, pm25_2008, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2008 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2008) %>% 
  print()

tail(pm25_meananual_ageb)

### 2009 ===========================
pm25_2009 <- 
  pm25 %>%
  filter(year == "2009" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2009$year)

est_ano_1 <-
  inner_join(sites, pm25_2009, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2009 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2009) %>% 
  print()

tail(pm25_meananual_ageb)

### 2010 ===========================
pm25_2010 <- 
  pm25 %>%
  filter(year == "2010" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2010$year)

est_ano_1 <-
  inner_join(sites, pm25_2010, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2010 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2010) %>% 
  print()

tail(pm25_meananual_ageb)

### 2011 ===========================
pm25_2011 <- 
  pm25 %>%
  filter(year == "2011" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2011$year)

est_ano_1 <-
  inner_join(sites, pm25_2011, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2011 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2011) %>% 
  print()

tail(pm25_meananual_ageb)

### 2012 ===========================
pm25_2012 <- 
  pm25 %>%
  filter(year == "2012" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2012$year)

est_ano_1 <-
  inner_join(sites, pm25_2012, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2012 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2012) %>% 
  print()

tail(pm25_meananual_ageb)

### 2013 ===========================
pm25_2013 <- 
  pm25 %>%
  filter(year == "2013" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2013$year)

est_ano_1 <-
  inner_join(sites, pm25_2013, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2013 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2013) %>% 
  print()

tail(pm25_meananual_ageb)

### 2014 ===========================
pm25_2014 <- 
  pm25 %>%
  filter(year == "2014" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2014$year)

est_ano_1 <-
  inner_join(sites, pm25_2014, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2014 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2014) %>% 
  print()

tail(pm25_meananual_ageb)

### 2015 ===========================
pm25_2015 <- 
  pm25 %>%
  filter(year == "2015" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2015$year)

est_ano_1 <-
  inner_join(sites, pm25_2015, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2015 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2015) %>% 
  print()

tail(pm25_meananual_ageb)

### 2016 ===========================
pm25_2016 <- 
  pm25 %>%
  filter(year == "2016" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2016$year)

est_ano_1 <-
  inner_join(sites, pm25_2016, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2016 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2016) %>% 
  print()

tail(pm25_meananual_ageb)

### 2017 ===========================
pm25_2017 <- 
  pm25 %>%
  filter(year == "2017" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2017$year)

est_ano_1 <-
  inner_join(sites, pm25_2017, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2017 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2017) %>% 
  print()

tail(pm25_meananual_ageb)

### 2018 ===========================
pm25_2018 <- 
  pm25 %>%
  filter(year == "2018" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2018$year)

est_ano_1 <-
  inner_join(sites, pm25_2018, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2018 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2018) %>% 
  print()

tail(pm25_meananual_ageb)

### 2019 ===========================
pm25_2019 <- 
  pm25 %>%
  filter(year == "2019" & !is.na(pm25)) %>% 
  print()

fecha_etiq_1 <- unique(pm25_2019$year)

est_ano_1 <-
  inner_join(sites, pm25_2019, by = "site") %>% 
  select(year, site, pm25) %>% 
  print()

est_ano_1 <- st_transform(est_ano_1, crs = 4326)
c_ageb <- st_transform(c_ageb, crs = 4326)

sp_est_ano_1 <- as_Spatial(est_ano_1)
class(sp_est_ano_1)

sp_ageb <- as_Spatial(c_ageb)
class(sp_ageb)

idw_estim5k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 5,
      nmin = 2)

idw_estim5k_1@data$var1.pred

idw_estim10k_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10,
      nmin = 2)

idw_estim10k_1@data$var1.pred

idw_estim10k_b_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 2.0,
      maxdist = 10)

idw_estim10k_b_1@data$var1.pred

idw_estimall_1 <- 
  idw(formula = pm25 ~ 1,
      locations = sp_est_ano_1,
      newdata = sp_ageb,
      idp = 1)

idw_estimall_1@data$var1.pred

idw5k_1 <- 
  bind_cols(c_ageb, idw5 = idw_estim5k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw5) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10k_1 <- 
  bind_cols(c_ageb, idw10 = idw_estim10k_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw10kb_1 <- 
  bind_cols(c_ageb, idw10b = idw_estim10k_b_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw10b) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idwall_1 <- 
  bind_cols(c_ageb, idw_all = idw_estimall_1@data$var1.pred) %>% 
  mutate(year = fecha_etiq_1) %>% 
  select(year, cvegeo, cve_mun, idw_all) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw5k_1, idw10k_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idw10kb_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

idw_ageb_1 <- 
  inner_join(idw_ageb_1, idwall_1, by = c("cvegeo", "year", "cve_mun")) %>% 
  print()

ano_2019 <- 
  idw_ageb_1 %>% 
  mutate(pm25_mean24_idw = case_when(is.na(idw5) & !is.na(idw10) ~ idw10,
                                     is.na(idw5) & is.na(idw10) & !is.na(idw10b) ~ idw10b,
                                     is.na(idw5) & is.na(idw10) & is.na(idw10b) ~ idw_all,
                                     TRUE ~ idw5)) %>% 
  select(year, cvegeo, cve_mun, pm25_mean24_idw) %>% 
  print()

# unir filas
pm25_meananual_ageb <-
  bind_rows(pm25_meananual_ageb, ano_2019) %>% 
  print()

tail(pm25_meananual_ageb)

pm25_meananual_ageb <-
  pm25_meananual_ageb %>% 
  rename(pm25 = pm25_mean24_idw) %>% 
  print()

saveRDS(pm25_meananual_ageb, file = "./Datos/idw_anual.rds")
