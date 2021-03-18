# ============================================================================ #
# Script: Union de datos de valores predichos y observados en 1 malla de datos
# Autor: Luis Enrique Reyes
# Fecha: 
# ============================================================================ #

library(tidyverse)


### 2000 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2000_tmp.rds") %>% 
  print()

pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2000_p_y_o.rds") %>% 
  print()

pm10 <-
  pm10 %>% 
  mutate(suf_pm25 = NA_real_) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("site", "year")) %>% 
  mutate(prop_gnl = 35.6) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2000.rds")

### 2001 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2001_tmp.rds") %>% 
  print()

pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2001_p_y_o.rds") %>% 
  print()

pm10 <-
  pm10 %>% 
  mutate(suf_pm25 = NA_real_) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("site", "year")) %>% 
  mutate(prop_gnl = 35.6) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2001.rds")

### 2002 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2002_tmp.rds") %>% 
  print()

pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2002_p_y_o.rds") %>% 
  print()

pm10 <-
  pm10 %>% 
  mutate(suf_pm25 = NA_real_) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("site", "year")) %>% 
  mutate(prop_gnl = 35.6) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2002.rds")

### 2003 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2003_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2003_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2003_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "NE", "NO", "SE", "SO2"),
                  datos_est_pm25 = c(NA_real_, 53.68, 50.68, 53.15, 52.33)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 34.9) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2003.rds")

### 2004 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2004_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2004_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2004_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "NE", "NO", "SE", "SO2"),
                  datos_est_pm25 = c(86.9, 75.9, 71, NA_real_, 74.8)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 36.7) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2004.rds")

### 2005 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2005_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2005_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2005_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "NE", "NO", "SE", "SO2"),
                  datos_est_pm25 = c(NA_real_, 46, 54.7, NA_real_, 31.3)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 32.6) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2005.rds")

### 2006 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2006_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2006_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2006_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "NE", "NO", "SE", "SO2"),
                  datos_est_pm25 = c(57.2, 54.2, 67.9, NA_real_, 63.2)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 34) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2006.rds")

### 2007 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2007_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2007_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2007_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "NE", "NO", "SE", "SO2"),
                  datos_est_pm25 = c(64.6, 48, 88.5, 18.3, 52.8)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 37.6) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2007.rds")
### 2008 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2008_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2008_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2008_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "NE", "NO", "SE", "SO2"),
                  datos_est_pm25 = c(75.4, 57.1, 80, 51.3, 75.9)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 34.3) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2008.rds")
### 2009 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2009_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2009_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2009_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "NE", "NO", "SE", "SO2"),
                  datos_est_pm25 = c(63, 54.5, 60.8, 96.7, 63)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 34) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2009.rds")

### 2010 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2010_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2010_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2010_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "N", "NE", "NO", "NO2", "SE", "SO2"),
                  datos_est_pm25 = c(56.7, 81.3, 51.2, 59.7, 13.4, 86.5, 78.6)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 37.4) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2010.rds")


### 2011 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2011_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2011_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2011_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "N", "NE", "NO", "NO2", "SE", "SO2"),
                  datos_est_pm25 = c(89, 34.2, 77.8, 90.6, NA_real_, 87, 95.8)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 34.3) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2011.rds")

### 2012 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2012_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2012_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2012_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "N", "NE", "NE2", "NO", "NO2", "SE", "SE2", "SO2"),
                  datos_est_pm25 = c(90.4, NA_real_, 38.2, 56, 96.17, NA_real_, 86.3, 19.6, 91.5)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 32.5) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2012.rds")

### 2013 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2013_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2013_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2013_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "N", "NE", "NE2", "NO", "NO2", "SE", "SE2", "SO2"),
                  datos_est_pm25 = c(41.1, NA_real_, 29.5, NA_real_, 64.8, NA_real_,
                                     45, 47.4, 53.4)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 36.3) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2013.rds")

### 2014 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2014_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2014_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2014_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "N", "NE", "NE2", "NO", "NO2", "S2", "SE", "SE2", "SO2"),
                  datos_est_pm25 = c(67, NA_real_, 79.4, 31, 57.2, NA_real_, 22, 55, NA_real_, 74)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 34.2) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2014.rds")

### 2015 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2015_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2015_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2015_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "N", "NE", "NE2", "NO", "NO2", "S2", "SE", "SE2", "SO2"),
                  datos_est_pm25 = c(48.7, NA_real_, 60, 70, 86, NA_real_, 
                                     29, 42.4, NA_real_, 42.7)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 37) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2015.rds")

### 2016 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2016_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2016_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2016_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "N", "NE", "NE2", "NO", "NO2", "S2", "SE", "SE2", "SO2"),
                  datos_est_pm25 = c(NA_real_, NA_real_, NA_real_, 67.2, 36.6, NA_real_, 
                                     45, 43.4, NA_real_, 50)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 38.1) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2016.rds")

### 2017 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2017_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2017_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2017_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "N", "NE", "NE2", "NO", "NO2", "S2", "SE", "SE2", "SO2"),
                  datos_est_pm25 = c(49, NA_real_, NA_real_, 27, NA_real_, NA_real_, 
                                     NA_real_, 69, NA_real_, 39)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 33.8) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2017.rds")

### 2018 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2018_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2018_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2018_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "N", "NE", "NE2", "NO", "NO2", "S2", "SE", "SE2", "SO2"),
                  datos_est_pm25 = c(41.6, NA_real_, 20.8, 40, 23.8, 18.6, 
                                     NA_real_, 58.6, 60.5, NA_real_)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 35.2) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2018.rds")

### 2019 =======================================================================

pm10 <- read_rds("./Datos/07_tmp_pm10_con_prop/pm10_2019_tmp.rds") %>% 
  print()
suf_est <- read_rds("./Datos/08_metricas_pm25_suf_estacion/pm25_2019_est_tmp.rds") %>% 
  rename(suf_pm25 = pm25,
         prop_est = prop) %>% 
  select(!pm10) %>% 
  print()
pm25 <- read_rds("./Datos/05_metricas_pm25_p_y_o/pm25_2019_p_y_o.rds") %>% 
  print()

pm10 <-
  left_join(pm10, suf_est, by = c("site")) %>% 
  print()

pm25 <-
  inner_join(pm10, pm25, by = c("year", "site")) %>% 
  print()

tmp <- data.frame(site = c("CE", "N", "NE", "NE2", "NO", "NO2", "S2", "SE", "SE2", "SO2"),
                  datos_est_pm25 = c(60, 66.8, 76.7, 82.7, 62.7, 77.5, 
                                     NA_real_, 72.8, 60.8, NA_real_)) %>% 
  print()

pm25 <-
  inner_join(pm25, tmp, by = "site") %>% 
  print()

pm25 <-
  pm25 %>% 
  mutate(prop_gnl = 40.4) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm25, file = "./Datos/09_tab_pm10_pm25/tab_2019.rds")

### UNIR TODAS =================================================================

pm_2000 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2000.rds") 
pm_2001 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2001.rds") 
pm_2002 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2002.rds") 
pm_2003 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2003.rds") 
pm_2004 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2004.rds") 
pm_2005 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2005.rds") 
pm_2006 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2006.rds") 
pm_2007 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2007.rds") 
pm_2008 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2008.rds") 
pm_2009 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2009.rds") 
pm_2010 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2010.rds") 
pm_2011 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2011.rds") 
pm_2012 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2012.rds") 
pm_2013 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2013.rds") 
pm_2014 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2014.rds") 
pm_2015 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2015.rds") 
pm_2016 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2016.rds") 
pm_2017 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2017.rds") 
pm_2018 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2018.rds") 
pm_2019 <- read_rds("./Datos/09_tab_pm10_pm25/tab_2019.rds") 

pm <-
  bind_rows(pm_2000, pm_2001, pm_2002, pm_2003, pm_2004, pm_2005, pm_2006, pm_2007,
            pm_2008, pm_2009, pm_2010, pm_2011, pm_2012, pm_2013, pm_2014, pm_2015,
            pm_2016, pm_2017, pm_2018, pm_2019) %>% 
  select(year, site, pm10, suf_pm25, datos_est_pm25, prop_est, prop_gnl, prop25_est,
         prop25_gnl, pm25) %>% 
  print()

saveRDS(pm, file = "./Datos/09_tab_pm10_pm25/tab_2000_2019.rds")


write.csv(pm,"./Datos/09_tab_pm10_pm25/tab_2000_2019.csv", row.names=F)
