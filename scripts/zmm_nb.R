
library(tidyverse)
library(janitor)
library(sf)

casos <- 
  st_read("./Capas/casos_utm.shp") %>% 
  rename(registro = registr, diagnostico = dignstc) %>% 
  print()

zmm_ageb <- 
  st_read("./Capas/zmm_ageb_utm.shp") %>% 
  clean_names() %>% 
  print()

zmm_ageb <- 
  zmm_ageb %>% 
  mutate(tot_casos = lengths(st_intersects(zmm_ageb, casos))) %>% 
  relocate(geometry, .after = last_col()) %>% 
  print()

round(mean(zmm_ageb$tot_casos) ^ 0 * exp(-mean(zmm_ageb$tot_casos)) / factorial(0) * 100, 1)

md_p1 <- glm(tot_casos ~ 1, family = poisson, data = zmm_ageb)

library(jtools)
print(summ(md_p1, confint = TRUE, digits = 5))

library(broom)
md_p1 %>% 
  glance() %>% 
  mutate(pearson_gof = sum(residuals(md_p1, type = "pearson") ^ 2)) %>% 
  select(logLik, deviance, pearson_gof, AIC, BIC, df.null, nobs, null.deviance) %>% 
  rename(deviance_gof = deviance) %>% 
  print()

library(performance)
check_overdispersion(md_p1)

library(MASS)
md_nb1 <- glm.nb(tot_casos ~ 1, data = zmm_ageb, link = log)
summ(md_nb1, digits = 5, confint = TRUE)

library(pscl)
odTest(md_nb1)

pred_nb1 <- 
  predprob(md_nb1) %>% 
  colMeans %>% 
  as_tibble() %>% 
  mutate(tot_casos = 0:5) %>% 
  rename(pred_nb = value) %>% 
  print()

casos_nb_counts <- 
  zmm_ageb %>% 
  mutate(casos_factor = factor(tot_casos, levels = 0:5)) %>% 
  tabyl(casos_factor) %>% 
  mutate(tot_casos = 0:5) %>% 
  right_join(pred_nb1, by = "tot_casos") %>% 
  dplyr::select(tot_casos, n, percent, pred_nb) %>% 
  rename(freq = n,
         observ_prop = percent) %>%
  print()

casos_nb_counts <- 
  casos_nb_counts %>% 
  mutate(pred_poiss = dpois(0:5, mean(fitted(md_p1)))) %>% 
  pivot_longer(cols = c(observ_prop, pred_nb, pred_poiss), 
               names_to = "tipo_valor", 
               values_to = "valor") %>% 
  print()

ggplot() + 
  geom_line(data = casos_nb_counts, aes(x = tot_casos, y = valor, colour = tipo_valor, size = tipo_valor)) + 
  geom_point(data = casos_nb_counts, aes(x = tot_casos, y = valor, colour = tipo_valor), size = 2) + 
  labs(title = "Probabilidad", x = "count", y = "") + 
  scale_x_continuous(breaks = 0:5) + 
  scale_colour_discrete(labels = c("Proporción observada", "Binomial Negativo", "Poisson")) + 
  scale_size_manual(values = c(3, 1 ,1)) + 
  theme_bw(base_family = "Avenir") +
  theme(plot.title = element_text(size = 20, hjust = 0.5), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(size = 12), 
        axis.title.x = element_text(size = 14), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(size = 14), 
        legend.key.width = unit(1.5, "cm"))

nb1.res_pred <- 
  zmm_ageb %>% 
  mutate(respear = residuals(md_nb1, "pearson"), 
         resdev = residuals(md_nb1, "deviance"), 
         nb1_pred = predict(md_nb1, newdata = zmm_ageb, type = "response")) %>% 
  print()


