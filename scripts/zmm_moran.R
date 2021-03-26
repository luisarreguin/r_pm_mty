
# https://rstudio-pubs-static.s3.amazonaws.com/126356_ef7961b3ac164cd080982bc743b9777e.html

library(rgdal)
casos <- readOGR("./Capas/casos_utm.shp")
zmm_mun <- readOGR("./Capas/zmm_mun_utm.shp")
zmm_ageb <- readOGR("./Capas/zmm_ageb_utm.shp")
zmm_area <- readOGR("./Capas/zmm_area_utm.shp")

plot(zmm_mun)
plot(casos, col = '#104E8B', cex = .5, pch = 20, add = T)

library(GISTools)
count_cases <- poly.counts(casos, zmm_ageb)
count_cases
zmm_ageb@data$tot_casos <- count_cases
head(zmm_ageb@data, 20)
zmm_ageb@data$casos_dens <- zmm_ageb$tot_casos/poly.areas(zmm_ageb)
head(zmm_ageb@data, 20)

library(classInt)
colours <- brewer.pal(5, "YlOrBr")
breaks <- classIntervals(zmm_ageb$casos_dens, n = 5, style = "jenks")
breaks <- breaks$brks
plot(breaks, col = colours)

plot(zmm_mun)
plot(zmm_ageb, col = colours[findInterval(zmm_ageb@data$casos_dens, breaks, 
                                          all.inside = TRUE)], axes=F, asp=T, add = T)

library(spdep)
coordsW <- coordinates(zmm_ageb)
plot(coordsW, cex = .3, pch = 20)

zmm_ageb_nb <- poly2nb(zmm_ageb, queen = T)
plot(zmm_ageb_nb, coordinates(coordsW), col = "red", cex = .3, pch = 20)
plot(zmm_ageb, border = "grey70", add = T)

zmm_ageb.lw <- nb2listw(zmm_ageb_nb, style = "C", zero.policy = TRUE)
head(zmm_ageb.lw$neighbours)

im_zmm_ageb_global_density <- moran.test(zmm_ageb@data$casos_dens, zmm_ageb.lw, zero.policy = TRUE)
im_zmm_ageb_global_density

gc_zmm_ageb_global_density <- geary.test(zmm_ageb@data$casos_dens, zmm_ageb.lw, zero.policy = TRUE)
gc_zmm_ageb_global_density

go_zmm_ageb_global_density <- globalG.test(zmm_ageb@data$casos_dens, zmm_ageb.lw, zero.policy = TRUE)
go_zmm_ageb_global_density

im_zmm_ageb_local <- localmoran(zmm_ageb@data$tot_casos, zmm_ageb.lw, zero.policy = TRUE)
head(im_zmm_ageb_local)
im_zmm_ageb_local_dens <- localmoran(zmm_ageb@data$casos_dens, zmm_ageb.lw, zero.policy = TRUE)
head(im_zmm_ageb_local_dens)

zmm_ageb@data$BLocI <- im_zmm_ageb_local[,1]
zmm_ageb@data$BLocIz <- im_zmm_ageb_local[,4]
zmm_ageb@data$BLocIR <- im_zmm_ageb_local_dens[,1]
zmm_ageb@data$BLocIRz <- im_zmm_ageb_local_dens[,4]

head(zmm_ageb@data)

moran_colours <- rev(brewer.pal(7, "RdGy"))

breaks <- c(-1000, -2.58, -1.96, -1.65, 1.65, 1.96, 2.58)
plot(breaks, col = moran_colours)

plot(zmm_ageb, col = moran_colours[findInterval(zmm_ageb@data$BLocIRz, breaks, all.inside = TRUE)], axes = F, asp = T)

go_zmm_ageb_local_dens <- localG(zmm_ageb@data$casos_dens, zmm_ageb.lw, zero.policy = TRUE)
head(go_zmm_ageb_local_dens)

zmm_ageb@data$BLocGiRz <- go_zmm_ageb_local_dens
head(zmm_ageb@data)

go_colours <- rev(brewer.pal(7, "RdBu"))
plot(zmm_ageb, col = go_colours[findInterval(zmm_ageb@data$BLocGiRz, breaks, all.inside = TRUE)], axes = F, asp = T)


