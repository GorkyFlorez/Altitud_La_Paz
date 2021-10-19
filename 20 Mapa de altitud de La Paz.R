# Librerias
library(raster)
library(sf)
library(ggspatial)
library(sp)
library(ggplot2)
library(tmap)
library(maptools)
library(elevatr)
# Cargamos data
Bol_dep    <- getData('GADM', country='Bolivia', level=1) %>%st_as_sf() 
Bolivia    <- getData('GADM', country='Bolivia', level=0) %>%st_as_sf() 
SurAmerica     <- st_read ("SHP/SurAmerica.shp")  
SurAmeric      <- st_transform(SurAmerica,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Paz       <- subset(Bol_dep, NAME_1 == "La Paz")
elevation  = get_elev_raster(Bolivia ,z=8)
Amb      <- crop(elevation , Bolivia)
Amb      <-Amb  <- mask(Amb  , Bol_dep)

alt_Paz    <- crop(elevation , Paz)
alt_Paz    <-alt_Paz  <- mask(alt_Paz  , Paz)
plot(alt_Paz )
# Creacion del DEM
slope = terrain(Amb , opt = "slope") 
aspect = terrain(Amb , opt = "aspect")
hill = hillShade(slope, aspect, angle = 40, direction = 270)

# Creacion del DEM
slope_paz = terrain(alt_Paz , opt = "slope") 
aspect_paz = terrain(alt_Paz , opt = "aspect")
hill_paz = hillShade(slope_paz, aspect_paz, angle = 40, direction = 270)

SU=ggplot()+
  geom_sf(data = SurAmeric , fill="white", color="black", size=0.2)+
  geom_sf(data = Bolivia, fill="black", size=0.2)+
  theme_void()+
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))
SU.grob <- ggplotGrob(SU)

Bol=ggplot()+
  geom_sf(data = Bol_dep, fill="white", color="black", size=0.5)+
  geom_sf_text(data = st_as_sf(Bol_dep), aes(label =  NAME_1), size = 2,family="serif") +
  geom_sf(data = Paz, fill="black")+
  theme_void()+
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))
Bol.grob <- ggplotGrob(Bol)

MAP=tm_shape(hill,ylim=c(-25,  -9),xlim=c( -75, -52)) +
  tm_raster(palette = gray(0:10 / 10), style = "cont", legend.show = FALSE)+
  tm_shape(Bol_dep)+
  tm_borders("white",lwd=2)+
  tm_text("NAME_1",size = .9, col="black",shadow=TRUE,
          bg.color="white", bg.alpha=.45)+
  tm_compass(type="arrow", position=c(.15, .05))+
  tm_scale_bar(position = c(0.2, .005), size=.8)+
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
            system.file("img/tmap.png", package = "tmap")),height = 2, position = c(0.005, 0.005))+
  tm_grid(col = "grey",ticks = T, labels.col = "black")+
  tm_layout(title = "MAPA de RELIEVE de la PAZ", fontfamily = "serif",
            title.position =  c(.5, .9))+
  tm_credits("Data: DEM SRTM \n#Aprende R desde Cero Para SIG \nGorky Florez Castillo",  bg.color="white", bg.alpha=.45,
             position = c(0.57, 0.0001), col = "black", fontface="bold", fontfamily = "serif")
g1= tmap_grob(MAP)

Paz_altf =tm_shape(hill_paz) +
  tm_raster(palette = gray(0:10 / 10), style = "cont", legend.show = FALSE)

Paz_altf1 = tmap_grob(Paz_altf )


# Mapa final
library(cowplot)
im= ggdraw() +
  coord_equal(xlim = c(0, 12), ylim = c(0, 10), expand = FALSE) +
  draw_plot(g1, width = 10, height = 10,x = 1, y = 0.05)+
  draw_plot(SU.grob , width =2, height = 2 ,x = 1, y = 6.5)+
  draw_plot(Bol.grob, width = 2.7, height = 2.7 ,x = 1, y = 2.3)+
  draw_plot(Paz_altf1, width = 5, height = 5 ,x = 7.6, y = 3)+
  theme_void()+
  theme(panel.background = element_rect(fill = "white"))
# Exportacion
ggsave(plot = im ,"MAPAS/PAZ.png",
       units = "cm", width = 29,height = 21, dpi = 900)# guardar grafico














