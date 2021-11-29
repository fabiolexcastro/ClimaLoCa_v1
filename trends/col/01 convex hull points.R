
# Load libraries -------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, hrbrthemes, rgeos, stringr, ggpubr, tidyverse, sf)

g <- gc(reset = TRUE)
rm(list = ls())


# Load data ---------------------------------------------------------------
cntr <- st_read('//dapadfs/workspace_cluster_9/Coffee_Cocoa2/PROJECTS/2021/CLIMA_LOCA/shp/base/all_countries.shp')
pnts.all <- st_read('../pnts/occ_run6.shp')
colm <- st_read('//catalogue/workspace-cluster9/CLIMA_LOCA/1.Data/shp/base/COL_adm1.shp')
st_crs(pnts.all) <- st_crs(ecdr)

# Intersect country and points --------------------------------------------
st_crs(pnts.all) <- st_crs(4326)
pnts.col <- st_intersection(x = pnts.all, y = colm)
pnts.col <- pnts.col %>% mutate(gid = 1)
plot(st_geometry(pnts.col))

st_write(pnts.col, '../pnts/occ_colm.shp')

# Convex hull -------------------------------------------------------------
plys <- pnts.col %>% 
  dplyr::group_by(gid) %>% 
  dplyr::summarise() %>% 
  st_cast('POLYGON') %>% 
  st_convex_hull()

# Aggregate points - read -------------------------------------------------
shpf <- st_read('../shp/cocoa_smooth_083.shp')

# To make the map ---------------------------------------------------------
gg_pnt <- ggplot() + 
  geom_sf(data = pnts.col, col = 'brown', size = 0.8) +
  geom_sf(data = colm, fill = NA, col = 'slategray') +
  coord_sf(ylim = c(-5, 12)) + 
  ggtitle(label = 'Puntos cacao') +
  theme_ipsum_es() +
  theme(legend.position  = 'bottom',
        legend.text = element_text(size = 12, face = 'bold', vjust = 0.5)) +
  labs(x = 'Lon', y = 'Lat')
  
gg_plg <- ggplot() + 
  geom_sf(data = shpf, col = 'brown', size = 0.8, fill = 'brown', alpha = 0.8) +
  geom_sf(data = colm, fill = NA, col = 'slategray') +
  coord_sf(ylim = c(-5, 12)) + 
  ggtitle(label = 'Polígonos cacao', subtitle = 'Tolerancia = 0.83 dd') +
  theme_ipsum_es() +
  theme(legend.position  = 'bottom', 
        legend.text = element_text(size = 12, face = 'bold', vjust = 0.5)) +
  labs(x = 'Lon', y = 'Lat')

gg_all <- ggarrange(gg_pnt, gg_plg, ncol = 2, nrow = 1)
ggsave(plot = gg_all, filename = '../png/maps/pnts_plgn.png',
       units = 'in', width = 11, height = 6.5, dpi = 300)



