
# Load libraries -------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, glue, hrbrthemes, rgeos, stringr, ggpubr, tidyverse, sf)

g <- gc(reset = TRUE)
rm(list = ls())

# Function to extract by mask  --------------------------------------------
make_extract <- function(var){
  
  # var <- vars[1]
  
  cat('Start ', var, '\n')
  fls <- grep(var, fles, value = TRUE)
  yrs <- parse_number(basename(fls))
  
  rst <- map(.x = 1:length(yrs), .f = function(k){
    
    cat('Years ', yrs[k], '\n')
    rs <- grep(yrs[k], fls, value = TRUE) %>% stack()
    rs <- raster::crop(rs, limt)
    rs <- raster::mask(rs, limt)
    dr <- glue('../raster/tc/{yrs[k]}')
    ifelse(!dir.exists(dr), dir.create(dr, recursive = TRUE), print('Already exists'))
    Map('writeRaster', x = unstack(rs), filename = glue('{dr}/{var}_{yrs[k]}_{1:12}.tif'), overwrite = TRUE)
    cat('Done\n')
    
  })
  
}

# Load data ---------------------------------------------------------------
root <- '//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_guatemala/_data/_nc/_world'
fles <- list.files(root, full.names = TRUE, pattern = '.nc$')
fles
vars <- c('ppt', 'tmax', 'tmin')
cntr <- 'COL'
limt <- st_read('//catalogue/workspace-cluster9/CLIMA_LOCA/1.Data/shp/base/COL_adm0.shp')

# Extract mask country ----------------------------------------------------
map(.x = vars[2:length(vars)], .f = make_extract)





