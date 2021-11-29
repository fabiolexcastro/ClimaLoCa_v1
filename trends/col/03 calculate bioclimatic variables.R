
# Load libraries -------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, glue, dismo, gtools, hrbrthemes, rgeos, stringr, ggpubr, tidyverse, sf)

g <- gc(reset = TRUE)
rm(list = ls())

# Function to extract by mask  --------------------------------------------
make_extract <- function(yr){
  
  # yr <- 1980
  
  cat('Start ', yr, '\n')
  fls <- grep(yr, fles, value = TRUE)
  ppt <- grep('ppt', fls, value = TRUE) %>% mixedsort()
  tmx <- grep('tmax', fls, value = TRUE) %>% mixedsort()
  tmn <- grep('tmin', fls, value = TRUE) %>% mixedsort()
  
  ppt <- stack(ppt)
  tmx <- stack(tmx)
  tmn <- stack(tmn)
  
  bcl <- dismo::biovars(prec = ppt, tmin = tmn, tmax = tmx)
  Map('writeRaster', x = unstack(bcl), filename = glue('../raster/bios/{yr}_bio_{1:19}.tif'), overwrite = TRUE)
  cat('Done\n')
  
}

# Load data ---------------------------------------------------------------
root <- '../raster/tc'
year <- list.files(root, full.names = TRUE)
fles <- list.files(year, full.names = TRUE, pattern = '.tif$')
vars <- c('ppt', 'tmax', 'tmin')
cntr <- 'COL'

# Make the bioclim variables ----------------------------------------------
map(.x = 1981:2020, .f = make_extract)





