


# Load libraries ----------------------------------------------------------
source('00 load libraries.R')


# Load data ---------------------------------------------------------------

# Slopes data
slp <- dir_ls('../raster/slopes', regexp = '.tif$') %>% 
  grep('slp', ., value = TRUE) %>% 
  as.character() %>% 
  mixedsort() %>% 
  grep(paste0(paste0('bio_', c(1:19), '.tif'), collapse = '|'), ., value = TRUE) %>% 
  stack()

# Administrative data 
dpt.trg <- st_read('../shp/dpto_object.shp')
mps.trg <- st_read('../shp/mpio_object.shp')

# Change the names of the colnames
dpt.trg <- transmute(dpt.trg, name = DPTO_CNMBR, type = 'dpto')
mpo.trg <- transmute(mps.trg, name = MPIO_CNMBR, type = 'mpio')

# Worldclim data ----------------------------------------------------------
fles <- dir_ls('D:/CIAT/DATA/worldclim/v2_0', regexp = '.tif$')


# Join both ---------------------------------------------------------------
zne <- rbind(dpt.trg, mpo.trg)
cca <- st_read('../shp/cocoa_smooth_083.shp')
plot(st_geometry(cca))

# Extract by mask  --------------------------------------------------------
zne.slp <- as(zne, 'Spatial')
slp <- raster::crop(slp, zne.slp) %>% raster::mask(., zne.slp)
slp <- round(slp, 3)
names(slp) <- glue('bio_{1:19}')

# Function to use ---------------------------------------------------------
get_clm <- function(zna, var){
  
  zna <- zne.slp[1,]
  var <- 'bio_13'
  
  rst <- slp[[parse_number(var)]]
  rst <- raster::crop(rst, zna) %>% raster::mask(., zna)
  crd <- as.data.frame(xyFromCell(object = rst, cell = which.max(rst[])))
  
  
  
  
}

