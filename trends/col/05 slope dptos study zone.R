


# Load libraries ----------------------------------------------------------
source('00 load libraries.R')

# Function to use ---------------------------------------------------------
get_clm <- function(zna, var){
  
  # zna <- zne.slp[1,]
  # var <- 'bio_19'
  
  nme <- zna$name
  cat(nme, '\n')
  
  rst <- slp[[parse_number(var)]]
  rst <- raster::crop(rst, zna) %>% raster::mask(., zna)
  
  # Extract by mask - Cocoa zone
  rst <- raster::crop(rst, cvx) %>% raster::mask(., cvx)
  
  # Which max - Raster
  crd <- as.data.frame(xyFromCell(object = rst, cell = which.max(abs(rst[]))))
  
  # Usar el centroide de cada zona
  mtx <- cbind(raster::extract(prec, crd[,1:2]),
               raster::extract(tmax, crd[,1:2]),
               raster::extract(tavg, crd[,1:2]),
               raster::extract(tmin, crd[,1:2]))
  mtx <- as_tibble(mtx) %>% 
    gather(variable, value) %>% 
    mutate(month = parse_number(variable), 
           variable = str_sub(variable, 1, 4)) %>% 
    inner_join(., data.frame(month = 1:12, month_abb = month.abb), by = 'month') %>% 
    dplyr::select(month_abb, variable, value) %>% 
    spread(variable, value) %>% 
    mutate(month_abb = factor(month_abb, levels = month.abb)) %>% 
    arrange(month_abb) %>% 
    mutate(mpio = nme) %>% 
    dplyr::select(mpio, month_abb, tmin, tavg, tmax, prec) %>% 
    mutate(variable = var, 
           longitud = crd$x, 
           latitud = crd$y)
  
  cat('Done!\n')
  return(mtx)
  
}
get_grp <- function(tbl){
  
  # tbl <- ara.b13
  
  nme <- unique(tbl$mpio)
  vrb <- unique(tbl$variable)
  
  cat(nme, ' ', vrb, '\n')
  ggp <- ggplot(data = ara.b13, aes(x = month_abb)) + 
    geom_col(aes(y = prec, fill = 'C')) + 
    scale_fill_manual(values = c('C' = '#63BE5C'), labels = c('C' = 'Prec')) +
    theme_ipsum_es() + 
    labs(x = '', y = 'Precipitación (mm)', fill = '')
  
  ggp <- ggp +
    geom_line(data = ara.b13, aes(x = month_abb, y = tmin * 10, linetype = 'D', group = 1), col = '#009933', size = 1.2) +
    geom_line(data = ara.b13, aes(x = month_abb, y = tavg * 10, linetype = 'E', group = 1), col = '#009933', size = 1.2) +
    geom_line(data = ara.b13, aes(x = month_abb, y = tmax * 10, linetype = 'D', group = 1), col = '#009933', size = 1.2) +
    scale_y_continuous(sec.axis = sec_axis(~./10, name = 'Temperatura ºC')) +
    scale_linetype_manual(name = ' ', 
                          values = c("D" = 2, 'E' = 1), 
                          labels = c("D" = "Temp. Min / Max.", 'E' = 'Temp. Promedio')) +
    theme(legend.position = 'bottom',
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 13),
          plot.title = element_text(size = 20, hjust = 0.5),
          legend.text = element_text(size = 13),
          legend.key.width = unit(3, 'line'),
          legend.title = element_text(size = 13, face = 'bold')) 
  
  ggsave(plot = ggp, filename = glue('../png/graphs/climatogram/{nme}_{vrb}.png'), units = 'in', 
         width = 8, height = 7, dpi = 300)
  cat('Done!\n')
  
}
get_slp <- function(tbl){
  
  # tbl <- ara.b13
  
  dfm <- dir_ls(path = '../raster/bios') %>% 
    grep(paste0(unique(tbl$variable), '.tif'), ., value = TRUE) %>% 
    mixedsort %>% 
    as.character() %>% 
    raster::stack() %>% 
    raster::extract(., crd[,1:2]) %>% 
    as_tibble() |>
    gather(var, value) %>% 
    separate(col = var, into = c('year', 'variable', 'numero'), sep = '_') %>% 
    mutate(variable = paste0(variable, '_', numero), 
           year = as.numeric(str_sub(year, 2, 5))) %>% 
    dplyr::select(-numero)
  
  slp <- pull(dfm, 3) %>% 
    ts() %>% 
    sens.slope() 
  
  rsl <- data.frame(slope = slp$estimates, pvalue = slp$p.value, 
                    variable = unique(tbl$variable), zone = unique(tbl$mpio))
  
  
  gpt <- ggplot(data = dfm, aes(x = year, y = value)) + 
    geom_line(group = 1, type = 'dashed') + 
    geom_smooth(se = FALSE, method = 'loess') +
    theme_ipsum_es() + 
    labs(x = '', y = '') 
  
  ggsave(plot = gpt, filename = glue('../png/graphs/climatogram/line_{unique(tbl$mpio)}_{unique(tbl$variable)}.png'), 
         units = 'in', width = 9, height = 7, dpi = 300)
  
}

# Load data ---------------------------------------------------------------
cvx <- raster::shapefile('../shp/cocoa_smooth_083.shp')

# Slopes data
slp <- dir_ls('../raster/slopes', regexp = '.tif$') %>% 
  grep('slp', ., value = TRUE) %>% 
  as.character() %>% 
  mixedsort() %>% 
  grep(paste0(paste0('bio_', c(1:19), '.tif'), collapse = '|'), ., value = TRUE) %>% 
  stack()

# Pvalue data
pvl <- dir_ls('../raster/slopes', regexp = '.tif$') %>% 
  grep('pvl', ., value = TRUE) %>% 
  as.character() %>% 
  mixedsort() %>% 
  grep(paste0(paste0('bio_', c(1:19), '.tif'), collapse = '|'), ., value = TRUE) %>% 
  stack()

# P value < 0.1 -----------------------------------------------------------
for(i in 1:19){
  slp[[i]][which(pvl[[i]][] > 0.10)] <- NA
}

# Administrative data 
dpt.trg <- st_read('../shp/dpto_object.shp')
mps.trg <- st_read('../shp/mpio_object.shp')
col <- shapefile('D:/CIAT/DATA/colombia/DPTO.shp')

# Change the names of the colnames
dpt.trg <- transmute(dpt.trg, name = DPTO_CNMBR, type = 'dpto')
mpo.trg <- transmute(mps.trg, name = MPIO_CNMBR, type = 'mpio')

# Worldclim data ----------------------------------------------------------
fles <- dir_ls('D:/CIAT/DATA/worldclim/v2_0', regexp = '.tif$')

prec <- grep('prec', fles, value = TRUE) %>% mixedsort %>% as.character %>% stack() %>% raster::crop(., col) %>% raster::mask(., col)
Map('writeRaster', x = unstack(prec), filename = glue('D:/OneDrive - CGIAR/Projects/Cacao Clima -  LoCa/tif/climate/worldclim/v2_0/prec_{1:12}.tif'))
prec <- raster::stack(glue('D:/OneDrive - CGIAR/Projects/Cacao Clima -  LoCa/tif/climate/worldclim/v2_0/prec_{1:12}.tif'))

tmax <- grep('tmax', fles, value = TRUE) %>% mixedsort %>% as.character %>% stack() %>% raster::crop(., col) %>% raster::mask(., col)
Map('writeRaster', x = unstack(tmax), filename = glue('D:/OneDrive - CGIAR/Projects/Cacao Clima -  LoCa/tif/climate/worldclim/v2_0/tmax_{1:12}.tif'))
tmax <- raster::stack(glue('D:/OneDrive - CGIAR/Projects/Cacao Clima -  LoCa/tif/climate/worldclim/v2_0/tmax_{1:12}.tif'))

tmin <- grep('tmin', fles, value = TRUE) %>% mixedsort %>% as.character %>% stack() %>% raster::crop(., col) %>% raster::mask(., col)
Map('writeRaster', x = unstack(tmin), filename = glue('D:/OneDrive - CGIAR/Projects/Cacao Clima -  LoCa/tif/climate/worldclim/v2_0/tmin_{1:12}.tif'))
tmin <- raster::stack(glue('D:/OneDrive - CGIAR/Projects/Cacao Clima -  LoCa/tif/climate/worldclim/v2_0/tmin_{1:12}.tif'))

tavg <- grep('tavg', fles, value = TRUE) %>% mixedsort %>% as.character %>% stack() %>% raster::crop(., col) %>% raster::mask(., col)
Map('writeRaster', x = unstack(tavg), filename = glue('D:/OneDrive - CGIAR/Projects/Cacao Clima -  LoCa/tif/climate/worldclim/v2_0/tavg_{1:12}.tif'), overwrite = TRUE)
tavg <- raster::stack(glue('D:/OneDrive - CGIAR/Projects/Cacao Clima -  LoCa/tif/climate/worldclim/v2_0/tavg_{1:12}.tif'))

# Join both ---------------------------------------------------------------
zne <- rbind(dpt.trg, mpo.trg)
cca <- st_read('../shp/cocoa_smooth_083.shp')
plot(st_geometry(cca))

plot(st_geometry(zne))
plot(st_geometry(cca), add = TRUE, border = 'red')

# Extract by mask  --------------------------------------------------------
zne.slp <- as(zne, 'Spatial')
slp <- raster::crop(slp, zne.slp) %>% raster::mask(., zne.slp)
slp <- round(slp, 3)
names(slp) <- glue('bio_{1:19}')

# -------------------------------------------------------------------------
# Get climate for each zone max value absolute ----------------------------
# -------------------------------------------------------------------------

# Arauca ------------------------------------------------------------------
ara.b13 <- get_clm(zna = zne.slp[1,], var = 'bio_19')
get_grp(tbl = ara.b13)
get_slp(tbl = ara.b13)
