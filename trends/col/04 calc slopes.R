
# Load libraries -------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, glue, dismo, gtools, hrbrthemes, rgeos, stringr, ggpubr, tidyverse, sf, trend)

g <- gc(reset = TRUE)
rm(list = ls())

# Function ----------------------------------------------------------------
calc_slope <- function(var){
  cat(var, '\n')
  stk <- grep(var, fles, value = TRUE) %>% 
    stack() %>% 
    raster::crop(., plgn) %>% 
    raster::mask(., plgn)
  cat('Raster to Table\n')
  tbl <- rasterToPoints(stk, spatial = FALSE)
  tbl <- tbl %>% as_tibble %>% mutate(gid = 1:nrow(.))
  tbl <- tbl %>% gather(var, value, -gid, -x, -y)
  tbl <- tbl %>% separate(data = ., col = var, into = c('year', 'variable', 'number'), sep = '_')
  tbl <- tbl %>% mutate(year = parse_number(year))
  tbl <- tbl %>% mutate(variable = paste0(variable, '_', number)) %>% dplyr::select(-number)
  gds <- unique(tbl$gid)
  
  trn <- map(.x = gds, .f = function(i){
    slp <- tbl %>% filter(gid == i) %>% arrange(year) %>% pull(value) %>% ts() %>% sens.slope() 
    rsl <- data.frame(gid = i, slope = slp$estimates, p_value = slp$p.value, variable = gsub('.tif', '', var))
  }) %>% bind_rows() %>% as_tibble()
  
  crd <- tbl %>% distinct(gid, x, y)
  rsl <- trn %>% mutate(slope_abs = abs(slope)) %>% top_n(x = ., n = 1, wt = slope_abs) %>% inner_join(., crd, by = 'gid')
  # trn <- inner_join(trn, crd, by = 'gid')
  cat('----Finish----\n')
  return(rsl)
  
}
make_graph <- function(var){
  
  # var <- 'bio_1'
  
  maxm <- trnd
  max <- maxm %>% filter(variable == var)
  vls <- list.files('../raster/bios', full.names = TRUE, pattern = '.tif') %>% 
    grep(paste0(var, '.tif'), ., value = TRUE) %>% 
    stack() %>% 
    raster::extract(.,  max[,6:7]) %>% 
    cbind(max, .) %>% 
    gather(var, value, -gid, -slope, -p_value, -x, -y, -variable) %>% 
    dplyr::select(var, value) %>% 
    mutate(year = parse_number(var)) %>% 
    filter(var != 'slope_abs') %>% 
    arrange(year)
  
  ggp <- ggplot(data = vls, aes(x = year, y = value)) +
    geom_smooth(se = FALSE, method = 'loess', size = 1.3) +
    geom_line(size = 1.1) +
    theme_ipsum_es() +
    theme(axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6)) +
    labs(x = '', y = 'Value') +
    scale_x_continuous(breaks = seq(1980, 2020, 5), labels = seq(1980, 2020, 5))
  
  cat('Done!\n')
  return(ggp)
  
}
get_values <- function(vr){
  # vr <- 'bio_1'
  df <- maxm %>% filter(variable == vr)
  rs <- grep(paste0(vr, '.tif'), fles, value = TRUE) %>% 
    stack() %>% 
    raster::extract(., df[,6:7]) %>% 
    as_tibble() %>% 
    gather(var, value) %>% 
    mutate(year = str_sub(var, 2, 5))
  cat('Done!')
  return(rs)
}

# Load data ---------------------------------------------------------------
fles <- list.files('../raster/bios', full.names = TRUE, pattern = '.tif$')
plgn <- shapefile('../shp/cocoa_smooth_083.shp')
mask <- raster('../raster/bios/1981_bio_1.tif') * 0 + 1
mktb <- rasterToPoints(mask, spatial = FALSE) %>% as_tibble() %>% mutate(gid = 1:nrow(.)) %>% dplyr::select(-3)

# Apply trends ------------------------------------------------------------
trnd <- map(.x = glue('bio_{1:19}.tif'), .f = calc_slope)
trnd <- bind_rows(trnd)
write.csv(trnd, '../tbl/trend/trends_bios.csv', row.names = FALSE)

trnd <- read_csv('../tbl/trend/trends_bios.csv')
trnd <- trnd %>% mutate(slope_abs = abs(slope))
trnd <- bind_rows(trnd)
pnts <- trnd
coordinates(pnts) <- ~ x + y
pnts@data$variable <- as.character(pnts@data$variable)
shapefile(pnts, '../shp/max_trends_v3.shp')

trnd
write.csv(trnd, '../tbl/trend/max_trends_bios.csv', row.names = FALSE)

trnd <- read_csv('../tbl/trend/max_trends_bios.csv')
maxm <- trnd

# Get graph ---------------------------------------------------------------
grps <- map(.x = unique(maxm$variable), .f = make_graph)

# Bio 1 Mean temperature
b01 <- grps[[1]] +
  theme(axis.text.x = element_text(size = 11), 
        axis.text.y = element_text(size = 11)) +
  annotate('text', x = 1995, y = 26.3, size = 4, label = 'Pendiente = 0.02°C por año') +
  annotate('text', x = 1995, y = 26.4, size = 4, label = 'p-valor = 0.0003')
ggsave(plot = b01, filename = '../png/graphs/b01.png', units = 'in', width = 5, height = 4, dpi = 300)

vls_b01 <- get_values(vr = 'bio_1')
25.6 + 0.017 * 40

# Bio 6 Min temp of coldest month
b06 <- grps[[6]] +
  theme(axis.text.x = element_text(size = 11), 
        axis.text.y = element_text(size = 11)) +
  annotate('text', x = 1992, y = 20.5, size = 4, label = 'Pendiente = 0.033°C por año') +
  annotate('text', x = 1992, y = 20.7, size = 4, label = 'p-valor = 0.001')
ggsave(plot = b06, filename = '../png/graphs/b06.png', units = 'in', width = 5, height = 4, dpi = 300)

vls_b06 <- get_values(vr = 'bio_6')
tail(vls_b06)
19.7 * 0.02

# Bio 9 Mean Temperature of Driest Quarter
b09 <- grps[[9]] +
  theme(axis.text.x = element_text(size = 11), 
        axis.text.y = element_text(size = 11)) +
  annotate('text', x = 1990, y = 28.0, size = 4, label = 'Pendiente = 0.05°C por año') +
  annotate('text', x = 1990, y = 28.3, size = 4, label = 'p-valor = 0.00001')
ggsave(plot = b09, filename = '../png/graphs/b09.png', units = 'in', width = 5, height = 4, dpi = 300)

vls_b09 <- get_values(vr = 'bio_9')

# Bio 11 Mean temp of coldest quarter
b11 <- grps[[7]] +
  annotate('text', x = 1985, y = 26.1, size = 3, label = 'Slope = 0.02C per year') +
  annotate('text', x = 1985, y = 26.3, size = 3, label = 'p-value = 0.02')
ggsave(plot = b11, filename = '../png/graphs/b11.png', units = 'in', width = 5, height = 4, dpi = 300)

vls_b11 <- get_values(vr = 'bio_11')
24 * 0.02

# Bio 12 prec acum
b12 <- grps[[12]] +
  theme(axis.text.x = element_text(size = 11), 
        axis.text.y = element_text(size = 11)) +
  annotate('text', x = 1995, y = 3800, size = 4, label = 'Pendiente: 17.8 mm por año') +
  annotate('text', x = 1995, y = 3900, size = 4, label = 'p-valor = 0.01')
ggsave(plot = b12, filename = '../png/graphs/b12.png', units = 'in', width = 5, height = 4, dpi = 300)

maxm

vls_b12 <- get_values(vr = 'bio_12')
32*40 + 2037

# Bio 17 
b17 <- grps[[17]] +
  theme(axis.text.x = element_text(size = 11), 
        axis.text.y = element_text(size = 11)) +
  annotate('text', x = 2005, y = 340, size = 4, label = 'Pendiente: -3.4 mm por año') +
  annotate('text', x = 2005, y = 360, size = 4, label = 'p-valor = 0.0311')
ggsave(plot = b17, filename = '../png/graphs/b17.png', units = 'in', width = 5, height = 4, dpi = 300)

vls_b17 <- get_values(vr = 'bio_17')

# Bio 19 precip of coldest quarter
b19 <- grps[[12]] +
  annotate('text', x = 1985, y = 2500, size = 3, label = 'Slope: 22 mm per year') +
  annotate('text', x = 1985, y = 2600, size = 3, label = 'p-value = 0.006')
ggsave(plot = b19, filename = '../png/graphs/b19.png', units = 'in', width = 5, height = 4, dpi = 300)

vls_b19 <- get_values(vr = 'bio_19')
22*40


