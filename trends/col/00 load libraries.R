
# Load libraries -------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, glue, dismo, fs, gtools, hrbrthemes, rgeos, stringr, ggpubr, tidyverse, sf, trend)

g <- gc(reset = TRUE)
rm(list = ls())

