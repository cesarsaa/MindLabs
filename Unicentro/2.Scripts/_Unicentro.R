# -------------------------------------------------------------------------
# Proyecto: Imagen de marca centro comercial Unicentro
# By:  Cesar A. Saavedra
# Mindlabs
# Julio 2024

# R options ---------------------------------------------------------------
g <- gc(reset = T); rm(list = ls())
options(warn = -1, scipen = 999)
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(dplyr, tidyr, ggplot2, foreign,
                                raster, geodata, terra, sf, sp, lubridate))
root <- "D:/UNICENTRO/"

# Lectura de datos --------------------------------------------------------
db <- foreign::read.spss(file = paste0(root,"Base_Final_(1)_15.07.2024.sav"),
                         use.value.labels = T,
                         to.data.frame = T)
head(db)

# analisis exploratorio ---------------------------------------------------


# wordcloud ---------------------------------------------------------------


# -------------------------------------------------------------------------


