# -------------------------------------------------------------------------
# Proyecto: Javeriana Tesos por el Ambiente
# By:  Cesar A. Saavedra
# Mindlabs
# Diciembre 2024
# R options ---------------------------------------------------------------
g <- gc(reset = T); rm(list = ls())
options(warn = -1, scipen = 999)
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(dplyr, tidyr, ggplot2, ggh4x, ggrepel, foreign, ggcharts, mdthemes,forcats,
                                readxl, raster, geodata, terra, sf, sp, lubridate))

# Ruta de trabajo ---------------------------------------------------------
root <- "/Users/cesara.saavedravanegas/Documents/GitHub/MindLabs/"
prj <- "Javeriana"
# Lectura de datos --------------------------------------------------------
db <- readxl::read_xlsx(paste0(root,prj,"/1.Data/DB_Tesos_Javeriana.xlsx"))
head(db)

# Descriptivo -------------------------------------------------------------
# Pregunta 6
Pr <- db |> dplyr::select(p6)
# prop.table(table(Pr))*100
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

# Pregunta 7
Pr <- db |> dplyr::select(p7)
# prop.table(table(Pr))*100
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

# Pregunta 14
Pr <- db |> dplyr::select(p25)
# prop.table(table(Pr))*100
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable
p23_1