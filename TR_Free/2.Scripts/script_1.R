# -------------------------------------------------------------------------
# Proyecto: Tarrito rojo sin azucar
# By:  Cesar A. Saavedra
# Mindlabs
# Octubre 2024
# R options ---------------------------------------------------------------
g <- gc(reset = T); rm(list = ls())
options(warn = -1, scipen = 999)
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(dplyr, tidyr, ggplot2, ggh4x, ggrepel, foreign, ggcharts, mdthemes,forcats,
                                readxl, raster, geodata, terra, sf, sp, lubridate))

# Ruta de trabajo ---------------------------------------------------------
root <- "/Users/cesara.saavedravanegas/Documents/GitHub/MindLabs/"
prj <- "TR_Free"
# Lectura de datos --------------------------------------------------------
db <- readxl::read_xlsx(paste0(root,prj,"/1.Data/Multivitaminico Pt.1.xlsx"))
db <- readxl::read_xlsx(paste0(root,prj,"/1.Data/Multivitaminico Pt.2.xlsx"))

head(db)

# -------------------------------------------------------------------------
Pr <- db |> dplyr::select(p1)
round(prop.table(table(Pr))*100,1)

Pr <- db |> dplyr::select(p2)
round(prop.table(table(Pr))*100,1)

Pr <- db |> dplyr::select(p3)
round(prop.table(table(Pr))*100,1)

Pr <- db |> dplyr::select(p4)
round(prop.table(table(Pr))*100,1)

Pr <- db |> dplyr::select(p5)
round(prop.table(table(Pr))*100,1)


Pr <- db |> dplyr::select(p7)
round(prop.table(table(Pr))*100,1)


Pr <- db |> dplyr::filter(p7 == "Sí") |>  dplyr::select(p8)
Pr <- db |> dplyr::filter(p1 == "Fui Consumidor de tarrito rojo pero ya no") |>  dplyr::select(p8)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable


Pr <- db |> 
  # dplyr::filter(p7 == "Sí") |>  
  dplyr::select(p20_1:p20_5)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

Pr <- db |> 
  # dplyr::filter(p7 == "Sí") |>  
  dplyr::select(p11)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

Pr <- db |> 
  # dplyr::filter(p7 == "Sí") |>  
  dplyr::select(p12_2)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

Pr <- db |> 
  # dplyr::filter(P13 == "Color Crema/Amarillo claro") |>  
  dplyr::select(P13)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

Pr <- db |> 
  # dplyr::filter(P13 == "Color Crema/Amarillo claro") |>  
  dplyr::select(P13)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable


Pr <- db |> 
  # dplyr::filter(P13 == "Color Crema/Amarillo claro") |>  
  dplyr::select(p35)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable


Pr <- db |> 
  # dplyr::filter(P13 == "Color Crema/Amarillo claro") |>  
  dplyr::select(p55)

Pr <- Pr |>  dplyr::mutate(p55=dplyr::case_when(p55 == 1 ~ "Nada importante",
                                          p55 == 2 ~ "2",
                                          p55 == 3 ~ "3",
                                          p55 == 4 ~ "4",
                                          p55 == 5 ~ "Muy importante"))
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable


Pr <- db |> 
  # dplyr::filter(P13 == "Color Crema/Amarillo claro") |>  
  dplyr::select(p60_1:p60_2)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable
fqTable$Categoria <- as.factor(fqTable$Categoria)
