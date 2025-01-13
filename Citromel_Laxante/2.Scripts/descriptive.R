# -------------------------------------------------------------------------
# Proyecto: Lema bronquisol
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
prj <- "Citromel_Laxante"
# Lectura de datos --------------------------------------------------------
db <- readxl::read_xlsx(paste0(root,prj,"/1.Data/db_citromel.xlsx"))
head(db)

# Demograficos ------------------------------------------------------------
# Pregunta 1
Pr <- db |> dplyr::select(p1)
round(prop.table(table(Pr))*100,1)
# Pregunta 2
Pr <- db |> dplyr::select(p2)
round(prop.table(table(Pr))*100,1)
# Pregunta 3
Pr <- db |> dplyr::select(p3)
round(prop.table(table(Pr))*100,1)
# Pregunta 4
Pr <- db |> dplyr::select(p4)
round(prop.table(table(Pr))*100,1)

# Medios publicitarios ----------------------------------------------------
# Pregunta 5
Pr <- db |> dplyr::select(p5_1:p5_5)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

# Pregunta 6
Pr <- db |> dplyr::select(p6)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

# -------------------------------------------------------------------------
db_flt <- db |> dplyr::filter(p5_3 == "Laxantes")
# -------------------------------------------------------------------------
# Pregunta 1
Pr <- db_flt |> dplyr::select(p1)
round(prop.table(table(Pr))*100,1)
# Pregunta 2
Pr <- db_flt |> dplyr::select(p2)
round(prop.table(table(Pr))*100,1)
# Pregunta 3
Pr <- db_flt |> dplyr::select(p3)
round(prop.table(table(Pr))*100,1)
# Pregunta 4
Pr <- db_flt |> dplyr::select(p4)
round(prop.table(table(Pr))*100,1)

# -------------------------------------------------------------------------
# Pregunta 6
Pr <- db_flt |> dplyr::select(p6)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

# Pregunta 7
Pr <- db_flt |> dplyr::select(p7)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 8
Pr <- db_flt |> dplyr::select(p8_1:p8_7)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable |> dplyr::arrange(desc(Porcentaje))

# Pregunta 9
Pr <- db_flt |> dplyr::select(p9_1:p9_9)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable


# Pregunta 11
Pr <- db_flt |> dplyr::select(p11)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 12
Pr <- db_flt |> dplyr::select(p12)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable
write.csv(fqTable, paste0(root,prj,"p12.csv"))


# Pregunta 13
Pr <- db_flt |> dplyr::select(p13_1:p13_8)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 14
Pr <- db_flt |> dplyr::select(p14)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

write.csv(fqTable, paste0(root,prj,"/p14.csv"))

# Pregunta 15
Pr <- db_flt |> dplyr::select(p15)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable
fqTable$Categoria <- as.factor(fqTable$Categoria)
sum(fqTable$Frecuencia)

# Pregunta 16
Pr <- db_flt |> dplyr::select(p16)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 17
Pr <- db_flt |> dplyr::select(p17)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 18
Pr <- db_flt |> dplyr::select(p18)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 19
Pr <- db_flt |> dplyr::select(p19)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 20
Pr <- db_flt |> dplyr::select(p20)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 22
Pr <- db_flt |> dplyr::select(p22_1:p22_5)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 23
Pr <- db_flt |> dplyr::select(p23)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 24
Pr <- db_flt |> dplyr::select(p24)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 25
Pr <- db_flt |> dplyr::select(p25_1:p25_8)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable


# Pregunta 27
Pr <- db_flt |> dplyr::select(p27_1:p27_6)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 28
Pr <- db_flt |> dplyr::select(p28)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable


# Pregunta 29
Pr <- db_flt |> dplyr::select(p29)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 33
Pr <- db_flt |> dplyr::select(p33_1:p33_6)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 35
Pr <- db_flt |> dplyr::select(p35)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 36
Pr <- db_flt |> dplyr::select(p36)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 37
Pr <- db_flt |> dplyr::select(p37)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable


# Pregunta 39
Pr <- db_flt |> dplyr::filter(p37 == "Sí") |> dplyr::select(p39)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable
fqTable$Categoria <- as.factor(fqTable$Categoria)

# Pregunta 40
Pr <- db_flt |> dplyr::filter(p37 == "Sí") |> dplyr::select(p40)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable
fqTable$Categoria <- as.factor(fqTable$Categoria)

# Pregunta 41
Pr <- db_flt |> dplyr::filter(p37 == "Sí") |> dplyr::select(p41)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable
fqTable$Categoria <- as.factor(fqTable$Categoria)


# Pregunta 43
Pr <- db_flt |> dplyr::filter(p37 == "Sí") |> dplyr::select(p43_1:p43_4)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable
fqTable$Categoria <- as.factor(fqTable$Categoria)

# Pregunta 44
Pr <- db_flt |> dplyr::filter(p13_5 == "Citromel") |> dplyr::select(p44)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable
fqTable$Categoria <- as.factor(fqTable$Categoria)

# Pregunta 45
Pr <- db_flt |> dplyr::select(p45_4)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable
fqTable$Categoria <- as.factor(fqTable$Categoria)

# Pregunta 47
Pr <- db_flt |> dplyr::select(p47)
Pr$p47 <- gsub("https:.*", "", Pr$p47)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable
fqTable$Categoria <- as.factor(fqTable$Categoria)


# Pregunta 48
Pr <- db_flt |> dplyr::select(p48)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable
fqTable$Categoria <- as.factor(fqTable$Categoria)

# Pregunta 49
Pr <- db_flt |> dplyr::select(p49)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = round((Frecuencia/nrow(Pr))*100,1)) |> 
  tidyr::drop_na(); fqTable
fqTable$Categoria <- as.factor(fqTable$Categoria)

