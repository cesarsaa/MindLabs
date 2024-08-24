# R options ---------------------------------------------------------------
g <- gc(reset = T); rm(list = ls())
options(warn = -1, scipen = 999)
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(dplyr, tidyr, ggplot2, foreign, ggcharts, mdthemes,forcats,
                                readxl, raster, geodata, terra, sf, sp, lubridate))

root <- "/Users/cesara.saavedravanegas/Documents/GitHub/MindLabs/"
prj <- "Cajas_compensacion"
# Lectura de datos --------------------------------------------------------
db <- foreign::read.spss(file = paste0(root,prj,"/1.Data/Estudio_de_cajas_de_compensacion_MindLabs_25 de julio de 2024_Base_Final_Turismo.sav"),
                         use.value.labels = F,
                         to.data.frame = T)
# -------------------------------------------------------------------------
# # Crear una función para aplicar case_when a cada columna
# transformacion_case_when <- function(x) {
#   dplyr::case_when(
#     x %in% c("5") ~ "Excelente",
#     x %in% c("4") ~ "Bueno",
#     x %in% c("3") ~ "Regular",
#     x %in% c("2") ~ "Malo",
#     x %in% c("1") ~ "Muy Malo"
#   )
# }
# 
# # Aplicar la función a las columnas Q59_1 a Q59_8 usando across
# db <- db %>%
#   mutate(across(starts_with("g"), transformacion_case_when))
# -------------------------------------------------------------------------
# variables of model
km_df <- db %>% dplyr::select(a_1, b_1, g, i, f_1, h,    # impacto en las relaciones sociales
                              c_1, d_1, e_1,             # impacto en la productividad
                              j,                         # intencion
                              Q59_1:Q59_8, Q63_1, Q64_1) # satifaccion
                      
## ------------------------------------------------------- #
# rows of the path matrix
Impacto_personal      = c(0,0,0,0)
Impacto_productividad = c(0,0,0,0)
Intencion_repeticion  = c(0,0,0,0)
Satisfaccion          = c(1,1,1,0)

# path matrix (inner model)
foot_path = rbind(Impacto_personal, Impacto_productividad, Intencion_repeticion, 
                  Satisfaccion)

# add column names
colnames(foot_path) = rownames(foot_path)

# blocks of indicators (outer model)
foot_blocks = list(1:6, 7:9, 10, 11:ncol(km_df))

# vector of modes (reflective)
foot_modes = c("A", "A", "A", "A")

# run plspm analysis
foot_pls = plspm(km_df, foot_path, foot_blocks, modes = foot_modes)

# path coefficients
foot_pls$path_coefs

# inner model
foot_pls$inner_model

# plotting results
# plotting results (inner model) - plot(foot_pls, what = "inner")
# png('Desktop/PLS/Results/PLS_PM/PLS_model.png', height = 12, width = 20, units = "in", res = 300)
plot(foot_pls, what = "inner",  box.prop = 0.5, box.cex = 1, 
     arr.width = 0.15, arr.pos = 0.5, cex.txt = 1.2, box.size = 0.085,
     lcol = "black", txt.col = "black", box.col = "gray96",
     colpos = "#4682B4", colneg = "tomato")

# Obtener scores y escalar los puntajes entre 0 y 100
Impacto_personal <- (latent_scores[,1])
Impacto_personal <- data.frame(scales::rescale(Impacto_personal, to = c(0, 100)))

Impacto_productividad <-(latent_scores[,2])
Impacto_productividad <- data.frame(scales::rescale(Impacto_productividad, to = c(0, 100)))

Intencion_repeticion <- (latent_scores[,3])
Intencion_repeticion <- data.frame(scales::rescale(Intencion_repeticion, to = c(0, 100)))

names(Impacto_personal) <- "Impacto_personal"
names(Impacto_productividad) <- "Impacto_productividad"
names(Intencion_repeticion) <- "Intencion_repeticion"

#
kpi <- ((0.1149) * Impacto_personal) + (0.1497 * Impacto_productividad) + ((0.259) * Intencion_repeticion)
# -------------------------------------------------------------------------

# a_1, b_1, g, i, f_1, h,    # impacto en las relaciones sociales
# c_1, d_1, e_1,             # impacto en la productividad
# j,                         # intencion
# Q59_1:Q59_8, Q63_1, Q64_1  # satifaccion

km_df <- km_df[1,]

kp_df <- km_df %>% dplyr::transmute(KPI_Satisfacción = rowMeans(dplyr::select(., Q59_1:Q59_8, Q63_1, Q64_1), na.rm = TRUE),
                                 KPI_Intención = rowMeans(dplyr::select(., j), na.rm = TRUE),
                                 KPI_Productividad = rowMeans(dplyr::select(., c_1, d_1, e_1), na.rm = TRUE),
                                 KPI_Relaciones = rowMeans(dplyr::select(., a_1, b_1, g, i, f_1, h), na.rm = TRUE))

kp_df <- kp_df %>% dplyr::mutate(KPI_General = rowMeans(dplyr::select(., KPI_Satisfacción, KPI_Intención, KPI_Productividad, KPI_Relaciones), na.rm = TRUE))

pesos <- c(0.3, 0.1, 0.2, 0.4) # Ajusta los pesos según sea necesario

22 * 0.3 + 8 * 0.1 + 3 * 0.2 + 21* 0.4
kp_df <- kp_df %>% dplyr::mutate(KPI_General = (pesos[1] * KPI_Satisfacción +
                                                pesos[2] * KPI_Intención +
                                                pesos[3] * KPI_Productividad +
                                                pesos[4] * KPI_Relaciones))

kp_df <- kp_df %>% dplyr::mutate(Categoría_KPI = case_when(KPI_General >= 4 ~ "Alto",
                                                           KPI_General >= 3 & KPI_General < 4 ~ "Medio",
                                                           TRUE ~ "Bajo"))

head(kp_df)
