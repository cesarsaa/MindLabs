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

db <- db %>% dplyr::filter(., Q9 == "Comfandi")
db <- db %>% dplyr::filter(., Q57 == "1")
# -------------------------------------------------------------------------
kp_df <- db %>%
  # satisfaccion
  mutate(across(c(Q59_1:Q59_8), ~ (. - 1) / 4)) %>%
  mutate(across(c(Q63_1), ~ (. - 1) / 4)) %>%
  mutate(across(c(Q64_1), ~ . / 10)) %>% 
  # intencion
  mutate(across(c(j), ~ . / 10)) %>% 
  # productividad c_1, d_1, e_1
  mutate(across(c(c_1), ~ (. - 1) / 3)) %>%
  mutate(across(c(d_1), ~ (. - 1) / 3)) %>%
  mutate(across(c(e_1), ~ (. - 1) / 3)) %>%
  # bienestar a_1, b_1, g, i
  mutate(across(c(a_1), ~ (. - 1) / 3)) %>%
  mutate(across(c(b_1), ~ (. - 1) / 3)) %>%
  mutate(across(c(g), ~ (. - 1) / 4)) %>%
  mutate(across(c(i), ~ . / 10)) %>% 
  # Impacto social f_1, h
  mutate(across(c(f_1), ~ (. - 1) / 3)) %>% 
  mutate(across(c(h), ~ . / 10)) %>% 
  dplyr::select(., a_1, b_1, g, i,
                c_1, d_1, e_1,f_1, h,j,
                Q59_1:Q59_8, Q63_1, Q64_1)

# -------------------------------------------------------------------------
kp_df <- kp_df %>%
  rowwise() %>%
  mutate(Indice_Satisfaccion = mean(c(Q59_1:Q59_8, Q63_1, Q64_1), na.rm = TRUE),
         Indice_Intencion = mean(c(j), na.rm = TRUE),
         Indice_Productividad = mean(c(c_1, d_1, e_1), na.rm = TRUE),
         Inidce_bienestar = mean(c(a_1, b_1, g, i), na.rm = TRUE),
         Inidce_social = mean(c(f_1, h), na.rm = TRUE)) %>% 
  dplyr::select(., Indice_Satisfaccion,
                Indice_Intencion,
                Indice_Productividad,
                Inidce_bienestar,
                Inidce_social)

# -------------------------------------------------------------------------
kp_df <- kp_df %>%
  mutate(Indicador_Final = (Indice_Satisfaccion * 0.3) + 
           (Indice_Intencion * 0.05) + 
           (Indice_Productividad * 0.2) + 
           (Inidce_bienestar * 0.25) +
           (Inidce_social * 0.20))

# -------------------------------------------------------------------------
kp_df <- kp_df %>% mutate(Indicador_Final_Escalado = Indicador_Final * 10) %>% 
  dplyr::select(., Indicador_Final_Escalado)

kp_df <- kp_df %>% dplyr::mutate(Desempeño = case_when(Indicador_Final_Escalado >= 0 & Indicador_Final_Escalado < 2 ~ "Muy Bajo",
                                                       Indicador_Final_Escalado >= 2 & Indicador_Final_Escalado < 4 ~ "Bajo",
                                                       Indicador_Final_Escalado >= 4 & Indicador_Final_Escalado < 6 ~ "Medio",
                                                       Indicador_Final_Escalado >= 6 & Indicador_Final_Escalado < 8 ~ "Alto",
                                                       Indicador_Final_Escalado >= 8 & Indicador_Final_Escalado < 10 ~ "Muy Alto"))

kp_df
# -------------------------------------------------------------------------
