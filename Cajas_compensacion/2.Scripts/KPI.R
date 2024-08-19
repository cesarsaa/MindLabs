# -------------------------------------------------------------------------
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
head(db)
# -------------------------------------------------------------------------
db <- db %>% dplyr::filter(., Q9 == "1")
db <- db %>% dplyr::filter(., Q57 == "1")

# Seleccion variables para KPI --------------------------------------------
km_db <- db %>% dplyr::select(.,a_1, b_1, c_1, d_1, e_1, f_1, g, h, i, j, 
                              Q59_1:Q59_8, Q63_1, Q64_1)
# -------------------------------------------------------------------------
#
kp_db <- km_db %>% 
  #Satisfaccion Q59_1:Q59_8, Q63_1, Q64_1
  mutate(across(c(Q59_1:Q59_8), ~ (. - 1) / 4)) %>% 
  mutate(across(c(Q63_1), ~ (. - 1) / 4)) %>% 
  mutate(across(c(Q64_1), ~ (. ) / 9)) %>% 
  #Bienestar personal a_1, b_1 g, i
  mutate(across(c(a_1), ~ (. - 1) / 3)) %>% 
  mutate(across(c(b_1), ~ (. - 1) / 3)) %>% 
  mutate(across(c(g), ~ (. ) / 9)) %>% 
  mutate(across(c(i), ~ (. ) / 9)) %>% 
  #Relaciones sociales f_1, h
  mutate(across(c(f_1), ~ (. - 1) / 3)) %>% 
  mutate(across(c(h), ~ (. -1) / 9)) %>%
  #Impacto productividad c_1, d_1, e_1
  mutate(across(c(c_1), ~ (. - 1) / 3)) %>% 
  mutate(across(c(d_1), ~ (. - 1) / 3)) %>%
  mutate(across(c(e_1), ~ (. - 1) / 3)) %>%
  #Intencion de repetir j
  mutate(across(c(j), ~ (. -1) / 9))
# -------------------------------------------------------------------------
kp_db <- kp_db %>% rowwise() %>%
  mutate(Indice_Satisfaccion = mean(c(Q59_1:Q59_8, Q63_1, Q64_1), na.rm = TRUE),
         Indice_Personal = mean(c(a_1, b_1, g, i), na.rm = TRUE),
         Indice_Social = mean(c(f_1,h), na.rm = TRUE),
         Indice_Productividad = mean(c(c_1,d_1,e_1), na.rm = TRUE),
         Indice_Repetir = mean(c(j), na.rm = TRUE))
# -------------------------------------------------------------------------
kp_db <- kp_db %>% mutate(Indicador_Final = (Indice_Satisfaccion * 0.3) + (Indice_Personal * 0.20) + (Indice_Social * 0.20) + (Indice_Productividad * 0.20) + (Indice_Repetir * 0.1),
         Indicador = Indicador_Final * 10) %>% 
  dplyr::select(., Indice_Satisfaccion,Indice_Personal,Indice_Social,
                Indice_Productividad,Indice_Repetir,Indicador)
# -------------------------------------------------------------------------
kp_db <- kp_db %>% mutate(Cat_KPI = case_when(Indicador >= 0 & Indicador < 2 ~ "Muy Bajo",
                                              Indicador >= 2 & Indicador < 4 ~ "Bajo",
                                              Indicador >= 4 & Indicador < 6 ~ "Medio",
                                              Indicador >= 6 & Indicador < 8 ~ "Alto",
                                              Indicador >= 8 & Indicador < 10 ~ "Muy Alto"))
# -------------------------------------------------------------------------
kp_db %>% dplyr::count(., Cat_KPI)
