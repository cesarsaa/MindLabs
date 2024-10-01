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
db <- db %>% dplyr::filter(., Q9 == "1") # Estas afiliado a alguna de estas cajas de compensación
db <- db %>% dplyr::filter(., Q57 == "2") # Has viajado con los servicios de turismo con Comfandi?

# I = ((r_1*w_1) + (r_2*w_2) + ... + (r_t*w_t)) / ((n* t))
# w <- Peso de la escala
# r <- Frecuencia de respuesta
# t <- Tamaño de la escala
# n <- total de observaciones

# Seleccion variables para KPI --------------------------------------------
km_db <- db %>% dplyr::select(.,a_1, b_1, c_1, d_1, e_1, f_1, g, h, i, j, 
                              Q59_1:Q59_8, Q63_1, Q64_1)

# -------------------------------------------------------------------------
# Seleccionar las columnas de interés
cols <- km_db %>% dplyr::select(a_1, b_1, c_1, d_1, e_1, f_1, g, h, i, j, 
                                Q59_1:Q59_8, Q63_1, Q64_1)
# Por ejemplo: si Q51 tiene escala 0-5, Q52 tiene 0-4, y así sucesivamente
escala_max <- c(4, 4, 4, 4, 4, 4,
                10, 10, 10, 10, 
                5, 5, 5, 5, 5, 5, 5, 5, 5, 10)  # Modifica según tus columnas

# Número de observaciones
n <- nrow(cols)

# Función para calcular el valor ponderado de cada columna según su escala y dividirlo entre n * max_valor
calcular_valor <- function(col, max_valor, n) {
  categorias <- 0:max_valor  # Generar el rango de la escala basado en max_valor
  suma_valores <- sum(sapply(categorias, function(x) sum(col == x) * x))
  valor_final <- suma_valores / (n * max_valor)
  return(valor_final)
}

# Aplicar la función a cada columna, utilizando el vector escala_max para cada columna
resultado <- (mapply(calcular_valor, 
                     col = cols,
                     max_valor = escala_max, 
                     MoreArgs = list(n = n)))

resultado <- data.frame(Columna = names(cols), Valor_Calculado = resultado)
resultado <- resultado %>% tidyr::pivot_wider(names_from = Columna, values_from = Valor_Calculado)

KPI <- resultado %>% dplyr::rowwise() %>% 
  dplyr::transmute(Indice_Satisfaccion = mean(c(Q59_1:Q59_8, Q63_1, Q64_1)),
                   Indice_Personal = mean(c(a_1, b_1, g, i)),
                   Indice_Social = mean(c(f_1,h)),
                   Indice_Productividad = mean(c(c_1,d_1,e_1)),
                   Indice_Repetir = mean(c(j)))


# -------------------------------------------------------------------------
# Indice_Satisfaccion   30%
# Indice_Personal       20%
# Indice_Social         20%
# Indice_Productividad  20%
# Indice_Repetir        10%

KPI <- KPI %>% mutate(Indicador_Final = (Indice_Satisfaccion * 0.3) + (Indice_Personal * 0.20) + (Indice_Social * 0.20) + (Indice_Productividad * 0.20) + (Indice_Repetir * 0.1),
                      Indicador_Final = Indicador_Final * 100)
KPI

# # -------------------------------------------------------------------------
# #
# cols <- km_db %>% dplyr::select(Q59_1:Q59_8)
# 
# calcular_valor <- function(col, max_valor) {
#   max_valor <- max(cols)
#   categorias <- 0:5  # Valores de la escala Likert
#   suma_valores <- sum(sapply(categorias, function(x) sum(col == x) * x))
#   valor_final <- suma_valores / (nrow(cols) * max_valor)
#   return(valor_final)
# }
# 
# # Aplicar la función a cada columna
# rslt <- sapply(cols, calcular_valor)
# # -------------------------------------------------------------------------
