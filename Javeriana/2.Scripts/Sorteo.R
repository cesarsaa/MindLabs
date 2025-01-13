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
ganadores_prev <- read.csv("/Users/cesara.saavedravanegas/Documents/ganadores_popsy.csv", sep = ",")
# -------------------------------------------------------------------------
# Definir el número de ganadores que deseas 117
# 100 bonos exito 6mil
# 17 bonos exito 300mil
# -------------------------------------------------------------------------
db <- db[!(db$Numero_documento %in% ganadores_prev$Numero_documento), ]
# -------------------------------------------------------------------------
n_ganadores <- 1  # Cambia este valor según lo necesites
db <- db |> distinct(Numero_documento, .keep_all = TRUE)

# Seleccionar aleatoriamente los valores
ganadores <- sample(db$Numero_documento, size = n_ganadores, replace = FALSE)

# Mostrar los ganadores
print(ganadores)

# Crear un DataFrame con los ganadores
ganadores_df <- data.frame(Numero_documento = ganadores)

df_ganadores <- merge(db, ganadores_df, by = "Numero_documento", all = F)
df_ganadores <- df_ganadores |> dplyr::select(Numero_documento, Nombre, Apellido, p8)

# Guardar los ganadores en un archivo CSV
write.csv(df_ganadores, "ganadores_bono_exito_100k.csv", row.names = FALSE)
