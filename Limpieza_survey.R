# -------------------------------------------------------------------------
# Cargar librerías necesarias
library(readxl)
library(dplyr)
g <- gc(reset = T); rm(list = ls())

# -------------------------------------------------------------------------
# Cargar el archivo Excel, especificando que la fila 2 tiene los nombres de las columnas
file_path <- "/Users/cesara.saavedravanegas/Downloads/Excel/Identidad.xlsx"
data <- read_excel(file_path, col_names = FALSE)

# Combinar la fila 1 y 2 para generar los nuevos nombres de las columnas
column_names <- paste(data[1, ], data[2, ], sep = "_")
# Asignar los nuevos nombres a las columnas
colnames(data) <- column_names
# Eliminar las dos primeras filas que ya no son necesarias
data <- data[-c(1, 2), ]
# Eliminar patrones indeseados de los nombres de las columnas
colnames(data) <- gsub("_Response", "", colnames(data), ignore.case = TRUE)
colnames(data) <- gsub("NA_", "", colnames(data), ignore.case = TRUE)
colnames(data) <- gsub("_", " ", colnames(data), ignore.case = TRUE)
colnames(data) <- gsub("Open-Ended Response", "", colnames(data), ignore.case = TRUE)
# Verifica los nuevos nombres de las columnas
head(colnames(data))
# Si deseas guardar el archivo con los nombres ajustados y patrones eliminados
openxlsx::write.xlsx(data, paste0(out,"data.xlsx"))
# Ver los primeros registros con los nuevos nombres
head(data)

# -------------------------------------------------------------------------
# Cargar el archivo Excel, especificando que la fila 2 tiene los nombres de las columnas
file_path <- "/Users/cesara.saavedravanegas/Downloads/Excel\ 2/Copia\ de\ Identidad.xlsx"
data2 <- read_excel(file_path, col_names = FALSE)

# Combinar la fila 1 y 2 para generar los nuevos nombres de las columnas
column_names <- paste(data2[1, ], data2[2, ], sep = "_")
# Asignar los nuevos nombres a las columnas
colnames(data2) <- column_names
# Eliminar las dos primeras filas que ya no son necesarias
data2 <- data2[-c(1, 2), ]
# Eliminar patrones indeseados de los nombres de las columnas
colnames(data2) <- gsub("_Response", "", colnames(data2), ignore.case = TRUE)
colnames(data2) <- gsub("NA_", "", colnames(data2), ignore.case = TRUE)
colnames(data2) <- gsub("_", " ", colnames(data2), ignore.case = TRUE)
colnames(data2) <- gsub("Open-Ended Response", "", colnames(data2), ignore.case = TRUE)
# Verifica los nuevos nombres de las columnas
head(colnames(data2))
#
out <- "/Users/cesara.saavedravanegas/Documents/GitHub/MindLabs/Identidad_visual/1.Data/"
# Si deseas guardar el archivo con los nombres ajustados y patrones eliminados
openxlsx::write.xlsx(data2, paste0(out,"data2.xlsx"))
# Ver los primeros registros con los nuevos nombres
head(data2)




