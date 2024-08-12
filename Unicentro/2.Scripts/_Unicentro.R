# -------------------------------------------------------------------------
# Proyecto: Imagen de marca centro comercial Unicentro
# By:  Cesar A. Saavedra
# Mindlabs
# Julio 2024

# R options ---------------------------------------------------------------
g <- gc(reset = T); rm(list = ls())
options(warn = -1, scipen = 999)
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(dplyr, tidyr, ggplot2, ggrepel, foreign, ggcharts, mdthemes,forcats,
                                readxl, raster, geodata, terra, sf, sp, lubridate))

root <- "/Users/cesara.saavedravanegas/Documents/GitHub/MindLabs/"
prj <- "Unicentro"
# Lectura de datos --------------------------------------------------------
db <- foreign::read.spss(file = paste0(root,prj,"/1.Data/Base_Final_Centro_Comercial.sav"),
                         use.value.labels = T,
                         to.data.frame = T)
head(db)

# analisis exploratorio ---------------------------------------------------
# Re clasificacion de variables -------------------------------------------
# 8 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P8_1 = dplyr::case_when(P8_1	%in% c("Sí")	~	"Unicentro",
                                                   TRUE ~ NA))

db <- db %>% dplyr::mutate(P8_2 = dplyr::case_when(P8_2	%in% c("Sí")	~	"Chipichape",
                                                   TRUE ~ NA))

db <- db %>% dplyr::mutate(P8_3 = dplyr::case_when(P8_3	%in% c("Sí")	~	"Cosmocentro",
                                                   TRUE ~ NA))

db <- db %>% dplyr::mutate(P8_4 = dplyr::case_when(P8_4	%in% c("Sí")	~	"Jardín Plaza",
                                                   TRUE ~ NA))

db <- db %>% dplyr::mutate(P8_5 = dplyr::case_when(P8_5	%in% c("Sí")	~	"Palmetto Plaza",
                                                   TRUE ~ NA))

db <- db %>% dplyr::mutate(P8_6 = dplyr::case_when(P8_6	%in% c("Sí")	~	"Pacific Mall",
                                                   TRUE ~ NA))

db <- db %>% dplyr::mutate(P8_7 = dplyr::case_when(P8_7	%in% c("Sí")	~	"Centenario",
                                                   TRUE ~ NA))

db <- db %>% dplyr::mutate(P8_8 = dplyr::case_when(P8_8	%in% c("Sí")	~	"La estación",
                                                   TRUE ~ NA))

db <- db %>% dplyr::mutate(P8_9 = dplyr::case_when(P8_9	%in% c("Sí")	~	"Mall Plaza",
                                                   TRUE ~ NA))
# 10 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P10_1 = dplyr::case_when(P10_1	%in% c("Sí")	~	"Unicentro",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P10_2 = dplyr::case_when(P10_2	%in% c("Sí")	~	"Chipichape",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P10_3 = dplyr::case_when(P10_3	%in% c("Sí")	~	"Cosmocentro",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P10_4 = dplyr::case_when(P10_4	%in% c("Sí")	~	"Jardín Plaza",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P10_5 = dplyr::case_when(P10_5	%in% c("Sí")	~	"Palmetto Plaza",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P10_6 = dplyr::case_when(P10_6	%in% c("Sí")	~	"Pacific Mall",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P10_7 = dplyr::case_when(P10_7	%in% c("Sí")	~	"Centenario",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P10_8 = dplyr::case_when(P10_8	%in% c("Sí")	~	"La estación",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P10_9 = dplyr::case_when(P10_9	%in% c("Sí")	~	"Mall Plaza",
                                                    TRUE ~ NA))
# 11 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P11_1 = dplyr::case_when(P11_1	%in% c("Sí")	~	"Unicentro",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P11_2 = dplyr::case_when(P11_2	%in% c("Sí")	~	"Chipichape",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P11_3 = dplyr::case_when(P11_3	%in% c("Sí")	~	"Cosmocentro",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P11_4 = dplyr::case_when(P11_4	%in% c("Sí")	~	"Jardín Plaza",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P11_5 = dplyr::case_when(P11_5	%in% c("Sí")	~	"Palmetto Plaza",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P11_6 = dplyr::case_when(P11_6	%in% c("Sí")	~	"Pacific Mall",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P11_7 = dplyr::case_when(P11_7	%in% c("Sí")	~	"Centenario",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P11_8 = dplyr::case_when(P11_8	%in% c("Sí")	~	"La estación",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P11_9 = dplyr::case_when(P11_9	%in% c("Sí")	~	"Mall Plaza",
                                                    TRUE ~ NA))
# 12 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P12_1 = dplyr::case_when(P12_1	%in% c("Sí")	~	"Unicentro",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P12_2 = dplyr::case_when(P12_2	%in% c("Sí")	~	"Chipichape",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P12_3 = dplyr::case_when(P12_3	%in% c("Sí")	~	"Cosmocentro",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P12_4 = dplyr::case_when(P12_4	%in% c("Sí")	~	"Jardín Plaza",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P12_5 = dplyr::case_when(P12_5	%in% c("Sí")	~	"Palmetto Plaza",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P12_6 = dplyr::case_when(P12_6	%in% c("Sí")	~	"Pacific Mall",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P12_7 = dplyr::case_when(P12_7	%in% c("Sí")	~	"Centenario",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P12_8 = dplyr::case_when(P12_8	%in% c("Sí")	~	"La estación",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P12_9 = dplyr::case_when(P12_9	%in% c("Sí")	~	"Mall Plaza",
                                                    TRUE ~ NA))
# 14 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P14_1 = dplyr::case_when(P14_1	%in% c("Sí")	~	"Unicentro",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P14_2 = dplyr::case_when(P14_2	%in% c("Sí")	~	"Chipichape",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P14_3 = dplyr::case_when(P14_3	%in% c("Sí")	~	"Cosmocentro",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P14_4 = dplyr::case_when(P14_4	%in% c("Sí")	~	"Jardín Plaza",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P14_5 = dplyr::case_when(P14_5	%in% c("Sí")	~	"Palmetto Plaza",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P14_6 = dplyr::case_when(P14_6	%in% c("Sí")	~	"Pacific Mall",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P14_7 = dplyr::case_when(P14_7	%in% c("Sí")	~	"Centenario",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P14_8 = dplyr::case_when(P14_8	%in% c("Sí")	~	"La estación",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P14_9 = dplyr::case_when(P14_9	%in% c("Sí")	~	"Mall Plaza",
                                                    TRUE ~ NA))

# 15 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P15_1 = dplyr::case_when(P15_1	%in% c("Sí")	~	"Unicentro",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P15_2 = dplyr::case_when(P15_2	%in% c("Sí")	~	"Chipichape",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P15_3 = dplyr::case_when(P15_3	%in% c("Sí")	~	"Cosmocentro",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P15_4 = dplyr::case_when(P15_4	%in% c("Sí")	~	"Jardín Plaza",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P15_5 = dplyr::case_when(P15_5	%in% c("Sí")	~	"Palmetto Plaza",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P15_6 = dplyr::case_when(P15_6	%in% c("Sí")	~	"Pacific Mall",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P15_7 = dplyr::case_when(P15_7	%in% c("Sí")	~	"Centenario",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P15_8 = dplyr::case_when(P15_8	%in% c("Sí")	~	"La estación",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P15_9 = dplyr::case_when(P15_9	%in% c("Sí")	~	"Mall Plaza",
                                                    TRUE ~ NA))


# 16 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P16_1 = dplyr::case_when(P16_1	%in% c("Sí")	~	"Ubicación",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P16_2 = dplyr::case_when(P16_2	%in% c("Sí")	~	"Variedad de tiendas para mi",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P16_3 = dplyr::case_when(P16_3	%in% c("Sí")	~	"Que realicen ferias y/o eventos de mi interés",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P16_4 = dplyr::case_when(P16_4	%in% c("Sí")	~	"Precios",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P16_5 = dplyr::case_when(P16_5	%in% c("Sí")	~	"Seguridad",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P16_6 = dplyr::case_when(P16_6	%in% c("Sí")	~	"Ambiente en general",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P16_7 = dplyr::case_when(P16_7	%in% c("Sí")	~	"Opciones de servicios",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P16_8 = dplyr::case_when(P16_8	%in% c("Sí")	~	"Opciones de entretenimiento",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P16_9 = dplyr::case_when(P16_9	%in% c("Sí")	~	"Opciones de comida",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P16_10 = dplyr::case_when(P16_10	%in% c("Sí")	~	"Facilidad y amplitud del Estacionamiento",
                                                     TRUE ~ NA))

db <- db %>% dplyr::mutate(P16_11 = dplyr::case_when(P16_11 %in% c("Sí")	~	"Que sea de fácil acceso",
                                                     TRUE ~ NA))

db <- db %>% dplyr::mutate(P16_12 = dplyr::case_when(P16_12	%in% c("Sí")	~	"Que no cobren parqueadero",
                                                     TRUE ~ NA))

db <- db %>% dplyr::mutate(P16_13 = dplyr::case_when(P16_13	%in% c("Sí")	~	"Opciones de tienda para toda la familia",
                                                     TRUE ~ NA))

# 22 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P22_1 = dplyr::case_when(P22_1	%in% c("Sí")	~	"Compras de hombre y/o mujer",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P22_2 = dplyr::case_when(P22_2	%in% c("Sí")	~	"Compra de accesorios",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P22_3 = dplyr::case_when(P22_3	%in% c("Sí")	~	"Compras de mercado",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P22_4 = dplyr::case_when(P22_4	%in% c("Sí")	~	"Ir a un bar",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P22_5 = dplyr::case_when(P22_5	%in% c("Sí")	~	"Compra de zapatos",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P22_6 = dplyr::case_when(P22_6	%in% c("Sí")	~	"Ir a comer a restaurantes formales",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P22_7 = dplyr::case_when(P22_7	%in% c("Sí")	~	"Compra de implementos y ropa deportiva",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P22_8 = dplyr::case_when(P22_8	%in% c("Sí")	~	"Compra de elementos para bebes y niños",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P22_9 = dplyr::case_when(P22_9	%in% c("Sí")	~	"Ir a comida rapida",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P22_10 = dplyr::case_when(P22_10	%in% c("Sí")	~	"Actividades de entretenimiento",
                                                     TRUE ~ NA))

db <- db %>% dplyr::mutate(P22_11 = dplyr::case_when(P22_11 %in% c("Sí")	~	"Uso de servicios",
                                                     TRUE ~ NA))

db <- db %>% dplyr::mutate(P22_12 = dplyr::case_when(P22_12	%in% c("Sí")	~	"Compra de elementos de salud y bienestar",
                                                     TRUE ~ NA))

db <- db %>% dplyr::mutate(P22_13 = dplyr::case_when(P22_13	%in% c("Sí")	~	"Compra de elementos de cuero",
                                                     TRUE ~ NA))

# 32 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P32_1 = dplyr::case_when(P32_1	%in% c("Sí")	~	"Limpieza y mantenimiento",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P32_2 = dplyr::case_when(P32_2	%in% c("Sí")	~	"Seguridad",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P32_3 = dplyr::case_when(P32_3	%in% c("Sí")	~	"Variedad de tiendas",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P32_4 = dplyr::case_when(P32_4	%in% c("Sí")	~	"Precios",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P32_5 = dplyr::case_when(P32_5	%in% c("Sí")	~	"Servicio al cliente",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P32_6 = dplyr::case_when(P32_6	%in% c("Sí")	~	"Opciones de comida",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P32_7 = dplyr::case_when(P32_7	%in% c("Sí")	~	"Estacionamiento",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P32_8 = dplyr::case_when(P32_8	%in% c("Sí")	~	"Ambiente general",
                                                    TRUE ~ NA))
# 35 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P35_1 = dplyr::case_when(P35_1	%in% c("Sí")	~	"Tienda de ropa y accesorios",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P35_2 = dplyr::case_when(P35_2	%in% c("Sí")	~	"Supermecados",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P35_3 = dplyr::case_when(P35_3	%in% c("Sí")	~	"Restaurantes y cafeterias",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P35_4 = dplyr::case_when(P35_4	%in% c("Sí")	~	"Cines",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P35_5 = dplyr::case_when(P35_5	%in% c("Sí")	~	"Bancos y cajeros automaticos",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P35_6 = dplyr::case_when(P35_6	%in% c("Sí")	~	"Gimnasio",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P35_7 = dplyr::case_when(P35_7	%in% c("Sí")	~	"Áreas de jugos para niños",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P35_8 = dplyr::case_when(P35_8	%in% c("Sí")	~	"Servicios de mensajeria",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P35_9 = dplyr::case_when(P35_9	%in% c("Sí")	~	"Serviteca",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P35_10 = dplyr::case_when(P35_10	%in% c("Sí")	~	"Karts",
                                                     TRUE ~ NA))

db <- db %>% dplyr::mutate(P35_11 = dplyr::case_when(P35_11 %in% c("Sí")	~	"Bolos",
                                                     TRUE ~ NA))


# 36 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P36_1 = dplyr::case_when(P36_1	%in% c("Sí")	~	"Tienda de ropa y accesorios",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P36_2 = dplyr::case_when(P36_2	%in% c("Sí")	~	"Supermecados",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P36_3 = dplyr::case_when(P36_3	%in% c("Sí")	~	"Restaurantes y cafeterias",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P36_4 = dplyr::case_when(P36_4	%in% c("Sí")	~	"Cines",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P36_5 = dplyr::case_when(P36_5	%in% c("Sí")	~	"Bancos y cajeros automaticos",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P36_6 = dplyr::case_when(P36_6	%in% c("Sí")	~	"Gimnasio",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P36_7 = dplyr::case_when(P36_7	%in% c("Sí")	~	"Áreas de jugos para niños",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P36_8 = dplyr::case_when(P36_8	%in% c("Sí")	~	"Servicios de mensajeria",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P36_9 = dplyr::case_when(P36_9	%in% c("Sí")	~	"Serviteca",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P36_10 = dplyr::case_when(P36_10	%in% c("Sí")	~	"Karts",
                                                     TRUE ~ NA))

db <- db %>% dplyr::mutate(P36_11 = dplyr::case_when(P36_11 %in% c("Sí")	~	"Bolos",
                                                     TRUE ~ NA))


# 38 ----------------------------------------------------------------------
db <- db %>% dplyr::mutate(P38_1 = dplyr::case_when(P38_1	%in% c("Sí")	~	"Cines",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P38_2 = dplyr::case_when(P38_2	%in% c("Sí")	~	"Area de juegos para niños",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P38_3 = dplyr::case_when(P38_3	%in% c("Sí")	~	"Eventos y conciertos",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P38_4 = dplyr::case_when(P38_4	%in% c("Sí")	~	"Salas de videojuegos",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P38_5 = dplyr::case_when(P38_5	%in% c("Sí")	~	"Áreas de descanso y recreación",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P38_6 = dplyr::case_when(P38_6	%in% c("Sí")	~	"Karts",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P38_7 = dplyr::case_when(P38_7	%in% c("Sí")	~	"Arenero",
                                                    TRUE ~ NA))

# 47 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P47_1 = dplyr::case_when(P47_1	%in% c("Sí")	~	"Variedad de tiendas",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P47_2 = dplyr::case_when(P47_2	%in% c("Sí")	~	"Oferta de entretenimiento",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P47_3 = dplyr::case_when(P47_3	%in% c("Sí")	~	"Calidad de la atencion al cliente",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P47_4 = dplyr::case_when(P47_4	%in% c("Sí")	~	"Ambiente",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P47_5 = dplyr::case_when(P47_5	%in% c("Sí")	~	"Oferta de servicios",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P47_6 = dplyr::case_when(P47_6	%in% c("Sí")	~	"Decoración",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P47_7 = dplyr::case_when(P47_7	%in% c("Sí")	~	"Ubicación",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P47_8 = dplyr::case_when(P47_8	%in% c("Sí")	~	"Accesibilidad",
                                                    TRUE ~ NA))
# 49 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P49_1 = dplyr::case_when(P49_1	%in% c("Sí")	~	"Variedad de tiendas",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P49_2 = dplyr::case_when(P49_2	%in% c("Sí")	~	"Seguridad",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P49_3 = dplyr::case_when(P49_3	%in% c("Sí")	~	"Limpieza",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P49_4 = dplyr::case_when(P49_4	%in% c("Sí")	~	"Oferta de entretenimiento",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P49_5 = dplyr::case_when(P49_5	%in% c("Sí")	~	"Atencion al cliente",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P49_6 = dplyr::case_when(P49_6	%in% c("Sí")	~	"Oferta de servicios",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P49_7 = dplyr::case_when(P49_7	%in% c("Sí")	~	"Cobro de parqueadero",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P49_8 = dplyr::case_when(P49_8	%in% c("Sí")	~	"Zonas verdes",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P49_9 = dplyr::case_when(P49_9	%in% c("Sí")	~	"Áreas de descanso",
                                                    TRUE ~ NA))


# 53 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P53_1 = dplyr::case_when(P53_1	%in% c("Sí")	~	"Facebook",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P53_2 = dplyr::case_when(P53_2	%in% c("Sí")	~	"Instagram",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P53_3 = dplyr::case_when(P53_3	%in% c("Sí")	~	"WhatsApp",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P53_4 = dplyr::case_when(P53_4	%in% c("Sí")	~	"Twitter",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P53_5 = dplyr::case_when(P53_5	%in% c("Sí")	~	"LinkedIn",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P53_6 = dplyr::case_when(P53_6	%in% c("Sí")	~	"TikTok",
                                                    TRUE ~ NA))

# 54 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P54_1 = dplyr::case_when(P54_1	%in% c("Sí")	~	"Noticias",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P54_2 = dplyr::case_when(P54_2	%in% c("Sí")	~	"Entretenimiento",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P54_3 = dplyr::case_when(P54_3	%in% c("Sí")	~	"Informacion general",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P54_4 = dplyr::case_when(P54_4	%in% c("Sí")	~	"Compras",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P53_5 = dplyr::case_when(P53_5	%in% c("Sí")	~	"Educación",
                                                    TRUE ~ NA))

# 57 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P57_1 = dplyr::case_when(P57_1	%in% c("Sí")	~	"Televisión",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P57_2 = dplyr::case_when(P57_2	%in% c("Sí")	~	"Radio",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P57_3 = dplyr::case_when(P57_3	%in% c("Sí")	~	"Redes sociales",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P57_4 = dplyr::case_when(P57_4	%in% c("Sí")	~	"En linea",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P57_5 = dplyr::case_when(P57_5	%in% c("Sí")	~	"Impresos",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P57_6 = dplyr::case_when(P57_6	%in% c("Sí")	~	"Publicidad exterior ",
                                                    TRUE ~ NA))

# 58 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P58_1 = dplyr::case_when(P58_1	%in% c("Sí")	~	"Ofertas y descuentos especiales",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P58_2 = dplyr::case_when(P58_2	%in% c("Sí")	~	"Nuevas tiendas o aperturas",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P58_3 = dplyr::case_when(P58_3	%in% c("Sí")	~	"Información de eventos",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P58_4 = dplyr::case_when(P58_4	%in% c("Sí")	~	"Invitación a alguna actividad promocional por fecha especial",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P58_5 = dplyr::case_when(P58_5	%in% c("Sí")	~	"Comodidades y servicios del centro comercial",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P58_6 = dplyr::case_when(P58_6	%in% c("Sí")	~	"Publicidad relacionada a un ambiente familiar y acogedor",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P58_7 = dplyr::case_when(P58_7	%in% c("Sí")	~	"Invitación a visitar el centro comercial",
                                                    TRUE ~ NA))

# 61 -------------------------------------------------------------------------
db <- db %>% dplyr::mutate(P61_1 = dplyr::case_when(P61_1	%in% c("Sí")	~	"Instagram",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P61_2 = dplyr::case_when(P61_2	%in% c("Sí")	~	"Facebook",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P61_3 = dplyr::case_when(P61_3	%in% c("Sí")	~	"TikTok",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P61_4 = dplyr::case_when(P61_4	%in% c("Sí")	~	"Whatsapp",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P61_5 = dplyr::case_when(P61_5	%in% c("Sí")	~	"Televisión",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P61_6 = dplyr::case_when(P61_6	%in% c("Sí")	~	"Radio",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P61_7 = dplyr::case_when(P61_7	%in% c("Sí")	~	"Prensa",
                                                    TRUE ~ NA))

db <- db %>% dplyr::mutate(P61_8 = dplyr::case_when(P61_8	%in% c("Sí")	~	"Publicidad exterior",
                                                    TRUE ~ NA))
# Analisis descriptivo ----------------------------------------------------
db <- db %>% dplyr::filter(., SEL != 1)
# Pregunta  -------------------------------------------------------------
Pr0 <- db %>% dplyr::select(SEL)
Pr0 %>% glimpse
# Analisis descriptivo
fqTable <- Pr0 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr0)) %>% 
  tidyr::drop_na()
#
df2 <- fqTable %>% 
  mutate(csum = rev(cumsum(rev(Porcentaje))), 
         pos = round(Porcentaje/2,2) + lead(csum, 1),
         pos = if_else(is.na(pos), round(Porcentaje/2,2), pos))

gg <- ggplot(fqTable, aes(x = "" , y = Porcentaje, fill = fct_inorder(Categoria))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(Porcentaje*100,2), "%")),
                   size = 5.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title =" ")) +
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_NSE.png"), gg, width=9, height=7, units = "in", dpi=366)
rm(df2)
# Pregunta 2 --------------------------------------------------------------
Pr2 <- db %>% dplyr::select(Gender)
Pr2 %>% glimpse

# Analisis descriptivo
fqTable <- Pr2 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr2)) %>% 
  tidyr::drop_na()
#
gg <- ggplot(fqTable, aes(x = "", y = Porcentaje, fill = Categoria)) +
  geom_col(color = "black") +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%")), color = c("white", "white"),
             position = position_stack(vjust = 0.5),
             show.legend = F) +
  guides(fill = guide_legend(title = " ")) +
  scale_fill_manual(values = c("steelblue", "#f41c5c")) +
  coord_polar(theta = "y") + 
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_gender.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 3 --------------------------------------------------------------
Pr3 <- db %>% dplyr::select(Range)
Pr3 %>% glimpse

# Analisis descriptivo
fqTable <- Pr3 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr3))*100) %>% 
  tidyr::drop_na()

#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = 1.2), color = "white") +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.1)) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_edad.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 6 --------------------------------------------------------------
Pr6 <- db %>% dplyr::select(P6)
Pr6 %>% glimpse
# Analisis descriptivo
fqTable <- Pr6 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr6)) %>% 
  tidyr::drop_na()

df2 <- fqTable %>% 
  mutate(csum = rev(cumsum(rev(Porcentaje))), 
         pos = round(Porcentaje/2,2) + lead(csum, 1),
         pos = if_else(is.na(pos), round(Porcentaje/2,2), pos))

gg <- ggplot(fqTable, aes(x = "" , y = Porcentaje, fill = fct_inorder(Categoria))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set1") +
  ggrepel::geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(Porcentaje*100,2), "%")),
                   size = 5.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title =" ")) +
  theme_void()

gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, highlight = c("Si"), bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("#ee1b24")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2), color = "black") +
  # scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_6A.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 8 --------------------------------------------------------------
Pr8 <- db %>% dplyr::select(P8_1:P8_9)
Pr8 %>% glimpse
# Analisis descriptivo
fqTable <- Pr8 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr8)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("steelblue")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_8.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 9 --------------------------------------------------------------
Pr9 <- db %>% dplyr::select(P9)
Pr9 %>% glimpse
# Analisis descriptivo
fqTable <- Pr9 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr9)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>% dplyr::filter(., Categoria != "{P8_other.NAOK}") %>% 
  bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("#ee1b24")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  # scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_9.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 10 --------------------------------------------------------------
Pr10 <- db %>% dplyr::select(P10_1:P10_9)
Pr10 %>% glimpse
# Analisis descriptivo
fqTable <- Pr10 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr10)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("#ee1b24")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  # scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_10.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 11 --------------------------------------------------------------
Pr11 <- db %>% dplyr::select(P11_1:P11_9)
Pr11 %>% glimpse
# Analisis descriptivo
fqTable <- Pr11 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr11)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("#ee1b24")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  # scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_11.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 12 --------------------------------------------------------------
Pr12 <- db %>% dplyr::select(P12_1:P12_9)
Pr12 %>% glimpse
# Analisis descriptivo
fqTable <- Pr12 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr12)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("#ee1b24")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  # scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_12.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 14 --------------------------------------------------------------
Pr14 <- db %>% dplyr::select(P14_1:P14_9)
Pr14 %>% glimpse
# Analisis descriptivo
fqTable <- Pr14 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr14)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("#ee1b24")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  # scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_14.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 15 --------------------------------------------------------------
Pr15 <- db %>% dplyr::select(P15_1:P15_9)
Pr15 %>% glimpse
# Analisis descriptivo
fqTable <- Pr15 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr15)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("#ee1b24")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  # scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_15.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 16 --------------------------------------------------------------
Pr16 <- db %>% dplyr::select(P16_1:P16_13)
Pr16 %>% glimpse
# Analisis descriptivo
fqTable <- Pr16 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr16)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("#ee1b24")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left")) +
  # geom_text(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left"), color = "black") +
  # scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_16.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 17 -------------------------------------------------------------
Pr17 <- db %>% dplyr::select(P17_1:P17_3)
Pr17 %>% glimpse
# Analisis descriptivo
fqTable <- Pr17 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr17)) %>% 
  dplyr::group_by(Variable) %>% 
  tidyr::drop_na()
#
write.csv(fqTable, paste0(root,prj,"/3.Results/Pregunta_17.csv"))
# Pregunta 18-19 ----------------------------------------------------------
Pr <- db %>% dplyr::select(P18,P19)
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr)) 
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Muy importante",
                                                          "Importante",
                                                          "Algo importante",
                                                          "Poco importante",
                                                          "Nada importante"), ordered = T)
#
gg <- fqTable %>% ggplot(aes(x = reorder(Categoria, +Porcentaje), y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  # geom_text(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left"), color = "black") +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left")) +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="RdYlGn", direction = -1)+
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable, ) +
  theme_ggcharts() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="none",
        # panel.background = element_rect(fill = "white",
        #                                 colour = "white",
        #                                 size = 0.5,
        #                                 linetype = "solid"),
        strip.background = element_blank(),
        strip.text.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_18-19.png"), gg, width=9, height=7, units = "in", dpi=366)


# Pregunta 21 -------------------------------------------------------------
Pr21 <- db %>% dplyr::select(P21)
Pr21 %>% glimpse
# Analisis descriptivo
fqTable <- Pr21 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr21)) %>% 
  tidyr::drop_na()
#
#
gg <- ggplot(fqTable, aes(x = "", y = Porcentaje, fill = Categoria)) +
  geom_col(color = "black") +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%")), color = c("white", "white"),
             position = position_stack(vjust = 0.5),
             show.legend = F) +
  guides(fill = guide_legend(title = " ")) +
  scale_fill_manual(values = c("steelblue", "#f41c5c")) +
  coord_polar(theta = "y") + 
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_21.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 22 --------------------------------------------------------------
Pr22 <- db %>% dplyr::select(P22_1:P22_13)
Pr22 %>% glimpse
# Analisis descriptivo
fqTable <- Pr22 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr22)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("#ee1b24")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left"), color = "black") +
  # scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_22.png"), gg, width=15, height=7, units = "in", dpi=366)
# Pregunta 23 --------------------------------------------------------------
Pr23 <- db %>% dplyr::select(P23V1)
Pr23 %>% glimpse
# Analisis descriptivo
fqTable <- Pr23 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr23)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  # bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("steelblue")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left")) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  # scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_23.png"), gg, width=15, height=7, units = "in", dpi=366)
# Pregunta 24 --------------------------------------------------------------
Pr24 <- db %>% dplyr::select(P24)
Pr24 %>% glimpse
# Analisis descriptivo
fqTable <- Pr24 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr24)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Una vez a la semana","Más de una vez a la semana","Cada dos meses"), bar_color = c("steelblue","orange")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  # scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_24.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 25 --------------------------------------------------------------
Pr25 <- db %>% dplyr::select(P25)
Pr25 %>% glimpse
# Analisis descriptivo
fqTable <- Pr25 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr25)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  # bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  bar_chart(Categoria, Porcentaje, highlight = c("Una vez al año","Una vez al mes","Cada dos meses"), bar_color = c("steelblue","orange")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  # scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_25.png"), gg, width=15, height=7, units = "in", dpi=366)
# Pregunta 26 --------------------------------------------------------------
Pr26 <- db %>% dplyr::select(P26)
Pr26 %>% glimpse
# Analisis descriptivo
fqTable <- Pr26 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr26)) %>% 
  tidyr::drop_na()
#
df2 <- fqTable %>% 
  mutate(csum = rev(cumsum(rev((Porcentaje)))), 
         pos = round(Porcentaje/2,2) + lead(csum, 1),
         pos = if_else(is.na(pos), round(Porcentaje/2,2), pos))

gg <- ggplot(fqTable, aes(x = "" , y = Porcentaje, fill = fct_inorder(Categoria))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(Porcentaje*100,2), "%")),
                   size = 5.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title =" ")) +
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_26.png"), gg, width=9, height=7, units = "in", dpi=366)
rm(df2)
# Pregunta 27 --------------------------------------------------------------
Pr27 <- db %>% dplyr::select(P27)
Pr27 %>% glimpse
# Analisis descriptivo
fqTable <- Pr27 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr27)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("#ee1b24")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  # scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_27.png"), gg, width=13, height=7, units = "in", dpi=366)

# Pregunta 28 --------------------------------------------------------------
Pr28 <- db %>% dplyr::select(P28)
Pr28 %>% glimpse
# Analisis descriptivo
fqTable <- Pr28 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr28)) 
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Muy satisfecho",
                                                          "Satisfecho",
                                                          "Ni satisfecho ni insatisfecho",
                                                          "Insatisfecho",
                                                          "Muy insatisfecho"), ordered = T)
#
gg <- fqTable %>% ggplot(aes(x = reorder(Categoria, +Porcentaje), y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = -0.1)) +
  # geom_text(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left"), color = "black") +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="RdYlGn", direction = -1)+
  scale_y_continuous(limits = c(0, 100)) +
  # facet_wrap(~ Variable, ) +
  # theme_ggcharts() +
  theme_void() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="none",
        # panel.background = element_rect(fill = "white",
        #                                 colour = "white",
        #                                 size = 0.5,
        #                                 linetype = "solid"),
        strip.background = element_blank(),
        strip.text.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_28A.png"), gg, width=13, height=7, units = "in", dpi=366)

# Pregunta 29 --------------------------------------------------------------
Pr29 <- db %>% dplyr::select(P29)
Pr29 %>% glimpse
# Analisis descriptivo
fqTable <- Pr29 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr29)) 
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("1 - Definitivamente no lo recomendaría",
                                                          "2","3","4","5",
                                                          "6","7","8","9",
                                                          "10 - Totalmente lo recomendaría"), ordered = T)
# 0 a 6 detractores
dtr <- ((0.017766497) + (0.005076142) + (0.015228426) + (0.010152284) + (0.058375635) + (0.030456853))*100
# 7 a 8 pasivos
# 9 a 10 promotores
pro <- ((0.195431472) + (0.408629442))*100
NPS <- (pro - dtr)

#
lvl <- c("1 - Definitivamente no lo recomendaría",
         "2","3","4","5",
         "6","7","8","9",
         "10 - Totalmente lo recomendaría")
# 
gg <- fqTable %>% ggplot(aes(x = factor(Categoria, level=lvl), y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = -0.1)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="RdYlGn")+
  scale_y_continuous(limits = c(0, 50)) +
  # facet_wrap(~ Variable, ) +
  # theme_ggcharts() +
  theme_void() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_29A.jpeg"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 30 --------------------------------------------------------------
Pr30 <- db %>% dplyr::select(P30)
Pr30 %>% glimpse
# Analisis descriptivo
fqTable <- Pr30 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr30)) %>% 
  tidyr::drop_na()
#
gg <- ggplot(fqTable, aes(x = "", y = Porcentaje, fill = Categoria)) +
  geom_col(color = "black") +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%")), color = c("white", "white"),
             position = position_stack(vjust = 0.5),
             show.legend = F) +
  guides(fill = guide_legend(title = " ")) +
  scale_fill_manual(values = c("steelblue", "#f41c5c")) +
  coord_polar(theta = "y") + 
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_30.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 31 --------------------------------------------------------------
Pr31 <- db %>% dplyr::select(P31)
Pr31 %>% glimpse
# Analisis descriptivo
fqTable <- Pr31 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr31)) %>% 
  tidyr::drop_na()
#
gg <- ggplot(fqTable, aes(x = "", y = Porcentaje, fill = Categoria)) +
  geom_col(color = "black") +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%")), color = c("white", "white"),
             position = position_stack(vjust = 0.5),
             show.legend = F) +
  guides(fill = guide_legend(title = " ")) +
  scale_fill_manual(values = c("steelblue", "#f41c5c")) +
  coord_polar(theta = "y") + 
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_31.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 32 --------------------------------------------------------------
Pr32 <- db %>% dplyr::select(P32_1:P32_8)
Pr32 %>% glimpse
# Analisis descriptivo
fqTable <- Pr32 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr32)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("steelblue")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_32.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 33 --------------------------------------------------------------
Pr33 <- db %>% dplyr::select(P33_1:P33_8)
Pr33 %>% glimpse
#
names(Pr33) <- c("Unicentro","Chipichape","Cosmocentro","Jardin Plaza",
                 "Palmetto Plaza","Pacific Mall","Centenario","La Estación")
# Analisis descriptivo
fqTable <- Pr33 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr33)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>% ggplot(aes(x = reorder(Categoria, +Porcentaje), y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_text(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left"), color = "black") +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  # scale_fill_brewer(palette="RdBu")+
  scale_y_continuous(limits = c(0, 35)) +
  facet_grid(~ Variable, scales = "free") +
  theme_ggcharts() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = 'bold'),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="bottom",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5,
                                        linetype = "solid"))

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_33.png"), gg, width=17, height=7, units = "in", dpi=366)

# Evaluacion de oferta, servicios, percepciones  --------------------------
# Pregunta 34 --------------------------------------------------------------
Pr34 <- db %>% dplyr::select(P34)
Pr34 %>% glimpse
# Analisis descriptivo
fqTable <- Pr34 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr34)) 
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Mucho mejor",
                                                          "Mejor",
                                                          "Igual",
                                                          "Peor",
                                                          "Mucho peor"), ordered = T)
lvl <- c("Mucho mejor",
         "Mejor",
         "Igual",
         "Peor",
         "Mucho peor")
# 
gg <- fqTable %>% ggplot(aes(x = reorder(factor(Categoria, level=lvl), +Porcentaje), y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = -0.1)) +
  # geom_text(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left"), color = "black") +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="RdYlGn", direction = -1)+
  scale_y_continuous(limits = c(0, 50)) +
  # facet_wrap(~ Variable, ) +
  theme_ggcharts() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="none",
        # panel.background = element_rect(fill = "white",
        #                                 colour = "white",
        #                                 size = 0.5,
        #                                 linetype = "solid"),
        strip.background = element_blank(),
        strip.text.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_34.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 35 --------------------------------------------------------------
Pr35 <- db %>% dplyr::select(P35_1:P35_11)
Pr35 %>% glimpse
# Analisis descriptivo
fqTable <- Pr35 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr35)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("steelblue")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_35.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 36 --------------------------------------------------------------
Pr36 <- db %>% dplyr::select(P36_1:P36_11)
Pr36 %>% glimpse
# Analisis descriptivo
fqTable <- Pr36 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr36)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("steelblue")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_36.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 37 --------------------------------------------------------------
Pr37 <- db %>% dplyr::select(P37)
Pr37 %>% glimpse
# Analisis descriptivo
fqTable <- Pr37 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr37)) 
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Excelente",
                                                          "Buena",
                                                          "Regular",
                                                          "Mala",
                                                          "Muy mala"), ordered = T)
lvl <- c("Excelente",
         "Buena",
         "Regular",
         "Mala",
         "Muy mala")
# 
gg <- fqTable %>% ggplot(aes(x = reorder(factor(Categoria, level=lvl), +Porcentaje), y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left")) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="RdYlGn", direction = -1)+
  scale_y_continuous(limits = c(0, 100)) +
  # facet_wrap(~ Variable, ) +
  theme_ggcharts() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="none",
        # panel.background = element_rect(fill = "white",
        #                                 colour = "white",
        #                                 size = 0.5,
        #                                 linetype = "solid"),
        strip.background = element_blank(),
        strip.text.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_37.png"), gg, width=13, height=7, units = "in", dpi=366)

# Pregunta 38 -------------------------------------------------------------
Pr38 <- db %>% dplyr::select(P38_1:P38_7)
Pr38 %>% glimpse
# Analisis descriptivo
fqTable <- Pr38 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr38)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("steelblue")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left")) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_38.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 39-40-42-43-44-45 --------------------------------------------------------------
Pr <- db %>% dplyr::select(P39,P40,P42:P45)
Pr %>% glimpse

#
Pr <- Pr %>% dplyr::mutate(P39 = dplyr::case_when(P39	%in% c("Excelente")	~	5,
                                                 P39	%in% c("Buena")	~	4,
                                                 P39	%in% c("Regular")	~	3,
                                                 P39	%in% c("Mala")	~	2,
                                                 P39	%in% c("Muy mala")	~	1),
                           P40 = dplyr::case_when(P40	%in% c("Excelente")	~	5,
                                                  P40	%in% c("Buena")	~	4,
                                                  P40	%in% c("Regular")	~	3,
                                                  P40	%in% c("Mala")	~	2,
                                                  P40	%in% c("Muy mala")	~	1),
                           P42 = dplyr::case_when(P42	%in% c("Excelente")	~	5,
                                                  P42	%in% c("Buena")	~	4,
                                                  P42	%in% c("Regular")	~	3,
                                                  P42	%in% c("Mala")	~	2,
                                                  P42	%in% c("Muy mala")	~	1),
                           P43 = dplyr::case_when(P43	%in% c("Excelente")	~	5,
                                                  P43	%in% c("Buena")	~	4,
                                                  P43	%in% c("Regular")	~	3,
                                                  P43	%in% c("Mala")	~	2,
                                                  P43	%in% c("Muy mala")	~	1),
                           P44 = dplyr::case_when(P44	%in% c("Excelente")	~	5,
                                                  P44	%in% c("Buena")	~	4,
                                                  P44	%in% c("Regular")	~	3,
                                                  P44	%in% c("Mala")	~	2,
                                                  P44	%in% c("Muy mala")	~	1),
                           P45 = dplyr::case_when(P45	%in% c("Excelente")	~	5,
                                                  P45	%in% c("Buena")	~	4,
                                                  P45	%in% c("Regular")	~	3,
                                                  P45	%in% c("Mala")	~	2,
                                                  P45	%in% c("Muy mala")	~	1))

#
names(Pr) <- c("Cómo calificaría la accesibilidad de Unicentro\nen términos de salidas y entradas",
               "Cómo calificaría las zonas verdes y la comodidad\nde las instalaciones",
               "Cómo calificaría la\nambientación y decoración",
               "Cómo calificaría la seguridad",
               "Cómo calificaría la limpieza",
               "Cómo calificaría las\nactividades y eventos que")


# Analisis descriptivo
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr)) 
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Excelente",
                                                          "Buena",
                                                          "Regular",
                                                          "Mala",
                                                          "Muy mala"), ordered = T)
fqTable$Categoria <- as.factor(fqTable$Categoria)

lvl <- c("5",
         "4",
         "3",
         "2",
         "1")
# 
gg <- fqTable %>% ggplot(aes(x = Categoria, y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left")) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="RdYlGn", direction = 1)+
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable, ) +
  theme_ggcharts() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="none")

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_39-45A.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 41 -------------------------------------------------------------
Pr41 <- db %>% dplyr::select(P41)
Pr41 %>% glimpse
# Analisis descriptivo
fqTable <- Pr41 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr41)) 
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Muy agradable",
                                                          "Agradable",
                                                          "Neutral",
                                                          "Desagradable",
                                                          "Muy desagradable"), ordered = T)
lvl <- c("Muy agradable",
         "Agradable",
         "Neutral",
         "Desagradable",
         "Muy desagradable")
#
gg <- fqTable %>% ggplot(aes(x = reorder(factor(Categoria, level=lvl), +Porcentaje), y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left")) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="RdYlGn", direction = -1)+
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable, ) +
  theme_ggcharts() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="none",
        # panel.background = element_rect(fill = "white",
        #                                 colour = "white",
        #                                 size = 0.5,
        #                                 linetype = "solid"),
        strip.background = element_blank(),
        strip.text.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_41.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 46 --------------------------------------------------------------
Pr46 <- db %>% dplyr::select(P46)
Pr46 %>% glimpse
# Analisis descriptivo
fqTable <- Pr46 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr46)) %>% 
  tidyr::drop_na()
#
df2 <- fqTable %>% 
  mutate(csum = rev(cumsum(rev(Porcentaje))), 
         pos = Porcentaje/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Porcentaje/2, pos))

gg <- ggplot(fqTable, aes(x = "" , y = Porcentaje, fill = fct_inorder(Categoria))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(Porcentaje*100,2), "%")),
                   size = 5.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title =" ")) +
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_46.png"), gg, width=9, height=7, units = "in", dpi=366)
rm(df2)

# Pregunta 47 -------------------------------------------------------------
Pr47 <- db %>% dplyr::select(P47_1:P47_8)
Pr47 %>% glimpse
# Analisis descriptivo
fqTable <- Pr47 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr47)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("steelblue")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left")) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_47.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 48 --------------------------------------------------------------
Pr48 <- db %>% dplyr::select(P48)
Pr48 %>% glimpse
# Analisis descriptivo
fqTable <- Pr48 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr48)) %>% 
  tidyr::drop_na()
#
df2 <- fqTable %>% 
  mutate(csum = rev(cumsum(rev(Porcentaje))), 
         pos = Porcentaje/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Porcentaje/2, pos))

gg <- ggplot(fqTable, aes(x = "" , y = Porcentaje, fill = fct_inorder(Categoria))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(Porcentaje*100,2), "%")),
                   size = 5.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title =" ")) +
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_48.png"), gg, width=9, height=7, units = "in", dpi=366)
rm(df2)
# Pregunta 49 -------------------------------------------------------------
Pr49 <- db %>% dplyr::select(P49_1:P49_9)
Pr49 %>% glimpse
# Analisis descriptivo
fqTable <- Pr49 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr49)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Unicentro"), bar_color = c("steelblue")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left")) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_49.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 50 -------------------------------------------------------------
Pr50 <- db %>% dplyr::select(P50)
Pr50 %>% glimpse
# Analisis descriptivo
fqTable <- Pr50 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr50)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  # bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  bar_chart(Categoria, Porcentaje, highlight = c("Si, después de 3 horas","Las primeras 3 horas son gratis"), bar_color = c("steelblue","orange")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_50.png"), gg, width=9, height=7, units = "in", dpi=366)

# Evaluacion de publicidad ------------------------------------------------
# Pregunta 51 -------------------------------------------------------------
Pr51 <- db %>% dplyr::select(P51)
Pr51 %>% glimpse
# Analisis descriptivo
fqTable <- Pr51 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr51)) %>% 
  tidyr::drop_na()
#
df2 <- fqTable %>% 
  mutate(csum = rev(cumsum(rev(Porcentaje))), 
         pos = Porcentaje/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Porcentaje/2, pos))

gg <- ggplot(fqTable, aes(x = "" , y = Porcentaje, fill = fct_inorder(Categoria))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(Porcentaje*100,2), "%")),
                   size = 5.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title =" ")) +
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_51.png"), gg, width=9, height=7, units = "in", dpi=366)
rm(df2)

# Pregunta 52 -------------------------------------------------------------
Pr52 <- db %>% dplyr::select(P52)
Pr52 %>% glimpse
# Analisis descriptivo
fqTable <- Pr52 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr52)) %>% 
  tidyr::drop_na()
#
df2 <- fqTable %>% 
  mutate(csum = rev(cumsum(rev(Porcentaje))), 
         pos = Porcentaje/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Porcentaje/2, pos))

gg <- ggplot(fqTable, aes(x = "" , y = Porcentaje, fill = fct_inorder(Categoria))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(Porcentaje*100,2), "%")),
                   size = 5.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title =" ")) +
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_52.png"), gg, width=9, height=7, units = "in", dpi=366)
rm(df2)

# Pregunta 53 -------------------------------------------------------------
Pr53 <- db %>% dplyr::select(P53_1:P53_6)
Pr53 %>% glimpse
# Analisis descriptivo
fqTable <- Pr53 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr53)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Si, después de 3 horas","Las primeras 3 horas son gratis"), bar_color = c("steelblue","orange")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left")) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_53.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 54 -------------------------------------------------------------
Pr54 <- db %>% dplyr::select(P54_1:P54_5)
Pr54 %>% glimpse
# Analisis descriptivo
fqTable <- Pr54 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr54)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Si, después de 3 horas","Las primeras 3 horas son gratis"), bar_color = c("steelblue","orange")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left")) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_54.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 55 -------------------------------------------------------------
Pr55 <- db %>% dplyr::select(P55)
Pr55 %>% glimpse
# Analisis descriptivo
fqTable <- Pr55 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr55)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Si, después de 3 horas","Las primeras 3 horas son gratis"), bar_color = c("steelblue","orange")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_55.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 56 -------------------------------------------------------------
Pr56 <- db %>% dplyr::select(P56)
Pr56 %>% glimpse
# Analisis descriptivo
fqTable <- Pr56 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr56)) %>% 
  tidyr::drop_na()
#
df2 <- fqTable %>% 
  mutate(csum = rev(cumsum(rev(Porcentaje))), 
         pos = Porcentaje/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Porcentaje/2, pos))

gg <- ggplot(fqTable, aes(x = "" , y = Porcentaje, fill = fct_inorder(Categoria))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(Porcentaje*100,2), "%")),
                   size = 5.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title =" ")) +
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_56.png"), gg, width=9, height=7, units = "in", dpi=366)
rm(df2)
# Pregunta 57 -------------------------------------------------------------
Pr57 <- db %>% dplyr::select(P57_1:P57_6)
Pr57 %>% glimpse
# Analisis descriptivo
fqTable <- Pr57 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr57)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Si, después de 3 horas","Las primeras 3 horas son gratis"), bar_color = c("steelblue","orange")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_57.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 58 -------------------------------------------------------------
Pr58 <- db %>% dplyr::select(P58_1:P58_7)
Pr58 %>% glimpse
# Analisis descriptivo
fqTable <- Pr58 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr58)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Si, después de 3 horas","Las primeras 3 horas son gratis"), bar_color = c("steelblue","orange")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 1.2)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_58.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 59 -------------------------------------------------------------
Pr59 <- db %>% dplyr::select(P59)
Pr59 %>% glimpse
# Analisis descriptivo
fqTable <- Pr59 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr59)) %>% 
  tidyr::drop_na()
#
df2 <- fqTable %>% 
  mutate(csum = rev(cumsum(rev(Porcentaje))), 
         pos = Porcentaje/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Porcentaje/2, pos))

gg <- ggplot(fqTable, aes(x = "" , y = Porcentaje, fill = fct_inorder(Categoria))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(Porcentaje*100,2), "%")),
                   size = 5.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title =" ")) +
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_59.png"), gg, width=9, height=7, units = "in", dpi=366)
rm(df2)
# Pregunta 60 -------------------------------------------------------------
Pr60 <- db %>% dplyr::select(P60)
Pr60 %>% glimpse
# Analisis descriptivo
fqTable <- Pr60 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr60)) %>% 
  tidyr::drop_na()
#
df2 <- fqTable %>% 
  mutate(csum = rev(cumsum(rev(Porcentaje))), 
         pos = Porcentaje/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Porcentaje/2, pos))

gg <- ggplot(fqTable, aes(x = "" , y = Porcentaje, fill = fct_inorder(Categoria))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(Porcentaje*100,2), "%")),
                   size = 5.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title =" ")) +
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_60.png"), gg, width=9, height=7, units = "in", dpi=366)
rm(df2)
# Pregunta 61 -------------------------------------------------------------
Pr61 <- db %>% dplyr::select(P61_1:P61_8)
Pr61 %>% glimpse
# Analisis descriptivo
fqTable <- Pr61 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr61)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Si, después de 3 horas","Las primeras 3 horas son gratis"), bar_color = c("steelblue","orange")) +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left")) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_61.png"), gg, width=15, height=7, units = "in", dpi=366)

# wordcloud ---------------------------------------------------------------
library(tm)
library(wordcloud)
library(RColorBrewer)
#
docs <- Corpus(VectorSource(db$P7))
inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convertir a letras minúsculas el texto.
docs <- tm_map(docs, content_transformer(tolower))
# Remover números
docs <- tm_map(docs, removeNumbers)
# Remover stopwords comunes
docs <- tm_map(docs, removeWords, stopwords("spanish"))
# Remover una palabra en particular.
# Se especifica las stopwords como un vector de caracteres.
docs <- tm_map(docs, removeWords, c("hace", "wow", "mall", "fechas","padr",
                                    "myyu", "ccial", "mes", "zumba","madr", 
                                    "bueno", "ofrece", "nueva", "sitios"))
# remover signos de puntuación
docs <- tm_map(docs, removePunctuation)
# Eliminar espacios en blanco extras.
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)
#crear matriz documento de términos
dtm <- TermDocumentMatrix(docs)
matriz <- as.matrix(dtm)
# ordenar filas de la matriz en orden descendente
v <- sort(rowSums(matriz),decreasing=TRUE)
# convertir a data frame
d <- data.frame(word = names(v),freq=v)
# mostrar los primeros 10 términos que más se repiten
head(d, 10)
write.csv(d, paste0(root,prj,"/3.Results/Pregunta_7.csv"))
#
set.seed(4321)
wordcloud(words = d$word, freq = d$freq, scale = , min.freq = 2,
          max.words=50, random.order=F, rot.per=0.1, 
          colors=brewer.pal(8, "Paired"),
          family="serif")

# -------------------------------------------------------------------------

