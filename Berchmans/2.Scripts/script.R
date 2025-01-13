# -------------------------------------------------------------------------
# Proyecto: Colegio Berchmans
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
prj <- "Berchmans"
# Lectura de datos --------------------------------------------------------
db <- readxl::read_xlsx(paste0(root,prj,"/1.Data/db_colegio.xlsx"))
# -------------------------------------------------------------------------
# Pregunta 1
Pr <- db %>% dplyr::select(p1)
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) %>% 
  tidyr::drop_na(); fqTable

# Pregunta 2
Pr <- db %>% dplyr::select(p2)
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) %>% 
  tidyr::drop_na(); fqTable

# Pregunta 3
Pr <- db %>% dplyr::select(p3)
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) %>% 
  tidyr::drop_na(); fqTable

# Pregunta 4
Pr <- db %>% dplyr::select(p4)
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) %>% 
  tidyr::drop_na(); fqTable

# Pregunta 4
Pr <- db %>% dplyr::select(p5_1:p5_7)
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) %>% 
  tidyr::drop_na(); fqTable

# Pregunta 6
Pr <- db %>% dplyr::select(p6_1:p6_9)
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) %>% 
  tidyr::drop_na(); fqTable

# Pregunta 7
Pr <- db %>% dplyr::select(p8)
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) %>% 
  tidyr::drop_na(); fqTable

# -------------------------------------------------------------------------
my_colors <- RColorBrewer::brewer.pal(9, "Greens")[4:9]
my_colors <- c("#00441B","#00441B","#00441B","#00441B","#00441B",
               "#00441B","#00441B","#00441B","#00441B")
# -------------------------------------------------------------------------
fqTable %>% 
  ggplot2::ggplot(aes(x = reorder(Categoria, +Porcentaje), y = Porcentaje, fill=Categoria)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single")) + 
  geom_text(aes(label = paste0(round(Porcentaje,2),"%")), size = 10, hjust = -0.1, position = position_dodge(0.9)) +
  ggtitle(" ") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 70)) +
  scale_fill_manual(values = my_colors, name = " ") +
  # scale_fill_brewer(palette="Greens", name = " ", direction = 1) +
  # facet_wrap(~ Variable) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", color = "white", hjust = 0, size = 15),
        strip.background = element_rect(fill = "dodgerblue3", linetype = "solid",
                                        color = "black", linewidth = 1),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text = element_text(size = 18),
        legend.position = "none",
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())
# -------------------------------------------------------------------------

fqTable$Categoria <- factor(fqTable$Categoria, 
                            levels = c("1: Nada importante",
                                       "2: Poco importante",
                                       "3: Medianamente importante",
                                       "4: Importante",
                                       "5: Muy importante"), ordered = T)
lvl <- c("1: Nada importante",
         "2: Poco importante",
         "3: Medianamente importante",
         "4: Importante",
         "5: Muy importante")

fqTable %>% 
  ggplot2::ggplot(aes(x = (Categoria), y = Porcentaje, fill=Categoria)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single")) + 
  geom_text(aes(label = paste0(round(Porcentaje,2),"%")), size = 10, hjust = -0.1, position = position_dodge(0.9)) +
  ggtitle(" ") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 70)) +
  scale_fill_brewer(palette="Greens", name = " ", direction = 1) +
  # facet_wrap(~ Variable) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", color = "white", hjust = 0, size = 15),
        # strip.background = element_rect(fill = "dodgerblue3", linetype = "solid",
        #                                 color = "black", linewidth = 1),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text = element_text(size = 18),
        legend.position = "none",
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())
# -------------------------------------------------------------------------

# Pregunta 16
fqTable <- db |> 
  dplyr::select(p13, p16) |> 
  dplyr::group_by(p13, p16) |>
  dplyr::summarise(frecuencia = n(), .groups = "drop") |>
  plyr::mutate(porcentaje = (frecuencia / sum(frecuencia)) * 100); fqTable
names(fqTable) <- c("Variable", "Categoria", "Frecuencia", "Porcentaje")
fqTable$Categoria <- as.factor(fqTable$Categoria)


# Pregunta 19
fqTable <- db |> 
  dplyr::select(p13, p17) |> 
  dplyr::group_by(p13, p17) |>
  dplyr::summarise(frecuencia = n(), .groups = "drop") |>
  plyr::mutate(porcentaje = (frecuencia / sum(frecuencia)) * 100); fqTable
names(fqTable) <- c("Variable", "Categoria", "Frecuencia", "Porcentaje")
fqTable$Categoria <- as.factor(fqTable$Categoria); fqTable


# Pregunta 20
Pr <- db |> 
  dplyr::select(p20_1:p20_5)
names(Pr) <- c("Color","Textura","Olor","Sabor","Sensación en la boca al \nterminar de probar el producto")
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable


fqTable %>% 
  ggplot2::ggplot(aes(x = (Categoria), y = Porcentaje, fill=Categoria)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single")) + 
  geom_text(aes(label = paste0(round(Porcentaje,2),"%")), size = 8, hjust = -0.1, position = position_dodge(0.9)) +
  ggtitle(" ") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 90)) +
  scale_fill_brewer(palette="RdYlGn", name = " ", direction = 1) +
  facet_wrap(~ Variable) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", color = "white", hjust = 0, size = 15),
        strip.background = element_rect(fill = "dodgerblue3", linetype = "solid",
                                        color = "black", linewidth = 1),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text = element_text(size = 18),
        legend.position = "none",
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

# Pregunta 28
fqTable <- db |> 
  dplyr::select(p13, p32) |> 
  dplyr::group_by(p13, p32) |>
  dplyr::summarise(frecuencia = n(), .groups = "drop") |>
  plyr::mutate(porcentaje = (frecuencia / sum(frecuencia)) * 100); fqTable
names(fqTable) <- c("Variable", "Categoria", "Frecuencia", "Porcentaje")
fqTable$Categoria <- as.factor(fqTable$Categoria)


fqTable %>% 
  ggplot2::ggplot(aes(x = (Categoria), y = Porcentaje, fill=Categoria)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single")) + 
  geom_text(aes(label = paste0(round(Porcentaje,2),"%")), size = 8, hjust = -0.1, position = position_dodge(0.9)) +
  ggtitle(" ") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 90)) +
  scale_fill_brewer(palette="RdYlGn", name = " ", direction = 1) +
  facet_wrap(~ Variable) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", color = "white", hjust = 0, size = 15),
        strip.background = element_rect(fill = "dodgerblue3", linetype = "solid",
                                        color = "black", linewidth = 1),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text = element_text(size = 18),
        legend.position = "none",
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())


