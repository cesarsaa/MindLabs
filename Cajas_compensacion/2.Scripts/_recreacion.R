# -------------------------------------------------------------------------
# Proyecto:
# By:  Cesar A. Saavedra
# Mindlabs
# Agosto 2024

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
                         use.value.labels = T,
                         to.data.frame = T)
head(db)

# analisis exploratorio ---------------------------------------------------
# Q1
pr1 <- db %>% dplyr::select(Q1,Q9)
pr1 %>% glimpse
# Analisis descriptivo
fqTable <- pr1 %>%
  group_by(., Q9) %>% 
  count(., Q1)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(pr1)) %>% 
  tidyr::drop_na()

gg <- fqTable %>% 
  ggplot(aes(x = reorder(Variable, +Porcentaje), y = Porcentaje*100, fill=Variable)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = -0.1)) +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="Set1", direction = -1)+
  scale_y_continuous(limits = c(0, 45)) +
  theme_ggcharts() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_blank())

# -------------------------------------------------------------------------
# Q3
pr3 <- db %>% dplyr::select(Q3,Q9)
pr3 %>% glimpse
# Analisis descriptivo
fqTable <- pr3 %>%
  group_by(., Q9) %>% 
  count(., Q3)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (prop.table(Frecuencia)*100))

gg <- fqTable %>% 
  ggplot(aes(x = reorder(Categoria, +Porcentaje), y = Porcentaje, fill=Variable)) +
  geom_bar(stat="identity", position = "dodge2", show.legend = T) + 
  geom_text(aes(label = ifelse(Variable == "Comfandi",
                               paste0(round(Porcentaje,2), "%"), "")),
            position = position_dodge(width = 1), vjust = 0, hjust = 0) +
  labs(title = " ",
       x = " ",
       y = "Porcentaje (%)",
       fill = " ") +
  coord_flip() +
  scale_fill_brewer(palette="Set1", direction = -1)+
  theme_ggcharts() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank())

# -------------------------------------------------------------------------
# Q4
pr4 <- db %>% dplyr::select(Q4,Q9)
pr4 %>% glimpse
# Analisis descriptivo
fqTable <- pr4 %>%
  group_by(., Q9) %>% 
  count(., Q4)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (prop.table(Frecuencia)*100))

gg <- fqTable %>% 
  ggplot(aes(x = reorder(Categoria, +Porcentaje), y = Porcentaje, fill=Variable)) +
  geom_bar(stat="identity", position = "dodge2", show.legend = T) + 
  geom_text(aes(label=paste0(round(Porcentaje,2), "%")), 
            position = position_dodge(width = 1), vjust = 0, hjust = 0) +
  labs(title = " ",
       x = " ",
       y = "Porcentaje (%)",
       fill = " ") +
  coord_flip() +
  scale_fill_brewer(palette="Set1", direction = -1) +
  theme_ggcharts() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank())


# -------------------------------------------------------------------------
# P4
pr4 <- db %>% dplyr::select(P4,Q9)
pr4 %>% glimpse
# Analisis descriptivo
fqTable <- pr4 %>%
  group_by(., Q9) %>% 
  count(., P4)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (prop.table(Frecuencia)*100))

gg <- fqTable %>% 
  ggplot(aes(x = reorder(Categoria, +Porcentaje), y = Porcentaje, fill=Variable)) +
  geom_bar(stat="identity", position = "dodge2", show.legend = T) + 
  geom_text(aes(label=paste0(round(Porcentaje,2), "%")), 
            position = position_dodge(width = 1), vjust = 0, hjust = 0) +
  labs(title = " ",
       x = " ",
       y = "Porcentaje (%)",
       fill = " ") +
  coord_flip() +
  scale_fill_brewer(palette="Set1", direction = -1) +
  theme_ggcharts() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank())

# -------------------------------------------------------------------------



