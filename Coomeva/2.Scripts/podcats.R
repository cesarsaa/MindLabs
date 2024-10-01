# -------------------------------------------------------------------------
# Proyecto:
# By:  Cesar A. Saavedra
# Mindlabs
# Septiembre 2024

# R options ---------------------------------------------------------------
g <- gc(reset = T); rm(list = ls())
options(warn = -1, scipen = 999)
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(dplyr, tidyr, ggplot2, ggrepel, foreign, ggcharts, mdthemes,forcats,
                                readxl, raster, geodata, terra, sf, sp, lubridate))

root <- "/Users/cesara.saavedravanegas/Documents/GitHub/MindLabs/"
prj <- "COOMEVA"

# -------------------------------------------------------------------------

db <- readxl::read_xlsx(paste0(root,prj,"/1.Data/videopodcast_COOMEVA.xlsx")) 
db
# -------------------------------------------------------------------------
Pr <- db %>% dplyr::select(`1. En una escala de 1 a 5, donde 1 es nada interesado y 5 es muy interesado, ¿Qué tan interesado estarías en seguir/ escuchar los videopodcast de Coomeva MP?`)
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) %>% 
  tidyr::drop_na()
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("5. Muy interesado",
                                                          "4. Interesado",
                                                          "3. Neutral",
                                                          "2. Poco interesado",
                                                          "1. Nada interesado"), 
                            ordered = T)

fqTable
#
#
lvl <- c("1. Nada interesado",
         "2. Poco interesado",
         "3. Neutral",
         "4. Interesado",
         "5. Muy interesado")
# 
gg <- fqTable %>% ggplot(aes(x = factor(Categoria, level=lvl), y = Porcentaje, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje,2),"%"), hjust = -0.1)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  ggtitle("¿Qué tan interesado estarías en seguir/escuchar \nlos videopodcast de Coomeva MP?") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="RdYlGn", direction = -1)+
  scale_y_continuous(limits = c(0, 50)) +
  # facet_wrap(~ Variable, ) +
  # theme_ggcharts() +
  theme_void() +
  theme(text = element_text(family="BombardGrotesk-Bold", 
                            size = 18),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 18,face = "bold"),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text = element_text(size = 18),
        legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_blank())
gg
#
ggplot2::ggsave(paste0(root,prj,"/3.Results/1.jpeg"), gg, width=9, height=7, units = "in", dpi=366)

# -------------------------------------------------------------------------
Pr <- db %>% dplyr::select(`¿Qué nombre creen que podría representar mejor la esencia de la marca Coomeva MP para el podcast? ¿Por qué?`)
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) %>% 
  tidyr::drop_na()
fqTable

gg <- fqTable %>% ggplot(aes(x = reorder(Categoria, Porcentaje), y = Porcentaje, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje,2),"%"), hjust = -0.1)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  ggtitle("¿Qué nombre creen que podría representar mejor la esencia \nde la marca Coomeva MP para el podcast?") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="Greens", direction = -1)+
  scale_y_continuous(limits = c(0, 40)) +
  # facet_wrap(~ Variable, ) +
  # theme_ggcharts() +
  theme_void() +
  theme(text = element_text(family="BombardGrotesk-Bold", 
                            size = 18),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 18,face = "bold"),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text = element_text(size = 18),
        legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_blank())
gg
#
ggplot2::ggsave(paste0(root,prj,"/3.Results/3.jpeg"), gg, width=12, height=7, units = "in", dpi=366)


# -------------------------------------------------------------------------
Pr <- db %>% dplyr::select(`SEÑAL SALUDABLE`)
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) %>% 
  tidyr::drop_na()
fqTable
#
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("5. Muy claro",
                                                          "4. Claro",
                                                          "3. Neutral",
                                                          "2. Poco claro",
                                                          "1. Nada claro"), 
                            ordered = T)

fqTable
#
lvl <- c("1. Nada claro",
         "2. Poco claro",
         "3. Neutral",
         "4. Claro",
         "5. Muy claro")
# 
gg <- fqTable %>% ggplot(aes(x = factor(Categoria, level=lvl), y = Porcentaje, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje,2),"%"), hjust = -0.1)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  ggtitle("¿Qué tan claro y pertinente te resulta cada nombre en \nrelación con el tema de salud?",
          subtitle = "SEÑAL SALUDABLE") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="Greens", direction = -1)+
  scale_y_continuous(limits = c(0, 50)) +
  # facet_wrap(~ Variable, ) +
  # theme_ggcharts() +
  theme_void() +
  theme(text = element_text(family="BombardGrotesk-Bold", 
                            size = 18),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 18,face = "bold"),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text = element_text(size = 18),
        legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_blank())
gg
#
ggplot2::ggsave(paste0(root,prj,"/3.Results/2.3.jpeg"), gg, width=9, height=7, units = "in", dpi=366)
