# -------------------------------------------------------------------------
# Proyecto: 
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
prj <- "Identidad_visual"
# -------------------------------------------------------------------------

db <- readxl::read_xlsx(paste0(root,prj,"/1.Data/CUAL0011_FREE.xlsx"))
head(db)

# -------------------------------------------------------------------------
Pr <- db %>% dplyr::select(`¿Cúal sería un precio tan bajo que te haría dudar de la calidad de Tarrito rojo sin azúcar de 240g?`)
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
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje) +
  geom_label(aes(label = paste0(round(Porcentaje,2),"%"), hjust = 1.1)) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = "¿Cúal sería un precio tan bajo que te haría \ndudar de la calidad de Tarrito rojo sin azúcar de 240g?",
       subtitle = " ",
       caption = " ") +
  scale_y_continuous(limits = c(0, 40)) +
  theme(text = element_text(family="BombardGrotesk-Bold", 
                            size = 18)
        ,axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 18,face = "bold"),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text = element_text(size = 18))
gg
#
ggplot2::ggsave(paste0(root,prj,"/3.Results/1_3.png"), gg, width=9, height=7, units = "in", dpi=366)

# -------------------------------------------------------------------------



