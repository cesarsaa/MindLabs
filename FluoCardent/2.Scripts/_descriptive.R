# R options ---------------------------------------------------------------
g <- gc(reset = T); rm(list = ls())
options(warn = -1, scipen = 999)
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(dplyr, tidyr, ggrepel, ggplot2, gganimate, foreign, ggcharts, mdthemes,forcats,
                                tm, wordcloud, readxl, raster, geodata, terra, sf, sp, lubridate))

root <- "/Users/cesara.saavedravanegas/Documents/GitHub/MindLabs/"
prj <- "FluoCardent"
# Lectura de datos --------------------------------------------------------
db <- foreign::read.spss(file = paste0(root,prj,"/1.Data/Estudio cremas dentales_MindLabs_Final.sav"),
                         use.value.labels = T,
                         to.data.frame = T)
head(db)
db <- db %>% dplyr::filter(., Q6 != "Fluocardent  ") #"Fluocardent  "
# -------------------------------------------------------------------------
Pr <- db %>% dplyr::select(Q20_1:Q20_7)
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
write.csv(fqTable,paste0(root,prj,"/3.Results/Pregunta_25B.csv"))
# -------------------------------------------------------------------------
gg <- fqTable %>%
  ggplot(aes(x = reorder(Categoria, +Porcentaje), 
             y = Porcentaje)) +
  geom_bar(stat="identity", 
           show.legend = F, 
           position = "dodge",
           fill="steelblue") + 
  geom_label(aes(label = paste0(round(Porcentaje,0),"%"), 
                 hjust = 0.4,
                 # vjust = -0.2,
                 vjust = 0.5
                 ), 
             size = 8) +
  # geom_text(aes(Categoria,
  #               Porcentaje,
  #               label=paste0(round(Porcentaje,0), "%"), 
  #               group=Categoria), 
  #           position = position_dodge(width = 1), 
  #           vjust = -0.2, 
  #           hjust = 0.4, 
  #           size = 7) +
  ggtitle("") +
  xlab("") + 
  ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 60)) +
  theme_ggcharts() +
  theme(strip.text = element_text(size = 18, 
                                  face = "bold"),
        axis.title.x = element_text(size = 18, 
                                    face = 'bold'),
        axis.text = element_text(size = 12),
        # strip.background = element_blank(),
        # axis.text.x = element_blank(),
        legend.position="none")
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_26B.png"), gg, width=12, height=7, units = "in", dpi=366)

# -------------------------------------------------------------------------
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Fluocardent  "), bar_color = c("steelblue")) +
  # geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = 0.2),
  #            size = 7) +
  geom_label(aes(label=paste0(round(Porcentaje,0),"%")),
             position = position_dodge(width = 1),
             show.legend = F,
             fill="white",
             size = 7) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(strip.text = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 18, face = 'bold'),
        axis.text = element_text(size = 10),
        strip.background = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_10.png"), gg, width=12, height=7, units = "in", dpi=366)
