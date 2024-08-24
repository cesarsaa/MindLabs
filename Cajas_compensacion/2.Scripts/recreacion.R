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
db <- foreign::read.spss(file = paste0(root,prj,"/1.Data/Base_Recreacion.sav"),
                         use.value.labels = T,
                         to.data.frame = T)
head(db)

# -------------------------------------------------------------------------
db %>% dplyr::count(., Q16) %>% drop_na() %>% dplyr::mutate(Porcentaje = prop.table(n)*100) 
db %>% dplyr::filter(., Q9 == "Comfandi") %>% dplyr::count(., Q28) %>% drop_na() %>% dplyr::mutate(Porcentaje = prop.table(n)*100) 
db %>% dplyr::count(., Q28) %>% drop_na() %>% dplyr::mutate(Porcentaje = prop.table(n)*100) 
db %>% dplyr::count(., Q29) %>% drop_na() %>% dplyr::mutate(Porcentaje = prop.table(n)*100) 
db %>% dplyr::filter(.,Q45db %>% dplyr::filter(., Q4 == "Sí") %>% 
  dplyr::count(., Q28) %>% dplyr::mutate(Porcentaje = round(prop.table(n),3)*100)

pr <- db %>% dplyr::select(.,Q17_1:Q17_9)
pr %>% glimpse

# -------------------------------------------------------------------------
pr %>% tidyr::pivot_longer(cols = Q17_1:Q17_9,
                           names_to = "measure",
                           values_to = "value") %>% 
  # dplyr::group_by(., Q4) %>%
  # dplyr::filter(., Q4 == "Sí") %>% 
  tidyr::drop_na() %>%
  dplyr::count(., value) %>% 
  dplyr::mutate(porcentaje = prop.table(n)*100)

# -------------------------------------------------------------------------
Pr <- db %>% 
  # dplyr::filter(., Q43 == "Si, antes iba más frecuentemente") %>%
  # dplyr::filter(., Q9 == "Comfandi") %>%
  # dplyr::select(Q15_1, Q15_5:Q15_9)
  dplyr::select(Q24_1:Q24_9)

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
# View(fqTable)
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("#1a70b7")) +
  # bar_chart(Categoria, Porcentaje, highlight = "Comfandi Pance ", bar_color = c("#1f38b2")) +
  geom_label(aes(label = paste0(round(Porcentaje,1),"%"), hjust = 1.2)) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ")
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_14.png"), gg, width=15, height=12, units = "in", dpi=366)

write.csv(fqTable, paste0(root,prj,"/3.Results/Pregunta_59.csv"), sep = ",")

# -------------------------------------------------------------------------
pr <- db %>% #dplyr::filter(., Q9 == "Comfandi") %>% 
  dplyr::select(Q14_1, Q14_5:Q14_11)

fqTable <- pr %>% tidyr::pivot_longer(cols = Q34_1:Q34_11,
                                      names_to = "measure",
                                      values_to = "value") %>% 
  dplyr::group_by(., Q7) %>%
  # dplyr::filter(., Q9 == "Sí") %>% 
  tidyr::drop_na() %>%
  dplyr::count(., value) %>% 
  dplyr::mutate(porcentaje = prop.table(n)*100)

gg <- fqTable %>% #dplyr::filter(., value != "Otros ¿Cuál?") %>% 
  ggplot(aes(x = reorder(value, +porcentaje), y = porcentaje, fill=Q7)) +
  geom_bar(stat="identity",show.legend = T, position = "dodge") + 
  geom_text(aes(value,porcentaje,label=paste0(round(porcentaje,2), "%"), 
                group=Q7), position = position_dodge(width = 1), vjust = 0.5, hjust = -0.1, size=4) +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="Set1",direction = -1, name = " ")+
  scale_y_continuous(limits = c(0, 50)) +
  theme_void() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank())
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_15.png"), gg, width=15, height=7, units = "in", dpi=366)


# -------------------------------------------------------------------------
gg <- fqTable %>% slice_max(n=6, order_by = Porcentaje, with_ties = TRUE) %>% 
  #dplyr::filter(., Categoria != "Otro ¿Cuál?") %>%
  ggplot(aes(x = reorder(Categoria, +Porcentaje), y = Porcentaje, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F, position = "dodge") + 
  geom_text(aes(Categoria,Porcentaje,label=paste0(round(Porcentaje,2), "%"),
                group=Categoria), position = position_dodge(width = 1), vjust = 0.5, hjust = -0.1, size=4) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_manual(values = c("#1a70b7","#1a70b7","#1a70b7","#1a70b7","#1a70b7", "#1a70b7",
                               "#1a70b7","#1a70b7","#1a70b7","#1a70b7", "#1a70b7")) +
  # scale_fill_brewer(palette="Paired",direction = 1, name = " ") +
  # scale_fill_manual(values = mycolors, name = " ") +
  # scale_y_continuous(limits = c(0, 40)) +
  theme_void() +
  theme(strip.text = element_text(size = 12, face = "bold", ),
        axis.title.x = element_text(size = 13, face = 'bold' ),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_blank())
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_24.png"), gg, width=14, height=7, units = "in", dpi=366)

# -------------------------------------------------------------------------
Pr3 <- db %>% dplyr::select(Q26_1:Q26_11)
Pr3 %>% glimpse

# Analisis descriptivo
fqTable <- Pr3 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr3)*100)) %>% 
  tidyr::drop_na()
fqTable
#
df2 <- fqTable %>% 
  mutate(csum = rev(cumsum(rev(Porcentaje))), 
         pos = Porcentaje/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Porcentaje/2, pos))

gg <- ggplot(fqTable, aes(x = "" , y = Porcentaje, fill = fct_inorder(Categoria))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  # scale_fill_brewer(palette = "Set1") +
  scale_fill_manual(values = c("#1a70b7","grey60","#a31116","#ff0000","#e24b37","#18a2f3")) +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(Porcentaje,1), "%")),
                   size = 5.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title =" ")) +
  theme_void()
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_22.png"), gg, width=9, height=7, units = "in", dpi=366)
rm(df2)  

# -------------------------------------------------------------------------
Pr37 <- db %>% dplyr::select(Q69)
Pr37 %>% glimpse
#
names(Pr37) <- c("La Experiencia en general \n de Comfandi Pance",
                 "El servicio al cliente /atención recibida",
                 "Las actividades / juegos terrestres",
                 "Las actividades /juegos Acuáticos ",
                 "Las canchas deportivas",
                 "Las piscinas",
                 "Los baños",
                 "La seguridad ofrecida en el lugar",
                 "Parqueadero",
                 "Alimentos y bebidas",
                 "Taquilla e ingreso al centro")

# Analisis descriptivo
fqTable <- Pr37 %>%
  gather(measure, value) %>%
  count(measure, value) %>% drop_na()
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr37)) 
levels(db$Q55)
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Excelente",
                                                          "Muy buena",
                                                          "Buena",
                                                          "Regular",
                                                          "Mala",
                                                          "Muy mala"), ordered = T)
lvl <- c("Muy mala",
         "Mala",
         "Regular",
         "Buena",
         "Muy buena",
         "Excelente")
# 
gg <- fqTable %>% ggplot(aes(x = (factor(Categoria, level=lvl)), y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  # geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left")) +
  geom_text(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left"), color = "black") +
  # geom_text(aes(Categoria,Porcentaje,label=paste0(round(Porcentaje,2), "%"), 
  #               group=Categoria), position = position_dodge(width = 1), vjust = 1, hjust = "left", size=4) +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="RdYlGn", direction = -1)+
  scale_y_continuous(limits = c(0, 50)) +
  # facet_wrap(~Variable) +
  # theme_ggcharts() +
  theme_void() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="none",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5,
                                        linetype = "solid"),
        strip.background = element_blank(),strip.text.x = element_blank()
        )
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_55.png"), gg, width=13, height=7, units = "in", dpi=366)

# -------------------------------------------------------------------------
Pr29 <- db %>% dplyr::select(Q58_1)
Pr29 %>% glimpse
# Analisis descriptivo
fqTable <- Pr29 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr29)) 
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("0",
                                                          "1","2","3","4","5",
                                                          "6","7","8","9",
                                                          "10"), ordered = T)
# 0 a 6 detractores
dtr <- ((0.005208333) + (0.005208333) + (0.007812500) + (0.010416667) + (0.007812500) + (0.067708333))*100
# 7 a 8 pasivos
# 9 a 10 promotores
pro <- ((0.138020833) + (0.328125000))*100
NPS <- (pro - dtr)
NPS
#
lvl <- c("0",
         "1","2","3","4","5",
         "6","7","8","9",
         "10")
# 
gg <- fqTable %>% ggplot(aes(x = factor(Categoria, level=lvl), y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  # geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = -0.1)) +
  geom_text(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left"), color = "black") +
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
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_58.png"), gg, width=12, height=7, units = "in", dpi=366)

