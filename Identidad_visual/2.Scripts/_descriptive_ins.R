# -------------------------------------------------------------------------
# Proyecto: Imagen de JGB
# By:  Cesar A. Saavedra
# Mindlabs
# Octubre 2024

# R options ---------------------------------------------------------------
g <- gc(reset = T); rm(list = ls())
options(warn = -1, scipen = 999)
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(dplyr, tidyr, ggplot2, ggh4x, ggrepel, foreign, ggcharts, mdthemes,forcats,
                                readxl, raster, geodata, terra, sf, sp, lubridate))

root <- "/Users/cesara.saavedravanegas/Documents/GitHub/MindLabs/"
prj <- "Identidad_visual"
# Lectura de datos --------------------------------------------------------
db <- readxl::read_xlsx(paste0(root,prj,"/1.Data/data.xlsx"))
head(db)

# Demograficos ------------------------------------------------------------
# Pregunta 1
Pr <- db %>% dplyr::select(`¿Cuál es su ciudad de residencia?`)
Pr %>% glimpse
prop.table(table(Pr))*100

# Pregunta 2
Pr <- db %>% dplyr::select(`Indica te identificas cómo:`)
Pr %>% glimpse
prop.table(table(Pr))*100

# Pregunta 3
Pr <- db %>% dplyr::select(`De acuerdo con lo que indica el recibo de servicios públicos, ¿a qué estrato corresponde tu hogar?`)
Pr %>% glimpse
prop.table(table(Pr))*100

# Pregunta 4
Pr <- db %>% dplyr::select(`¿En qué rango de edad te encuentras?`)
Pr %>% glimpse
# Pr <- as.factor(Pr$`¿En qué rango de edad te encuentras?`)
# Analisis descriptivo
fqTable <- Pr %>% count()
names(fqTable) <- c("Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = round(prop.table(Frecuencia)*100,1))

# Pregunta 5
Pr <- db %>% dplyr::select(`¿En los últimos meses recuerdas haber visto u oído publicidad de algún laboratorio farmacéutico / empresa de productos para la salud / cuidado personal?`)
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>% count()
names(fqTable) <- c("Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = round(prop.table(Frecuencia)*100,1))

# Pregunta 6
Pr <- db %>% dplyr::select(`¿De qué marca era esa publicidad?`)
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>% count()
names(fqTable) <- c("Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = round(prop.table(Frecuencia)*100,1))

# Pregunta 7
Pr <- db[,7]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>% count()
names(fqTable) <- c("Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = round(prop.table(Frecuencia)*100,1))

# Exploracion de categoria ------------------------------------------------
# Pregunta 8
Pr <- db[,9]
Pr %>% glimpse
# Analisis descriptivo
Pr[1]<- tolower(Pr$`Cuándo piensas en productos de salud, bienestar y cuidado personal, ¿Cuál es la primera marca que se te viene a la mente?`)
fqTable <- Pr %>% count()
names(fqTable) <- c("Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = round(prop.table(Frecuencia)*100,1)) %>% 
  tidyr::drop_na()
write.csv(Pr, paste0(root,prj,"/3.Results/Pregunta8.csv"))

# Pregunta 9
Pr <- db[,10:25]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable$measure <- gsub("^.*\\?\\s*", "", fqTable$measure, ignore.case = F)
names(fqTable) <- c("Marca","Categoria","n","Frecuencia")
# grafico
gg <- fqTable %>%
  bar_chart(Marca, Frecuencia) +
  geom_label(aes(label = paste0(round(Frecuencia,2),"%"), hjust = 1.1)) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = "¿Con qué característica asocias/identificas a cada una de estas empresas?",
       subtitle = " ",
       caption = " ") +
  # scale_y_continuous(limits = c(0, 40)) +
  theme(text = element_text(family="BombardGrotesk-Bold", 
                            size = 18)
        ,axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 18,face = "bold"),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text = element_text(size = 18))
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/¿Con qué característica asocias-identificas a cada una de estas empresas?.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 10
Pr <- db[,26]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
names(fqTable) <- c("pregunta","Categoria","n","Frecuencia")
# grafico
gg <- fqTable %>%
  bar_chart(Categoria, Frecuencia) +
  geom_label(aes(label = paste0(round(Frecuencia,2),"%"), hjust = 1.1)) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = "¿Cuál es tu marca favorita de productos de salud y bienestar?",
       subtitle = " ",
       caption = " ") +
  # scale_y_continuous(limits = c(0, 40)) +
  theme(text = element_text(family="BombardGrotesk-Bold", 
                            size = 18)
        ,axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 18,face = "bold"),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text = element_text(size = 18))
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/¿Cuál es tu marca favorita de productos de salud y bienestar?.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 11
Pr <- db[,28:34]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
names(fqTable) <- c("pregunta","Categoria","n","Frecuencia")
# grafico
gg <- fqTable %>%
  bar_chart(Categoria, Frecuencia) +
  geom_label(aes(label = paste0(round(Frecuencia,2),"%"), hjust = 1.1)) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = "¿Cuáles son los aspectos que más valoras de tu marca favorita?",
       subtitle = " ",
       caption = " ") +
  # scale_y_continuous(limits = c(0, 40)) +
  theme(text = element_text(family="BombardGrotesk-Bold", 
                            size = 18)
        ,axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 18,face = "bold"),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text = element_text(size = 18))
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/¿Cuáles son los aspectos que más valoras de tu marca favorita?.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 12
Pr <- db[,36:42]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable$measure <- gsub("^.*importante\\s*", "", fqTable$measure, ignore.case = T)

# Pregunta 13
Pr <- db[,43:47]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable$measure <- as.factor(fqTable$measure)
fqTable$measure <- gsub("^.*\\?\\s*", "", fqTable$measure, ignore.case = T)
# grafico
gg <- fqTable %>% 
  ggplot2::ggplot(aes(x = interaction(value,measure, sep = "&"), y = Porcentaje, fill = measure)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single")) + 
  ggplot2::labs(title = "¿Qué te hace sentir la marca a nivel emocional?",
                x = " ",
                y = " ",
                fill = " ") +
  ggplot2::coord_flip() +
  theme_ggcharts() +
  # ggplot2::scale_fill_brewer(palette = "Paired") +
  scale_fill_manual(values = c("#1a70b7","#1a70b7","#1a70b7",
                               "#1a70b7","#1a70b7","#1a70b7",
                               "#1a70b7","#1a70b7","#1a70b7",
                               "#1a70b7","#1a70b7","#1a70b7",
                               "#1a70b7","#1a70b7","#1a70b7"))+
  ggplot2::scale_x_discrete(guide = guide_axis_nested(delim = "&")) +
  geom_label(aes(label = paste0(round(Porcentaje,2),"%"), size = 10, hjust = -0.1),colour = "white") +
  ggplot2::theme(text = element_text(family="BombardGrotesk-Bold",size = 18),
                 axis.text.x = element_blank(),
                 axis.line.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 strip.text = element_text(size = 18,face = "bold"),
                 axis.title.x = element_text(size = 18,face = 'bold'),
                 axis.text = element_text(size = 18),
                 legend.position = "none")
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/¿Qué te hace sentir la marca a nivel emocional?.png"), gg, width=18, height=8, units = "in", dpi=366)

# Pregunta 14
Pr <- db[,48]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()

# Pregunta 15
Pr <- db[,49:57]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable$measure <- as.factor(fqTable$measure)
fqTable$measure <- gsub("(JGB).*", "", fqTable$measure, ignore.case = T)
# grafico
gg <- fqTable %>% 
  ggplot(aes(x = reorder(value, +Porcentaje), y = Porcentaje)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single"), fill="#1a70b7") + 
  geom_label(aes(label = paste0(round(Porcentaje,2),"%")), size = 10, vjust = -0.1) +
  ggplot2::labs(title = " ",
                x = " ",
                y = " ",
                fill = " ") +
  # ggplot2::coord_flip() +
  scale_y_continuous(limits = c(0, 80)) +
  theme_ggcharts() +
  # ggplot2::scale_fill_brewer(palette = "Paired") +
  # scale_fill_manual(values = c("#1a70b7","#1a70b7","#1a70b7",
  #                              "#1a70b7","#1a70b7","#1a70b7",
  #                              "#1a70b7","#1a70b7","#1a70b7"))+
  # ggplot2::scale_x_discrete(guide = guide_axis_nested(delim = "&")) +
  ggplot2::theme(text = element_text(family="BombardGrotesk-Bold",size = 18),
                 # axis.text.x = element_blank(),
                 axis.line.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 strip.text = element_text(size = 18,face = "bold"),
                 axis.title.x = element_text(size = 18,face = 'bold'),
                 axis.text = element_text(size = 18),
                 legend.position = "none")
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/Con que colores asocias la marca.png"), gg, width=15, height=9, units = "in", dpi=366)

# Pregunta 16
Pr <- db[,58]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable$measure <- as.factor(fqTable$measure)

# Pregunta 17
Pr <- db[,59:63]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable$measure <- as.factor(fqTable$measure)
fqTable$measure <- gsub("(JGB).*", "", fqTable$measure, ignore.case = T)
# grafico
gg <- fqTable %>% 
  ggplot(aes(x = reorder(value, +Porcentaje), y = Porcentaje)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single"), fill="#1a70b7") + 
  geom_label(aes(label = paste0(round(Porcentaje,2),"%")), hjust = -0.1, size = 12) +
  ggplot2::labs(title = " ",
                x = " ",
                y = " ",
                fill = " ") +
  ggplot2::coord_flip() +
  scale_y_continuous(limits = c(0, 80)) +
  theme_ggcharts() +
  # ggplot2::scale_fill_brewer(palette = "Paired") +
  # scale_fill_manual(values = c("#1a70b7","#1a70b7","#1a70b7",
  #                              "#1a70b7","#1a70b7"))+
  # ggplot2::scale_x_discrete(guide = guide_axis_nested(delim = "&")) +
  ggplot2::theme(text = element_text(family="BombardGrotesk-Bold",size = 18),
                 # axis.text.x = element_blank(),
                 axis.line.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 strip.text = element_text(size = 20,face = "bold"),
                 axis.title.x = element_text(size = 18,face = 'bold'),
                 axis.text = element_text(size = 18),
                 legend.position = "none")
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/Cuando escuchas el nombre “JGB” ¿En qué tipo de productos piensas?.png"), gg, width=22, height=12, units = "in", dpi=366)

# Pregunta 18
Pr <- db[,65:78]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable$measure <- as.factor(fqTable$measure)
# grafico
gg <- fqTable %>%
  bar_chart(value, Porcentaje) +
  geom_label(aes(label = paste0(round(Porcentaje,2),"%"), hjust = 1.1), size = 12) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = "Con cuáles características de personalidad relacionas JGB",
       subtitle = " ",
       caption = " ") +
  scale_y_continuous(limits = c(0, 80)) +
  # scale_y_continuous(limits = c(0, 40)) +
  theme(text = element_text(family="BombardGrotesk-Bold", 
                            size = 18)
        ,axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 18,face = "bold"),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text = element_text(size = 18))
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/Con cuáles características de personalidad relacionas JGB.png"), gg, width=12, height=7, units = "in", dpi=366)

# Pregunta 19
Pr <- db[,80:88]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable$measure <- as.factor(fqTable$measure)
# grafico
gg <- fqTable %>%
  bar_chart(value, Porcentaje) +
  geom_label(aes(label = paste0(round(Porcentaje,2),"%"), hjust = 1.1)) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = "¿Actualmente utilizas algún producto de JGB?",
       subtitle = " ",
       caption = " ") +
  # scale_y_continuous(limits = c(0, 40)) +
  theme(text = element_text(family="BombardGrotesk-Bold", 
                            size = 18)
        ,axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 18,face = "bold"),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text = element_text(size = 18))
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/¿Actualmente utilizas algún producto de JGB?.png"), gg, width=12, height=7, units = "in", dpi=366)

# Pregunta 20
Pr <- db[,90:94]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable$measure <- gsub("^.*\\?\\s*", "", fqTable$measure, ignore.case = T)
# grafico
gg <- fqTable %>% 
  ggplot2::ggplot(aes(x = measure, y = Porcentaje, fill=value)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single")) + 
  geom_text(aes(label = paste0(round(Porcentaje,2),"%")), size = 12, hjust = -0.1, position = position_dodge(0.9)) +
  ggtitle("¿Qué tan dispuesto estarías a comprar estos productos?") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 60)) +
  scale_fill_brewer(palette="RdYlGn", name = " ") +
  theme_ggcharts() +
  ggplot2::theme(text = element_text(family="BombardGrotesk-Bold",size = 18),
                 axis.text.x = element_blank(),
                 axis.line.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 strip.text = element_text(size = 18,face = "bold"),
                 axis.title.x = element_text(size = 18,face = 'bold'),
                 axis.text = element_text(size = 18),
                 legend.position = "bottom")
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/Qué tan dispuesto estarías a comprar estos productos.jpeg"), gg, width=25, height=13, units = "in", dpi=366)


# Pregunta 21
Pr <- db[,95]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable$measure <- gsub("^.*\\?\\s*", "", fqTable$measure, ignore.case = T)
# grafico
gg <- fqTable %>%
  bar_chart(value, Porcentaje) +
  geom_label(aes(label = paste0(round(Porcentaje,2),"%"), hjust = 1.1)) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = "¿Qué podrías decir sobre los productos de JGB?",
       subtitle = " ",
       caption = " ") +
  # scale_y_continuous(limits = c(0, 40)) +
  theme(text = element_text(family="BombardGrotesk-Bold", 
                            size = 18)
        ,axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 18,face = "bold"),
        axis.title.x = element_text(size = 18,face = 'bold'),
        axis.text = element_text(size = 18))
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/¿¿Qué podrías decir sobre los productos de JGB?.png"), gg, width=12, height=7, units = "in", dpi=366)

# Pregunta 22
Pr <- db[,97]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable$measure <- gsub("^.*\\?\\s*", "", fqTable$measure, ignore.case = T)
# grafico
gg <- fqTable %>% 
  ggplot2::ggplot(aes(x = value, y = Porcentaje, fill=value)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single")) + 
  geom_text(aes(label = paste0(round(Porcentaje,2),"%")), size = 10, vjust = -0.1, position = position_dodge(0.9)) +
  ggtitle("¿Consideras que los productos de JGB son de buena calidad?") +
  xlab("") + 
  ylab("Porcentaje (%)") +
  # coord_flip() +
  scale_fill_brewer(palette="RdYlGn", name = " ") +
  theme_ggcharts() +
  ggplot2::theme(text = element_text(family="BombardGrotesk-Bold",size = 18),
                 # axis.text.x = element_blank(),
                 axis.line.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 strip.text = element_text(size = 18,face = "bold"),
                 axis.title.x = element_text(size = 18,face = 'bold'),
                 axis.text = element_text(size = 18),
                 legend.position = "none")
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/¿Consideras que los productos de JGB son de buena calidad?.jpeg"), gg, width=12, height=9, units = "in", dpi=366)

# Pregunta 23
Pr <- db[,98]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable$measure <- gsub("^.*\\?\\s*", "", fqTable$measure, ignore.case = T)

# -------------------------------------------------------------------------
# Pregunta 25
Pr <- db[,107:108]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()

# Pregunta 26
Pr <- db[,109:110]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()

# -------------------------------------------------------------------------
# Pregunta 27
Pr <- db[,111:114]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()


# Camino 1 ----------------------------------------------------------------
# Pregunta 28
Pr <- db[,116:121]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable

# Pregunta 29
Pr <- db[,123]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable

# Pregunta 30
Pr <- db[,124:129]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable

# Camino 2 ----------------------------------------------------------------
# Pregunta 31
Pr <- db[,131:136]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable

# Pregunta 32
Pr <- db[,138]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable

# Pregunta 33
Pr <- db[,139:144]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable

# -------------------------------------------------------------------------
# Pregunta 34
Pr <- db[,146]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable

# -------------------------------------------------------------------------
# Pregunta 35
Pr <- db[,147:156]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable
fqTable$measure <- gsub(".png*", "", fqTable$measure, ignore.case = T)

# -------------------------------------------------------------------------
# Pregunta 36
Pr <- db[,157:159]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable
fqTable$measure <- gsub("^.*\\?\\s*", "", fqTable$measure, ignore.case = T)
# grafico
gg <- fqTable %>% 
  ggplot2::ggplot(aes(x = measure, y = Porcentaje, fill=value)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single")) + 
  geom_text(aes(label = paste0(round(Porcentaje,2),"%")), hjust = -0.1, position = position_dodge(0.9), size=12) +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 55)) +
  scale_fill_brewer(palette="RdYlGn", name = " ") +
  theme_ggcharts() +
  ggplot2::theme(text = element_text(family="BombardGrotesk-Bold",size = 18),
                 # axis.text.x = element_blank(),
                 axis.line.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 strip.text = element_text(size = 18,face = "bold"),
                 axis.title.x = element_text(size = 18,face = 'bold'),
                 axis.text = element_text(size = 18),
                 legend.position = "bottom")
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/¿Cuál de los siguientes mensajes considera más apropiado a JGB?.png"), gg, width=18, height=12, units = "in", dpi=366)

# -------------------------------------------------------------------------
# Pregunta 37
Pr <- db[,160]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable

# Pregunta 38
Pr <- db[,162:164]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable$measure <- gsub("^.*\\?\\s*", "", fqTable$measure, ignore.case = T)
fqTable


# Publicidad --------------------------------------------------------------
# Pregunta 39
Pr <- db[,165:168]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable

# Pregunta 40
Pr <- db[,170:174]
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  tidyr::gather(measure, value) %>% 
  dplyr::group_by(., measure) %>% 
  dplyr::count(., value) %>% 
  dplyr::mutate(Porcentaje = round(n/nrow(Pr)*100,2)) %>% 
  tidyr::drop_na()
fqTable
