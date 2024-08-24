# -------------------------------------------------------------------------
# Proyecto: JGB, Floucardent
# By:  Cesar A. Saavedra
# Mindlabs
# Agosto 2024

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

db <- db %>% dplyr::mutate(REGION = dplyr::case_when(REGION	%in% c("Barranquilla")	~	"Barranquilla",
                                                     REGION	%in% c("Bogotá")	~	"Bogotá, D.C.",
                                                     REGION	%in% c("Cali ")	~	"Cali",
                                                     REGION	%in% c("Medellín")	~	"Medellín"))

# Analisis descriptivo ----------------------------------------------------
# Pregunta 2 -------------------------------------------------------------
Pr2 <- db %>% dplyr::select(REGION)
Pr2 %>% glimpse
# Analisis descriptivo
fqTable <- Pr2 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr2))*100) %>% 
  tidyr::drop_na()

#
shp <- terra::vect(paste0(root,"Shp_colombia/col_admbnda_adm2.shp"))
col <- sf::st_as_sf(shp)
data1 <- dplyr::inner_join(col, fqTable, by = c('ADM2_ES' = 'Categoria'))

#
gg <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = sf::st_as_sf(data1), aes(fill = Porcentaje), col = 'grey50') +
  # ggplot2::scale_fill_gradientn(colors = brewer.pal(n = 3, name = 'YlOrRd')) +
  ggplot2::geom_sf(data = sf::st_as_sf(col), fill = NA, col = 'grey20') +
  ggplot2::coord_sf() +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = 'bottom',
                 axis.text.y = element_text(angle = 90, hjust = 0.5))

ggplot2::ggsave(paste0(root,prj,"/3.Results/map.png"), 
                gg,width=10, height=7, units = "in", dpi=360)
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
                   aes(y = pos, label = paste0(round(Porcentaje,2), "%")),
                   size = 5.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title =" ")) +
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_2.png"), gg, width=9, height=7, units = "in", dpi=366)
rm(df2)
# Pregunta 3 --------------------------------------------------------------
Pr3 <- db %>% dplyr::select(SEL)
Pr3 %>% glimpse

# Analisis descriptivo
fqTable <- Pr3 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr3)*100)) %>% 
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
                   aes(y = pos, label = paste0(round(Porcentaje,2), "%")),
                   size = 5.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title =" ")) +
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_3.png"), gg, width=9, height=7, units = "in", dpi=366)
rm(df2)
# Pregunta 4 --------------------------------------------------------------
Pr4 <- db %>% dplyr::select(GENDER)
Pr4 %>% glimpse
# Analisis descriptivo
fqTable <- Pr4 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr4)) %>% 
  tidyr::drop_na()

gg <- ggplot(fqTable, aes(x = "", y = Porcentaje, fill = Categoria)) +
  geom_col(color = "black") +
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%")), color = c("white", "white"),
             position = position_stack(vjust = 0.5),
             show.legend = F) +
  guides(fill = guide_legend(title = " ")) +
  scale_fill_manual(values = c("steelblue", "#f41c5c")) +
  coord_polar(theta = "y") + 
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_4.png"), gg, width=9, height=7, units = "in", dpi=366)
rm(df2)

# Pregunta 5 --------------------------------------------------------------
Pr3 <- db %>% dplyr::select(AGE_RANGE,GENDER)
Pr3 %>% glimpse

# Analisis descriptivo
fqTable <- Pr3 %>% 
  # group_by(., GENDER) %>% 
  count(AGE_RANGE) %>% 
  dplyr::mutate(Porcentaje = (prop.table(n)*100))

fqTable <- fqTable %>%
  mutate(Porcentaje_total = ifelse(GENDER == "Masculino",
                                   -Porcentaje, Porcentaje),
         Porcentaje_total = ifelse(GENDER == "Masculino", -Porcentaje, Porcentaje))

gg <- fqTable %>% 
  ggplot(aes(x = AGE_RANGE,
             y = Porcentaje_total, fill = GENDER)) +
  geom_col(position = "stack", alpha = 0.6) + 
  coord_flip() +
  scale_fill_manual(values = c("midnightblue", "darkred")) +
  theme_ggcharts() +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0)) +
  scale_y_continuous(labels = abs) +
  labs(y = " ",
       x = " ",
       title = " ", 
       subtitle = " ") +
  guides(fill=guide_legend(title=" "))

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_edad.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 6 --------------------------------------------------------------
Pr6 <- db %>% dplyr::select(F6_1:F6_5)
Pr6 %>% glimpse
# Analisis descriptivo
fqTable <- Pr6 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr6))*100) %>% 
  tidyr::drop_na()

gg <- fqTable %>%
  lollipop_chart(x = Categoria, y = Porcentaje) +
  geom_text(aes(label = paste0(round(Porcentaje,2),"%"), hjust = 1.2, vjust = -1), color = "black") +
  labs(x = NULL,
       y = " ",
       title = " ") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, .05)))

gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  geom_text(aes(label = paste0(Porcentaje,"%"), hjust = 1.2), color = "black") +
  # scale_y_continuous(expand = expansion()) +
  labs(x = NULL,
       y = "Porcentaje (%)",
       title = " ",
       subtitle = " ",
       caption = " ") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_6.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 7 --------------------------------------------------------------
# Top of mind
Pr7 <- db %>% dplyr::select(Q1_1) #Q1_1,Q1_2,Q1_3
Pr7 %>% glimpse

docs <- Corpus(VectorSource(Pr7))
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
# remover signos de puntuación
docs <- tm_map(docs, removePunctuation)
# Eliminar espacios en blanco extras.
docs <- tm_map(docs, stripWhitespace)
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
wordcloud(words = d$word, freq = d$freq, scale = , min.freq = 1,
          max.words=500, random.order=F, rot.per=0.1, 
          colors=brewer.pal(8, "Paired"),
          family="serif")

# Pregunta 8 --------------------------------------------------------------
Pr8 <- db %>% dplyr::select(Q2)
Pr8 %>% glimpse
# Analisis descriptivo
fqTable <- Pr8 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr8)) %>% 
  tidyr::drop_na()

fqTable$frame=rep('a',2)
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_8.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 9 --------------------------------------------------------------
Pr9 <- db %>% dplyr::select(Q3_1,Q3_2,Q3_3)
Pr9 %>% glimpse

docs <- Corpus(VectorSource(Pr9))
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
# remover signos de puntuación
docs <- tm_map(docs, removePunctuation)
# Eliminar espacios en blanco extras.
docs <- tm_map(docs, stripWhitespace)
#crear matriz documento de términos
dtm <- TermDocumentMatrix(docs)
matriz <- as.matrix(dtm)
# ordenar filas de la matriz en orden descendente
v <- sort(rowSums(matriz),decreasing=TRUE)
# convertir a data frame
d <- data.frame(word = names(v),freq=v)
# mostrar los primeros 10 términos que más se repiten
head(d, 10)
write.csv(d, paste0(root,prj,"/3.Results/Pregunta_9.csv"))
#
set.seed(4321)
wordcloud(words = d$word, freq = d$freq, scale = , min.freq = 2,
          max.words=50, random.order=F, rot.per=0.1, 
          colors=brewer.pal(8, "Paired"),
          family="serif")

# Pregunta 10 --------------------------------------------------------------
Pr10 <- db %>% dplyr::select(Q4_1:Q4_8)
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
  # bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  bar_chart(Categoria, Porcentaje, highlight = c("Fluocardent  "), bar_color = c("steelblue")) +
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
Pr11 <- db %>% dplyr::select(Q5_1:Q5_8)
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
gg <- fqTable %>% dplyr::filter(., Categoria != "${q://QID11/ChoiceTextEntryValue/8}") %>% 
  # bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  bar_chart(Categoria, Porcentaje, highlight = c("Fluocardent  "), bar_color = c("steelblue")) +
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_11.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 12 --------------------------------------------------------------
Pr12 <- db %>% dplyr::select(Q6)
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
  # bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  bar_chart(Categoria, Porcentaje, highlight = c("Fluocardent  "), bar_color = c("steelblue")) +
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_12.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 12-A --------------------------------------------------------------
Pr <- db %>% dplyr::select(Q8_1:Q8_12)
Pr %>% glimpse
# Analisis descriptivo
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr)) %>% 
  tidyr::drop_na()
#
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  # bar_chart(Categoria, Porcentaje, highlight = c("Fluocardent  "), bar_color = c("steelblue")) +
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_12A.png"), gg, width=9, height=7, units = "in", dpi=366)

#
Pr <- db %>% dplyr::select(Q6, Q8_1:Q8_12)

fqTable <- Pr %>% dplyr::group_by(., Q6) %>% 
  tidyr::pivot_longer(cols = starts_with("Q8_"), 
                      names_to = "Question", 
                      values_to = "Response") %>% 
  dplyr::count(Question, Response) %>% tidyr::drop_na() %>% 
  dplyr::mutate(Porcentaje = prop.table(n))

gg <- fqTable %>% dplyr::filter(., Q6 == c("Fluocardent  ")) %>% 
  ggplot(aes(x = reorder(Response, +Porcentaje), y = Porcentaje*100, fill=Q6)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left")) +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  facet_wrap(~Q6, ncol = 2) +
  scale_fill_brewer(palette="Set3", direction = 1)+
  # scale_y_continuous(limits = c(0, 60)) +
  theme_ggcharts() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="none")

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_12_1.png"), gg, width=20, height=12, units = "in", dpi=366)

# Pregunta 13 --------------------------------------------------------------
Pr13 <- db %>% dplyr::select(Q9)
Pr13 %>% glimpse
# Analisis descriptivo
fqTable <- Pr13 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr13)) 
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Muy a menudo",
                                                          "A menudo",
                                                          "De vez en cuando",
                                                          "Raramente",
                                                          "Nunca"), ordered = T)
#
lvl <- c("Muy a menudo",
         "A menudo",
         "De vez en cuando",
         "Raramente",
         "Nunca")
# 
gg <- fqTable %>% ggplot(aes(x = factor(Categoria, level=lvl), y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = -0.1)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="RdYlGn",direction = -1)+
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_13.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 14 --------------------------------------------------------------
Pr9 <- db %>% dplyr::select(Q10)
Pr9 %>% glimpse

docs <- Corpus(VectorSource(Pr9))
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
# remover signos de puntuación
docs <- tm_map(docs, removePunctuation)
# Eliminar espacios en blanco extras.
docs <- tm_map(docs, stripWhitespace)
#crear matriz documento de términos
dtm <- TermDocumentMatrix(docs)
matriz <- as.matrix(dtm)
# ordenar filas de la matriz en orden descendente
v <- sort(rowSums(matriz),decreasing=TRUE)
# convertir a data frame
d <- data.frame(word = names(v),freq=v)
# mostrar los primeros 10 términos que más se repiten
head(d, 10)
write.csv(d, paste0(root,prj,"/3.Results/Pregunta_14.csv"))
#
set.seed(4321)
wordcloud(words = d$word, freq = d$freq, scale = , min.freq = 2,
          max.words=50, random.order=F, rot.per=0.1, 
          colors=brewer.pal(8, "Paired"),
          family="serif")

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
  geom_label(aes(label = paste0(Porcentaje*100,"%"), hjust = 1.2)) +
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
  geom_label(aes(label = paste0(Porcentaje*100,"%"), hjust = 1.2)) +
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
write.csv(Pr17, paste0(root,prj,"/3.Results/Pregunta_17.csv"))
# Pregunta 15 ----------------------------------------------------------
Pr15 <- db %>% dplyr::select(Q18_1:Q18_8)
Pr15 %>% glimpse
# Analisis descriptivo
fqTable <- Pr15 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr15)) %>% 
  tidyr::drop_na()

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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_15.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 16 --------------------------------------------------------------
Pr16 <- db %>% dplyr::select(Q11_1:Q11_8)
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
  bar_chart(Categoria, Porcentaje, highlight = c("Salud general y bienestar"), bar_color = c("steelblue")) +
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_16.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 17 --------------------------------------------------------------
Pr9 <- db %>% dplyr::select(Q12)
Pr9 %>% glimpse

docs <- Corpus(VectorSource(Pr9))
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
# remover signos de puntuación
docs <- tm_map(docs, removePunctuation)
# Eliminar espacios en blanco extras.
docs <- tm_map(docs, stripWhitespace)
#crear matriz documento de términos
dtm <- TermDocumentMatrix(docs)
matriz <- as.matrix(dtm)
# ordenar filas de la matriz en orden descendente
v <- sort(rowSums(matriz),decreasing=TRUE)
# convertir a data frame
d <- data.frame(word = names(v),freq=v)
# mostrar los primeros 10 términos que más se repiten
head(d, 10)
write.csv(d, paste0(root,prj,"/3.Results/Pregunta_14.csv"))
#
set.seed(4321)
wordcloud(words = d$word, freq = d$freq, scale = , min.freq = 2,
          max.words=500, random.order=F, rot.per=0.1, 
          colors=brewer.pal(8, "Paired"),
          family="serif")

# Pregunta 18 -------------------------------------------------------------
Pr18 <- db %>% dplyr::select(Q13)
Pr18 %>% glimpse
# Analisis descriptivo
fqTable <- Pr18 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr18)) 
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("1",
                                                          "2",
                                                          "3",
                                                          "4",
                                                          "5"), ordered = T)
fqTable <- fqTable %>% dplyr::mutate(Categoria = dplyr::case_when(Categoria == 1 ~ "1",
                                                                  Categoria == 2 ~ "2",
                                                                  Categoria == 3 ~ "3",
                                                                  Categoria == 4 ~ "4",
                                                                  Categoria == 5 ~ "5"))
#
lvl <- c("1",
         "4",
         "3",
         "2",
         "5")
# 
gg <- fqTable %>% ggplot(aes(x = order(Categoria, +Porcentaje), y = Porcentaje*100, fill=Categoria)) +
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_18.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 19 ----------------------------------------------------------
Pr9 <- db %>% dplyr::select(Q14)
Pr9 %>% glimpse

docs <- Corpus(VectorSource(Pr9))
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
# remover signos de puntuación
docs <- tm_map(docs, removePunctuation)
# Eliminar espacios en blanco extras.
docs <- tm_map(docs, stripWhitespace)
#crear matriz documento de términos
dtm <- TermDocumentMatrix(docs)
matriz <- as.matrix(dtm)
# ordenar filas de la matriz en orden descendente
v <- sort(rowSums(matriz),decreasing=TRUE)
# convertir a data frame
d <- data.frame(word = names(v),freq=v)
# mostrar los primeros 10 términos que más se repiten
head(d, 10)
write.csv(d, paste0(root,prj,"/3.Results/Pregunta_14.csv"))
#
set.seed(4321)
wordcloud(words = d$word, freq = d$freq, scale = , min.freq = 2,
          max.words=500, random.order=F, rot.per=0.1, 
          colors=brewer.pal(8, "Paired"),
          family="serif")
# Pregunta 20 -------------------------------------------------------------
Pr9 <- db %>% dplyr::select(Q15)
Pr9 %>% glimpse

docs <- Corpus(VectorSource(Pr9))
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
# remover signos de puntuación
docs <- tm_map(docs, removePunctuation)
# Eliminar espacios en blanco extras.
docs <- tm_map(docs, stripWhitespace)
#crear matriz documento de términos
dtm <- TermDocumentMatrix(docs)
matriz <- as.matrix(dtm)
# ordenar filas de la matriz en orden descendente
v <- sort(rowSums(matriz),decreasing=TRUE)
# convertir a data frame
d <- data.frame(word = names(v),freq=v)
# mostrar los primeros 10 términos que más se repiten
head(d, 10)
write.csv(d, paste0(root,prj,"/3.Results/Pregunta_14.csv"))
#
set.seed(4321)
wordcloud(words = d$word, freq = d$freq, scale = , min.freq = 2,
          max.words=500, random.order=F, rot.per=0.1, 
          colors=brewer.pal(8, "Paired"),
          family="serif")

# Pregunta 21 -------------------------------------------------------------
Pr21 <- db %>% dplyr::select(Q16_1:Q16_11)
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
gg <- fqTable %>%
  bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_26.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 23 -------------------------------------------------------------
Pr23 <- db %>% dplyr::select(Q18_1.0,Q18_2.0,Q18_3.0,Q18_4.0,Q18_5.0,
                             Q18_6.0,Q18_7.0,Q18_8.0,Q18_9.0)
Pr23 %>% glimpse

names(Pr23) <- c("Me gustó la idea de comercial que acabo de ver",
                "Me parece que el mensaje del comercial es claro",
                "El comercial dice cosas relevantes (importantes para mi)",
                "El comercial me motiva a comprar ese producto",
                "Te parece creíble el mensaje de la campaña",
                "Sientes confianza en la marca fluocardent \n después de ver el animatic",
                "El comercial es diferente a los de otras marcas ",
                "El comercial es apropiado para fluocardent ",
                "Es claro que Fluocardent es una marca de JGB")

# Analisis descriptivo
fqTable <- Pr23 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr23)) 

# 
gg <- fqTable %>% ggplot(aes(x = Categoria, y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = -0.1)) +
  # geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="RdYlGn", direction = 1)+
  scale_y_continuous(limits = c(0, 50)) +
  facet_wrap(~ Variable) +
  # theme_ggcharts() +
  theme_void() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="none")

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_23.png"), gg, width=25, height=9, units = "in", dpi=366)

# Pregunta 24 -------------------------------------------------------------
Pr24 <- db %>% dplyr::select(Q19)
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_24.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 25 -------------------------------------------------------------
Pr25 <- db %>% dplyr::select(Q20_1:Q20_7)
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_25.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 26 -------------------------------------------------------------
Pr26 <- db %>% dplyr::select(Q21_1:Q21_9)
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_26.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 27 --------------------------------------------------------------
Pr27 <- db %>% dplyr::select(Q22)
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
Pr28 <- db %>% dplyr::select(Q23_1:Q23_8)
Pr28 %>% glimpse
# Analisis descriptivo
fqTable <- Pr28 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr28)) %>% drop_na()

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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_28.png"), gg, width=13, height=7, units = "in", dpi=366)

# Pregunta 29 --------------------------------------------------------------
Pr29 <- db %>% dplyr::select(Q24)
Pr29 %>% glimpse
# Analisis descriptivo
fqTable <- Pr29 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr29)) 

# 
gg <- fqTable %>% ggplot(aes(x = factor(Categoria, level=lvl), y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="RdYlGn")+
  scale_y_continuous(limits = c(0, 100)) +
  # facet_wrap(~ Variable, ) +
  theme_ggcharts() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_29.png"), gg, width=13, height=7, units = "in", dpi=366)

Pr29 <- db %>% dplyr::select(Q24_1_TEXT)
Pr29 %>% glimpse

write.csv(Pr29, paste0(root,prj,"/3.Results/Pregunta_29.csv"))

# Pregunta 30 --------------------------------------------------------------
Pr30 <- db %>% dplyr::select(Q25)
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
df2 <- fqTable %>% 
  mutate(csum = rev(cumsum(rev(Porcentaje))), 
         pos = Porcentaje/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Porcentaje/2, pos))

gg <- ggplot(fqTable, aes(x = "" , y = Porcentaje, fill = fct_inorder(Categoria))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(Porcentaje,2), "%")),
                   size = 5.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title =" ")) +
  theme_void()

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_30.png"), gg, width=9, height=7, units = "in", dpi=366)
rm(df2)

Pr30 <- db %>% dplyr::select(Q25_4_TEXT)
write.csv(Pr29, paste0(root,prj,"/3.Results/Pregunta_30.csv"))

# Pregunta 31 --------------------------------------------------------------
Pr31 <- db %>% dplyr::select(Q26_1:Q26_9)
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_31.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 32 --------------------------------------------------------------
Pr32 <- db %>% dplyr::select(Q27)
Pr32 %>% glimpse
# Analisis descriptivo
fqTable <- Pr32 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr32))
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Muy adecuada ",
                                                          "Adecuada",
                                                          "Neutral",
                                                          "Poco adecuada",
                                                          "Nada adecuada"), ordered = T)
#
gg <- fqTable %>% ggplot(aes(x = reorder(Categoria, +Porcentaje), y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = -0.1)) +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="RdYlGn", direction = -1)+
  scale_y_continuous(limits = c(0, 60)) +
  # facet_wrap(~ Variable, ) +
  theme_ggcharts() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_32.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 33 --------------------------------------------------------------
Pr33 <- db %>% dplyr::select(Q28)
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_33.png"), gg, width=17, height=7, units = "in", dpi=366)

# Pregunta 34 --------------------------------------------------------------
Pr34 <- db %>% dplyr::select(Q29)
Pr34 %>% glimpse
# Analisis descriptivo
fqTable <- Pr34 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr34)) 
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Totalmente de acuerdo",
                                                          "De acuerdo",
                                                          "Neutral",
                                                          "En desacuerdo",
                                                          "Totalmente en desacuerdo"), ordered = T)
lvl <- c("Totalmente de acuerdo",
         "De acuerdo",
         "Neutral",
         "En desacuerdo",
         "Totalmente en desacuerdo")
# 
gg <- fqTable %>% ggplot(aes(x = reorder(factor(Categoria, level=lvl), +Porcentaje), y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left")) +
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_34.png"), gg, width=13, height=7, units = "in", dpi=366)

# Pregunta 35 --------------------------------------------------------------
Pr35 <- db %>% dplyr::select(Q30)
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_35.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 36 --------------------------------------------------------------
Pr36 <- db %>% dplyr::select(Q31_1:Q31_8)
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_36.png"), gg, width=20, height=7, units = "in", dpi=366)

# Pregunta 37 --------------------------------------------------------------
Pr37 <- db %>% dplyr::select(Q32_1:Q32_10)
Pr37 %>% glimpse
# Analisis descriptivo
fqTable <- Pr37 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr37)) 

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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_37.png"), gg, width=13, height=7, units = "in", dpi=366)

# Pregunta 38 -------------------------------------------------------------
Pr38 <- db %>% dplyr::select(Q33)
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_38.png"), gg, width=20, height=12, units = "in", dpi=366)

# Pregunta 39 -------------------------------------------------------------
Pr39 <- db %>% dplyr::select(Q34_1:Q34_7)
Pr39 %>% glimpse
# Analisis descriptivo
fqTable <- Pr39 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr39)) %>% 
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_39.png"), gg, width=20, height=12, units = "in", dpi=366)

# Pregunta 39 -------------------------------------------------------------
Pr40 <- db %>% dplyr::select(Q35)
write.csv(Pr40, paste0(root,prj,"/3.Results/Pregunta_40.csv"))
# Pregunta --------------------------------------------------------------
Pr <- db %>% dplyr::select(P39,P40,P42:P45)
Pr %>% glimpse

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
lvl <- c("Excelente",
         "Buena",
         "Regular",
         "Mala",
         "Muy mala")
# 
gg <- fqTable %>% ggplot(aes(x = reorder(factor(Categoria, level=lvl), +Porcentaje), y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_text(aes(label = paste0(Porcentaje*100,"%"), hjust = "left"), color = "black") +
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
        legend.position="bottom")

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_39-45.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 41 -------------------------------------------------------------
Pr41 <- db %>% dplyr::select(Q36_1:Q36_5)
Pr41 %>% glimpse
# Analisis descriptivo
fqTable <- Pr41 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr41)) %>% drop_na()

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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_41.png"), gg, width=15, height=7, units = "in", dpi=366)


# Pregunta 42 -------------------------------------------------------------
Pr42 <- db %>% dplyr::select(Q37)
Pr42 %>% glimpse
# Analisis descriptivo
fqTable <- Pr42 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr42)) 
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("1",
                                                          "2",
                                                          "3",
                                                          "4",
                                                          "5"), ordered = T)
lvl <- c("1",
         "2",
         "3",
         "4",
         "5")
# 
gg <- fqTable %>% ggplot(aes(x = ((Categoria)), y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = "left")) +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="RdYlGn", direction = 1)+
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_42.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 43 -------------------------------------------------------------
Pr43 <- db %>% dplyr::select(Q38_1:Q38_4)
Pr43 %>% glimpse
# Analisis descriptivo
fqTable <- Pr43 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr43)) %>% drop_na()

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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_43.png"), gg, width=20, height=12, units = "in", dpi=366)

# Pregunta 44 -------------------------------------------------------------
Pr44 <- db %>% dplyr::select(Q39)
Pr44 %>% glimpse
# Analisis descriptivo
fqTable <- Pr44 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr44))
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Muy positiva",
                                                          "Positiva",
                                                          "Neutral",
                                                          "Negativa",
                                                          "Muy negativa"), ordered = T)
#
gg <- fqTable %>% ggplot(aes(x = reorder(Categoria, +Porcentaje), y = Porcentaje*100, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F) + 
  geom_label(aes(label = paste0(round(Porcentaje*100,2),"%"), hjust = -0.1)) +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="RdYlGn", direction = -1)+
  scale_y_continuous(limits = c(0, 60)) +
  # facet_wrap(~ Variable, ) +
  theme_ggcharts() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank())

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_44.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 45 -------------------------------------------------------------
Pr45 <- db %>% dplyr::select(Q40_1:Q40_10)
Pr45 %>% glimpse
# Analisis descriptivo
fqTable <- Pr45 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr45)) %>% 
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_45.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 46 --------------------------------------------------------------
Pr46 <- db %>% dplyr::select(Q41)
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
gg <- fqTable %>%
  # bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  bar_chart(Categoria, Porcentaje, highlight = c("Sí, aporta credibilidad",
                                                 "Si, los productos de JGB son buenos",
                                                 "Sí, aporta confianza en la calidad del producto"), bar_color = c("steelblue","steelblue","steelblue")) +
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_46.png"), gg, width=9, height=7, units = "in", dpi=366)
rm(df2)

# Pregunta 47 -------------------------------------------------------------
Pr47 <- db %>% dplyr::select(Q42)
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
  # bar_chart(Categoria, Porcentaje, bar_color = c("steelblue")) +
  bar_chart(Categoria, Porcentaje, highlight = c("Perdería Fluocardent porque es menos conocida ",
                                                 "La campaña perdería credibilidad",
                                                 "Me sentiría menos confiado/a en el producto"), 
            bar_color = c("steelblue","steelblue","steelblue")) +
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_47.png"), gg, width=15, height=7, units = "in", dpi=366)
# Pregunta 48 --------------------------------------------------------------
Pr48 <- db %>% dplyr::select(Q43_1:Q43_12)
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_48.png"), gg, width=20, height=12, units = "in", dpi=366)
rm(df2)
# Pregunta 49 -------------------------------------------------------------
Pr49 <- db %>% dplyr::select(Q44)
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
Pr50 <- db %>% dplyr::select(Q45_1:Q45_7)
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_50.png"), gg, width=15, height=7, units = "in", dpi=366)

# Pregunta 51 -------------------------------------------------------------
Pr51 <- db %>% dplyr::select(Q46)
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
gg <- fqTable %>% dplyr::filter(., Categoria != "${q://QID55/ChoiceTextEntryValue/7}") %>% 
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_51.png"), gg, width=15, height=7, units = "in", dpi=366)


# Pregunta 52 -------------------------------------------------------------
Pr52 <- db %>% dplyr::select(Q47)
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
Pr53 <- db %>% dplyr::select(Q48_1:Q48_7)
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

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_53.png"), gg, width=15, height=7, units = "in", dpi=366)
