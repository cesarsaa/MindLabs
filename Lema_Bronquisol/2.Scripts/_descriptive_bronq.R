# -------------------------------------------------------------------------
# Proyecto: Lema bronquisol
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
prj <- "Lema_Bronquisol"
# Lectura de datos --------------------------------------------------------
db <- readxl::read_xlsx(paste0(root,prj,"/1.Data/data_base.xlsx"))
db <- db[!is.na(db$P5), ]
head(db)
# Demograficos ------------------------------------------------------------
# Pregunta 1
Pr <- db |> dplyr::select(P1)
round(prop.table(table(Pr))*100,1)
# Pregunta 2
Pr <- db |> dplyr::select(P2)
round(prop.table(table(Pr))*100,1)
# Pregunta 3
Pr <- db |> dplyr::select(P3)
round(prop.table(table(Pr))*100,1)
# Pregunta 4
Pr <- db |> dplyr::select(P4)
round(prop.table(table(Pr))*100,1)

# Medios publicitarios ----------------------------------------------------
# Pregunta 5
Pr <- db |> dplyr::select(P5)
# prop.table(table(Pr))*100
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>%
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr)) |> 
  tidyr::drop_na(); fqTable
#
gg <- ggplot(fqTable, aes(x = "", y = Porcentaje, fill = Categoria)) +
  geom_col(color = "black") +
  geom_label(aes(label = paste0(round(Porcentaje*100,1),"%")), color = c("white", "white"),
             position = position_stack(vjust = 0.5),
             show.legend = F) +
  guides(fill = guide_legend(title = " ")) +
  scale_fill_manual(values = c("steelblue", "#ff040b")) +
  coord_polar(theta = "y") + 
  theme_void()
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/P5.png"), gg, width=9, height=7, units = "in", dpi=366)

# Pregunta 6
Pr <- db |> dplyr::select(P6)
write.csv(Pr, paste0(root,prj,"/3.Results/P6.csv"))
prop.table(table(Pr))*100

# Pregunta 7
Pr <- db |> dplyr::select(P7:P7_4)
# prop.table(table(Pr))*100
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 9
Pr <- db |> dplyr::select(P9:P9_3)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr)) |> 
  tidyr::drop_na(); fqTable

# Pregunta 10
Pr <- db |> dplyr::select(P10:P10_3)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = Frecuencia/nrow(Pr)) |> 
  tidyr::drop_na(); fqTable

# Evaluacion Lema ---------------------------------------------------------
# Pregunta 11
Pr <- db |> dplyr::select(P11)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable
#
gg <- fqTable %>% 
  ggplot2::ggplot(aes(x = reorder((Categoria), +Porcentaje), y = Porcentaje)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single"),fill="#849DB1") + #dde9f7, #849DB1
  geom_text(aes(label = paste0(round(Porcentaje,2),"%")), hjust = -0.1, position = position_dodge(0.9),size = 12) +
  ggtitle("") +
  xlab("") + 
  ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 80)) +
  theme_gray() +
  ggplot2::theme(text = element_text(family="BombardGrotesk-Bold",size = 18),
                 axis.line.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 strip.text = element_text(size = 18,face = "bold"),
                 axis.title.x = element_text(size = 18,face = 'bold'),
                 axis.text = element_text(size = 18),
                 legend.position = "bottom")
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/P11.png"), gg, width=12, height=9, units = "in", dpi=366, device = png)

# Pregunta 12
Pr <- db |> dplyr::select(P12)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

# Pregunta 13
Pr <- db |> dplyr::select(P13)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable
#
gg <- fqTable %>% 
  ggplot2::ggplot(aes(x = reorder((Categoria), +Porcentaje), y = Porcentaje)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single"),fill="#849DB1") + #dde9f7, #849DB1
  geom_text(aes(label = paste0(round(Porcentaje,2),"%")), hjust = -0.1, position = position_dodge(0.9),size = 12) +
  ggtitle("") +
  xlab("") + 
  ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 90)) +
  theme_gray() +
  ggplot2::theme(text = element_text(family="BombardGrotesk-Bold",size = 18),
                 axis.line.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 strip.text = element_text(size = 18,face = "bold"),
                 axis.title.x = element_text(size = 18,face = 'bold'),
                 axis.text = element_text(size = 18),
                 legend.position = "bottom")
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/P13.png"), gg, width=12, height=9, units = "in", dpi=366, device = png)

# Pregunta 14
Pr <- db |> dplyr::select(P14:P14_6)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

# Pregunta 15
Pr <- db |> dplyr::select(P15:P15_6)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

# Pregunta 17
Pr <- db |> dplyr::select(P17)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable
#
gg <- fqTable %>% 
  ggplot2::ggplot(aes(x = reorder((Categoria), +Porcentaje), y = Porcentaje)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single"),fill="#849DB1") + #dde9f7, #849DB1
  geom_text(aes(label = paste0(round(Porcentaje,2),"%")), hjust = -0.1, position = position_dodge(0.9),size = 12) +
  ggtitle("") +
  xlab("") + 
  ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 90)) +
  theme_gray() +
  ggplot2::theme(text = element_text(family="BombardGrotesk-Bold",size = 18),
                 axis.line.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 strip.text = element_text(size = 18,face = "bold"),
                 axis.title.x = element_text(size = 18,face = 'bold'),
                 axis.text = element_text(size = 18),
                 legend.position = "bottom")
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/P17.png"),gg,width=12, height=9, units = "in", dpi=366)

# Pregunta 18
Pr <- db |> dplyr::select(P18:P18_5)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

# Pregunta 19
Pr <- db |> dplyr::select(P19)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

# Pregunta 20
Pr <- db |> dplyr::select(P20)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable


# Pregunta 21
Pr <- db |> dplyr::select(P21)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable
fqTable$Categoria <- as.factor(fqTable$Categoria)
#
gg <- fqTable %>% 
  ggplot(aes(x = reorder(Categoria, +Porcentaje), y = Porcentaje, fill=Categoria)) +
  geom_col(position = position_dodge2(preserve = "single"),show.legend = F) + #dde9f7, #849DB1
  geom_text(aes(label = paste0(round(Porcentaje,2),"%")), hjust = -0.1, position = position_dodge(0.9),size = 12) +
  ggtitle("") +
  xlab("") + 
  ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_brewer(palette="RdYlGn", direction = 1)+
  scale_y_continuous(limits = c(0, 80)) +
  theme_gray() +
  ggplot2::theme(text = element_text(family="BombardGrotesk-Bold",size = 18),
                 axis.line.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 strip.text = element_text(size = 18,face = "bold"),
                 axis.title.x = element_text(size = 18,face = 'bold'),
                 axis.text = element_text(size = 18),
                 legend.position = "bottom")
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/P20.png"), gg, width=18, height=12, units = "in", dpi=366, device = png)

# Pregunta 23
Pr <- db |> dplyr::select(P23)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

# Pregunta 27
Pr <- db |> dplyr::select(P27)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable
#
gg <- fqTable %>% 
  ggplot(aes(x = reorder(Categoria, +Porcentaje), y = Porcentaje)) +
  geom_col(position = position_dodge2(preserve = "single"),show.legend = F, fill="#849DB1") + #dde9f7, #849DB1
  geom_text(aes(label = paste0(round(Porcentaje,2),"%")), hjust = -0.1, position = position_dodge(0.9),size = 12) +
  ggtitle("") +
  xlab("") + 
  ylab("Porcentaje (%)") +
  coord_flip() +
  # scale_fill_brewer(palette="RdYlBu", direction = 1)+
  scale_y_continuous(limits = c(0, 80)) +
  theme_gray() +
  ggplot2::theme(text = element_text(family="BombardGrotesk-Bold",size = 18),
                 axis.line.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 strip.text = element_text(size = 18,face = "bold"),
                 axis.title.x = element_text(size = 18,face = 'bold'),
                 axis.text = element_text(size = 18),
                 legend.position = "bottom")
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/P27.png"), gg, width=18, height=12, units = "in", dpi=366, device = png)

# Pregunta 28
Pr <- db |> dplyr::select(P28)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

# Pregunta 29
Pr <- db |> dplyr::select(P29)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable


# Imagen comercial --------------------------------------------------------
# Pregunta 32
Pr <- db |> dplyr::select(P32)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

# Pregunta 35
Pr <- db |> dplyr::select(P35)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

# Pregunta 36
Pr <- db |> dplyr::select(P36)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable
# -------------------------------------------------------------------------
# Pregunta 38
Pr <- db |> dplyr::select(P38)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable

# Pregunta 40
Pr <- db |> dplyr::select(P34)
fqTable <- Pr |> 
  gather(measure, value) |> 
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) |> 
  tidyr::drop_na(); fqTable
