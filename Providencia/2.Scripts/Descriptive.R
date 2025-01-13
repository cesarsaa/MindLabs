
# -------------------------------------------------------------------------
# Proyecto: Providencia
# By:  Cesar A. Saavedra
# Mindlabs
# Diciembre 2024
# R options ---------------------------------------------------------------
g <- gc(reset = T); rm(list = ls())
options(warn = -1, scipen = 999)
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(dplyr, tidyr, ggplot2, ggh4x, ggrepel, foreign, ggcharts, mdthemes,forcats,
                                readxl, raster, geodata, terra, sf, sp, lubridate))

# Ruta de trabajo ---------------------------------------------------------
root <- "/Users/cesara.saavedravanegas/Documents/GitHub/MindLabs/"
prj <- "Providencia"
# Lectura de datos --------------------------------------------------------
db <- readxl::read_xlsx(paste0(root,prj,"/1.Data/Estudio_i_marca.xlsx"))
db <- readxl::read_xlsx("/Users/cesara.saavedravanegas/Desktop/Estudio\ imagen\ de\ marca_2.xlsx")
head(db)

# Demograficos ------------------------------------------------------------
# Pregunta 1
Pr <- db |> dplyr::select(p1)
round(prop.table(table(Pr))*100,1)

# Pregunta 2
Pr <- db |> dplyr::select(p2)
round(prop.table(table(Pr))*100,1)

# Pregunta 3
Pr <- db |> dplyr::select(p3)
round(prop.table(table(Pr))*100,1)

# Pregunta 4
Pr <- db |> dplyr::select(p4)
round(prop.table(table(Pr))*100,1)

# Pregunta 5
Pr <- db |> dplyr::select(p5)
round(prop.table(table(Pr))*100,1)

# Pregunta 5
Pr <- db |> dplyr::select(p12)
round(prop.table(table(Pr))*100,1)

# Pregunta 
Pr <- db |> dplyr::select(p19)
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> tidyr::drop_na() |> 
  dplyr::mutate(Porcentaje = (Frecuencia/sum(Frecuencia))*100); fqTable

# Pregunta 
Pr <- db |> dplyr::select(p14, p19) |> 
  dplyr::filter(p14 == "Providencia") |> #Bogotá, Providencia, Mayagüez, Manuelita, RioPaila
  dplyr::select(p19)
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> tidyr::drop_na() |> 
  dplyr::mutate(Porcentaje = (Frecuencia/sum(Frecuencia))*100); fqTable
# write.csv(fqTable, "data.csv")
fqTable$Categoria <- gsub("https:.*", "", fqTable$Categoria)

# Pregunta 
Pr <- db |> dplyr::select(p14,p1) |> 
  dplyr::filter(p14 == "Manuelita") |> dplyr::select(p19) #Providencia
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable |> tidyr::drop_na() |> 
  dplyr::mutate(Porcentaje = (Frecuencia/sum(Frecuencia))*100); fqTable

fqTable$Categoria <- gsub("https:.*", "", fqTable$Categoria)
# -------------------------------------------------------------------------

fqTable %>% 
  ggplot2::ggplot(aes(x = reorder(Categoria, +Porcentaje), y = Porcentaje, fill=Categoria)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single")) + 
  geom_text(aes(label = paste0(round(Porcentaje,2),"%")), size = 10, hjust = -0.1, position = position_dodge(0.9)) +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 40)) +
  # scale_fill_brewer(palette="Blues", name = " ") +
  scale_fill_manual(values = c("#3182bd","#3182bd",
                               "#3182bd","#3182bd","#3182bd","#3182bd",
                               "#3182bd","#3182bd","#3182bd")) +
  # theme_ggcharts() +
  theme_bw() +
  ggplot2::theme(text = element_text(family="BombardGrotesk-Bold",size = 18),
                 # axis.text.x = element_blank(),
                 axis.line.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 strip.text = element_text(size = 18,face = "bold"),
                 axis.title.x = element_text(size = 18,face = 'bold'),
                 axis.text = element_text(size = 18),
                 legend.position = "none")

# -------------------------------------------------------------------------
# Analisis de Correspondencia Multiple

df <- db |> dplyr::select(p21_1:p21_2) |> drop_na()

colnames(df) <- c("Marca Confiable",
                  "Marca innovadora"),
                  "Marca que endulza mejor",
                  "Marca con amplia trayectoria",
                  "Marca Experta",
                  "Es la marca que me ofrece mejor relación costo /beneficio",
                  "Marca Cercana",
                  "Es la Marca líder en el mercado",
                  "Marca Lejana",
                  "Marca Nueva",
                  "Es una marca reconocida",
                  "Es una marca de excelente calidad /pureza",
                  "Es una marca saludable",
                  "Es una marca que no se encuentra fácil en supermercados",
                  "Es sostenible con el medio ambiente",
                  "Es la más dulce",
                  "Es una marca costosa",
                  "Es la marca más procesada",
                  "Es la más natural",
                  "Es una marca económica",
                  "Es orgánica",
                  "Se disuelve fácil",
                  "Es una marca con producto de fácil disolución",
                  "Es la marca que no modifica el sabor de las comidas",
                  "Es la marca que no modifica el color de las comidas")

library(FactoMineR)
library(factoextra)

uni.mca <- FactoMineR::MCA(df)
get_eigenvalue(uni.mca)

fviz_mca_biplot(uni.mca, repel = TRUE, 
                ggtheme = theme_grey())

fviz_mca_var(uni.mca, col.var = "purple", shape.var = 10, repel = TRUE,
             ggtheme = theme_grey())  

fviz_mca_var(uni.mca, col.var = "cos2", 
             repel = TRUE,
             gradient.cols = c("#00AFBB", "#E7B800","#FC4E07"),
             ggtheme = theme_grey())

fviz_mca_var(uni.mca, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800","#FC4E07"),
             ggtheme = theme_grey(),
             max.overlaps = "ggrepel.max.overlaps",
             repel = TRUE) +
  labs(title = "Contribución")



