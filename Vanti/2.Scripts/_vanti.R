# -------------------------------------------------------------------------
# Proyecto: Imagen de marca centro comercial Unicentro
# By:  Cesar A. Saavedra
# Mindlabs
# Julio 2024

# R options ---------------------------------------------------------------
g <- gc(reset = T); rm(list = ls())
options(warn = -1, scipen = 999)
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(dplyr, tidyr, ggplot2, foreign, ggcharts, mdthemes,forcats,
                                readxl, ggh4x, raster, geodata, terra, sf, sp, lubridate))

root <- "/Users/cesara.saavedravanegas/Documents/GitHub/MindLabs/"
prj <- "Vanti"
# Lectura de datos --------------------------------------------------------
db <- readxl::read_excel(paste0(root,prj,"/1.Data/Asistencias_hogar.xlsx"))
head(db)
# -------------------------------------------------------------------------
db$date_created <- as.Date(db$date_created)
db <- db %>% dplyr::mutate(Estrato = dplyr::case_when(date_created == ("2024-07-24")	~	"NSE 1 y 2",
                                                      date_created == ("2024-07-19")	~	"NSE 3, 4 y 5"))

db$Valor_dispuesto <- as.numeric(db$Valor_dispuesto)

labels <- c("0-20mil", "20mil-40mil", "40mil-60mil", "60mil-80mil", "80mil-100mil", "100mil-120mil")
breaks <- c(0, 20000, 40000, 60000, 80000, 100000, 120000)
db$valores_grupos <- cut(db$Valor_dispuesto, 
                         breaks = breaks, 
                         labels = labels, 
                         include.lowest = TRUE)
# -------------------------------------------------------------------------
gg <- db %>% 
  dplyr::group_by(Estrato, Marca_alguna_opción) %>%
  dplyr::count(valores_grupos) %>% 
  dplyr::mutate(Por = (prop.table(n)*100)) %>% 
  ggplot2::ggplot(aes(x = interaction(valores_grupos, Marca_alguna_opción, sep = "&"), y = n, fill = Estrato)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single")) + 
  ggplot2::geom_text(aes(label = paste0(round(n,2),"")),
                     position=position_dodge(width=0.9), size = 4, vjust= -0.5) +
  ggplot2::labs(title = " ",
                x = " ",
                y = " ",
                fill = " ") +
  # ggplot2::coord_flip() +
  theme_void() +
  ggplot2::scale_fill_brewer(palette = "Paired") +
  ggplot2::scale_x_discrete(guide = guide_axis_nested(delim = "&")) +
  ggplot2::theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5),
                 plot.title = element_text(size=16, face='bold'),
                 strip.text = element_text(size = 9, face = "bold"),
                 axis.title.x = element_text(size = 9, face = 'bold'),
                 axis.title.y = element_text(size = 9, face = 'bold'),
                 axis.text = element_text(size = 9),
                 legend.title=element_text(size=10), 
                 legend.text=element_text(size=10),
                 legend.position="bottom")

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pr_1.png"), gg, width=12, height=7, units = "in", dpi=366)

# -------------------------------------------------------------------------
#
gg <- db %>% dplyr::filter(., Marca_alguna_opción == "Triángulo") %>% 
  dplyr::group_by(Marca_alguna_opción,Estrato) %>%
  dplyr::count(Si_18.990) %>% 
  dplyr::mutate(Por = (prop.table(n)*100)) %>% 
  ggplot2::ggplot(aes(x = interaction(Si_18.990, Marca_alguna_opción, sep = "&"), 
                      y = n, fill = Estrato)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single")) + 
  ggplot2::geom_text(aes(label = paste0(round(n,2),"")),
                     position=position_dodge(width=0.9), size = 4, vjust= -0.5) +
  ggplot2::labs(title = "Si este seguro tuviera un precio de $18.990 ¿estarías dispuesto(a) a pagarlo?",
                x = " ",
                y = " ",
                fill = " ") +
  # ggplot2::coord_flip() +
  theme_void() +
  ggplot2::scale_fill_brewer(palette = "Paired") +
  ggplot2::scale_x_discrete(guide = guide_axis_nested(delim = "&")) +
  ggplot2::theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5),
                 plot.title = element_text(size=16, face='bold'),
                 strip.text = element_text(size = 9, face = "bold"),
                 axis.title.x = element_text(size = 9, face = 'bold'),
                 axis.title.y = element_text(size = 9, face = 'bold'),
                 axis.text = element_text(size = 9),
                 legend.title=element_text(size=10), 
                 legend.text=element_text(size=10),
                 legend.position="bottom")

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pr_2.png"), gg, width=9, height=7, units = "in", dpi=366)

# -------------------------------------------------------------------------
#
gg <- db %>% dplyr::filter(., Marca_alguna_opción == "Cuadrado") %>% 
  dplyr::group_by(Marca_alguna_opción,Estrato) %>%
  dplyr::count(Si_21.990) %>% 
  dplyr::mutate(Por = (prop.table(n)*100)) %>% 
  ggplot2::ggplot(aes(x = interaction(Si_21.990, Marca_alguna_opción, sep = "&"), 
                      y = n, fill = Estrato)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single")) + 
  ggplot2::geom_text(aes(label = paste0(round(n,2),"")),
                     position=position_dodge(width=0.9), size = 4, vjust= -0.5) +
  ggplot2::labs(title = "Si este seguro tuviera un precio de $21.990 ¿estarías dispuesto(a) a pagarlo?",
                x = " ",
                y = " ",
                fill = " ") +
  # ggplot2::coord_flip() +
  theme_void() +
  ggplot2::scale_fill_brewer(palette = "Paired") +
  ggplot2::scale_x_discrete(guide = guide_axis_nested(delim = "&")) +
  ggplot2::theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5),
                 plot.title = element_text(size=16, face='bold'),
                 strip.text = element_text(size = 9, face = "bold"),
                 axis.title.x = element_text(size = 9, face = 'bold'),
                 axis.title.y = element_text(size = 9, face = 'bold'),
                 axis.text = element_text(size = 9),
                 legend.title=element_text(size=10), 
                 legend.text=element_text(size=10),
                 legend.position="bottom")

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pr_3.png"), gg, width=9, height=7, units = "in", dpi=366)

# -------------------------------------------------------------------------
#
gg <- db %>% dplyr::filter(., Marca_alguna_opción == "Círculo") %>% 
  dplyr::group_by(Marca_alguna_opción,Estrato) %>%
  dplyr::count(Si_29.990) %>% 
  dplyr::mutate(Por = (prop.table(n)*100)) %>% 
  ggplot2::ggplot(aes(x = interaction(Si_29.990, Marca_alguna_opción, sep = "&"), 
                      y = n, fill = Estrato)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single")) + 
  ggplot2::geom_text(aes(label = paste0(round(n,2),"")),
                     position=position_dodge(width=0.9), size = 4, vjust= -0.5) +
  ggplot2::labs(title = "Si este seguro tuviera un precio de $29.990 ¿estarías dispuesto(a) a pagarlo?",
                x = " ",
                y = " ",
                fill = " ") +
  # ggplot2::coord_flip() +
  theme_void() +
  ggplot2::scale_fill_brewer(palette = "Paired") +
  ggplot2::scale_x_discrete(guide = guide_axis_nested(delim = "&")) +
  ggplot2::theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5),
                 plot.title = element_text(size=16, face='bold'),
                 strip.text = element_text(size = 9, face = "bold"),
                 axis.title.x = element_text(size = 9, face = 'bold'),
                 axis.title.y = element_text(size = 9, face = 'bold'),
                 axis.text = element_text(size = 9),
                 legend.title=element_text(size=10), 
                 legend.text=element_text(size=10),
                 legend.position="bottom")

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pr_4.png"), gg, width=9, height=7, units = "in", dpi=366)

# -------------------------------------------------------------------------
#
gg <- db %>% dplyr::filter(., Marca_alguna_opción == "Rombo") %>% 
  dplyr::group_by(Marca_alguna_opción,Estrato) %>%
  dplyr::count(Si_32.990) %>% 
  dplyr::mutate(Por = (prop.table(n)*100)) %>% 
  ggplot2::ggplot(aes(x = interaction(Si_32.990, Marca_alguna_opción, sep = "&"), 
                      y = n, fill = Estrato)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single")) + 
  ggplot2::geom_text(aes(label = paste0(round(n,2),"")),
                     position=position_dodge(width=0.9), size = 4, vjust= -0.5) +
  ggplot2::labs(title = "Si este seguro tuviera un precio de $32.990 ¿estarías dispuesto(a) a pagarlo?",
                x = " ",
                y = " ",
                fill = " ") +
  # ggplot2::coord_flip() +
  theme_void() +
  ggplot2::scale_fill_brewer(palette = "Paired") +
  ggplot2::scale_x_discrete(guide = guide_axis_nested(delim = "&")) +
  ggplot2::theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5),
                 plot.title = element_text(size=16, face='bold'),
                 strip.text = element_text(size = 9, face = "bold"),
                 axis.title.x = element_text(size = 9, face = 'bold'),
                 axis.title.y = element_text(size = 9, face = 'bold'),
                 axis.text = element_text(size = 9),
                 legend.title=element_text(size=10), 
                 legend.text=element_text(size=10),
                 legend.position="bottom")

ggplot2::ggsave(paste0(root,prj,"/3.Results/Pr_5.png"), gg, width=9, height=7, units = "in", dpi=366)

# -------------------------------------------------------------------------

  
