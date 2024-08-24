# -------------------------------------------------------------------------
Pr <- db %>% 
  # dplyr::filter(., Q43 == "Si, antes iba más frecuentemente") %>%
  # dplyr::filter(., Q36 == "Si") %>% 
  # dplyr::filter(., Q9 == "Comfandi") %>%
  dplyr::select(Q16_1:Q16_9)
  # dplyr::select(Q14_1, Q14_5:Q14_11)
Pr %>% glimpse
# -------------------------------------------------------------------------
# Analisis descriptivo
fqTable <- Pr %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% 
  dplyr::mutate(Porcentaje = (Frecuencia/nrow(Pr))*100) %>% 
  tidyr::drop_na()
fqTable
unique(fqTable$Categoria)
# -------------------------------------------------------------------------
gg <- fqTable %>% dplyr::filter(., Categoria != "Club Noel ") %>% 
  ggplot(aes(x = reorder(Categoria, +Porcentaje), y = Porcentaje, fill=Categoria)) +
  geom_bar(stat="identity",show.legend = F, position = "dodge") + 
  geom_text(aes(Categoria,Porcentaje,label=paste0(round(Porcentaje,2), "%"), 
                group=Categoria), position = position_dodge(width = 1), vjust = 0.5, hjust = -0.1, size=4) +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_fill_manual(values = c("#1a70b7","#1a70b7","#1a70b7",
                               "#1a70b7","#1a70b7","#1a70b7",
                               "#1a70b7","#1a70b7","#1a70b7",
                               "#1a70b7","#1a70b7","#1a70b7",
                               "#1a70b7","#1a70b7","#1a70b7",
                               "#1a70b7","#1a70b7")) +
  # scale_fill_manual(values = c("Comfandi Pance" = "#1a70b7",
  #                              "Otros ¿Cuál?" = "#e24b37",
  #                              "Parque de la Caña" = "#e24b37",
  #                              "Acuaparque de la Caña " = "#e24b37",
  #                              "Los Andes Club " ="#e24b37",
  #                              "Centro Recreacional Yanaconas" = "#e24b37",
  #                              "Tardes caleñas" = "#e24b37",
  #                              "Club del Valle" = "#e24b37",
  #                              "Los Andes Club" = "#e24b37")) +
  theme_void() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position="bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank())
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_46.png"), gg, width=17, height=12, units = "in", dpi=366)

# -------------------------------------------------------------------------
db %>% 
  dplyr::group_by(., Q9) %>%
  # dplyr::filter(., Q9) %>%
  dplyr::count(., Q15_1) %>% 
  tidyr::drop_na() %>%
  mutate(por = prop.table(n))
pr <- db %>% #dplyr::filter(., Q9 == "Comfandi") %>% 
  dplyr::select(Q9,Q15_1,Q15_5:Q15_9)
pr %>% glimpse

fqTable <- pr %>% tidyr::pivot_longer(cols = Q15_1:Q15_9,
                                      names_to = "measure",
                                      values_to = "value") %>% 
  dplyr::group_by(., Q9) %>%
  tidyr::drop_na() %>%
  dplyr::count(., value) %>% 
  dplyr::mutate(porcentaje = prop.table(n)*100)

gg <- fqTable %>%
  ggplot(aes(x = reorder(value, +porcentaje), y = porcentaje, fill=Q9)) +
  geom_bar(stat="identity",show.legend = T, position = "dodge") + 
  geom_text(aes(value,porcentaje,label=paste0(round(porcentaje,2), "%"), 
                group=Q9), position = position_dodge(width = 1), vjust = 0.5, hjust = -0.1, size=4) +
  ggtitle("") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  # scale_fill_brewer(palette="Set1",direction = -1, name = " ")+
  scale_fill_manual(values = c("Comfandi" = "#1a70b7",
                               "No estoy afiliado a ninguna caja de compensación" = "#e24b37",
                               "Comfenalco" = "#0b5829"), , name = " ") +
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
ggplot2::ggsave(paste0(root,prj,"/3.Results/Pregunta_15A.png"), gg, width=15, height=7, units = "in", dpi=366)

# -------------------------------------------------------------------------
