gg <- fqTable|> arrange(desc(Porcentaje)) |> # slice(1:5) |> 
  ggplot2::ggplot(aes(x = reorder(Categoria, +Porcentaje), y = Porcentaje, fill=Categoria)) +
  ggplot2::geom_col(position = position_dodge2(preserve = "single")) + 
  geom_text(aes(label = paste0(round(Porcentaje,1),"%")), size = 10, hjust = -0.1, position = position_dodge(0.9)) +
  ggtitle(" ") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  # scale_fill_brewer(palette="Paired", name = " ", direction = 1) +
  scale_fill_manual(values = c("#2596be","#2596be","#2596be","#2596be",
                               "#2596be","#2596be","#2596be","#2596be",
                               "#2596be","#2596be","#2596be","#2596be",
                               "#2596be","#2596be","#2596be","#2596be",
                               "#2596be","#2596be")) +
  theme_test() +
  theme_linedraw() +
  ggplot2::theme(axis.line.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 strip.text = element_text(size = 20),
                 axis.title.x = element_text(size = 20),
                 axis.text = element_text(size = 20),
                 legend.position = "none",
                 panel.background = element_rect(fill=NA, color=NA),
                 plot.background = element_rect(fill=NA, color=NA),
                 # axis.line = element_line(color="black"),
                 panel.grid.major = element_line(colour = "gray50"),
                 panel.grid.minor = element_line(color = "gray70"))
gg
ggplot2::ggsave(paste0(root,prj,"/3.Results/p25.png"), gg, width=15, height=9, units = "in", dpi=366)

# RColorBrewer::display.brewer.all()
