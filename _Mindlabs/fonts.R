library(rebus)
library(stringr)
library(tidyverse)

fuentes_Sys <- list.files("~/Library/Fonts/") %>% 
  str_remove_all(pattern = "." %R% capture(one_or_more(WRD)) %R% END)

install.packages("extrafont")
library(extrafont)
extrafonts::font_import() 
fuentes_Imp <- fonts()

fuentes <- append(fuentes_Imp, fuentes_Sys) %>% 
  unique() %>% 
  sort()
fuentes
