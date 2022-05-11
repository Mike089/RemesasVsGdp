library(patchwork)
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(firatheme)
library(showtext)
library(ggthemes)
library(viridis)
library(hrbrthemes)
library(scico)
library("Cairo")
library(ggstream)
library(ggplot2)
library(ggtext)
options(scipen = 9999)

showtext_auto()


font_add_google(family = "patua-one", "Patua One")
font_add_google(family = "montserrat", "Montserrat")
font_add_google(family = "lato", "Lato")
font_add_google(family = "squada-one", "Squada One")


remesas <- read.csv("remesas_gdp_mex.csv")

remesas <- remesas %>%
    mutate(percent = (remesas/gdp))


remesas %>%
    ggplot(aes(x = years, y = gdp))+
    geom_area(fill = "#de2d26")+
    geom_area(aes(x = years, y = remesas), fill = "#31a354")+
    scale_y_continuous(labels = unit_format(unit = "T", scale = 1e-12), breaks = seq(0,1300000000000,250000000000),
                       expand = c(0,0),position = "right")+
    scale_x_continuous(breaks = seq(1980,2020,5))+
    labs(title ="<span style = 'color:#31a354'>REMESAS</span> <span style = 'color:#FFFFFF'>COMO % DEL</span> 
         <span style = 'color:#de2d26'>PIB</span> <span style = 'color:#FFFFFF'>EN MÉXICO</span>",
         y = "PIB Nominal (USD)",
         x = "",
         caption = "Fuente: WorldBank.org | Visualización: Miguel HG (@mike_dvz)")+
    theme_fira()+
    geom_segment(data = remesas,aes( x= 1980, xend= 1980, y = 0, yend = 40000000000), color = "white",linetype=2,size =1)+
    geom_segment(data = remesas,aes( x= 1985, xend= 1985, y = 0, yend = 80000000000), color = "white",linetype=2,size =1)+
    geom_segment(data = remesas,aes( x= 1990, xend= 1990, y = 0, yend = 100000000000), color = "white",linetype=2,size =1)+
    geom_segment(data = remesas,aes( x= 1995, xend= 1995, y = 0, yend = 190000000000), color = "white",linetype=2,size =1)+
    geom_segment(data = remesas,aes( x= 2000, xend= 2000, y = 0, yend = 106000000000), color = "white",linetype=2,size =1)+
    geom_segment(data = remesas,aes( x= 2005, xend= 2005, y = 0, yend = 260000000000), color = "white",linetype=2,size =1)+
    geom_segment(data = remesas,aes( x= 2010, xend= 2010, y = 0, yend = 200000000000), color = "white",linetype=2,size =1)+
    geom_segment(data = remesas,aes( x= 2015, xend= 2015, y = 0, yend = 230000000000), color = "white",linetype=2,size =1)+
    geom_segment(data = remesas,aes( x= 2020, xend= 2020, y = 0, yend = 400000000000), color = "white",linetype=2,size =1)+
    annotate("text", y = 40000000000+20000000000, x = 1980, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "0.51%")+
    annotate("text", y = 80000000000+20000000000, x = 1985, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "0.83%")+
    annotate("text", y = 100000000000+20000000000, x = 1990, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "1.19%")+
    annotate("text", y = 190000000000+20000000000, x = 1995, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "1.21%")+
    annotate("text", y = 106000000000+20000000000, x = 2000, family = "squada-one", size = 6,
            lineheight = .9, color = "white", label = "1.06%")+
    annotate("text", y = 260000000000+20000000000, x = 2000+5, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "2.59%")+
    annotate("text", y = 200000000000+20000000000, x = 2000+10, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "2.08%")+
    annotate("text", y = 230000000000+20000000000, x = 2000+15, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "2.24%")+
    annotate("text", y = 400000000000+20000000000, x = 2000+20, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "4.01%")+
    theme(plot.background = element_rect(fill = "#252525", color = NA),
          axis.text = element_text(color = "white", family = "montserrat", size = 14, face = "bold"),
          legend.text = element_text(color = "white", family = "montserrat",face = "bold", size = 12),
          axis.title.y = element_text(color = "white", family = "montserrat", size = 13, face = "bold"),
          legend.title = element_text(color = "white", family = "montserrat", size = 11, face = "bold"),
          axis.title.x = element_text(color = "white", family = "montserrat", size = 13, face = "bold"),
          plot.title = element_markdown(family = "patua-one", size = 35, face = "bold"),
          plot.subtitle = element_text(color = "white", family = "montserrat", size = 14, face = "bold"),
          plot.caption = element_text(color = "white", family = "patua-one", size = 13, face = "bold"),
          legend.position = "none")


ggsave(plot = last_plot(), "remesas.png", height = 965,width = 1269, units = "px",dpi = 150)




remesas %>%
    ggplot(aes(x = years, y = gdp))+
    geom_area(fill = "#de2d26")+
    geom_area(aes(x = years, y = remesas), fill = "#31a354")+
    scale_y_continuous(labels = unit_format(unit = "T", scale = 1e-12), breaks = seq(0,1300000000000,250000000000),
                       expand = c(0,0),position = "right")+
    scale_x_continuous(breaks = seq(1980,2020,5))+
    labs(title ="<span style = 'color:#31a354'>Remittances</span> <span style = 'color:#FFFFFF'>as % of</span> <span style = 'color:#de2d26'>GDP</span> 
         <span style = 'color:#FFFFFF'>in Mexico</span>",
         y = "Nominal GDP (In USD)",
         x = "",
         caption = "Source: WorldBank.org | Visualization: Miguel HG (@mike_dvz)")+
    theme_fira()+
    geom_segment(data = remesas,aes( x= 1980, xend= 1980, y = 0, yend = 40000000000), color = "white",linetype=2,size =1)+
    geom_segment(data = remesas,aes( x= 1985, xend= 1985, y = 0, yend = 80000000000), color = "white",linetype=2,size =1)+
    geom_segment(data = remesas,aes( x= 1990, xend= 1990, y = 0, yend = 100000000000), color = "white",linetype=2,size =1)+
    geom_segment(data = remesas,aes( x= 1995, xend= 1995, y = 0, yend = 190000000000), color = "white",linetype=2,size =1)+
    geom_segment(data = remesas,aes( x= 2000, xend= 2000, y = 0, yend = 106000000000), color = "white",linetype=2,size =1)+
    geom_segment(data = remesas,aes( x= 2005, xend= 2005, y = 0, yend = 260000000000), color = "white",linetype=2,size =1)+
    geom_segment(data = remesas,aes( x= 2010, xend= 2010, y = 0, yend = 200000000000), color = "white",linetype=2,size =1)+
    geom_segment(data = remesas,aes( x= 2015, xend= 2015, y = 0, yend = 230000000000), color = "white",linetype=2,size =1)+
    geom_segment(data = remesas,aes( x= 2020, xend= 2020, y = 0, yend = 400000000000), color = "white",linetype=2,size =1)+
    annotate("text", y = 40000000000+20000000000, x = 1980, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "0.51%")+
    annotate("text", y = 80000000000+20000000000, x = 1985, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "0.83%")+
    annotate("text", y = 100000000000+20000000000, x = 1990, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "1.19%")+
    annotate("text", y = 190000000000+20000000000, x = 1995, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "1.21%")+
    annotate("text", y = 106000000000+20000000000, x = 2000, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "1.06%")+
    annotate("text", y = 260000000000+20000000000, x = 2000+5, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "2.59%")+
    annotate("text", y = 200000000000+20000000000, x = 2000+10, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "2.08%")+
    annotate("text", y = 230000000000+20000000000, x = 2000+15, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "2.24%")+
    annotate("text", y = 400000000000+20000000000, x = 2000+20, family = "squada-one", size = 6,
             lineheight = .9, color = "white", label = "4.01%")+
    theme(plot.background = element_rect(fill = "#252525", color = NA),
          axis.text = element_text(color = "white", family = "montserrat", size = 14, face = "bold"),
          legend.text = element_text(color = "white", family = "montserrat",face = "bold", size = 12),
          axis.title.y = element_text(color = "white", family = "montserrat", size = 13, face = "bold"),
          legend.title = element_text(color = "white", family = "montserrat", size = 11, face = "bold"),
          axis.title.x = element_text(color = "white", family = "montserrat", size = 13, face = "bold"),
          plot.title = element_markdown(family = "patua-one", size = 35, face = "bold"),
          plot.subtitle = element_text(color = "white", family = "montserrat", size = 14, face = "bold"),
          plot.caption = element_text(color = "white", family = "patua-one", size = 13, face = "bold"),
          legend.position = "none")


ggsave(plot = last_plot(), "remesas_eng.png", height = 965,width = 1269, units = "px",dpi = 150)















