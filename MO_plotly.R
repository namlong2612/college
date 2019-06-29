library(tidyverse)
library(dplyr)
library(plotly)
library(readxl)
library(ggplot2)

modata <- read_excel("MO_AllCounties_CDs_MMG_2016.xlsx")
# modata <- MOdata %>% filter(Area != "St. Louis" & Area != "St. Louis city" & Area != "Saint Charles")

plot_ly(data=modata, x = ~Area, y = ~MOdata$`Food insecurity rate`, type="box")
adair <- modata %>% filter(County == "Adair")




p <- plot_ly(modata, x = ~Population, y = ~`Food insecurity rate`, type = 'scatter', color = ~Area, mode = 'markers',
        text = ~paste(County)) 
p %>% layout(title="Food insecurity rate for MO counties")

# MO heat map
library(maps)
mo_map <- map_data("county", region="missouri") %>% mutate(region=subregion)

modataa <- modata
colnames(modataa)[colnames(modataa)=="County"] <- "subregion"
modataa$subregion <- tolower(modataa$subregion)
moinfo <- inner_join(mo_map, modataa, by="subregion")



mo_plot <- ggplot(data = mo_map, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(data = moinfo, aes(fill = `Food insecurity rate`), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_void() +
  ggtitle("Food insecurity rate in MO")+
  scale_fill_gradientn(colours = rev(rainbow(7)),
                                    breaks = c(0.1, 0.12, 0.14, 0.16, 0.18, 0.2,0.22, 0.24))
ggplotly(mo_plot)


