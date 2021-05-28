#setwd("~//praxiskurs_fss21/")
options(repr.plot.width=8, repr.plot.height=3,repr.plot.res = 300)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)
library(plotly)
library(dplyr)

df_kreisinzidenz <- readxl::read_excel("data/Fallzahlen_Kum_Tab.xlsx",sheet="LK_7-Tage-Inzidenz (fixiert)",
                                      skip= 4)
df_kreisinzidenz %>%head

as.Date(44344,origin="1899-12-30")

df_kreisinzidenz <- df_kreisinzidenz %>% select(-c("...1",LKNR)) %>%
 tidyr::pivot_longer(cols=!LK,names_to = "date",values_to="value") %>% 
    mutate(date=
           ifelse(str_detect(date,"44"),
                as.Date(as.numeric(date),origin="1899-12-30") %>% as.character,
                as.Date(date,format="%d.%m.%Y") %>% as.character
                 ) %>%
           as.Date)
df_kreisinzidenz %>% head

p <-     ggplot(df_kreisinzidenz) + 
    geom_path(aes(x=date,y=value,color=LK))
p + scale_color_discrete(guide=F)

# ggplotly(p)

library(rgdal)
library(broom)
sp_shapes <- readOGR(dsn="data/kreisgrenzen/Kreisgrenzen_2019.shp", stringsAsFactors = F)

p <- ggplot(sp_shapes) + geom_polygon(aes(x=long,y=lat,group=group), colour = "black", fill = NA)
p

p + theme_void() + coord_quickmap()

sp_shapes@data %>% str

p <- ggplot(sp_shapes) + 
    geom_polygon(aes(x=long,y=lat,group=group,fill=as.numeric(id)), colour = "black") +
    theme_void() + coord_quickmap() 
p

sp_shapes@data %>% head

df_shapes <- tidy(sp_shapes)
df_shapes %>% str

ggplot(df_shapes) + 
    geom_polygon(aes(x=long,y=lat,group=group,fill=as.numeric(id)), colour = "black") +
    theme_void() + coord_quickmap() 

str(sp_shapes@data)

df_shapes <- df_shapes %>% mutate(FID=as.numeric(id)+1)  %>%  left_join( sp_shapes@data,by="FID") 
df_shapes %>% 
    head


ggplot(
    df_shapes %>% mutate(value=str_count(GEN)) 
) +
    geom_polygon(aes(x=long,y=lat,group=group,fill=value), colour = "black") +
    theme_void() + coord_quickmap() +
    scale_fill_continuous()

p <- ggplot(
    df_shapes %>% mutate(value=str_count(GEN)) 
) +
    geom_polygon(aes(x=long,y=lat,group=group,fill=value, text=GEN), colour = "black") +
    theme_void() + coord_quickmap() 
p

#ggplotly(p,tooltip="GEN")
