setwd("~//praxiskurs_fss21/")
options(repr.plot.width=8, repr.plot.height=3,repr.plot.res = 300)
library(ggplot2)
library(ggcorrplot)

library(magrittr)
library(tidyr)
library(plotly)
library(dplyr)

download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv", "data/owid-covid-data.csv")

df_covid <- read.csv("data/RKI_COVID19.csv")
df_owid <- read.csv("data/owid-covid-data.csv")

df_owid$date <- as.Date(df_owid$date)
df_covid$Meldedatum <- as.Date(df_covid$Meldedatum)
dates <- seq(min(df_covid$Meldedatum)-3, max(df_covid$Meldedatum), by="day") 
df_complete  <- data.frame(dates = dates )
df_complete$cases_per_day <- sapply(dates, function(date) sum(df_covid$AnzahlFall[df_covid$Meldedatum == date])  )
df_complete$runmean_cases <- caTools::runmean(df_complete$cases_per_day, k=7, align="center")
df_complete$deaths_per_day <- sapply(dates, function(date) sum(df_covid$AnzahlTodesfall[df_covid$Meldedatum == date])  )
df_complete$runmean_deaths <- caTools::runmean(df_complete$deaths_per_day, k=7, align="center")
df_complete <- df_complete[dates<=max(df_covid$Meldedatum),]

#ggplotly(
    ggplot(df_owid, aes(x=date,y=new_cases_smoothed_per_million,color=location)) + geom_path()
#)

df_vacc <-  df_owid  %>% 
    dplyr::filter(continent=="Europe") %>%
    dplyr::filter(date>="2021-01-01") %>% 
    dplyr::select(c(location,date,new_vaccinations_smoothed_per_million)) %>% 
    tidyr::pivot_wider(names_from= location, values_from=new_vaccinations_smoothed_per_million)
df_vacc %>% head

corr_matrix <- df_vacc %>% dplyr::select(-date) %>% cor(use="pairwise.complete.obs")
corrplot::corrplot(corr_matrix)

ggcorrplot::ggcorrplot(corr_matrix) 

names(df_owid)

#ggplotly(
    ggplot(df_owid %>% 
           group_by(location) %>% 
           summarise(pop_density = population_density[1], cases = max(total_cases_per_million) )) +
    geom_point(aes(x=pop_density,y=cases, text = location))
#    ,    tooltip=c("text","x","y")
#    )  

cols <- df_owid %>% names


suppressWarnings(
    factors <- df_owid %>% group_by(location) %>%
    dplyr::summarise_at( c("total_cases_per_million","total_deaths_per_million", cols[44:length(cols)]),max,na.rm=T) %>% 
    mutate(ifr=total_deaths_per_million/total_cases_per_million)
    )

head(factors)

factors <- factors  %>% 
    mutate(across(!location,function(col) replace(col,is.infinite(col),NA) ))

options(repr.plot.width=8, repr.plot.height=8,repr.plot.res = 300)

factors %>% 
    dplyr::select(-location) %>% 
    cor(use="pairwise.complete.obs") %>% 
    ggcorrplot()

#ggplotly(
    ggplot(factors) +  geom_point(aes(x=stringency_index,y=ifr,text=location)) +
        geom_smooth(aes(x=stringency_index,y=ifr),method="lm",na.rm=T)
#    ,tooltip=c("text","x","y")
#    ) 

#ggplotly(
    ggplot(factors) +  geom_point(aes(y=total_deaths_per_million,x=total_cases_per_million,text=location,size=population)) +
        geom_smooth(aes(y=total_deaths_per_million,x=total_cases_per_million),method="lm",na.rm=T)
# ,
#    tooltip=c("text","x","y")
#    ) 

lm(ifr~ population,data=factors )

cols

formel <- as.formula(paste("ifr ~", paste(collapse ="+",cols[44:length(cols)],sep=" " )))
formel

model <- lm(formel,data=factors)
model

summary(model)

df_covid %>% head

df_agegroups <- df_covid %>% 
    dplyr::select(c(Altersgruppe,AnzahlFall,AnzahlTodesfall)) %>% 
    tidyr::pivot_longer(cols = Altersgruppe,values_to =  "Altersgruppe")
df_agegroups %>% head

df_agegroups  <- df_agegroups %>% 
    group_by(Altersgruppe) %>% 
    summarise(cases = sum(AnzahlFall),deaths=sum(AnzahlTodesfall))
df_agegroups

options(repr.plot.width=8, repr.plot.height=4,repr.plot.res = 300)

ggplot(df_agegroups, aes(x=cases, y= Altersgruppe)) +
    geom_histogram(stat="identity",fill="blue",alpha=0.5)

df_agegroups %>% tidyr::pivot_longer(cols = c(cases,deaths),names_to = "category")

ggplot(
    df_agegroups %>% 
    tidyr::pivot_longer(cols = c(cases,deaths),names_to = "category")
    )+
    geom_histogram(aes(x=value, y= Altersgruppe, fill=category),stat="identity",alpha=0.7)

df_agegroups

bins <- c(0,4,14,34,59,79,100)
df_ages <- integer(0)
for(i in 1:(length(bins)-1) ){
    age_bin  <- seq(bins[i],bins[i+1]-1, by = 1)
    df_ages  <- c(df_ages, sample(age_bin, size =  df_agegroups$cases[i], replace = T ) )
}
head(df_ages)

q <- qplot(x=df_ages,geom = "histogram",bins="20",xlab="Age")
q
