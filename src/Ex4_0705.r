setwd("~//praxiskurs_fss21/")
options(repr.plot.width=8, repr.plot.height=3,repr.plot.res = 300)
library(ggplot2)
library(magrittr)
library(tidyr)

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

date_limits <- c(as.Date("2020-09-12"),as.Date("2020-10-27"))
p <- ggplot(df_complete) +
    geom_path(aes(x=dates, y=cases_per_day),color="grey") + 
    geom_path(aes(x=dates, y=runmean_cases),color="blue") 
q <- p + scale_x_date(breaks=seq(min(dates),max(dates),by="2 weeks"), limits= date_limits) +
    scale_y_log10(limits=c(300,20000))
q

df_exp  <-  df_complete %>% dplyr::filter(dates>=date_limits[1] & dates<=date_limits[2])
len  <- length(df_exp$dates)
model <- lm(log(runmean_cases) ~ 1+dates, data = df_exp)
print(model)

lm.fit(x=cbind(rep(1,46),1:46),y=log(df_exp$runmean_cases)) 

summary(model)

line <- predict(model,df_exp) %>% exp
line

q + geom_path(data = data.frame(dates = df_exp$dates, y = line), aes(x=dates,y=line)) 

p + scale_y_log10(limits=c(1e3,30e3)) + theme_minimal()

ggplot(df_owid[df_owid$location=="Germany" & df_owid$date>="2021-01-01",]) +
    geom_path(aes(x=date,y=new_vaccinations_smoothed_per_million)) 

ggplot(df_owid[df_owid$continent =="Europe" & df_owid$date>="2021-01-01",]) +
    geom_path(aes(x=date,y=new_vaccinations, color=location)) 

vacc_germany <- df_owid$new_vaccinations_smoothed_per_million[df_owid$location=="Germany" & df_owid$date>="2021-01-01"]
vacc_france <-  df_owid$new_vaccinations_smoothed_per_million[df_owid$location=="France" & df_owid$date>="2021-01-01"]
correlation_france <- cor(vacc_germany, vacc_france, use="pairwise.complete.obs")
cat(paste0("Vaccination correlation with France is ", correlation_france,"\n"))


eur_countries <- df_owid %>% dplyr::filter(continent=="Europe") %>% extract2("location") %>% unique
eur_countries

for(country in eur_countries){
    vacc_country <-  df_owid$new_vaccinations_smoothed_per_million[df_owid$location==country & df_owid$date>="2021-01-01"]
    correlation_france <- cor(vacc_germany, vacc_country, use="pairwise.complete.obs")
    cat(paste0("Vaccination correlation with ", country, " is ", correlation_france,"\n"))
}

country

df_owid %>% dplyr::filter(location==country) %>% head

relig_income

relig_income %>%
  pivot_longer(cols=!religion, names_to = "income", values_to = "count") %>% 
  head()


fish_encounters

fish_encounters %>%
    pivot_wider(names_from = station, values_from = seen)


df_vacc <- df_owid  %>% 
    dplyr::filter(location %in% c("Germany","France")) %>%
    dplyr::filter(date>="2021-01-01") %>% 
    dplyr::select(c(location,date,new_vaccinations_smoothed_per_million)) 
df_vacc %>% head

df_vacc %<>% tidyr::pivot_wider(names_from= location, values_from=new_vaccinations_smoothed_per_million)
df_vacc %>% head

df_vacc %>% 
    dplyr::select(-date)  %>% 
    cor(use="pairwise.complete.obs")

df_owid %>% head

p  <-  ggplot(df_owid  %>%  
             dplyr::filter(continent=="Europe", date>="2021-01-01" ) ) +
    geom_path(aes(x=date,y=new_vaccinations_smoothed_per_million, color=location)) 
p

library(plotly)
ggplotly(p)

names(df_owid)

ggplotly(
    ggplot(df_owid %>% 
           group_by(location) %>% 
           summarise(pop_density = population_density[1], cases = max(total_cases_per_million) )) +
    geom_point(aes(x=pop_density,y=cases, text = location)),
    tooltip=c("text","x","y")
    )  


