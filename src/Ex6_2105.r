#setwd("~//praxiskurs_fss21/")
options(repr.plot.width=8, repr.plot.height=3,repr.plot.res = 300)
library(ggplot2)

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

df_agegroups <- df_covid %>% 
    dplyr::select(c(Altersgruppe,AnzahlFall,AnzahlTodesfall)) %>% 
    tidyr::pivot_longer(cols = Altersgruppe,values_to =  "Altersgruppe")
df_agegroups %>% head

df_agegroups  <- df_agegroups %>% 
    group_by(Altersgruppe) %>% 
    summarise(cases = sum(AnzahlFall),deaths=sum(AnzahlTodesfall))


options(repr.plot.width=8, repr.plot.height=4,repr.plot.res = 300)

ggplot(df_agegroups, aes(x=cases, y= Altersgruppe)) +
    geom_histogram(stat="identity",fill="blue",alpha=0.5)

bins <- c(0,5,15,35,60,80,100)
df_ages <- integer(0)
for(i in 1:(length(bins)-1) ){
    age_bin  <- seq(bins[i],bins[i+1]-1, by = 1)
    df_ages  <- c(df_ages, sample(age_bin, size =  df_agegroups$cases[i], replace = T ) )
}
head(df_ages)

qplot(x=df_ages,geom = "histogram",bins="50",xlab="Age") + 
    geom_vline(xintercept = median(df_ages),color="blue") + 
    geom_vline(xintercept = mean(df_ages), color="red") +
    scale_x_continuous(n.breaks = 10)


df_ages_left_skewed <- integer(0)
for(i in 1:(length(bins)-1) ){
    age_bin  <- seq(bins[i],bins[i+1]-1, by = 1)
    df_ages_left_skewed  <- c(df_ages_left_skewed, sample(age_bin, size =  df_agegroups$cases[i], replace = T, 
                                              prob = 1/(age_bin+1)^10) )
}
qplot(x=df_ages_left_skewed,geom = "histogram",bins="50",xlab="Age")+ 
    geom_vline(xintercept = median(df_ages_left_skewed),color="blue") + 
    geom_vline(xintercept = mean(df_ages_left_skewed), color="red") +
    scale_x_continuous(n.breaks = 10)



df_ages_right_skewed <- integer(0)
for(i in 1:(length(bins)-1) ){
    age_bin  <- seq(bins[i],bins[i+1]-1, by = 1)
    df_ages_right_skewed  <- c(df_ages_right_skewed, sample(age_bin, size =  df_agegroups$cases[i], replace = T, 
                                              prob = (age_bin+1)^10) )
}
qplot(x=df_ages_right_skewed,geom = "histogram",bins="50",xlab="Age")+ 
    geom_vline(xintercept = median(df_ages_right_skewed),color="blue") + 
    geom_vline(xintercept = mean(df_ages_right_skewed), color="red") +
    scale_x_continuous(n.breaks = 10)


table(df_ages)

smoothed <- stats::ksmooth(x=seq(0,100,by=1), y = table(df_ages),kernel = "normal", bandwidth = 15)
ggplot(as.data.frame(smoothed)) + geom_line(aes(x=x,y=y))

df_covid %>% head

df_population <- read.csv2("https://service.destatis.de/bevoelkerungspyramide/data/14_bevoelkerungsvorausberechnung_daten.csv")

df_population %>% head

(df_population2020 <- df_population %>% filter(Variante==1,Simulationsjahr==2020))

df_population2020  <- df_population2020 %>% 
    summarise_at(vars(starts_with("Bev")),sum) %>% 
    tidyr::pivot_longer(cols = everything(),names_to = "age")

df_population2020 %>% head

library(stringr)
df_population2020 <- df_population2020 %>% 
    filter(age!="Bev") %>% 
    mutate(age=str_extract(age,"(\\d+)(?!.*\\d)") %>% as.numeric, age = age-1) %>%  
    mutate(group = sapply(age,function(x) sum(x>=bins) ))
df_population2020 %>% head

df_agegroups$Altersgruppe

(df_population_grouped <- df_population2020 %>% 
    group_by(group) %>% 
    summarise(population=sum(value)) %>% 
    mutate(Altersgruppe = df_agegroups$Altersgruppe[-7]))

df_agegroups <- df_population_grouped %>% select(-group) %>%  inner_join(df_agegroups)
df_agegroups

ggplot(df_agegroups) + 
    geom_histogram(aes(y = Altersgruppe, x = cases/(population * 1e3/1e5 ) ), stat = "identity", fill="blue")


