setwd("~//praxiskurs_fss21/")
options(repr.plot.width=8, repr.plot.height=3,repr.plot.res = 300)


df_covid <- read.csv("data/RKI_COVID19.csv")

df_covid$Meldedatum <- as.Date(df_covid$Meldedatum)
dates <- seq(min(df_covid$Meldedatum)-3, max(df_covid$Meldedatum), by="day") 
df_complete  <- data.frame(dates = dates )
df_complete$cases_per_day <- sapply(dates, function(date) sum(df_covid$AnzahlFall[df_covid$Meldedatum == date])  )
df_complete$runmean_cases <- caTools::runmean(df_complete$cases_per_day, k=7, align="center")
df_complete$deaths_per_day <- sapply(dates, function(date) sum(df_covid$AnzahlTodesfall[df_covid$Meldedatum == date])  )
df_complete$runmean_deaths <- caTools::runmean(df_complete$deaths_per_day, k=7, align="center")
df_complete <- df_complete[dates<=max(df_covid$Meldedatum),]

library(ggplot2)
p <- ggplot(df_complete) +
    geom_path(aes(x=dates, y=cases_per_day),color="grey") + 
    geom_path(aes(x=dates, y=runmean_cases, color= runmean_cases)) +
    scale_x_date(breaks=seq(min(dates),max(dates),by="3 months")) 
p

p + scale_x_date(breaks=seq(min(dates),max(dates),by="2 weeks"), 
                    limits=c(as.Date("2020-08-01"),as.Date("2020-11-15"))) 

date_limits <- c(as.Date("2020-09-12"),as.Date("2020-10-27"))
q <- p + scale_x_date(breaks=seq(min(dates),max(dates),by="2 weeks"), limits= date_limits) +
    scale_y_log10(limits=c(300,20000))
q

y_diff <- log(df_complete$runmean_cases[df_complete$dates %in% date_limits])
intercept  <- y_diff[1]
y_diff <- y_diff[2]-y_diff[1]
x_diff <- (date_limits[2]-date_limits[1])

intercept

x_diff

slope <- y_diff/as.numeric(x_diff)
slope

line <-exp(intercept + (0:45) * slope)
df_line <- data.frame(dates= seq(date_limits[1],date_limits[2], by = "1 day"),line=line)
head(df_line)

q + geom_path(data=df_line,aes(x=dates,y=line), color="green" )

df_owid <- read.csv("data/owid-covid-data.csv")
head(df_owid)

colnames(df_owid)

df_owid$date <- as.Date(df_owid$date)


ggplot(df_owid[df_owid$location=="Germany" & df_owid$date>="2021-01-01",]) +
    geom_path(aes(x=date,y=new_vaccinations)) 

ggplot(df_owid[df_owid$continent =="Europe" & df_owid$date>="2021-01-01",]) +
    geom_path(aes(x=date,y=new_vaccinations, color=location)) 

head(df_owid[df_owid$continent =="Europe" & df_owid$date>="2021-01-01",])

vacc_germany <- df_owid$new_vaccinations_smoothed_per_million[df_owid$location=="Germany" & df_owid$date>="2021-01-01"]
vacc_france <-  df_owid$new_vaccinations_smoothed_per_million[df_owid$location=="France" & df_owid$date>="2021-01-01"]
correlation_france <- cor(vacc_germany, vacc_france, use="pairwise.complete.obs")
cat(paste0("Vaccination correlation with France is ", correlation_france,"\n"))


cat(paste0("Vaccination correlation with France is ", format(correlation_france*100,digits=4),"%\n"))



