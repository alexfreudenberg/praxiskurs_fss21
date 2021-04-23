# Mit setwd setzt ihr in R euer "working directory"
setwd("~/Documents/praxiskurs_fss21/")

# Daten werden eingelesen mit der Funktion read.csv bzw read.csv2
df_covid <- read.csv("data/RKI_COVID19.csv")

dates <- unique(df_covid$Meldedatum)
cases_per_day  <- sapply(dates, function(date) sum(df_covid$AnzahlFall[df_covid$Meldedatum == date])  )
str(cases_per_day)

# Wir speichern die Daten im richten Format und erstellen einen data frame dafür
dates <- as.Date(dates, format="%Y/%m/%d")
df_cases_per_day <- data.frame(dates=dates,cases=cases_per_day)
df_cases_per_day <- df_cases_per_day[order(df_cases_per_day$dates),]
head(df_cases_per_day)

plot(df_cases_per_day$dates,df_cases_per_day$cases, 
     type="l", xlab="Day", ylab="Date", main="Timeseries of cases per day")

# Mit dem POSIX Format lassen sich auch einfach Informationen aus Datumsangaben extrahieren, 
# beispielsweise den Wochentag
df_cases_per_day$wday  <- weekdays(df_cases_per_day$dates)

# Damit können wir uns auch die Gesamtanzahl an Fällen pro Wochentag anschauen
wdays <- c("Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag")
cases_per_wday <-  sapply(wdays, function(wday) sum(df_cases_per_day$cases[df_cases_per_day$wday ==wday] ))
print(cases_per_wday)

# ... und wieder verschönern
plot(cases_per_wday, type="b", xaxt="n", ylim =c(0,max(cases_per_wday)))
axis(1,at = 1:7,labels = wdays,cex.axis=0.8)

ticks  <- seq(df_cases_per_day$dates[1],df_cases_per_day$dates[length(dates)], by="month")
ticks

plot(as.POSIXct(df_cases_per_day$dates),df_cases_per_day$cases, 
     type="l", xlab="Day", ylab="Date", main="Timeseries of cases per day")
axis.POSIXct(1,at=ticks,format="%Y-%m")

days <- seq(min(df_cases_per_day$dates),max(df_cases_per_day$dates),by="day")

head(days %in% df_cases_per_day$dates)

df_missing_days <- data.frame(dates = days[! as.Date(days) %in% as.Date(df_cases_per_day$dates)], cases = 0)
df_missing_days$wday <- weekdays(df_missing_days$dates)
head(df_missing_days)

df_cases_per_day  <- rbind(df_cases_per_day, df_missing_days)
df_cases_per_day <- df_cases_per_day[order(df_cases_per_day$dates),]
head(df_cases_per_day)

plot(df_cases_per_day[,c("dates","cases")], 
     type="l", xlab="Day", ylab="Date", main="Timeseries of cases per day",xaxt="n")
axis.POSIXct(1,at=ticks,format="%Y-%m")

library(ggplot2)

ggplot(df_cases_per_day, aes(x = dates, y=cases)) + geom_path()

df_cases_per_day$runmean <- caTools::runmean(df_cases_per_day$cases,k=7,align="center")
ggplot(df_cases_per_day) + geom_point(aes(x = dates, y=cases))

p <- ggplot(df_cases_per_day)

p <- p + geom_path(aes(x = dates, y=cases)) + labs(title="Visualization of daily cases", x="Date", y="Cases")
p

p + geom_path(aes(x = dates, y=runmean),colour="blue")

p <- p + geom_path(aes(x = dates, y=runmean),colour="blue",size=2) + theme_light() 
p

p + scale_x_date(breaks=ticks)

p <- p + scale_x_date(breaks= seq(as.Date("2020-01-07"), as.Date("2021-05-01"), by = "3 months"),
                minor_breaks = ticks)
p

ggplot(df_cases_per_day)+ geom_histogram(aes(x=cases))

ggplot(df_cases_per_day)+ geom_histogram(aes(x=cases),bins=40, color="white")

df_cases_per_state <- data.frame()
for(land in unique(df_covid$Bundesland) ){
    df_temp <- data.frame(date = unique(df_covid$Meldedatum), bundesland = land)
    df_temp$cases  <- sapply(unique(df_covid$Meldedatum), function(date)
        sum(df_covid$AnzahlFall[df_covid$Meldedatum==date & df_covid$Bundesland == land]))
    df_cases_per_state <- rbind(df_cases_per_state, df_temp)
}

head(df_cases_per_state)

options(repr.plot.width=8, repr.plot.height=3,repr.plot.res = 300)
ggplot(df_cases_per_state)+geom_line(aes(x=as.Date(date), y= cases, color= bundesland))

laender <- c("Bayern","Baden-Württemberg","Rheinland-Pfalz")
ggplot(df_cases_per_state[df_cases_per_state$bundesland %in% laender,])+
    geom_line(aes(x=as.Date(date), y= cases, color= bundesland))

p+ scale_y_log10()


