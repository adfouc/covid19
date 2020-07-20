
library(dslabs)
library(tidyverse)    # includes readr
library(readxl)
library(lubridate)

# FICHIER 1 (moins intéressant que fichier 2)

path <- "~/Projects/cov19/COVID-19/who_covid_19_situation_reports/who_covid_19_sit_rep_time_series"
setwd("~/Projects/cov19/COVID-19/")
fullpath<-file.path("who_covid_19_situation_reports","who_covid_19_sit_rep_time_series","who_covid_19_sit_rep_time_series.csv")
# list.files()
# file.exists(fullpath)

dat <- read_csv(fullpath)

# head(dat)
# dat%>%filter(`Country/Region`=="China")
# str(dat)
# levels(factor(dat$`Country/Region`))
# levels(factor(dat$`Province/States`))
# levels(factor(dat$`WHO region`))

# dat[str_detect(colnames(dat),"\\d")]
datecols<-colnames(dat[str_detect(colnames(dat),"\\d")])
cov<-dat %>% gather(date,count,datecols) %>% filter(!is.na(count)) %>% rename(country=`Country/Region`, region=`WHO region`, state=`Province/States`)

# cov %>% count(country, region,state)%>%view()
# cov%>%group_by(country,region,state)%>%filter(date==max(date) & state %in% c("Deaths","Confirmed"))%>%summarize(max(date),sum(count))%>%view()
# cov%>%filter(region=="European Region")

cov2<-cov%>%filter(is.na(state)) %>% mutate(region=factor(region))

#cov2%>%group_by(date,region)%>%summarize(sum(count))

# cov2%>% group_by(date,region)%>%
#   summarize(total=sum(count))%>%arrange(region,date)%>%view()
# str(cov2)
# class(cov2)

cov2<-cov2%>%mutate(date=mdy(date))

# PLOT : total cases evolution by region
cov2%>% group_by(date,region)%>%
  summarize(total=sum(count))%>%
  ggplot(aes(x=date,y=total,colour=region))+
  geom_line(na.rm=TRUE)+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_log10()

# FILE 2 (3 fichiers séparés)
# ---------------
setwd("~/Projects/cov19/COVID-19/")
fullpath<-file.path("csse_covid_19_data/csse_covid_19_time_series")
# list.files(fullpath)

timeseriesfile <- function(fullpath, suffix) {
  #filename<-paste("time_series_19-covid-", str_trim(suffix),".csv", sep = "")
  filename<-paste("time_series_covid19_", str_trim(suffix),".csv", sep = "")
  path<-file.path(fullpath,filename)
  dat <- read_csv(path)
  dat[str_detect(colnames(dat),"\\d")]
  datecols<-colnames(dat[str_detect(colnames(dat),"\\d")])
  dat %>% gather(date,count,datecols) %>% 
    filter(!is.na(count)) %>% 
    rename(country=`Country/Region`,  state=`Province/State`) %>% 
    mutate(type=suffix)%>%
    mutate(date=mdy(date), country=factor(country), state=factor(state))
}
# OBSOLETE
# deaths <-timeseriesfile (fullpath,"Deaths")
# confirmed <-timeseriesfile (fullpath,"Confirmed")
# recov <-timeseriesfile (fullpath,"Recovered")
# time_series <- rbind(confirmed, deaths, recov)

deaths <-timeseriesfile (fullpath,"deaths_global")
confirmed <-timeseriesfile (fullpath,"confirmed_global")
recov <-timeseriesfile (fullpath,"recovered_global")

# deaths %>% select(state,country) %>% unique %>%view

time_series <- rbind(confirmed, deaths, recov)

time_series <- time_series %>% mutate(type=recode(type,"confirmed_global"="Confirmed", "deaths_global"="Deaths","recovered_global"="Recovered"))
  
# PLOT : evol temporelle Chine France echelle LOG10
time_series %>% group_by(date,country, type)%>%
  summarize(total=sum(count))%>%
  filter(country %in% c("France","China")) %>%
  ggplot(aes(x=date,y=total,colour=type))+
  geom_line(na.rm=TRUE)+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_log10() +
  facet_wrap(~country)

 # PLOT : evol temporelle  6 pays echelle LOG2
time_series %>% group_by(date,country, type)%>%
  summarize(total=sum(count))%>%
  filter(country %in% c("France","China","Spain","Italy","US","Germany")) %>% 
  ggplot(aes(x=date,y=total,colour=type))+
  geom_line(na.rm=TRUE)+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2")+
  facet_wrap(~country, ncol = 3)

# PLOT : evol temporelle 20 fev. pays europe + US 
time_series %>% group_by(date,country, type)%>%
  summarize(total=sum(count))%>%
  filter(country %in% c("France","United Kingdom","Spain","Italy","US","Germany") &
           date>="2020-02-20") %>% 
  ggplot(aes(x=date,y=total,colour=type))+
  geom_line(na.rm=TRUE)+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~country, ncol = 3)

# PLOT : comparaison totaux actuels 6 pays 
time_series %>% group_by(country, type)%>%
  filter(country %in% c("France","China","Spain","Italy","US","Germany")) %>% 
  filter(date==max(date)) %>% 
  summarize(total=sum(count),date=max(date))%>% 
  # spread(type,total) %>%
  ggplot(aes(x=country,total, fill=type))+geom_bar(stat="identity") 
  
# PLOT : comparaison ratios actuels 6 pays
time_series %>% spread(type,count) %>% 
  filter(country %in% c("France","China","Spain","Italy","US","Germany")) %>%
  #mutate(DeathsPer=ifelse(Confirmed!=0,Deaths/Confirmed*100,0), RecovPer= ifelse(Confirmed!=0,Recovered/Confirmed*100,0)) %>%
  group_by(country)%>%
  filter(date==max(date)) %>% 
  summarize(deathratio=100*sum(Deaths)/sum(Confirmed),
            recovratio=100*sum(Recovered)/sum(Confirmed), 
            illratio=100-deathratio-recovratio, date=max(date))  %>%
  gather(type,ratio,deathratio, recovratio, illratio) %>%
  ggplot(aes(x=country,ratio, fill=type))+geom_bar(stat="identity") 



# -------------
# population count by country : 

#
## Load the population file

setwd("~/Projects/cov19/COVID-19/")
population_data <- read.csv(file="population_by_country.csv", header = TRUE)

population_data<-population_data %>% select(Country..or.dependency., Population..2020.) %>% setNames(c("country","population"))
population_data <- population_data %>% mutate(population = parse_number(as.character(population)))
str(population_data)

#
## join time_series and population

# levels(time_series$country) [! levels(time_series$country) %in% levels(population_data$country) ]
# 
# levels(population_data$country) %>% str_subset(pattern = "Ta")

time_series <- time_series %>% mutate(country=recode(country, "Congo (Brazzaville)"="Congo", "Congo (Kinshasa)"="DR Congo", 
                                      "Cote d'Ivoire"="Côte d'Ivoire", "Korea, South"="South Korea",
                                      "US"="United States", "Taiwan"="Taïwan"))

time_series <-  left_join(time_series, population_data, by="country") %>% mutate(countPerThousand=count/population*1000)

# time_series %>% filter(is.na(countPerThousand)) %>% select (country)%>% unique()
# time_series %>% select (country)%>% unique() %>% arrange %>%view

#
# PLOT : evol temporelle pour 100 Khab depuis 20 fev. pays europe + US 
#
time_series %>% group_by(date,country, type)%>%
  summarize(total=sum(countPerThousand))%>%
  filter(country %in% c("France","United Kingdom","Spain","Italy","United States","Germany") &
           date>="2020-02-20") %>% 
  ggplot(aes(x=date,y=total,colour=type))+
  geom_line(na.rm=TRUE)+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~country, ncol = 3)

# a table with only the latest data

last_cov  <- time_series %>% 
  group_by(country, type)%>%
  filter(date==max(date)) %>% 
  summarize(count=sum(count),date=max(date), countPer100K=sum(countPerThousand)*100, population=mean(population)) %>%
  ungroup()
         
# PLOT : nombre de morts pour 100.000 hab, derniers chiffres, 30 premiers pays, filtre population>200K hab
last_cov %>% 
  filter( type=="Deaths" & !is.na(countPer100K) & population>200000) %>% 
  mutate(country=reorder(country, countPer100K)) %>%
  top_n(wt = countPer100K, n=30) %>%
  ggplot(aes(country,countPer100K)) +
  geom_col()  +
  ggtitle("30 pays les plus impactés par COVID19 (pour 100.000 habitants)" ) +
  coord_flip() 

#-------------------------------------------
# Global Historical Climatology Network - Daily (GHCN-Daily), Version 3
#

### load and transform climate data

# D = 11 character station identification code
# YEAR/MONTH/DAY = 8 character date in YYYYMMDD format (e.g. 19860529 = May 29, 1986)
# ELEMENT = 4 character indicator of element type 
# DATA VALUE = 5 character data value for ELEMENT 
# M-FLAG = 1 character Measurement Flag 
# Q-FLAG = 1 character Quality Flag 
# S-FLAG = 1 character Source Flag 
# OBS-TIME = 4-character time of observation in hour-minute format (i.e. 0700 =7:00 am)
# 
# See section III of the GHCN-Daily readme.txt file for an explanation of ELEMENT codes and their units as well as the M-FLAG, Q-FLAGS and S-FLAGS.
# 
# The OBS-TIME field is populated with the observation times contained in NOAA/NCDC’s Multinetwork Metadata System (MMS).  

setwd("~/Projects/cov19/COVID-19/")
filename<-"GHCN-2020.csv"
file2 <- "ghcnd-countries.txt"

climate_raw <- read_csv(filename, col_names = c("station_id","date","data_type","value","mflag","qflag","sflag","obs_time"))
countries <- read_csv(file2, col_names = c("code"))
countries <- countries %>% separate(code, c("code","name"), sep = " ", extra = "merge")
  
climate <- climate_raw %>% mutate(code=substr(station_id,1,2))  
# unique(climate$code)
# unique(countries$code)
climate <- climate %>% left_join(., countries, by="code")

climate <- climate %>% 
  filter(data_type %in% c("TMIN","TMAX") )  %>%
  spread(key = data_type, value = value) %>%
  group_by(date, code,name) %>%
  summarize(tmin=mean(TMIN, na.rm = TRUE), tmax=mean(TMAX, na.rm = TRUE)) %>% 
  mutate(tmin=ifelse(tmin=="NaN",tmax,tmin), tmax=ifelse(tmax=="NaN",tmin,tmax))

climate <- climate %>% ungroup() %>% mutate(tmin = tmin/10, tmax= tmax/10, tmid = (tmax+tmin)/2 , date = as.Date(as.character(date),format="%Y%m%d"))

###  join time_series and climate

# last temperature data 
last_temp <- climate %>% group_by(name) %>% filter(date==max(date)) %>% rename(country=name)

left_join(last_cov, last_temp, by="country") %>% filter(is.na(tmid)) %>% select(country)%>%unique %>% pull
# "South Korea", "Congo", "Côte d'Ivoire",  "Liberia", Yemen", "Taiwan*", "Kosovo", "Burundi", "Czechia"  
# todo

last_cov_temp <- left_join(last_cov, last_temp, by="country")

# PLOT : death ratio against mid temperature
last_cov_temp %>% filter(type=="Deaths" & !is.na(tmid))%>% 
  ggplot(aes(tmid, countPer100K)) + geom_point() 

