## Download files


## To refresh COVID project
##
# cd /Users/foucart/Projects/cov19/COVID-19
# git pull


#-------------------------------------------
# To download data of Population by country
#

## get the data from WEB and save to FILE
# 
library(rvest)
url <- "https://www.worldometers.info/world-population/population-by-country/"
pbc <- read_html(url)
class(pbc)
pbc
# html_nodes() extracts all nodes of different types
tab <- pbc %>% html_nodes("table")
tab <- tab[[1]]

# html_table() converts an HTML table to a data frame.
tab <- tab %>% html_table
class(tab)
tab
setwd("~/Projects/cov19/COVID-19/")
write.csv(x=tab, file="population_by_country.csv")

#-------------------------------------------
# To download data of Global Historical Climatology Network - Daily (GHCN-Daily), Version 3
#

### download and unzip files
library(R.utils)

setwd("~/Projects/cov19/COVID-19/")

url<-"https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/2020.csv.gz"

filename<-"GHCN-2020.csv.gz"
download.file(url, filename)
gunzip(filename, ext="gz")
filename<-"GHCN-2020.csv"

file2 <- "ghcnd-countries.txt"
url<-"https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-countries.txt"
download.file(url, file2)

##
