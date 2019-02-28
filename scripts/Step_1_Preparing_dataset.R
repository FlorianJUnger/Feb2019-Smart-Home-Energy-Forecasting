# Preparing data set
# Florian UNGER
# 3/2/2019

### Packages 

install.packages(c("RMySQL","lubridate","cowplot","imputeTS",
                   "fracdiff","uroot","lattice","grid","chron",
                   "ggplot","padr","zoo","forecast","scales","opera",
                   "forecastHybrid", "DBI", "rstudioapi"))

pacman::p_load(RMySQL, dplyr, zoo, padr, imputeTS, 
               uroot,lubridate,ggplot, maps, cowplot, 
               plotly,forecast, scales, forecast, padr, 
               opera, forecastHybrid, rstudioapi)

### Github

current_path = rstudioapi::getActiveDocumentContext()$path #save working directory
setwd(dirname(current_path))
setwd("..")

### Creating datasets

con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

dbListTables(con) #List the tables contained in the database

yr2006 <- dbGetQuery(con, "SELECT * from yr_2006") #Get data from the specific years
yr2007 <- dbGetQuery(con, "SELECT * from yr_2007")
yr2008 <- dbGetQuery(con, "SELECT * from yr_2008")
yr2009 <- dbGetQuery(con, "SELECT * from yr_2009")
yr2010 <- dbGetQuery(con, "SELECT * from yr_2010")
YearAll <- bind_rows(     #Combine 2007,2008, 2009, 2010 to a common df
  yr2007, yr2008, yr2009, yr2010
)

#Renaming columns

# YearAll <- plyr::rename(YearAll, Kitchen = Sub_metering_1,
#                         Laundry = Sub_metering_2, 
#                         HVAC = Sub_metering_3,
#                         Active = Global_active_power,
#                         Reactive = Global_reactive_power, 
#                         Voltage = voltage,
#                         Intensity = Global_intensity)

colnames(YearAll)[which(names(YearAll) == "Sub_metering_1")] <- "Kitchen"
colnames(YearAll)[which(names(YearAll) == "Sub_metering_2")] <- "Laundry"
colnames(YearAll)[which(names(YearAll) == "Sub_metering_3")] <- "HVAC"
colnames(YearAll)[which(names(YearAll) == "Global_active_power")] <- "Active"
colnames(YearAll)[which(names(YearAll) == "Global_reactive_power")] <- "Reactive"
colnames(YearAll)[which(names(YearAll) == "voltage")] <- "Voltage"
colnames(YearAll)[which(names(YearAll) == "Global_intensity")] <- "Intensity"

#Creating Datetime in the Posixct format
YearAll$DateTime <- lubridate::ymd(YearAll$Date)
class(YearAll$Date)
YearAll$DateTime <- lubridate::ymd_hms(paste(YearAll$Date, YearAll$Time))
class(YearAll$Datetime)

YearAll <- YearAll[,c(    #moving the DateTime to the first column
  ncol(YearAll), 1:(ncol(YearAll)-1)
)]

YearAll$year <- year(YearAll$DateTime) #Create attribute with lubridate
YearAll$month <- month(YearAll$DateTime)
YearAll$day <- day(YearAll$DateTime)
YearAll$week <- week(YearAll$DateTime)

#change to kilowatt/hour
YearAll$Active <- YearAll$Active/60
YearAll$Active <- YearAll$Active*0.1472

#change to kilowatt watt/hour
YearAll$Reactive <- YearAll$Reactive/60
YearAll$Kitchen <- YearAll$Kitchen/1000
YearAll$Laundry <- YearAll$Laundry/1000
YearAll$HVAC <- YearAll$HVAC/1000

#calculate other electricity usage
YearAll$Other <- 
  YearAll$Active-YearAll$Kitchen-YearAll$Laundry-YearAll$HVAC
