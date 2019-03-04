source("Step_1_Preparing_dataset.R")

### Padding and imputating data (I/II) 

YAwithNA <- pad(YearAll, break_above = 3) #create NA rows
allNAYA <- YAwithNA[is.na(YAwithNA$Kitchen)== TRUE,] #create new DF with NAs

### Visualising Gaps

#NA duration 
allNAYA$Date <- as.Date(allNAYA$DateTime) 
NAs <- allNAYA %>% group_by(Date, Kitchen) %>% dplyr::summarise(count=n()) #NAs per Day
NAs$year <- year(NAs$Date) #Create attribute with lubridate

#NAs 2007
NAsplot <- ggplot(NAs, aes(count))+
  stat_bin(binwidth = 30, fill = "lightblue", color = "Black")+
  stat_bin(binwidth = 30, geom="text", aes(label=ifelse(..count.. == 0, "", ..count..)),cex=5,vjust=-0.5)+
  xlab("Missing values in Min")+ylab("Frequency")+ggtitle("NAs by frequency 2007")+facet_grid(~year)

#Creating a heatmap to plot the values 
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")
calendarHeat(YearAll$DateTime, YearAll$Active)

### Padding and imputating data with different techniques depending on the length

YAfill <- zoo::na.locf(YAwithNA, maxgap = 1000, na.rm = FALSE) #replace all NAs with previous one
count(YAfill[is.na(YAfill$Kitchen) == TRUE,]) #~300 errors less
YAfill <- na.kalman(YAwithNA)
count(YAfill[is.na(YAfill$Kitchen) == TRUE,])
