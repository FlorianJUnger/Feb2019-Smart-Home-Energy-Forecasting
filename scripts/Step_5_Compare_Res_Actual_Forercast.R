source("Step_4_Comparing_different_forecasting_algos.R")

### Compare forecasts models

#Holt- Winters
weeks_2007_2010 <- as.vector(week_msts_Act)
weeks_2010.5 <- weeks_2007_2010 [157:205]
res_HW_2010 <- weeks_2010.5 - HW.forecast.v1$mean
checkresiduals(res_HW_2010)

#Arima 
weeks_2010.4 <- weeks_2007_2010 [157:204]
res_Arima_2010 <- weeks_2010.4 - AAr.forcast.v1$mean
checkresiduals(res_Arima_2010)

#Snaive
res_Snaive_2010 <- weeks_2010.5 - Snaivefit1$mean
checkresiduals(res_Snaive_2010)

#Naive
res_Naive_2010 <- weeks_2010.5 - modelNaive$mean
checkresiduals(res_Naive_2010)

#Hybrid v1
res_H1_2010 <- weeks_2010.5 - ModelHybridfit1$mean 
checkresiduals(res_H1_2010)

#SuperHybrid v1
res_SH_2010 <- weeks_2010.4 - z 
checkresiduals(res_SH_2010)

#Accuracy, ACF, Residual Analysis -> SNaive, Hybrid1, SuperHyb are the best

### Actual Forecast

# Model I: ARIMA 
FC.Arima.para <-auto.arima(week_msts_Act) #apply total to get seasonality & parameters 
FC.modelAAr1 <- Arima(week_msts_Act,order = c(1,0,1),seasonal = c(1,0,1)) #model with good parameters
FC.AAr.forcast.v1 <- forecast(FC.modelAAr1, h = 10) #create forecaset for the next 49 weeks
plot(FC.AAr.forcast.v1) 

# Model II: HW
FC.modelHW <- HoltWinters(week_msts_Act) 
FC.HW.forecast.v1 <- forecast(FC.modelHW, h = 10)
plot(FC.HW.forecast.v1)

# Model III: Naive
FC.modelNaive <- naive(week_msts_Act, h = 10) 
plot(FC.modelNaive)

# Model IV: SNAIVE
FC.Snaivefit1 <- snaive(week_msts_Act, h = 10)                              
plot(FC.Snaivefit1)              

# Model V: Hybrid
FC.ModelHybridm <- hybridModel(week_msts_Act)
FC.hybridmod.fit1 <- forecast(FC.ModelHybridm, h=10)
plot(FC.hybridmod.fit1, main = "Title")

#compare reality and the forecast for 2010/2011
autoplot(FC.AAr.forcast.v1, series="Arima", PI=FALSE,cex=2) +
  autolayer(FC.HW.forecast.v1, series="HW", PI=FALSE,cex=2)+
  autolayer(FC.Snaivefit1, series="S-Naive", PI=FALSE,cex=2)+
  autolayer(FC.hybridmod.fit1, series="Hybrid", PI=FALSE,cex=2)+
  xlim(c(2010.85,2011.1))+
  xlab("Years") + ylab("Total costs per week in EUR")+ggtitle("Forecast 10 weeks with different models")

# Forecasted Values

ForecastedMeans.df <- as.data.frame(FC.AAr.forcast.v1$mean)
ForecastedMeans.df$HW <- (FC.HW.forecast.v1$mean)
ForecastedMeans.df$Naive <- (FC.modelNaive$mean)
ForecastedMeans.df$Snaive <- (FC.Snaivefit1$mean)
ForecastedMeans.df$Hybrid <- (FC.hybridmod.fit1$mean)
colnames(ForecastedMeans.df)[1] <- "Arima"

write.csv(ForecastedMeans.df,file = "Forecasted.csv")
write.csv(FC.Snaivefit1,file = "Days.csv")
