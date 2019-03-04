source("Step_3_Group_Create_Decompose_TS.R")

### Forecast 

# Creating windows 
msts.weeks.2007.09 <- window(week_msts_Act, start = c(2007,1), end = c(2009,52)) #52 as 52 weeks
msts.week.2010 <- window(week_msts_Act, start = c(2010,1))

## MODEL I: ARIMA 

Arima.parameters <-auto.arima(week_msts_Act) #apply total to get seasonality & parameters 

modelAAr1 <- Arima(msts.weeks.2007.09,order = c(1,0,1),seasonal = c(1,0,1)) #model with good parameters
AAr.forcast.v1 <- forecast(modelAAr1, h = 48) #create forecaset for the next 49 weeks
plot(AAr.forcast.v1) 

plot.ts(AAr.forcast.v1$residuals)
plotForecastErrors <- function(forecasterrors)
  
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors,na.rm=TRUE)/4
  mysd   <- sd(forecasterrors,na.rm=TRUE)
  mymin  <- min(forecasterrors,na.rm=TRUE) - mysd*5
  mymax  <- max(forecasterrors,na.rm=TRUE) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(AAr.forcast.v1$residuals)


## MODEL II: HOLT - WINTERS

modelHW <- HoltWinters(msts.weeks.2007.09) #predict 2007-2009
HW.forecast.v1 <- forecast(modelHW, h = 49)
plot(HW.forecast.v1)

plotForecastErrors(HW.forecast.v1$residuals)

## MODEL III: NAIVE 

modelNaive <- naive(msts.weeks.2007.09, h = 49) #predict 2007-2009
plot(modelNaive)

## MODEL IV: SNAIVE

Snaivefit1 <- snaive(msts.weeks.2007.09, h = 49)                              
plot(Snaivefit1)                             

## MODEL V: Super Hybrid

ARIMA_opera <- forecast(auto.arima(msts.weeks.2007.09), h=49)
HW_opera_model <- HoltWinters(msts.weeks.2007.09)
HW_opera <- forecast(HW_opera_model, h=49)
SNAIVE_opera <- forecast(snaive(msts.weeks.2007.09), h=49)

X <- cbind(ARIMA=ARIMA_opera$mean, SNAIVE=SNAIVE_opera$mean, HW = HW_opera$mean)

df_opera <- cbind(week_msts_Act, X)
colnames(df_opera) <- c("Active","ARIMA","SNAIVE","HW")
autoplot(df_opera)

MLpol0 <- mixture(model = "MLpol", loss.type = "square")
weights <- predict(MLpol0, X, msts.week.2010, type='weights')

z <- ts(predict(MLpol0, X, msts.week.2010, type='response'), start=c(2010,1), freq=52)
df_final_opera <- cbind(week_msts_Act, z)

autoplot(df_final_opera)
accuracy(z, msts.week.2010)

## VI: Super Hybrid upgrade

HW_opera_model <- HoltWinters(msts.weeks.2007.09)
HW_opera <- forecast(HW_opera_model, h=49)
ModelHybrid <- hybridModel(msts.weeks.2007.09)
ModelHybridfit1 <- forecast(ModelHybrid, h = 49)
SNAIVE_opera <- forecast(snaive(msts.weeks.2007.09), h=49)

X1 <- cbind(HYBRID=ModelHybridfit1$mean, SNAIVE=SNAIVE_opera$mean, HW = HW_opera$mean)

df_operav1 <- cbind(week_msts_Act, X1)
colnames(df_operav1) <- c("Active","HYBRID","SNAIVE","HW")
autoplot(df_operav1)

MLpol01 <- mixture(model = "MLpol", loss.type = "absolute")
weights1 <- predict(MLpol01, X1, msts.week.2010, type='weights') 

z1 <- ts(predict(MLpol01, X1, msts.week.2010, type='response'), start=c(2010,1), freq=52)
df_final_operav1 <- cbind(week_msts_Act, z1)

autoplot(df_final_operav1)
accuracy(z1, msts.week.2010)

## Hybrid models

ModelHybrid <- hybridModel(msts.weeks.2007.09)
ModelHybridfit1 <- forecast(ModelHybrid, h = 49)

ModelHybrid2 <- hybridModel(msts.weeks.2007.09, weights = "insample")
ModelHybridfit2 <- forecast(ModelHybrid, h = 49)

## ACCURACY

ACCAmira <- accuracy(AAr.forcast.v1, msts.week.2010)[,1:3]
ACCHW <- accuracy(HW.forecast.v1, msts.week.2010)[,1:3]
ACCNAI <- accuracy(modelNaive, msts.week.2010)[,1:3]
ACCSNAI <- accuracy(Snaivefit1, msts.week.2010)[,1:3]
ACCHybrv1 <- accuracy(ModelHybridfit1,msts.week.2010)[,1:3]
#ACCSupHyb <- accuracy(z, msts.week.2010)[,1:3]
#ACCSupHyb1 <- accuracy(z1, msts.week.2010)[,1:3]

Accuracyplot <- rbind(ACCAmira, ACCHW, ACCNAI, ACCSNAI, ACCHybrv1)
row.names(Accuracyplot) <- c("Training.ARIMA", "Test.ARIMA", "Training.HW", "Test.HW", 
                             "Training.Naive", "Test.Naive", "Training.SNaive", "Test.SNaive",
                             "Training.Hybrid","Test.Hybrid")

write.csv(Accuracyplot,file = "Accuracyplot.csv")
write.csv(Accuracyplot,file = "Accuracyplot1.csv")


#compare reality and the forecast for 2010 - SNaive & Hybridnormal are the most accurace
autoplot(week_msts_Act, cex = 2) +
  autolayer(ModelHybridfit1, series="Hybridnormal", PI=FALSE, cex = 1)+
  autolayer(HW.forecast.v1, series="HW", PI=FALSE, cex = 1)+
  autolayer(modelNaive, series="Naive", PI=FALSE, cex = 1)+
  autolayer(AAr.forcast.v1, series="Arima", PI=FALSE, cex = 1)+
  autolayer(Snaivefit1, series="S-Naive", PI=FALSE, cex = 1)+
  xlim(c(2010,2010.93))+
  xlab("Years") + ylab("Euros")+ggtitle("Forecast 2010 with different models")

