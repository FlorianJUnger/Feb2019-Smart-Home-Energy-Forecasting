source("Step_2_Padding_imputing.R")

### Group by 

aggregated_df <- c() #aggregated DF

granularity <- c("year", "month","week", "day", "hour")

for(g in granularity){ #grouping them by year, month, week, day
  aggregated_df[[g]] <- YAfill %>%
    group_by(DateTime=floor_date(DateTime, g)) %>%
    dplyr::summarize_at(vars(
      Kitchen,
      Laundry,
      HVAC,
      Active,
      Other),
      funs(sum))
}

ts_year <- ts(aggregated_df[["year"]], start = c(2007,1), frequency = 1)
ts_month <- ts(aggregated_df[["month"]], start = c(2007,1), frequency = 12)
ts_week <- ts(aggregated_df[["week"]], start = c(2007,1), frequency = 52)
ts_day <- ts(aggregated_df[["day"]], start = c(2007,1), frequency = 365)
ts_hour <- ts(aggregated_df[["hour"]], start = c(2007,1), frequency = 8760)
msts_day <- msts(aggregated_df[["day"]], start = c(2007,1), seasonal.periods = c(7,30,365.25))
msts_week_real <- aggregated_df[["week"]] %>% arrange(DateTime)
msts_week <- msts(aggregated_df[["week"]], seasonal.periods = c(4,52), start = c(2007,1))

### Creating time series 

#creating list of time series; e.g.: year_ts contains ts of Kit, Lau, HV, Ac, Oth
year_ts <- list(vector(length = 5))
month_ts <- list(vector(length = 5))
week_ts <- list(vector(length = 5))
day_ts <- list(vector(length = 5))

appliances <- c("Kitchen","Laundry", "HVAC", "Active", "Other") 

#ts creation with ts
for (a in appliances){ #year - ts
  year_ts[[a]] <- ts_year[,a]
}
for (a in appliances){ #month - ts
  month_ts[[a]] <- ts_month[,a]
}
for (a in appliances){ #week - ts
  week_ts[[a]] <- ts_week[,a]
}
for (a in appliances){ #day - ts
  day_ts[[a]] <- ts_day[,a]
}

#Granularity Decision
gran_year <- plot(ts_year, main="Granularity Year")
gran_month <- plot(ts_month, main="Granularity Month")
gran_week <- plot(ts_week, main="Granularity Week")
gran_day <- plot(ts_day, main="Granularity Day")
Granularityplots <- plot_grid(gran_year, gran_month,gran_week, gran_day)
Granularityplots

#ACreate TS for Active energy 
day_msts_Act <- msts_day[,"Active"] #day & msts
plot(day_msts_Act)
week_msts_Act <- msts_week[,"Active"] #week & msts
plot(week_msts_Act)
day_ts_Act <- ts_day[,"Active"] #day & ts
plot(day_ts_Act)
week_ts_Act <- ts_week[,"Active"] #week & ts
plot(week_ts_Act)

### Decomposing the time series 

# DAY 
decomday <- list()
appliances <- c("Kitchen","Laundry", "HVAC", "Active", "Other") 

for (a in appliances) {
  decomday[[a]] <- stl(day_ts[[a]], s.window = 7, s.degree = 1,t.window = NULL
  )}

# WEEK 
decomweek <- list()
for (a in appliances) {
  decomweek[[a]] <- stl(week_ts[[a]], s.window = 7, s.degree = 1, t.window = NULL
  )}

# MONTH 
decommonth <- list()
for (a in appliances) {
  decommonth[[a]] <- stl(month_ts[[a]], s.window = 7, s.degree = 1, t.window = NULL
  )}

#Decomposing for Active
decom_wk_Act_ts <- stl(week_ts_Act,s.window = 7, #week and stl
                       s.degree = 1, t.window = NULL)
plot(decom_wk_Act_ts)

decom_day_Act_ts <- stl(day_ts_Act, s.window = 7, #day and stl
                        s.degree = 1, t.window = NULL)
plot(decom_day_Act_ts)

decom_day_Act_msts <- mstl(day_msts_Act, s.window = 7, #day and mstl
                           s.degree = 1,t.window = NULL)
plot(decom_day_Act_msts)

decom_week_Act_msts <- mstl(week_msts_Act, s.window = 7, #week and mstl
                            s.degree = 1, t.window = NULL)
plot(decom_week_Act_msts)

### Compare the models 

#Percentage of randomness 
mean(abs(remainder(decom_day_Act_msts)))/mean(day_msts_Act) #ABS of msts daily
mean(abs(remainder(decom_week_Act_msts)))/mean(week_msts_Act) #ABS of msts daily
mean(abs(remainder(decom_day_Act_ts)))/mean(day_ts_Act) #ABS of ts daily
mean(abs(remainder(decom_wk_Act_ts)))/mean(week_ts_Act) #ABS of ts daily