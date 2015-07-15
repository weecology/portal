library(plyr)

### FUNCTION

plot_season = function(df, season){
  
  # creates a plot of the entire timeseries of landsat data,
  # color coding points by season
  
  season_data = df[df$season == season, c("Date","NDVI", "colors")]
  min_year = min(df$Year)
  max_year = max(df$Year)
  if(season == 1){
    season_label = "winter"
  } else if (season == 2){
    season_label = "spring"
  } else if (season == 3){
    season_label = "summer"
  } else {
    season_label = "fall"
  }
  plot(season_data$Date, season_data$NDVI, col=season_data$colors,
       xlab=paste(season_label, "Year"), ylab=paste(season_label, "NDVI"))
  }

add_month = function(df){
  
  # takes the Julian Day from the dataframe and creates a column in the
  # dataframe with the month of the NDVI value
  
  full_date <- paste(df$Year, df$JulDay, sep="-")
  date = as.Date(strptime(full_date, format="%Y-%j"), format="%m-%d-%Y")
  as.numeric(format(date, format = "%m")) 
}

add_season = function(df){
  
  year_start = 1     #Jan1
  winter_end = 59    #Feb28
  spring_end = 151   #May31
  summer_end = 243   #Aug31
  fall_end = 334     #Nov30
  year_end = 365     #Dec31
  
  ifelse(df$JulDay <= winter_end,1,
         ifelse (df$JulDay <= spring_end,2,
                 ifelse(df$JulDay <= summer_end,3,
                        ifelse (df$JulDay <= fall_end,4,
                                1))))
}

### MAIN CODE
data = read.csv(file = "Landsat_NDVI.csv")

# Dates for cutting year into seasons

season_cutoffs = list(winter_first=1:2, spring=3:5, summer=6:8,
                      fall=9:11, winter_end=12)
winter_end = list()   
spring_end = 151   
summer_end = 243   
fall_end = 334     
year_end = 365
#Jan1#Feb28
 #May31
#Aug31
#Nov30
#Dec31
# Adding time identifiers  to dataframe

data$season = ifelse(data$JulDay <= winter_end,1,
                     ifelse (data$JulDay <= spring_end,2,
                             ifelse(data$JulDay <= summer_end,3,
                                    ifelse (data$JulDay <= fall_end,4,
                                            1))))

data$wateryr = ifelse(data$JulDay <= fall_end,data$Year,
                      data$Year+1)

data$colors = ifelse(data$season == 1,"blue",
                     ifelse (data$season == 2,"chartreuse",
                             ifelse(data$season == 3,"darkgreen", 
                                    ifelse (data$season == 4,"brown", 
                                            "blue"))))
data$season = add_season(data)
data$month = add_month(data)

# Plot all data using season as point color

plot(data$Date, data$NDVI, col=data$colors)
overall.lm = lm(data$NDVI ~ data$Date)
summary(overall.lm)

#Plot just one season
plot_season(data,1)
plot_season(data,2)
plot_season(data, 3)
plot_season(data,4)

overall_mean = mean(data$NDVI, na.rm=TRUE)
result_all = ddply(data,c("wateryr"),summarise,mean=mean(NDVI, na.rm=TRUE),
               sd=sd(NDVI, na.rm=TRUE), N = sum(!is.na(NDVI)), N_below = sum(NDVI < overall_mean, na.rm=TRUE))

result = result_all[which(result_all$N > 16),]
result$percentbelow = result$N_below/result$N

plot(result$wateryr, result$sd)
plot(result$sd ~ result$mean)
with(result, text(result$sd ~ result$mean, labels = result$wateryr), pos = 4)

plot(result$percentbelow ~ result$mean)
with(result, text(result$percentbelow ~ result$mean, labels = result$wateryr), pos = 4)

plot(result$percentbelow ~ result$sd)
with(result, text(result$percentbelow ~ result$sd, labels = result$wateryr), pos = 4)


