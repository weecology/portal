library(plyr)

### FUNCTION

add_month = function(df){
  
  # takes the Julian Day from the dataframe and creates a column in the
  # dataframe with the month of the NDVI value
  
  full_date <- paste(df$Year, df$JulDay, sep="-")
  date = as.Date(strptime(full_date, format="%Y-%j"), format="%m-%d-%Y")
  as.numeric(format(date, format = "%m")) 
}

add_season = function(month, seasons){
 ifelse(month <= max(seasons$winter_first),1,
        ifelse (month <= max(seasons$spring),2,
                ifelse(month <= max(seasons$summer),3,
                       ifelse (month <= max(seasons$fall),4,
                               1))))
}

add_season_colors = function(dfseason, colors){
 ifelse(dfseason == 1,colors$winter,
        ifelse (dfseason == 2,colors$spring,
                ifelse(dfseason == 3,colors$summer,
                       ifelse (dfseason == 4,colors$fall,
                               1))))
}
### MAIN CODE

data = read.csv(file = "Landsat_NDVI_adj_allsats.csv")

# Add time identifiers
data$month = add_month(data)

seasons = list(winter_first=1:2, spring=3:5, summer=6:8,
               fall=9:11, winter_end=12)
color_list = list(winter="blue", spring="chartreuse", 
                  summer="forestgreen", fall="brown")
data$wateryr = ifelse(data$month <= max(seasons$fall),data$Year,
                      data$Year+1)

# Summarizing Data at different scales

month_scale = ddply(data, c("Year", "month"), summarise,
                    N = sum(!is.na(NDVI)),
                    median = median(NDVI, na.rm=TRUE),
                    sd = sd(NDVI, na.rm=TRUE))
month_scale$season = add_season(as.vector(month_scale$month),seasons)
data_colors = add_season_colors(as.vector(month_scale$season), 
                                       color_list)

time = as.vector(month_scale$Year) + as.vector(month_scale$month)/12
plot(time, month_scale$median, type='o', col=data_colors)


year_scale = ddply(month_scale, ~ Year, summarise,
                    N = sum(!is.na(median)),
                    median = median(median, na.rm=TRUE),
                    sd = sd(median, na.rm=TRUE))
plot(year_scale$Year, year_scale$median)
lines(year_scale$Year, year_scale$median, type = "o", col="black")
