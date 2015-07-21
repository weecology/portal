library(plyr)


### FUNCTION
#### This file takes the raw NDVI file that has the day-level images from
#### multiple satellites and reduces that to monthly values. There are a
#### few months that are still missing data, these are filled in by taking
#### the mean from the months flanking the missing data.

### FUNCTIONS

convert_julianday_to_monthyr = function(Year, JulianDay){

  # Converts separate Julian Day and Yr COlumns from a 
  # dataframe to a single Year-month format
  
  combined <- paste(Year, JulianDay, sep="-")
  date = as.Date(combined, format="%Y-%j")
  strftime(date, "%Y-%m")
}

add_missing_months = function(df){
  df$current_dates = as.Date(paste(df$date,"-",15,sep=""))
  first_date = as.Date(min(df$current_date))
  end_date = as.Date(max(df$current_date))
  full = seq(from=first_date, to=end_date, by='1 month') 
  data.frame(Date=full, NDVI = df$median[match(full,df$current_dates)])
}

### MAIN CODE

data = read.csv(file = "Landsat_NDVI_adj_allsats.csv")

# Processing data from Main Processing file (LANDSAT.R) to monthly
# summaries compatible with timeseries analysis. 

data$date = convert_julianday_to_monthyr(data$Year, data$JulDay)

month_missingdates = ddply(data, ~ date, summarise,
                    N = sum(!is.na(NDVI)),
                    median = median(NDVI, na.rm=TRUE),
                    sd = sd(NDVI, na.rm=TRUE))
all_months = add_missing_months(month_missingdates)
all_months$date = strftime(as.Date(all_months$Date), "%Y-%m")
all_months = data.frame(all_months$date, all_months$NDVI)
colnames(all_months) = c("Date","NDVI")

write.csv(all_months, "Monthly_Landsat_NDVI.csv")
