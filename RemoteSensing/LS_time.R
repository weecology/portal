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
  full_date = as.Date(strptime(combined, format="%Y-%j"), format="%m-d-%Y")
  date = as.Date(full_date, format="%Y-%j")
  strftime(date, "%Y-%m")
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

month_missingdates$current_dates = as.Date(paste(month_missingdates$date,"-",15,sep=""))
first_date = as.Date(min(month_missingdates$current_date))
end_date = as.Date(max(month_missingdates$current_date))

full = seq(from=first_date, to=end_date, by='1 month') 

all_months = data.frame(Date=full, NDVI = month_missingdates$median[match(full,month_missingdates$current_dates)])
write.csv(all_months, "Monthly_NDVI.csv")
