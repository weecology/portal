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

month_w_NAs = ddply(data, ~ date, summarise,
                    N = sum(!is.na(NDVI)),
                    median = median(NDVI, na.rm=TRUE),
                    sd = sd(NDVI, na.rm=TRUE))

month_wo_NAs = month_w_NAs[order(month_w_NAs$date),] 

NA_rows = which(is.na(month_wo_NAs$median))
previous_row = NA_rows-1
next_row = NA_rows+1

NA_row_info = data.frame(NA_rows, previous_row, next_row)

NA_row_info$previous_value = month_wo_NAs[NA_row_info[,2], c("median")]
NA_row_info$next_value = month_wo_NAs[NA_row_info[,3], c("median")]
NA_row_info$mean = (NA_row_info$previous_value + NA_row_info$next_value) /2

NA_data = data.frame(NA_row_info$NA_rows, NA_row_info$mean)
i=1
for (row in NA_row_info$NA_rows){
  month_wo_NAs[row,]$median = NA_row_info[i,]$mean
  i= i+1
}

write.csv(month_wo_NAs, "Monthly_NDVI.csv")
