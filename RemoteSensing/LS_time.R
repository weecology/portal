library(plyr)


### FUNCTION

convert_julianday_to_monthyr = function(Year, JulianDay){

  
  combined <- paste(Year, JulianDay, sep="-")
  full_date = as.Date(strptime(combined, format="%Y-%j"), format="%m-d-%Y")
  date = as.Date(full_date, format="%Y-%j")
  strftime(date, "%Y-%m")
}


### MAIN CODE

data = read.csv(file = "Landsat_NDVI_adj_allsats.csv")

# Add time identifiers
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
