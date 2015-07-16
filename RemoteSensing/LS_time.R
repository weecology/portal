library(plyr)

### FUNCTION

add_month = function(df){
  
  # takes the Julian Day from the dataframe and creates a column in the
  # dataframe with the month of the NDVI value
  
  full_date <- paste(df$Year, df$JulDay, sep="-")
  date = as.Date(strptime(full_date, format="%Y-%j"), format="%m-%d-%Y")
  as.numeric(format(date, format = "%m")) 
}

### MAIN CODE

data = read.csv(file = "Landsat_NDVI_adj_allsats.csv")

# Add time identifiers
data$month = add_month(data)

write.csv(month_scale, "monthly_NDVI.csv")
# monthly_NDVI: median NDVI from all images from that month. N is the number
# of images used to calculate the median. 