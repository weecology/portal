library(plyr)

convert_julianday_to_monthyr = function(Year, JulianDay){
  
  # Converts separate Julian Day and Yr COlumns from a 
  # dataframe to a single Year-month format
  
  combined <- paste(Year, JulianDay, sep="-")
  full_date = as.Date(strptime(combined, format="%Y-%j"), format="%m-d-%Y")
  date = as.Date(full_date, format="%Y-%j")
  strftime(date, "%Y-%m")
}

NDVI_regression = function(x_NDVI, y_NDVI){
  
  # takes two lists of NDVI data and returns
  # the intercept and intercept pvalue for the regression
  
  NDVI.lm = lm(y_NDVI ~ x_NDVI)
  stat.coef = summary(NDVI.lm)$coefficients
  intercept = stat.coef[1,1]
  pvalue = stat.coef[1,4]
  return(c(intercept, pvalue))
}

Determine_correction_factor = function(lm_results){
  
  # uses the pvalue from NDVI_regression to determine whether the intercept
  # is significantly different from zero. If yes, then returns
  # calculated intercept. If no, then returns 0.
  
  if (lm_results[2] < 0.1){
    intercept = lm_results[1]
  } else {
    intercept = 0
  }
  return(intercept)
}

Calc_andcheck_satellite_corrections = function(x_NDVI, y_NDVI){
  
  # Takes overlap NDVI data from two satellites, checks to see if
  # these data are significantly different. Returns the correction
  # factor needed to make the satellite data more similar.
  
  results = NDVI_regression(x_NDVI, y_NDVI)
  correction_amount = Determine_correction_factor(results)
  result_wcorrection = NDVI_regression(x_NDVI, y_NDVI-correction_amount)
  if (result_wcorrection[2] > 0.1){
    correction=correction_amount
  } else {
    correction='ERROR'
  }
  return(correction)
}

Apply_correction = function(df_NDVI, correction){
  
  # Applys the appropriate NDVI correction factor 
  # to the NDVI data in the raw datafiles
  
  df_NDVI - correction
}
### MAIN CODE

data = read.csv("ModisNDVI_MOD_clean_summary.csv")
landsat = read.csv("monthly_NDVI.csv")

# Processing data from Main Processing file (LANDSAT.R) to monthly
# summaries compatible with timeseries analysis. 

data$date = convert_julianday_to_monthyr(data$year, data$julianday)
modis = ddply(data, ~ date, summarise,median = median(mean, na.rm=TRUE))

overlap_data = merge(modis, landsat, by.x = "date", by.y="Date")

Modis_LS_offset = Calc_andcheck_satellite_corrections(overlap_data$NDVI, 
                                                      overlap_data$median)
modis$median_new = Apply_correction(modis$median, Modis_LS_offset)

merged = merge(modis, landsat, by.x = "date", by.y="Date", all.y = TRUE)

na_rows = which(is.na(merged$NDVI))
merged$NDVI[na_rows] <- merged$median_new[na_rows]
