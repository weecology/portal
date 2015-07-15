#### The goal of this code is to create a single LANDSAT file
#### from the various LANDSAT satellites. This involves the following 
#### steps which are all accomplished in this file:
####  1) Import raw data files
####  2) Check where the data overlaps between satellites and extract that data
####  3) See if overlap data is statistically indistinguishable or not
####  4) Correct satellite data to be comparable if necessary
####  5) Bind corrected files together to create 1 timeseries
#### This is not completely automated yet. There are a couple of hand processing
#### steps in this workflow. Determining Which satellites overlap was done 
#### by eye first and regressions are only done for those satelite pairs.
#### Also, lining up individual passes between satellites to get the passes
#### done closest in time is also done by hand because I couldn't figure out
#### how to automate that step.

library(dplyr)

### FUNCTIONS

create_fractional_date = function(year,juldate) {
  # Converts an integer year and julian day into a the format Year.fraction_of_year
  
  year + juldate/366
    
}

turn_framename_to_string <- function(v1) {
  
  # takes the variable name given and returns that variable name as a string
  
  deparse(substitute(v1))
}

create_subsetfile = function(df, yr){
  
  # creates a subset of the NDVI file that is only for the overlap of years
  # that occurs in two different files
  
  frame = data.frame()
  for (item in yr) {
    subsetdata = df[which(df$Year == item),]
    reduced_data = data.frame(subsetdata$Year, subsetdata$JulianDay, subsetdata$Median)
    frame = rbind(frame, reduced_data)
  }
  return(frame)
}

find_overlap_yr = function(df1, df2) {
  
  # find which years are in both dataframes
  
  intersect(df1$Year, df2$Year)
}

make_overlap_file = function(df1, df2, sat1, sat2){
  
  # creates .csv files that can be used to line up NDVI
  # data from different satellites. Needs to be given
  # the satellite files and the names of the satellites.
  # This function called 2 other functions: find_overlap_yr()
  # and create_subsetfile()
  
  yr = find_overlap_yr(df1, df2)
  data1 = create_subsetfile(df1, yr)
  colnames(data1) = c(paste(sat1,"year"), paste(sat1,"Jul"), paste(sat1,"NDVI"))
  data2 = create_subsetfile(df2, yr)
  colnames(data2) = c(paste(sat2,"year"), paste(sat2,"Jul"), paste(sat2,"NDVI"))
  filename1 = paste("Subset_",yr[1],"_",sat1,".csv")
  filename2 = paste("Subset_",yr[1],"_",sat2,".csv")
  write.csv(data1, file = paste("./Transitionalfiles/",filename1, sep=""))
  write.csv(data2, file = paste("./Transitionalfiles/",filename1, sep=""))
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

make_core_version = function(df, dfNDVI){
  LS_NDVI = data.frame(df[[4]], df[[5]], dfNDVI)
  colnames(LS_NDVI) = c("Year", "JulDay", "NDVI")
  return(LS_NDVI)
}
### MAIN CODE

LS5 = read.csv(file.path("./rawdata",'LandsatNDVI_L5.csv'))
LS7_off = read.csv(file.path("./rawdata",'LandsatNDVI_L7_SLCoff.csv'))
LS7_on = read.csv(file.path("./rawdata",'LandsatNDVI_L7_SLCon.csv'))
LS8 = read.csv(file.path("./rawdata",'LandsatNDVI_L8.csv'))

# creates files of only the data that overlaps between
# satellites ov interest

make_overlap_file(LS5,LS7_off, "L5", "L7off")
make_overlap_file(LS5,LS7_on, "L5", "L7on")
make_overlap_file(LS7_off, LS8, "L7off", "L8")

# files written in above code are lined up by hand (I know, but it was beyond me)
# and reimported as single datasets for each comparison

LS5_LS7off = read.csv(file.path("./Transitionalfiles/","Overlap_L5_L7off.csv"))
LS5_LS7on = read.csv(file.path("./Transitionalfiles/","Overlap_L5_L7on.csv"))
LS7off_L8 = read.csv(file.path("./Transitionalfiles/","Overlap_L7off_L8.csv"))

# Conduct linear regression to see if data needs correcting
# This occurrs as a multistep process. 
#   Step 1: Compare LS5 and L7off, Correct data in dataframe LS7_off if needed

LS5_Ls7off_offset = Calc_andcheck_satellite_corrections(LS5_LS7off$L5.NDVI, LS5_LS7off$L7off.NDVI)
LS7_off$new_NDVI = Apply_correction(LS7_off$Median, LS5_Ls7off_offset)

#   Step 2: Compare LS5 and L7on. Correct data in dataframe LS7_on if needed

LS5_Ls7on_offset = Calc_andcheck_satellite_corrections(LS5_LS7on$L5.NDVI, LS5_LS7on$L7on.NDVI)
LS7_on$new_NDVI = Apply_correction(LS7_on$Median, LS5_Ls7on_offset)

#   Step 3: Adjust LS7off in overlap file and compare with LS8. Adjust
#           LS8 if needed.

adj_LS7off = LS7off_L8$L7off.NDVI - LS5_Ls7off_offset

results = NDVI_regression(adj_LS7off, LS7off_L8$L8.NDVI)
LS7off_Ls8_offset = Calc_andcheck_satellite_corrections(adj_LS7off, LS7off_L8$L8.NDVI)
LS8$new_NDVI = Apply_correction(LS8$Median, LS7off_Ls8_offset)

# Strip raw datafiles to the core data to be used and bind the data together in one file

NDVI_L5 = make_core_version(LS5, LS5$Median)
NDVI_L7off = make_core_version(LS7_off, LS7_off$new_NDVI)
NDVI_L7on = make_core_version(LS7_on, LS7_on$new_NDVI)
NDVI_L8 = make_core_version(LS8, LS8$new_NDVI)

NDVI_all = rbind_list(NDVI_L5, NDVI_L7off, NDVI_L7on, NDVI_L8)

write.csv(NDVI_all, file = "Landsat_NDVI_adj_allsats.csv")
