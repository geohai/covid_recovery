rm(list=ls())
setwd("~/Dropbox/nerd_stuff/postdoc/code/spatial_recovery/scripts/")

#### libraries ####
if (!require(dplyr)) install.packages("dplyr")

#### data dir ####
# CHANGE THIS TO THE OUTPUT DIR WHERE THE CSVs ARE! #
data_dir = "/media/benjamin/blue_benny/foot_traffic_data/Outputs/"

MSA_list = c("New York", "Los Angeles", "Chicago", "Dallas", "Houston",
             "Washington", "Philadelphia", "Miami", "Atlanta", "Boston",
             "Phoenix", "San Francisco", "Riverside", "Detroit", "Seattle",
             "Minneapolis", "San Diego", "Tampa", "Denver", "Baltimore")

data_all_summer = data.frame()

for (city in MSA_list){
  #### loading csv ####
  data_MSA = read.csv(paste0(data_dir, city, ".csv"))
  
  #### remove rows with NAs in naics ####
  data_MSA = data_MSA[!is.na(data_MSA$naics),]
  
  #### replacing NA values in other cols with zeros ####
  data_MSA[is.na(data_MSA)] = 0
  
  #### add a day to the date column and make it a date ####
  data_MSA$date_start = as.Date(paste0("01-", data_MSA$date_start), "%d-%m-%Y")
  
  #### remove data prior to Jan 2019 ####
  after_2018 = format(data_MSA$date_start, format="%Y")!="2018"
  data_MSA = data_MSA[after_2018,]
  
  #### change GEOID and above_median_income variables to factors ####
  data_MSA$above_median_income = factor(data_MSA$above_median_income)
  data_MSA$GEOID = factor(data_MSA$GEOID)
  
  #### filter by summer months ####
  summer_months = c("May", "June", "July", "August", "September")
  summer_rows = months(data_MSA$date_start) %in% summer_months
  data_MSA_summer = data_MSA[summer_rows,]
  
  #### create month variable ####
  data_MSA_summer$month = months(data_MSA_summer$date_start)
  
  #### create year variable ####
  data_MSA_summer$year = format(data_MSA_summer$date_start, format="%Y")
  
  #### create variable for MSA ####
  data_MSA_summer$MSA = city
  
  #### append this MSA to the others ####
  data_all_summer = rbind(data_all_summer, data_MSA_summer)
}

data_all_summer$MSA = factor(data_all_summer$MSA)
data_all_summer$year = factor(data_all_summer$year)
data_all_summer$month = factor(data_all_summer$month)
glimpse(data_all_summer)

################################################################################
################################################################################
################################################################################
naics_list = sort(unique(data_all_summer$naics))
naics_list = c(44, 45)
for (code in naics_list){

  #### reduce data to individual naics ####
  print(paste("naics: ", code))
  data_all_summer_naics = data_all_summer[data_all_summer$naics==code,]
  
  if (nrow(data_all_summer_naics) < 1000){
    print("skipping naics because too few samples")
    next
  }
  
  #### Model 1: Basic Regression ####
  # Y = beta0 + beta1*is2020 + beta2*is2021 + factor(month) + factor(MSA) + eps
  fit_1 = lm(visitors ~ year + month + MSA,
             data=data_all_summer_naics)
  print(summary(fit_1))

  #### Model 2: Add interactions ####
  # Y = beta0 + beta1*is2020 + beta2*is2021 + beta3*below_med_inc +
  #     beta4*below_med_inc:is2020 + beta5*below_med_inc:is2021 +
  #     factor(month) + factor(MSA) + eps
  fit_2 = lm(visitors ~ year + above_median_income + above_median_income:year +
               month + MSA,
             data=data_all_summer_naics)
  print(summary(fit_2))
  
  #### Model 3: change MSA to Census Tract ####
  # Y = beta0 + beta1*is2020 + beta2*is2021 + beta4*below_med_inc:is2020 + 
  #     beta5*below_med_inc:is2021 + factor(month) + factor(census_tract) + eps
#  fit_3 = lm(visitors ~ year + above_median_income:year + month + GEOID,
    #         data=data_all_summer_naics)
#  print(summary(fit_3))
  print("-------------------------------------------------------------")
}


