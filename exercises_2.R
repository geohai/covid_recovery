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

useful_naics = c(44, 45, 54, 61, 62, 72, 81)
summer_months = c("May", "June", "July", "August", "September")

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
  
  #### changing the above_median variable to words for readability ####
  data_MSA$SES = if_else(data_MSA$above_median_income==1, "above", "below")
  
  #### filter by summer months ####
  summer_rows = months(data_MSA$date_start) %in% summer_months
  data_MSA_summer = data_MSA[summer_rows,]
  
  #### filter by useful naics ####
  naics_we_need = data_MSA_summer$naics %in% useful_naics
  data_MSA_summer_naics = data_MSA_summer[naics_we_need, ]
  
  #### create month variable ####
  data_MSA_summer_naics$month = months(data_MSA_summer_naics$date_start)
  
  #### create year variable ####
  data_MSA_summer_naics$year = format(data_MSA_summer_naics$date_start,
                                      format="%Y")
  
  #### create variable for MSA ####
#  data_MSA_summer_naics$MSA = city
  
  #### create variable for MSA x SES ####
  data_MSA_summer_naics$MSA_SES = paste0(city,
                                         "_",
                                         data_MSA_summer_naics$SES,
                                         "_median")

  #### append this MSA to the others ####
  data_all_summer = rbind(data_all_summer, data_MSA_summer_naics)
}

#### change variables to factors ####
data_all_summer$MSA_SES = factor(data_all_summer$MSA_SES)
data_all_summer$year = factor(data_all_summer$year)
data_all_summer$month = factor(data_all_summer$month)

#### create variables for year x SES ####
data_all_summer$above_median_2020 = factor(if_else(data_all_summer$year==2020 &
                                                   data_all_summer$SES=="above",
                                                   1,
                                                   0))

data_all_summer$above_median_2021 = factor(if_else(data_all_summer$year==2021 &
                                                   data_all_summer$SES=="above",
                                                   1,
                                                   0))

#### have a look at what's happened ####
glimpse(data_all_summer)


################################################################################
########################### RETAIL SECTOR ######################################
################################################################################

naics_retail = c(44, 45)

#### filter by retail naics ####
retail_rows = data_all_summer$naics %in% naics_retail
data_all_summer_retail = data_all_summer[retail_rows,]

#### Model 1: Basic Regression ####
# Y = beta0 + beta1*is2020 + beta2*is2021 + FE(month) + FE(MSA-SES) + eps
fit_1 = lm(log(visitors) ~ year + month + MSA_SES,
           data=data_all_summer_retail)
summary(fit_1)

 #### Model 2: Add interactions ####
# Y = beta0 + beta1*is2020 + beta2*is2021 + beta3*below_med_inc +
#     beta4*below_med_inc:is2020 + beta5*below_med_inc:is2021 +
#     FE(month) + FE(MSA-SES) + eps
fit_2 = lm(log(visitors) ~ year + above_median_2020 + above_median_2021 + month
           + MSA_SES,
           data=data_all_summer_retail)
summary(fit_2)

 ################################################################################
########################## SERVICES SECTOR #####################################
################################################################################

naics_services = c(54, 71, 72, 81)

#### filter by retail naics ####
services_rows = data_all_summer$naics %in% naics_services
data_all_summer_services = data_all_summer[services_rows,]

#### Model 1: Basic Regression ####
fit_1 = lm(log(visitors) ~ year + month + MSA_SES,
           data=data_all_summer_services)
summary(fit_1)


#### Model 2: Add interactions ####
fit_2 = lm(log(visitors) ~ year + above_median_2020 + above_median_2021 + month
           + MSA_SES,
           data=data_all_summer_services)
summary(fit_2)


################################################################################
################ EDUCATION SERVICES SECTOR ############################
################################################################################

naics_ed = c(61)

#### filter by retail naics ####
ed_rows = data_all_summer$naics %in% naics_ed
data_all_summer_ed = data_all_summer[ed_rows,]

#### Model 1: Basic Regression ####
fit_1 = lm(log(visitors) ~ year + month + MSA_SES,
           data=data_all_summer_ed)
summary(fit_1)

#### Model 2: Add interactions ####
fit_2 = lm(log(visitors) ~ year + above_median_2020 + above_median_2021 + month
           + MSA_SES, data=data_all_summer_ed)
summary(fit_2)


################################################################################
################ HEALTH SERVICES SECTOR ############################
################################################################################

naics_health = c(62)

#### filter by retail naics ####
health_rows = data_all_summer$naics %in% naics_health
data_all_summer_health = data_all_summer[health_rows,]

#### Model 1: Basic Regression ####
fit_1 = lm(log(visitors) ~ year + month + MSA_SES,
           data=data_all_summer_health)
summary(fit_1)

#### Model 2: Add interactions ####
fit_2 = lm(log(visitors) ~ year + above_median_2020 + above_median_2021 + month
           + MSA_SES, data=data_all_summer_health)
summary(fit_2)
