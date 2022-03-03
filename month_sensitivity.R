rm(list=ls())
setwd("~/Dropbox/nerd_stuff/postdoc/code/spatial_recovery/scripts/")

#### libraries ####
if (!require(dplyr)) install.packages("dplyr")

#### data dir ####
# CHANGE THIS TO THE OUTPUT DIR WHERE THE CSVs ARE! #
data_dir = "/media/benjamin/blue_benny/foot_traffic_data/Outputs/"

months = c("April", "May", "June", "July", "August", "September", "October")

MSA_list = c("New York", "Los Angeles", "Chicago", "Dallas", "Houston",
             "Washington", "Philadelphia", "Miami", "Atlanta", "Boston",
             "Phoenix", "San Francisco", "Riverside", "Detroit", "Seattle",
             "Minneapolis", "San Diego", "Tampa", "Denver", "Baltimore")
useful_naics = c(44, 45, 54, 61, 62, 72, 81)
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
  summer_rows = months(data_MSA$date_start) %in% months
  data_MSA_summer = data_MSA[summer_rows,]
  
  #### filter by useful naics ####
  naics_we_need = data_MSA_summer$naics %in% useful_naics
  data_MSA_summer_naics = data_MSA_summer[naics_we_need, ]
  
  #### create month variable ####
  data_MSA_summer_naics$month = months(data_MSA_summer_naics$date_start)
  
  #### create year variable ####
  data_MSA_summer_naics$year = format(data_MSA_summer_naics$date_start,
                                      format="%Y")
  
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

#### create variables for SES x year x month ####
data_all_summer$above_2020_may = factor(if_else(data_all_summer$year==2020 &
                                                data_all_summer$SES=="above" &
                                                data_all_summer$month=="May",
                                                1,
                                                0))
data_all_summer$above_2020_jun = factor(if_else(data_all_summer$year==2020 &
                                                data_all_summer$SES=="above" &
                                                data_all_summer$month=="June",
                                                1,
                                                0))
data_all_summer$above_2020_jul = factor(if_else(data_all_summer$year==2020 &
                                                data_all_summer$SES=="above" &
                                                data_all_summer$month=="July",
                                                1,
                                                0))
data_all_summer$above_2020_aug = factor(if_else(data_all_summer$year==2020 &
                                                data_all_summer$SES=="above" &
                                                data_all_summer$month=="August",
                                                1,
                                                0))
data_all_summer$above_2020_sep = factor(if_else(data_all_summer$year==2020 &
                                                data_all_summer$SES=="above" &
                                                data_all_summer$month=="September",
                                                1,
                                                0))
data_all_summer$above_2021_may = factor(if_else(data_all_summer$year==2021 &
                                                data_all_summer$SES=="above" &
                                                data_all_summer$month=="May",
                                                1,
                                                0))
data_all_summer$above_2021_jun = factor(if_else(data_all_summer$year==2021 &
                                                data_all_summer$SES=="above" &
                                                data_all_summer$month=="June",
                                                1,
                                                0))
data_all_summer$above_2021_jul = factor(if_else(data_all_summer$year==2021 &
                                                data_all_summer$SES=="above" &
                                                data_all_summer$month=="July",
                                                1,
                                                0))
data_all_summer$above_2021_aug = factor(if_else(data_all_summer$year==2021 &
                                                data_all_summer$SES=="above" &
                                                data_all_summer$month=="August",
                                                1,
                                                0))
data_all_summer$above_2021_sep = factor(if_else(data_all_summer$year==2021 &
                                                data_all_summer$SES=="above" &
                                                data_all_summer$month=="September",
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

#### Model Monthly Sensitivity ####
fit_1 = lm(log(visitors) ~ year + above_2020_may + above_2020_jun +
                           above_2020_jul + above_2020_aug + above_2020_sep +
                           above_2021_may + above_2021_jun + above_2021_jul +
                           above_2021_aug + above_2021_sep + month + MSA_SES,
           data=data_all_summer_retail)
summary(fit_1)

################################################################################
########################## SERVICES SECTOR #####################################
################################################################################

naics_services = c(54, 71, 72, 81)

#### filter by services naics ####
services_rows = data_all_summer$naics %in% naics_services
data_all_summer_services = data_all_summer[services_rows,]

#### Model Monthly Sensitivity ####
fit_1 = lm(log(visitors) ~ year + above_2020_may + above_2020_jun +
             above_2020_jul + above_2020_aug + above_2020_sep +
             above_2021_may + above_2021_jun + above_2021_jul +
             above_2021_aug + above_2021_sep + month + MSA_SES,
           data=data_all_summer_services)
summary(fit_1)

################################################################################
################ EDUCATION SERVICES SECTOR ############################
################################################################################

naics_ed = c(61)

#### filter by education naics ####
ed_rows = data_all_summer$naics %in% naics_ed
data_all_summer_ed = data_all_summer[ed_rows,]

#### Model Monthly Sensitivity ####
fit_1 = lm(log(visitors) ~ year + above_2020_may + above_2020_jun +
             above_2020_jul + above_2020_aug + above_2020_sep +
             above_2021_may + above_2021_jun + above_2021_jul +
             above_2021_aug + above_2021_sep + month + MSA_SES,
           data=data_all_summer_ed)
summary(fit_1)

################################################################################
################ HEALTH SERVICES SECTOR ############################
################################################################################

naics_health = c(62)

#### filter by health naics ####
health_rows = data_all_summer$naics %in% naics_health
data_all_summer_health = data_all_summer[health_rows,]

#### Model Monthly Sensitivity ####
fit_1 = lm(log(visitors) ~ year + above_2020_may + above_2020_jun +
             above_2020_jul + above_2020_aug + above_2020_sep +
             above_2021_may + above_2021_jun + above_2021_jul +
             above_2021_aug + above_2021_sep + month + MSA_SES,
           data=data_all_summer_health)
summary(fit_1)
