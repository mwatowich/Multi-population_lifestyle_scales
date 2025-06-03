### Multi-population continuous lifestyle scales from Watowich et al. 2024, medRxiv

## Code to generate the location-based urbanicity score. Higher = more urban
## Adapted from Novak et al. 2012. "The development and validation of an urbanicity scale in a multi-country study"


# Load libraries 
library(dplyr)
library(geosphere)

#############################################
###                Turkana                ###
#############################################

# "turkana" is a file of complete data with lifestyle and demographic data. Generate urbanicity indexes using data from all individuals, not only those in your study.
# "sampling_locs" is a file of town, latitude and longitude


### Add population density from external data source -----------------
density=read.delim("./data/Kenya_density_2020_2pt5_min.txt")
density <- density[density$x < max(sampling_locs$X_longitude,na.rm=T) & 
                     density$x > min(sampling_locs$X_longitude,na.rm=T),]
res<-as.data.frame(matrix(ncol=4, nrow=dim(sampling_locs)[1]))
for (i in 1:dim(sampling_locs)[1]){
  sampling_locs <- sampling_locs
  y1=sampling_locs$Y_latitude[i]
  x1=sampling_locs$X_longitude[i]
  tmp<-subset(density,x>(x1-0.1) & x<(x1+0.1) & y>(y1-0.1) & y<(y1+0.1))
  tmp$dist<-0
  for (k in 1:dim(tmp)[1]){
    tmp$dist[k]<-distm(c(tmp$x[k], tmp$y[k]), c(x1,y1), fun = distHaversine)}
  tmp2<-tmp[(which(tmp$dist==min(tmp$dist))),]
  res[i,1:4]<-t(as.numeric(tmp2[1,1:4] ))
}
colnames(res) <- c("X_res", "Y_res", "ken_general_2020", "distance")
# 
density2=as.data.frame(cbind(sampling_locs,res))


# Make population a numeric variable, based on Novak 2012. Population density < 500 broken into bins of 100
pop_categories = c(0,100,200,300,400,500,1000,2000,3000,4000,6000,8000,10000,15000,20000,Inf)

density2$pop_cat <- cut(
  density2$ken_general_2020,
  breaks = pop_categories, # n labels = breaks-1 
  labels = c((10/(length(pop_categories)-1)) * 1:(length(pop_categories)-1)), # break categories into even parts, scaled 0-10 
  include.lowest = T,
  right = F
)
density2$pop_cat <- as.numeric(as.character(density2$pop_cat))

# Merge pop categories with sampling locations 
sampling_locs <- left_join(sampling_locs, 
                           density2[,c('StandardizedName_year','ken_general_2020','Y_latitude','X_longitude','pop_cat')], 
                           by = c('StandardizedName_year','Y_latitude','X_longitude'))

rm(tmp, tmp2, density, density2, res)
#
### Aggregate proportion of population for different columns ----------------

## Aggregate proportion of population in a location who worked wage labor
table(turkana$main_subsistence_activity)

# Main subsistence activity 
turkana$main_subsistence_activity <- stringr::str_to_title(turkana$main_subsistence_activity)
turkana[which(turkana$main_subsistence_activity == "Other/Not Answered"),]$main_subsistence_activity <- NA
turkana[which(turkana$main_subsistence_activity == "Hunting And Gathering"),]$main_subsistence_activity <- "Hunting and Gathering"
turkana[which(turkana$main_subsistence_activity == "Self-Employment"),]$main_subsistence_activity <- "Self Employment"

sub_strategies <- c('Animal Keeping','Farming','Fishing','Gathering','Herding',
                    'Hunting and Gathering','Hunting and Gathering, Animal Keeping')

not_wage <- turkana %>% 
  group_by(Standardized_name) %>%
  summarise(prop_not_wage = sum(main_subsistence_activity %in% sub_strategies, na.rm=T)/sum(complete.cases(main_subsistence_activity)))

sampling_locs <- merge(sampling_locs, not_wage, by.x = "StandardizedName_year", by.y = "Standardized_name")

sampling_locs[which(is.na(sampling_locs$prop_not_wage)),]$prop_not_wage <- NA 

#rm
rm(not_wage)



## Make 'not answered' into NAs 
for (i in c("presence_of_flush_toilet",
            "presence_of_electricity",
            "presence_of_television_set",
            "presence_of_mobile_phone",
            "highest_education_level")) {
  turkana[which(turkana[[i]] == "other/not answered"),][[i]] <- NA
}



## Proportion per sample location with flush toilet 
prop_plumbing <- turkana %>% 
  group_by(Standardized_name) %>%
  summarise(prop_toilet = sum(presence_of_flush_toilet == "Yes", na.rm=T)/sum(complete.cases(presence_of_flush_toilet)))

sampling_locs <- merge(sampling_locs, prop_plumbing, by.x = "StandardizedName_year" , by.y = "Standardized_name")


##  Proportion per sample location with electricity
prop_electricity <- turkana %>% 
  group_by(Standardized_name) %>%
  summarise(prop_electricity = sum(presence_of_electricity == "Yes", na.rm=T)/sum(complete.cases(presence_of_electricity)))

sampling_locs <- merge(sampling_locs, prop_electricity, by.x = "StandardizedName_year" , by.y = "Standardized_name")


##  Proportion per sample location with electricity
prop_tv <- turkana %>% 
  group_by(Standardized_name) %>%
  summarise(prop_tv = sum(presence_of_television_set == "Yes", na.rm=T)/sum(complete.cases(presence_of_television_set)))

sampling_locs <- merge(sampling_locs, prop_tv,  by.x = "StandardizedName_year" , by.y = "Standardized_name")


##  Proportion per sample location with electricity
prop_phone <- turkana %>% 
  group_by(Standardized_name) %>%
  summarise(prop_phone = sum(presence_of_mobile_phone == "Yes", na.rm=T)/sum(complete.cases(presence_of_mobile_phone)))

sampling_locs <- merge(sampling_locs, prop_phone, by.x = "StandardizedName_year" , by.y = "Standardized_name")


## Total number of people
tot<-as.data.frame(aggregate(turkana$sex ~ turkana$Standardized_name,FUN=function(x) length(x)))
names(tot)<-c('Standardized_name','total')

sampling_locs <- merge(sampling_locs, tot, by.x = "StandardizedName_year" , by.y = "Standardized_name")


# Remove locations with <5 people 
sampling_locs <- sampling_locs[-which(sampling_locs$total < 5),]



## Aggregate education for ppl over/under 40 

# Highest education level 
table(turkana$highest_education_level)
turkana[which(turkana$highest_education_level %in% c("Primary school (lower)","Primary school (upper)","Primary School")),]$highest_education_level <- "Primary school"
turkana[which(turkana$highest_education_level == "Secondary"),]$highest_education_level <- "Secondary school"
turkana[which(turkana$highest_education_level == "Tertiary"),]$highest_education_level <- "Tertiary school"

# Get proportion of ppl with some education, of people with reported education 
turkana_40plus<-subset(turkana,age>40)
turkana_40less<-subset(turkana,age<=40)
turkana_40plus_ed<-as.data.frame(aggregate(turkana_40plus$highest_education_level ~ turkana_40plus$Standardized_name,FUN=function(x) length(which(x!='none' & x!='None')) / length(which(x!='NA'))))
turkana_40less_ed<-as.data.frame(aggregate(turkana_40less$highest_education_level ~ turkana_40less$Standardized_name,FUN=function(x) length(which(x!='none' & x!='None')) / length(which(x!='NA'))))
names(turkana_40plus_ed)<-c('Standardized_name','prop_40plus_ed')
names(turkana_40less_ed)<-c('Standardized_name','prop_40less_ed')


## Combine data 
sampling_locs <- merge(sampling_locs, turkana_40plus_ed, by.x = "StandardizedName_year" , by.y = "Standardized_name")
sampling_locs <- merge(sampling_locs, turkana_40less_ed, by.x = "StandardizedName_year" , by.y = "Standardized_name")


### Add up urbanicity score -----------------
sampling_locs$urb_score <- sampling_locs$pop_cat +
  10 - (10 * sampling_locs$prop_not_wage) + 
  5 * sampling_locs$prop_toilet +
  5 * sampling_locs$prop_electricity + 
  5 * sampling_locs$prop_tv +
  5 * sampling_locs$prop_phone +
  10 * sampling_locs$prop_40plus_ed +
  10 * sampling_locs$prop_40less_ed

sampling_locs$urb_score <- round(sampling_locs$urb_score, 3)
sampling_locs <- sampling_locs[-which(is.na(sampling_locs$urb_score)),]
hist(sampling_locs$urb_score, breaks = 20)
# 

#############################################
###               Orang Asli              ###
#############################################

# caMetric is a file of communities in the OA HeLP study, community attributes (eg. does the community have cell service?), and lat/long
# trad_lifestyle is a file with person-level information about individuals subsistence strategies/occupations, etc.
# pers_info is a file with information on personal attributes (eg. DOB, sex)
# medical is a file with the data an individual attended a clinic - from DOB and visit data, can get age


## The first score maximizes comparability between OA HeLP and the Turkana Health and Genomics Project data. 
# Community-level acculturation index based on individual-level data 
# This score is called "urbanicity score" or "urb score"
#         Score for population density + 
#         10 points- 10*(proportion of population involved in non-wage labor) + 
#         5 points* proportion of households with flush toilets
#         5 points* proportion households with electricity
#         5 points* proportion households with television
#         5 points* proportion households with mobile phone
#         10*proportion >40yo with some education
#         10*proportion <=40yo with some education

## The second score maximizes similarity to Novak 2012 and uses observations/data at the community level AND the individual-level. This score is called "community score"
#         Population density: 'Estimated.population.size.of.village..including.adults.and.children'
#         Proportion not working wage labor (10 points-10*(proportion of population not working wage labor)): 'wage_past_month'
#         Village access (2=Paved road, 1=unpaved road for motor traffic, 0=non-motorized roads): 'Village.is.accessible.by.which.of.the.following.'
#         2 = Waste infrastructure: 'Which.of.the.following.water.sources.does.this.village.have.'
#         2* Proportion of households with flush toilets: "where_poop..."
#         2 = Electricity infrastructure:
#         2* Proportion households with electricity
#         2* Proportion households with television
#         2* Proportion households with mobile phone
#         2 = Public internet: 'Does.this.village.have.internet.access.'
#         2 = Public telephone: 'Does.this.village.have.cellular.signal.'
#         4* Length of time to get to nearest primary school (4 = 0-15min, 3 = 15-30min, 2 = 30-1hr, 1 = +1hr)
#         4* Length of time to get to nearest secondary school (4 = 0-15min, 3 = 15-30min, 2 = 30-1hr, 1 = +1hr)
#         2 = Hospital (public or private) available: (within 30 minutes yes/no)
#         2 = Health center (public or private) available (within 30 minutes yes/no)
#         4* Proportion >40yo with some education
#         4* Proportion <=40yo with some education

### Add population density from external data source -----------------------------

density_oa = read.delim(normalizePath("~/Documents/_Vanderbilt/OrangAsli/population_data/Malaysia_density_2020_2pt5_min.txt", mustWork = FALSE))
density_oa <- density_oa[density_oa$x < max(caMetric$Longitude..GPS., na.rm = T) &
                           density_oa$x > min(caMetric$Longitude..GPS., na.rm = T), ]

caMetric_completeLatLong <- caMetric[which(complete.cases(caMetric$lat)), ]
res_oa <- as.data.frame(matrix(ncol = 4, nrow = nrow(caMetric_completeLatLong)))
res_oa$village_id <- caMetric_completeLatLong$village_id
for (i in 1:nrow(res_oa)) {
  y1 = caMetric_completeLatLong$lat[i]
  x1 = caMetric_completeLatLong$long[i]
  tmp <- subset(density_oa, x > (x1 - 0.1) &
                  x < (x1 + 0.1) & y > (y1 - 0.1) & y < (y1 + 0.1))
  tmp$dist <- 0
  for (k in 1:dim(tmp)[1]) {
    tmp$dist[k] <- distm(c(tmp$x[k], tmp$y[k]), c(x1, y1), fun = distHaversine)
  }
  tmp2 <- tmp[(which(tmp$dist == min(tmp$dist))), ]
  res_oa[i, 1:4] <- t(as.numeric(tmp2[1, 1:4]))
}
colnames(res_oa) <- c("X_res",
                      "Y_res",
                      "malaysia_general_2020",
                      "distance",
                      "village_id")

# Merge
caMetric <- merge(caMetric, res_oa[, c("malaysia_general_2020", "village_id")], by = "village_id", all.x = T)

# Make population a numeric variable, based on Novak 2012. Population density < 500 broken into bins of 100
pop_categories = c(0,100,200,300,400,500,1000,2000,3000,4000,6000,8000,10000,15000,20000,Inf)

caMetric$pop_cat <- cut(
  caMetric$malaysia_general_2020,
  breaks = pop_categories, # n labels = breaks-1 
  labels = c((10/(length(pop_categories)-1)) * 1:(length(pop_categories)-1)), # break categories into even parts, scaled 0-10 
  include.lowest = T,
  right = F
)
caMetric$pop_cat <- as.numeric(as.character(caMetric$pop_cat))


### Proportion who do not work wage labor - aggregate per location
not_wage <- trad_lifestyle %>% group_by(interview_location_med) %>%
  summarise(prop_not_wage = sum(wage_past_month == 0, na.rm=T)/sum(wage_past_month %in% c(0, 1)))
caMetric <- merge(caMetric, not_wage, by.x = "village_id", by.y = "interview_location_med", all.x = T)


### Village accessibility 
caMetric$village_access <- NA
caMetric[grep("walking/trekking", caMetric$Village.is.accessible.by.which.of.the.following.), ]$village_access <- 0
caMetric[grep("boat|motorbike", caMetric$Village.is.accessible.by.which.of.the.following.), ]$village_access <- 1
caMetric[grep("truck", caMetric$Village.is.accessible.by.which.of.the.following.), ]$village_access <- 2
caMetric[grep("car", caMetric$Village.is.accessible.by.which.of.the.following.), ]$village_access <- 3


### Sewage infrastructure
caMetric$water <- NA
caMetric$water[grep("none (only river/lake)", caMetric$water.source, fixed = T)] <- 0
caMetric$water[grep("gravity", caMetric$water.source)] <- 1
caMetric$water[grep("well", caMetric$water.source, fixed = F)] <- 2
caMetric$water[grep("water filtration tank system", caMetric$water.source)] <- 2
caMetric$water[grep("undergound pump", caMetric$water.source)] <- 3
caMetric$water[grep("piped", caMetric$water.source)] <- 4


### Proportion with flush toilets
prop_plumbing <- trad_lifestyle %>% group_by(interview_location_med) %>%
  summarise(prop_toilet = sum(where_poop___toilet == 1, na.rm = T)/sum(where_poop___toilet %in% c(0, 1)))
caMetric <- merge(caMetric, prop_plumbing, by.x = "village_id", by.y = "interview_location_med", all.x = T)


### Electricity
caMetric$electricity <- case_when(
  # power from electricity
  caMetric$Does.this.village.have.electricity.from.power.lines. == "Yes" ~ "3",
  
  # no electricity but power from 2 other sources
  (
    caMetric$Does.this.village.have.electricity.from.power.lines. == "No" &
      caMetric$Are.there.any.major.solar.systems.in.the.village..at.least.200.watts.. == "Yes" &
      caMetric$Are.there.any.generators.in.the.village. == "Yes"
  ) ~ "2",
  
  # no electricity but power from solar system (yes, this is meant to be the same score as above)
  (
    caMetric$Does.this.village.have.electricity.from.power.lines. == "No" &
      caMetric$Are.there.any.major.solar.systems.in.the.village..at.least.200.watts.. == "Yes" &
      caMetric$Are.there.any.generators.in.the.village. == "No"
  ) ~ "2",
  
  # no electricity but power from generators
  (
    caMetric$Does.this.village.have.electricity.from.power.lines. == "No" &
      caMetric$Are.there.any.major.solar.systems.in.the.village..at.least.200.watts.. == "No" &
      caMetric$Are.there.any.generators.in.the.village. == "Yes"
  ) ~ "1",
  
  # no electricity source
  (
    caMetric$Does.this.village.have.electricity.from.power.lines. == "No" &
      caMetric$Are.there.any.major.solar.systems.in.the.village..at.least.200.watts.. == "No" &
      caMetric$Are.there.any.generators.in.the.village. == "No"
  ) ~ "0",
  
  .default = "FIX"
)
caMetric$electricity <- as.numeric(caMetric$electricity)


### Proportion with electricity
prop_elec <- trad_lifestyle %>% group_by(interview_location_med) %>%
  summarise(prop_electricity = sum(electricity_resid == 1,na.rm = T)/sum(electricity_resid %in% c(0, 1)))
caMetric <- merge(caMetric, prop_elec, by.x = "village_id", by.y = "interview_location_med", all.x = T)


### Proportion with television 
prop_tv <- trad_lifestyle %>% group_by(interview_location_med) %>%
  summarise(prop_tv = sum(hh_item___tv == 1, na.rm = T)/sum(hh_item___tv %in% c(0, 1)))
caMetric <- merge(caMetric, prop_tv, by.x = "village_id", by.y = "interview_location_med", all.x = T)


### Proportion with smart phone 
prop_phone <- trad_lifestyle %>% group_by(interview_location_med) %>%
  summarise(prop_smart_phone = sum(hh_item___smart_phone == 1, na.rm = T)/sum(hh_item___smart_phone %in% c(0, 1)))
caMetric <- merge(caMetric, prop_phone, by.x = "village_id", by.y = "interview_location_med", all.x = T)


### Internet
caMetric$internet <- NA
caMetric$internet[which(caMetric$Does.this.village.have.internet.access. == "none")] <- 0
caMetric$internet[which(caMetric$Does.this.village.have.internet.access. == "no")] <- 0
caMetric$internet[grep("satellite", caMetric$Does.this.village.have.internet.access.)] <- 1
caMetric$internet[grep("mobile", caMetric$Does.this.village.have.internet.access.)] <- 2
caMetric$internet[grep("fibre", caMetric$Does.this.village.have.internet.access.)] <- 3


### Cell service
table(caMetric$Does.this.village.have.cellular.signal.)
caMetric$cell <- case_when(
  caMetric$Does.this.village.have.cellular.signal. == "Yes, everywhere or most places in village" ~ "2",
  caMetric$Does.this.village.have.cellular.signal. == "Yes, but only in a few locations" ~ "1",
  caMetric$Does.this.village.have.cellular.signal. == "No" ~ "0",
  .default = "FIX"
)
caMetric$cell <- as.numeric(caMetric$cell)


### Make scale for time to nearest primary school - time is in minutes
caMetric$Hours.to.primary.school_scale <- as.numeric(as.character(
  cut(
    caMetric$Hours.to.primary.school_2,
    breaks = c(0, 15, 30, 60, Inf),
    labels = c(3, 2, 1, 0), 
    include.lowest = T,
    right = F
  )
))
caMetric$Hours.to.primary.school_scale[which(is.na(caMetric$Hours.to.primary.school_scale))] <- 0


### Make scale for time to nearest secondary school - time is in minutes
caMetric$Hours.to.secondary.school_scale <- as.numeric(as.character(
  cut(
    caMetric$Hours.to.secondary.school_2,
    breaks = c(0, 15, 30, 60, Inf),
    labels = c(3, 2, 1, 0),
    include.lowest = T,
    right = F
  )
))
caMetric$Hours.to.secondary.school_scale[which(is.na(caMetric$Hours.to.secondary.school_scale))] <- 0


### Make scale for time to nearest hospital - time is in minutes
caMetric$Hours.to.hospital_scale <- as.numeric(case_when(caMetric$Hours.to.hospital_2 < 30 ~ 1,
                                                         caMetric$Hours.to.hospital_2 >= 30 ~ 0, .default = NA))
caMetric$Hours.to.hospital_scale[which(is.na(caMetric$Hours.to.hospital_scale))] <- 0


### Make scale for time to nearest clinic - time is in minutes
caMetric$Hours.to.clinic_scale <- as.numeric(case_when(caMetric$Hours.to.clinic_2 < 30 ~ 1,
                                                       caMetric$Hours.to.clinic_2 >= 30 ~ 0, .default = NA))
caMetric$Hours.to.clinic_scale[which(is.na(caMetric$Hours.to.clinic_scale))] <- 0



### Avg education level for ppl over/under 40 
ed_info <- merge(trad_lifestyle[,c("rid","highest_education_stage","interview_location_med")],
                 medical[,c("rid","medical_age","interview_location_med","med_date")],
                 by = c("rid","interview_location_med"))
ed_info <- merge(ed_info, 
                 pers_info[,c("rid","date_of_birth","sex")], 
                 by = c("rid"))
# format dates 
ed_info$date <- as.Date(ed_info$med_date)
ed_info$date_of_birth <- as.Date(ed_info$date_of_birth)
# calc age 
library(eeptools)
ed_info$age <- NA
ed_info$age <- age_calc(dob = ed_info$date_of_birth, 
                        enddate = ed_info$date, 
                        units = "years", precise = TRUE)
# split into over/under 40yo, find avg education level
ed_info <- ed_info %>% mutate(age_cat = case_when(age>=40 ~ "ed_over40",
                                                  age<40 ~ "ed_under40",
                                                  .default = NA))

avg_education <- ed_info %>% group_by(interview_location_med, age_cat) %>%
  summarise(avg_education = median(highest_education_stage, na.rm=T)) %>% 
  tidyr::pivot_wider(names_from = "age_cat", values_from = "avg_education")

caMetric <- merge(caMetric, avg_education, by.x = "village_id", by.y = "interview_location_med", all.x = T)
# rm
rm(avg_education)
# 
### Generate scores ------------------------------------ 

### Make pertinent variables into proportions
prop_max_func <- function(column) {
  prop <- column / max(column, na.rm = T)
  return(prop)
}

caMetric <- caMetric %>% mutate(across(
  c(
    village_access:cell,
    Hours.to.primary.school_scale,
    Hours.to.secondary.school_scale,
    ed_over40, 
    ed_under40
  ),
  prop_max_func
))



## Score #1 - community-level acculturation score based on community-level and individual-level data (maximizes similarity to Novak score)
## "community_score"
caMetric$community_score <-
  caMetric$pop_cat +
  10 - (10 * caMetric$prop_not_wage) + 
  2 * caMetric$village_access +
  2 * caMetric$water +
  2 * caMetric$prop_toilet + 
  2 * caMetric$electricity + 
  2 * caMetric$prop_electricity + 
  2 * caMetric$prop_tv + 
  2 * caMetric$prop_smart_phone + 
  2 * caMetric$internet +
  2 * caMetric$cell +
  4 * caMetric$Hours.to.primary.school_scale +
  4 * caMetric$Hours.to.secondary.school_scale +
  2 * caMetric$Hours.to.hospital_scale +
  2 * caMetric$Hours.to.clinic_scale +
  4 * caMetric$ed_over40 +
  4 * caMetric$ed_under40

caMetric$community_score <- round(caMetric$community_score, 3)
hist(caMetric$community_score, breaks = 20)



## Score #2 - community-level acculturation score based on individual-level data (adapt Novak score for use with OA Help and THGP data, maximize similarity to THGP data). 
## "urb score"
caMetric$urb_score <-
  caMetric$pop_cat +
  10 - (10 * caMetric$prop_not_wage) + 
  5 * caMetric$prop_toilet +
  5 * caMetric$prop_electricity + 
  5 * caMetric$prop_tv +
  5 * caMetric$prop_smart_phone +
  10 * caMetric$ed_over40 +
  10 * caMetric$ed_under40

caMetric$urb_score <- round(caMetric$urb_score, 3)
hist(caMetric$urb_score, breaks = 20)

cor.test(caMetric$community_score, caMetric$urb_score)
