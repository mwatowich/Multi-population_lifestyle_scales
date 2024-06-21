### Multi-population continuous lifestyle scales from Watowich et al. 2024, in prep

## Code to generate the location-based urbanicity score. Higher = more urban
## Adapted from Novak et al. 2012. "The development and validation of an urbanicity scale in a multi-country study"


# Load libraries 
library(dplyr)
library(geosphere)


## Calculate location-based urbanicity score 

## Turkana ---------------------------------

# Aggregate proportion per sample location 
plumbing<-aggregate(data_turkana$presence_of_flush_toilet ~ data_turkana$Standardized_name,FUN=function(x) length(which(x=='Yes')) / length(which(x=='Yes' | x=='No')))
electricity<-aggregate(data_turkana$presence_of_electricity ~ data_turkana$Standardized_name,FUN=function(x) length(which(x=='Yes')) / length(which(x=='Yes' | x=='No')))
tv<-aggregate(data_turkana$presence_of_television_set ~ data_turkana$Standardized_name,FUN=function(x) length(which(x=='Yes')) / length(which(x=='Yes' | x=='No')))
phone<-aggregate(data_turkana$presence_of_mobile_phone ~ data_turkana$Standardized_name,FUN=function(x) length(which(x=='Yes')) / length(which(x=='Yes' | x=='No')))
not_wage<-aggregate(data_turkana$main_subsistence_activity ~ data_turkana$Standardized_name,FUN=function(x) length(which(x %in% c('Animal keeping','Farming','Fishing','Gathering','Herding','Hunting and Gathering'))) / length(which(x!='NA'))) 
tot<-as.data_turkana.frame(aggregate(data_turkana$gender ~ data_turkana$Standardized_name,FUN=function(x) length(x))) 
names(tot)<-c('location','total')

# Aggregate education for ppl over/under 40
data_turkana_40plus<-subset(data_turkana,age>40)
data_turkana_40less<-subset(data_turkana,age<=40)
data_turkana_40plus_ed<-as.data_turkana.frame(aggregate(data_turkana_40plus$highest_education_level ~ data_turkana_40plus$Standardized_name,FUN=function(x) length(which(x!='none' & x!='None')) / length(which(x!='NA'))))
data_turkana_40less_ed<-as.data_turkana.frame(aggregate(data_turkana_40less$highest_education_level ~ data_turkana_40less$Standardized_name,FUN=function(x) length(which(x!='none' & x!='None')) / length(which(x!='NA'))))
names(data_turkana_40plus_ed)<-c('location','prop_40plus_ed')
names(data_turkana_40less_ed)<-c('location','prop_40less_ed')

# Join 
urban<-as.data.frame(cbind(plumbing[,1:2],electricity[,2],tv[,2],phone[,2]))
urban<-full_join(urban, not_wage, by = "data_turkana$Standardized_name")
names(urban)<-c('location','prop_toilet','prop_electricity','prop_tv','prop_phone','prop_not_wage')

urban2<-merge(urban,data_turkana_40plus_ed,by='location')
urban3<-merge(urban2,data_turkana_40less_ed,by='location')
urban4<-merge(urban3,tot,by='location')

# Estimate population density per location - generated from Gridded Population of the World 
density=read.delim("Kenya_density_2020_2pt5_min.txt")

# Pull density from closest coordinates
density <- density[density$x < max(sampling_locs$X_longitude,na.rm=T) & 
                     density$x > min(sampling_locs$X_longitude,na.rm=T),]
res<-as.data.frame(matrix(ncol=4, nrow=dim(sampling_locs[!is.na(sampling_locs$Y_latitude),])[1]))
for (i in 1:dim(sampling_locs[!is.na(sampling_locs$Y_latitude),])[1]){
  sampling_locs <- sampling_locs[!is.na(sampling_locs$Y_latitude),]
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
density2=as.data.frame(cbind(sampling_locs[!is.na(sampling_locs$Y_latitude),],res))
urban5=merge(density2[,c('Sampling_location','Standardized_name','ken_general_2020','Y_latitude','X_longitude')],
             urban4,
             by.y='location',by.x='Standardized_name')

urban5$pop_cat<-1
urban5$pop_cat[which(urban5$ken_general_2020>10)]<-2
urban5$pop_cat[which(urban5$ken_general_2020>50)]<-3
urban5$pop_cat[which(urban5$ken_general_2020>100)]<-4
urban5$pop_cat[which(urban5$ken_general_2020>200)]<-5
urban5$pop_cat[which(urban5$ken_general_2020>300)]<-6
urban5$pop_cat[which(urban5$ken_general_2020>400)]<-7
urban5$pop_cat[which(urban5$ken_general_2020>500)]<-8
urban5$pop_cat[which(urban5$ken_general_2020>1000)]<-9
urban5$pop_cat[which(urban5$ken_general_2020>1500)]<-10

urban5$urb_score <- urban5$pop_cat + (10-(10*urban5$prop_not_wage))+(5*urban5$prop_electricity)+(5*urban5$prop_toilet)+(5*urban5$prop_tv)+(5*urban5$prop_phone)+10*urban5$prop_40plus_ed+10*urban5$prop_40less_ed
urban5<-subset(urban5, urb_score!='NA')



## Orang Asli ------------------------------

# Aggregate by location - wage labor, sewage, electricity 
not_wage<-aggregate(data_orangAsli$wage_past_month ~ data_orangAsli$interview_location_med,FUN=function(x) length(which(x == 0))/length(which(x!='NA')))
plumbing<-aggregate(data_orangAsli$where_poop___toilet ~ data_orangAsli$interview_location_med,FUN=function(x) length(which(x == 1)) / length(which(x!='NA')))
electricity_prop<-aggregate(data_orangAsli$electricity_resid ~ data_orangAsli$interview_location_med,FUN=function(x) length(which(x==1)) / length(which(x!='NA')))

# Is there electricity in the community that comes from power lines? Aggregate per location 
data_orangAsli[data_orangAsli$interview_location_med==0 & data_orangAsli$electricity_resid_source___power_lines==1,]$electricity_resid_source___power_lines <- 1
communities_w_power <- data_orangAsli %>%
  group_by(interview_location_med, electricity_resid_source___power_lines) %>%
  summarise(count=n()) %>% 
  filter(electricity_resid_source___power_lines == 1 & count > 1)
data_orangAsli$community_power_lines <- 0
data_orangAsli[data_orangAsli$interview_location_med %in% communities_w_power$interview_location_med,]$community_power_lines <- 1
electricity_community <- aggregate(data_orangAsli$community_power_lines ~ data_orangAsli$interview_location_med,FUN=function(x) length(which(x==1)) / length(which(x!='NA')))

# Aggregate by location - material wealth via household items 
tv<-aggregate(data_orangAsli$hh_item___tv ~ data_orangAsli$interview_location_med,FUN=function(x) length(which(x==1)) / length(which(x!='NA')))
smart_phone<-aggregate(data_orangAsli$hh_item___smart_phone ~ data_orangAsli$interview_location_med,FUN=function(x) length(which(x==1)) / length(which(x!='NA')))

# Aggregate education for people over/under 40
data_orangAsli_40plus<-subset(data_orangAsli,age>40)
data_orangAsli_40less<-subset(data_orangAsli,age<=40)
data_orangAsli_40plus_ed<-as.data.frame(aggregate(data_orangAsli_40plus$highest_education_stage ~ data_orangAsli_40plus$interview_location_med,FUN=function(x) length(which(x!=0)) / length(which(x!='NA'))))
data_orangAsli_40less_ed<-as.data.frame(aggregate(data_orangAsli_40less$highest_education_stage ~ data_orangAsli_40less$interview_location_med,FUN=function(x) length(which(x!=0)) / length(which(x!='NA'))))
names(data_orangAsli_40plus_ed)<-c('interview_location_med','prop_40plus_ed')
names(data_orangAsli_40less_ed)<-c('interview_location_med','prop_40less_ed')

# Number of people sampled 
tot_oa<-as.data.frame(aggregate(data_orangAsli$sex_medical ~ data_orangAsli$interview_location_med,FUN=function(x) length(x))) 
names(tot_oa)<-c('location','total')

# Highest village access 
data_orangAsli$vill_access <- ifelse(data_orangAsli$vill_access_by___car==1, 3, 
                                ifelse(data_orangAsli$vill_access_by___truck==1, 2, 
                                       ifelse(data_orangAsli$vill_access_by___moto==1, 1, 
                                              ifelse(data_orangAsli$vill_access_by___boat==1, 1, 
                                                     ifelse(data_orangAsli$vill_access_by___none==1, 0, NA)))))
data_orangAsli <- left_join(data_orangAsli, 
                       data_orangAsli %>% 
                         dplyr::group_by(interview_location_med, vill_access) %>%
                         dplyr::summarize(count = n()) %>%
                         dplyr::arrange(interview_location_med, desc(count)) %>%
                         dplyr::slice(1) %>%
                         dplyr::select(interview_location_med, top_selected_vill_access = vill_access), 
                       by = c("interview_location_med"))

# Bind together
urban_oa<-as.data.frame(cbind(not_wage[,1:2],plumbing[,2],electricity_prop[,2],electricity_community[,2],tv[,2],smart_phone[,2]))
names(urban_oa)<-c('location','prop_not_wage','prop_toilet','prop_electricity','community_electricity','prop_tv','prop_smart_phone')
urban2_oa<-merge(urban_oa,data_orangAsli_40plus_ed,by.x='location',by.y='interview_location_med') 
urban3_oa<-merge(urban2_oa,data_orangAsli_40less_ed,by.x='location',by.y='interview_location_med')
urban3_oa<-merge(urban3_oa,tot_oa,by='location')

# Add lat/long 
urban4_oa <- merge(urban3_oa, 
                   distinct(data_orangAsli[,c("interview_location_med","lat","long")]), 
                   by.x = "location", by.y = "interview_location_med")

## Estimate population density per location - generated from Gridded Population of the World 
density_oa=read.delim("Malaysia_density_2020_2pt5_min.txt")

## Pull density from closest coordinates
density_oa <- density_oa[density_oa$x < max(data_orangAsli$long,na.rm=T) &
                           density_oa$x > min(data_orangAsli$long,na.rm=T),]
res_oa<-as.data.frame(matrix(ncol=4, nrow=nrow(urban4_oa)))
res_oa$location <- urban4_oa$location
for (i in 1:nrow(res_oa)){
  y1=urban4_oa$lat[i]
  x1=urban4_oa$long[i]
  tmp<-subset(density_oa,x>(x1-0.1) & x<(x1+0.1) & y>(y1-0.1) & y<(y1+0.1))
  tmp$dist<-0
  for (k in 1:dim(tmp)[1]){
    tmp$dist[k]<-distm(c(tmp$x[k], tmp$y[k]), c(x1,y1), fun = distHaversine)}
  tmp2<-tmp[(which(tmp$dist==min(tmp$dist))),]
  res_oa[i,1:4]<-t(as.numeric(tmp2[1,1:4]))
}
colnames(res_oa) <- c("X_res", "Y_res", "malaysia_general_2020", "distance","location")
urban5_oa=left_join(res_oa, urban4_oa, by=c('location'))

# Assign number based on population density 
urban5_oa$pop_cat<-1
urban5_oa$pop_cat[which(urban5_oa$malaysia_general_2020>10)]<-2
urban5_oa$pop_cat[which(urban5_oa$malaysia_general_2020>50)]<-3
urban5_oa$pop_cat[which(urban5_oa$malaysia_general_2020>100)]<-4
urban5_oa$pop_cat[which(urban5_oa$malaysia_general_2020>200)]<-5

# Make score 
urban5_oa$urb_score <- 10 - (10*urban5_oa$prop_not_wage) + 5*urban5_oa$prop_toilet + 
  5*urban5_oa$prop_electricity + 5*urban5_oa$community_electricity + 
  5*urban5_oa$prop_tv + 5*urban5_oa$prop_smart_phone + 
  10*urban5_oa$prop_40plus_ed + 10*urban5_oa$prop_40less_ed + 
  urban5_oa$pop_cat
