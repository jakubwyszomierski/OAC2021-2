setwd("~/Desktop/PhD/GIT/OAC2021/")

library(nomisr)
library(dplyr)
library(sf)
library(tmap)
library(tidyverse)
library(readxl)
library(Hmisc)
library(tmap)
library(stringr)
library(plotrix)




####################################################################
###########         SIMPLIFICATION OF SHAPEFILES      ##############
####################################################################


if(!file.exists("shp_2011_simple_30.RDS")){
  
  shp_2011 = st_read("../../shapefiles/OA_SA_2011/infuse_oa_lyr_2011.shp") %>% 
    mutate(country=substr(geo_code,1,1)) %>% 
    rename('Geography_Code' = 'geo_code') %>% select(Geography_Code, country, geometry)
  shp_2011_simple_30 <- st_simplify(shp_2011, preserveTopology = TRUE, dTolerance = 30)
  
  saveRDS(shp_2011_simple_30, "shp_2011_simple_30.RDS")
  
} else{
  shp_2011_simple = readRDS("shp_2011_simple_30.RDS")
}




if(!file.exists("hybrid_shp_simple_30.RDS")){
  
  shp_2011_S_NI = shp_2011 %>% filter(country %in% c("S", "N"))
  shp_2021 = st_read("../../shapefiles/boundaries_2021/OA_2021/OA_2021_EW_BGC.shp") %>% 
    mutate(country = substr(OA21CD,1,1)) %>% rename('Geography_Code' = 'OA21CD') %>%
    select(Geography_Code, country, geometry)
  hybrid_shp = bind_rows(shp_2021,shp_2011_S_NI)
  
  hybrid_shp_simple_30 <- st_simplify(hybrid_shp, preserveTopology = TRUE, dTolerance = 30)
  shp_2021_simple_30 <- st_simplify(shp_2021, preserveTopology = TRUE, dTolerance = 30)
  
  saveRDS(hybrid_shp_simple_30, "hybrid_shp_simple_30.RDS")
  saveRDS(shp_2021_simple_30, "shp_2021_simple_30.RDS")
  
} else{
  hybrid_shp_simple = readRDS("hybrid_shp_simple_30.RDS")
  shp_2021_simple = readRDS("shp_2021_simple_30.RDS")
}




object.size(hybrid_shp_simple) / 1000000



################################################################
###########         FINAL VARIABLE SELECTION      ##############
################################################################

common_variables = read.csv("Data/Lookups/common_variables.csv") %>% select(-CategoryCode11, -TableCode)
OAC_variables = common_variables %>% filter(Code %in% c(
  # TS001                                                         Residency type
  "NM_2021_1_2",                                                   # 1. Lives in a communal establishment
  
  
  # TS002                                                          Legal partnership status
  "NM_2022_1_1",                                                   # 2. Never married and never registered a civil partnership
  "NM_2022_1_1001",                                                # 3. Married or in a registered civil partnership
  "separated_divorced",                                            # 4. Separated or divorced
  
  
  # TS003                                                         Household composition
 'NM_2023_1_1001',                                            # ?.  One-person household
  "no_children",                                                   # 5. Families with no children
  "dependent_children",                                            # 6. Families dependent children
#   "nondependent_children",                                         # 7. Families with non-dependent children 
  
  
  # TS004                                                         Country of birth
  "NM_2024_1_1",                                                   # 8. Europe: United Kingdom
  "NM_2024_1_1002",                                                # 9. Europe: EU countries
  "NM_2024_1_1003",                                                # 10. Europe: Non-EU countries
  'NM_2024_1_7',                                               # ?.  Africa
  # ? NM_2024_1_8                                               # ?.  Middle East and Asia
  
  
  # TS006                                                         Population density
  "NM_2026_1_0",                                                   # 11. Usual residents per square kilometre
  
  
  # TS007A                                                        Age structure
  "NM_2020_1_1",                                                   # 12. Aged 4 years and under
 # "age_20_29",                                                     # 13. Aged 20 to 29
 # "age_30_39",                                                     # 14. Aged 30 to 39
 # "age_40_54",                                                     # 15. Aged 40 to 54
 # "age_55_69",                                                     # 16. Aged 55 to 69
 # "age_70_84",                                                     # 17. Aged 70 to 84
  "NM_2020_1_18",                                                  # 18. Aged 85 years and over


### CHRIS's
  'age_5_14', 
  'age_25_44', 
  'age_45_64', 
  'age_65_84',
  
  
  # 
  
  # TS019                                                          Migrant Indicator
  "NM_2039_1_1",                                                  # 19. Address one year ago is the same as the address of enumeration
  
  # TS021                                                         Ethnic group
  "NM_2041_1_12",                                                 # 20. Asian, Asian British or Asian Welsh: Bangladeshi 
  "NM_2041_1_13",                                                 # 21. Asian, Asian British or Asian Welsh: Chinese 
  "NM_2041_1_10",                                                 # 22. Asian, Asian British or Asian Welsh: Indian 
  "NM_2041_1_11",                                                 # 23. Asian, Asian British or Asian Welsh: Pakistani 
  "NM_2041_1_14",                                                 # 24. Asian, Asian British or Asian Welsh: Other Asian 
  "NM_2041_1_1002",                                               # 25. Black, Black British, Black Welsh, Caribbean or African
  "NM_2041_1_1003",                                               # 26. Mixed or Multiple ethnic groups
  "NM_2041_1_1004",                                               # 27. White
  # #? NM_2041_1_1005                                               # ?. Other ethnic group
  
  
  # TS023                                                           Multiple ethnic group
  # "multi_ethnic_household",                                       # 28. Multi-ethnic household
  "NM_2042_1_2",
  
  
  # TS029                                                           Proficiency in English
  "cannot_speak_English",                                         # 29. Cannot speak English well or at all
  
  
  # TS030                                                           Religion
  "NM_2049_1_1",                                                  # 30. No religion 
  "NM_2049_1_2",                                                  # 31. Christian 
  "NM_2049_1_8",                                                  # 32. Other religion
  
  
  # TS038                                                           Disability
  "SIR",                                                          # 33. Standardised Illness Ratio
  
  
  # TS039                                                           Provision of unpaid care
  "provides_unpaid_care",                                         # 34. Provides unpaid care
  
  
  #  # TS044                                                           Accommodation type
  "NM_1549_1_3",                                                  # 35. Unshared dwelling: Whole house or bungalow: Detached
  "NM_1549_1_4",                                                  # 36. Unshared dwelling: Whole house or bungalow: Semi-detached
  "NM_1549_1_5",                                                  # 37. Unshared dwelling: Whole house or bungalow: Terraced (including end-terrace
  "flat",                                                         # 38. Lives in a flat
  
  
  # TS045                                                           Car or van availability
  "cars_2_or_more",                                               # 39. Two or more cars or vans
  
  
  # TS053                                                           Occupancy rating for rooms
  "under_occupation",                                              # 49. Under occupation
  "overcrowding",                                                  # 41. Overcrowding
  
  
  #  # TS054                                                           Tenure
  "NM_2072_1_1003",                                               # 42. Social rented
  "NM_2072_1_1004",                                               # 43. Private rented
  "ownership_or_shared",                                          # 44.Ownership or shared ownership
  # 
  
  # TS059                                                           Hours worked
  "NM_2076_1_1001",                                               # 45. Part-time
  "NM_2076_1_1002",                                               # 46. Full-time
  
  
  # TS062                                                           NS-SeC
  #  "NM_2079_1_1",                                                 # ? L1, L2 and L3 Higher managerial, administrative and professional occupations
  #  "NM_2079_1_2",                                                 # ? L4, L5 and L6 Lower managerial, administrative and professional occupations
  #  "NM_2079_1_3",                                                 # ? L7 Intermediate occupations
  #  "NM_2079_1_4",                                                 # ? L8 and L9 Small employers and own account workers
  #  "NM_2079_1_5",                                                 # ? L10 and L11 Lower supervisory and technical occupations
  #  "NM_2079_1_6",                                                 # ? L12 Semi-routine occupations
  #  "NM_2079_1_7",                                                 # ? L13 Routine occupations
  #  "NM_2079_1_8",                                                 # ? L14.1 and L14.2 Never worked and long-term unemployed
  "NM_2079_1_9",                                                  # 47. L15 Full-time students
  
  
  # TS063                                                           Occupation
  "NM_2080_1_1",                                                  # 48. 1. Managers, directors and senior officials
  "NM_2080_1_2",                                                  # 39. 2. Professional occupations
  "NM_2080_1_3",                                                  # 50. 3. Associate professional and technical occupations
  "NM_2080_1_4",                                                  # 51. 4. Administrative and secretarial occupations
  "NM_2080_1_5",                                                  # 52. 5. Skilled trades occupations
  "NM_2080_1_6",                                                  # 53. 6. Caring, leisure and other service occupations
  "NM_2080_1_7",                                                  # 54. 7. Sales and customer service occupations
  "NM_2080_1_8",                                                  # 55. 8. Process, plant and machine operatives
  "NM_2080_1_9",                                                  # 56. 9. Elementary occupations
  
  
  # # TS066                                                           Economic activity status
  "NM_1565_1_8",                                                  # 57. Economically active: Unemployed
  #
  # # TS067                                                           Highest level of qualification
  # "NM_2084_1_4",                                                  # 58. Apprenticeship
'level_1_2_and_appr',
 # "under_2_level",                                                # 59. No qualification, Level 1 or 2 Level of qualification
  ## "level_3_or_4",                                                  # 60. Level 3 or Level 4 of qualification
  'NM_2084_1_5',                     #                                Level 3 qualifications   
  'NM_2084_1_6'                      #                               Level 4 qualifications or above

))


#### REORDER SOME VARIABLES

OAC_variables = (OAC_variables %>% dplyr::add_row(OAC_variables[OAC_variables$Code=="NM_2020_1_18",], .before = which(OAC_variables$Code=="NM_2039_1_1")))



OAC_variables = (OAC_variables %>%dplyr::add_row(OAC_variables[OAC_variables$Code=="level_1_2_and_appr",], .before = which(OAC_variables$Code=="NM_2084_1_5")))
OAC_variables = OAC_variables[-which(OAC_variables$Code=="NM_2020_1_18")[1],]  #,which(OAC_variables$Code=="under_2_level")[2]),]
OAC_variables = OAC_variables[-which(OAC_variables$Code=="level_1_2_and_appr")[2],] 

#### SHORTEN NAMES OF SOME VARIABLES
OAC_variables = OAC_variables %>% mutate(Name=gsub("Asian, Asian British or Asian Welsh: ", "", Name),
                                         Name=gsub("Unshared dwelling: Whole house or bungalow: ", "", Name),
                                         Name=recode(Name, 'Black, Black British, Black Welsh, Caribbean or African'='Black', 
                                                     'Address one year ago is the same as the address of enumeration'='Same address year ago'))  

#### CREATE A UNIQUE VARIABLE ID
OAC_variables$encoding = paste0("v", str_pad(c(1:nrow(OAC_variables)),2,pad="0"))
rownames(OAC_variables) <-1:nrow(OAC_variables)


write.csv(OAC_variables, "Data/Lookups/OAC_variables.csv", row.names=F)



####################################################
###########         LOADING DATA      ##############
####################################################

Census_2011_all_variables= read.csv("Data/Clean/Transformed/Census_2011_common_var_prop_perc_IHS_range.csv")
EW_OAC_all_variables = read.csv("Data/Clean/Transformed/Census_2021_common_var_prop_perc_IHS_range.csv")
Hybrid_OAC_all_variables = read.csv("Data/Clean/Transformed/hybrid_UK_2021_prop_perc_IHS_range.csv")
Aged_Scotland_all_variables = read.csv("Data/Clean/Transformed/hybrid_UK_2021_aged_Scotland_prop_perc_IHS_range.csv")



#### SUBSET DATA TO CONTAIN FINAL VARIABLES

for(df in c("Census_2011_all_variables", "Hybrid_OAC_all_variables", "EW_OAC_all_variables", "Aged_Scotland_all_variables")){
  tab = get(df)
  tab_OAC = subset(tab, select=names(tab) %in% c("Geography_Code", OAC_variables$Code))
  tab_OAC = tab_OAC[,c(1,match(OAC_variables$Code, colnames(tab_OAC)))]
  names(tab_OAC)[match(OAC_variables[,"Code"], names(tab_OAC))] = OAC_variables[,"encoding"]
  
  #### DENOTE COUNTRY OF A ROW
  tab_OAC = tab_OAC %>% mutate(country=substr(Geography_Code,1,1)) %>% mutate(country=ifelse(country=="S", "S", "EWN"))
  
  head(tab_OAC)
  
  if(df=="Census_2011_all_variables"){
    Census_2011_OAC = tab_OAC
    write.csv(Census_2011_OAC, "Data/Clean/Final_variables/Census_2011_OAC.csv", row.names=F)
  } else{
  assign(paste0(gsub("_all_variables", "", df)), tab_OAC)
    write.csv(tab_OAC, paste0("Data/Clean/Final_variables/", gsub("_all_variables", "", df),".csv"), row.names=F)
    ""
  }

}

identical(Aged_Scotland$v15, Hybrid_OAC$v15)
identical(Aged_Scotland$v16, Hybrid_OAC$v16)
identical(Aged_Scotland$v17, Hybrid_OAC$v17)
identical(Aged_Scotland$v18, Hybrid_OAC$v18)
identical(Aged_Scotland$v19, Hybrid_OAC$v19)







#########################################################
###########         MAPS COMPARISON      ################
#########################################################


Census_2011_Range_shp = merge(shp_2011_simple, Census_2011_OAC %>% select(-country), by="Geography_Code", all=T)
Hybrid_OAC_Range_shp = merge(hybrid_shp_simple, Hybrid_OAC %>% select(-country), by="Geography_Code", all=T) 
Aged_Scotland_Range_shp = merge(hybrid_shp_simple, Aged_Scotland %>% select(-country), by="Geography_Code", all=T) 





#rm(list=ls())
#Census_2011_shp = readRDS("Environments/Census_2011_shp.RDS")
#Hybrid_OAC_shp = readRDS("Environments/Hybrid_OAC_shp.RDS")
#OAC_variables=read.csv("OAC_variables.csv")

###########################################################
##########       COMPARE AGING AGE BANDS        ###########
###########################################################

for(var2 in unique(OAC_variables$encoding)[15:18]){
  
  start_time=Sys.time()
  t=OAC_variables[OAC_variables$encoding==var2, "Name"]
  gsub(" ", "_", t)
  print(paste0(var2, " ||| ", t))
 
  
  
  tm_normal= tm_shape(Hybrid_OAC_Range_shp[, var2]) + tm_fill(var2, title="Scotland carried forward") + 
    tm_layout(frame=F, legend.position = c("left", "top")) 
  tm_ageing = tm_shape(Aged_Scotland_Range_shp[, var2]) + tm_fill(var2, title="Aged Scotland") + 
    tm_layout(frame=F, legend.position = c("left", "top")) 
  
  tm = tmap_arrange(tm_normal, tm_ageing, outer.margins = c(0.001,0.001,0.001,0.001), ncol=2)
  
  tmap_save(tm, filename = paste0("Maps/Ageing_",  gsub(" ", "_", t), ".png"), dpi = 500)
   
}



#####   EQUAL CATEGORIES

for(d in c("Hybrid_OAC_Range_shp", "Aged_Scotland_Range_shp")){
  
  tab = get(d)

for(var2 in unique(OAC_variables$encoding)[13:18]){

  if(file.exists(paste0("Maps/Comparing_Ranges_UK/Pretty/Census_2011/Census_2011_", var2, ".png"))){
    next
  }
  
  start_time=Sys.time()
  t=OAC_variables[OAC_variables$encoding==var2, "Name"]
  gsub(" ", "", t)
  print(paste0(var2, " ||| ", t))
  
  
  tm_var_2011 = tm_shape(Census_2011_Range_shp[, var2]) + tm_fill(var2, title=t) + 
    tm_layout(frame=F, legend.position = c("left", "top")) 
  tm_var_hybrid = tm_shape(tab[, var2]) + tm_fill(var2, title=t) + 
  tm_layout(frame=F, legend.position = c("left", "top")) 
  
  tm = tmap_arrange(tm_var_2011, tm_var_hybrid, outer.margins = c(0.001,0.001,0.001,0.001), ncol=2)

  tmap_save(tm, filename = paste0("Maps/Comparing_Ranges_UK/Pretty/",  gsub(" ", "", t), ".png"), dpi = 500)
  
  print(round(Sys.time() - start_time),4)
}

  rm(var2, t, tm_var_2011,start_time)

}





#####   QUANTILE CATEGORIES
  for(d in c("Hybrid_OAC_Range_shp", "Aged_Scotland_Range_shp")){
    
    tab = get(d)
for(var2 in unique(OAC_variables$encoding)[13:18]){

  if(file.exists(paste0("Maps/Comparing_Ranges_UK/Quantile/",d,"_" ,var2,"_", gsub(" ", "_", t), ".png"))){
    next
  }
  
  start_time=Sys.time()
  t=OAC_variables[OAC_variables$encoding==var2, "Name"]
  print(paste0(var2, " ||| ", t))
  
  tm_var_2011 = tm_shape(Census_2011_Range_shp[, var2]) + tm_fill(var2, title=t, style="quantile") + 
    tm_layout(frame=F, legend.position = c("left", "top")) 
  tm_var_hybrid = tm_shape(tab[, var2]) + tm_fill(var2, title=t, style="quantile") + 
    tm_layout(frame=F, legend.position = c("left", "top")) 
  tm = tmap_arrange(tm_var_2011, tm_var_hybrid, outer.margins = c(0.001,0.001,0.001,0.001), ncol=2)
  
  tmap_save(tm, filename = paste0("Maps/Comparing_Ranges_UK/Quantile/",d,"_" ,var2,"_", gsub(" ", "_", t), ".png"), dpi = 500)
  print(round(Sys.time() - start_time, 4))
}
    
  rm(var2, t, tm_var_2011,tm_var_hybrid, tm , start_time)
}

  
  
  
  
  
  Census_2011_perc = read.csv("Data/Clean/Percentages/Census_2011_common_var_prop_perc.csv")
  hybrid_perc =  read.csv("Data/Clean/Percentages/hybrid_UK_2021_prop_perc.csv")
  Aged_Scotland_perc = read.csv("Data/Clean/Percentages/hybrid_UK_2021_aged_Scotland_prop_perc.csv")
  
  Aged_Scotland_shp =  merge(hybrid_shp_simple_200,Aged_Scotland_perc ,by="Geography_Code", all=T)
  Census_2011_perc_shp = merge(shp_2011_simple_200, Census_2011_perc,by="Geography_Code", all=T)
  Hybrid_perc_shp = merge(hybrid_shp_simple_200,hybrid_perc ,by="Geography_Code", all=T)
  
  
  
  #####   COMPARING PERCENTAGES
  for(var2 in unique(OAC_variables$Code)){
    
    start_time=Sys.time()
    
    t2=OAC_variables[OAC_variables$Code==var2, "Name"]
    enc =  OAC_variables[OAC_variables$Code==var2,"encoding"]
    
    if(file.exists(paste0("Maps/Comparing_Percents/OAC_Percents/", enc,"_", gsub(" ", "_", t), ".png"))){
      next
    }
    
    print(paste0(enc, " ||| ", t))
    
    br1 = floor(min(c(Census_2011_perc[,var], hybrid_perc[,var])))
    br2 = ceiling(max(c(Census_2011_perc[,var], hybrid_perc[,var])))
    
    tm_var_2011 = tm_shape(Census_2011_perc_shp[, var2]) + 
      tm_fill(var2, title=t, breaks = seq(br1,br2, (br2-br1)/5)) + 
      tm_layout(frame=F, legend.position = c("left", "top")) 
    
    tm_var_hybrid = tm_shape(Hybrid_perc_shp[, var2]) + 
      tm_fill(var2, title=t, breaks = seq(br1,br2, (br2-br1)/5)) + 
      tm_layout(frame=F, legend.position = c("left", "top")) 
    
    tm = tmap_arrange(tm_var_2011, tm_var_hybrid, outer.margins = c(0.001,0.001,0.001,0.001), ncol=2)
    
    system.time(tmap_save(tm, filename = paste0("Maps/Comparing_Percents/OAC_Percents/",enc,"_", gsub(" ", "_", t), ".png"), dpi = 1000))
    
    
    tm_var_2011 = tm_shape(Census_2011_Range_shp[, enc]) + 
      tm_fill(enc, title=t, style="quantile") + 
      tm_layout(frame=F, legend.position = c("left", "top")) 
    
    tm_var_hybrid = tm_shape(Hybrid_OAC_Range_shp[, enc]) + 
      tm_fill(enc, title=t, style="quantile") + 
      tm_layout(frame=F, legend.position = c("left", "top")) 
    
    tm = tmap_arrange(tm_var_2011, tm_var_hybrid, outer.margins = c(0.001,0.001,0.001,0.001), ncol=2)
    system.time(tmap_save(tm, filename = paste0("Maps/Comparing_Ranges_UK/OAC_Range/quant_",enc,"_", gsub(" ", "_", t2), ".png"), dpi = 1000))
    
    
    
    
    print(round(Sys.time() - start_time, 4))
  }
  rm(var2, t, tm_var_2011,tm_var_hybrid, tm , start_time)
  
  
  
  
  
  
  
#   
#   qual = read.csv("~/Desktop/qual.csv",row.names=NULL, skip=7)[,c(2,15,16)]
#   qual = qual[complete.cases(qual),]
#   colnames(qual) = c("Geography_Code", "Level4", "Level4_perc")
#   
#   qual_m = merge(qual, Census_2011_perc %>% select(Geography_Code, NM_2084_1_6), by="Geography_Code", all=T)
#   qual_m = merge(qual_m, Census_2011_OAC %>% select(Geography_Code, v61), by="Geography_Code", all=T)
#   qual_m$level4_nomis_trans = log(qual_m[,"Level4_perc"] + sqrt(qual_m[,"Level4_perc"]^2+1))
#   qual_m$level4_nomis_trans = round(Range_0_to_1(qual_m$level4_nomis_trans),4)
#   qual_m$v61=round(qual_m$v61,4)
#   
#   
#   summary(qual_m)
#   head(qual_m)
#   
#   # qual_m = merge(qual_m, hybrid_perc %>% select(Geography_Code, NM_2084_1_6), by="Geography_Code", all=T, suffixes = c("_perc", "_hybrid"))
#   
# 
#   system.time(tmap_save(tm_shape(merge(shp_2011_simple_200, qual_m,by="Geography_Code", all=T)) + tm_fill("Level4_perc"), filename="~/Desktop/level4.png", dpi=1000))
#   
#   head(qual)
#   
#   
#   Range_0_to_1 <- function(x){(x-min(x))/(max(x)-min(x))}
# 
#   
# 


############################################################
###########         DENSITY COMPARISON      ###############
###########################################################


  
Hybrid_OAC = Hybrid_OAC %>% mutate(country=substr(Geography_Code,1,1))
Hybrid_OAC_EWNI_S = Hybrid_OAC %>% mutate(country=ifelse(country=="S", "S", "EWN"))


for(ts in unique(OAC_variables$TS_code)){
  
  
  table_codes = (OAC_variables %>% filter(TS_code==ts))$encoding
  
  start_time=Sys.time()
  print(ts)
  dat = Hybrid_OAC_EWNI_S[,c("Geography_Code", table_codes, "country")] %>% pivot_longer(cols=-c(Geography_Code,country), names_to="encoding", values_to="value")
  dat = merge(dat, OAC_variables %>% select(encoding, Name), by="encoding")
  
  dat_2011 = Census_2011_range[,c("Geography_Code", table_codes, "country")] %>% pivot_longer(cols=-c(Geography_Code,country), names_to="encoding", values_to="value")
  dat_2011 = merge(dat_2011, OAC_variables %>% select(encoding, Name), by="encoding")
  
  r=rbind(dat %>% mutate(source="Hybrid"), dat_2011 %>% mutate(source="Census2011"))
  gg= ggplot(data = dat) + 
    stat_density(mapping =aes(x=value, col=country), geom="line", position = "identity", size=1, alpha=0.7 )+
    labs(x="Range", y="Density")+ 
    theme_minimal() + theme( axis.title = element_text(size=21),axis.text = element_text(size=17),strip.text.x = element_text(size=18), 
                             legend.position = "bottom",legend.key.height = unit(01.2, "cm"),  legend.key.width = unit(1.75,"cm"), 
                             legend.text =  element_text(size=16),legend.title = element_text(size=19), legend.box = "vertical")
  
  gg2= ggplot()+
    geom_density(r %>% filter(source=="Census2011"), mapping=aes(x=value,fill=country),col=NA, alpha=0.3)+
    stat_density(r %>% filter(source=="Hybrid"), mapping = aes(x=value, colour=country), geom="line", position = "identity",size=1, alpha=0.8) +
    labs(x="Range", y="Density")+ 
    theme_minimal() + theme( axis.title = element_text(size=21),axis.text = element_text(size=17),strip.text.x = element_text(size=18), 
                             legend.position = "bottom",legend.key.height = unit(01.2, "cm"),  legend.key.width = unit(1.75,"cm"), 
                             legend.text =  element_text(size=16),legend.title = element_text(size=19), legend.box = "vertical")
  
  if(ts%in% c("TS004", "TS021")){
    gg= gg+ facet_wrap(~Name, labeller = labeller(Name = label_wrap_gen(30)), ncol=2, scales="free_y")
    gg2=gg2 + facet_wrap(~Name, labeller = labeller(Name = label_wrap_gen(30)), ncol=2, scales="free_y")
  } else{
    gg= gg + facet_wrap(~Name, labeller = labeller(Name = label_wrap_gen(30)), ncol=2) 
    gg2=gg2 +facet_wrap(~Name, labeller = labeller(Name = label_wrap_gen(30)), ncol=2, scales="free_y") + 
      guides(fill = guide_legend(order = 2, override.aes = list(shape=c(15,15)), title="Census 2011"))+ 
      guides(color = guide_legend(order = 1, override.aes = list(shape = c(16, 16), linetype = c("solid", "solid")), title="Hybrid 2021")) 
    
  }
  
  
  ggsave(filename = paste0("Plots/V62/Versus_Scotland/density_", ts, ".png"),gg, dpi = 500, bg="white",  width=250, height=297,unit="mm") 
  ggsave(filename = paste0("Plots/V62/Census2011/density_", ts, ".png"),gg2, dpi = 500, bg="white",  width=250, height=297,unit="mm") 
  # stat_density(mapping=aes(x=value, col="country"), stat="identity" )
  end_time=Sys.time()
  print(round(end_time-start_time, 4))
}
  