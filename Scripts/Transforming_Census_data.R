setwd("~/Desktop/PhD/GIT/OAC2021")

library(nomisr)
library(dplyr)
library(sf)
library(tmap)
library(tidyverse)
library(readxl)
library(Hmisc)
library(tmap)
library(stringr)
options( digits=10, max.print = 1000)


##### LOAD VARIABLE LOOKUP

all_variable_codes = read.csv("Data/Lookups/all_variable_codes.csv")
OA_to_LA_lookup = read.csv("../../shapefiles/OA_2011_to_OA_2021.csv") %>% distinct(OA21CD, .keep_all = T )%>% select(OA21CD, LAD22CD) 
OA_changes = read.csv("../../shapefiles/OA_2011_to_OA_2021.csv")
head(OA_to_LA_lookup)

######## READ CENSUS CSV

Census_2011 =  read.csv("Data/Clean/Raw_counts/Census_2011_all.csv")
Census_2011_oa_changed = read.csv("Data/Clean/Raw_counts/OA_changed_Census_2011_raw_all.csv")
Census_2021 = read.csv("Data/Clean/Raw_counts/Census_2021_all.csv")
Census_2021_oa_changed = read.csv("Data/Clean/Raw_counts/OA_changed_Census_2021_raw_all.csv")


head(Census_2011)





#######       SUBSET TO CONTAIN COMMON VARIABLES
Census_2011_common_var = subset(Census_2011, select=names(Census_2011) %in% c("Geography_Code",unique(all_variable_codes$Code)))
Census_2021_common_var = subset(Census_2021, select=names(Census_2021) %in% c("Geography_Code",unique(all_variable_codes$Code)))
Census_2011_oa_changed_common_var = subset(Census_2011_oa_changed, select=names(Census_2011_oa_changed) %in% c("Geography_Code",unique(all_variable_codes$Code)))
Census_2021_oa_changed_common_var = subset(Census_2021_oa_changed, select=names(Census_2021_oa_changed) %in% c("Geography_Code",unique(all_variable_codes$Code)))


# 
# ensus_2011_common_var[,match(colnames(Census_2021_common_var),colnames(Census_2011_common_var))]

# ensus_2011_common_var[,c(1,match(OAC_variables$Code, colnames(Hybrid_OAC)))]
# 
colnames(Census_2011_common_var)
colnames(Census_2021_common_var)




#############################################################
########          AGGREGATING CATEGORIES          ###########
#############################################################



for(dat in c("Census_2011_common_var", "Census_2021_common_var", "Census_2011_oa_changed_common_var", "Census_2021_oa_changed_common_var")){
  
  tab= get(dat) %>% mutate(age_5_19 = NM_2020_1_2 + NM_2020_1_3 + NM_2020_1_4,
                           age_20_29 = NM_2020_1_5 + NM_2020_1_6,
                           age_30_39 = NM_2020_1_7 + NM_2020_1_8,
                           age_40_54 = NM_2020_1_9 + NM_2020_1_10 + NM_2020_1_11,
                           age_55_69 = NM_2020_1_12 + NM_2020_1_13 + NM_2020_1_14,
                           age_70_84 = NM_2020_1_15 + NM_2020_1_16 + NM_2020_1_17,
                           age_5_14 = NM_2020_1_2 + NM_2020_1_3, 
                           age_25_44 = NM_2020_1_6 + NM_2020_1_7+NM_2020_1_8+NM_2020_1_9, 
                           age_45_64 = NM_2020_1_10 + NM_2020_1_11 + NM_2020_1_12 + NM_2020_1_13,
                           age_65_84 = NM_2020_1_14 +  NM_2020_1_15 + NM_2020_1_16 + NM_2020_1_17, 
                           
                           separated_divorced = NM_2022_1_1004 + NM_2022_1_1005,
                           no_children = NM_2023_1_4 + NM_2023_1_7,
                           dependent_children =  NM_2023_1_5 +  NM_2023_1_8  + NM_2023_1_10 ,# + NM_2023_1_13 ,
                           nondependent_children = NM_2023_1_6  + NM_2023_1_9 + NM_2023_1_11 ,
                           multi_ethnic_household =  NM_2042_1_3 + NM_2042_1_4 + NM_2042_1_5,
                           cannot_speak_English =  NM_2048_1_4 + NM_2048_1_5,
                           provides_unpaid_care = NM_2057_1_101 + NM_2057_1_102 + NM_2057_1_6  ,
                           flat = NM_1549_1_7 + NM_1549_1_8 + NM_1549_1_9,
                           cars_2_or_more = NM_2063_1_3 +  NM_2063_1_4,
                           under_occupation = NM_2071_1_1 + NM_2071_1_2,
                           overcrowding = NM_2071_1_4 +  NM_2071_1_5 ,
                           ownership_or_shared = NM_2072_1_1001 + NM_2072_1_1002,
                           under_2_level = NM_2084_1_1 + NM_2084_1_2 + NM_2084_1_3,
                           level_3_or_4 =  NM_2084_1_5 + NM_2084_1_6,
                           level_1_2_and_appr = NM_2084_1_2 + NM_2084_1_3+NM_2084_1_4,
                           NM_2037_1_7 = NM_2037_1_7 + NM_2037_1_8 + NM_2037_1_9) %>% select(-NM_2037_1_8,-NM_2037_1_9)
  
  
  assign(dat,tab)
  
}

########    AGE
##      age_5_19 = NM_2020_1_2 + NM_2020_1_3 + NM_2020_1_4
##      age_20_29 = NM_2020_1_5 + NM_2020_1_6
##      age_30_39 = NM_2020_1_7 + NM_2020_1_8
##      age_40_54 = NM_2020_1_9 + NM_2020_1_10 + NM_2020_1_11
##      age_55_69 = NM_2020_1_12 + NM_2020_1_13 + NM_2020_1_14
##      age_70_84 = NM_2020_1_15 + NM_2020_1_16 + NM_2020_1_17

#######      Legal partnership status
###   Separated_divorced = NM_2022_1_1004 + NM_2022_1_1005

#######       Household composition
## Families with no children = 
##        NM_2023_1_4 (Single family household: Married or civil partnership couple: No children) + 
##        NM_2023_1_7 (Single family household: Cohabiting couple family: No children)  
## Families with dependent children =
##         NM_2023_1_5  (Single family household: Married or civil partnership couple: Dependent children) + 
##        NM_2023_1_8   (Single family household: Cohabiting couple family: With dependent children) + 
##        NM_2023_1_10  (Single family household: Lone parent family: With dependent children) + 
##        NM_2023_1_13  (Other household types: With dependent children)
## Families with non-dependent children =
##        NM_2023_1_6   (Single family household: Married or civil partnership couple: All children non-dependent) +
##        NM_2023_1_9   (Single family household: Cohabiting couple family: All children non-dependent) +
##        NM_2023_1_11  (Single family household: Lone parent family: All children non-dependent)

#######      Multiple ethnic group
### Ethnic group differs  = 
##        NM_2042_1_3       (Ethnic groups differ between generations but not within partnerships) + 
##        NM_2042_1_4       (Ethnic groups differ within partnerships) +
##        NM_2042_1_5       (Any other combination of multiple ethnic identities)

##### Proficiency in English
## Cannot speak English well or at all =
##      NM_2048_1_4  (Cannot speak English well) +
##      NM_2048_1_5  # Cannot speak English)

#### Provision of unpaid care
## Provides unpaid care =
##        NM_2057_1_101     (Provides 19 hours or less unpaid care a week) +
##        NM_2057_1_102     (Provides 20 to 49 hours unpaid care a week) +
##        NM_2057_1_6       (Provides 50 or more hours unpaid care a week)

####  Accommodation type
##        Flat = 
##        NM_1549_1_7       (Unshared dwelling: Flat, maisonette or apartment: Purpose-built block of flats or tenement) + 
##        NM_1549_1_8       (Unshared dwelling: Flat, maisonette or apartment: Part of a converted or shared house (including bed-sits)) +
##        NM_1549_1_9       (Unshared dwelling: Flat, maisonette or apartment: In commercial building)

#### Car or van availability
##  2 or more cars =
##          NM_2063_1_3     (2 cars or vans in household) +
##          NM_2063_1_4     (3 or more cars or vans in household)

####    Occupancy rating
##      Under-occpuation = 
##                    NM_2071_1_1       (Occupancy rating of rooms: +2 or more) +
##                    NM_2071_1_2       (Occupancy rating of rooms: +1)
##      Over-crowding = 
##                    NM_2071_1_4       (Occupancy rating of rooms: -1)
##                    NM_2071_1_5       (Occupancy rating of rooms: -2 or less)

####    Tenure
##      Ownership or shared ownership = 
##          NM_2072_1_1001        (Owned) + 
##          NM_2072_1_1002        (hared ownership)

#### Highest level of qualifications
## Under 2 level qualification = 
##        NM_2084_1_1       (No qualifications) + 
##        NM_2084_1_2       (Level 1 and entry level qualifications) +
##        NM_2084_1_3       (Level 2 qualifications)

##  3 or 4 level qualification =
##        NM_2084_1_5     (Level 3 qualifications)
##        NM_2084_1_6     (Level 4 qualifications or above)


# Level 1, 2 or Apprentencishp
##        NM_2084_1_2       (Level 1 and entry level qualifications)
##        NM_2084_1_3       (Level 2 qualifications)
##        NM_2084_1_4       Apprentenciship



aggregated_categories = data.frame(Code="age_5_19", Name="Aged 5 to 19", TableCode="NM_2020_1", TS_code="TS007A", TableName="Age structure", CategoryCode=NA, encoding=NA)
aggregated_categories = rbind(aggregated_categories, data.frame(Code="age_20_29", Name="Aged 20 to 29", TableCode="NM_2020_1", TS_code="TS007A", TableName="Age structure", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="age_30_39", Name="Aged 30 to 39", TableCode="NM_2020_1", TS_code="TS007A", TableName="Age structure", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="age_40_54", Name="Aged 40 to 54", TableCode="NM_2020_1", TS_code="TS007A", TableName="Age structure", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="age_55_69", Name="Aged 55 to 69", TableCode="NM_2020_1", TS_code="TS007A", TableName="Age structure", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="age_5_14", Name="Aged 5 to 14", TableCode="NM_2020_1", TS_code="TS007A", TableName="Age structure", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="age_25_44", Name="Aged 25 to 44", TableCode="NM_2020_1", TS_code="TS007A", TableName="Age structure", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="age_45_64", Name="Aged 45 to 64", TableCode="NM_2020_1", TS_code="TS007A", TableName="Age structure", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="age_65_84", Name="Aged 65 to 84", TableCode="NM_2020_1", TS_code="TS007A", TableName="Age structure", CategoryCode=NA, encoding=NA))

aggregated_categories = rbind(aggregated_categories,data.frame(Code="separated_divorced", Name="Separated or divorced", TableCode="NM_2022_1", TS_code="TS002", TableName="Legal partnership status", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="no_children", Name="Families with no children", TableCode="NM_2023_1", TS_code="TS003", TableName="Household composition", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="dependent_children", Name="Families dependent children", TableCode="NM_2023_1", TS_code="TS003", TableName="Household composition", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="nondependent_children", Name="Families with non-dependent children", TableCode="NM_2023_1", TS_code="TS003", TableName="Household composition", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="multi_ethnic_household", Name="Multi-ethnic household", TableCode="NM_2042_1", TS_code="TS023", TableName="Multiple ethnic group", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="cannot_speak_English", Name="Cannot speak English well or at all", TableCode="NM_2048_1", TS_code="TS029", TableName="Proficiency in English", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="provides_unpaid_care", Name="Provides unpaid care", TableCode="NM_2057_1", TS_code="TS039", TableName="Provision of unpaid care", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="flat", Name="Lives in a flat", TableCode="NM_1549_1", TS_code="TS044", TableName="Accomodation type", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="cars_2_or_more", Name="Two or more cars or vans", TableCode="NM_2063_1", TS_code="TS045", TableName="Car or van availability", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="under_occupation", Name="Under occupation", TableCode="NM_2071_1", TS_code="TS053", TableName="Occupancy rating for rooms", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="overcrowding", Name="Overcrowding", TableCode="NM_2071_1", TS_code="TS053", TableName="Occupancy rating for roomss", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="ownership_or_shared", Name="Ownership or shared ownership", TableCode="NM_2072_1", TS_code="TS054", TableName="Tenure", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="under_2_level", Name="No qualification, Level 1 or 2 Level of qualification", TableCode="NM_2084_1", TS_code="TS067", TableName="Highest level of qualification", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="level_3_or_4", Name="Level 3 or Level 4 of qualification", TableCode="NM_2084_1", TS_code="TS067", TableName="Highest level of qualification", CategoryCode=NA, encoding=NA))
aggregated_categories = rbind(aggregated_categories,data.frame(Code="level_1_2_and_appr", Name="Level 1, Level 2 or Apprenticeship", TableCode="NM_2084_1", TS_code="TS067", TableName="Highest level of qualification", CategoryCode=NA, encoding=NA))








#### list of common variables
common_variables = rbind(all_variable_codes %>% filter(Code %in% colnames(Census_2011_common_var)), 
                         aggregated_categories %>% rename(TableName21=TableName,CategoryCode11=CategoryCode) %>% select(-encoding)) %>% 
  arrange(TS_code)

### get a list of denominators (excluding Density)
denominators = common_variables %>% filter(CategoryCode11==0 | TS_code=="TS006")

### list of common variables, without denominators
common_variables_without_denominators = common_variables %>% filter(CategoryCode11!=0 | is.na(CategoryCode11))


#write.csv(common_variables, "common_variables.csv", row.names=F)



#################################################
########      CALCULATE PERCENTAGES       #######
#################################################

rm(Census_2011_common_var_prop, Census_2021_common_var_prop, Census_2011_oa_changed_common_var_prop, Census_2021_oa_changed_common_var_prop)

for(deno in unique(denominators$Code)){
  
  #print(paste(deno,"||| ", (paste(match(deno, unique(denominators$Code)))),
  #            "out of", length(unique(denominators$Code))))
### get TS code of a denominator
denominator_TS_code = denominators[denominators$Code==deno, "TS_code"]

### get nominators of the denominator
nominators = (common_variables %>% filter(TS_code==denominator_TS_code, Code!=deno))$Code


for(dat in c("Census_2011_common_var", "Census_2021_common_var", "Census_2011_oa_changed_common_var", "Census_2021_oa_changed_common_var")){
  
  table = get(dat)
  table = table[, c("Geography_Code",deno, nominators)]
  
  if(deno!="NM_2026_1_0"){
    
    for(i in nominators){
      table[,i] = round((table[,i] / table[, deno]), digits=4)
      table[is.na(table)]=0
# 
     #  
     #  table[,i] = table[,i]*100
     #  table[,i] = log(table[,i] + sqrt(table[,i]^2+1))
     #  table[,i] = Range_0_to_1(table[,i])
      
    }
    #### remove denominator
    table = table[,c("Geography_Code", nominators)]
  }
  
  

  
  if(!exists(paste0(dat,"_prop"))){
    assign(paste0(dat,"_prop"),table)
    
  } else {
    assign(paste0(dat,"_prop"), merge(get(paste0(dat,"_prop")), table, by="Geography_Code", all=T)) 
  }
  
  
  cat("\r",deno,"||| ", (paste(match(deno, unique(denominators$Code)))),
      "out of", length(unique(denominators$Code))," --- ", dat)
  flush.console()
}
}


#common_variables_without_denominators$encoding = paste0("r", str_pad(c(1:nrow(common_variables_without_denominators)),3,pad="0"))

head(Census_2011_common_var_prop)

#write.csv(Census_2011_common_var_prop, "Census_2011_common_var_prop.csv", row.names=F)
#write.csv(Census_2011_common_var, "Census_2011_common_var.csv", row.names=F)
#





##########################################################
##########      DO NI AND SCOTLAND          ##############
##########################################################

source("Scripts/NI.R")



head(Final_NI_2021)
head(Census_2021_common_var)
head(Census_2011_common_var)
identical(colnames(Census_2011_common_var), colnames(Census_2021_common_var))
identical(colnames(Census_2011_common_var), colnames(Final_NI_2021))



hybrid_UK_2021 = rbind(Census_2021_common_var, Final_NI_2021, Census_2011_common_var %>% filter(grepl("S", Geography_Code)))



#### AGING SCOTLAND 
Aged_Scotland_2011 = 
  Census_2011_common_var %>% filter(grepl("S", Geography_Code)) %>% 
  mutate(age_25_44 = NM_2020_1_4+NM_2020_1_5+NM_2020_1_6+NM_2020_1_7, # 15-34
         age_45_64 = NM_2020_1_8+NM_2020_1_9+NM_2020_1_10+NM_2020_1_11,   # 34-54
         age_65_84 = NM_2020_1_12+NM_2020_1_13+NM_2020_1_14+NM_2020_1_15, # 55 - 74
         NM_2020_1_18 = NM_2020_1_16 + NM_2020_1_17 , #????
         # age_20_29, # this will be the same, because there's probably a georaphy to young people
         age_30_39 = NM_2020_1_5 + NM_2020_1_6, # 20-29
         age_40_54 = NM_2020_1_7 + NM_2020_1_8 + NM_2020_1_9, #30 - 44
         age_55_69 = NM_2020_1_10 + NM_2020_1_11 + NM_2020_1_12, #45 - 59
         age_70_84 = NM_2020_1_13 + NM_2020_1_14 + NM_2020_1_15) #60 -75 )


hybrid_UK_2021_aged_Scotland = rbind(Census_2021_common_var, Final_NI_2021,Aged_Scotland_2011)

write.csv(hybrid_UK_2021_aged_Scotland, "Data/Clean/Raw_counts/hybrid_UK_2021_aged_Scotland.csv", row.names=F)

rm(d)

for(d in c("hybrid_UK_2021", "hybrid_UK_2021_aged_Scotland")){
  tab = get(d)
  rm(prop)

for(deno in unique(denominators$Code)){
  
  #print(paste(deno,"||| ", (paste(match(deno, unique(denominators$Code)))),
  #            "out of", length(unique(denominators$Code))))
  ### get TS code of a denominator
  denominator_TS_code = denominators[denominators$Code==deno, "TS_code"]
  
  ### get nominators of the denominator
  nominators = (common_variables %>% filter(TS_code==denominator_TS_code, Code!=deno))$Code
  
  table = tab[, c("Geography_Code",deno, nominators)]
    
    if(deno!="NM_2026_1_0"){
      
      for(i in nominators){
        table[,i] = round((table[,i] / table[, deno]), digits=4)
        table[is.na(table)]=0
       
      }
  
      
      
      #### remove denominator
      table = table[,c("Geography_Code", nominators)]
    }
  

    if(!exists("prop")){
      prop = table
    } else {
      prop = merge(prop, table, by="Geography_Code", all=T)
      
    }
    
  assign(paste0(d, "_prop"), prop)
    
    cat("\r",deno,"||| ", (paste(match(deno, unique(denominators$Code)))),
        "out of", length(unique(denominators$Code)))
    flush.console()
  }
}


#save.image("IMAGE.RData")
#load("IMAGE.RData")
 
 ##########################################################################
 ############         STANDARDISED ILLNESS RATIO         ##################
 ##########################################################################
 
 
 #######################################
 ########         2021        ##########
 #######################################
 
 ill_prop_2021 =  read_excel("Data/SIR/disabilitycensus2021.xlsx", sheet="Table 2", skip=4)
 
 ill_prop_2021 = ill_prop_2021 %>% rename(Disability_status=`Disability status`, Age_specific_perc = `Age-specific Percentage`, Area_Code =`Area Code`) %>% 
    filter(Sex=="Persons", Category=="Two category", Disability_status == "Disabled") %>% 
    mutate(sir_age_band = ifelse(Age %in% c("Under 1", "1 to 4", "5 to 9", "10 to 14", 
                                       "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", "90+"), 
                                                       "0_14_65_over", "15_64")) %>%
   select(-Year, -Area_Code, -Notes, -`Lower 95% Confidence Interval`, -`Upper 95% Confidence Interval`, -Category) %>% 
   group_by(Disability_status, sir_age_band) %>% summarise(count=sum(Count), population=sum(Population), ill_prop = count/population)
 
 
 for(dat in c("Census_2021_common_var", "Census_2021_oa_changed_common_var")){
   
   tab2= get(dat) %>% 
     mutate(tot_0_14_65_over = NM_2020_1_1 + NM_2020_1_2 + NM_2020_1_3 + NM_2020_1_14 + NM_2020_1_15 + 
              NM_2020_1_16 + NM_2020_1_17 + NM_2020_1_18, tot_15_64 = NM_2020_1_0 - tot_0_14_65_over, 
                                 Disabled = NM_2056_1_1 + NM_2056_1_2) %>% 
     select(Geography_Code,NM_2020_1_0, tot_0_14_65_over, tot_15_64, Disabled) 
   
   
   SIR = tab2 %>% mutate(exp_ill_0_14_65_over = (ill_prop_2021[ill_prop_2021$sir_age_band=="0_14_65_over","ill_prop"][[1]] * tot_0_14_65_over), 
                                                        exp_ill_15_64 = (ill_prop_2021[ill_prop_2021$sir_age_band=="15_64","ill_prop"][[1]] * tot_15_64), 
                                                        exp_ill = exp_ill_0_14_65_over + exp_ill_15_64, 
                                                        SIR = round(Disabled / exp_ill *100,4)) %>% select(Geography_Code, SIR)
   
   
#   tab = add_column(get(dat) %>% arrange(Geography_Code), SIR %>% arrange(Geography_Code) %>% select(SIR), .after="NM_2056_1_4")
   tab = merge(tab, SIR, by="Geography_Code", all=T)
   if(dat=="Census_2021_common_var"){
     Census_2021_common_var_prop = merge(Census_2021_common_var_prop, SIR, by="Geography_Code", all=T)
   }
   if(dat =="Census_2021_oa_changed_common_var"){
     Census_2021_oa_changed_common_var_prop = merge(Census_2021_oa_changed_common_var_prop, SIR, by="Geography_Code", all=T)
   }
   assign(dat,tab)
   
 }


 
 
 #########################################
 ###########       2011          #########
 #########################################
 
 
 age_structure_oa = read.csv("Data/SIR/agestructureoa2011.csv", skip=7)[,-1]
 age_structure_oa = age_structure_oa %>%  mutate(tot_0_15_65_over = Age.0.to.4 + Age.5.to.7 + Age.8.to.9 + Age.10.to.14 + Age.15 + Age.65.to.74 + Age.75.to.84 + Age.85.to.89 + Age.90.and.over,
                                                 tot_16_64 = Age.16.to.17 + Age.18.to.19 + Age.20.to.24 + Age.25.to.29 + Age.30.to.44 + Age.45.to.59 + Age.60.to.64,
                                                 country = substr(mnemonic, 1,1),
                                                 country = recode(country,"E"= "England",  "N" = "Northern Ireland", "S" = "Scotland", "W" = "Wales")) %>% 
   select(mnemonic, All.usual.residents, tot_0_15_65_over,tot_16_64, country) 
 
 head(age_structure_oa)
 
 
 
 
 ill_prop_2011 = read.csv("Data/SIR/disabilitycensus2011.csv",skip = 7)[1:7,]
 colnames(ill_prop_2011) = c("country", "Total","Limited_a_lot","Limited_a_little", "Not_limited", "Limited_a_lot_16_to_64", "Limited_a_little_16_64", "Not_limited_16_64")
 
 ill_prop_2011 = ill_prop_2011 %>% mutate(Limited_a_lot_0_15_65_over = Limited_a_lot - Limited_a_lot_16_to_64,
                          Limited_a_little_0_15_65_over = Limited_a_little - Limited_a_little_16_64,
                          Not_limited_0_15_65_over = Not_limited - Not_limited_16_64,
                          Limited_0_15_65_over = Limited_a_lot_0_15_65_over + Limited_a_little_0_15_65_over, 
                          Limited_16_64 = Limited_a_lot_16_to_64 + Limited_a_little_16_64) 
 
 ill_prop_2011 = merge(ill_prop_2011, age_structure_oa %>% group_by(country) %>% summarise_if(is.numeric, sum), by="country") %>%
   select(country, Total, tot_0_15_65_over, tot_16_64, Limited_0_15_65_over,Limited_16_64) %>% mutate(Total=as.numeric(Total))
 
 ill_prop_2011 = ill_prop_2011 %>% janitor::adorn_totals("row", name = "United Kingdom") %>% 
   mutate(ill_prop_0_15_65_over = Limited_0_15_65_over / tot_0_15_65_over, 
          ill_prop_16_64 = Limited_16_64 / tot_16_64)
 
 
 
 SIR_2011 = merge(Census_2011 %>% mutate( Disabled = NM_2056_1_1 + NM_2056_1_2) %>% 
                    select(Geography_Code,NM_2020_1_0, Disabled), 
                  age_structure_oa %>% select(-All.usual.residents), by.x="Geography_Code", by.y="mnemonic") 
 
 SIR_2011  = SIR_2011 %>% mutate(exp_ill_0_15_65_over = (ill_prop_2011[ill_prop_2011$country=="United Kingdom", "ill_prop_0_15_65_over"]*tot_0_15_65_over), 
                                 exp_ill_16_64 = (ill_prop_2011[ill_prop_2011$country=="United Kingdom", "ill_prop_16_64"] * tot_16_64), 
                                 exp_ill = exp_ill_0_15_65_over + exp_ill_16_64,
                                 SIR = round(Disabled / exp_ill * 100, 4)) %>% select(Geography_Code, SIR)
 
 head(SIR_2011)
 
 #Census_2011 = add_column(Census_2011 %>% arrange(Geography_Code), SIR_2011 %>% arrange(Geography_Code) %>% select(SIR), .after="NM_2056_1_2")
 
 #Census_2011  = merge(Census_2011, SIR_2011 %>% arrange(Geography_Code) %>% select(SIR), by="Geography_Code", all=T)
 Census_2011_common_var_prop  = merge(Census_2011_common_var_prop, SIR_2011 %>% arrange(Geography_Code), by="Geography_Code", all=T)
 
 
 
 #### OA_changed 2011
 
 #### AGGREGATE TO 2021 CENSUS GROUPS  
 age_structure_oa_changed = merge(age_structure_oa, OA_changes %>% filter(CHNGIND=="M") %>% select(OA11CD, OA21CD), 
                                  by.x="mnemonic", by.y="OA11CD", all.x=T) %>%
   mutate(Geography_Code = ifelse(is.na(OA21CD), mnemonic, OA21CD)) %>% select(-OA21CD)%>% 
   group_by(Geography_Code) %>% summarise_if(is.numeric, sum) %>% ungroup() %>%
   mutate( country = substr(Geography_Code, 1,1),
           country = recode(country,"E"= "England",  "N" = "Northern Ireland", "S" = "Scotland", "W" = "Wales"))
 
 
 ill_prop_2011_oa_changed = read.csv("Data/SIR/disabilitycensus2011.csv",skip = 7)[1:7,]
 colnames(ill_prop_2011_oa_changed) = c("country", "Total","Limited_a_lot","Limited_a_little", "Not_limited", "Limited_a_lot_16_to_64", "Limited_a_little_16_64", "Not_limited_16_64")
 
 ill_prop_2011_oa_changed = ill_prop_2011_oa_changed %>% mutate(Limited_a_lot_0_15_65_over = Limited_a_lot - Limited_a_lot_16_to_64,
                                          Limited_a_little_0_15_65_over = Limited_a_little - Limited_a_little_16_64,
                                          Not_limited_0_15_65_over = Not_limited - Not_limited_16_64,
                                          Limited_0_15_65_over = Limited_a_lot_0_15_65_over + Limited_a_little_0_15_65_over, 
                                          Limited_16_64 = Limited_a_lot_16_to_64 + Limited_a_little_16_64) 
 
 
 ill_prop_2011_oa_changed = merge(ill_prop_2011_oa_changed, age_structure_oa_changed %>% group_by(country) %>% summarise_if(is.numeric, sum), by="country") %>%
   select(country, Total, tot_0_15_65_over, tot_16_64, Limited_0_15_65_over,Limited_16_64) %>% mutate(Total=as.numeric(Total))
 
 ill_prop_2011_oa_changed = ill_prop_2011_oa_changed %>% janitor::adorn_totals("row", name = "United Kingdom") %>% 
   mutate(ill_prop_0_15_65_over = Limited_0_15_65_over / tot_0_15_65_over, 
          ill_prop_16_64 = Limited_16_64 / tot_16_64)
 
 SIR_2011_oa_changed = merge(Census_2011_oa_changed %>% mutate( Disabled = NM_2056_1_1 + NM_2056_1_2) %>% 
                    select(Geography_Code,NM_2020_1_0, Disabled), 
                  age_structure_oa_changed %>% select(-All.usual.residents), by="Geography_Code") 
 
 SIR_2011_oa_changed  = SIR_2011_oa_changed %>% 
   mutate(exp_ill_0_15_65_over = (ill_prop_2011_oa_changed[ill_prop_2011_oa_changed$country=="United Kingdom", "ill_prop_0_15_65_over"]*tot_0_15_65_over), 
                                 exp_ill_16_64 = (ill_prop_2011_oa_changed[ill_prop_2011_oa_changed$country=="United Kingdom", "ill_prop_16_64"] * tot_16_64), 
                                 exp_ill = exp_ill_0_15_65_over + exp_ill_16_64,
                                 SIR = round(Disabled / exp_ill * 100, 4)) %>% select(Geography_Code, SIR)
 
 
 
 #Census_2011_oa_changed = add_column(Census_2011_oa_changed %>% arrange(Geography_Code), SIR_2011_oa_changed %>% arrange(Geography_Code) %>% select(SIR), .after="NM_2056_1_2")

 Census_2011_oa_changed_common_var_prop  = merge(Census_2011_oa_changed_common_var_prop, SIR_2011_oa_changed %>% arrange(Geography_Code), by="Geography_Code", all=T)


##############################################################
#############         HYBRID 2011/2021  SIR         ##########
##############################################################
 
 
 
ill_prop_2011
ill_prop_2021

ill_prop_2011 %>% filter(country %in% c("Northern Ireland", "Scotland"))
ill_prop_2011_format= data.frame(country=c("Northern Ireland", "Northern Ireland" , "Scotland", "Scotland"),
                         Disability_status=c("Disabled", "Disabled","Disabled", "Disabled"), 
           sir_age_band = c("0_15_65_over", "16_64", "0_15_65_over", "16_64"),
           count = c(177258,197388,517448,522923), 
           population = c(643043,1167820, 1806665,3488738),
           ill_prop=NA)

ill_prop_2011_format= ill_prop_2011_format %>% group_by(sir_age_band) %>% summarise(count=sum(count), population=sum(population)) %>%
  mutate(ill_prop = count/population)

ill_prop_hybrid = data.frame(age_band = c("young_old", "production_age"), 
                             count = c((ill_prop_2011_format[ill_prop_2011_format$sir_age_band=="0_15_65_over", "count"][[1]]) + 
                                      (ill_prop_2021[ill_prop_2021$sir_age_band=="0_14_65_over", "count"][[1]]),
                                      
                                      (ill_prop_2011_format[ill_prop_2011_format$sir_age_band=="16_64", "count"][[1]]) + 
                                        (ill_prop_2021[ill_prop_2021$sir_age_band=="15_64", "count"][[1]])
                                      ),
                             
                             population = c((ill_prop_2011_format[ill_prop_2011_format$sir_age_band=="0_15_65_over", "population"][[1]]) + 
                                         (ill_prop_2021[ill_prop_2021$sir_age_band=="0_14_65_over", "population"][[1]]),
                                       
                                       (ill_prop_2011_format[ill_prop_2011_format$sir_age_band=="16_64", "population"][[1]]) + 
                                         (ill_prop_2021[ill_prop_2021$sir_age_band=="15_64", "population"][[1]])
                             )
                             
                             ) %>% mutate(ill_prop = round(count/population,4))
                             
                           

tab= hybrid_UK_2021 %>% 
  mutate(tot_0_14_65_over = NM_2020_1_1 + NM_2020_1_2 + NM_2020_1_3 + NM_2020_1_14 + NM_2020_1_15 + 
           NM_2020_1_16 + NM_2020_1_17 + NM_2020_1_18, 
         tot_15_64 = NM_2020_1_0 - tot_0_14_65_over, 
         Disabled = NM_2056_1_1 + NM_2056_1_2) %>% 
  select(Geography_Code,NM_2020_1_0, tot_0_14_65_over, tot_15_64, Disabled) 


SIR = tab %>% mutate(exp_ill_young_old = (ill_prop_hybrid[ill_prop_hybrid$age_band=="young_old", "ill_prop"][[1]] * tot_0_14_65_over),
                     exp_production_age = (ill_prop_hybrid[ill_prop_hybrid$age_band=="production_age", "ill_prop"][[1]] *tot_15_64),
                     exp_ill = round(exp_ill_young_old + exp_production_age), 
                     SIR = round(Disabled/exp_ill * 100, 4)) %>% select(Geography_Code, SIR)


head(SIR)                   
                       
                      

hybrid_UK_2021 = merge(hybrid_UK_2021, SIR, by="Geography_Code", all=T)
hybrid_UK_2021_prop  = merge(hybrid_UK_2021_prop, SIR, by="Geography_Code", all=T)


hybrid_UK_2021_aged_Scotland  = merge(hybrid_UK_2021_aged_Scotland, SIR, by="Geography_Code", all=T)
hybrid_UK_2021_aged_Scotland_prop  = merge(hybrid_UK_2021_aged_Scotland_prop, SIR, by="Geography_Code", all=T)

#############################################
#######       TRANSFORMATIONS       #########
#############################################

Range_0_to_1 <- function(x){(x-min(x))/(max(x)-min(x))}

# df_list = c("Census_2011_common_var","Census_2021_common_var",
#             "Census_2011_oa_changed_common_var", "Census_2021_oa_changed_common_var",
#             "Census_2011_common_var_prop", "Census_2021_common_var_prop",
#             "Census_2011_oa_changed_common_var_prop", "Census_2021_oa_changed_common_var_prop", 
#             "hybrid_UK_2021", "hybrid_UK_2021_prop")

df_list = c("Census_2011_common_var_prop", "Census_2021_common_var_prop",
          # "Census_2011_oa_changed_common_var_prop", "Census_2021_oa_changed_common_var_prop", 
           "hybrid_UK_2021_prop", "hybrid_UK_2021_aged_Scotland_prop")


for(dat in df_list){
  
  print(dat)
  
  
  
  tab = get(dat)
  
  
  
  #### percentages
  tab <- tab %>% 
    mutate_at(vars(-Geography_Code, -SIR, NM_2026_1_0), ~ . * 100)
  
  assign(paste0(dat,"_perc"),tab)
  
  
  #### Inverse Hyperbolic Sine
  tab[,-1] = log(tab[,-1] + sqrt(tab[,-1]^2+1))
#  assign(paste0(dat,"_perc_IHS"),tab)
  
  #### Range standardization
  tab[,-1] = apply(tab[,-1], 2, Range_0_to_1)
  tab[,-1] = round(tab[,-1],4)
  assign(paste0(dat,"_perc_IHS_range"),tab)
  
  print(paste0("Creating: ", dat,"_perc", " ||| ", #dat,"_perc_IHS", " ||| ", 
               dat,"_perc_IHS_range"))
}




head(hybrid_UK_2021_prop_perc_IHS_range)
head(Census_2021_common_var_prop_perc_IHS_range)





write.csv(Census_2011_common_var_prop_perc, "Data/Clean/Percentages/Census_2011_common_var_prop_perc.csv", row.names = F)
write.csv(Census_2021_common_var_prop_perc, "Data/Clean/Percentages/Census_2021_common_var_prop_perc.csv", row.names = F)
write.csv(hybrid_UK_2021_prop_perc, "Data/Clean/Percentages/hybrid_UK_2021_prop_perc.csv", row.names = F)
write.csv(hybrid_UK_2021_aged_Scotland_prop_perc, "Data/Clean/Percentages/hybrid_UK_2021_aged_Scotland_prop_perc.csv", row.names = F)

write.csv(Census_2011_common_var_prop_perc_IHS_range,"Data/Clean/Transformed/Census_2011_common_var_prop_perc_IHS_range.csv", row.names = F)
write.csv(Census_2021_common_var_prop_perc_IHS_range,"Data/Clean/Transformed/Census_2021_common_var_prop_perc_IHS_range.csv", row.names = F)
write.csv(hybrid_UK_2021_prop_perc_IHS_range,"Data/Clean/Transformed/hybrid_UK_2021_prop_perc_IHS_range.csv", row.names = F)
write.csv(hybrid_UK_2021_aged_Scotland_prop_perc_IHS_range,"Data/Clean/Transformed/hybrid_UK_2021_aged_Scotland_prop_perc_IHS_range.csv", row.names = F)






##########################################################
#######       COMBINING 2011 and 2021 DATA       #########
##########################################################


# ### round to 4 decimal points
# Census_2011_common_var_prop_perc_IHS_range[,-1] = round(Census_2011_common_var_prop_perc_IHS_range[,-1],4)
# hybrid_UK_2021_prop_perc_IHS_range[,-1] = round(hybrid_UK_2021_prop_perc_IHS_range[,-1], 4)
# 
nrow(Census_2011_common_var_prop_perc_IHS_range)
nrow(hybrid_UK_2021_prop_perc_IHS_range)




common_variables = rbind(common_variables,data.frame(Code="SIR", Name="Standardised Illness Ratio", 
                                                     TableCode="NM_2056_1", TS_code="TS038", TableName21="Disability", 
                                                     CategoryCode11=NA))
common_variables = common_variables %>% mutate(id=1:nrow(common_variables)) %>% arrange(TS_code, id) %>% select(-id)
write.csv(common_variables, "Data/Lookups/common_variables.csv", row.names = F)














plot_colors = RColorBrewer::brewer.pal(n = 4, name = "Set1")


for(ts in unique(common_variables_without_denominators$TS_code)[]){
  cat("\r", ts, " --- ", paste(match(ts, unique(common_variables_without_denominators$TS_code))), "out of", length(unique(common_variables_without_denominators$TS_code)))  
  flush.console()
  table_codes = (common_variables_without_denominators %>% filter(TS_code==ts))$Code
  
#  EW21_S11_prop_perc_IHS_range[, table_codes]
#  Census_2011_common_var_prop_perc_IHS_range[, table_codes]
#  
#  r = rbind( EW21_S11_prop_perc_IHS_range[, table_codes] ,  Census_2011_common_var_prop_perc_IHS_range[, table_codes])
  
  
  dat1 = hybrid_UK_2021_prop_perc_IHS_range[,c("Geography_Code", table_codes)] %>% 
    tidyr::pivot_longer(cols = -c(Geography_Code), 
                        names_to = "Code", values_to = "Value") 
  
  dat2 = Census_2011_common_var_prop_perc_IHS_range[,c("Geography_Code", table_codes)] %>% 
    tidyr::pivot_longer(cols = -c(Geography_Code), 
                        names_to = "Code", values_to = "Value")
  
  r = rbind(dat1 %>% mutate(source="Hybrid"), dat2  %>% mutate(source="Census2011")) %>% 
    mutate(country = ifelse(grepl("S", Geography_Code), "Scotland", "EW"),
           country = ifelse(grepl("N", Geography_Code), "N", country)) %>% 
    mutate(diff = as.factor(paste0(source,"_",country)))
  
  r = merge(r, common_variables_without_denominators %>% select(Code, Name), by="Code", all.x=T)
 
  

  if(length(table_codes)<10){
    
    
      gg_plot = ggplot() + 
        geom_density(r %>% filter(source=="Census2011"), mapping=aes(x=Value,fill=country),col=NA, alpha=0.3)+
        stat_density(r %>% filter(source=="Hybrid"), mapping = aes(x=Value, colour=country), geom="line", position = "identity",size=1, alpha=0.8) +
      
      facet_wrap(~Name, labeller = labeller(Name = label_wrap_gen(30)), scales="free_y")+ 
      labs(x="Range", y="Density")+  
      theme_minimal() + theme( axis.title = element_text(size=21),axis.text = element_text(size=17),strip.text.x = element_text(size=18), 
                               legend.position = "bottom",legend.key.height = unit(01.2, "cm"),  legend.key.width = unit(1.75,"cm"), 
                               legend.text =  element_text(size=16),legend.title = element_text(size=19), legend.box = "vertical")+
      guides(fill = guide_legend(order = 2, override.aes = list(shape=c(15,15,15)), title="Census 2011"))+ 
      guides(color = guide_legend(order = 1, override.aes = list(shape = c(16, 16, 16), linetype = c("solid", "solid", "solid")), title="Hybrid Census")) 
  
    ggsave(filename = paste0(getwd(),"/Plots/Range_density/",gsub(" ", "_", common_variables_without_denominators[common_variables_without_denominators$TS_code==ts,"TableName21"][1]), ".png"),
           gg_plot,dpi = 400, bg="white")#,  width=210, height=297,unit="mm") 
    
  }
  
  
  
  
  
  
  if(length(table_codes)>9 & length(table_codes)<18){
    
    for(half in 1:2){
      
      data_half =  r %>% filter(Code %in% split(table_codes, cut(seq_along(table_codes),2,labels = FALSE))[[half]])
      gg_plot = ggplot() + 
        geom_density(data_half %>% filter(source=="Census2011"), mapping=aes(x=Value,fill=country),col=NA, alpha=0.4)+
        stat_density(data_half %>% filter(source=="Hybrid"), mapping = aes(x=Value, colour=country), geom="line", position = "identity",size=1, alpha=0.8)+
        
        facet_wrap(~Name, labeller = labeller(Name = label_wrap_gen(30)), scales="free_y")+ 
        labs(x="Range", y="Density")+  
        theme_minimal() + theme( axis.title = element_text(size=21),axis.text = element_text(size=17),strip.text.x = element_text(size=18), 
                                 legend.position = "bottom",legend.key.height = unit(01.2, "cm"),  legend.key.width = unit(1.75,"cm"), 
                                 legend.text =  element_text(size=16),legend.title = element_text(size=19), legend.box = "vertical")+
        guides(fill = guide_legend(order = 2, override.aes = list(shape=c(15,15,15)), title="Census 2011"))+ 
        guides(color = guide_legend(order = 1, override.aes = list(shape = c(16, 16, 16), linetype = c("solid", "solid", "solid")), title="Hybrid Census")) 
      
      
      
     ggsave(filename = paste0(getwd(),"/Plots/Range_density/",gsub(" ", "_", common_variables_without_denominators[common_variables_without_denominators$TS_code==ts,"TableName21"][1]),"_part", half, ".png"),
            gg_plot,dpi = 400, bg="white")# ,  width=210, height=297,unit="mm") 
      
    }
  }
   
  if(length(table_codes)>17){
    
    for(third in 1:3){

      data_third =  r %>% filter(Code %in% split(table_codes, cut(seq_along(table_codes),3,labels = FALSE))[[third]])
      
      gg_plot = ggplot() + 
        geom_density(data_third %>% filter(source=="Census2011"), mapping=aes(x=Value,fill=country),col=NA, alpha=0.4)+
        stat_density(data_third %>% filter(source=="EW21_11"), mapping = aes(x=Value, colour=country), geom="line", position = "identity",size=1, alpha=0.8)+
        
        facet_wrap(~Name, labeller = labeller(Name = label_wrap_gen(30)), scales="free_y")+ 
        labs(x="Range", y="Density")+  
        theme_minimal() + theme( axis.title = element_text(size=21),axis.text = element_text(size=17),strip.text.x = element_text(size=18), 
                                 legend.position = "bottom",legend.key.height = unit(01.2, "cm"),  legend.key.width = unit(1.75,"cm"), 
                                 legend.text =  element_text(size=16),legend.title = element_text(size=19), legend.box = "vertical")+
        guides(fill = guide_legend(order = 2, override.aes = list(shape=c(15,15,15)), title="Census 2011"))+ 
        guides(color = guide_legend(order = 1, override.aes = list(shape = c(16, 16, 16), linetype = c("solid", "solid", "solid")), title="Hybrid Census")) 
      
      
      
       ggsave(filename = paste0(getwd(),"/Plots/Range_density/",gsub(" ", "_", common_variables_without_denominators[common_variables_without_denominators$TS_code==ts,"TableName21"][1]),"_part", third, ".png"),
             gg_plot,dpi = 400, bg="white")#,  width=210, height=297,unit="mm") 
      
    }
  }
  
  
  
  
  
  
}





for(ts in unique(common_variables_without_denominators$TS_code)[]){
  cat("\r", ts, " --- ", paste(match(ts, unique(common_variables_without_denominators$TS_code))), "out of", length(unique(common_variables_without_denominators$TS_code)))  
  flush.console()
  table_codes = (common_variables_without_denominators %>% filter(TS_code==ts))$Code
  
  
  Census_long21_Range =   Census_2021_common_var_prop_perc_IHS_range[,c("Geography_Code", table_codes)] %>% 
    tidyr::pivot_longer(cols = -c(Geography_Code), 
                        names_to = "Code", values_to = "Value")
  
  Census_long21_perc = Census_2021_common_var_prop_perc[,c("Geography_Code", table_codes)] %>% 
    tidyr::pivot_longer(cols = -c(Geography_Code), 
                        names_to = "Code", values_to = "Value") 
  
  Census_long21 = rbind(Census_long21_perc %>% mutate(Value=Value/100,type="Percentage"), Census_long21_Range %>% mutate(type="Standardized"))
  
  Census_long21 = merge(Census_long21 , common_variables %>% select(Code, Name),by="Code")
  
  
  distribution_plots_by_tables = ggplot(Census_long21, aes(x=Value, fill=type)) + 
    geom_histogram(binwidth =0.01, alpha=0.4, position = "identity") +
    facet_wrap(~Name,labeller = labeller(Name = label_wrap_gen(30))) + labs(x="Value", y="Count")+  
    theme_minimal() + theme( axis.title = element_text(size=19),axis.text = element_text(size=15),strip.text.x = element_text(size=13), 
                             legend.position = "bottom",legend.key.height = unit(1, "cm"),  legend.key.width = unit(1.5,"cm"), legend.text =  element_text(size=18)) +
    guides(fill=guide_legend(title=""))
  
}




for(table in unique(final_codes$TS_code)){
  
  cat("\r", table, " --- ", paste(match(table, unique(final_codes$TS_code))), "out of", length(unique(final_codes$TS_code)))
  flush.console()
  table_codes = (final_codes %>% filter(TS_code==table))$encoding
  
  if(length(table_codes)==9){
    no_col=3
  } else {
    no_col=2
  }
  
  
  
  Census_long21_Range =   Census_2021_IHS_Range[,c("Geography_Code", table_codes)] %>% 
    tidyr::pivot_longer(cols = -c(Geography_Code), 
                        names_to = "encoding", values_to = "Value")
  
  Census_long21_perc = Census_2021_perc[,c("Geography_Code", table_codes)] %>% 
    tidyr::pivot_longer(cols = -c(Geography_Code), 
                        names_to = "encoding", values_to = "Value") 
  
  
  Census_long21 = rbind(Census_long21_perc %>% mutate(Value=Value/100,type="Percentage"), Census_long21_Range %>% mutate(type="Standardized"))
  
  Census_long21 = merge(Census_long21 , final_codes %>% select(encoding, Name),by="encoding")
  
  if(length(table_codes)>10){
    
    for(half in 1:2){
      
      #  half_split_1 = Census_long21 %>% filter(encoding %in% split(table_codes, cut(seq_along(table_codes),2,labels = FALSE))[[1]])
      #  half_split_2 = split(table_codes, cut(seq_along(table_codes),2,labels = FALSE))[[2]]
      
      data_half =  Census_long21 %>% filter(encoding %in% split(table_codes, cut(seq_along(table_codes),2,labels = FALSE))[[half]])
      
      
      if(length(unique(data_half$encoding))==9){
        no_col=3
      } else {
        no_col=2
      }
      
      distribution_plots_by_tables = ggplot(data_half, aes(x=Value, fill=type)) + 
        geom_histogram(binwidth = 0.01, alpha=0.4, position = "identity") +
        #  geom_density(alpha=0.4)+ 
        facet_wrap(~Name,ncol = no_col,labeller = labeller(Name = label_wrap_gen(30))) + labs(x="Value", y="Count")+  
        theme_minimal() + theme( axis.title = element_text(size=19),axis.text = element_text(size=15),strip.text.x = element_text(size=13), 
                                 legend.position = "bottom",legend.key.height = unit(1, "cm"),  legend.key.width = unit(1.5,"cm"), legend.text =  element_text(size=18)) +
        guides(fill=guide_legend(title=""))
      
      
      ggsave(filename = paste0(getwd(),"/Plots/Statistical_distribution/",gsub(" ", "_", (final_codes %>% filter(TS_code==table))$TableName21)[1],"_part", half, ".png"),
             distribution_plots_by_tables,dpi = 600, bg="white",  width=210, height=297,unit="mm") 
    } 
    
  } else {
    
    if(table=="TS006"){
      bins = 10
    } else{
      bins=0.01
    }
    
    distribution_plots_by_tables = ggplot(Census_long21, aes(x=Value, fill=type)) + 
      geom_histogram(binwidth = bins, alpha=0.4, position = "identity") +
      #  geom_density(alpha=0.4)+ 
      facet_wrap(~Name,ncol = no_col,labeller = labeller(Name = label_wrap_gen(30))) + labs(x="Value", y="Count")+  
      theme_minimal() + theme( axis.title = element_text(size=19),axis.text = element_text(size=15),strip.text.x = element_text(size=13), 
                               legend.position = "bottom",legend.key.height = unit(1, "cm"),  legend.key.width = unit(1.5,"cm"), legend.text =  element_text(size=18)) +
      guides(fill=guide_legend(title=""))
    
    ggsave(filename = paste0(getwd(),"/Plots/Statistical_distribution/",gsub(" ", "_", (final_codes %>% filter(TS_code==table))$TableName21)[1], ".png"),
           distribution_plots_by_tables,dpi = 600, bg="white",  width=210, height=297,unit="mm") 
  }
  
  
  
  
}
