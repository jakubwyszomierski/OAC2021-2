setwd("~/Desktop/PhD/GIT/OAC2021")
library(nomisr)
library(dplyr)
library(sf)
library(tmap)
library(tidyverse)
library(readxl)

load("Data/Objects/mynomis_function.RData")
variable_reference_2011 = read.csv("Data/Lookups/Variable_reference_2011.csv")
variable_reference_2021 = read.csv("Data/Lookups/Variable_reference_2021.csv")
#codes_names = read.csv("Data/Codes_names.csv")
sa_lookup = read_excel("../../shapefiles/11DC_Lookup_1_0.xls", sheet=1)




Final_codes_11_21 = read.csv("Data/Lookups/Final_codes_11_21.csv") 
variable_reference_2011$order = 1:nrow(variable_reference_2011)
variable_reference_2021$order = 1:nrow(variable_reference_2021)

all_variable_codes = read.csv("Data/Lookups/All_variable_codes.csv")
# codes_names = rbind(Final_codes_11_21 %>% select(TableVariableCode11, VariableName11, TableCode11,TableName21, TS_code, CategoryCode11)%>% 
#                       rename(Code=TableVariableCode11, Name=VariableName11, TableCode=TableCode11), 
#                     Final_codes_11_21 %>% select(TableVariableCode21, VariableName21, TableCode21,TableName21,TS_code,CategoryCode11) %>% 
#                       rename(Code=TableVariableCode21,Name=VariableName21, TableCode=TableCode21)) %>% 
#   distinct(Code, .keep_all=T) 
# 
# codes_names = merge(codes_names, variable_reference_2011 %>% select(TableVariableCode11, order), 
#                     by.x="Code", by.y="TableVariableCode11", all.x=T)
# codes_names = merge(codes_names, variable_reference_2021 %>% select(TableVariableCode21, order), 
#                     by.x="Code", by.y="TableVariableCode21", all.x=T)
# 
# codes_names =   codes_names %>% mutate(order = ifelse(is.na(order.x), order.y, order.x)) %>% arrange(TS_code, order) %>% 
#   select(Code, Name, TableCode, TS_code, TableName21, CategoryCode11)
# 
# 
# codes_names[codes_names$TS_code=="TS001", "TableName21"] = "Residency type"
# codes_names[codes_names$TS_code=="TS007A", "TableName21"] = "Age structure"






# Census_2011 =  read.csv("Data/Clean/Census_2011_all.csv") # %>% mutate(NM_2037_1_7 = NM_2037_1_7 + NM_2037_1_8 + NM_2037_1_9) #%>% select(-NM_2037_1_8, - NM_2037_1_9)
# #Census_2011_oa_changed = read.csv("Data/Clean/OA_changed/Census_2011_oa_changed_raw_all.csv")
# Census_2021 = read.csv("Data/Clean/Census_2021_all.csv")#  %>% mutate(NM_2037_1_7 = NM_2037_1_7 + NM_2037_1_8 + NM_2037_1_9) # %>% select(-NM_2037_1_8, - NM_2037_1_9)
# #Census_2021_oa_changed = read.csv("Data/Clean/OA_changed/Census_2021_oa_changed_raw_all.csv")
# 
# 
# 
# Census_2011_common_var = subset(Census_2011, select=names(Census_2011) %in% c("Geography_Code",unique(codes_names$Code)))
# Census_2021_common_var = subset(Census_2021, select=names(Census_2021) %in% c("Geography_Code",unique(codes_names$Code)))
#Census_2011_oa_changed_common_var = subset(Census_2011_oa_changed, select=names(Census_2011_oa_changed) %in% c("Geography_Code",unique(codes_names$Code)))
#Census_2021_oa_changed_common_var = subset(Census_2021_oa_changed, select=names(Census_2021_oa_changed) %in% c("Geography_Code",unique(codes_names$Code)))

# Census_2011_common_var = read.csv("Census_2011_common_var.csv")
# Census_2011_common_var_prop = read.csv("Census_2011_common_var_prop.csv")
NI_2011_common_var_prop = merge(Census_2011_common_var_prop %>% filter(grepl("N", Geography_Code)), sa_lookup %>% select(-LGD2014NAME), by.x="Geography_Code", by.y="SA2011")
NI_2011_common_var = merge(Census_2011_common_var %>% filter(grepl("N", Geography_Code)), sa_lookup %>% select(-LGD2014NAME), by.x="Geography_Code", by.y="SA2011")










###############################################################
############        NI 2021 PROPORTIONS           #############
###############################################################

census_tables_all =  read.csv("Data/Lookups/Tables_metadata.csv")
head(census_tables_all)

### PHASE 1
census_tables_all[census_tables_all$TS_code=="TS001", "MS_code"] = "MS-A15"
census_tables_all[census_tables_all$TS_code=="TS004", "MS_code"] = "MS-A16"
census_tables_all[census_tables_all$TS_code=="TS006", "MS_code"] = "MS-A14"
census_tables_all[census_tables_all$TS_code=="TS007A", "MS_code"] ="MS-A02"
census_tables_all[census_tables_all$TS_code=="TS008", "MS_code"] = "MS-A07"
census_tables_all[census_tables_all$TS_code=="TS017", "MS_code"] = "MS-E02"
census_tables_all[census_tables_all$TS_code=="TS021", "MS_code"] = "MS-B01"
census_tables_all[census_tables_all$TS_code=="TS029", "MS_code"] = "MS-B14"
census_tables_all[census_tables_all$TS_code=="TS030", "MS_code"] = "MS-B19"
### PHASE 2
census_tables_all[census_tables_all$TS_code=="TS037", "MS_code"] = "MS-D01"
census_tables_all[census_tables_all$TS_code=="TS038", "MS_code"] = "MS-D02"
census_tables_all[census_tables_all$TS_code=="TS039", "MS_code"] = "MS-D17"
census_tables_all[census_tables_all$TS_code=="TS044", "MS_code"] = "MS-E06"
census_tables_all[census_tables_all$TS_code=="TS045", "MS_code"] = "MS-E10"
census_tables_all[census_tables_all$TS_code=="TS046", "MS_code"] = "MS-E11"
census_tables_all[census_tables_all$TS_code=="TS054", "MS_code"] = "MS-E15"

### PHASE 3
census_tables_all[census_tables_all$TS_code=="TS003", "MS_code"] = "MS-A25"
census_tables_all[census_tables_all$TS_code=="TS002", "MS_code"] = "MS-A30"
census_tables_all[census_tables_all$TS_code=="TS023", "MS_code"] = "MS-B04"
census_tables_all[census_tables_all$TS_code=="TS067", "MS_code"] = "MS-G01"
census_tables_all[census_tables_all$TS_code=="TS066", "MS_code"] = "MS-H02"
census_tables_all[census_tables_all$TS_code=="TS059", "MS_code"] = "MS-H05"
census_tables_all[census_tables_all$TS_code=="TS063", "MS_code"] = "MS-H09"
census_tables_all[census_tables_all$TS_code=="TS061", "MS_code"] = "MS-I01"
census_tables_all[census_tables_all$TS_code=="TS019", "MS_code"] = "MS-K01"



census_tables_EW_OA =census_tables_all%>% 
  filter(grepl("UK", Census11) | TableName21 %in% c("Religion", "Proficiency in English")) %>%
  filter(TableCode11 %nin% c("NM_1575_1", "NM_1533_1"))





############    READ IN EXCEL SPREADSHEETS

rm(NI_LGD_2021, NI_variables_2021)
for(i in tolower(unique(census_tables_all[complete.cases(census_tables_all),"MS_code"]))){
  
#  print(i)
        if(any(grepl(i, list.files("Data/NISRA_2021/Phase1")))){
          dat = suppressMessages(read_excel(paste0("Data/NISRA_2021/Phase1/census-2021-",i,".xlsx"),sheet=2))
          print(paste(i,"phase1"))
        } 
  
         if(any(grepl(i, list.files("Data/NISRA_2021/Phase2")))){
           dat = suppressMessages(read_excel(paste0("Data/NISRA_2021/Phase2/census-2021-",i,".xlsx"),sheet=2))
           print(paste(i,"phase2"))
         }
  
          if(toupper(i) %in% excel_sheets("Data/NISRA_2021/census-2021-main-statistics-for-northern-ireland-phase-3-draft-table-layouts.xlsx")){
            dat = suppressMessages(read_excel("Data/NISRA_2021/census-2021-main-statistics-for-northern-ireland-phase-3-draft-table-layouts.xlsx", sheet = toupper(i)))
            print(paste(i,"phase3"))
          }
  
   colnames(dat)[1] = "first"

   colname = as.character(dat[ which(grepl("Geography", dat$first))[1],])
   colname = gsub("[note 1]", "", colname, fixed =T)
   colname = gsub("[note 2]", "", colname, fixed =T)
   colname = gsub("\r\n", "", colname)
   colname = trimws(colname)
   colname = gsub("-", "_", colname)
   colname = gsub(" ", "_", colname)
  
   colnames(dat) = colname
   
   if(i %in% c("ms-d01", "ms-d02")){
 #   colname[grepl("Usual_residents", colname)]
     
    dat = dat[, c("Geography", "Geography_code", colname[grepl("All_usual_residents",colname)])]
     colname = intersect(colname, names(dat))
   }
   
   if(i== "ms-d17"){
     
     dat = dat[, c("Geography", "Geography_code", colname[grepl("All_usual_residents_aged_5_and_over",colname)])]
     colname = intersect(colname, names(dat))
   }
   
   if(i %in% c("ms-h05", "ms-h09", "ms-h02")){
    
     dat = dat[,c("Geography", "Geography_code", colname[grepl("usual_residents", colname, ignore.case = T)])]
     colname = intersect(colname, names(dat))
   }
   
   colname[-c(1:2)]
   
   dat_variables = data.frame(var_name = colname[-c(1:2)], MS_code = gsub("ms\\-", "", i))
   dat_variables$ms_VariableCode = paste0(gsub("ms\\-", "", i),"_", 0:(nrow(dat_variables)-1))
  # colnames(dat_variables) =# dat_variables$ms_VariableCode
   
   ind = which(grepl("Northern Ireland", dat$Geography))[1]
   dat_value = dat[(ind+1):(ind+11),]
   
   colnames(dat_value) = c("Geography", "Geography_code", dat_variables$ms_VariableCode)
    
            if(!exists("NI_LGD_2021")){
              NI_LGD_2021 = dat_value
              NI_variables_2021 = dat_variables
            } else{
              NI_LGD_2021 = merge(NI_LGD_2021, dat_value %>% select(-Geography), by="Geography_code", all=T, suffixes=c("", gsub("ms\\-", "", i)))
              NI_variables_2021 = rbind(NI_variables_2021, dat_variables)
            } 
          
}

head(NI_variables_2021)
NI_LGD_2021 = NI_LGD_2021 %>% mutate_at(vars(-c("Geography", "Geography_code")),as.numeric)
any(NI_LGD_2021==0)
NI_LGD_2021[is.na(NI_LGD_2021)] = 0



#####################################################################
#########       TRANSFORM VARIALBES FOR LGD STATISTICS     ##########
#####################################################################
NI_LGD_2021_long = NI_LGD_2021 %>%
  
  mutate(NM_2022_1_1001 = a30_2 + a30_3,
         NM_2023_1_5 = a25_5 + a25_6,
         NM_2023_1_8 = a25_9 + a25_10,
         NM_2023_1_10 = a25_12 + a25_13 + a25_15 + a25_16,
         NM_2023_1_11 = a25_14 + a25_17,
         NM_2023_1_13 = a25_19 + a25_20,
         NM_2023_1_14 =  a25_21 + a25_22 + a25_23,
         NM_2023_1_1001 = a25_1 + a25_2                                                        , 
         NM_2023_1_1002 =   a25_3 + a25_4 + a25_5 + a25_6 + a25_7 + a25_8 + a25_9 + a25_10+
           a25_11 + a25_12 + a25_13 + a25_14 + a25_15 + a25_16 + a25_17 + a25_18,
         NM_2023_1_1003  = a25_4 + a25_5 + a25_6 + a25_7                                       , 
         NM_2023_1_1004  =  a25_8 + a25_9 + a25_10 + a25_11                                    , 
         NM_2023_1_1005  = a25_12 + a25_13 + a25_14 + a25_15 + a25_16 + a25_17                 , 
         NM_2023_1_1007  = a25_19 + a25_20 + a25_21 + a25_22 + a25_23                          ,
         NM_2024_1_1001 =   a16_1 + a16_2 + a16_3 + a16_4  + a16_5 + a16_6 + a16_7,
         NM_2024_1_1 = a16_1 + a16_2 + a16_3 + a16_4 ,                             
         NM_2024_1_1002  = a16_5 + a16_6             ,                             
         NM_2024_1_9  = a16_10 + a16_11,
         NM_2020_1_18 = a02_18 + a02_19,
         NM_2039_1_3 = k01_2  + k01_3,
         NM_2041_1_3 = b01_2 + b01_3,
         NM_2041_1_14 = b01_6 + b01_9,
         NM_2041_1_1002 = b01_10 + b01_11,
         NM_2041_1_1005 = b01_8 + b01_13,
         NM_2049_1_2 = b19_1 +b19_2 + b19_3 + b19_4,
         NM_1508_1_5 = e10_4 + e10_5 + e10_6,
         NM_2072_1_7 =  e15_7 + e15_8 + e15_9 + e15_10,
         NM_2072_1_1001 =e15_1 + e15_2,
         NM_2072_1_1003 = e15_4 + e15_5,
         NM_2072_1_1004 =  e15_6 + e15_7 + e15_8 + e15_10,
         NM_1565_1_1 = h02_1 + h02_2 + h02_3 + h02_4 + h02_5 + h02_6 + h02_7 + h02_8,
         NM_1565_1_10 =  h02_9 + h02_10 + h02_11 + h02_12 + h02_13,
         NM_2057_1_102 = d17_3 + d17_3, 
         NM_2076_1_1001 = h05_1 + h05_2 ,
         NM_2076_1_1002 = h05_3  + h05_4,
         NM_2063_1_4 = e10_4 +  e10_5 + e10_6,
         NM_1558_1_6 = e11_4 + e11_6 + e11_7 + e11_8,
         NM_2048_1_2= b14_1 + b14_2
         #NM_2037_1_7 = NM_2037_1_7 + NM_2037_1_8 + NM_2037_1_9
         ) %>% 
  tidyr::pivot_longer(col =-c(Geography_code, Geography), names_to="ms_VariableCode", values_to="Value") %>% 
  mutate(ms_VariableCode=recode(ms_VariableCode,

##         "a15"
## ms_VariableCode                             var_name
'a15_0'  = 'NM_2021_1_0' ,#                   All_usual_residents
'a15_1'  = 'NM_2021_1_1' ,#               Resident_in_a_household
'a15_2'  = 'NM_2021_1_2' ,#  Resident_in_a_communal_establishment


##        "a30"
## ms_VariableCode                                                                      var_name
'a30_0'   =  'NM_2022_1_0'   ,#                                        All_usual_residents_aged_16_and_over
'a30_1'   =  'NM_2022_1_1'   ,#              Single_(never_married_or_never_registered_a_civil_partnership)
# a30_2   =   NM_1500_1_2                                                                        Married
# a30_3   =   NM_1500_1_3                                                         In_a_civil_partnership
'a30_4'   =   'NM_2022_1_1004' ,#  Separated_(but_still_legally_married_or_still_legally_in_a_civil_partnership)
'a30_5'   =   'NM_2022_1_1005' ,#     Divorced_or_formerly_in_a_civil_partnership_which_is_now_legally_dissolved
'a30_6'   =   'NM_2022_1_1006' ,#                          Widowed_or_surviving_partner_from_a_civil_partnership

# NM_2022_1_1001 = a30_2 + a30_3


##         "a25"
## ms_VariableCode                                                                                     var_name
'a25_0'    = 'NM_2023_1_0'      , #                                                        All_usual_residents_in_households
'a25_1'    = 'NM_2023_1_1'      , #                                                   One_person_household:_Aged_66_and_over
'a25_2'    = 'NM_2023_1_2'      , #                                                              One_person_household:_Other
'a25_3'    = 'NM_2023_1_3'      , #                                            Single_family_household:_All_aged_66_and_over
'a25_4'    = 'NM_2023_1_4'      , #                Single_family_household:_Married_or_civil_partnership_couple:_No_children
##  #  a25_5    = NM_2023_1_5   , #           Single_family_household:_Married_or_civil_partnership_couple:_One_dependent_child
##  #  a25_6    = NM_2023_1_5   , #Single_family_household:_Married_or_civil_partnership_couple:_Two_or_more_dependent_children
'a25_7'    = 'NM_2023_1_6'      , # Single_family_household:_Married_or_civil_partnership_couple:_All_children_non_dependent
'a25_8'    = 'NM_2023_1_7'   , #                              Single_family_household:_Cohabiting_couple_family:_No_children
##  #  a25_9    = NM_2023_1_8   , #                      Single_family_household:_Cohabiting_couple_family:_One_dependent_child
##  # a25_10    = NM_2023_1_8   , #           Single_family_household:_Cohabiting_couple_family:_Two_or_more_dependent_children
 'a25_11'    = 'NM_2023_1_9'    , #              Single_family_household:_Cohabiting_couple_family:_All_children_non_dependent
##  # a25_12    = NM_2023_1_10                      Single_family_household:_Lone_parent_family_(female):_One_dependent_child
##  # a25_13    = NM_2023_1_10           Single_family_household:_Lone_parent_family_(female):_Two_or_more_dependent_children
##  # a25_14    = NM_2023_1_11               Single_family_household:_Lone_parent_family_(female):_All_children_non_dependent
##  # a25_15    = NM_2023_1_10                        Single_family_household:_Lone_parent_family_(male):_One_dependent_child
##  # a25_16    = NM_2023_1_10             Single_family_household:_Lone_parent_family_(male):_Two_or_more_dependent_children
##  # a25_17    = NM_2023_1_11                 Single_family_household:_Lone_parent_family_(male):_All_children_non_dependent
##  # # a25_18    =                                                           Single_family_household:_Other_family_composition
##  # a25_19    = NM_2023_1_13                                                     Other_household_types:_One_dependent_child
##  # a25_20    = NM_2023_1_13                                          Other_household_types:_Two_or_more_dependent_children
##  # a25_21    = NM_2023_1_14                                              Other_household_types:_All_in_full_time_education
##  # a25_22    = NM_2023_1_14                                                    Other_household_types:_All_aged_66_and_over
##  # a25_23    = NM_2023_1_14                                                Other_household_types:_Other_family_composition


# NM_2023_1_5 = a25_5 + a25_6
# NM_2023_1_8 = a25_9 + a25_10
# NM_2023_1_10 = a25_12 + a25_13 + a25_15 + a25_16
# NM_2023_1_11 = a25_14 + a25_17
# NM_2023_1_13 =a25_19 + a25_20
# NM_2023_1_14 + a25_21 + a25_22 + a25_23

# NM_2023_1_1001 = a25_1 + a25_2                                                                                    One-person household
# NM_2023_1_1002 =   a25_3 + a25_4 + a25_5 + a25_6 + a25_7 + a25_8 + a25_9 + a25_10 + 
#                   a25_11 + a25_12 + a25_13 + a25_14 + a25_15 + a25_16 + a25_17 + a25_18         Single family household
# NM_2023_1_1003  = a25_4 + a25_5 + a25_6 + a25_7                                                             Single family household: Married or civil partnership couple
# NM_2023_1_1004  =  a25_8 + a25_9 + a25_10 + a25_11                                                          Single family household: Cohabiting couple family
# NM_2023_1_1005  = a25_12 + a25_13 + a25_14 + a25_15 + a25_16 + a25_17                                 Single family household: Lone parent family
# NM_2023_1_1007  = a25_19 + a25_20 + a25_21 + a25_22 + a25_23                                             Other household types




##      "a16"
## ms_VariableCode                                     var_name
 'a16_0' = 'NM_2024_1_0'       ,#                      All_usual_residents
 # a16_1  = NM_2024_1_1         ,#     Europe:_United_Kingdom:_Northern_Ireland
 # a16_2  = NM_2024_1_1         ,#        Europe:_United_Kingdom:_England
 # a16_3  = NM_2024_1_1         ,#       Europe:_United_Kingdom:_Scotland
 # a16_4  = NM_2024_1_1         ,#          Europe:_United_Kingdom:_Wales
 # a16_5  = NM_2024_1_1002      ,#               Europe:_Republic_of_Ireland
 # a16_6  = NM_2024_1_1002      ,#     Europe:_Other_EU_countries[notes_1,_2]
 'a16_7'  = 'NM_2024_1_1003'    ,#      Europe:_Other_non_EU_countries[note_3]
 'a16_8'  = 'NM_2024_1_7'       ,#                                   Africa
 'a16_9'  = 'NM_2024_1_8'       ,#                     Middle_East_and_Asia
# a16_10  = NM_2024_1_9         ,#        North_America,_Central_America_and_Caribbean
# a16_11  = NM_2024_1_9         ,#                          South_America
 'a16_12'  = 'NM_2024_1_10'     ,#              Antarctica,_Oceania_and_Other

# NM_2024_1_1001 =   a16_1 + a16_2 + a16_3 + a16_4  + a16_5 + a16_6 + a16_7           Europe
# NM_2024_1_1 = a16_1 + a16_2 + a16_3 + a16_4                                         Europe: United Kingdom
# NM_2024_1_1002  = a16_5 + a16_6                                                     Europe: EU countries
# NM_2024_1_9  = a16_10 + a16_11                                                      The Americas and the Caribbean





##         "a14"
## ms_VariableCode                                                   var_name
## a14_0                                              All_usual_residents
## a14_1                                                  Area_(hectares)
 'a14_2'   = 'NM_2026_1_0', #  Population_density_(number_of_usual_residents_per_hectare)

 
##        "a02"
## ms_VariableCode            var_name
  'a02_0'  = 'NM_2020_1_0'   , # All_usual_residents
  'a02_1'  = 'NM_2020_1_1'   , #           0_4_years
  'a02_2'  = 'NM_2020_1_2'   , #           5_9_years
  'a02_3'  = 'NM_2020_1_3'   , #         10_14_years
  'a02_4'  = 'NM_2020_1_4'   , #         15_19_years
  'a02_5'  = 'NM_2020_1_5'   , #         20_24_years
  'a02_6'  = 'NM_2020_1_6'   , #         25_29_years
  'a02_7'  = 'NM_2020_1_7'   , #         30_34_years
  'a02_8'  = 'NM_2020_1_8'   , #         35_39_years
  'a02_9'  = 'NM_2020_1_9'   , #         40_44_years
  'a02_10'  = 'NM_2020_1_10' , #           45_49_years
  'a02_11'  = 'NM_2020_1_11' , #           50_54_years
  'a02_12'  = 'NM_2020_1_12' , #           55_59_years
  'a02_13'  = 'NM_2020_1_13' , #           60_64_years
  'a02_14'  = 'NM_2020_1_14' , #           65_69_years
  'a02_15'  = 'NM_2020_1_15' , #           70_74_years
  'a02_16'  = 'NM_2020_1_16' , #           75_79_years
  'a02_17'  = 'NM_2020_1_17' , #           80_84_years
##  a02_18  = NM_2020_1_18             85_89_years
##  a02_19  = NM_2020_1_18               90+_years

# NM_2020_1_18 = a02_18 + a02_19

##       "a07"
## ms_VariableCode            var_name
 'a07_0'  = 'NM_2028_1_0'  ,#  All_usual_residents
 'a07_1'  = 'NM_2028_1_1'  ,#               Female
 'a07_2'  = 'NM_2028_1_2'  ,#                 Male


##       "e02"
## ms_VariableCode                                                          var_name
'e02_0'  = 'NM_2037_1_0'          ,#                                             All_households
'e02_1'  = 'NM_2037_1_2'          ,#                                      Households:_1_person
'e02_2'  = 'NM_2037_1_3'          ,#                                      Households:_2_people
'e02_3'  = 'NM_2037_1_4'          ,#                                      Households:_3_people
'e02_4'  = 'NM_2037_1_5'          ,#                                      Households:_4_people
'e02_5'  = 'NM_2037_1_6'          ,#                                      Households:_5_people
 'e02_6'  = 'NM_2037_1_7' ,# +  NM_2037_1_8 + NM_2037_1_9                  Households:_6_or_more_people
## e02_7                    All_usual_residents:_Resident_in_a_household
## e02_8                                          Average_household_size


##        "k01"
## ms_VariableCode                    ,#                                                                   var_name
'k01_0'    = 'NM_2039_1_0'         ,#           All_usual_residents_aged_1_or_more_in_the_area
'k01_1'    = 'NM_2039_1_1'         ,#           Lived_at_same_address_one_year_ago
## k01_2    =                      ,#          Lived_elsewhere_one_year_ago;_within_the_Local_Government_District
## k01_3    =                      ,#          Lived_elsewhere_one_year_ago_outside_the_Local_Government_District_but_within_Northern_Ireland
# 'k01_4'   = 'NM_2039_1_4'          ,#           Lived_elsewhere_one_year_ago_outside_Northern_Ireland

# NM_2039_1_3 = k01_2  + k01_3

##         "b01"
## ms_VariableCode            var_name
 'b01_0'  = 'NM_2041_1_0'          ,#       All_usual_residents
 'b01_1'  = 'NM_2041_1_1004'       ,#           White
# b01_2  = NM_2041_1_3             ,#        Irish_Traveller
# b01_3  = NM_2041_1_3             ,#        Roma
 'b01_4'  = 'NM_2041_1_10'         ,#           Indian
 'b01_5'  = 'NM_2041_1_13'         ,#           Chinese
 #b01_6  = NM_2041_1_14            ,#        Filipino
'b01_7'  = 'NM_2041_1_11'         ,#           Pakistani
 #b01_8  = NM_2041_1_1005          ,#        Arab
# b01_9  = NM_2041_1_14            ,#         Other_Asian
#b01_10  = NM_2041_1_1002          ,#         Black_African
#b01_11  = NM_2041_1_1002          ,#         Black_Other
'b01_12'  = 'NM_2041_1_1003'       ,#            Mixed
#b01_13  = NM_2041_1_1005          ,#         Other_ethnicities


# NM_2041_1_3 = b01_2 + b01_3
# NM_2041_1_14 = b01_6 + b01_9
# NM_2041_1_1002 = b01_10 + b01_11
#NM_2041_1_1005 = b01_8 + b01_13


##         "b04"
## ms_VariableCode                                                                                  var_name
'b04_0'   = 'NM_2042_1_0'    , #                                                                             All_households
'b04_1'   = 'NM_2042_1_1'    , #                                                                       One_person_household
'b04_2'   = 'NM_2042_1_2'    , #      More_than_one_person_in_a_household:_All_household_members_have_the_same_ethnic_group
'b04_3'   = 'NM_2042_1_3'    , #  More_than_one_person_in_a_household:_Different_ethnic_groups_between_the_generations_only
'b04_4'   = 'NM_2042_1_4'    , #           More_than_one_person_in_a_household:_Different_ethnic_groups_within_partnerships
'b04_5'   = 'NM_2042_1_5'    , #       More_than_one_person_in_a_household:_Any_other_combination_of_multiple_ethnic_groups
##     

##        "b14"
## ms_VariableCode                                                  var_name
'b14_0'   = 'NM_2048_1_0'    , #                         All_usual_residents_aged_3_and_over
# 'b14_1'   = 'NM_2048_1_1'    , #                                    Main_language_is_English
# 'b14_2'   = 'NM_2048_1_2'    , #   Main_language_is_not_English:_Can_speak_English_very_well
'b14_3'   = 'NM_2048_1_3'    , #        Main_language_is_not_English:_Can_speak_English_well
'b14_4'   = 'NM_2048_1_4'    , #     Main_language_is_not_English:_Cannot_speak_English_well
'b14_5'   = 'NM_2048_1_5'    , #          Main_language_is_not_English:_Cannot_speak_English

# NM_2048_1_2= b14_1 + b14_2
##        "b19"
## ms_VariableCode                                      var_name
'b19_0'   = 'NM_2049_1_0'  , #                             All_usual_residents
# b19_1   = NM_2049_1_2    , #                                      Catholic
# b19_2   = NM_2049_1_2    , #                Presbyterian_Church_in_Ireland
# b19_3   = NM_2049_1_2    , #                             Church_of_Ireland
# b19_4   = NM_2049_1_2    , #                   Methodist_Church_in_Ireland
# b19_5   = NM_2049_1_2    , # Other_Christian_(including_Christian_related)
'b19_6'   = 'NM_2049_1_8'  , #                                Other_religions
'b19_7'   = 'NM_2049_1_1'  , #                                    No_religion
'b19_8'   = 'NM_2049_1_9'  , #                            Religion_not_stated

# NM_2049_1_2 = b19_1 +b19_2 + b19_3 + b19_4

##         "d01"
## ms_VariableCode                              var_name
'd01_0'   = 'NM_2055_1_0'  , #                        All_usual_residents
'd01_1'   = 'NM_2055_1_1'  , #      All_usual_residents:_Very_good_health
'd01_2'   = 'NM_2055_1_2'  , #           All_usual_residents:_Good_health
'd01_3'   = 'NM_2055_1_3'  , #           All_usual_residents:_Fair_health
'd01_4'   = 'NM_2055_1_4'  , #            All_usual_residents:_Bad_health
'd01_5'   = 'NM_2055_1_5'  , #       All_usual_residents:_Very_bad_health


##        "d02"
## ms_VariableCode                                                   var_name
 'd02_0'   = 'NM_2056_1_0'   ,#                                         All_usual_residents
 'd02_1'   = 'NM_2056_1_1'   ,#     All_usual_residents:Day_to_day_activities_limited_a_lot
 'd02_2'   = 'NM_2056_1_2'   ,#  All_usual_residents:Day_to_day_activities_limited_a_little
 'd02_3'   = 'NM_2056_1_1002'   ,#       All_usual_residents:Day_to_day_activities_not_limited




##         "d17"
## ms_VariableCode                                                                      var_name
'd17_0'   ='NM_2057_1_0'  , #                                            All_usual_residents_aged_5_and_over
'd17_1'   ='NM_2057_1_1'  , #                    All_usual_residents_aged_5_and_over:Provides_no_unpaid_care
'd17_2'   ='NM_2057_1_101'  , #   All_usual_residents_aged_5_and_over:Provides_1_19_hours_unpaid_care_per_week
#'d17_3'   ='NM_2057_1_3'  , #  All_usual_residents_aged_5_and_over:Provides_20_34_hours_unpaid_care_per_week
# 'd17_4'   ='NM_2057_1_4'  , #  All_usual_residents_aged_5_and_over:Provides_35_49_hours_unpaid_care_per_week
'd17_5'   ='NM_2057_1_6'  , #    All_usual_residents_aged_5_and_over:Provides_50+_hours_unpaid_care_per_week

# NM_2057_1_102 = d17_3 + d17_3


##       "e06"
## ms_VariableCode                                                                                var_name
'e06_0'   = 'NM_1549_1_0'  , #                                                                           All_households
'e06_1'   = 'NM_1549_1_3'  , #                                                        Whole_house_or_bungalow:_Detached
'e06_2'   = 'NM_1549_1_4'  , #                                                   Whole_house_or_bungalow:_Semi_detached
'e06_3'   = 'NM_1549_1_5'  , #                                Whole_house_or_bungalow:_Terraced_(including_end_terrace)
'e06_4'   = 'NM_1549_1_7'  , #                              Flat,_maisonette_or_apartment:_Purpose_built_block_of_flats
'e06_5'   = 'NM_1549_1_8'  , #  Flat,_maisonette_or_apartment:_Part_of_a_converted_or_shared_house_(including_bed_sits)
'e06_6'   = 'NM_1549_1_9'  , #                                  Flat,_maisonette_or_apartment:_In_a_commercial_building
'e06_7'   = 'NM_1549_1_10' , #                                            Caravan_or_other_mobile_or_temporary_structure


##       "e10"
## ms_VariableCode                                               var_name
'e10_0'   =  'NM_2063_1_0' , #                                            All_households
'e10_1'   =  'NM_2063_1_1' , #                                 No_cars_or_vans_available
'e10_2'   =  'NM_2063_1_2' , #                                    1_car_or_van_available
'e10_3'   =  'NM_2063_1_3' , #                                  2_cars_or_vans_available
## 'e10_4'   =  'NM_2063_1_4' , #                                  3_cars_or_vans_available
##  e10_5   =  NM_1508_1_5                                   4_cars_or_vans_available
##  e10_6   =  NM_1508_1_5                           5_or_more_cars_or_vans_available
##  e10_7            Total_number_of_cars_or_vans_available_in_the_area
##  e10_8        Average_number_of_cars_or_vans_available_per_household

# NM_2063_1_4 = e10_4 +  e10_5 + e10_6

##          "e11"
## ms_VariableCode                                    var_name
'e11_0'   = 'NM_1558_1_0'  , #                                All_households
'e11_1'   = 'NM_1558_1_4'  , #                                      Oil_only
'e11_2'   = 'NM_1558_1_2'  , #                                Mains_gas_only
'e11_3'   = 'NM_1558_1_3'  , #   Electric_(for_example_storage_heaters)_only
##  e11_4   =              , #           Tank_or_bottled_gas_only
'e11_5'   ='NM_1558_1_5'   , # Solid_fuel_(for_example_coal)_only
# 'e11_6'   = 'NM_2064_1_8' , #                  Renewable_heating_system_only
# 'e11_7'   = 'NM_2064_1_6' , #     Wood_(for_example_logs_or_waste_wood)_only
##  e11_8   =    NM_1558_1_6          , #         Other_central_heating_only
'e11_9'   = 'NM_1558_1_7' , #           Two_or_more_types_of_central_heating
'e11_10'   = 'NM_1558_1_1' , #                             No_central_heating


# NM_1558_1_6 = e11_4 + e11_6 + e11_7 + e11_8


##        "e15"
## ms_VariableCode               , #                                  var_name
'e15_0'   = 'NM_2072_1_0'     , #                                            All_households
'e15_1'   = 'NM_2072_1_1'     , #                             Owner_occupied:_Owns_outright
'e15_2'   = 'NM_2072_1_2'     , #              Owner_occupied:_Owns_with_a_mortgage_or_loan
'e15_3'   = 'NM_2072_1_1002'  , #                   Shared_ownership:_Part_owns_and_part_rents
'e15_4'   = 'NM_2072_1_4'     , #         Social_rented:_Northern_Ireland_Housing_Executive
'e15_5'   = 'NM_2072_1_5'     , #    Social_rented:_Housing_association_or_charitable_trust
'e15_6'   = 'NM_2072_1_6'     , #                          Private_rented:_Private_landlord
## e15_7   = NM_2072_1_7      , #                           Private_rented:_Letting_agency
## e15_8   = NM_2072_1_7      , #           Private_rented:_Employer_of_a_household_member
## e15_9   = NM_2072_1_7      , # Private_rented:_Relative_or_friend_of_a_household_member
## e15_10   = NM_2072_1_7     , #                                     Private_rented:_Other
'e15_11'   = 'NM_2072_1_8'    , #                                            Lives_rent_free


# NM_2072_1_7 =  e15_7 + e15_8 + e15_9 + e15_10
# NM_2072_1_1001 =e15_1 + e15_2
# NM_2072_1_1003 = e15_4 + e15_5
# NM_2072_1_1004 =  e15_6 + e15_7 + e15_8 + e15_9 + e15_10

##        "h05"
## ms_VariableCode                                                    var_name
'h05_0'   = 'NM_2076_1_0'   ,#               All_usual_residents_aged_16_and_over_in_employment
'h05_1'   = 'NM_2076_1_1'   ,#      Usual_residents:Part_time:_15_hours_or_less_worked_per_week
'h05_2'   = 'NM_2076_1_2'   ,#          Usual_residents:_Part_time:_16_30_hours_worked_per_week
'h05_3'   = 'NM_2076_1_3'   ,#           Usual_residents:Full_time:_31_48_hours_worked_per_week
'h05_4'   = 'NM_2076_1_4'   ,#            Usual_residents:_Full_time:_49+_hours_worked_per_week

# NM_2076_1_1001 = h05_1 + h05_2     Part-time
# NM_2076_1_1002 = h05_3  + h05_4    Full-time
##          "i01"
## ms_VariableCode                                                                          var_name
##  i01_0        All_usual_residents_aged_16_and_over_(excluding_full_time_students)_in_employment
##  i01_1                                                              Work_mainly_at_or_from_home
##  i01_2                                                                     Driving_a_car_or_van
##  i01_3                                                                Passenger_in_a_car_or_van
##  i01_4                                                         Car_or_van_pool,_sharing_driving
##  i01_5                                                                    Bus,_minibus_or_coach
##  i01_6                                                                                     Taxi
##  i01_7                                                                                    Train
##  i01_8                                                             Motorcycle,_scooter_or_moped
##  i01_9                                                                                  Bicycle
## i01_10                                                                                  On_foot
## i01_11                                                                             Other_method


##        "h09"
## ms_VariableC'ode        '  ,#                                                   var_name
'h09_0'   = 'NM_2080_1_0'  ,#                       All_usual_residents_aged_16_and_over_in_employment
'h09_1'   = 'NM_2080_1_1'  ,#             Usual_residents:_1._Managers,_directors_and_senior_officials
'h09_2'   = 'NM_2080_1_2'  ,#                             Usual_residents:_2._Professional_occupations
'h09_3'   = 'NM_2080_1_3'  ,#     Usual_residents:_3._Associate_professional_and_technical_occupations
'h09_4'   = 'NM_2080_1_4'  ,#           Usual_residents:_4._Administrative_and_secretarial_occupations
'h09_5'   = 'NM_2080_1_5'  ,#                           Usual_residents:_5._Skilled_trades_occupations
'h09_6'   = 'NM_2080_1_6'  ,#        Usual_residents:_6._Caring,_leisure_and_other_service_occupations
'h09_7'   = 'NM_2080_1_7'  ,#               Usual_residents:_7._Sales_and_customer_service_occupations
'h09_8'   = 'NM_2080_1_8'  ,#                Usual_residents:_8._Process,_plant_and_machine_operatives
'h09_9'   = 'NM_2080_1_9'  ,#                               Usual_residents:_9._Elementary_occupations


##      "h02"
 ## ms_VariableCode                                                                                          var_name
 'h02_0'   ='NM_1565_1_0'      , #                                                              All_usual_residents_aged_16_and_over
 'h02_1'   ='NM_1565_1_2'      , #                        Usual_residents_aged_16_and_over:_Economically_active:_Employee:_Part_time
 'h02_2'   ='NM_1565_1_3'      , #                        Usual_residents_aged_16_and_over:_Economically_active:_Employee:_Full_time
 'h02_3'   ='NM_1565_1_4'      , #    Usual_residents_aged_16_and_over:_Economically_active:_Self_employed_with_employees:_Part_time
 'h02_4'   ='NM_1565_1_5'      , #    Usual_residents_aged_16_and_over:_Economically_active:_Self_employed_with_employees:_Full_time
 'h02_5'   ='NM_1565_1_6'      , # Usual_residents_aged_16_and_over:_Economically_active:_Self_employed_without_employees:_Part_time
 'h02_6'   ='NM_1565_1_7'      , # Usual_residents_aged_16_and_over:_Economically_active:_Self_employed_without_employees:_Full_time
 'h02_7'   ='NM_1565_1_8'      , #                                 Usual_residents_aged_16_and_over:_Economically_active:_Unemployed
 'h02_8'   ='NM_1565_1_9'      , #                          Usual_residents_aged_16_and_over:_Economically_active:_Full_time_student
 'h02_9'   = 'NM_1565_1_11'     , #                                   Usual_residents_aged_16_and_over:_Economically_inactive:_Retired
'h02_10'   = 'NM_1565_1_12'     , #    Usual_residents_aged_16_and_over:_Economically_inactive:_Student_(including_full_time_students)
'h02_11'   = 'NM_1565_1_13'     , #              Usual_residents_aged_16_and_over:_Economically_inactive:_Looking_after_home_or_family
'h02_12'   = 'NM_1565_1_14'     , #                Usual_residents_aged_16_and_over:_Economically_inactive:_Long_term_sick_or_disabled
'h02_13'   = 'NM_1565_1_15'     , #                                     Usual_residents_aged_16_and_over:_Economically_inactive:_Other

 
## NM_1565_1_1 = h02_1 + h02_2 + h02_3 + h02_4 + h02_5 + h02_6 + h02_7 + h02_8      Economically active: Total
## NM_1565_1_10 =  h02_9 + h02_10 + h02_11 + h02_12 + h02_13               Economically inactive: Total
 
##       "g01"
# ms_VariableCode                             var_name
'g01_0'    = 'NM_2084_1_0'        ,#    All_usual_residents_aged_16_and_over
'g01_1'    = 'NM_2084_1_1'        ,#                       No_qualifications
'g01_2'    = 'NM_2084_1_2'        ,#                  Level_1_qualifications
'g01_3'    = 'NM_2084_1_3'        ,#                  Level_2_qualifications
'g01_4'    = 'NM_2084_1_4'        ,#                          Apprenticeship
'g01_5'    = 'NM_2084_1_5'        ,#                  Level_3_qualifications
'g01_6'    = 'NM_2084_1_6'        ,#        Level_4_qualifications_and_above
'g01_7'    = 'NM_2084_1_7'        ,#                    Other_qualifications
)) 




####### CHANGE VARIABLES FOR OAC2021
NI_LGD_2021_wide = tidyr::spread(NI_LGD_2021_long %>% select(Geography, Geography_code, ms_VariableCode, Value), 
                               key = ms_VariableCode, value = Value) %>% 
  mutate(
    age_5_19 = NM_2020_1_2 + NM_2020_1_3 + NM_2020_1_4,
    age_20_29 = NM_2020_1_5 + NM_2020_1_6,
    age_30_39 = NM_2020_1_7 + NM_2020_1_8,
    age_40_54 = NM_2020_1_9 + NM_2020_1_10 + NM_2020_1_11,
    age_55_69 = NM_2020_1_12 + NM_2020_1_13 + NM_2020_1_14,
    age_70_84 = NM_2020_1_15 + NM_2020_1_16 + NM_2020_1_17,
    separated_divorced = NM_2022_1_1004 + NM_2022_1_1005,
    no_children = NM_2023_1_4 + NM_2023_1_7,
    dependent_children =  NM_2023_1_5 +  NM_2023_1_8  + NM_2023_1_10 ,#+ NM_2023_1_13 ,
    nondependent_children = NM_2023_1_6  + NM_2023_1_9 + NM_2023_1_11 ,
    multi_ethnic_household =  NM_2042_1_3 + NM_2042_1_4 + NM_2042_1_5,
    cannot_speak_English =  NM_2048_1_4 + NM_2048_1_5,
    provides_unpaid_care = NM_2057_1_101 + NM_2057_1_102 + NM_2057_1_6  ,
    flat = NM_1549_1_7 + NM_1549_1_8 + NM_1549_1_9,
    cars_2_or_more = NM_2063_1_3 +  NM_2063_1_4,
    under_occupation =0,  #NM_2071_1_1 + NM_2071_1_2,
    overcrowding =  0, #NM_2071_1_4 +  NM_2071_1_5 ,
    ownership_or_shared = NM_2072_1_1001 + NM_2072_1_1002,
    under_2_level = NM_2084_1_1 + NM_2084_1_2 + NM_2084_1_3,
    level_3_or_4 =  NM_2084_1_5 + NM_2084_1_6,
    level_1_2_and_appr = NM_2084_1_2 + NM_2084_1_3+NM_2084_1_4,
  )


#############     CHANGES
####     Household size       => 6 people or over in a household
####     Migrant Indicator    => 2021 NI informs on people who moved out from NI, while 2011 informs on people who moved out from the UK. 
####     Ethnic group         => 2021 does not inform on the numbers of Pakistatni, so this will be borrowed from 2011


NI_LGD_2021_common_var = subset(NI_LGD_2021_wide, select = names(NI_LGD_2021_wide) %in% c("Geography_code", names(Census_2011_common_var)))
available_NI_LGD_2021_variables = names(which((colSums(NI_LGD_2021_common_var[,-1])!=0)))

### LIST OF VARIABLES THAT ARE UNAVAILABLE IN 2021 LOCAL DISTRICT LEVEL (OR WILL BE), BUT ARE AT SA 2011
all_variable_codes %>% filter(Code %in% setdiff(colnames(Census_2011_common_var), colnames(NI_LGD_2021_common_var)))


#common_variables = read.csv("common_variables.csv")
####      TO BE BORROWED FROM 2011 SA !!!!!   
common_variables %>% filter(Code %nin% available_NI_LGD_2021_variables) %>%select(-CategoryCode11)
common_variables %>% filter(Code %in% available_NI_LGD_2021_variables) %>%select(-CategoryCode11)


head(NI_LGD_2021_common_var[,c("Geography_code",available_NI_LGD_2021_variables)])







####################################################################################
#######       CALCULATE CHANGES BETWEEN 2011 and 2021 AT THE LGD LEVEL      ########
####################################################################################

#
#common_variables = read.csv("common_variables.csv")

### get a list of denominators (excluding Density)
denominators = common_variables %>% filter(CategoryCode11==0 | TS_code=="TS006")

### list of common variables, without denominators
common_variables_without_denominators = common_variables %>% filter(CategoryCode11!=0 | is.na(CategoryCode11))




########     STATISTICS AT THE LGD LEVEL FOR 2011 Northern Ireland
NI_LGD_2011_common_var = NI_2011_common_var %>% group_by(LGD2014) %>% 
  summarise(across(where(is.numeric) & !NM_2026_1_0, sum)) 



LGD_change = data.frame(LGD = NI_LGD_2021_common_var$Geography_code) %>% arrange(LGD)

for(deno in unique(denominators$Code)){
  denominator_TS_code = denominators[denominators$Code==deno, "TS_code"]
  nominators = (common_variables %>% filter(TS_code==denominator_TS_code, Code!=deno))$Code 
  nominators = intersect(nominators,available_NI_LGD_2021_variables)
  
  
  if(!identical(nominators, character(0))){
    
    if(deno =="NM_2026_1_0"){
      next
    }
    
    LGD_2011 = NI_LGD_2011_common_var %>% select(LGD2014, deno,nominators) %>% arrange(LGD2014)
    LGD_2021 = NI_LGD_2021_common_var %>% select(Geography_code, deno, nominators) %>% arrange(Geography_code)
    
    LGD_change[, paste0(deno, "_change")] = round(((LGD_2021[,deno] - LGD_2011[, deno])/ LGD_2011[, deno]), 4)
    
    for(n in nominators){
      
      
      prop_2011= LGD_2011[,n] / LGD_2011[,2]
      prop_2021= LGD_2021[,n] / LGD_2021[,2]
      
      LGD_change[,paste0(n, "_prop_change")] = round(((prop_2021 - prop_2011) / prop_2011),4)
      
      
     # for(lgd in unique(LGD_change$Geography_code)){
     #   LGD_change[LGD_change$Geography_code==lgd, paste0(n, "_prop_change")]
     #   test %>% filter(LGD2014==lgd)
     # }
      
    }
    
    #for(i in 1:nrow(LGD_change)){
    #  LGD_change[i,paste0(n,"_change" )] = round(((LGD_2021[i,n] - LGD_2011[i,n]) / LGD_2011[i,n]),4)
    #}
  }
}




available_NI_LGD_2021_variables



#NI_2011_common_var_prop_modelling = subset(NI_2011_common_var_prop, select=names(NI_2011_common_var_prop) %in% c("Geography_Code","LGD2014", unique(available_NI_LGD_2021_variables)))
#NI_2011_common_var_prop_modelling = subset(NI_2011_common_var_prop, select=c(Geography_Code,LGD2014,NM_2021_1_1:ownership_or_shared))


NI_2011_common_var_prop_modelling = subset(NI_2011_common_var_prop, select=names(NI_2011_common_var_prop) %in% c("Geography_Code", "LGD2014", available_NI_LGD_2021_variables))

NI_2021_modelled = NULL
for(lgd in unique(LGD_change$LGD)){
 print(lgd)
  
rm(NI_2011_modelled_lgd)
          for(var in colnames(NI_2011_common_var_prop_modelling %>% select(-Geography_Code, -LGD2014))){
            
            cat("\r",var,"||| ", (paste(match(var, colnames(NI_2011_common_var_prop_modelling)))),
                "out of", length(colnames(NI_2011_common_var_prop_modelling))," --- ", lgd)
            flush.console()
            
                     if(var=="NM_2026_1_0"){
                       next
                     }
            
             tab = NI_2011_common_var_prop_modelling %>% filter(LGD2014==lgd) %>% select(Geography_Code, var)
             LGD_change_var_lgd = LGD_change[LGD_change$LGD==lgd, paste0(var, "_prop_change")]
             
             tab$modelled  = round(tab[,var] * (1 + LGD_change_var_lgd),4)
             tab = tab %>% mutate(modelled = ifelse(modelled>1, 1, modelled))
             
             #colnames(tab)[3] = paste0("modelled_", var)
             colnames(tab)[3] = paste0(var)
             tab = tab %>% select(-var) 
             
                    if(!exists("NI_2011_modelled_lgd")){
                      NI_2011_modelled_lgd = tab 
                         } else {
                           
                      NI_2011_modelled_lgd = merge(NI_2011_modelled_lgd, tab, by="Geography_Code", all=T)
                         }
             
            }
  
  NI_2021_modelled = rbind(NI_2021_modelled, NI_2011_modelled_lgd)
  
  
}


#summary(NI_2021_modelled)

#### number of rows for which the proportions are greater than 1      # 2378
nrow(NI_2021_modelled %>% filter_at(vars(-Geography_Code), any_vars(. > 1))) 

#### number of rows for which the proportions are greater than 1, besides those living in residential dwelling      ### 286
nrow(NI_2021_modelled %>% filter_at(vars(-Geography_Code,-NM_2021_1_1), any_vars(. > 1)))

##### WILL DO THIS IN A LOOP ABOVE




# ####    RETRIEVE MODELLED COUNTS FOR DISABILITY AND AGE TO CALCULATE SIR
# 
# (common_variables %>% filter(TableCode %in% c("NM_2020_1", "NM_2056_1"), CategoryCode11!=0))$Code
# 
# NI_2021_modelled_for_SIR = subset(NI_2021_modelled, select = names(NI_2021_modelled) %in% c("Geography_Code", ((common_variables %>% filter(TableCode %in% c("NM_2020_1", "NM_2056_1"), CategoryCode11!=0))$Code)))
# head(NI_2021_modelled_for_SIR)
# 
# 
# NI_2021_modelled_for_SIR = merge(NI_2011_common_var %>% select(Geography_Code,LGD2014, NM_2020_1_0), NI_2021_modelled_for_SIR, by="Geography_Code", all=T)
# 
# 
# 
# for(i in 1:nrow(NI_2021_modelled_for_SIR)){
#   lgd = NI_2021_modelled_for_SIR[i, "LGD2014"]
# 
#   NI_2021_modelled_for_SIR[i,"NM_2020_1_0"] = round(NI_2021_modelled_for_SIR[i, "NM_2020_1_0"] * (1 + LGD_change[LGD_change$LGD==lgd,"NM_2020_1_0_change"]))
# 
# }
# 
# 
# for(c in colnames(NI_2021_modelled_for_SIR)[-c(1:2, 25)]){
#   NI_2021_modelled_for_SIR[,c] = round(NI_2021_modelled_for_SIR[,c] * NI_2021_modelled_for_SIR[,"Population_size"])
# }
# 
# 
# 
# 





intersect(available_NI_LGD_2021_variables, denominators$Code)

NI_2021_modelled_m = merge(NI_2011_common_var %>% select(Geography_Code,LGD2014,intersect(available_NI_LGD_2021_variables, denominators$Code) ), NI_2021_modelled, by="Geography_Code", all=T)


  
for(i in 1:nrow(NI_2021_modelled_m)){
  lgd = NI_2021_modelled_m[i, "LGD2014"]
  
  for(deno in intersect(available_NI_LGD_2021_variables, denominators$Code)){
    if(deno == "NM_2026_1_0"){
      next
    }
    NI_2021_modelled_m[i,deno] = round(NI_2021_modelled_m[i, deno] * (1 + LGD_change[LGD_change$LGD==lgd,paste0(deno,"_change")]))
    

  }
  cat("\r", i, "out of", nrow(NI_2021_modelled_m))
flush.console()
  }


head(NI_2021_modelled_m)


for(deno in denominators$Code){
  denominator_TS_code = denominators[denominators$Code==deno, "TS_code"]
  nominators = (common_variables %>% filter(TS_code==denominator_TS_code, Code!=deno))$Code 
  nominators = intersect(nominators,colnames(NI_2021_modelled_m))
  
  
  for(c in nominators){
    NI_2021_modelled_m[,c]  = round(NI_2021_modelled_m[,c] * NI_2021_modelled_m[,deno])  
  }
}


(NI_2021_modelled_m[,-c(1:2)] - NI_2011_common_var[, colnames(NI_2021_modelled_m)][,-c(1:2)])

NI_2021_modelled_m = NI_2021_modelled_m %>% select(-LGD2014)

setdiff(colnames(NI_2011_common_var),colnames(NI_2021_modelled_m))
NI_2021_borrowed = subset(NI_2011_common_var  , select=names(NI_2011_common_var) %in% c("Geography_Code",setdiff(colnames(NI_2011_common_var),colnames(NI_2021_modelled_m)))) %>% 
  select(-LGD2014)

Final_NI_2021 = merge(NI_2021_modelled_m,NI_2021_borrowed,  by="Geography_Code", all=T)
head(Final_NI_2021)


 
Final_NI_2021 = Final_NI_2021[,match(colnames(NI_2011_common_var %>% select(-LGD2014)), colnames(Final_NI_2021))]
 