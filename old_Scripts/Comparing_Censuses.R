

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
OA_changes = read.csv("../../shapefiles/OA_2011_to_OA_2021.csv")

#### analyse changed OAs
### U - No change (175,466)
### S - Split (2011 OA splint into two or more 2021 OAs) (12,661)
### M - Merge (2011 OAs merged into one OA_2021) (1040)
### X - irregular or fragmented relationship (redesignment of OAs) (397)




Final_codes_11_21 = read.csv("Data/Lookups/Final_codes_11_21.csv") 


variable_reference_2011 = read.csv("Data/Lookups/Variable_reference_2011.csv")
variable_reference_2021 = read.csv("Data/Lookups/Variable_reference_2021.csv")

## this will help keeping the original order of the variables
variable_reference_2011$order = 1:nrow(variable_reference_2011)
variable_reference_2021$order = 1:nrow(variable_reference_2021)


census_tables_EW_OA = read.csv("Data/Lookups/Tables_metadata.csv") %>% 
  filter(grepl("UK", Census11) | TableName21 %in% c("Religion", "Proficiency in English")) %>%
  filter(TableCode11 %nin% c("NM_1575_1", "NM_1533_1"))







###     NM_1500_1   NM_2022_1   Legal partnership status  
###     NM_1504_1   NM_2041_1   Ethnic group              
###     NM_1508_1   NM_2063_1   Car or van availability   
###     NM_1531_1   NM_2020_1   Age by five-year age bands
###     NM_1538_1   NM_2023_1   Household composition     
###     NM_1552_1   NM_2072_1   Tenure                    
###     NM_1565_1   NM_2083_1   Economic activity status  


###     NM_1504_1   NM_2041_1   Ethnic group            
###     NM_1549_1   NM_2062_1   Accommodation type      
###     NM_1558_1   NM_2064_1   Central heating         
###     NM_1565_1   NM_2083_1   Economic activity status










#############################################################
#################       Census 2011         #################
#############################################################

rm(Census_2011,Census_2011_oa_changed)

##### AGGREGATE 2011 CENSUS
for(nomis_file_11 in unique(census_tables_EW_OA$TableCode11)){

  table_wide11 = read.csv(paste0("Data/API/Census2011/TYPE299/Value/", nomis_file_11, ".csv"))
  
           if(nomis_file_11 %nin% c("NM_526_1", "NM_616_1")){
             ni_table_wide11 = read.csv(paste0("Data/API/Census2011/TYPE258/Value/", nomis_file_11, ".csv"))
             
                    if(any(colnames(table_wide11)!=colnames(ni_table_wide11))){
                      break
                    } else{
                      table_wide11 = rbind(table_wide11, ni_table_wide11)
                    }
           }
  
  
  print( paste(nomis_file_11,"||| ", (paste(match(nomis_file_11, unique(census_tables_EW_OA$TableCode11)))),
               "out of", length(unique(census_tables_EW_OA$TableCode11)), " --- number of rows:", nrow(table_wide11)))
  
  
  
  if(nomis_file_11 %nin% c("NM_1549_1", "NM_1558_1","NM_1565_1")){
    
    ###### Aggregate by adding
    if(nomis_file_11=="NM_1504_1"){
      table_wide11$NM_1504_1_1 = table_wide11$NM_1504_1_1 + table_wide11$NM_1504_1_2
      print("Adding")
    }
    
    table_long11 = table_wide11 %>% tidyr::pivot_longer(cols = -c(Geography_Code), 
                                                        names_to = "TableVariableCode11", values_to = "Value")
    
    table_long11 = merge(table_long11, Final_codes_11_21 %>% select(TableVariableCode11,TableVariableCode21), by="TableVariableCode11") %>% 
      select(Geography_Code, TableVariableCode21, Value) 
    
    
    ### Aggregate by grouping
    if(nomis_file_11 %in% c("NM_1500_1","NM_1508_1","NM_1531_1","NM_1538_1","NM_1552_1", "NM_1544_1")){
      table_long11 = table_long11 %>% group_by(Geography_Code, TableVariableCode21) %>% summarise(Value=sum(Value))
      print("Grouping")
    }
    table_wide11 = tidyr::spread(table_long11 %>% select(Geography_Code, TableVariableCode21, Value), 
                                 key = TableVariableCode21, value = Value)
  }
  

  if(nomis_file_11 =="NM_526_1"){
    
    proficiency_in_english_Scotland = read_excel("Data/API/Census2011/NRS_2011_Proficiency_in_English.xls",skip = 12, range = cell_cols("B:G"))[-c(1:5),]
    
    colnames(proficiency_in_english_Scotland) = 
      c("Geography_Code", "NM_2048_1_0","NM_2048_1_2", "NM_2048_1_3", "NM_2048_1_4", "NM_2048_1_5")
    
    proficiency_in_english_Scotland = proficiency_in_english_Scotland[complete.cases(proficiency_in_english_Scotland),]
    proficiency_in_english_Scotland = proficiency_in_english_Scotland %>% mutate_at(vars(NM_2048_1_0:NM_2048_1_5), as.numeric)
    
       proficiency_in_english_Northern_Ireland = read_excel("Data/API/Census2011/NISRA_2011_Proficiency_in_English.xlsx", sheet=2, skip=4)[,-1]
       colnames(proficiency_in_english_Northern_Ireland) = c("Geography_Code", "NM_2048_1_0","NM_2048_1_1","NM_2048_1_2", "NM_2048_1_3", "NM_2048_1_4", "NM_2048_1_5")
       
       proficiency_in_english_Northern_Ireland = proficiency_in_english_Northern_Ireland[complete.cases(proficiency_in_english_Northern_Ireland),]
       proficiency_in_english_Northern_Ireland = proficiency_in_english_Northern_Ireland%>% mutate(NM_2048_1_2 = NM_2048_1_1 + NM_2048_1_2)  %>% select(-NM_2048_1_1)
       
    table_wide11 = table_wide11 %>% mutate(NM_2048_1_2 = NM_2048_1_1 + NM_2048_1_2)  %>% select(-NM_2048_1_1)
    table_wide11 = rbind(table_wide11, proficiency_in_english_Scotland, proficiency_in_english_Northern_Ireland)
    
    print(paste(nomis_file_11,"||| ", (paste(match(nomis_file_11, unique(census_tables_EW_OA$TableCode11)))),
                "out of", length(unique(census_tables_EW_OA$TableCode11)), " --- number of rows:", nrow(table_wide11)))
    
  }
  
  if(nomis_file_11=="NM_616_1"){
    

    religion_Scotland = read_excel("Data/API/Census2011/NRS_2011_Religion.xls",skip = 4)[-c(1:8),-1]
    colnames(religion_Scotland) = c("Geography_Code", "NM_2049_1_0", "NM_2049_1_2", "NM_2049_1_3", 
                                    "NM_2049_1_4", "NM_2049_1_5", "NM_2049_1_6", "NM_2049_1_7", "NM_2049_1_8", "NM_2049_1_1", "NM_2049_1_9")
    
    religion_Scotland = religion_Scotland[complete.cases(religion_Scotland),] %>% 
      mutate_at(vars(NM_2049_1_0:NM_2049_1_9), as.numeric) %>% 
      mutate(NM_2049_1_8 = NM_2049_1_8 +NM_2049_1_7 + NM_2049_1_6+NM_2049_1_5+NM_2049_1_4+NM_2049_1_3) %>% 
      select(Geography_Code,NM_2049_1_0,NM_2049_1_1,NM_2049_1_2, NM_2049_1_8, NM_2049_1_9)
    
      religion_Northern_Ireland = read_excel("Data/API/Census2011/NISRA_2011_Religion.xlsx",sheet = 3)[-c(1:5),c(2:11)]
      colnames(religion_Northern_Ireland) = c("Geography_Code", "NM_2049_1_0", "Catholic", "Presbyterian", 
                                              "Church_of_Ireland","Methodists", "Other_Christian",
                                              "NM_2049_1_8","NM_2049_1_1", "NM_2049_1_9")
      
      religion_Northern_Ireland = religion_Northern_Ireland[complete.cases(religion_Northern_Ireland),] %>% 
        mutate_at(vars(NM_2049_1_0:NM_2049_1_9), as.numeric) %>% 
        mutate(NM_2049_1_2 = Catholic + Presbyterian + Church_of_Ireland+Methodists+Other_Christian) %>%
        select(Geography_Code,NM_2049_1_0,NM_2049_1_1,NM_2049_1_2, NM_2049_1_8, NM_2049_1_9)
    
    
    
    table_wide11 = table_wide11 %>% 
      mutate(NM_2049_1_8 = NM_2049_1_8 +NM_2049_1_7 + NM_2049_1_6+NM_2049_1_5+NM_2049_1_4+NM_2049_1_3) %>% 
      select(Geography_Code,NM_2049_1_0,NM_2049_1_1,NM_2049_1_2, NM_2049_1_8, NM_2049_1_9)
    
    table_wide11 = rbind(table_wide11, religion_Scotland, religion_Northern_Ireland)
    
    print(paste(nomis_file_11,"||| ", (paste(match(nomis_file_11, unique(census_tables_EW_OA$TableCode11)))),
                "out of", length(unique(census_tables_EW_OA$TableCode11)), " --- number of rows:", nrow(table_wide11)))
  }
  
  
  #### AGGREGATE TO 2021 CENSUS GROUPS  
  table_wide11_oa_changed = merge(table_wide11, OA_changes %>% filter(CHNGIND=="M") %>% select(OA11CD, OA21CD), 
                                  by.x="Geography_Code", by.y="OA11CD", all.x=T) %>%
    mutate(Geography_Code = ifelse(is.na(OA21CD), Geography_Code, OA21CD)) %>% select(-OA21CD) 
  

  if(nomis_file_11 != "NM_160_1"){
    table_wide11_oa_changed = table_wide11_oa_changed %>% group_by(Geography_Code) %>% 
      summarise_if(is.numeric, sum)
  } else {
    table_wide11_oa_changed = table_wide11_oa_changed %>% group_by(Geography_Code) %>% 
      summarise_if(is.numeric, mean)
  }
  

  table_wide11[is.na(table_wide11)]=0
  table_wide11_oa_changed[is.na(table_wide11_oa_changed)]=0

  
  
  if(!exists("Census_2011")){
 
    Census_2011 = table_wide11
    Census_2011_oa_changed = table_wide11_oa_changed
    
  } else{
  #  Census_2011_raw = merge(Census_2011_raw, table_wide11_raw, by="Geography_Code", all=T)
    Census_2011 = merge(Census_2011, table_wide11, by="Geography_Code", all=T)
    #Census_2011_oa_changed_raw = merge(Census_2011_oa_changed_raw, table_wide11_oa_changed_raw, by="Geography_Code", all=T)
    Census_2011_oa_changed = merge(Census_2011_oa_changed, table_wide11_oa_changed, by="Geography_Code", all=T)
  }
  
  
  
  
}


if(any(is.na(Census_2011))){
  print("UNEVEN NUMBER OF ROWS")
}





#############################################################
#################       Census 2021         #################
#############################################################



rm(Census_2021, Census_2021_oa_changed)

##### AGGREGATE 2021 CENSUS
for(nomis_file_21 in unique(census_tables_EW_OA$TableCode21)){
  
  
  table_wide21 = read.csv(paste0("Data/API/TYPE150/Value/", nomis_file_21, ".csv"))
  
  print( paste(nomis_file_21,"||| ", (paste(match(nomis_file_21, unique(census_tables_EW_OA$TableCode21)))),
               "out of", length(unique(census_tables_EW_OA$TableCode21)), " --- number of rows:", nrow(table_wide21)))
  
  
  
  
  ### Accomodation type
  if(nomis_file_21 =="NM_2062_1"){
    table_wide21$NM_2062_1_5 =   table_wide21$NM_2062_1_5  + table_wide21$NM_2062_1_6
    
    table_long21 = table_wide21 %>% tidyr::pivot_longer(cols = -c(Geography_Code), names_to = "TableVariableCode21", values_to = "Value")
    table_long21 = merge(table_long21, Final_codes_11_21 %>% select(TableVariableCode11,TableVariableCode21), by="TableVariableCode21") %>% 
      select(Geography_Code, TableVariableCode11, Value)
    
    table_long21
    
    table_wide21 = tidyr::spread(table_long21 %>% select(Geography_Code, TableVariableCode11, Value),
                                 key = TableVariableCode11, value = Value)
  }
  
  #### Central heating
  
  
  
  
  
  if(nomis_file_21 == "NM_2064_1"){
    table_wide21$NM_2064_1_2 = table_wide21$NM_2064_1_2+ table_wide21$NM_2064_1_3
    table_wide21$NM_2064_1_6 = table_wide21$NM_2064_1_6 + table_wide21$NM_2064_1_7
    table_wide21$NM_2064_1_8 = table_wide21$NM_2064_1_8 + table_wide21$NM_2064_1_9 + table_wide21$NM_2064_1_10
    
    table_long21 = table_wide21 %>% tidyr::pivot_longer(cols = -c(Geography_Code), names_to = "TableVariableCode21", values_to = "Value")
    table_long21 = merge(table_long21, Final_codes_11_21 %>%  select(TableVariableCode11,TableVariableCode21), by="TableVariableCode21") %>% 
      select(Geography_Code, TableVariableCode11, Value)
    table_wide21 = tidyr::spread(table_long21 %>% select(Geography_Code, TableVariableCode11, Value), 
                                 key = TableVariableCode11, value = Value)
  }
  
  
  #### Ethnic group
  if(nomis_file_21 == "NM_2083_1"){
    
    table_long21 = table_wide21 %>% tidyr::pivot_longer(cols = -c(Geography_Code), names_to = "TableVariableCode21", values_to = "Value")
    table_long21 = merge(table_long21, Final_codes_11_21%>%  select(TableVariableCode11,TableVariableCode21), by="TableVariableCode21") %>% 
      select(Geography_Code, TableVariableCode11, Value)
    table_wide21 = tidyr::spread(table_long21 %>% select(Geography_Code, TableVariableCode11, Value), 
                                 key = TableVariableCode11, value = Value)
    table_wide21$NM_1565_1_1 = table_wide21$NM_1565_1_1 + table_wide21$NM_1565_1_9 
    
  }
  
  ### Main language = merging English main language with english not main but speaks very well
  if(nomis_file_21=="NM_2048_1"){
    table_wide21 = table_wide21 %>% mutate(NM_2048_1_2 = NM_2048_1_1 + NM_2048_1_2)  %>% select(-NM_2048_1_1, -NM_2048_1_1001)
    
  }
  
  
  ### Religion
  if(nomis_file_21=="NM_2049_1"){
    table_wide21 = table_wide21 %>% 
      mutate(NM_2049_1_8 = NM_2049_1_8 +NM_2049_1_7 + NM_2049_1_6+NM_2049_1_5+NM_2049_1_4+NM_2049_1_3) %>% 
      select(Geography_Code,NM_2049_1_0,NM_2049_1_1,NM_2049_1_2, NM_2049_1_8, NM_2049_1_9)
  }
  
  #### AGGREGATE TO 2021 CENSUS GROUPS  
  table_wide21_oa_changed = merge(table_wide21, OA_changes %>% filter(CHNGIND=="S") %>% select(OA11CD, OA21CD,CHNGIND), 
                                  by.x="Geography_Code", by.y="OA21CD", all.x=T) %>%
    mutate(Geography_Code = ifelse(is.na(OA11CD), Geography_Code, OA11CD)) %>% select(-OA11CD) 

  
  
  
  if(nomis_file_21!="NM_2026_1"){
    
    table_wide21_oa_changed = table_wide21_oa_changed %>% group_by(Geography_Code) %>% 
      summarise_if(is.numeric, sum)

  
    
  } else {
    
    ###### Population density in 2011 is measured as number of people per hectare, 
    ###### meanwhile for Census 2021 it is number of people per km2
    ### changing to hectare per person, beacaue 2011 is already rounded 
    table_wide21$NM_2026_1_0 = round(table_wide21$NM_2026_1_0/100, 1)
    
    table_wide21_oa_changed = table_wide21_oa_changed %>% group_by(Geography_Code) %>% 
      summarise_if(is.numeric, mean)
    table_wide21_oa_changed$NM_2026_1_0 = round(table_wide21_oa_changed$NM_2026_1_0/100, 1)

  }
 
 
  table_wide21_oa_changed[is.na(table_wide21_oa_changed)]=0
  table_wide21[is.na(table_wide21)]=0

  
  if(!exists("Census_2021")){
    Census_2021 = table_wide21
    Census_2021_oa_changed = table_wide21_oa_changed

  } else{
    Census_2021 = merge(Census_2021, table_wide21, by="Geography_Code", all=T)
    Census_2021_oa_changed = merge(Census_2021_oa_changed, table_wide21_oa_changed, by="Geography_Code", all=T)
  
  }
}

summary(Census_2021)
if(any(is.na(Census_2021))){
  print("UNEVEN NUMBER OF ROWS")
}




#######       SAVE RAW VALUES
write.csv(Census_2011, "Data/Clean/Raw_counts/Census_2011_all.csv", row.names = F)
write.csv(Census_2011_oa_changed, "Data/Clean/Raw_counts/OA_changed_Census_2011_raw_all.csv", row.names = F)
write.csv(Census_2021, "Data/Clean/Raw_counts/Census_2021_all.csv", row.names = F)
write.csv(Census_2021_oa_changed, "Data/Clean/Raw_counts/OA_changed_Census_2021_raw_all.csv", row.names = F)





## # 
##  Census_2011 =  read.csv("Data/Clean/Census_2011_all.csv")
##  Census_2011_oa_changed = read.csv("Data/Clean/OA_changed/Census_2011_oa_changed_raw_all.csv")
##  Census_2021 = read.csv("Data/Clean/Census_2021_all.csv")
##  Census_2021_oa_changed = read.csv("Data/Clean/OA_changed/Census_2021_oa_changed_raw_all.csv")
 

####            
####            #######       SUBSET TO CONTAIN COMMON VARIABLES
####            Census_2011_common_var = subset(Census_2011, select=names(Census_2011) %in% c("Geography_Code",unique(codes_names$Code)))
####            Census_2021_common_var = subset(Census_2021, select=names(Census_2021) %in% c("Geography_Code",unique(codes_names$Code)))
####            Census_2011_oa_changed_common_var = subset(Census_2011_oa_changed, select=names(Census_2011_oa_changed) %in% c("Geography_Code",unique(codes_names$Code)))
####            Census_2021_oa_changed_common_var = subset(Census_2021_oa_changed, select=names(Census_2021_oa_changed) %in% c("Geography_Code",unique(codes_names$Code)))
####            
####            
####            
####            
####            
####            
####            
####            #################################################
####            ########      CALCULATE PERCENTAGES       #######
####            #################################################
####            
####            
####            #### list of common variables
####            common_variables = codes_names %>% filter(Code %in% colnames(Census_2011_common_var))
####            ### get a list of denominators (excluding Density)
####            denominators = common_variables %>% filter(CategoryCode11==0 | TS_code=="TS006")
####            
####            
####            
####            
####            
####            
####            rm(Census_2011_common_var_prop,Census_2021_common_var_prop,
####               Census_2011_oa_changed_common_var_prop,Census_2021_oa_changed_common_var_prop)
####            
####            
####            for(deno in unique(denominators$Code)){
####              
####              print(paste(deno,"||| ", (paste(match(deno, unique(denominators$Code)))),
####                           "out of", length(unique(denominators$Code))))
####              
####              ### get TS code of a denominator
####              denominator_TS_code = denominators[denominators$Code==deno, "TS_code"]
####              
####              ### get nominators of the denominator
####              nominators = (common_variables %>% filter(TS_code==denominator_TS_code, Code!=deno))$Code
####              
####              
####              table11 = Census_2011_common_var[, c("Geography_Code",deno, nominators)]
####              table21 = Census_2021_common_var[, c("Geography_Code",deno, nominators)]
####              table11_oa_changed = Census_2011_oa_changed_common_var[, c("Geography_Code",deno, nominators)]
####              table21_oa_changed = Census_2021_oa_changed_common_var[, c("Geography_Code",deno, nominators)]
####            
####              if(ncol(table11)!=ncol(table21)){
####                print("STOP")
####                break
####              }
####              
####              if(deno!="NM_2026_1_0"){
####              for(i in 3:ncol(table11)){
####                
####                table11[,i] = round((table11[,i] / table11[, deno]), digits=4)
####                table21[,i] = round((table21[,i] / table21[, deno]), digits=4)
####                table11_oa_changed[,i] = round((table11_oa_changed[,i] / table11_oa_changed[, deno]), digits=4)
####                table21_oa_changed[,i] = round((table21_oa_changed[,i] / table21_oa_changed[, deno]), digits=4)
####               
####                table11[is.na(table11)]=0
####                table21[is.na(table21)]=0 
####                table11_oa_changed[is.na(table11_oa_changed)]=0
####                table21_oa_changed[is.na(table21_oa_changed)]=0 
####              }
####              }
####            
####              
####            if(!exists("Census_2011_common_var_prop")){
####              Census_2011_common_var_prop = table11
####              Census_2021_common_var_prop = table21
####              Census_2011_oa_changed_common_var_prop = table11_oa_changed
####              Census_2021_oa_changed_common_var_prop = table21_oa_changed
####              
####            } else{
####              
####              Census_2011_common_var_prop = merge(Census_2011_common_var_prop, table11, by="Geography_Code", all=T)
####              Census_2021_common_var_prop = merge(Census_2021_common_var_prop, table21, by="Geography_Code", all=T)
####              Census_2011_oa_changed_common_var_prop = merge(Census_2011_oa_changed_common_var_prop, table11_oa_changed, by="Geography_Code", all=T)
####              Census_2021_oa_changed_common_var_prop = merge(Census_2021_oa_changed_common_var_prop, table21_oa_changed, by="Geography_Code", all=T)
####            }
####              
####            }
####            
####            
####            
####            
####            ###############################################################
####            ########     Remove denominators from the tables     ##########
####            ###############################################################
####            
####            common_variables_without_denominators = codes_names %>% filter(Code %in% colnames(Census_2011_common_var), CategoryCode11!=0) 
####            common_variables_without_denominators$encoding = paste0("r", str_pad(c(1:nrow(common_variables_without_denominators)),3,pad="0"))
####            
####            
####            df_list = c("Census_2011_common_var","Census_2021_common_var",
####                        "Census_2011_oa_changed_common_var", "Census_2021_oa_changed_common_var",
####                        "Census_2011_common_var_prop", "Census_2021_common_var_prop",
####                        "Census_2011_oa_changed_common_var_prop", "Census_2021_oa_changed_common_var_prop")
####            
####            for(dat in df_list){
####              print(paste(dat,"||| ", (paste(match(dat, df_list))),
####                          "out of", length(df_list)))
####              
####              tab = get(dat)
####              tab = subset(tab, select=names(tab) %nin% c(denominators[denominators$CategoryCode11==0, "Code"]))
####              names(tab)[match(common_variables_without_denominators[,"Code"], 
####                               names(tab))] = common_variables_without_denominators[,"encoding"]
####              tab = tab[,c("Geography_Code", common_variables_without_denominators$encoding)]
####              
####              assign(dat,tab)
####            
####            }
####            
####            
####            
####            #############################################
####            #######       TRANSFORMATIONS       #########
####            #############################################
####            
####            Range_0_to_1 <- function(x){(x-min(x))/(max(x)-min(x))}
####            
####            for(dat in df_list){
####              print(paste(dat,"||| ", (paste(match(dat, df_list))),
####                          "out of", length(df_list)))
####              
####              tab = get(dat)
####              
####              #### percentages
####              tab[,-1] = tab[,-1] *100
####              assign(paste0(dat,"_perc"),tab)
####            
####              
####              #### Inverse Hyperbolic Sine
####              tab[,-1] = log(tab[,-1] + sqrt(tab[,-1]^2+1))
####              assign(paste0(dat,"_perc_IHS"),tab)
####             
####              #### Range standardization
####              tab[,-1] = apply(tab[,-1], 2, Range_0_to_1)
####              assign(paste0(dat,"_perc_IHS_range"),tab)
####              
####              print(paste0("Creating: ", dat,"_perc", " ||| ", dat,"_perc_IHS", " ||| ", dat,"_perc_IHS_range"))
####            }
####            
####            
####            
####            ##########################################################
####            #######       FINAL SELECTION OF VARIABLES       #########
####            ##########################################################
####            
####            
####            
####            common_variables_without_denominators
####            
####            aggregated_categories = data.frame(Code="age_5_19", Name="Aged 5 to 19", TableCode="NM_2020_1", TS_code="TS007A", TableName="Age structure", CategoryCode=NA, encoding=NA)
####            aggregated_categories = rbind(aggregated_categories, data.frame(Code="age_20_29", Name="Aged 20 to 29", TableCode="NM_2020_1", TS_code="TS007A", TableName="Age structure", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="age_30_39", Name="Aged 30 to 39", TableCode="NM_2020_1", TS_code="TS007A", TableName="Age structure", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="age_40_54", Name="Aged 40 to 54", TableCode="NM_2020_1", TS_code="TS007A", TableName="Age structure", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="age_55_69", Name="Aged 55 to 69", TableCode="NM_2020_1", TS_code="TS007A", TableName="Age structure", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="age_70_84", Name="Aged 70 to 84", TableCode="NM_2020_1", TS_code="TS007A", TableName="Age structure", CategoryCode=NA, encoding=NA))
####                                              
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="separated_divorced", Name="Separated or divorced", TableCode="NM_2022_1", TS_code="TS002", TableName="Legal partnership status", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="no_children", Name="Families with no children", TableCode="NM_2023_1", TS_code="TS003", TableName="Household composition", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="dependent_children", Name="Families dependent children", TableCode="NM_2023_1", TS_code="TS003", TableName="Household composition", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="nondependent_children", Name="Families with non-dependent children", TableCode="NM_2023_1", TS_code="TS003", TableName="Household composition", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="multi_ethnic_household", Name="Multi-ethnic household", TableCode="NM_2042_1", TS_code="TS023", TableName="Multiple ethnic group", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="cannot_speak_English", Name="Cannot speak English well or at all", TableCode="NM_2048_1", TS_code="TS029", TableName="Proficiency in English", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="provides_npaid_care", Name="Provides unpaid care", TableCode="NM_2057_1", TS_code="TS039", TableName="Provision of unpaid care", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="flat", Name="Lives in a flat", TableCode="NM_1549_1", TS_code="TS044", TableName="Accomodation type", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="cars_2_or_more", Name="Two or more cars or vans", TableCode="NM_2063_1", TS_code="TS045", TableName="Car or van availability", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="under_occupation", Name="Under occupation", TableCode="NM_2071_1", TS_code="TS053", TableName="Number of rooms", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="overcrowding", Name="Overcrowding", TableCode="NM_2071_1", TS_code="TS053", TableName="Number of rooms", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="ownership_or_shared", Name="Ownership or shared ownership", TableCode="NM_2072_1", TS_code="TS054", TableName="Tenure", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="under_2_level", Name="No qualification, Level 1 or 2 Level of qualification", TableCode="NM_2084_1", TS_code="TS067", TableName="Highest level of qualification", CategoryCode=NA, encoding=NA))
####            aggregated_categories = rbind(aggregated_categories,data.frame(Code="level_3_or_4", Name="Level 3 or Level 4 of qualification", TableCode="NM_2084_1", TS_code="TS067", TableName="Highest level of qualification", CategoryCode=NA, encoding=NA))
####            
####            
####            
####            Census_2011_common_var_agg = subset(Census_2011, select=names(Census_2011) %in% c("Geography_Code",unique(final_oac_list$Code)))
####            Census_2021_common_var_agg = subset(Census_2021, select=names(Census_2021) %in% c("Geography_Code",unique(final_oac_list$Code)))
####            
####            
####            EW21_S11 = rbind(Census_2021_common_var_agg, 
####                             Census_2011_common_var_agg %>% filter(grepl("S", Geography_Code)))
####            
####            
####            Scotland_2011_codes  = Census_2011_common_var_agg %>% filter(grepl("S", Geography_Code)) %>% distinct(Geography_Code)
####            
####            
####            
####            EW21_S11 = EW21_S11 %>% mutate(age_5_19 = NM_2020_1_2 + NM_2020_1_3 + NM_2020_1_4,
####                                               age_20_29 = NM_2020_1_5 + NM_2020_1_6,
####                                               age_30_39 = NM_2020_1_7 + NM_2020_1_8,
####                                               age_40_54 = NM_2020_1_9 + NM_2020_1_10 + NM_2020_1_11,
####                                               age_55_69 = NM_2020_1_12 + NM_2020_1_13 + NM_2020_1_14,
####                                               age_70_84 = NM_2020_1_15 + NM_2020_1_16 + NM_2020_1_17,
####                                               separated_divorced = NM_2022_1_1004 + NM_2022_1_1005,
####                                               no_children = NM_2023_1_4 + NM_2023_1_7,
####                                               dependent_children =  NM_2023_1_5 +  NM_2023_1_8  + NM_2023_1_10 + NM_2023_1_13 ,
####                                               nondependent_children = NM_2023_1_6  + NM_2023_1_9 + NM_2023_1_11 ,
####                                               multi_ethnic_household =  NM_2042_1_3 + NM_2042_1_4 + NM_2042_1_5,
####                                               cannot_speak_English =  NM_2048_1_4 + NM_2048_1_5,
####                                               provides_npaid_care = NM_2057_1_101 + NM_2057_1_102 + NM_2057_1_6  ,
####                                               flat = NM_1549_1_7 + NM_1549_1_8 + NM_1549_1_9,
####                                               cars_2_or_more = NM_2063_1_3 +  NM_2063_1_4,
####                                               under_occupation = NM_2071_1_1 + NM_2071_1_2,
####                                               overcrowding = NM_2071_1_4 +  NM_2071_1_5 ,
####                                               ownership_or_shared = NM_2072_1_1001 + NM_2072_1_1002,
####                                               under_2_level = NM_2084_1_1 + NM_2084_1_2 + NM_2084_1_3,
####                                               level_3_or_4 =  NM_2084_1_5 + NM_2084_1_6
####                                               )
####             
####            
####            head(EW21_S11)
####             
####             
####            final_oac_list = rbind(common_variables %>% rename(TableName=TableName21, CategoryCode=CategoryCode11), aggregated_categories %>% select(-encoding))
####            
####            #################################################
####            ########      CALCULATE PERCENTAGES       #######
####            #################################################
####            
####            
####            #### list of common variables
####            #common_variables = codes_names %>% filter(Code %in% colnames(Census_2011_common_var_agg))
####            ### get a list of denominators (excluding Density)
####            denominators = common_variables %>% filter(CategoryCode11==0 | TS_code=="TS006")
####            
####            
####            
####            
####            
####            
####            rm(EW21_S11_prop)#,
####              # Census_2011_oa_changed_common_var_prop,Census_2021_oa_changed_common_var_prop)
####            
####            
####            for(deno in unique(denominators$Code)){
####              
####              print(paste(deno,"||| ", (paste(match(deno, unique(denominators$Code)))),
####                          "out of", length(unique(denominators$Code))))
####              
####              ### get TS code of a denominator
####              denominator_TS_code = denominators[denominators$Code==deno, "TS_code"]
####              
####              ### get nominators of the denominator
####              nominators = (final_oac_list %>% filter(TS_code==denominator_TS_code, Code!=deno))$Code
####              
####              
####              table = EW21_S11[, c("Geography_Code",deno, nominators)]
####              
####              if(deno!="NM_2026_1_0"){
####                for(i in 3:ncol(table)){
####                  
####                  table[,i] = round((table[,i] / table[, deno]), digits=4)
####                 
####                }
####                 table[is.na(table)]=0 
####              }
####              
####              
####              if(!exists("EW21_S11_prop")){
####                EW21_S11_prop = table
####              } else{
####                
####                EW21_S11_prop = merge(EW21_S11_prop, table, by="Geography_Code", all=T)
####            
####              }
####              
####            }
####            
####            any(is.na(EW21_S11_prop))
####            
####            EW21_S11_prop = merge(EW21_S11_prop, Scotland_2011_codes %>% mutate(country="Scotland"), by="Geography_Code", all=T)
####            EW21_S11_prop$country = ifelse(is.na(EW21_S11_prop$country), "EW", EW21_S11_prop$country)
####            
####            
####            ####### TRANSFORMATIONS
####            
####            
####            Range_0_to_1 <- function(x){(x-min(x))/(max(x)-min(x))}
####            
####            for(dat in c("EW21_S11_prop")){
####            
####              tab = get(dat)
####              if("country" %in% colnames(tab)){
####                tab = tab %>% select(-country)
####              }
####              #### percentages
####              tab[,-1] = tab[,-1] *100
####              assign(paste0(dat,"_perc"),tab)
####              
####              
####              #### Inverse Hyperbolic Sine
####              tab[,-1] = log(tab[,-1] + sqrt(tab[,-1]^2+1))
####              assign(paste0(dat,"_perc_IHS"),tab)
####              
####              #### Range standardization
####              tab[,-1] = apply(tab[,-1], 2, Range_0_to_1)
####              assign(paste0(dat,"_perc_IHS_range"),tab)
####              
####              print(paste0("Creating: ", dat,"_perc", " ||| ", dat,"_perc_IHS", " ||| ", dat,"_perc_IHS_range"))
####            }
####             
####            
####            
####            EW21_S11_prop_perc_IHS_range = merge(EW21_S11_prop_perc_IHS_range, Scotland_2011_codes %>% mutate(country="Scotland"), by="Geography_Code", all=T)
####            EW21_S11_prop_perc_IHS_range$country = ifelse(is.na(EW21_S11_prop_perc_IHS_range$country), "EW", EW21_S11_prop_perc_IHS_range$country)
####            
####            
####            
####            
####            
####            
####            ggplot(EW21_S11_prop_perc_IHS_range) + geom_density(aes(x=dependent_children, y=..density.., fill=country), alpha=0.3)
####            ggplot(EW21_S11_prop_perc_IHS_range) + geom_histogram(aes(x=dependent_children, fill=country, stat="identity"), alpha=0.3, binwidth = 0.01)
####            
####            
####            
####            
####             
####            
####            
####            
####            
####            
####            
####            
####            
####            for(table in unique(final_oac_list$TS_code)){
####              
####              cat("\r", table, " --- ", paste(match(table, unique(final_oac_list$TS_code))), "out of", length(unique(final_oac_list$TS_code)))
####              flush.console()
####              table_codes = (final_oac_list %>% filter(TS_code==table))$Code
####              
####              if(length(table_codes)==9){
####                no_col=3
####              } else {
####                no_col=2
####              }
####              
####              
####              EW21_S11_prop_perc_IHS_range[,c(table_codes, "country")]  %>% 
####                pivot_longer(-country) %>% 
####                mutate(country = as.factor(country)) %>% 
####                ggplot(aes(value)) + 
####                geom_density(aes(fill = country), alpha  = 0.5) + 
####                facet_wrap(~name)
####              
####              
####              
####              
####              
####              EW21_S11_long =   EW21_S11_prop_perc_IHS_range[,c("Geography_Code", table_codes)] %>% 
####                tidyr::pivot_longer(cols = -c(Geography_Code), 
####                                    names_to = "encoding", values_to = "Value")
####              
####              EW21_S11_long = merge(EW21_S11_long, Scotland_2011_codes %>% mutate(country="Scotland"), by="Geography_Code", all=T)
####              EW21_S11_long$country = ifelse(is.na(EW21_S11_long$country), "EW", EW21_S11_long$country)
####              
####              
####              ggplot(EW21_S11_long) + geom_density(aes(y=..density..,fill=country), alpha=0.3) + facet_wrap(~encoding)
####              
####              for(code in table_codes) {
####                ggplot(EW21_S11_prop_perc_IHS_range) + geom_density(aes(x=EW21_S11_prop_perc_IHS_range[,code], y=..density..,fill=country), alpha=0.3)
####              }
####              
####            
####              
####              
####            
####              
####              
####            }
####            
####            
####            
####            
####            
####            
####            
####            
####            
####            
####             
####            ########    AGE
####            ##      age_5_19 = NM_2020_1_2 + NM_2020_1_3 + NM_2020_1_4
####            ##      age_20_29 = NM_2020_1_5 + NM_2020_1_6
####            ##      age_30_39 = NM_2020_1_7 + NM_2020_1_8
####            ##      age_40_54 = NM_2020_1_9 + NM_2020_1_10 + NM_2020_1_11
####            ##      age_55_69 = NM_2020_1_12 + NM_2020_1_13 + NM_2020_1_14
####            ##      age_70_84 = NM_2020_1_15 + NM_2020_1_16 + NM_2020_1_17
####            
####            #######      Legal partnership status
####            ###   Separated_divorced = NM_2022_1_1004 + NM_2022_1_1005
####            
####            #######       Household composition
####            ## Families with no children = 
####            ##        NM_2023_1_4 (Single family household: Married or civil partnership couple: No children) + 
####            ##        NM_2023_1_7 (Single family household: Cohabiting couple family: No children)  
####            ## Families with dependent children =
####            ##         NM_2023_1_5  (Single family household: Married or civil partnership couple: Dependent children) + 
####            ##        NM_2023_1_8   (Single family household: Cohabiting couple family: With dependent children) + 
####            ##        NM_2023_1_10  (Single family household: Lone parent family: With dependent children) + 
####            ##        NM_2023_1_13  (Other household types: With dependent children)
####            ## Families with non-dependent children =
####            ##        NM_2023_1_6   (Single family household: Married or civil partnership couple: All children non-dependent) +
####            ##        NM_2023_1_9   (Single family household: Cohabiting couple family: All children non-dependent) +
####            ##        NM_2023_1_11  (Single family household: Lone parent family: All children non-dependent)
####            
####            #######      Multiple ethnic group
####            ### Ethnic group differs  = 
####            ##        NM_2042_1_3       (Ethnic groups differ between generations but not within partnerships) + 
####            ##        NM_2042_1_4       (Ethnic groups differ within partnerships) +
####            ##        NM_2042_1_5       (Any other combination of multiple ethnic identities)
####            
####            ##### Proficiency in English
####            ## Cannot speak English well or at all =
####            ##      NM_2048_1_4  (Cannot speak English well) +
####            ##      NM_2048_1_5  # Cannot speak English)
####            
####            #### Provision of unpaid care
####            ## Provides unpaid care =
####            ##        NM_2057_1_101     (Provides 19 hours or less unpaid care a week) +
####            ##        NM_2057_1_102     (Provides 20 to 49 hours unpaid care a week) +
####            ##        NM_2057_1_6       (Provides 50 or more hours unpaid care a week)
####            
####            ####  Accommodation type
####            ##        Flat = 
####            ##        NM_1549_1_7       (Unshared dwelling: Flat, maisonette or apartment: Purpose-built block of flats or tenement) + 
####            ##        NM_1549_1_8       (Unshared dwelling: Flat, maisonette or apartment: Part of a converted or shared house (including bed-sits)) +
####            ##        NM_1549_1_9       (Unshared dwelling: Flat, maisonette or apartment: In commercial building)
####            
####            #### Car or van availability
####            ##  2 or more cars =
####            ##          NM_2063_1_3     (2 cars or vans in household) +
####            ##          NM_2063_1_4     (3 or more cars or vans in household)
####            
####            ####    Occupancy rating
####            ##      Under-occpuation = 
####            ##                    NM_2071_1_1       (Occupancy rating of rooms: +2 or more) +
####            ##                    NM_2071_1_2       (Occupancy rating of rooms: +1)
####            ##      Over-crowding = 
####            ##                    NM_2071_1_4       (Occupancy rating of rooms: -1)
####            ##                    NM_2071_1_5       (Occupancy rating of rooms: -2 or less)
####            
####            ####    Tenure
####            ##      Ownership or shared ownership = 
####            ##          NM_2072_1_1001        (Owned) + 
####            ##          NM_2072_1_1002        (hared ownership)
####            
####            #### Highest level of qualifications
####            ## Under 2 level qualification = 
####            ##        NM_2084_1_1       (No qualifications) + 
####            ##        NM_2084_1_2       (Level 1 and entry level qualifications) +
####            ##        NM_2084_1_3       (Level 2 qualifications)
####            
####            ##  3 or 4 level qualification =
####            ##        NM_2084_1_5     (Level 3 qualifications)
####            ##        NM_2084_1_6     (Level 4 qualifications or above)
####            
####            
####            
####            
####            
####            ##########################################################
####            #######       MERGING 2021 AND 2011 DATA       ###########
####            ##########################################################
####            
####            
####            
####            
####            
####            
####            
####            
####            
####            
####            nrow(Census_2011_raw_common_var %>% filter(grepl("S", Geography_Code))) 
####            EW21_S11 = rbind(Census_2021_raw_common_var %>% mutate(country = "EW"), 
####                             Census_2011_prop_common_var %>% filter(grepl("S", Geography_Code)) %>% 
####                               mutate(country="Scotland"))
####            
####            OA_21 = sf::st_read("../../shapefiles/boundaries_2021/OA_2021/OA_2021_EW_BGC.shp")
####            OA_11 = sf::st_read("../../shapefiles/OA_SA_2011/infuse_oa_lyr_2011.shp")
####            
####            nrow(OA_11 %>% filter(grepl("S", geo_code)))
####            
####            OA_shp = rbind(OA_21 %>% select(OA21CD, geometry), OA_11 %>% select(geo_code, geometry))
####            
####            
####            
####            
####            
####            ggplot() + geom_histogram(EW21_S11, mapping = aes(x=NM_2023_1_3, fill=country, y=..density..), binwidth=2) +
####              geom_density(alpha=0.5)
####            