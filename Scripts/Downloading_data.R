#### Download data


setwd("~/Desktop/PhD/GIT/OAC2021")
library(nomisr)
library(dplyr)
library(sf)
library(tmap)
library(tidyverse)
library(readxl)

load("mynomis_function.RData")
census_tables_EW_OA = read.csv("Data/Lookups/Tables_metadata.csv") %>% 
  filter(grepl("UK", Census11) | 
           TableName21 %in% c("Proficiency in English", "Religion"),
         TableName21 %nin% c("Schoolchildren and full-time students", 
                             "Employment history")) 

variable_reference_2011 = read.csv("Data/Lookups/Variable_reference_2011.csv")
variable_reference_2021 = read.csv("Data/Lookups/Variable_reference_2021.csv")




#########################################################
############     GET 2021 CENSUS DATA      ##############
#########################################################

##    TYPE150     =     2021 output areas                      
##    TYPE151     =     2021 super output areas - lower layer  
##    TYPE152     =     2021 super output areas - middle layer 
##    TYPE154     =     2022 local authorities: districts      
##    TYPE155     =     2022 local authorities: counties       
##    TYPE480     =     regions                                
##    TYPE499     =     countries     





### Retrieve tables for all tables where the lowest resolution is TYPE150 (Output Area)
### Filter the tables to include only those where data are available for England & Wales
#census_tables_EW_OA = census_tables_all %>% filter(spatial_resolution_code=="TYPE150", coverage=="England and Wales")

resolution_code = "TYPE150"

#tables = NULL
#### Download both Values (20100) and Percent (20301)


for(table_id in census_tables_EW_OA$TableCode21){
  
  if(paste0(table_id, ".csv") %in% list.files(paste0(getwd(),"/Data/API/", resolution_code,"/Value"))){
    print(paste(table_id, "READY"))
    next
  }
  
  print(table_id)
  
  table =  mynomis(id = table_id, geography=resolution_code,tidy=T)[,c(9,14,21,13, 20)]
  
  
  # Change names of the columns
  colnames(table) = c("Geography_Code", "Variable_Name", "Value", "VariableCode", "Measure")
  
  # Insert the ID of a table
  table$Table_id = table_id
  
  # Insert name of the table
  table$Table_Name =  variable_reference_2021[variable_reference_2021$TableCode==table_id, "TableName"][1]
  # Table$Table_Name = census_tables[census_tables$id==table_id, "name.value"]
  
  # Create a unique identifier for a variable
  table$TableVariableCode = paste0(table_id, "_", table$VariableCode)
  
  # Change from 'long' to 'wide' format
  
  for(mes in unique(table$Measure)){
    if(mes=="Percent"){
      next
    }
    
    table_wide = table %>% filter(Measure==mes) %>% 
      select(Geography_Code, TableVariableCode, Value) %>% 
      spread(TableVariableCode, value=Value)
    
    
    write.csv(table_wide, paste0(getwd(),"/Data/API/", resolution_code, "/", mes, "/", table_id, ".csv"), row.names = F)
    
 
  }
  
  
  if(resolution_code=="TYPE150"){
    Sys.sleep(60)
  }
  
  if(resolution_code=="TYPE152"){
    Sys.sleep(30)
  }
  
  
  # Bind the datasets
  #  tables=rbind(tables, table)
  
}








#########################################################
############     GET 2011 CENSUS DATA      ##############
#########################################################
 
 
unique(census_tables_EW_OA$TableCode11)
nomis_get_metadata(id=census_tables_EW_OA[1,"TableCode11"], concept = "GEOGRAPHY", type="type")


### TYPE297          2011 super output areas - middle layer   
### TYPE298           2011 super output areas - lower layer   
### TYPE299                               2011 output areas   
### TYPE258                             2011 NI small areas   

##### this is  for ENGLAND, WALES AND SCOTLAND (TYPE 299) and NORTHERN IRELAND (TYPE258)
resolution_code = c("TYPE299", "TYPE258")


for(res_code in resolution_code){
# tables_2011 = NULL
for(table_id in unique(census_tables_EW_OA$TableCode11)){
  
  if(paste0(table_id, ".csv") %in% list.files(paste0(getwd(), "/Data/API/Census2011/",res_code,"/Value"))){
    print(paste(table_id, "READY"))
    next
  } else {
    print(paste(table_id," --- ",   (paste(match(table_id, unique(census_tables_EW_OA$TableCode11)),"out of", length(unique(census_tables_EW_OA$TableCode11))))))
  }
  
  ### check if the table contains VALUES and PERCENT
  
  if(res_code %nin% unique((nomis_get_metadata(id=table_id, concept = "GEOGRAPHY", type="type")$id))){
    print(paste("Table  ", table_id, "  not available for  ", res_code))
    next
  }
  if(  nrow(mynomis(table_id, geography="TYPE499", tidy=T) %>% distinct(measures_name)) == 2){
    #### It is faster to download VALUE and PERCENT seperately and bind them
    Start = Sys.time()
    ### Value = 20100
    table_concepts = nomis_get_metadata(table_id)
    #nomis_codelist(table_id, concept="time")
    

    if(any(table_concepts$conceptref=="RURAL_URBAN")){
      
      table_value = mynomis(id = table_id, geography = res_code, tidy=T, measures=20100, rural_urban="0",time = "2011")
      table_perc = mynomis(id = table_id, geography = res_code, tidy=T, measures=20301, rural_urban="0",time = "2011")
      table_value = table_value[,c(9, 20, 27, 19,26)]
      table_perc = table_perc[,c(9, 20, 27, 19,26)]
      
    } else {
      
      table_value = mynomis(id = table_id, geography = res_code, tidy=T, measures=20100, time = "2011")
   #   table_perc = mynomis(id = table_id, geography = res_code, tidy=T, measures=20301,time = "2011")
     table_value = table_value[,c(9,14,21,13, 20)] 
  #   table_perc = table_perc[,c(9,14,21,13, 20)] 
    }
    
    ### Percent = 20301

    
    table = rbind(table_value, table_perc)
    End = Sys.time()
    
    End-Start
  } else { 
    
    table= mynomis(id = table_id, geography = res_code, tidy=T)[,c(9,14,21,13, 20)]
    
  }
  #     Start = Sys.time()
  #     table =  mynomis(id = table_id, geography=resolution_code,tidy=T)[,c(9,14,21,13, 20)]
  #     End = Sys.time()
  
  # Change names of the columns
  colnames(table) = c("Geography_Code", "Variable_Name", "Value", "VariableCode", "Measure")
  
  # Insert the ID of a table
  table$Table_id = table_id
  
  # Insert name of the table
  
  table$Table_Name =  variable_reference_2011[variable_reference_2011$TableCode==table_id, "TableName"][1]
  # Table$Table_Name = census_tables[census_tables$id==table_id, "name.value"]
  
  # Create a unique identifier for a variable
  table$TableVariableCode = paste0(table_id, "_", table$VariableCode)
  
  # Change from 'long' to 'wide' format
  for(mes in unique(table$Measure)){
    
    if(mes=="Percent"){
      next
    }
    
    table_wide = table %>% filter(Measure==mes) %>% 
      select(Geography_Code, TableVariableCode, Value) %>% 
      spread(TableVariableCode, value=Value)
    
    write.csv(table_wide, paste0(getwd(), "/Data/API/Census2011/", res_code, "/", mes, "/", table_id, ".csv"), row.names = F)
    

    
  }
  
  
  if(res_code %in% c("TYPE299", "TYPE258")){
    print(paste(mes,"Wrote down"))
    Sys.sleep(60)
  }
  
  if(res_code=="TYPE298"){
    print(paste(mes,"Wrote down"))
    Sys.sleep(30)
  }
  
  #  # Bind the datasets
  # tables_2011=rbind(tables_2011, table)
  
}
}



#####################################################################
############     GET NORTHERN IRELAND CENSUS DATA      ##############
#####################################################################


### LOAD FUNCTION FOR DOWNLOADING DATA




#
#resolution_code = "TYPE258"
## tables_2011 = NULL
#for(table_id in unique(census_tables_EW_OA$TableCode11)){
#  
#  if(paste0(table_id, ".csv") %in% list.files(paste0("~/Desktop/PhD/GIT/OAC2021/Data/API/Census2011/Northern_Ireland/",resolution_code,"/Value"))){
#    print(paste(table_id, "READY"))
#    next
#  } else {
#    print(paste(table_id," --- ",   (paste(match(table_id, unique(codes11_21$TableCode11)),"out of", length(unique(codes11_21$TableCode11))))))
#  }
#  
#  
#  
#  if(  nrow(mynomis(table_id, geography="TYPE499", tidy=T) %>% distinct(measures_name)) == 2){
#    #### It is faster to download VALUE and PERCENT seperately and bind them
#    Start = Sys.time()
#    ### Value = 20100
#    table_value = mynomis(id = table_id, geography = resolution_code, tidy=T, measures=20100)[,c(9,14,21,13, 20)]
#    
#    ### Percent = 20301
#    table_perc = mynomis(id = table_id, geography = resolution_code, tidy=T, measures=20301)[,c(9,14,21,13, 20)]
#    
#    table = rbind(table_value, table_perc)
#    End = Sys.time()
#    
#    End-Start
#  } else { 
#    
#    table= mynomis(id = table_id, geography = resolution_code, tidy=T)[,c(9,14,21,13, 20)]
#    
#  }
#  #     Start = Sys.time()
#  #     table =  mynomis(id = table_id, geography=resolution_code,tidy=T)[,c(9,14,21,13, 20)]
#  #     End = Sys.time()
#  
#  
#  
#  # Change names of the columns
#  colnames(table) = c("Geography_Code", "Variable_Name", "Value", "VariableCode", "Measure")
#  
#  # Insert the ID of a table
#  table$Table_id = table_id
#  
#  # Insert name of the table
#  
#  table$Table_Name =  variable_reference_2011[variable_reference_2011$TableCode==table_id, "TableName"][1]
#  # Table$Table_Name = census_tables[census_tables$id==table_id, "name.value"]
#  
#  # Create a unique identifier for a variable
#  table$TableVariableCode = paste0(table_id, "_", table$VariableCode)
#  
#  # Change from 'long' to 'wide' format
#  
#  for(mes in unique(table$Measure)){
#    
#    table_wide = table %>% filter(Measure==mes) %>% 
#      select(Geography_Code, TableVariableCode, Value) %>% 
#      spread(TableVariableCode, value=Value)
#    
#    write.csv(table_wide, paste0("~/Desktop/PhD/GIT/OAC2021/Data/API/Census2011/Northern_Ireland/", resolution_code, "/", mes, "/", table_id, ".csv"), row.names = F)
#    
#    
#    if(resolution_code=="TYPE258"){
#      print("Wrote down")
#      Sys.sleep(60)
#    }
#    
#
#  }
#  
#  #  # Bind the datasets
#  # tables_2011=rbind(tables_2011, table)
#  
#}
#
#









### 
### proficiency_in_english_Scotland = read_excel("Data/API/Census2011/NRS_2011_Proficiency_in_English.xls",skip = 12, range = cell_cols("B:G"))[-c(1:5),]
### colnames(proficiency_in_english_Scotland) = c("Geography_Code", "Total", "Speaks_english_very_well", "Speaks_english_well", "Does_not_speak_english_well", "Does_not_speak_english_at_all")
### 
### 
### proficiency_in_english_Northern_Ireland = read_excel("Data/API/Census2011/Northern_Ireland/NISRA_2011_Proficiency_in_English.xlsx", sheet=2, skip=4)[,-1]
### colnames(proficiency_in_english_Northern_Ireland) = c("Geography_Code", "Total", "Main_language_is_English", "Main_language_is_not_english_can_speak_very_well",
###                                                       "Main_language_is_not_english_can_speak_well","Main_language_is_not_english_cannot_speak_well", 
###                                                       "Main_language_is_not_english_cannot_speak_english")
### table_wide2 =  read.csv("Data/API/Census2011/TYPE299/Value/NM_526_1.csv")
### 
### table_wide2 = merge(table_wide2 %>%  tidyr::pivot_longer(cols = -c(Geography_Code), names_to = "TableVariableCode11", values_to = "Value"), variable_reference_2011 %>% 
###         select(TableVariableCode11, VariableName11), by="TableVariableCode11") 
### 
### 
### 
### 
### 
### table_wide = read.csv("Data/API/Census2011/TYPE299/Value/NM_616_1.csv")
### table_wide2 = merge(table_wide %>%  tidyr::pivot_longer(cols = -c(Geography_Code), names_to = "TableVariableCode11", values_to = "Value"), variable_reference_2011 %>% 
###                       select(TableVariableCode11, VariableName11), by="TableVariableCode11") 
### table_wide2 %>% distinct(VariableName11)
### 
### 
