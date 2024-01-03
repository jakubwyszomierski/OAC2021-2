
#### METADATA TABLES

setwd("~/Desktop/PhD/GIT/OAC2021")

library(nomisr)
library(dplyr)
library(sf)
library(tidyverse)
library(readxl)




##############################################################
###########       OVERWRITE NOMIS FUNCTION       #############
##############################################################


mynomis <- function(id, time = NULL, date = NULL, geography = NULL,
                    sex = NULL, measures = NULL,
                    additional_queries = NULL, exclude_missing = FALSE,
                    select = NULL, tidy = FALSE,
                    tidy_style = "snake_case", query_id = NULL, ...){
  if (missing(id)) {
    stop("Dataset ID must be specified", call. = FALSE)
  }
  
  # check for use or time or data parameter
  if (is.null(date) == FALSE) {
    time_query <- paste0("&date=", paste0(date, collapse = ","))
  } else if (is.null(time) == FALSE) {
    time_query <- paste0("&time=", paste0(time, collapse = ","))
  } else {
    time_query <- ""
  }
  
  geography_query <- ifelse(!is.null(geography),
                            paste0(
                              "&geography=",
                              paste0(geography, collapse = ",")
                            ),
                            ""
  )
  
  if (length(additional_queries) > 0) {
    additional_query <- additional_queries
    
    message("The `additional_query` parameter is
            deprecated, please use ... instead")
  } else {
    additional_query <- NULL
  }
  
  # Check for sex queries and return either sex or c_sex or gender
  if (length(sex) > 0) {
    sex_lookup <- nomis_data_info(id)$components.dimension[[1]]$conceptref
    
    if ("C_SEX" %in% sex_lookup) {
      sex_query <- paste0("&c_sex=", paste0(sex, collapse = ","))
    } else if ("SEX" %in% sex_lookup) {
      sex_query <- paste0("&sex=", paste0(sex, collapse = ","))
    } else if ("GENDER" %in% sex_lookup) {
      sex_query <- paste0("&gender=", paste0(sex, collapse = ","))
    } else {
      sex_query <- ""
    }
  } else {
    sex_query <- ""
  }
  
  exclude_query <- ifelse(exclude_missing == TRUE,
                          "&ExcludeMissingValues=true",
                          ""
  )
  
  select_query <- ifelse(!is.null(select),
                         paste0(
                           "&select=",
                           paste0(
                             unique(c(toupper(select), "RECORD_COUNT")),
                             collapse = ","
                           )
                         ),
                         ""
  )
  
  measures_query <- ifelse(!is.null(measures),
                           paste0(
                             "&MEASURES=",
                             paste0(measures, collapse = ",")
                           ),
                           ""
  )
  
  query_id <- ifelse(!is.null(query_id), paste0("&queryid=", query_id), "")
  
  dots <- rlang::list2(...) ## eval the dots
  names(dots) <- toupper(names(dots))
  dots_vector <- c()
  
  for (i in seq_along(dots)) { # retrieve the dots
    dots_vector[i] <- ifelse(length(dots[[i]]) > 0,
                             paste0(
                               "&", toupper(names(dots[i])), "=",
                               paste0(dots[[i]], collapse = ",")
                             ),
                             ""
    )
  }
  
  dots_query <- paste0(dots_vector, collapse = "")
  
  if (!is.null(getOption("nomisr.API.key"))) {
    api_query <- paste0("&uid=", getOption("nomisr.API.key"))
    max_length <- 1000000
  } else {
    api_query <- ""
    max_length <- 25000
  }
  
  query <- paste0(
    id, ".data.csv?", dots_query, time_query, geography_query, sex_query,
    exclude_query, select_query, api_query, additional_query, measures_query,
    query_id
  )
  
  first_df <- nomisr:::nomis_get_data_util(query)
  
  names(first_df) <- toupper(names(first_df))
  
  if (as.numeric(first_df$RECORD_COUNT)[1] >= max_length) {
    # if amount available is over the limit of 15 total calls at a time
    # downloads the extra data and binds it all together in a tibble
    if (interactive() &&
        as.numeric(first_df$RECORD_COUNT)[1] >= (15 * max_length)) {
      # For more than 15 total requests at one time.
      message(
        "Warning: You are trying to acess more than ",
        paste0((15 * max_length)), " rows of data."
      )
      message("This may cause timeout and/or automatic rate limiting.")
      
      
      #######     JUST GET RID OF THE MENU THING    ########
      
      
      # if (menu(c("Yes", "No"),
      #          title = "Do you want to continue?"
      # ) == 2) {
      #   stop(call. = FALSE)
      # }
      
    }
    
    record_count <- first_df$RECORD_COUNT[1]
    
    seq_list <- seq(from = max_length, to = record_count, by = max_length)
    
    pages <- list()
    
    for (i in seq_along(seq_list)) {
      query2 <- paste0(
        query, "&recordOffset=",
        format(seq_list[i], scientific = FALSE)
      )
      # R can paste large numbers as scientific notation, which causes an error
      # format(seq_list[i], scientific = FALSE)) prevents that
      
      message("Retrieving additional pages ", i, " of ", length(seq_list))
      
      pages[[i]] <- nomisr:::nomis_get_data_util(query2)
    }
    
    df <- tibble::as_tibble(dplyr::bind_rows(first_df, pages))
  } else {
    df <- first_df
  }
  
  if (!is.null(select) & !("RECORD_COUNT" %in% toupper(select))) {
    df$RECORD_COUNT <- NULL
  }
  
  if (tidy == TRUE) {
    df <- nomisr:::nomis_tidy(df, tidy_style)
  }
  
  df
}
save(mynomis, file="mynomis_function.RData")





############################################################
###########     LOAD TOPIC SUMMARIES DATA      #############
############################################################

TS_meta = read_excel("Data/Lookups/Topic_summaries_metadata.xlsx", sheet = 2)[,-c(5,6)]
colnames(TS_meta) = c("TableName", "Domain", "Denominator", "Geography", "Census11")


TS_meta$Domain = str_sub(TS_meta$Domain, start=17)
TS_meta$Geography = sub("\\/.*", "", TS_meta$Geography)
TS_meta$TableName = gsub("\r\n", "",TS_meta$TableName)
TS_meta$lowercase = tolower(TS_meta$TableName)
tail(TS_meta)










#########################################################################
###########       RETRIEVE INFORMATION FROM NOMIS API       #############
#########################################################################

census_tables_nomis = as.data.frame(nomis_data_info())  %>% select(name.value, id)
census_tables = census_tables_nomis[grepl("TS", census_tables_nomis$name.value),] 


### Get GEOGRAPHY CODES
nomis_get_metadata(id=census_tables[1,"id"], concept = "GEOGRAPHY", type="type")
nomis_get_data(id=census_tables[1,"id"], geography="TYPE499") %>% distinct(GEOGRAPHY_NAME)



### get the lowest spatial resolutions of all tables
for(table_id in census_tables$id){
  
  # retrieve resolution metadata for the tahble
  table_spatial_resolution = as.data.frame(nomis_get_metadata(id=table_id, concept = "GEOGRAPHY", type="type"))
  
  #retrieve id of the resultion
  census_tables[census_tables$id==table_id, "spatial_resolution_code"] = table_spatial_resolution[1,"id"]
  
  #retrieve name of the resolution
  census_tables[census_tables$id==table_id,"spatial_resolution_label"] = table_spatial_resolution[1,"label.en"]
  
  census_tables[census_tables$id==table_id, "coverage"] = as.character((nomis_get_data(id=table_id, geography="TYPE499") %>% distinct(GEOGRAPHY_NAME))[1,1])
  
  census_tables[census_tables$id==table_id, "coverage_code"] = as.character((nomis_get_data(id=table_id, geography="TYPE499") %>% distinct(GEOGRAPHY_CODE))[1,1])
}



### retrieve TS encoding


census_tables$TS_code = str_sub(census_tables$name.value, end=5)
census_tables$name.value = str_sub(census_tables$name.value, start=9)
census_tables[census_tables$name.value=="Number of disabled people in the household", "name.value"] = "Number of disabled people in household"
census_tables$lowercase = tolower(census_tables$name.value)

census_tables[census_tables$id=="NM_2020_1", "TS_code"] = "TS007A"


census_tables$lowercase = gsub(" ", "", census_tables$lowercase)
TS_meta$lowercase = gsub(" ", "", TS_meta$lowercase)


tail(TS_meta)

census_tables_all = merge(TS_meta, census_tables, by="lowercase", all=T) %>% arrange(TS_code)







census_tables_all[!complete.cases(census_tables_all), ]
census_tables_all[!complete.cases(census_tables_all), ] %>% filter(coverage=="England and Wales", spatial_resolution_code=="TYPE150")



### keep only OA
census_tables_all = census_tables_all %>% filter(spatial_resolution_label=="2021 output areas", coverage=="England and Wales")  %>% 
  select(TS_code,  id,TableName,Domain,Denominator,spatial_resolution_code, Geography, coverage, Census11) 

census_tables_all[census_tables_all$id=="NM_2020_1","TableName"]= "Age by five-year age bands"
census_tables_all[census_tables_all$id=="NM_2020_1","Domain"] = "Demography and Migration"
census_tables_all[census_tables_all$id=="NM_2020_1","Denominator"] = "All usual residents"
census_tables_all[census_tables_all$id=="NM_2020_1","Geography"] = "OA"
census_tables_all[census_tables_all$id=="NM_2020_1","Census11"] = "KS102UK"


census_tables_all = census_tables_all %>% arrange(Domain,TS_code)


###########################################################################
###########       FIND TABLES NAMES FOR 2021 FROM 2011      ###############
###########################################################################

# numver of usual residents in households and communal establishements
census_tables_all[census_tables_all$TS_code=="TS001", "Census11"] = "QS101UK"
# legal partnership status
census_tables_all[census_tables_all$TS_code=="TS002", "Census11"] = "KS103UK"
# household composition  | 2011 = households
census_tables_all[census_tables_all$TS_code=="TS003", "Census11"] = "QS113UK"
# country of birth
census_tables_all[census_tables_all$TS_code=="TS004", "Census11"] = "QS203UK"
# passport held
census_tables_all[census_tables_all$TS_code=="TS005", "Census11"] = "KS205EW"
# population density
census_tables_all[census_tables_all$TS_code=="TS006", "Census11"] = "QS102UK"
# age by five-year age bands
census_tables_all[census_tables_all$TS_code=="TS007A", "Census11"] = "QS103UK"
# sex
census_tables_all[census_tables_all$TS_code=="TS008", "Census11"] = "QS104UK"
# Households by deprivation dimensions
census_tables_all[census_tables_all$TS_code=="TS011", "Census11"] = "QS119EW"
#Year of arrival in UK
census_tables_all[census_tables_all$TS_code=="TS015", "Census11"] = "QS801EW"
# Length of residence
census_tables_all[census_tables_all$TS_code=="TS016", "Census11"] = "QS803EW"
# Household size
census_tables_all[census_tables_all$TS_code=="TS017", "Census11"] = "QS406UK"
# Age of arrival in the UK
census_tables_all[census_tables_all$TS_code=="TS018", "Census11"] = "QS802EW"
# Migrant Indicator
census_tables_all[census_tables_all$TS_code=="TS019", "Census11"] = "UKMIG008"
# Number of non-UK short term residents by sex
census_tables_all[census_tables_all$TS_code=="TS020", "Census11"] = "ST1117EWLA"
# Ethnic group
census_tables_all[census_tables_all$TS_code=="TS021", "Census11"] = "KS201UK"
# Multiple ethnic group
census_tables_all[census_tables_all$TS_code=="TS023", "Census11"] = "QS202UK"
# Household language
census_tables_all[census_tables_all$TS_code=="TS025", "Census11"] = "KS206EW"
# multiple main lagunages in household
census_tables_all[census_tables_all$TS_code=="TS026", "Census11"] = NA
# National identity
census_tables_all[census_tables_all$TS_code=="TS027", "Census11"] = "KS202EW"
# Proficiency in English
census_tables_all[census_tables_all$TS_code=="TS029", "Census11"] = "QS205EW"
# Religion
census_tables_all[census_tables_all$TS_code=="TS030", "Census11"] = "KS209EW"
# Welsh language skills (detailed)
census_tables_all[census_tables_all$TS_code=="TS032", "Census11"] = "QS207WA"
# W
census_tables_all[census_tables_all$TS_code=="TS033", "Census11"] = "QS207WA"
census_tables_all[census_tables_all$TS_code=="TS034", "Census11"] = "QS207WA"
census_tables_all[census_tables_all$TS_code=="TS035", "Census11"] = "QS207WA"
census_tables_all[census_tables_all$TS_code=="TS036", "Census11"] = "QS207WA"

# General Health
census_tables_all[census_tables_all$TS_code=="TS037", "Census11"] = "QS302UK"
# Disability
census_tables_all[census_tables_all$TS_code=="TS038", "Census11"] = "QS303UK"
# Provision of unpaid care
census_tables_all[census_tables_all$TS_code=="TS039", "Census11"] = "QS301UK"
# Number of households
census_tables_all[census_tables_all$TS_code=="TS041", "Census11"] = "QS113EW"
# Accomodation type   | 2011 = households
census_tables_all[census_tables_all$TS_code=="TS044", "Census11"] = "QS402UK"
# Car or van availability
census_tables_all[census_tables_all$TS_code=="TS045", "Census11"] = "KS404UK"
# Central heating
census_tables_all[census_tables_all$TS_code=="TS046", "Census11"] = "QS415UK"
# Number of bedrooms
census_tables_all[census_tables_all$TS_code=="TS050", "Census11"] = "QS411EW"
# Number of rooms
census_tables_all[census_tables_all$TS_code=="TS051", "Census11"] = "QS407UK"
# Occupancy rating of bedrooms
census_tables_all[census_tables_all$TS_code=="TS052", "Census11"] = "QS412EW"
# Occupancy rating for rooms
census_tables_all[census_tables_all$TS_code=="TS053", "Census11"] = "QS408UK"
# Tenure
census_tables_all[census_tables_all$TS_code=="TS054", "Census11"] = "QS405UK"
# Purupose of second addres
census_tables_all[census_tables_all$TS_code=="TS055", "Census11"] = NA
# Second addres indicator
census_tables_all[census_tables_all$TS_code=="TS056", "Census11"] = "QS106EW"
# Distance travelled to work
census_tables_all[census_tables_all$TS_code=="TS058", "Census11"] = "QS702EW"
# Hours worked
census_tables_all[census_tables_all$TS_code=="TS059", "Census11"] = "QS604UK"
# Industry
census_tables_all[census_tables_all$TS_code=="TS061", "Census11"] = "QS701EW"
#NS-SeC
census_tables_all[census_tables_all$TS_code=="TS062", "Census11"] = "KS611UK"
# Occupation
census_tables_all[census_tables_all$TS_code=="TS063", "Census11"] = "KS608UK"
# Employment history
census_tables_all[census_tables_all$TS_code=="TS065", "Census11"] = "QS612UK"
# Economic activity Status
census_tables_all[census_tables_all$TS_code=="TS066", "Census11"] = "QS601UK"
# Highest level of qualification
census_tables_all[census_tables_all$TS_code=="TS067", "Census11"] = "QS501UK"
#Schoolchildren and full-time students
census_tables_all[census_tables_all$TS_code=="TS068", "Census11"] = "QS105UK"
census_tables_all[census_tables_all$TS_code=="TS075", "Census11"] = NA

census_tables_all[census_tables_all$VariableName=="Economically active and a full-time student", "Census11"] = "QS603UK"
census_tables_all[census_tables_all$VariableName=="Economically active and a full-time student:In employment", "Census11"] = "QS603UK"
census_tables_all[census_tables_all$VariableName=="Economically active and a full-time student: Unemployed", "Census11"] = "QS603UK"
census_tables_all[census_tables_all$VariableName=="Economically inactive: Student", "Census11"] = "QS603UK"






census_tables_all %>% filter(!grepl("UK", Census11))



#### Retrieve codes for Census 11 tables
census_2011_ids = census_tables_nomis %>% filter(grepl(paste(unique(census_tables_all$Census11), collapse = "|"),name.value)) %>% 
mutate(nomis_code = sub(" -.*", "", name.value), TableName11 = stringr::str_extract(name.value, "(?<=- ).+")) 


census_tables_all = merge(census_tables_all, census_2011_ids %>% select(-name.value) , by.x="Census11", by.y="nomis_code", all.x=T) %>%
  select(TS_code, id.x, TableName, Domain,Denominator,Census11, id.y, TableName11) %>% arrange(TS_code)
colnames(census_tables_all) = c("TS_code", "TableCode21", "TableName21", "Domain21", "Denominator21", "Census11", "TableCode11", "TableName11")






census_tables_all = census_tables_all %>% filter(!is.na(Census11), Census11!="ST1117EWLA")

### Save metadata
write.csv(census_tables_all,"Data/Lookups/Tables_metadata.csv",row.names = F)





##############################################################
###########       LIST OF 2021 VARIABLES      ###############
##############################################################

### this will be only at the 2021 level



variable_reference = NULL
### but not for LA 
for(table_id in census_tables_all$TableCode21){
  
  ### retrieve code for the coverage of the table (England & Wales or just Wales)
  coverage_code = as.character((nomis_get_data(id=table_id, geography="TYPE499") %>% 
                                  distinct(GEOGRAPHY_CODE))[1,1])
  
  ### retrieve table from NOMIS
  # if(table_id %in% c("NM_2029_1", "NM_2098_1_0")){
  #   reference = as.data.frame(mynomis(id = table_id, geography=coverage_code,measures=20100,))[,c( 20, 19, 26)]
  # } else {
  reference = as.data.frame(mynomis(id = table_id, geography=coverage_code,measures=20100,))[,c(14,13, 20)]
  # }  
  
  ### change names of the columns
  colnames(reference) = c("VariableName21", "CategoryCode21","MeasureName")
  
  #  ### change the name of the first table 
  #  variable_reference$TableName[variable_reference$TableName=="Number of usual residents in households and communal establishments"] = "Residence type"
  #  
  #  if(table_id=="NM_2021_1"){
  #    reference$TableName <- "Residency type"
  #  }
  
  ### insert the table ID
  reference$TableCode21 = table_id
  
  ### create a unique code for a variable (Table ID + Variable encoding)
  reference$TableVariableCode21 = paste0(table_id, "_", reference$CategoryCode21)
  
 #  ### append the information on the coverage of the data
 #  reference$coverage = as.character((nomis_get_data(id=table_id, geography="TYPE499") %>% distinct(GEOGRAPHY_NAME))[1,1])
 #  
 #  ### apend the code of the coverage
 #  reference$coverage_code = coverage_code
  
  ### bind the tables
  variable_reference = rbind(variable_reference,reference)
  
  
}


head(variable_reference)

  ### merge with the tables' metadata
variable_reference = merge(variable_reference, census_tables_all,  by="TableCode21",  all=T) %>% 
  select(TableVariableCode21,CategoryCode21,VariableName21, Denominator21, TS_code, TableName21, TableCode21, Domain21, TableCode11)





########  Correct inconsistencies in VariableNames and Denominators
unique(variable_reference$Denominator21)
variable_reference %>% filter(CategoryCode21==0) %>% distinct(VariableName21) %>% arrange(VariableName21)


## "All usual residents aged 16 years and over in households" = "All usual residents aged 16 years and over"
# "Total: All usual residents" = "Total: All Usual Residents"
# "Total: All usual residents aged 16 and over" = "Total: All usual residents aged 16 years and over"

variable_reference = variable_reference %>% mutate(VariableName21=recode(VariableName21, 
                                                                       'Total: All Usual Residents'="Total: All usual residents",
                                                                       'Total: All usual residents aged 16 and over' = "Total: All usual residents aged 16 years and over"),
                                                   Denominator21=recode(Denominator21,
                                                                      'All usual residents aged 16 years and over in households' = "All usual residents aged 16 years and over"))

#unclear
# "Total: All households"  ? == ? "Number of households"
# "All persons" ? == ? "Total: All usual residents"



#### some variables are not reported in the TS table provided by ONS
variable_reference %>% filter(CategoryCode21 > 8000)




###############################################################
############        NESTED CATEGORIES         ###############
###############################################################

# These are the tables and categories that are nested
unique((variable_reference %>% filter(CategoryCode21>999))$TS_code)
variable_reference %>% filter(TS_code %in% unique((variable_reference %>% filter(CategoryCode21>999))$TS_code))


#variable_reference %>% filter(TS_code %in% unique((variable_reference %>% filter(CategoryCode>999))$TS_code)) %>% select(-variable, -TableName)

###   Identify steps of nesting by looking at collons (:) - these separate categories 
variable_reference$nested = str_count(variable_reference$VariableName21, ":")

### Total categories have collons by default so change these
variable_reference$nested = ifelse(variable_reference$CategoryCode21==0, 0, variable_reference$nested)

### Variables in this table have collons by design so change these to 0
variable_reference[variable_reference$TS_code=="TS019", "nested"] = 0










########## this is not necessary..... 
####        for(ts in unique((variable_reference)$TS_code)){
####         
####         #ts =  "TS027"
####         #ts= "TS029"
####         # TS052
####         # TS053
####         # TS054
####         # TS065
####         # TS066
####         #  ts = "TS021"
####         
####         # subset by table
####         ts_tabel =  variable_reference %>% filter(TS_code == ts, CategoryCode<8000) %>% select(VariableName, nested,CategoryCode, TableVariableCode)
####         
####         #### Code of the primary variables | multiply by 10, because some variables have more than 10 variables at one level
####         ts_tabel[ts_tabel$nested==0, "myvar"] = ((0:(nrow( ts_tabel[ts_tabel$nested==0,])-1))*10)
####         
####         #### remove all white spaces from variables names (this helps with grepl())
####         ts_tabel$VariableName=gsub(" ", "", ts_tabel$VariableName)  
####         ts_tabel$VariableName=gsub("[[:punct:]]", "", ts_tabel$VariableName)  
####         
####         ts_tabel$or = 1:nrow(ts_tabel)
####         isnaor = ts_tabel[!is.na(ts_tabel$myvar), "or"]
####         
####         #  
####         #  ## place of a index
####         #  pl = which(isnaor==ts_tabel[ts_tabel$VariableName==primary,"or"])
####         #  
####         #  # get the next or
####         #  isnaor[pl + 1]
####         #  
####         #  ts_tabel$VariableName=gsub(")", "", ts_tabel$VariableName)  
####         
####         ### Skip this table - this is National identity for UK and due to its nature, does not work really well with the code
####         ### But it has no nested categories so no need to worry about that
####         if(ts %in% c("TS027", "TS052", "TS053", "TS065")){
####           next
####         }
####         
####         #### by non-nested (0 nested) variables
####         for(primary in ts_tabel[ts_tabel$nested==0, "VariableName"]){
####           
####           ts_tabel[ts_tabel$VariableName==primary,"or"]
####           ### select table for the primary variables (nested = 0)
####           ts0 = ts_tabel %>% filter(grepl(primary, VariableName)) %>% 
####             
####             filter(or>=ts_tabel[ts_tabel$VariableName==primary,"or"] )
####           
####           
####           ### run only if there are nested categories
####           if(nrow(ts0)!=1){
####             
####             #### by first order nested (1 nested)
####             ts1 = ts0 %>% filter(nested==1)
####             
####             ### create new encoding for level 1
####             ts1$myvar = as.numeric(paste0(ts0[ts0$nested==0, "myvar"],rownames(ts1)))*10
####             
####             ## change codes for the reference table 
####             for(code in unique(ts1$TableVariableCode)){
####               
####               ts0[ts0$TableVariableCode==code,"myvar"] = ts1[ts1$TableVariableCode==code, "myvar"]
####             }
####             
####             
####             ### By second order nesting (2 level)
####             if(any(ts0$nested==2)){
####               for(secondary in unique(ts1$VariableName)){
####                 
####                 #### select second nests with grepl()
####                 ts2 = ts0 %>% filter(grepl(secondary, VariableName), nested==2)
####                 
####                 ### if there are no nested categories then skip to the next level
####                 if(nrow(ts2)==0){
####                   next
####                 }
####                 
####                 ### create new encoding for level 2
####                 ts2$myvar = as.numeric(paste0(ts0[ts0$VariableName==secondary, "myvar"], rownames(ts2)))*10
####                 
####                 ### change encoding in the reference table
####                 for(code2 in unique(ts2$TableVariableCode)){
####                   ts0[ts0$TableVariableCode==code2,"myvar"] = ts2[ts2$TableVariableCode==code2, "myvar"]
####                   
####                 }
####                 
####                 ### By third order nesting (3 level)
####                 if(any(ts0$nested==3)){
####                   for(third in unique(ts2$VariableName)){
####                     
####                     ##### select third nest with grepl()
####                     ts3 = ts0 %>% filter(grepl(third, VariableName), nested==3)
####                     
####                     ### create new encoding for level 3
####                     ts3$myvar = as.numeric(paste0(ts0[ts0$VariableName==third, "myvar"], rownames(ts3)))*10
####                     
####                     #### change encoding in the reference table
####                     for(code3 in unique(ts3$TableVariableCode)){
####                       
####                       ts0[ts0$TableVariableCode==code3,"myvar"] = ts3[ts3$TableVariableCode==code3, "myvar"]
####                     }
####                   }
####                 }
####               }
####             }
####             
####             for(cd in unique(ts0$TableVariableCode)){
####               #### change encoding in the first table
####               ts_tabel[ts_tabel$TableVariableCode==cd, "myvar"] =   ts0[ts0$TableVariableCode==cd,"myvar"]
####             }
####           }
####         }
####         
####         ###### change encoding in the input table
####         for(id in unique(ts_tabel$TableVariableCode)){
####           variable_reference[variable_reference$TableVariableCode==id, "myvar"] =   ts_tabel[ts_tabel$TableVariableCode==id,"myvar"]
####         }
####        
####          
####        
####        ##### if some tables were skipped (because of the collons), then put their original encoding (this is also for the 9999 variables)
####        variable_reference$myvar=ifelse(is.na(variable_reference$myvar), variable_reference$CategoryCode, variable_reference$myvar)



#### #### modify newly created variables
#### variable_reference= variable_reference %>% mutate(MyTableVariableCode = paste0(TableCode, "_", myvar), MyCategoryCode=as.numeric(myvar)) %>% 
####   select(TableVariableCode, MyTableVariableCode, MyCategoryCode, VariableName, TableName, TableCode, TS_code, Domain, Denominator, nested, Census11)


#### change nesting level of all (those without collons where 0, same as the denominators)
variable_reference$nested = variable_reference$nested + 1

### set the denominator to 0 level
variable_reference$nested = ifelse(variable_reference$CategoryCode21==0, 0, variable_reference$nested)

####    change the problematic categories to NA
variable_reference$nested = ifelse(variable_reference$CategoryCode21>9990 & variable_reference$CategoryCode21<10000, NA, variable_reference$nested)

variable_reference_2021 = variable_reference %>% arrange(TS_code)
write.csv(variable_reference_2021,"Data/Lookups/Variable_reference_2021.csv",row.names = F)







##############################################################
###########       LIST OF 2011 VARIABLES      ###############
##############################################################


### These are the tables available for England and Wales 2021, and were also available for whole UK 2011
### This is needed as we need to simulate Scotland and Northern Ireland


uk_census_tables_2011 = census_tables_all %>% filter(grepl("UK", Census11) | TableName21 %in% c("Religion","Proficiency in English"))





# m = merge(uk_variable_reference, uk_codes, by="Census11", all=T)
# m = m %>% select(TS_code, TableCode, TableName, Domain, MyTableVariableCode, MyCategoryCode, TableVariableCode, VariableName, Denominator, nested, Census11,id, name.value ) %>% 
#   arrange(TS_code)
# colnames(m) = c("TS_code", "TableCode21", "TableName21", "Domain", "MyTableVariableCode21", "MyCategoryCode21", "TableVariableCode21", "VariableName21", "Denominator", 
#                 "nested", "Census11", "TableCode11", "TableName11")



variable_reference_2011 = NULL
for(table_id in unique(uk_census_tables_2011$TableCode11)){
  
  ### retrieve code for the coverage of the table (England & Wales or just Wales)
  coverage_code = as.character((nomis_get_data(id=table_id, geography="TYPE499") %>% 
                                  distinct(GEOGRAPHY_CODE))[1,1])
  
  reference = as.data.frame(mynomis(id = table_id, geography=coverage_code,measures=20100,))[,c(14,13, 20)]
  
  if(table_id=="NM_1280_1"){
    reference = as.data.frame(mynomis(id = table_id, geography=coverage_code,measures=20100)) 
    colnames(reference)[c(14,20)] = c("sex", "age")
    reference = reference %>% filter(sex=="All persons", age=="All categories: Age")
    reference = reference[,c(26,25,32)]
  }
  
  if(table_id %in% c("NM_526_1","NM_616_1")){
    reference = as.data.frame(mynomis(id = table_id, geography=coverage_code,measures=20100, rural_urban="0"))
    colnames(reference)[14] = "rural_urban_name"
    reference = reference %>% filter(rural_urban_name=="Total")
    reference = reference[, c(20, 19,26)] 
  }
  
  
  ### change names of the columns
  colnames(reference) = c("VariableName11", "CategoryCode11","MeasureName")
  
  
  
  ### insert the table ID
  reference$TableCode11 = table_id
  
  ### create a unique code for a variable (Table ID + Variable encoding)
  reference$TableVariableCode11 = paste0(table_id, "_", reference$CategoryCode11)
  
 # ### append the information on the coverage of the data
 # reference$coverage = as.character((nomis_get_data(id=table_id, geography="TYPE499") %>% distinct(GEOGRAPHY_NAME))[1,1])
 # 
 # ### apend the code of the coverage
 # reference$coverage_code = coverage_code
  
  ### bind the tables
  variable_reference_2011 = rbind(variable_reference_2011,reference)
  
  
}


variable_reference_2011 = merge(variable_reference_2011, census_tables_all, by="TableCode11", allx=T) %>% 
  select(TableVariableCode11, CategoryCode11, VariableName11, TableName11, TableCode11,Denominator21, TS_code, Domain21, TableName21, TableCode21 ) %>% 
  arrange(TS_code)


write.csv(variable_reference_2011, "Data/Lookups/Variable_reference_2011.csv", row.names = F)

variable_reference_2011 = read.csv("Data/Lookups/Variable_reference_2011.csv")
variable_reference_2011$order = 1:nrow(variable_reference_2011)





########################################################################    
############        MATCH UP VARIABLES BETWEEN CENSUS       ############
########################################################################    


#####################################################################################################################  
#############                                           !!!!!!!                                         #############   
#############    !!!!    MATCH UP IS CREATED BASED ON THE Variable_reference_2011.csv      !!!!!!!      #############
#############                                           !!!!!!!                                         #############  
#####################################################################################################################   









Matched_variables = read.csv("Data/Lookups/Matching_variables_11_21.csv") 

Matched_variables[Matched_variables==""] = NA
colnames(Matched_variables) = c("TableVariableCode11", "CategoryCode11", "VariableName11", "TableName11", "TableCode11", "Denominator21", "TS_code", 
                         "Domain21","TableName21", "TableCode21", "TableVariableCode21", "Agg1", "Agg2")


# ### repairing
# m = merge(variable_reference_2011 %>% mutate(order = 1:nrow(variable_reference_2011)), 
#           Matched_variables %>% select(TableVariableCode11, TableVariableCode21, Agg1, Agg2), by="TableVariableCode11", all.x=T) %>% 
#   arrange(TS_code, order) %>% select(-order)
# write.csv(m, "~/Desktop/m.csv", row.names=F)
# 





Matched_variables = merge(Matched_variables, variable_reference_2021 %>% select(TableVariableCode21, VariableName21), 
                          by="TableVariableCode21", all.x=T) 

Matched_variables = merge(Matched_variables, variable_reference_2011 %>% select(TableVariableCode11, order), 
                          by="TableVariableCode11", all.x=T) %>% 
  arrange(order) %>% select(-order)%>% filter(!is.na(TableVariableCode21))






 #duplicating variables that were merged from 2011 to 2021
# for(i in 1:nrow(Matched_variables)){
#   #  print(codes11_21[i,])
#   if(!is.na(Matched_variables[i,"Agg1"])){
#     t2 = Matched_variables[i,]
#     t2$TableVariableCode21 = t2$Agg1
#     Matched_variables=rbind(Matched_variables, t2)
#   }
#   
#   if(!is.na(Matched_variables[i,"Agg2"])){
#     t2 = Matched_variables[i,]
#     t2$TableVariableCode21 = t2$Agg2
#     Matched_variables=rbind(Matched_variables, t2)
#   }
# }

Matched_variables = Matched_variables %>% select(-Agg1, -Agg2)

### Variables that were disaggregated
print(Matched_variables %>% group_by(TableVariableCode11) %>% mutate(n=n()) %>% filter(n>1),n=15)

### Variables that were aggregated
print(Matched_variables %>% group_by(TableVariableCode21) %>% mutate(n=n()) %>% filter(n>1), n=100)


Matched_variables %>% group_by(TableVariableCode11) %>% mutate(n=n()) %>% filter(n>1) %>% ungroup() %>% 
  distinct(TableCode11, .keep_all = T) %>% select(TableCode11, TableCode21, TableName21)

Matched_variables %>% group_by(TableVariableCode21) %>% mutate(n=n())  %>% filter(n>1) %>% ungroup() %>%
  distinct(TableCode11, .keep_all = T) %>% select(TableCode11, TableCode21, TableName21)



### change Proficiency in English
Matched_variables %>% filter(TableName21=="Proficiency in English")

Matched_variables[Matched_variables$TableVariableCode11=="NM_526_1_1","VariableName11"] ="Main language or speaks very well"
Matched_variables[Matched_variables$TableVariableCode11=="NM_526_1_1","VariableName21"] ="Main language or speaks very well"

Matched_variables[Matched_variables$TableVariableCode11=="NM_526_1_2","VariableName11"] ="Main language or speaks very well"
Matched_variables[Matched_variables$TableVariableCode11=="NM_526_1_2","VariableName21"] ="Main language or speaks very well"

Matched_variables[Matched_variables$TableVariableCode11=="NM_526_1_3","VariableName11"] ="Speaks English well"
Matched_variables[Matched_variables$TableVariableCode11=="NM_526_1_3","VariableName21"] ="Speaks English well"

Matched_variables[Matched_variables$TableVariableCode11=="NM_526_1_4","VariableName11"] ="Cannot speak English well"
Matched_variables[Matched_variables$TableVariableCode11=="NM_526_1_4","VariableName21"] ="Cannot speak English well"

Matched_variables[Matched_variables$TableVariableCode11=="NM_526_1_5","VariableName11"] ="Cannot speak English"
Matched_variables[Matched_variables$TableVariableCode11=="NM_526_1_5","VariableName21"] ="Cannot speak English"


### Variables that were disaggregated
print(Matched_variables %>% group_by(TableVariableCode11) %>% mutate(n=n()) %>% filter(n>1),n=15)

### Variables that were aggregated
print(Matched_variables %>% group_by(TableVariableCode21) %>% mutate(n=n()) %>% filter(n>1), n=100)


Matched_variables %>% group_by(TableVariableCode11) %>% mutate(n=n()) %>% filter(n>1) %>% ungroup() %>% 
  distinct(TableCode11, .keep_all = T) %>% select(TableCode11, TableCode21, TableName21)

Matched_variables %>% group_by(TableVariableCode21) %>% mutate(n=n())  %>% filter(n>1) %>% ungroup() %>%
  distinct(TableCode11, .keep_all = T) %>% select(TableCode11, TableCode21, TableName21)


Matched_variables[Matched_variables$TS_code=="TS001", "TableName21"] = "Residency type"
Matched_variables[Matched_variables$TS_code=="TS007A", "TableName21"] = "Age structure"


write.csv(Matched_variables, "Data/Lookups/Final_codes_11_21.csv", row.names = F) 






# not sure if needed 

### all variables

variable_reference_2021 = read.csv("Data/Lookups/Variable_reference_2021.csv")
variable_reference_2021$order = 1:nrow(variable_reference_2021)

codes_names = rbind(Matched_variables %>% select(TableVariableCode11, VariableName11, TableCode11,TableName21, TS_code, CategoryCode11)%>% 
                      rename(Code=TableVariableCode11, Name=VariableName11, TableCode=TableCode11), 
                    Matched_variables %>% select(TableVariableCode21, VariableName21, TableCode21,TableName21,TS_code,CategoryCode11) %>% 
                      rename(Code=TableVariableCode21,Name=VariableName21, TableCode=TableCode21)) %>% 
  distinct(Code, .keep_all=T) 

codes_names = merge(codes_names, variable_reference_2011 %>% select(TableVariableCode11, order), 
                    by.x="Code", by.y="TableVariableCode11", all.x=T)
codes_names = merge(codes_names, variable_reference_2021 %>% select(TableVariableCode21, order), 
                    by.x="Code", by.y="TableVariableCode21", all.x=T)

codes_names =   codes_names %>% mutate(order = ifelse(is.na(order.x), order.y, order.x)) %>% arrange(TS_code, order) %>% 
  select(Code, Name, TableCode, TS_code, TableName21, CategoryCode11)





write.csv(codes_names, "Data/Lookups/All_variable_codes.csv", row.names = F)



