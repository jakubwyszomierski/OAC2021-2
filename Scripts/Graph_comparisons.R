### Create Maps


setwd("~/Desktop/PhD/GIT/OAC2021")

library(nomisr)
library(dplyr)
library(sf)
library(tmap)
library(tidyverse)
library(readxl)
library(Hmisc)
library(tmap)
library(ggplot2)


#### LOAD SHAPEFILES

shp_2011_simple = readRDS("Data/Objects/shp_2011_simple_30.RDS")
hybrid_shp_simple = readRDS("Data/Objects/hybrid_shp_simple_30.RDS")
shp_2021_simple = readRDS("Data/Objects/shp_2021_simple_30.RDS")

# MSOA21_boundaries = sf::st_read("~/Desktop/PhD/shapefiles/boundaries_2021/MSOA_2021/MSOA_2021_EW_BGC.shp")
# MSOA11_boundaries = sf::st_read("~/Desktop/MSOA_2011_GEN/MSOA_(Dec_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shp")
# LAD_to_country = read.csv("~/Desktop/PhD/shapefiles/LAD_to_Country_21.csv") %>% filter(CTRY21NM=="England")
# LAD21_boundaries = sf::st_read("~/Desktop/PhD/shapefiles/boundaries_2021/LAD_2021/Local_Authority_Districts_(December_2021)_UK_BGC.shp") %>% 
#   filter(LAD21CD %in% unique(LAD_to_country$LAD21CD))
# 
# OA_SA_2011 = sf::st_read("~/Desktop/PhD/shapefiles/OA_SA_2011/infuse_oa_lyr_2011.shp")

OAC_variables = read.csv("Data/Lookups/OAC_variables.csv")

list.files("Data/Clean/")
#### LOAD CENSUS

# Census_2011 = read.csv("~/Desktop/PhD/GIT/OAC2021/Data/Clean/Census_2011.csv")
# Census_2021 = read.csv("~/Desktop/PhD/GIT/OAC2021/Data/Clean/Census_2021.csv")

Census_2011_perc = read.csv("~/Desktop/PhD/GIT/OAC2021/Data/Clean/Percentages/Census_2011_common_var_prop_perc.csv")
Census_2021_perc = read.csv("~/Desktop/PhD/GIT/OAC2021/Data/Clean/Percentages/Census_2021_common_var_prop_perc.csv")
Hybrid_perc = read.csv("~/Desktop/PhD/GIT/OAC2021/Data/Clean/Percentages/hybrid_UK_2021_prop_perc.csv")
Aged_Scotland_perc = read.csv("~/Desktop/PhD/GIT/OAC2021/Data/Clean/Percentages/hybrid_UK_2021_aged_Scotland_prop_perc.csv")


Census_2011_IHS_Range = read.csv("~/Desktop/PhD/GIT/OAC2021/Data/Clean/Transformed/Census_2011_common_var_prop_perc_IHS_range.csv")
Census_2021_IHS_Range = read.csv("~/Desktop/PhD/GIT/OAC2021/Data/Clean/Transformed/Census_2021_common_var_prop_perc_IHS_range.csv")
Hybrid_OAC_Range = read.csv("~/Desktop/PhD/GIT/OAC2021/Data/Clean/Transformed/hybrid_UK_2021_prop_perc_IHS_range.csv")
Aged_Scotland_Range = read.csv("~/Desktop/PhD/GIT/OAC2021/Data/Clean/Transformed/hybrid_UK_2021_aged_Scotland_prop_perc_IHS_range.csv")



final_codes = read.csv("Data/Lookups/Final_codes_11_21.csv")
final_codes = read.csv("Data/Lookups/All_variable_codes.csv")
head(final_codes)
#final_codes = read.csv("~/Desktop/PhD/GIT/OAC2021/Data/Final_Codes_Names.csv")





######### Merge datasets with shapefiles
Census_2011_sf = merge(shp_2011_simple, Census_2011_perc, by="Geography_Code")
Aged_Scotland_sf = merge(hybrid_shp_simple, Aged_Scotland_perc, by="Geography_Code")




Census_2011_trs_sf = merge(shp_2011_simple, Census_2011_IHS_Range , by="Geography_Code")
Aged_Scotland_trs_sf = merge(hybrid_shp_simple, Aged_Scotland_Range, by="Geography_Code")






##################################################################
########        PRODUCE MAPS COMPARING VARIABLES      ############
##################################################################




for(var in colnames(Census_2011_perc)[-1]){
 

  
  if(var %nin% colnames(Census_2011_perc)){
    print(paste(var, "MISSING"))
    next
  }
  cat("\r", var, " --- ", paste(match(var, colnames(Census_2011_perc)[-1])), "out of", length(colnames(Census_2011_perc)[-1]))
  flush.console()

  
  # Get the name of a variable
  var_name = final_codes[final_codes$Code==var,"Name"][1]
  
  # Get the name of a table for a variable
  table_name =  final_codes[final_codes$Code==var,"TableName21"][1]
  table_name = gsub(" ", "_", table_name)
  
#  final_codes %>% distinct(TableName21, .keep_all=T)
#  if(table_name %in% c("Central heating", "Number of rooms", "Schoolchildren and full-time students", "General health", "Economic activity status")){
#    next
#  }

  if(var %nin% OAC_variables$Code){
    next
  }
  if(var=="SIR"){
    next
  }
  
  # Create folders if they don't exist
  if(table_name %nin%  list.files("~/Desktop/PhD/GIT/OAC2021/Maps/Comparing_Percents/")){
    
    #   dir.create(paste0("~/Desktop/PhD/GIT/OAC2021/Maps/Change/", table_name))
    #   dir.create(paste0("~/Desktop/PhD/GIT/OAC2021/Maps/Change_Points/", table_name))
    dir.create(paste0("~/Desktop/PhD/GIT/OAC2021/Maps/Comparing_Percents/", table_name))
    dir.create(paste0("~/Desktop/PhD/GIT/OAC2021/Maps/Comparing_Ranges/", table_name))
  }
  

  
  if(is.na(var_name)){
    var_name = var
  }
  
  
  if(paste0(gsub(" ", "_", var_name), ".png") %in% list.files(paste0("Maps/Comparing_Percents/", table_name, "/"))){
    
  }
  ### Two maps comparing percentages
  ### Same scale, first change proportions to percentages
  
  
  br1 = floor(min(c(Census_2011_perc[,var], Aged_Scotland_perc[,var])))
  br2 = ceiling(max(c(Census_2011_perc[,var], Aged_Scotland_perc[,var])))
  
  #Census_2011_sf_subset = Census_2011_sf %>% select(MSOA11CD, var, geometry)
  #Census_2021_sf_subset = Census_2021_sf %>% select(MSOA21CD, var, geometry)
  
  ### Create maps
  start_time=Sys.time()
  tm_2011 = tm_shape(Census_2011_sf %>% select(Geography_Code, var)) + 
   tm_fill(var, title="", legend.hist = T)+#, breaks = seq(br1,br2, (br2-br1)/5)) + 
#    tm_borders(col = var )+ 
  #  tm_polygons(var, title="", legend.hist=T, breaks = seq(br1,br2, (br2-br1)/5), border.col = NA) + 
  #  tm_shape(LAD21_boundaries) + tm_borders(lwd=0.2)+
    # tm_borders(lwd = 0.025) + 
    tm_layout(frame=F,
              outer.margins = c(0.01,0.01,0.01,0.01),
              inner.margins = c(0.01,0.01,0.01,0.01),
              legend.text.size = 0.7,
              legend.title.size = 0.25,
              title = var_name, 
              legend.outside = T,
              legend.outside.position = "bottom",
              legend.stack = "horizontal",
              legend.hist.width = 1,
              legend.hist.height = 0.4)
  
  tm_2021 = tm_shape(Aged_Scotland_sf %>% select(Geography_Code, var)) + 
    tm_fill(var, title="", legend.hist = T)+#, breaks=seq(br1,br2, (br2-br1)/5)) +
  #  tm_shape(LAD21_boundaries) + tm_borders(lwd=0.2)+
    #   tm_borders(lwd = 0.025)  + 
    tm_layout(frame=F,
              outer.margins = c(0.01,0.01,0.01,0.01),
              inner.margins = c(0.01,0.01,0.01,0.01),
              legend.text.size = 0.8,
              legend.title.size = 0.25,
              title = var_name, 
              legend.outside = T,
              legend.outside.position = "bottom",
              legend.stack = "horizontal",
              legend.hist.width = 1,
              legend.hist.height = 0.4)
  

#  arr =  tmap_arrange(tm_2011, tm_2021, outer.margins = c(0.001,0.001,0.001,0.001), ncol=2) 
  
  
 # tmap_save(arr, paste0("~/Desktop/PhD/GIT/OAC2021/Maps/Comparing_Percents/", 
 #                       table_name, "/", gsub(" ", "_", var_name), ".png"), dpi=1000)
 # 
  Sys.time() - start_time
  
  ######    COMPARE VARIABLE RANGES
  
  
  
  tm_2011_trs = tm_shape(Census_2011_trs_sf) +
    tm_fill(var, title="", legend.hist = T) + 
 #   tm_shape(LAD21_boundaries) + tm_borders(lwd=0.2)+
    # tm_borders(lwd = 0.025) + 
    tm_layout(frame=F,
              outer.margins = c(0.01,0.01,0.01,0.01),
              inner.margins = c(0.01,0.01,0.01,0.01),
              legend.text.size = 0.7,
              legend.title.size = 0.25,
              title = var_name, 
              legend.outside = T,
              legend.outside.position = "bottom",
              legend.stack = "horizontal",
              legend.hist.width = 1,
              legend.hist.height = 0.4)
  
  tm_2021_trs  = tm_shape(Aged_Scotland_trs_sf) + 
    tm_fill(var, title="", legend.hist = T) + 
 #   tm_shape(LAD21_boundaries) + tm_borders(lwd=0.2)+
    # tm_borders(lwd = 0.025) + 
    tm_layout(frame=F,
              outer.margins = c(0.01,0.01,0.01,0.01),
              inner.margins = c(0.01,0.01,0.01,0.01),
              legend.text.size = 0.7,
              legend.title.size = 0.25,
              title = var_name, 
              legend.outside = T,
              legend.outside.position = "bottom",
              legend.stack = "horizontal",
              legend.hist.width = 1,
              legend.hist.height = 0.4)
  
  arr_trs =  tmap_arrange(tm_2011_trs , tm_2021_trs,
                          outer.margins = c(0.001,0.001,0.001,0.001), ncol=2 ) 
  # tmap_save(arr_trs, "~/Desktop/test_trs.png", dpi=300)
  tmap_save(arr_trs, paste0("~/Desktop/PhD/GIT/OAC2021/Maps/Comparing_Ranges/", 
                            table_name, "/", gsub(" ", "_", var_name), ".png"), dpi=800)
  
  
  
  #######################################################################
  ##########    COMPARE CHANGES ACROSS THE YEARS (ONE MAP)      #########
  #######################################################################
  
  ### This might be useful to evaluate change in the proportion of variables, 
  ### but the problem arises with the correspondence of spatial areas across the years - 
  ### some areas were aggregated or dissagregated. Might do it if have more time
  
  #   #### Percentage change and change of percentages point
  #
  #   MSOA_merge = merge(MSOA21_boundaries, Census_2021 %>% select(Geography_Code, var) %>% 
  #                        rename(var_2011=var), 
  #                      by.x="MSOA21CD", by.y="Geography_Code")
  #   MSOA_merge = merge(MSOA_merge, Census_2011 %>% select(Geography_Code, var) %>% 
  #                        rename(var_2021=var), 
  #                      by.x="MSOA21CD", by.y="Geography_Code")
  #   
  #   MSOA_merge$change = round(( MSOA_merge$var_2021 - MSOA_merge$var_2011 )/ MSOA_merge$var_2011,2)
  #   MSOA_merge$change_points =  MSOA_merge$var_2021 - MSOA_merge$var_2011 
  #   
  #   
  #   tm_change = tm_shape(MSOA_merge) + tm_fill("change", title="") +tm_borders(lwd = 0.025)  + 
  #     tm_layout( frame=F,legend.text.size =1, title=var_name,legend.title.size = 0.25 )
  #   
  #   tm_change_points = tm_shape(MSOA_merge) + tm_fill("change_points", title="", alpha = 0.5) +tm_borders(lwd = 0.025)  + 
  #     tm_layout( frame=F,legend.text.size =1, title=var_name,legend.title.size = 0.25 )
  #   
  #   
  #   tmap_save(tm_change, paste0("~/Desktop/Maps/Change/",  table_name, "/",gsub(" ", "_", var_name), ".png"))
  #   tmap_save(tm_change_points, paste0("~/Desktop/Maps/Change_Points/", table_name, "/", gsub(" ", "_", var_name), ".png"))
  #
  
  
}





