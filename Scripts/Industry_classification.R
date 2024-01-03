




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
library(RColorBrewer)

#### LOAD SHAPEFILES
shp_2011_simple = readRDS("Data/Objects/shp_2011_simple_30.RDS")
hybrid_shp_simple = readRDS("Data/Objects/hybrid_shp_simple_30.RDS")
shp_2021_simple = readRDS("Data/Objects/shp_2021_simple_30.RDS")


Industry_8 = readRDS("Data/Clustering/OAC_8_Supergroups_clustering_Industry_2k.RDS")
Industry_7  = readRDS("Data/Clustering/OAC_7_Supergroups_clustering_Industry_1k.RDS")

Occupation_8 = readRDS("Data/Clustering/OAC_Supergroups_clustering.RDS")
Occupation_7 = readRDS("Data/Clustering/OAC_7_Supergroups_clustering.RDS")

OAC_variables =read.csv("Data/Lookups/OAC_variables.csv")
data = read.csv("Data/Clean/Final_variables/Aged_Scotland.csv") %>% select(-country) %>% select(Geography_Code)



Clusters = cbind(data, Occupation_8$cluster, Occupation_7$cluster, Industry_8$cluster, Industry_7$cluster)[,-1]
colnames(Clusters)= c("Occupation_8", "Occupation_7", "Industry_8", "Industry_7")

Clusters = Clusters %>% mutate(Occupation_8=recode(Occupation_8,
                                                  '1' = '8', 
                                                  '2' = '5', 
                                                  '3' = '3', 
                                                  '4' = '1', 
                                                  '5' = '4', 
                                                  '6' = '7', 
                                                  '7' = '6', 
                                                  '8' = '2'),
                               
                              Occupation_7=recode(Occupation_7,
                                                  '1' = '6', 
                                                  '2' = '4', 
                                                  '3' = '3', 
                                                  '4' = '5', 
                                                  '5' = '7', 
                                                  '6' = '1', 
                                                  '7' = '2'),
                              
               
                               
                              Industry_7=recode(Industry_7,
                                                 '1' = '7',
                                                 '3' = '6',
                                                 '4' = '1', 
                                                 '5' = '3', 
                                                 '6' = '2',
                                                 '7' = '4',
                                                 '2' = '5'),
                               
                               Industry_8=recode(Industry_8,
                                                  '1' = '2',
                                                  '2' = '3',
                                                  '3' = '8',
                                                  '4' = '1', 
                                                  '5' = '7', 
                                                  '6' = '4',
                                                  '7' = '5',
                                                  '8' = '6'), )



### OCCUPATION 8 VS INDUSTRY 8
round(prop.table(table(Clusters$Industry_8,Clusters$Occupation_8, dnn = c("Inudstry_8", "Occupation_8")), margin=1),4)*100

### OCCUPATION 7 VS INDUSTRY 7
round(prop.table(table( Clusters$Industry_7,Clusters$Occupation_7, dnn = c("Inudstry_7", "Occupation_7")), margin=1),4)*100

### OCCUPATION 8 VS INDUSTRY 7
round(prop.table(table( Clusters$Industry_7,Clusters$Occupation_8, dnn = c("Inudstry_7", "Occupation_8")), margin=1),4)*100

### OCCUPATION 7 VS INDUSTRY 8
round(prop.table(table( Clusters$Industry_8,Clusters$Occupation_7, dnn = c("Inudstry_8", "Occupation_7")), margin=1),4)*100


### OCCUPATION 7 VS OCCUPATION 8
round(prop.table(table( Clusters$Occupation_8,Clusters$Occupation_7, dnn = c("Occupation_8", "Occupation_7")), margin=1),4)*100


### OCCUPATION 7 VS OCCUPATION 8
round(prop.table(table( Clusters$Industry_8,Clusters$Industry_7, dnn = c("Industry_8", "Industry_7")), margin=1),4)*100


colors = brewer.pal(8, "Set1")
OAC_colors = c('1' = colors[3], '2' = "#dbb6a2", '3' = colors[8], '4' = colors[2], '5' = colors[6], '6' = colors[5], '7'=colors[4], '8' = colors[1])

OAC_colors_7 = c('1' = colors[3],  '3' = colors[8], '4' = colors[2], '5' = colors[6], '6' = colors[5], '7'=colors[4], '2' = colors[1])

Clusters$Geography_Code = rownames(Clusters)

Aged_shp = merge(hybrid_shp_simple, Clusters, by="Geography_Code") 



tmap_save(tm_shape(Aged_shp) + 
            tm_fill("Industry_7",  alpha = 0.9, style = "cat", palette=OAC_colors_7, title="Supergroups") +
            tm_layout(frame=F, legend.position = c(0.02, 0.81), legend.text.size = 0.8, legend.width=1.2), 
          filename = paste0("Maps/Clustering/Industry_7_OAC.png"), dpi = 500)





