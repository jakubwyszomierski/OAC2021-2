### OAC clustering


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


OAC_variables =read.csv("Data/Lookups/OAC_variables.csv")
Aged_Scotland_OAC = read.csv("Data/Clean/Final_variables/Aged_Scotland.csv") %>% select(-country)   


################################################
###########       SUPERGROUPS       ############
################################################



Input_data = Aged_Scotland_OAC
rownames(Input_data) = Input_data$Geography_Code
Input_data = Input_data[, OAC_variables$encoding]

fit=NA
OAC_Supergroups_clustering=ls()

for (i in 1:2000){
    
    cat("\r", i)
    flush.console()
    
    clustering <- kmeans(x=Input_data, centers=8, iter.max=1000000, nstart=1)
    
    fit[i] <- clustering$tot.withinss
    
    if (fit[i] < min(fit[1:(i-1)])){
      OAC_Supergroups_clustering <- clustering
    }
}
  
# saveRDS(OAC_Supergroups_clustering, "Data/Clustering/OAC_Supergroups_clustering.RDS")
OAC_Supergroups_clustering = readRDS("Data/Clustering/OAC_Supergroups_clustering.RDS")


Aged_Scotland_clusters = cbind(Aged_Scotland_OAC, OAC_Supergroups_clustering$cluster)
colnames(Aged_Scotland_clusters)[ncol(Aged_Scotland_clusters)] = "Supergroup8"


### RECLASSIFY THE GROUP ALLOCATION
Aged_Scotland_clusters = Aged_Scotland_clusters %>% mutate(Supergroup8=recode(Supergroup8,
                                                                             '1' = '8', 
                                                                             '2' = '5', 
                                                                             '3' = '3', 
                                                                             '4' = '1', 
                                                                             '5' = '4', 
                                                                             '6' = '7', 
                                                                             '7' = '6', 
                                                                             '8' = '2'))




Supergroups_centers =  as.data.frame(OAC_Supergroups_clustering$centers)
Supergroups_centers$id = rownames(Supergroups_centers)

Supergroups_centers = Supergroups_centers %>% mutate(Supergroup8=recode(id,'1' = '8', 
                                                                       '2' = '5', 
                                                                       '3' = '3', 
                                                                       '4' = '1', 
                                                                       '5' = '4', 
                                                                       '6' = '7', 
                                                                       '7' = '6', 
                                                                       '8' = '2')) %>%
  select(-id)  %>% arrange(Supergroup8)



write.csv(Supergroups_centers, "Data/Clustering/OAC_centroids.csv", row.names = F)


################################################
###########       SUPERGROUPS       ############
################################################






### DEFINE THE NUMBER (PREFFERED AND ALTERNATIVE) OF GROUPS FOR EACH SUPERGOUPS 
Groups_per_super = c(3, 3, 3, 3, 2, 3, 2, 2)



#### PERFORM CLUSTERING
lis=list()
  
  for(Supergroup in 1:8){
    
    # Get the number of Groups for SuperGroup
    n_Groups = Groups_per_super[Supergroup]
    clusters <- list()
    fit <- NA
    
    ### GET THE DATA FOR SPECIFIC SUPEGROUP
    dat = Aged_Scotland_clusters %>% filter(Supergroup8==Supergroup) %>% select(-Geography_Code, -Supergroup8)
    
    for(i in 1:10){
      
      cat("\r", "Running ",Supergroup," Supergroup |  Iteration: ", i, sep="")
      flush.console()
      
      clustering <- kmeans(x=dat, centers=n_Groups, iter.max=1000000, nstart=1)
      
      fit[i] <- clustering$tot.withinss
      if (fit[i] <= min(fit[1:(i-1)])){
        clusters <- clustering
        }
    }
    
    ### STORE IN A LIST
    lis[[Supergroup]]=clusters
    
  }

OAC_Groups_clustering <-  lis
saveRDS(OAC_Groups_clustering, "Data/Clustering/OAC_Groups_clustering.RDS")

OAC_Groups_clustering = readRDS("Data/Clustering/OAC_Groups_clustering.RDS")



Aged_Scotland_Groups = Aged_Scotland_clusters 


### RETRIEVE ASSIGNMENTS OF THE GROUPS

Groups_bind = NULL
for(list_element in 1:length(OAC_Groups_clustering)){

    # RETRIEVE THE SOLUTION FOR THE SUBGROUP
    element = OAC_Groups_clustering[[list_element]]
    df = data.frame(Supergroup = list_element, cluster = element$cluster)
    df$Geography_Code = rownames(df)
    
    # BIND INTO ONE DATAFRAME
    Groups_bind = rbind(Groups_bind, df)
}
  

Groups_bind
  # RECODE GROUPS 
Groups_bind = Groups_bind %>% mutate(cluster = recode(cluster,'1' ="a", '2' = "b", '3' = "c", '4' = "d", '5'= "e"))
colnames(Groups_bind)[2] = "Group"

### BIND WITH THE PARENT DATA
Aged_Scotland_clusters = merge(Aged_Scotland_clusters, Groups_bind %>% mutate(Group = paste0(Supergroup, Group)) %>% select(-Supergroup) , by="Geography_Code", all=T)
  
  







#########################################################################
#############          SUBGROUPS FOR AGED SCOTLAND          #############
#########################################################################


Subgroups  = c(2, 2,2, ## 1a, 1b,1c
                       3, 2, 2, ## 2a, 2b, 2c
                       4, 2, 2, ## 3a, 3b, 3c, 
                       3, 4, 2, ## 4a, 4b, 4c,
                       3, 2, ## 5a, 5b
                       3,3,2, ## 6a, 6b, 6c,
                       2, 2, ## 7a, 7b, 
                       2, 3) ## 8a, 8b


groups_subgroups = data.frame(Group = sort(unique(Aged_Scotland_clusters$Group)), Subgroups)
rownames(Aged_Scotland_clusters) = Aged_Scotland_clusters$Geography_Code



lis_subgroups = list()

for(G in unique(groups_subgroups$Group)){
    ### GET THE NUMBER OF SOLUTIONS
    n_Subgroups = groups_subgroups[groups_subgroups$Group==G, "Subgroups"]
    clusters <- list()
    fit <- NA
    
  
    cat("\r",  "Group: ",G)
    flush.console()
    ### CLUSTER THE GROUP
    for(i in 1:2000){
      
      cat("\r", "Running ",G," Group  |  Iteration: ", i, sep="")
      flush.console()
      
      clustering <- kmeans(x=dat, centers=n_Subgroups, iter.max=1000000, nstart=1)
      
      fit[i] <- clustering$tot.withinss
      if (fit[i] <= min(fit[1:(i-1)])){
        clusters <- clustering}
    }
    
    lis_subgroups[[G]]=clusters
    
  }
  
OAC_Subgroups_clustering <- lis_subgroups




# 
# Subgroups_agreed_for_agreed = readRDS("Data/Objects/agreed_for_agreed_subgroups.RDS")
# Subgroups_agreed_for_alternative = readRDS("Data/Objects/agreed_for_alternative_subgroups.RDS")
# 
# names(Subgroups_agreed_for_agreed)
# 
# OAC_Subgroups_clustering = Subgroups_agreed_for_agreed
# OAC_Subgroups_clustering$`1a` = Subgroups_agreed_for_alternative$`1a`
# OAC_Subgroups_clustering$`1b` = Subgroups_agreed_for_alternative$`1b`
# OAC_Subgroups_clustering$`1c` = Subgroups_agreed_for_alternative$`1c`
# 
# OAC_Subgroups_clustering$`7a` = Subgroups_agreed_for_alternative$`7a`
# OAC_Subgroups_clustering$`7b` = Subgroups_agreed_for_alternative$`7b`
# OAC_Subgroups_clustering = OAC_Subgroups_clustering[-19]
# 
# OAC_Subgroups_clustering = OAC_Subgroups_clustering[order(names(OAC_Subgroups_clustering))]
# 
# names(OAC_Subgroups_clustering)
# 

saveRDS(OAC_Subgroups_clustering, "Data/Clustering/OAC_Subgroups_clustering.RDS")
OAC_Subgroups_clustering = readRDS("Data/Clustering/OAC_Subgroups_clustering.RDS")




### RETRIEVE ASSIGNMENTS OF THE SUBGROUPS

  Subgroups_bind = NULL
  
  for(list_element in names(OAC_Subgroups_clustering)){
    
    # RETRIEVE THE SOLUTION FOR THE SUBGROUP
    element = OAC_Subgroups_clustering[[list_element]]
    
    df = data.frame(Group = list_element, cluster = element$cluster)
    df$Geography_Code = rownames(df)
    colnames(df)[2] = "Subgroup"
    
    # BIND INTO ONE DATAFRAME
    Subgroups_bind = rbind(Subgroups_bind, df)
  }
  
  
  ### BIND WITH THE PARENT DATA
Aged_Scotland_clusters = merge(Aged_Scotland_clusters, Subgroups_bind %>% mutate(Subgroup = paste0(Group, Subgroup)) %>% 
                                   select(-Group), by="Geography_Code", all=T)
  

  
write.csv(Aged_Scotland_clusters, "Data/Clustering/OAC_assignment.csv", row.names = F)
 


