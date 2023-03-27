#### CLUSTERING

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



### LOAD DATA
OAC_variables =read.csv("Data/Lookups/OAC_variables.csv")
Hybrid_OAC = read.csv("Data/Clean/Final_variables/Hybrid_OAC.csv") %>% select(-country)                #### UK DATA WITH AGE FOR SCOTLAND CARRIED FORWARD
Census_2011_OAC = read.csv("Data/Clean/Final_variables/Census_2011_OAC.csv")%>% select(-country)       #### 2011 CENSUS DATA FOR THE REFERENCE 
EW_OAC = read.csv("Data/Clean/Final_variables/EW_OAC.csv")%>% select(-country)                         #### ENGLAND AND WALES 2021 DATA
Aged_Scotland_OAC = read.csv("Data/Clean/Final_variables/Aged_Scotland.csv") %>% select(-country)   





####################################################
#############       SUPERGROUPS        #############
####################################################

#### RUN CLUSTER ANALYSIS FOR ALL DATASETS 
number_of_iterations = 2000

for(d in c("Hybrid_OAC", "EW_OAC", "Census_2011_OAC","Aged_Scotland_OAC")[4]){
  
  Input_data = get(d)
  rownames(Input_data) = Input_data$Geography_Code
  Input_data = Input_data[, OAC_variables$encoding]
  
  for(k in 8){
    fit=NA
    for (i in 1:number_of_iterations){
      
      cat("\r", "Running ",d," for ",k, " clusters. ", "Iteration: ", i, " ( ",round(i/number_of_iterations*100,3)," % )", sep="")
      flush.console()
      
      clustering <- kmeans(x=Input_data, centers=k, iter.max=1000000, nstart=1)
      
      fit[i] <- clustering$tot.withinss
      
      if (fit[i] < min(fit[1:(i-1)])){
        assign(paste0("Class_",d, "_",k), clustering)
      }
    }
  }
}


#### SAVE THE ASSIGNMENTS 
Hybrid_UK_clusters = cbind(Hybrid_OAC,  Class_Hybrid_OAC_7$cluster, Class_Hybrid_OAC_8$cluster)
colnames(Hybrid_UK_clusters)[(ncol(Hybrid_UK_clusters)-1):ncol(Hybrid_UK_clusters)] = c("Supergroup7", "Supergroup8")

Aged_Scotland_clusters = cbind(Aged_Scotland_OAC, Class_Aged_Scotland_OAC_7$cluster, Class_Aged_Scotland_OAC_8$cluster)
colnames(Aged_Scotland_clusters)[(ncol(Aged_Scotland_clusters)-1):ncol(Aged_Scotland_clusters)] = c("Supergroup7", "Supergroup8")

EW_clusters= cbind(EW_OAC,  Class_EW_OAC_7$cluster, Class_EW_OAC_8$cluster)
colnames(EW_clusters)[(ncol(EW_clusters)-1):ncol(EW_clusters)] = c("Supergroup7", "Supergroup8")

OAC_2011_clusters= cbind(Census_2011_OAC, Class_Census_2011_OAC_7$cluster,  Class_Census_2011_OAC_8$cluster)
colnames(EW_clusters)[(ncol(EW_clusters)-1):ncol(EW_clusters)] = c("Supergroup7", "Supergroup8")

# 
# write.csv(Hybrid_UK_clusters, "Data/Clean/Clustering/Hybrid_UK_clusters.csv", row.names = F)
# write.csv(Aged_Scotland_clusters, "Data/Clean/Clustering/Aged_Scotland_clusters.csv", row.names = F)
# write.csv(EW_clusters, "Data/Clean/Clustering/EW_clusters.csv", row.names = F)
# write.csv(OAC_2011_clusters, "Data/Clean/Clustering/OAC_2011_clusters.csv", row.names = F)


### SAVE CLUSTER OBJECTS
saveRDS(Class_Aged_Scotland_OAC_8, "Data/Objects/Aged_Scotland_8_class_10k.RDS")


f = data.frame(fit, id=1:length(fit))
saveRDS(f, "Data/Objects/fit_aged_8_10k.RDS")





######################################################################
#############          GROUPS FOR AGED SCOTLAND          #############
######################################################################

### LOAD IN THE CLUSTER SOLUTION
Class_Aged_Scotland_OAC_8 = readRDS("Data/Objects/Aged_Scotland_8_class_2k.RDS")


### Bind with the data
Aged_Scotland_clusters = cbind(Aged_Scotland_OAC, Class_Aged_Scotland_OAC_8$cluster)
colnames(Aged_Scotland_clusters)[ncol(Aged_Scotland_clusters)] = "Supergroup8"


### RECLASSIFY THE GROUP ALLOCATION
Aged_Scotland_clusters = Aged_Scotland_clusters %>% mutate(Reclassed8=recode(Supergroup8,
                                                                             '1' = '8', 
                                                                             '2' = '5', 
                                                                             '3' = '3', 
                                                                             '4' = '1', 
                                                                             '5' = '4', 
                                                                             '6' = '7', 
                                                                             '7' = '6', 
                                                                             '8' = '2'))


### DEFINE THE NUMBER (PREFFERED AND ALTERNATIVE) OF GROUPS FOR EACH SUPERGOUPS 
agreed = c(2, 3, 3, 3, 2, 3, 3, 2)
alternative = c(3, 2 , 2, 3, 3, 2, 2, 3)
#Groups_clustering = data.frame(Reclassed = 1:8,agreed, alternative)




#### PERFORM CLUSTERING
for(n_group in c("agreed","alternative")){
  
  ### GET THE NUMBER OF GROUPS PER EACH SUPERGROUP  
  Groups_per_super = get(n_group)
  
  lis=list()
  
  for(Reclassed in 1:8){
    
    # Get the number of Groups for SuperGroup
    n_Groups = Groups_per_super[Reclassed]
    clusters <- list()
    fit <- NA
    
    ### GET THE DATA FOR SPECIFIC SUPEGROUP
    dat = Aged_Scotland_clusters %>% filter(Reclassed8==Reclassed) %>% select(-Geography_Code,-Reclassed8, -Supergroup8)
    
    for(i in 1:number_of_iterations){
      
      cat("\r", "Running ",Reclassed," Reclassed for ", n_group, " |  Iteration: ", i, " ( ",round(i/number_of_iterations*100,3)," % )", sep="")
      flush.console()
      
      clustering <- kmeans(x=dat, centers=n_Groups, iter.max=1000000, nstart=1)
      
      fit[i] <- clustering$tot.withinss
      if (fit[i] <= min(fit[1:(i-1)])){
        clusters <- clustering}
    }
    
    ### STORE IN A LIST
    lis[[Reclassed]]=clusters
    
  }
  ### SAVE THE CLUSTERING SOLUTION FOR PREFFERED AND ALTERNATIVE
  assign(paste0(n_group,"_groups"), lis)
}

# saveRDS(agreed_groups, "Data/Objects/Agreed_groups.RDS")
# saveRDS(alternative_groups, "Data/Objects/Alternative_groups.RDS")

agreed_groups = readRDS("Data/Objects/Agreed_groups.RDS")
alternative_groups = readRDS("Data/Objects/Alternative_groups.RDS")




Aged_Scotland_Groups = Aged_Scotland_clusters %>% select(-Supergroup8)


### RETRIEVE ASSIGNMENTS OF THE GROUPS
for(groups_assignments in c("agreed_groups", "alternative_groups")){
  
  Groups_bind = NULL
  Supergroup_list = get(groups_assignments)
  for(list_element in 1:length(Supergroup_list)){
  
    # RETRIEVE THE SOLUTION FOR THE SUBGROUP
    element = Supergroup_list[[list_element]]
    df = data.frame(Supergroup = list_element, cluster = element$cluster)
    df$Geography_Code = rownames(df)
    
    # BIND INTO ONE DATAFRAME
    Groups_bind = rbind(Groups_bind, df)
  }
  # RECODE GROUPS 
  Groups_bind = Groups_bind %>% mutate(cluster = recode(cluster,'1' ="a", '2' = "b", '3' = "c", '4' = "d", '5'= "e"))
  colnames(Groups_bind)[2] = groups_assignments
  
  ### BIND WITH THE PARENT DATA
  Aged_Scotland_Groups = merge(Aged_Scotland_Groups, Groups_bind %>% select(-Supergroup), by="Geography_Code", all=T)
  

}


### CONVERT GROUP NAMES TO INCLUDE SUPERGROUP ID
Aged_Scotland_Groups = Aged_Scotland_Groups %>% mutate(agreed_groups = paste0(Reclassed8, agreed_groups), 
                                                       alternative_groups = paste0(Reclassed8, alternative_groups))












#########################################################################
#############          SUBGROUPS FOR AGED SCOTLAND          #############
#########################################################################



agreed_for_agreed  = c(2, 2, ## 1a, 1b
                       3, 2, 2, ## 2a, 2b, 2c
                       4, 2, 2, ## 3a, 3b, 3c, 
                       3, 4, 2, ## 4a, 4b, 4c,
                       3, 2, ## 5a, 5b
                       3,3,2, ## 6a, 6b, 6c,
                       3, 2, 2, ## 7a, 7b, 7c
                       2, 3) ## 8a, 8b

alternative_for_agreed  = c(3, 3, ## 1a, 1b
                            4, 2, 2, ## 2a, 2b, 2c
                            3, 2, 3, ## 3a, 3b, 3c, 
                            2, 2, 2, ## 4a, 4b, 4c,
                            2, 3, ## 5a, 5b
                            2, 2, 2, ## 6a, 6b, 6c,
                            2, 3, 2, ## 7a, 7b, 7c
                            3, 2) ## 8a, 8b


agreed_subgroups = data.frame(agreed_group = sort(unique(Aged_Scotland_Groups$agreed_groups)), agreed_for_agreed, alternative_for_agreed)


agreed_for_alternative  = c(2, 2,2, ## 1a, 1b,1c
                            4, 2, ## 2a, 2b
                            2, 2,  ## 3a, 3b,
                            3, 4, 2, ## 4a, 4b, 4c,
                            3, 3, 2, ## 5a, 5b, 5c
                            3, 4, ## 6a, 6b, 
                            2, 2, ## 7a, 7b, 
                            2, 2,2) ## 8a, 8b, 8c

alternative_for_alternative  = c(2,3 ,2, ## 1a, 1b,1c
                                 3, 4, ## 2a, 2b
                                 2, 4,  ## 3a, 3b,
                                 3, 2,2 , ## 4a, 4b, 4c,
                                 2,2,3, ## 5a, 5b, 5c
                                 2, 2, ## 6a, 6b, 
                                 2, 2, ## 7a, 7b, 
                                 2, 4,2) ## 8a, 8b, 8c

alternative_subgroups = data.frame(alternative_group = sort(unique(Aged_Scotland_Groups$alternative_groups)), agreed_for_alternative, alternative_for_alternative)






rownames(Aged_Scotland_Groups) = Aged_Scotland_Groups$Geography_Code

for(subgroups_division in c("agreed_subgroups", "alternative_subgroups")){
  
  # SELECT IF TAKING FROM AGREED OR ALTERNATIVE GROUPS
  Group_selection = get(subgroups_division)
  
  ### SELECT IF TAKING AGREED OR ALTERNATIVE SUBGROUPS
  for(solution in 2:3){
    
    
    sol = Group_selection[,c(1, solution)]
    colnames(sol) = c("group", "solution")
    
    #assign(paste0("Subgroups_",colnames(Group_selection)[solution]), list())
    lis = list()
    ### SELECT THE GROUP
    for(Group in unique(sol[,1])){
      
      ### GET THE NUMBER OF SOLUTIONS
      n_Subgroups = sol[sol$group==Group, "solution"]
      clusters <- list()
      fit <- NA
      
      
      ### SELECT DATA FOR THE GROUP
      if(subgroups_division == "agreed_subgroups"){
        dat = Aged_Scotland_Groups %>% filter(agreed_groups==Group) %>% select(-Geography_Code, -Reclassed8, -agreed_groups, -alternative_groups)
      } else{
        dat = Aged_Scotland_Groups %>% filter(alternative_groups==Group) %>% select(-Geography_Code, -Reclassed8, -agreed_groups, -alternative_groups)
      }
      
      
      cat("\r", solution, " ||| Group: ",Group)
      flush.console()
      ### CLUSTER THE GROUP
      for(i in 1:number_of_iterations){
        
        cat("\r", "Running ",Group," Group  |  Iteration: ", i, " ( ",round(i/number_of_iterations*100,3)," % )", sep="")
        flush.console()
        
        clustering <- kmeans(x=dat, centers=n_Subgroups, iter.max=1000000, nstart=1)
        
        fit[i] <- clustering$tot.withinss
        if (fit[i] <= min(fit[1:(i-1)])){
          clusters <- clustering}
      }
      
      lis[[Group]]=clusters
    
      }
    
    assign(paste0("Subgroups_",colnames(Group_selection)[solution]), lis)
  
    }
}


#saveRDS(Subgroups_agreed_for_agreed, "Data/Objects/agreed_for_agreed_subgroups.RDS")
#saveRDS(Subgroups_alternative_for_agreed, "Data/Objects/alternative_for_agreed_subgroups.RDS")
#saveRDS(Subgroups_agreed_for_alternative, "Data/Objects/agreed_for_alternative_subgroups.RDS")
#saveRDS(Subgroups_alternative_for_alternative, "Data/Objects/alternative_for_alternative_subgroups.RDS")


Subgroups_agreed_for_agreed = readRDS("Data/Objects/agreed_for_agreed_subgroups.RDS")
Subgroups_alternative_for_agreed = readRDS("Data/Objects/alternative_for_agreed_subgroups.RDS")
Subgroups_agreed_for_alternative = readRDS("Data/Objects/agreed_for_alternative_subgroups.RDS")
Subgroups_alternative_for_alternative = readRDS("Data/Objects/alternative_for_alternative_subgroups.RDS")



Aged_Scotland_Subgroups = Aged_Scotland_Groups


### RETRIEVE ASSIGNMENTS OF THE SUBGROUPS
for(subgroups_assignments in c("Subgroups_agreed_for_agreed", "Subgroups_alternative_for_agreed", "Subgroups_agreed_for_alternative", "Subgroups_alternative_for_alternative")){
  
  Subgroups_bind = NULL
  Group_list = get(subgroups_assignments)
  
  for(list_element in names(Group_list)){
    
    # RETRIEVE THE SOLUTION FOR THE SUBGROUP
    element = Group_list[[list_element]]
    
    df = data.frame(Group = list_element, cluster = element$cluster)
    df$Geography_Code = rownames(df)
    colnames(df)[2] = gsub("Subgroups_", "", subgroups_assignments)
    
    # BIND INTO ONE DATAFRAME
    Subgroups_bind = rbind(Subgroups_bind, df)
  }


  
  ### BIND WITH THE PARENT DATA
  Aged_Scotland_Subgroups = merge(Aged_Scotland_Subgroups, Subgroups_bind %>% select(-Group), by="Geography_Code", all=T)
  
}



Aged_Scotland_Subgroups = Aged_Scotland_Subgroups %>%  mutate(agreed_for_agreed = paste0(agreed_groups, agreed_for_agreed),
                                                              alternative_for_agreed = paste0(agreed_groups, alternative_for_agreed), 
                                                              agreed_for_alternative = paste0(alternative_groups, agreed_for_alternative), 
                                                              alternative_for_alternative = paste0(alternative_groups, alternative_for_alternative))


write.csv(Aged_Scotland_Subgroups, "Data/Clustering/Aged_Scotland_Subgroups.csv", row.names = F)
