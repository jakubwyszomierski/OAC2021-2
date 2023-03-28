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

### LOAD DATA
OAC_Supergroups_clustering = readRDS("Data/Clustering/OAC_Supergroups_clustering.RDS")
OAC_Groups_clustering =readRDS("Data/Clustering/OAC_Groups_clustering.RDS")
OAC_Subgroups_clustering =readRDS("Data/Clustering/OAC_Subgroups_clustering.RDS")


Aged_Scotland_OAC = read.csv("Data/Clustering/OAC_assignment.csv")
OAC_variables = read.csv("Data/Lookups/OAC_variables.csv")


OA_lookup = read.csv("~/Desktop/PhD/shapefiles/boundaries_2021/Lookup_2021.csv")
OA_lookup_london = OA_lookup %>% filter(lad22nm %in% unique(OA_lookup$lad22nm)[1:33]) %>% select(oa21cd, lad22nm)


### PICK COLORS
display.brewer.pal(8, "Set1")
colors = brewer.pal(8, "Set1")
OAC_colors = c('1' = colors[3], '2' = "#dbb6a2", '3' = colors[8], '4' = colors[2], '5' = colors[6], '6' = colors[5], '7'=colors[4], '8' = colors[1])






###################################################################
###############              PROPORTIONS            ###############
###################################################################

population = read.csv("Data/Clean/Raw_counts/hybrid_UK_2021_aged_Scotland.csv")[,1:2]
frequencies = Aged_Scotland_OAC %>% select(Geography_Code,Supergroup8,Group, Subgroup)

frequencies = merge(frequencies, population, by="Geography_Code", all=T) %>% rename(population = NM_2021_1_0)
summary(frequencies)
UK_pop = sum(frequencies$population)



#########     FREQUENCIES OF SUPERGROUPS    
frequencies %>% group_by(Supergroup8) %>% summarise(Supergroup_n=n(), 
                                                   Supergroup_OA_perc = Supergroup_n / nrow(frequencies)*100, 
                                                   Supergroup_pop_size = sum(population), 
                                                   Supergroup_pop_perc = Supergroup_pop_size/UK_pop*100)



########     FREQUENCIES OF GROUPS 
print(frequencies %>% group_by(Group, Supergroup8) %>% 
        summarise(Group_n=n(), Group_OA_perc = Group_n/nrow(frequencies)*100, 
                  Group_pop_size = sum(population), Group_pop_perc = Group_pop_size/UK_pop*100), n=500) 



#######     FREQUENCIES OF SUBGROUPS
print(frequencies  %>% group_by(Subgroup, Group, Supergroup8) %>% 
        summarise(Subgroup_n=n(), Subgroup_OA_perc = Subgroup_n/nrow(frequencies)*100, 
                  Subgroup_pop_size = sum(population), Subgroup_pop_perc = Subgroup_pop_size/UK_pop*100), n=100)





###################################################################
###############         2D clusters location        ###############
###################################################################


library(ggpubr)
library(factoextra)

png(file="Plots/FVIZ_Supergroups.png",res=120, width = 1000, height=1000)
fviz_cluster(OAC_Supergroups_clustering, data = Aged_Scotland_OAC[,OAC_variables$encoding],
             palette = OAC_colors, 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())
dev.off()










###################################################################
#################         BAR PLOTS           #####################
###################################################################

Global_Mean = Aged_Scotland_OAC[,OAC_variables$encoding] %>% summarise_all(mean)






########################################
########      SUPERGROUPS      #########
########################################


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
  select(-id) %>% arrange(Supergroup8)


Supergroups_centers_diff = Supergroups_centers


### CALCULATE THE DIFFERENCE BETWEEN THE CLUSTER CENTROID TO THE GLOBAL MEAN
for(class in 1:nrow(Supergroups_centers)){
  Supergroups_centers_diff[class,1:60] = (Supergroups_centers_diff[class,1:60] - Global_Mean)
}


min_change = -0.6
max_change = 0.6
for(reclass in unique(Supergroups_centers_diff$Supergroup8)){

  Supergroup_mean = Supergroups_centers_diff %>% filter(Supergroup8==reclass) %>% select(-Supergroup8)
  
  ### CREATE A BAR GRAPH
  gg_bar = ggplot(data=pivot_longer(Supergroup_mean, cols=everything())) + geom_col(aes(y=value, x=name, fill=value)) + 
    scale_x_discrete(labels=c(1:ncol(Global_Mean))) + 
    scale_y_continuous(limits = c(min_change,max_change)) +  
    labs(x="", y="Difference from UK mean", title = paste("Supergroup", reclass, "\n")) +  
    scale_fill_gradient(limits=c(min_change,max_change), low = "blue", high="red", ) + 
    theme_minimal() +  theme(axis.text = element_text(size=13), axis.title.y = element_text(size=19),legend.position="none",
                             plot.title= element_text(hjust = 0.5, size=23), plot.margin = margin(1,1,1,1, "cm"))
  
  ggsave(filename=paste0("Plots/Bar_plots/Supergroups/Supergroup_",reclass, ".png"), gg_bar, bg = "white", width=20, height = 9)
}


write.csv(Supergroups_centers[,c(61,1:60)], "Data/Clustering/OAC_centroids.csv", row.names = F)









##########################################
##########         GROUPS        #########
##########################################

Groups_centroids = NULL

for(i in 1:length(OAC_Groups_clustering)){
  
  Groups_cluster_centers = as.data.frame(OAC_Groups_clustering[[i]]$centers)
  Groups_cluster_centers_diff = Groups_cluster_centers
  Groups_cluster_centers_diff_to_super = Groups_cluster_centers
  
  Supergroup_mean = Aged_Scotland_OAC %>% filter(Supergroup8==i) %>% select(OAC_variables$encoding) %>% summarise_all(mean)
  
  for(class in 1:nrow(Groups_cluster_centers_diff)){
    Groups_cluster_centers_diff[class,] = Groups_cluster_centers_diff[class,] - Global_Mean
    Groups_cluster_centers_diff_to_super[class,] = Groups_cluster_centers_diff_to_super[class,] - Supergroup_mean
  }
  
  

#  min_change = min(c(min(Groups_cluster_centers_diff),  min(Groups_cluster_centers_diff_to_super)))
#  max_change = max(c(max(Groups_cluster_centers_diff),  max(Groups_cluster_centers_diff_to_super)))

  min_change = -0.8#floor(min_change*10)/10
  max_change = 0.8 #ceiling(max_change*10)/10
  
  
  Groups_cluster_centers$id = rownames(Groups_cluster_centers)
  Groups_cluster_centers = Groups_cluster_centers %>% mutate(id=recode(id, '1' ="a", '2' = "b", '3' = "c", '4' = "d", '5'= "e"), 
                                                             Group=paste0(i, id)) %>% select(-id)
  
  Groups_centroids = rbind(Groups_centroids, Groups_cluster_centers)
  
  Groups_cluster_centers_diff$id = rownames(Groups_cluster_centers_diff)
  Groups_cluster_centers_diff = Groups_cluster_centers_diff %>% mutate(id=recode(id, '1' ="a", '2' = "b", '3' = "c", '4' = "d", '5'= "e"))
  
  Groups_cluster_centers_diff_to_super$id = rownames(Groups_cluster_centers_diff_to_super)
  Groups_cluster_centers_diff_to_super = Groups_cluster_centers_diff_to_super %>% mutate(id=recode(id, '1' ="a", '2' = "b", '3' = "c", '4' = "d", '5'= "e"))
  
  
  diff_long = pivot_longer(Groups_cluster_centers_diff %>% mutate(id=paste0("Group ",i, id)), cols=-c(id))
  diff_long_supergroups = pivot_longer(Groups_cluster_centers_diff_to_super %>% mutate(id=paste0("Group ",i, id)), cols=-c(id))
  diff_long_two_means = merge(diff_long_supergroups,diff_long, by=c("name","id" ), suffixes = c( "_supergroup","_global"))
  diff_long_two_means = pivot_longer(diff_long_two_means, cols=-c(id, name), names_to = "mean", values_to="value")
  
  
  for(group_label in sort(unique(diff_long_two_means$id))){
    
   group_bar_plot = ggplot(data=diff_long_two_means %>% filter(id == group_label)) + 
      geom_col(aes(y=value, x=name, fill=mean), position = "dodge",width = 0.85, alpha=0.85)+
     scale_x_discrete(limits = rev(levels(as.factor(diff_long_two_means$name)))) + 
      scale_y_continuous(limits = c(min_change,max_change), breaks = seq(min_change, max_change, 0.2)) +  
     #facet_wrap(~id, nrow=1, scales = "free_x") +
     scale_fill_discrete(labels=c('Difference from the UK mean', 'Difference from the Supergroup mean')) + 
      labs(x="", y="Difference from the mean", title = group_label) +  
      coord_flip() + 
      theme_minimal() + 
     theme(legend.position = "bottom", legend.title = element_blank(),legend.direction='vertical',
           axis.text = element_text(size=17), axis.title.x = element_text(size=23), panel.spacing = unit(5, "lines"),
           plot.margin = margin(1,1,1,1, "cm"), legend.text = element_text(size=18), 
           plot.title = element_text(hjust = 0.5, size=30))
   

    
   ggsave(filename=paste0("Plots/Bar_plots/Groups/",gsub(" ","_",group_label), ".png"), group_bar_plot, bg = "white", height = 22,width=11)
  }
  
  
}



Groups_centroids = Groups_centroids[,c(61,1:60)]
write.csv(Groups_centroids, "Data/Clustering/OAC_Groups_centroids.csv", row.names=F)



#############################################
##########         SUBGROUPS        #########
#############################################

Subgroup_centroids = NULL
  for(i in names(OAC_Subgroups_clustering)){

    
   # sub_solution = solution
  
    Subgroups_cluster_centers = as.data.frame(OAC_Subgroups_clustering[[i]]$centers)
    Subgroups_cluster_centers_diff = Subgroups_cluster_centers
    Subgroups_cluster_centers_to_Supergroup = Subgroups_cluster_centers
    Subgroups_cluster_centers_to_Group = Subgroups_cluster_centers
    
    
   # colnames(sub_solution)[grep(subgroup_solution, colnames(sub_solution))] = "var"
    
    
    Supergroup_mean = Aged_Scotland_OAC %>% filter(Supergroup8==substr(i, 1,1)) %>% select(OAC_variables$encoding) %>% summarise_all(mean)
    
     Group_mean = Aged_Scotland_OAC %>% filter(Group==i)%>% select(OAC_variables$encoding) %>% summarise_all(mean)

  
    
    for(class in 1:nrow(Subgroups_cluster_centers_diff)){
      Subgroups_cluster_centers_diff[class,] = Subgroups_cluster_centers_diff[class,] - Global_Mean
      Subgroups_cluster_centers_to_Supergroup[class,] = Subgroups_cluster_centers_to_Supergroup[class,] - Supergroup_mean
      Subgroups_cluster_centers_to_Group[class,] = Subgroups_cluster_centers_to_Group[class,] - Group_mean
    }
    
    
    
   # min_change = min(c(min(Subgroups_cluster_centers_diff),  min(Subgroups_cluster_centers_to_Supergroup),min(Subgroups_cluster_centers_to_Group)))
   # max_change = max(c(max(Subgroups_cluster_centers_diff),  max(Subgroups_cluster_centers_to_Supergroup),max(Subgroups_cluster_centers_to_Group)))
   # 
    min_change = -0.8# floor(min_change*10)/10
    max_change = 0.8# ceiling(max_change*10)/10
    
    
    Subgroups_cluster_centers$id = rownames(Subgroups_cluster_centers)
    Subgroups_cluster_centers = Subgroups_cluster_centers %>% mutate(id=paste0(i,id))
    
    Subgroup_centroids = rbind(Subgroup_centroids, Subgroups_cluster_centers)
    
    
    Subgroups_cluster_centers_diff$id = rownames(Subgroups_cluster_centers_diff)
    Subgroups_cluster_centers_diff = Subgroups_cluster_centers_diff %>% mutate(id=paste0(i,id))
    
    Subgroups_cluster_centers_to_Supergroup$id = rownames(Subgroups_cluster_centers_to_Supergroup)
    Subgroups_cluster_centers_to_Supergroup = Subgroups_cluster_centers_to_Supergroup %>% mutate(id=paste0(i,id))
    
    Subgroups_cluster_centers_to_Group$id = rownames(Subgroups_cluster_centers_to_Group)
    Subgroups_cluster_centers_to_Group = Subgroups_cluster_centers_to_Group %>% mutate(id=paste0(i,id))
    
    

    
    diff_long = pivot_longer(Subgroups_cluster_centers_diff %>% mutate(id=paste0("Subgroup ",id)), cols=-c(id))
    diff_long_supergroups = pivot_longer(Subgroups_cluster_centers_to_Supergroup %>% mutate(id=paste0("Subgroup ",id)), cols=-c(id))
    diff_long_groups = pivot_longer(Subgroups_cluster_centers_to_Group %>% mutate(id=paste0("Subgroup ",id)), cols=-c(id))
    
   diff_long_three_means =  rbind(diff_long %>% mutate(mean = "value_global"), 
          diff_long_supergroups%>% mutate(mean = "value_Supergroup"),
          diff_long_groups%>% mutate(mean = "value_Group"))
    
   diff_long_three_means$mean = factor(diff_long_three_means$mean, levels=c("value_global","value_Supergroup", "value_Group"))
    
  #  diff_long_two_means = merge(diff_long_groups,diff_long_supergroups, by=c("name","id" ), suffixes = c( "_group","_supergroup"))
  #  diff_long_two_means = pivot_longer(diff_long_two_means, cols=-c(id, name), names_to = "mean", values_to="value")
    
    
    for(subgroup_label in sort(unique(diff_long_three_means$id))){
      
      subgroup_bar_plot = ggplot(data=diff_long_three_means %>% filter(id == subgroup_label)) + 
        geom_col(aes(y=value, x=name, fill=mean), position = "dodge",width = 0.75, alpha=0.85)+
        scale_x_discrete( limits = rev(levels(as.factor(diff_long_three_means$name)))) + 
        scale_y_continuous(limits = c(min_change,max_change), breaks=seq(min_change, max_change, by=0.2)) +  
        scale_fill_manual( values = c('#F8766D', '#619CFF', '#7CAE00'),
                           labels=c('Difference from the UK mean','Difference from the Supergroup mean', 'Difference from the Group mean'))+
        #facet_wrap(~id, nrow=1, scales = "free_x") +
   #   scale_fill_discrete() + 
        labs(x="", y="Difference from the mean", title = subgroup_label) +  
        coord_flip() + 
        theme_minimal() + 
        theme(legend.position = "bottom", legend.title = element_blank(),legend.direction='vertical',
              axis.text = element_text(size=17), axis.title.x = element_text(size=23), panel.spacing = unit(5, "lines"),
              plot.margin = margin(1,1,1,1, "cm"), legend.text = element_text(size=18), 
              plot.title = element_text(hjust = 0.5, size=30))
      
      
      
     ggsave(filename=paste0("Plots/Bar_plots/Subgroups/",gsub(" ","_",subgroup_label), ".png"), subgroup_bar_plot, bg = "white", height = 22, width=11)
    }
    
    
  }
  
Subgroup_centroids = Subgroup_centroids[,c(61,1:60)]
colnames(Subgroup_centroids)[1] = "Subgroup"

write.csv(Subgroup_centroids, "Data/Clustering/OAC_Subgroups_centroids.csv", row.names=F)








# 
# ##################################################################
# ##############           RADIAL PLOTS           ##################
# ##################################################################


# in(Supergroups_centers_diff[,1:60])
# ax(Supergroups_centers_diff[,1:60])

# ## SUPERGROUPS
# or(group in sort(unique(Supergroups_centers_diff$Supergroup8))){
#  
#  ### CREATE A RADIAL PLOT
#  
#    png(file=paste0("Plots/Radial_plots/Supergroups/Radial_plot_",group, ".png"),res=120, width = 1000, height=1000)
#  
#    radial.plot(rbind(0,Supergroups_centers_diff %>% filter(Supergroup8==group) %>% select(-Supergroup8)), rp.type = "p", radial.lim = c(-0.6,0.6), 
#                labels=c(1:ncol(Global_Mean)), line.col = c("red", "blue"), lwd = 3, grid.col = "grey90", 
#                main = paste("Supergroup", group), mar=c(4,4,4,4))
#    
#  dev.off()
#  
# 








# ###### GROUPS



# or(i in 1:length(agreed_groups)){
#  
#  Groups_cluster_centers = as.data.frame(agreed_groups[[i]]$centers)
#  Groups_cluster_centers_diff = Groups_cluster_centers
#  Groups_cluster_centers_diff_to_super = Groups_cluster_centers
#  
#  Supergroup_mean = Aged_Scotland_Subgroups %>% filter(Supergroup8==i) %>% select(OAC_variables$encoding) %>% summarise_all(mean)
#  
#  for(class in 1:nrow(Groups_cluster_centers_diff)){
#    Groups_cluster_centers_diff[class,] = Groups_cluster_centers_diff[class,] - Global_Mean
#    Groups_cluster_centers_diff_to_super[class,] = Groups_cluster_centers_diff_to_super[class,] - Supergroup_mean
#  }
#  



# roups_cluster_centers_diff$id = rownames(Groups_cluster_centers_diff)
# roups_cluster_centers_diff = Groups_cluster_centers_diff %>% mutate(id=recode(id, '1' ="a", '2' = "b", '3' = "c", '4' = "d", '5'= "e"))

# roups_cluster_centers_diff_to_super$id = rownames(Groups_cluster_centers_diff_to_super)
# roups_cluster_centers_diff_to_super = Groups_cluster_centers_diff_to_super %>% mutate(id=recode(id, '1' ="a", '2' = "b", '3' = "c", '4' = "d", '5'= "e"))



# or(group in sort(unique(Groups_cluster_centers_diff$id))){
#  
#  ### CREATE A RADIAL PLOT
#  
#  png(file=paste0("Plots/Radial_plots/Groups/Radial_plot_",i, group, ".png"),res=120, width = 1000, height=1000)
#  
#  radial.plot(
#    rbind(0,
#          Groups_cluster_centers_diff %>% filter(id==group) %>% select(-id), 
#          Groups_cluster_centers_diff_to_super %>% filter(id==group) %>% select(-id)
#          ), rp.type = "p", radial.lim = c(-0.6,0.6), 
#              labels=c(1:ncol(Global_Mean)), line.col = c("red","orange", "blue"), lwd = 3, grid.col = "grey90", 
#  
#              main = paste0("Group ", i, group), mar=c(4,4,4,4))
#  
#  dev.off()
#  
# 
# 
# 
# 
# 
# 

###################################################################
#################            MAPS             #####################
###################################################################

### MERGE WITH SHAPEFILE
Aged_shp = merge(hybrid_shp_simple, Aged_Scotland_OAC %>% select(-c(OAC_variables$encoding)), by="Geography_Code") 

### GET MAPS FOR LONDON
London_maps = st_read("~/Desktop/PhD/shapefiles/boundaries_2021/OA_2021/OA_2021_EW_BGC.shp")
London_boroughs = st_read("~/Desktop/PhD/shapefiles/London_boundaries_2011/London_Borough_Excluding_MHW.shp")

London_maps = Aged_shp %>% filter(Geography_Code %in% OA_lookup_london$oa21cd)
group_colors = brewer.pal(5, "Set1")[-4]



tmap_mode("view")
tm_shape(Aged_shp %>% filter(Subgroup=="4c2")) + 
   tm_fill(col="Subgroup",  palette=group_colors, 
           alpha = 0.5,  colorNA = "grey90", legend.show = F) +
  tm_borders()+
  tm_layout(frame=F)
tmap_mode("plot")





########################################
########      SUPERGROUPS      #########
########################################

#### UK MAP
tmap_save(tm_shape(Aged_shp) + 
            tm_fill("Supergroup8",  alpha = 0.9, style = "cat", palette=OAC_colors, title="Supergroups") +
            tm_layout(frame=F, legend.position = c("left", "bottom")), 
          filename = paste0("Maps/Clustering/Aged_Scotland_OAC.png"), dpi = 2500)

### LONDON MAP
tmap_save(tm_shape(London_maps) + 
            tm_fill("Supergroup8",  alpha = 0.9, style = "cat", palette=OAC_colors, colorNA = "grey94", title="Supergroups") +
            tm_shape(London_boroughs)  + tm_borders(col="grey45", lwd = 1.1) + 
            tm_layout(frame=F, legend.position = c("left", "bottom")), 
          filename = paste0("Maps/Clustering/London_OAC.png"), dpi = 2000)







### SEPERATE CLUSTERS
for(shp in c("Aged_shp", "London_maps")){
  print(shp)
  dat = get(shp)
  
for(cluster in c(1:8)){
  print(cluster)

  
  tm_Supergroup = tm_shape(dat %>% mutate(Supergroup8 = as.factor(ifelse(Supergroup8==cluster, cluster, NA)))) + 
    tm_fill("Supergroup8",  alpha = 0.9,palette = OAC_colors,colorNA = "grey90", legend.show = T, showNA = F, title="Supergroup") +
    tm_compass(position=c("right", "bottom"), size = 3) + 
    tm_layout(frame=F, legend.position = c("left", "bottom"))
  
  
  if(shp=="London_maps"){
    tm_Supergroup = tm_Supergroup + tm_shape(London_boroughs) + tm_borders(col="grey45", lwd = 1.1) + 
      tm_scale_bar(position = c("right", "bottom"),width = 0.15)
    save_name = "London_"
  } else{
    save_name="UK_"
  }
    
  tmap_save(tm_Supergroup, filename = paste0("Maps/Clustering/Supergroups/",save_name,"Supergroup_", cluster, ".png"), dpi=1700)

  
}
}





# Define the initial color in hex format
initial_color <- "#FF5733"
# Define the number of colors to create
n_colors <- 3
# Create a sequence of increasing lightness values between 0 and 1
lightness_vals <- seq(0, 1, length.out = n_colors)
# Apply the lighten function to the initial color for each lightness value
lighter_colors <- sapply(lightness_vals, function(l) colorspace::lighten(initial_color, amount = l))
# View the resulting lighter colors
lighter_colors





#######################################
########        GROUPS        #########
#######################################

for(shp in c("Aged_shp", "London_maps")){
  dat = get(shp)

for(Reclass in c(1:8)){
  
  ### SAVE PREFFERED GROUP SOLUTION
  
  
  # Define the initial color in hex format
  initial_color <- OAC_colors[Reclass][[1]]
  # Define the number of colors to create
  n_colors <- length(unique(Aged_Scotland_OAC[Aged_Scotland_OAC$Supergroup8==Reclass, "Group"]))
  # Create a sequence of increasing lightness values between 0 and 1
  lightness_vals <- seq(0.1, 0.8, length.out = n_colors)
  # Apply the lighten function to the initial color for each lightness value
  lighter_colors <- sapply(lightness_vals, function(l) colorspace::lighten(initial_color, amount = l))
  # View the resulting lighter colors
  lighter_colors
  
  
  
     tm_Group <-  tm_shape(dat %>% mutate(Group = ifelse(Supergroup8==Reclass, Group, NA ))) + 
              tm_fill(col="Group",  alpha = 0.9, style = "cat", palette=lighter_colors, colorNA = "grey94", 
                      title=paste("Cluster ", Reclass), showNA = F) +
              tm_layout(frame=F, legend.position = c("left", "bottom"))
  
                     if(shp=="London_maps"){
                       tm_Group = tm_Group + tm_shape(London_boroughs) + tm_borders(col="grey45", lwd = 1.1) + 
                         tm_scale_bar(position = c("right", "bottom"),width = 0.15)
                       save_name = "London_"
                     } else{
                       save_name="UK_"
                     }

     tmap_save(tm_Group, dpi = 1700,
                                filename = paste0("Maps/Clustering/Groups/",save_name,"Groups_",Reclass ,".png"))
  
  }
}











##########################################
########        SUBGROUPS        #########
##########################################



for(shp in c("Aged_shp", "London_maps")){
  dat= get(shp)
  

  ### GET THE GROUP
  for(gr in sort(unique(dat$Supergroup8))){
  

    
    ### SELECT THE VARIABLES BASED ON THE SOLUTION GROUP


    
      
         #### GET THE SUBGROUP SOLUTION
     #       for(subgroups in colnames(sub_dat)[2:3]){

     #         sub_dat2 = sub_dat
     #         colnames(sub_dat2)[grep(subgroups, colnames(sub_dat2))] = "subg"
              

    
    
    # Define the initial color in hex format
    initial_color <- OAC_colors[gr][[1]]
    # Define the number of colors to create
    n_colors <- length(unique(Aged_Scotland_OAC[Aged_Scotland_OAC$Supergroup8==Reclass, "Subgroup"]))
    # Create a sequence of increasing lightness values between 0 and 1
    lightness_vals <- seq(0.1, 0.9, length.out = n_colors)
    # Apply the lighten function to the initial color for each lightness value
    lighter_colors <- sapply(lightness_vals, function(l) colorspace::lighten(initial_color, amount = l))
    # View the resulting lighter colors
    lighter_colors
    
    
    
              ### SAVE PREFFERED GROUP SOLUTION
              tm_Subgroup <-  tm_shape(dat %>% mutate(Subgroup = ifelse(Supergroup8==gr, Subgroup, NA ))) + 
                tm_fill(col="Subgroup",  alpha = 0.9, style = "cat", palette=lighter_colors, colorNA = "grey94", title=paste("Group", gr), showNA = F) +
                tm_layout(frame=F, legend.position = c("left", "bottom"))
              
                        if(shp=="London_maps"){
                          tm_Subgroup = tm_Subgroup + tm_shape(London_boroughs) + tm_borders(col="grey45", lwd = 1.1) + 
                            tm_scale_bar(position = c("right", "bottom"),width = 0.15)
                          save_name = "London_"
                        } else{
                          save_name="UK_"
                        }
              
              tmap_save(tm_Subgroup, 
                                         filename = paste0("Maps/Clustering/Subgroups/",
                                                           save_name, gr, ".png"), dpi = 1700)
              
            }
    }



