
setwd("~/Desktop/PhD/GIT/OAC2021")

library(nomisr)
library(dplyr)
library(sf)
library(tmap)
library(tidyverse)
library(readxl)
library(Hmisc)
library(tmap)
library(corrplot)
library(magrittr)
library(grDevices)
library(reshape2)

options( digits=5, max.print = 1000)


##### LOAD DATA
# final_codes = read.csv("Data/Selected_Codes_Names.csv")

all_variable_codes = read.csv("Data/Lookups/All_variable_codes.csv") %>% select(-CategoryCode11) %>% arrange(TS_code, TableCode)
OAC_variables = read.csv("Data/Lookups/OAC_variables.csv")
common_variables = read.csv("Data/Lookups/common_variables.csv")

#Census_2011 = read.csv("Data/Clean/Census_2011_OA.csv")
#Census_2021 = read.csv("Data/Clean/Census_2021_OA.csv")

Census_2011_perc = read.csv("Data/Clean/Percentages/Census_2011_common_var_prop_perc.csv")
Census_2021_perc = read.csv("Data/Clean/Percentages/Census_2021_common_var_prop_perc.csv")
Aged_Scotland_perc = read.csv("Data/Clean/Percentages/hybrid_UK_2021_aged_Scotland_prop_perc.csv")


Census_2011_IHS_Range = read.csv("Data/Clean/Transformed/Census_2011_common_var_prop_perc_IHS_range.csv")
Census_2021_IHS_Range = read.csv("Data/Clean/Transformed/Census_2021_common_var_prop_perc_IHS_range.csv")
Aged_Scotland_Range = read.csv("Data/Clean/Transformed/hybrid_UK_2021_aged_Scotland_prop_perc_IHS_range.csv")


head(common_variables)

### order of the variables
intersect(common_variables$Code, names(Aged_Scotland_perc))

original_variables = common_variables %>% filter(Code %in% intersect(common_variables$Code, names(Aged_Scotland_perc)), 
                                               !is.na(CategoryCode11))
original_variables$encoding = paste0("a", str_pad(c(1:nrow(original_variables)),3,pad="0"))


Aged_Scotland_perc_original = Aged_Scotland_perc[,unique(original_variables$Code)]
colnames(Aged_Scotland_perc_original) = original_variables$encoding 

Census_2021_perc_original = Census_2021_perc[,unique(original_variables$Code)]
colnames(Census_2021_perc_original) = original_variables$encoding 

Census_2011_perc_original = Census_2011_perc[,unique(original_variables$Code)]
colnames(Census_2011_perc_original) = original_variables$encoding 

# 
# 
# Census_2011_oa_changed_perc = read.csv("Data/Clean/Census_2011_oa_changed.csv")
# Census_2021_oa_changed_perc = read.csv("Data/Clean/Census_2021_oa_changed.csv")
# Census_2011_oa_changed_perc[,-1] = Census_2011_oa_changed_perc[,-1] *100
# Census_2021_oa_changed_perc[,-1] = Census_2021_oa_changed_perc[,-1] *100
# 
# Census_2011_oa_changed_IHS_Range = read.csv("Data/Clean/Census_2011_oa_changed_IHS_Range.csv")
# Census_2021_oa_changed_IHS_Range = read.csv("Data/Clean/Census_2021_oa_changed_IHS_Range.csv")
# 


###################################################################################
####################       CORRELATION MATRIX BETWEEN VARIABLES      ##############
###################################################################################


####    Transform to matrix
Census_2011_perc_matrix <- as.matrix(data.frame(Census_2011_perc_original))
Census_2021_perc_matrix <- as.matrix(data.frame(Census_2021_perc_original))
Aged_Scotland_perc_matrix <- as.matrix(data.frame(Aged_Scotland_perc_original))


Aged_Scotland_perc_matrix


  
### Run correlation tests (and round to 6 decimal points, to avoid getting a floating point)
Census_2011_cor_coef <-  round(rcorr(Census_2011_perc_matrix,  type="pearson")$r,6)
Census_2011_cor_pval <-  round(rcorr(Census_2011_perc_matrix,  type="pearson")$P,6)


Census_2021_cor_coef <-  round(rcorr(Census_2021_perc_matrix,  type="pearson")$r,6)
Census_2021_cor_pval <-  round(rcorr(Census_2021_perc_matrix,  type="pearson")$P,6)



Aged_Scotland_cor_coef <-  round(rcorr(Aged_Scotland_perc_matrix,  type="pearson")$r,6)
Aged_Scotland_cor_pval <-  round(rcorr(Aged_Scotland_perc_matrix,  type="pearson")$P,6)



### The p-value for che correlation between the same variables is NA (understandably, because it is the same variable)
### I will set it to 1 (the highest posssible value for p-value) so that the correlation plot deems the correlation insignificant
Census_2011_cor_pval[is.na(Census_2011_cor_pval)] = 1
Census_2021_cor_pval[is.na(Census_2021_cor_pval)] = 1
Aged_Scotland_cor_pval[is.na(Aged_Scotland_cor_pval)] = 1



#### Correlation matrix for 2011 
png(file="Plots/Correlation/Corrplot_Census_2021_all_vars.png", width = 3000, height=3000)
corrplot(Census_2021_cor_coef, method="color", diag=F,
         p.mat=Census_2021_cor_pval, sig.level = 0.05,insig = 'blank',
         col = COL2('RdYlBu', 5), 
         cl.pos="b",cl.ratio=0.1, cl.cex = 4, 
         tl.cex = 1.5, tl.col = "black") %>% 
  corrRect(name = c((original_variables %>% distinct(TableName21, .keep_all=T))$encoding,
                    paste0("a", nrow(original_variables))), lwd=12, col="black") 
dev.off()


#### CORRELATION MATRIX FOR AGED SCOTLAND ALL VARS
png(file="Plots/Correlation/Corrplot_Aged_Scotland_all_vars.png", width = 3000, height=3000)
corrplot(Aged_Scotland_cor_coef, method="color", diag=F,
          p.mat=Aged_Scotland_cor_pval, sig.level = 0.05,insig = 'blank',
         col = COL2('RdYlBu', 5), 
         cl.pos="b",cl.ratio=0.1, cl.cex = 4, 
         tl.cex = 1.5, tl.col = "black")  %>% 
  corrRect(name = c((original_variables %>% distinct(TableName21, .keep_all=T))$encoding,
                    paste0("a", nrow(original_variables))), lwd=12, col="black") 

dev.off()





##############################################################################
#############         CORRELATIONS FOR OAC VARIABLES          ################
##############################################################################


####    Transform to matrix

Census_2021_perc_OAC_matrix <- as.matrix(data.frame(Census_2021_perc[,OAC_variables$Code]))
Aged_Scotland_perc_OAC_matrix <- as.matrix(data.frame(Aged_Scotland_perc[,OAC_variables$Code]))


Census_2021_cor_OAC_coef <-  round(rcorr(Census_2021_perc_OAC_matrix,  type="pearson")$r,6)
Census_2021_cor_OAC_pval <-  round(rcorr(Census_2021_perc_OAC_matrix,  type="pearson")$P,6)

Aged_Scotland_cor_OAC_coef <-  round(rcorr(Aged_Scotland_perc_OAC_matrix,  type="pearson")$r,6)
Aged_Scotland_cor_OAC_pval <-  round(rcorr(Aged_Scotland_perc_OAC_matrix,  type="pearson")$P,6)




rownames(Aged_Scotland_cor_OAC_coef) <- colnames(Aged_Scotland_cor_OAC_coef) <- OAC_variables$encoding
rownames(Aged_Scotland_cor_OAC_pval) <- colnames(Aged_Scotland_cor_OAC_pval) <- OAC_variables$encoding

rownames(Census_2021_cor_OAC_coef) <- colnames(Census_2021_cor_OAC_coef) <- OAC_variables$encoding
rownames(Census_2021_cor_OAC_pval) <- colnames(Census_2021_cor_OAC_pval) <- OAC_variables$encoding


png(file="Plots/Correlation/Corrplot_Aged_Scotland_OAC_vars.png", width = 3000, height=3000)
corrplot(Aged_Scotland_cor_OAC_coef, method="color", diag=F,
         p.mat=Aged_Scotland_cor_OAC_pval, sig.level = 0.05,insig = 'blank',
         col = COL2('RdYlBu', 5), 
         cl.pos="b",cl.ratio=0.1, cl.cex = 5, 
         tl.cex = 2.25, tl.col = "black") %>% 
  corrRect(name=c((OAC_variables %>% distinct(TableName21, .keep_all = T))$encoding, 
                  OAC_variables[nrow(OAC_variables), "encoding"]), lwd=10, col="black")
dev.off()



png(file="Plots/Correlation/Corrplot_Census_2021_OAC_vars.png", width = 3000, height=3000)
corrplot(Census_2021_cor_OAC_coef, method="color", diag=F,
         p.mat=Census_2021_cor_OAC_pval, sig.level = 0.05,insig = 'blank',
         col = COL2('RdYlBu', 5), 
         cl.pos="b",cl.ratio=0.1, cl.cex = 5, 
         tl.cex = 2.25, tl.col = "black") %>% 
  corrRect(name=c((OAC_variables %>% distinct(TableName21, .keep_all = T))$encoding, 
                  OAC_variables[nrow(OAC_variables), "encoding"]), lwd=10, col="black")
dev.off()








####################################################################################
############         CORRELATION MEASURES AND DISTRIBUTION          ################
####################################################################################


#########     MERGE DATASETS ACROSS THE YEARS


Census_both_perc = merge(Census_2011_perc, Census_2021_perc, by="Geography_Code", suffixes=c("_2011", "_2021"))
Census_both_range =  merge(Census_2011_IHS_Range, Census_2021_IHS_Range, by="Geography_Code", suffixes=c("_2011", "_2021"))


nrow(Census_both_oa_changed_perc)
nrow(Census_both_oa_changed_range)

#### Calculate correlation for each variable between the years
Var_correlation_between_census = data.frame(Code = colnames(Census_2021_perc)[-1], 
                                            var_name=NA,  table=NA, cor_between_years=NA, pvalue = NA )




for(c in unique(Var_correlation_between_census$Code)){
  cat("\r", c, " --- ", paste(match(c, unique(Var_correlation_between_census$Code))), "out of", 
      length(unique(Var_correlation_between_census$Code)))
  flush.console()
  
  
  Var_correlation_between_census[Var_correlation_between_census$Code==c,"var_name"] = common_variables[common_variables$Code==c,"Name"]
  Var_correlation_between_census[Var_correlation_between_census$Code==c,"table"] = common_variables[common_variables$Code==c,"TableName21"]

  Var_correlation_between_census[Var_correlation_between_census$Code==c,"cor_between_years"] = round(cor.test(Census_both_perc[,paste0(c, "_2011")],
                                                                                                              Census_both_perc[,paste0(c, "_2021")])$estimate,4)
  
  Var_correlation_between_census[Var_correlation_between_census$Code==c,"pvalue"] = round(cor.test(Census_both_perc[,paste0(c, "_2011")],
                                                                                                   Census_both_perc[,paste0(c, "_2021")])$p.value, 4)
  
  #Var_correlation_between_census[Var_correlation_between_census$encoding==u,"ad_test_2011"] = round(nortest::ad.test(Census_2011_oa_changed_IHS_range[, u])$p.value, 6)
  Var_correlation_between_census[Var_correlation_between_census$Code==c,"ad_test_2021"] = round(nortest::ad.test(Census_2021_perc[, c])$p.value, 6)
  
  #Var_correlation_between_census[Var_correlation_between_census$encoding==u,"skewness_2011"] = moments::skewness(Census_2011_oa_changed_IHS_range[,u])
  Var_correlation_between_census[Var_correlation_between_census$Code==c,"skewness_2021"] = moments::skewness(Census_2021_perc[,c])
  
 # Var_correlation_between_census[Var_correlation_between_census$encoding==u,"kurtosis_2011"] = moments::kurtosis(Census_2011_oa_changed_IHS_range[,u])
  Var_correlation_between_census[Var_correlation_between_census$Code==c,"kurtosis_2021"] = moments::kurtosis(Census_2021_perc[,c])
  
}






#Var_correlation_between_census$diff = round((Var_correlation_between_census$cor_oa_changed - Var_correlation_between_census$cor) / Var_correlation_between_census$cor, 4)
### See the most extreme correlations across the years
Var_correlation_between_census %>% filter(cor_between_years ==1 | cor_between_years<0.5) %>% arrange(Code)
# Var_correlation_between_census %>% arrange(encoding)




###########################################################################
#############         GRAPH ON CORRELATION BETWEEN YEARS      #############
###########################################################################



#match((final_codes%>% distinct(TS_code, .keep_all=T))$encoding, )

first_bg_col  = "grey85"
second_bg_col = "grey93"

dat = Var_correlation_between_census %>% filter(Code %in% unique(original_variables$Code))
dat = merge(dat, original_variables %>% select(Code, encoding), by="Code")


dat = Var_correlation_between_census %>% filter(Code %in% unique(OAC_variables$Code))
dat = merge(dat, OAC_variables %>% select(Code, encoding), by="Code") %>% arrange(encoding)


gg_var_cor_between_census =   
  ggplot() +
 geom_col(dat, 
          mapping=aes(x=encoding, y=cor_between_years,fill=cor_between_years),col="grey50",size=0.2) + 
#  geom_col(dat %>% mutate(cor_between_years=abs(cor_between_years)), 
#           mapping=aes(x=Code, y=cor_between_years),fill="lightblue",col="grey50",size=0.2) + 
  coord_flip() +
  scale_fill_continuous(low="yellow", high="#ff293b", name="", limits=c(0, 1)) + 
 # scale_x_discrete( breaks = dat$encoding[seq(1,length(dat$encoding), by=10)]) + 
  geom_hline(yintercept=0.25, linetype="dashed", color = "grey45", size=0.6, alpha=0.7)+
  geom_hline(yintercept=0.5,  linetype="dashed", color = "grey45", size=0.6, alpha=0.7)+
  geom_hline(yintercept=0.75, linetype="dashed", color = "grey45", size=0.6, alpha=0.7)+
  geom_hline(yintercept=1,    linetype="dashed", color = "grey45", size=0.6, alpha=0.7)+
  labs(x="Variable", y="Correlation coefficient") +
  theme(axis.line = element_blank(),panel.background = element_blank(), 
        legend.text = element_text(size = 13), legend.title = element_text(size=15), 
        axis.text.x = element_text(size=13),  axis.text.y = element_text(size=8),
        axis.title = element_text(size=15), legend.position = "bottom", 
        legend.key.height = unit(0.7, "cm"),  legend.key.width = unit(1,"cm"))
  
  

gg_var_cor_between_census
ggsave(gg_var_cor_between_census,file="Plots/Correlation/Correlation_between_all_OAC_variavbles.png", 
       width=210, height=297, units = "mm", dpi=600)








###################################################################################
##############        ANALYSE CORRELATION BETWEEN AGEGROUPS        ###############
###################################################################################


age_codes = ((common_variables %>% filter(TableName21=="Age structure"))$Code)[-1]
ages_between_years = data.frame(variable2011 = age_codes[1:16], variable2021=age_codes[3:18], correlation=NA, p.value=NA)


for(i in 1:nrow(ages_between_years)){
  var1 = paste0(ages_between_years[i,"variable2011"], "_2011")
  var2 = paste0(ages_between_years[i,"variable2021"], "_2021")
  
  ages_between_years[i, "correlation"] =  round(cor.test(Census_both_perc[,var1], Census_both_perc[,var2])$estimate,2)
  ages_between_years[i, "p.value"] =  round(cor.test(Census_both_perc[,var1], Census_both_perc[,var2])$p.value,4)
}
ages_between_years$id = 1:nrow(ages_between_years)

ages_between_years = merge(ages_between_years, common_variables %>% select(Code, Name), by.x="variable2011", by.y="Code")
ages_between_years = merge(ages_between_years, common_variables %>% select(Code, Name), by.x="variable2021", by.y="Code")
ages_between_years = ages_between_years %>% rename(Variable2011 = Name.x, Variable2021=Name.y) %>% arrange(id)  %>% 
  select(Variable2011, Variable2021, correlation, p.value, id)












########################################################################################################
##################      IDENTIFYING HIGHLY CORRELATED VARIABLES IN BOTH CENSUS        ##################
########################################################################################################

### transform correlation matrix into lowe triangle (this way, pairs are not duplicated when filtering)
lower_Census_2011_cor_coef <- Census_2011_cor_coef
lower_Census_2011_cor_coef[lower.tri(Census_2011_cor_coef)]<-NA
lower_Census_2011_cor_coef<-as.matrix(as.data.frame(lower_Census_2011_cor_coef))
correlation_matrix_long_2011 = melt(lower_Census_2011_cor_coef)


lower_Census_2021_cor_coef <- Census_2021_cor_coef
lower_Census_2021_cor_coef[lower.tri(Census_2021_cor_coef)]<-NA
lower_Census_2021_cor_coef<-as.matrix(as.data.frame(lower_Census_2021_cor_coef))
correlation_matrix_long_2021 = melt(lower_Census_2021_cor_coef)



#### filter pairs of variables that exhibit strong correlations
under_threshold_2011 = correlation_matrix_long_2011 %>% filter(value>0.6 | value< (-0.6)) %>% 
  filter(Var1!=Var2)

under_threshold_2021 = correlation_matrix_long_2021 %>% filter(value>0.6 | value< (-0.6)) %>% 
  filter(Var1!=Var2)


### Retrieve names of the variables 
under_threshold_2011$Name1 = common_variables[under_threshold_2011$Var1,"Name" ]
under_threshold_2011$Name2 = common_variables[under_threshold_2011$Var2,"Name" ]

under_threshold_2021$Name1 = common_variables[under_threshold_2021$Var1,"Name" ]
under_threshold_2021$Name2 = common_variables[under_threshold_2021$Var2,"Name" ]




#### set of variables that are highly correlated in both years
intersect(paste(under_threshold_2011$Name1, " ||| ", under_threshold_2011$Name2), paste(under_threshold_2021$Name1," ||| ", under_threshold_2021$Name2))





##### For each variable, calculate a number of variables with which it is highly correlated
#### number of occurrence of problematic variables

under_threshold_2011_var_count = merge(under_threshold_2011 %>% group_by(Var1) %>% summarise(n=n()) %>% filter(n>1) %>% arrange(desc(n)),
                                       under_threshold_2011 %>% group_by(Var2) %>% summarise(n=n()) %>% filter(n>1) %>% arrange(desc(n)),
                                       by.x="Var1",by.y="Var2", all=T)

under_threshold_2011_var_count[is.na(under_threshold_2011_var_count)] = 0
under_threshold_2011_var_count = under_threshold_2011_var_count %>% mutate(n_all = n.x+n.y) %>% arrange(desc(n_all))
under_threshold_2011_var_count = merge(under_threshold_2011_var_count, original_variables, by.x="Var1", by.y="encoding") %>% 
  arrange(desc(n_all), TableName21) %>% select(Var1,n_all, Name, TableName21)



under_threshold_2021_var_count = merge(under_threshold_2021 %>% group_by(Var1) %>% summarise(n=n()) %>% filter(n>1) %>% arrange(desc(n)),
                                       under_threshold_2021 %>% group_by(Var2) %>% summarise(n=n()) %>% filter(n>1) %>% arrange(desc(n)),
                                       by.x="Var1",by.y="Var2", all=T)
under_threshold_2021_var_count[is.na(under_threshold_2021_var_count)] = 0
under_threshold_2021_var_count = under_threshold_2021_var_count %>% mutate(n_all = n.x+n.y)
under_threshold_2021_var_count = merge(under_threshold_2021_var_count, original_variables, by.x="Var1", by.y="encoding") %>% 
  arrange(desc(n_all), TableName21) %>% select(Var1,n_all, Name, TableName21)



under_threshold_between_census_var_count = merge(under_threshold_2011_var_count,under_threshold_2021_var_count, 
                                                 by="Var1", all=T) %>% mutate(n_all = n_all.x+n_all.y) 

under_threshold_between_census_var_count[is.na(under_threshold_between_census_var_count)] = 0
under_threshold_between_census_var_count = under_threshold_between_census_var_count %>% mutate(n_all = n_all.x+n_all.y) %>% 
  select(Var1, n_all, n_all.x, n_all.y) %>% arrange(desc(n_all))
colnames(under_threshold_between_census_var_count) = c("Var1", "H_cor_both", "H_cor_2011", "H_cor_2021")


under_threshold_between_census_var_count = merge(under_threshold_between_census_var_count, original_variables, by.x="Var1", by.y="encoding") %>% 
  arrange(desc(H_cor_both))  %>% select(Var1,H_cor_both,H_cor_2011, H_cor_2021, Name, TableName21)





#choosing_variables = merge(Var_correlation_between_census, original_variables, by.x="encoding", by.y="Var1", all=T) %>% 
#  select(encoding, var_name, table, cor_between_years,pvalue, ad_test_2021, skewness_2021, kurtosis_2021, H_cor_both, H_cor_2011, H_cor_2021) 
#
#choosing_variables[is.na(choosing_variables)] = 0


###### MAPS FOR POORLY CORRELATED VARIABLES


#saveRDS(choosing_variables, "choosing_variables.RData")




##################################################################################################
######################        STATISTICAL DISTRIBUTION OF VARIABLES           #####################
##################################################################################################

choosing_variables = readRDS("~/Desktop/PhD/GIT/OAC2021/choosing_variables.RData")

summary(choosing_variables)


# Census_2021_IHS_Range = read.csv("Data/Clean/Census_2021_IHS_Range_OA.csv")
# Census_2021_perc = read.csv("Data/Clean/Census_2021_perc_OA.csv")
# final_codes = read.csv("Data/Selected_Codes_Names.csv")




nrow(Census_2021_IHS_Range)
nrow(Census_2021_perc)
options(scipen=10000)


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
  
  
#######################################################
###########          AGED SCOTLAND         ############
#######################################################





