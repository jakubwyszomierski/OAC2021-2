
setwd("~/Desktop")

library(dplyr)

Industry_2021 = read.csv("Industry_EW_2021.csv", skip=8)[,-1] %>% filter(!is.na(Total))
Industry_2011 = read.csv("Industry_NI_S_2011.csv", skip=7)[,-1] %>% filter(!is.na(All.categories..Industry))



tail(Industry_2021)
tail(Industry_2011)



colnames(Industry_2021) = c("Geography_Code", "Total", "ABDE", "C", "F", "GI", "HJ","KLMN", "OPQ", "RSTU")
colnames(Industry_2011) = c("Geography_Code", "Total", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L","M","N", "O", "P","Q", "RS","T","U")



Industry_2011 = Industry_2011 %>% mutate(ABDE = A + B + D + E, GI = G + I, HJ = H + J, KLMN = K + L + M + N, OPQ = O + P + Q, RSTU = RS + `T` + U) %>% 
  select(Geography_Code, Total, ABDE, C, `F`, GI,HJ, KLMN, OPQ, RSTU)



Industry = rbind(Industry_2021, Industry_2011)


### TRANSFORM DATA
Industry_perc=Industry




for(i in colnames(Industry)[-c(1:2)]){
  Industry_perc[,i] = round((Industry[,i] / Industry$Total), digits=4)*100
}

Industry_perc = Industry_perc %>% replace(is.na(.), 0) %>% select(-Total)




Industry_transformed = Industry_perc

Industry_transformed[,-1] = log(Industry_transformed[,-1] + sqrt(Industry_transformed[,-1]^2+1))

Range_0_to_1 <- function(x){(x-min(x))/(max(x)-min(x))}




Industry_transformed[,-1] = apply(Industry_transformed[,-1], 2, Range_0_to_1)
Industry_transformed[,-1] = round(Industry_transformed[,-1],4)

summary(Industry_transformed)
