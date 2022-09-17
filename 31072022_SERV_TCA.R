
# Not needed to generage anymore this data.
# standard servings were taken from the SENC recommendations for Spain
# sex-specific servings were taken from the NCI data (see excel file)

setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/FoodGrams/ServingsData")

SERV<-read.csv("31032017_TCA_FoodMean_sex.csv", header=T, sep=";", na.strings="NA")


library(plyr)

SERV.MEAN<-ddply(SERV, .(Variable.name), summarize, 
                Serve.g=round(mean(Serving..gr., na.rm = TRUE),3), 
                Serve.g.M=round(mean(Serving.M, na.rm = TRUE),3), 
                serve.g.W=round(mean(Serving.W, na.rm = TRUE),3)) 

setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodNutrients")
write.csv(SERV, "31032017_TCA_FoodMean_sex.csv") ## Save data
