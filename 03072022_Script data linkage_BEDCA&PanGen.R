
##########################################
#### LINKAGE IDs Diet Pangen with BEDCA IDs
#### 06 March 2017
#### Noelia and Esther
###########################################


# START FROM HERE USING THE BEDCA TOTAL DATABASE WITH PANGEN´S IDs. The purpose is to calculate the mean nutrient values
# because some of the PanGen food items were grouped. In fact, BEDCA database had 939 food items whereas PanGen had only 97
# The reason is that for some foods we did not have any item in the questionnaire, whereas for some others, the dietary questionnaire combined multiple food items
# for example aubergine, cucumber and zucchini into one single food item
# take the Total BEDCA database (Tota0.Ids) and merge it with the PanGen correspoding food groups

# Total0.Ids<-load: 06032017_TCA_ BEDCA_withPanGen Ids.csv

Total0.Ids<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/FoodNutrient/06032017_TCA_ BEDCA_withPanGen Ids.csv", header=TRUE)

# calculate the average mean intakes of all nutrients at the same time by PanGen items, i.e. "Variable.name":

library(plyr)

FoodMean<-ddply(Total0.Ids, .(Variable.name), summarize, kJmean=round(mean(kJ, na.rm = TRUE),3), 
                kcalmean=round(mean(kcal, na.rm = TRUE),3), 
                Fat..gmean=round(mean(Fat..g, na.rm = TRUE),3), 
                Proteins..gmean=round(mean(Proteins..g, na.rm = TRUE),3), 
                Humidity..gmean=round(mean(Humidity..g, na.rm = TRUE),3), 
                Carbs..gmean=round(mean(Carbs..g, na.rm = TRUE),3), 
                Sucrose..gmean=round(mean(Sucrose..g, na.rm = TRUE),3), 
                Fiber..gmean=round(mean(Fiber..g, na.rm = TRUE),3), 
                Starch..gmean=round(mean(Starch..g, na.rm = TRUE),3), 
                Sugar..gmean=round(mean(Sugar..g, na.rm = TRUE),3), 
                Cholesterol..mgmean=round(mean(Cholesterol..mg, na.rm = TRUE),3), 
                Equivalentes.de.retinol..ug=round(mean(Equivalentes.de.retinol..ug, na.rm = TRUE),3), 
                Vitamina.D..ug=round(mean(Vitamina.D..ug, na.rm = TRUE),3), 
                Vitamina.E..mgmean=round(mean(Vitamina.E..mg, na.rm = TRUE),3), 
                Vitamina.B8..ugmean=round(mean(Vitamina.B8..ug, na.rm = TRUE),3), 
                Vitamina.B9..ugmean=round(mean(Vitamina.B9..ug, na.rm = TRUE),3), 
                Vitamina.B3..mgmean=round(mean(Vitamina.B3..mg, na.rm = TRUE),3), 
                Vitamina.B5..mgmean=round(mean(Vitamina.B5..mg, na.rm = TRUE),3), 
                Vitamina.B2..mgmean=round(mean(Vitamina.B2..mg, na.rm = TRUE),3), 
                Vitamina.B1..mgmean=round(mean(Vitamina.B1..mg, na.rm = TRUE),3), 
                Vitamina.B12..ugmean=round(mean(Vitamina.B12..ug, na.rm = TRUE),3), 
                Vitamina.B6..mgmean=round(mean(Vitamina.B6..mg, na.rm = TRUE),3), 
                Vitamina.C..mgmean=round(mean(Vitamina.C..mg, na.rm = TRUE),3), 
                Calcium..mgmean=round(mean(Calcium..mg, na.rm = TRUE),3), 
                Iron..mgmean=round(mean(Iron..mg, na.rm = TRUE),3), 
                Potasium..mgmean=round(mean(Potasium..mg, na.rm = TRUE),3), 
                Magnesium..mgmean=round(mean(Magnesium..mg, na.rm = TRUE),3), 
                Sodium..mgmean=round(mean(Sodium..mg, na.rm = TRUE),3), 
                f.sforo..mg.mean=round(mean(f.sforo..mg., na.rm = TRUE),3), 
                Copper..mgmean=round(mean(Copper..mg, na.rm = TRUE),3), 
                Iodide..ugmean=round(mean(Iodide..ug, na.rm = TRUE),3), 
                Selenium..ugmean=round(mean(Selenium..ug, na.rm = TRUE),3), 
                Zinc..mgmean=round(mean(Zinc..mg, na.rm = TRUE),3), 
                Linoleic..gmean=round(mean(Linoleic..g, na.rm = TRUE),3), 
                Linolenic..mgmean=round(mean(Linolenic..mg, na.rm = TRUE),3), 
                Araquidonic..gmean=round(mean(Araquidonic..g, na.rm = TRUE),3), 
                DHA..gmean=round(mean(DHA..g, na.rm = TRUE),3), 
                EPA..gmean=round(mean(EPA..g, na.rm = TRUE),3), 
                Estearic..gmean=round(mean(Estearic..g, na.rm = TRUE),3), 
                Lauric..gmean=round(mean(Lauric..g, na.rm = TRUE),3), 
                Miristic..gmean=round(mean(Miristic..g, na.rm = TRUE),3), 
                Polyunsaturated..gmean=round(mean(Polyunsaturated..g, na.rm = TRUE),3), 
                Saturated..gmean=round(mean(Saturated..g, na.rm = TRUE),3), 
                Trans..gmean=round(mean(Trans..g, na.rm = TRUE),3), 
                Alcohol..gmean=round(mean(Alcohol..g, na.rm = TRUE),3)) 
              
write.csv(FoodMean, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodNutrients/07032017_TCAtotFoodMean.csv") ## Save data





