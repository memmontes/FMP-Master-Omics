
## SCRIPT SCORE Anti-diabetic diet

# To calculate the DRRD score (DRRDS) (41), we assigned participants 
# a quintile value between 1 (intake consistent with the highest T2D risk)
# and 5 (for the lowest T2D risk) for each of 9 dietary factors: 
# cereal fiber, nuts, coffee (caffeinated and decaffeinated), whole fruits (raisins, 
# prunes, bananas, cantaloupwatermelons, fresh apples/pears, oranges, grapefruits,
# strawberries, blueberries, peaches/apricots/plums), and ratio of
# polyunsaturated to saturated fat in ascending order; and GI,
# trans fat, SSBs/fruit juices (apple, orange, grapefruit, and other
# fruit juices), and red and processed meats in descending order.
# We modified the previous version of the DRRDS (41) by
# incorporating data on fruits and fruit juices in relation to diabetes
# risk (35, 36): we added total fruits as a diabetes protective factor
# and combined fruit juices with SSBs as 1 adverse factor. The
# DRRDS (range = 9-45) was the sum of the quintile values.

# Protective dietary factors
# 1) cereal fiber: gra9700 whole grain bread
# 2) nuts: gra7900
# 3) coffee (caff and decaff): fre_coffee + fre_decoffee
# 4) whole fruits: gra_gallfru in food groups
# 5) ratio of polyunsaturated to saturated fat: TO BE OBTAINED

# Negative factors
# 6) GI: TO BE OBTAINED
# 7) trans fats: TO BE OBTAINED
# 8) SSBs/fruit juices: gra_sgsugbev in food groups
# 9) red and processed meats: gra_sgredmeat + gra_sgcuredprocessedmeat


# load the data from Microbiome:

setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodGrams")
FoodGrams<-read.csv("14072022_GramsDayAll with Zeros.csv")
FoodGramas.Group<-read.csv("14072022_GramsDayall_StandardServing(GROUPS).csv")

# load the data from Microbiome and pangen:

setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME_PANGEN/FoodGrams")
FoodGramsPM<-read.csv("20072022_FoodGramsPM_complete.csv")
FoodGramsPM.Group<-read.csv("20072022_FoodGramsPM(GROUPS).csv")

# load the data for nutrients:
setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodNutrients")
Nutr<-read.csv("20072022_Nutrients_Serving with GI.csv")
setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME_PANGEN/FoodNutrients")
NutrPM<-read.csv("20072022_FoodNutrientsPM.csv")

# some descriptives
summary(FoodGramsPM$gra9700)
summary(FoodGramsPM$gra7900)

# remove subjects with NAs for the score
FoodGramsPM<-FoodGramsPM[!is.na(FoodGramsPM$gra9700),]
FoodGramsPM.Group<-FoodGramsPM.Group[!is.na(FoodGramsPM.Group$gra9700),]
FoodGrams<-FoodGrams[!is.na(FoodGrams$gra9700),]
FoodGramas.Group<-FoodGramas.Group[!is.na(FoodGramas.Group$gra9700),]
Nutr<-Nutr[!is.na(Nutr$kJ),]
NutrPM<-NutrPM[!is.na(NutrPM$kJ),]

# metadata:
meta<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Microbiome_metadata/NEW UPDATE ALL VARIABLES/14052018_metadata_AllMicrobiome.csv", header=TRUE, sep=";")
metaPM<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/Esther Molina/EMBL_STAY/Imputation/ImputationOnAllData/01062018_imputeddataNew.csv", header=TRUE, sep=";") # ONLY FOR THE SPANISH DATA

## Calculate CUTPOINTS AMONG THE CONTROLS, and then aplly these cutpoints to all including cases:
## Consider pangen and microbiome in combination to test better...

meta<-meta[,c(2,3,4,5)]
FoodGrams<-merge(FoodGrams, meta, by="subject")
FoodGramas.Group<-merge(FoodGramas.Group, meta, by="subject")

FoodGrams_cc<-FoodGrams[FoodGrams$casecontrol==0,]
FoodGramsGroup_cc<-FoodGramas.Group[FoodGramas.Group$casecontrol==0,]

Nutr<-merge(Nutr, meta, by="subject")
Nutr_cc<-Nutr[Nutr$casecontrol==0,]

metaPM<-metaPM[,c(1,3,4)]
FoodGramsPM<-merge(FoodGramsPM, metaPM, by="subject")
FoodGramsPM.Group<-merge(FoodGramsPM.Group, metaPM, by="subject")

FoodGramsPM_cc<-FoodGramsPM[FoodGramsPM$casecontrol==0,]
FoodGramsGroupPM_cc<-FoodGramsPM.Group[FoodGramsPM.Group$casecontrol==0,]

NutrPM<-merge(NutrPM, metaPM, by="subject")
NutrPM_cc<-NutrPM[NutrPM$casecontrol==0,]



################################################################
## COMP 1: Component of cereal fiber

quantile(FoodGramsPM_cc$gra9700, probs = seq(0, 1, 0.50), na.rm = TRUE, names = TRUE, type = 7)
summary(FoodGramsPM_cc$gra9700)

#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(FoodGramsPM_cc$gra9700, probs = seq(0, 1, by = 0.5))), 
      labels=c("Q1","Q2"), include.lowest = TRUE, include.highest=TRUE)
}

#Add the quintile to the dataframe
fiber <- sapply(FoodGramsPM$gra9700, ApplyQuintiles)


summary(FoodGramsPM_cc$gra9700)
#aa<-FoodGramsPM_cc[FoodGramsPM_cc$gra9700>0,]

# need to compute this by the median

fiber<-ifelse(FoodGramsPM$gra9700<=0, 1, NA)
fiber<-ifelse(FoodGramsPM$gra9700>0, 2, fiber)
#fiber<-ifelse(FoodGramsPM$gra9700>=7, 3, fiber)
#fiber<-ifelse(FoodGramsPM$gra9700>=40, 4, fiber)

table(fiber, useNA="always")

fiber <- factor(fiber,
                    levels = c(1,2),
                    labels = c("Q1", "Q2"))

#Since the consumption was relatively infrequent
#in our population, we assigned a score of 5 to consumers over the median, 
# a score of 3 to consumers below the median
# and a score of 1 to non-consumers

# since there are many non-consumers, we do as with juices



##########################################################
## COMP 2: Component of nuts

quantile(FoodGramsPM_cc$gra7900, probs = seq(0, 1, 1/2), na.rm = TRUE, names = TRUE, type = 7)
summary(FoodGramsPM_cc$gra7900)

#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(FoodGramsPM_cc$gra7900, probs = seq(0, 1, by = 0.50))), 
      labels=c("Q1","Q2"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
nuts <- sapply(FoodGramsPM$gra7900, ApplyQuintiles)


# since there are many non-consumers, we do as with juices to have an additional cutpoint

#> summary(FoodGramsPM_cc$gra7900)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   2.008   5.482   4.016  75.000 

nuts<-ifelse(FoodGramsPM.Group$gra7900<=0, 1, NA)
nuts<-ifelse(FoodGramsPM.Group$gra7900>0 & FoodGramsPM.Group$gra7900<3, 2, nuts)
nuts<-ifelse(FoodGramsPM.Group$gra7900>=3, 3, nuts)


table(nuts, useNA="always")

nuts <- factor(nuts,
                 levels = c(1,2,3),
                 labels = c("Q1", "Q2", "Q3"))


##########################################################
## COMP 3: Component of coffee

FoodGramsPM$gra_decoffee<-ifelse(is.na(FoodGramsPM$gra_decoffee), 0, FoodGramsPM$gra_decoffee)
FoodGramsPM_cc$gra_decoffee<-ifelse(is.na(FoodGramsPM_cc$gra_decoffee), 0, FoodGramsPM_cc$gra_decoffee)


FoodGramsPM$coffee<-FoodGramsPM$gra_coffee+FoodGramsPM$gra_decoffee

FoodGramsPM_cc$coffee<-FoodGramsPM_cc$gra_coffee+FoodGramsPM_cc$gra_decoffee

quantile(FoodGramsPM_cc$coffee, probs = seq(0, 1, 0.3333), na.rm = TRUE, names = TRUE, type = 7)
summary(FoodGramsPM_cc$coffee)


#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(FoodGramsPM_cc$coffee, probs = seq(0, 1, by = 0.33))), 
      labels=c("Q1","Q2","Q3"), include.lowest = TRUE)
}


#Add the quintile to the dataframe
coffee <- sapply(FoodGramsPM$coffee, ApplyQuintiles)

coffee<-ifelse(FoodGramsPM$coffee>=600, 3, coffee)

coffee <- factor(coffee,
                 levels = c(1,2,3),
                 labels = c("Q1", "Q2", "Q3"))


# need to assign cutpoints manually

#coffee<-ifelse(FoodGramsPM$coffee<=0, 1, NA)
#coffee<-ifelse(FoodGramsPM$coffee>0, 2, coffee)
#coffee<-ifelse(FoodGramsPM$coffee>=400, 3, coffee)
#coffee<-ifelse(FoodGramsPM$coffee>=600, 4, coffee)

table(coffee, useNA="always")

#coffee <- factor(coffee,
#                levels = c(1,2,3,4),
#                labels = c("Q1", "Q2", "Q3", "Q4"))



##########################################################
## COMP 4: for whole fruits: gra_gallfru

quantile(FoodGramsGroupPM_cc$gra_gallfru, probs = seq(0, 1, 0.20), na.rm = TRUE, names = TRUE, type = 7)
summary(FoodGramsGroupPM_cc$gra_gallfru)

FoodGramsGroupPM_cc$fru<-FoodGramsGroupPM_cc$gra_gallfru+FoodGramsGroupPM_cc$gra14500
FoodGramas.Group$fru<-FoodGramas.Group$gra_gallfru+FoodGramas.Group$gra14500
FoodGramsPM.Group$fru<-FoodGramsPM.Group$gra_gallfru+FoodGramsPM.Group$gra14500

#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(FoodGramsGroupPM_cc$fru, probs = seq(0, 1, by = 0.20))), 
      labels=c("Q1","Q2","Q3", "Q4", "Q5"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
fruits <- sapply(FoodGramsPM.Group$fru, ApplyQuintiles)

table(fruits, useNA="always")

summary(FoodGramsGroupPM_cc$fru)

summary(FoodGramsPM.Group$fru)

#is.na(fruits)

fruits<-ifelse(FoodGramsPM.Group$fru>1000, 5, fruits)

fruits<- factor(fruits,
                 levels = c(1,2,3,4,5),
                 labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))


table(fruits, useNA="always")


###########################################################
## COMP 5: ratio of poly and saturated fats


NutrPM_cc$ratioFat<-NutrPM_cc$Polyunsaturated.g/NutrPM_cc$Saturated.g
Nutr$ratioFat<-Nutr$Polyunsaturated.g/Nutr$Saturated.g
NutrPM$ratioFat<-NutrPM$Polyunsaturated.g/NutrPM$Saturated.g


quantile(NutrPM_cc$ratioFat, probs = seq(0, 1, 0.20), na.rm = TRUE, names = TRUE, type = 7)
summary(NutrPM_cc$ratioFat)

#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(NutrPM_cc$ratioFat, probs = seq(0, 1, by = 0.20))), 
      labels=c("Q1","Q2","Q3", "Q4", "Q5"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
ratio <- sapply(NutrPM$ratioFat, ApplyQuintiles)

table(ratio, useNA="always")

ratio<-ifelse(is.na(ratio), 1, ratio)

ratio<- factor(ratio,
                levels = c(1,2,3,4,5),
                labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))


table(ratio, useNA="always")


###########################################################
## COMP 6: GI

quantile(Nutr$GI, probs = seq(0, 1, 0.20), na.rm = TRUE, names = TRUE, type = 7)
summary(Nutr$GI)

#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(Nutr$GI, probs = seq(0, 1, by = 0.20))), 
      labels=c("Q1","Q2","Q3", "Q4", "Q5"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
gi <- sapply(Nutr$GI, ApplyQuintiles)

# Another way of GI based on sugars:


quantile(FoodGramsGroupPM_cc$gra_gsugar, probs = seq(0, 1, 0.20), na.rm = TRUE, names = TRUE, type = 7)
summary(FoodGramsGroupPM_cc$gra_gsugar)

#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(FoodGramsGroupPM_cc$gra_gsugar, probs = seq(0, 1, by = 0.25))), 
      labels=c("Q1","Q2","Q3", "Q4"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
gi <- sapply(FoodGramsPM.Group$gra_gsugar, ApplyQuintiles)

table(gi, useNA="always")

gi<-ifelse(FoodGramsPM.Group$gra_gsugar>=60, 4, gi)

gi<- factor(gi,
               levels = c(1,2,3,4),
               labels = c("Q1", "Q2", "Q3", "Q4"))


table(gi, useNA="always")


# with sugar or sucrose:

summary(NutrPM_cc$Sucrose.g)
summary(NutrPM_cc$Sugar.g)

#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(NutrPM_cc$Sucrose.g, probs = seq(0, 1, by = 0.20))), 
      labels=c("Q1","Q2","Q3", "Q4", "Q5"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
gi <- sapply(NutrPM$Sucrose.g, ApplyQuintiles)

table(gi, useNA="always")

gi<-ifelse(NutrPM$Sucrose.g>90, 5, gi)

gi<- factor(gi,
            levels = c(1,2,3,4,5),
            labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))


table(gi, useNA="always")


# With sugar:

#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(NutrPM_cc$Sugar.g, probs = seq(0, 1, by = 0.20))), 
      labels=c("Q1","Q2","Q3", "Q4", "Q5"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
#gi <- sapply(NutrPM$Sugar.g, ApplyQuintiles)

table(gi, useNA="always")


#gi<-ifelse(NutrPM$Sugar.g>=100, 5, gi)

#gi<- factor(gi,
#            levels = c(1,2,3,4,5),
#            labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))


table(gi, useNA="always")


###########################################################
## COMP 7: trans fats

quantile(NutrPM_cc$Trans.g, probs = seq(0, 1, 0.20), na.rm = TRUE, names = TRUE, type = 7)
summary(NutrPM_cc$Trans.g)
summary(NutrPM$Trans.g)


#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(NutrPM_cc$Trans.g, probs = seq(0, 1, by = 0.20))), 
      labels=c("Q1","Q2","Q3", "Q4", "Q5"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
trans <- sapply(NutrPM$Trans.g, ApplyQuintiles)

table(trans, useNA="always")

trans<-ifelse(NutrPM$Trans.g>=8.5, 5, trans)
trans<-ifelse(NutrPM$Trans.g<=0.12, 1, trans)


trans<- factor(trans,
               levels = c(1,2,3,4,5),
               labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))


table(trans, useNA="always")



###########################################################
## COMP 8: SSBs/fruit juices: sgsugbev in food groups

FoodGramsGroupPM_cc$ju<-FoodGramsGroupPM_cc$gra14300+FoodGramsGroupPM_cc$gra14400+FoodGramsGroupPM_cc$gra14700
FoodGramas.Group$ju<-FoodGramas.Group$gra14300+FoodGramas.Group$gra14400+FoodGramas.Group$gra14700
FoodGramsPM.Group$ju<-FoodGramsPM.Group$gra14300+FoodGramsPM.Group$gra14400+FoodGramsPM.Group$gra14700


quantile(FoodGramsGroupPM_cc$ju, probs = seq(0, 1, 0.333), na.rm = TRUE, names = TRUE, type = 7)
summary(FoodGramsGroupPM_cc$ju)

#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(FoodGramsGroupPM_cc$ju, probs = seq(0, 1, by = 0.333))), 
      labels=c("Q1","Q2","Q3"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
juices <- sapply(FoodGramsPM.Group$ju, ApplyQuintiles)

# Turati paper:
#Since the consumption of sugarsweetened
#beverages and fruit juices was relatively infrequent
#in our population [i.e., 605 subjects (61.9%) did not
#                   drink either sugar-sweetened beverages or fruit juices], we
#assigned a score of 5 to non-drinkers, a score of 3 to drinkers
#of ??? 2.5 drinks per week (i.e., the median value among
#                          drinking controls), and a score of 1 to drinkers of more than
#2.5 drinks per week.

# let´s try like this:

#> summary(FoodGramsGroupPM_cc$ju)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0     0.0    28.0   104.1   136.5  1500.0

#> summary(FoodGramsGroupPM_cc$gra_sgjuice)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    0.00   28.00   88.46  172.00 1425.00 
#> summary(FoodGramsGroupPM_cc$gra_sgsugbev)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0      14      86     131     200    1425 
#> summary(FoodGramsGroupPM_cc$gra_sgsoftdr)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0     0.0    14.0    66.4    56.0  1400.0

summary(FoodGramsGroupPM_cc$gra_sgsugbev)

#juices<-ifelse(FoodGramsPM.Group$gra_sgsugbev<=0, 1, NA)
#juices<-ifelse(FoodGramsPM.Group$gra_sgsugbev>0 & FoodGramsPM.Group$gra_sgsugbev<86, 2, juices)
#juices<-ifelse(FoodGramsPM.Group$gra_sgsugbev>=86, 3, juices)



#table(juices, useNA="always")

#juices <- factor(juices,
#                 levels = c(1,2,3),
#                 labels = c("Q1", "Q2", "Q3"))


# for sugar-sweetened beverages specifically: fre14300

summary(FoodGramsGroupPM_cc$gra14300)
summary(FoodGramsPM.Group$gra14300)

juices<-ifelse(FoodGramsPM.Group$gra14300<=0, 1, NA)
juices<-ifelse(FoodGramsPM.Group$gra14300>0 & FoodGramsPM.Group$gra14300<42.54, 2, juices)
juices<-ifelse(FoodGramsPM.Group$gra14300>=42.54, 3, juices)

table(juices, useNA="always")

juices <- factor(juices,
                 levels = c(1,2,3),
                 labels = c("Q1", "Q2", "Q3"))

summary(FoodGramsGroupPM_cc$gra14500) # natural juices
summary(FoodGramsPM.Group$gra14500)

summary(FoodGramsGroupPM_cc$gra14700) # comercial juices
summary(FoodGramsPM.Group$gra14700)


#FoodGramsGroupPM_cc$ju<-FoodGramsGroupPM_cc$gra14300+FoodGramsGroupPM_cc$gra14700
#FoodGramas.Group$ju<-FoodGramas.Group$gra14300+FoodGramas.Group$gra14700
#FoodGramsPM.Group$ju<-FoodGramsPM.Group$gra14300+FoodGramsPM.Group$gra14700

summary(FoodGramsGroupPM_cc$ju) # comercial juices
summary(FoodGramsPM.Group$ju)

#juices<-ifelse(FoodGramsPM.Group$ju<=0, 1, NA)
#juices<-ifelse(FoodGramsPM.Group$ju>0 & FoodGramsPM.Group$ju<80.0, 2, juices)
#juices<-ifelse(FoodGramsPM.Group$ju>=80.0, 3, juices)

#table(juices, useNA="always")

#juices <- factor(juices,
#                 levels = c(1,2,3),
#                 labels = c("Q1", "Q2", "Q3"))


###########################################################
## COMP 9: meats: gra_sgredmeat + gra_sgcuredprocessedmeat

FoodGramas.Group$meats<-FoodGramas.Group$gra_sgredmeat+FoodGramas.Group$gra_sgcuredprocessedmeat

FoodGramsGroupPM_cc$meats<-FoodGramsGroupPM_cc$gra_sgredmeat+FoodGramsGroupPM_cc$gra_sgcuredprocessedmeat

FoodGramsPM.Group$meats<-FoodGramsPM.Group$gra_sgredmeat+FoodGramsPM.Group$gra_sgcuredprocessedmeat

quantile(FoodGramsGroupPM_cc$meats, probs = seq(0, 1, 0.20), na.rm = TRUE, names = TRUE, type = 7)
summary(FoodGramsGroupPM_cc$meats)

#function to break them into quintilesa
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(FoodGramsGroupPM_cc$meats, probs = seq(0, 1, by = 0.20))), 
      labels=c("Q1","Q2","Q3", "Q4", "Q5"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
meats <- sapply(FoodGramsPM.Group$meats, ApplyQuintiles)

table(meats, useNA="always")

##########################################################
## compute the score: 5 points lowest T2D risk and 1 point the highest risk

identical(FoodGramas.Group$subject, FoodGrams$subject)

# positive components (5)

#FoodGrams$SC_fiber<-NA
#FoodGrams$SC_fiber<-ifelse(fiber=="Q3", 5, NA)
#FoodGrams$SC_fiber<-ifelse(fiber=="Q2", 2.5, FoodGrams$SC_fiber)
#FoodGrams$SC_fiber<-ifelse(fiber=="Q1", 1, FoodGrams$SC_fiber)

#FoodGrams$SC_nuts<-NA
#FoodGrams$SC_nuts<-ifelse(nuts=="Q2", 5, NA)
#FoodGrams$SC_nuts<-ifelse(nuts=="Q1", 1, FoodGrams$SC_nuts)

#FoodGrams$SC_fruits<-NA
#FoodGrams$SC_fruits<-ifelse(fruits=="Q5", 5, NA)
#FoodGrams$SC_fruits<-ifelse(fruits=="Q4", 4, FoodGrams$SC_fruits)
#FoodGrams$SC_fruits<-ifelse(fruits=="Q3", 3, FoodGrams$SC_fruits)
#FoodGrams$SC_fruits<-ifelse(fruits=="Q2", 2, FoodGrams$SC_fruits)
#FoodGrams$SC_fruits<-ifelse(fruits=="Q1", 1, FoodGrams$SC_fruits)


#FoodGrams$SC_coffee<-NA
#FoodGrams$SC_coffee<-ifelse(coffee=="Q4", 5, NA)
#FoodGrams$SC_coffee<-ifelse(coffee=="Q3", 3.5, FoodGrams$SC_coffee)
#FoodGrams$SC_coffee<-ifelse(coffee=="Q2", 2.5, FoodGrams$SC_coffee)
#FoodGrams$SC_coffee<-ifelse(coffee=="Q1", 1, FoodGrams$SC_coffee)

#FoodGrams$SC_ratio<-NA
#FoodGrams$SC_ratio<-ifelse(ratio=="Q5", 5, NA)
#FoodGrams$SC_ratio<-ifelse(ratio=="Q4", 4, FoodGrams$SC_ratio)
#FoodGrams$SC_ratio<-ifelse(ratio=="Q3", 3, FoodGrams$SC_ratio)
#FoodGrams$SC_ratio<-ifelse(ratio=="Q2", 2, FoodGrams$SC_ratio)
#FoodGrams$SC_ratio<-ifelse(ratio=="Q1", 1, FoodGrams$SC_ratio)

#negative components (4)

#FoodGrams$SC_gi<-NA
#FoodGrams$SC_gi<-ifelse(gi=="Q5", 1, NA)
#FoodGrams$SC_gi<-ifelse(gi=="Q4", 2, FoodGrams$SC_gi)
#FoodGrams$SC_gi<-ifelse(gi=="Q3", 3, FoodGrams$SC_gi)
#FoodGrams$SC_gi<-ifelse(gi=="Q2", 4, FoodGrams$SC_gi)
#FoodGrams$SC_gi<-ifelse(gi=="Q1", 5, FoodGrams$SC_gi)


#FoodGrams$SC_trans<-NA
#FoodGrams$SC_trans<-ifelse(trans=="Q5", 1, NA)
#FoodGrams$SC_trans<-ifelse(trans=="Q4", 2, FoodGrams$SC_trans)
#FoodGrams$SC_trans<-ifelse(trans=="Q3", 3, FoodGrams$SC_trans)
#FoodGrams$SC_trans<-ifelse(trans=="Q2", 4, FoodGrams$SC_trans)
#FoodGrams$SC_trans<-ifelse(trans=="Q1", 5, FoodGrams$SC_trans)

#FoodGrams$SC_meats<-NA
#FoodGrams$SC_meats<-ifelse(meats=="Q5", 1, NA)
#FoodGrams$SC_meats<-ifelse(meats=="Q4", 2, FoodGrams$SC_meats)
#FoodGrams$SC_meats<-ifelse(meats=="Q3", 3, FoodGrams$SC_meats)
#FoodGrams$SC_meats<-ifelse(meats=="Q2", 4, FoodGrams$SC_meats)
#FoodGrams$SC_meats<-ifelse(meats=="Q1", 5, FoodGrams$SC_meats)

#FoodGrams$SC_juices<-NA
#FoodGrams$SC_juices<-ifelse(juices=="Q1", 5, NA)
#FoodGrams$SC_juices<-ifelse(juices=="Q2", 3.5, FoodGrams$SC_juices)
#FoodGrams$SC_juices<-ifelse(juices=="Q3", 2.5, FoodGrams$SC_juices)
#FoodGrams$SC_juices<-ifelse(juices=="Q4", 1, FoodGrams$SC_juices)


#FoodGrams$SC_juices<-NA
#FoodGrams$SC_juices<-ifelse(juices=="Q1", 5, NA)
#FoodGrams$SC_juices<-ifelse(juices=="Q2", 2.5, FoodGrams$SC_juices)
#FoodGrams$SC_juices<-ifelse(juices=="Q3", 1, FoodGrams$SC_juices)


# same for all:


# positive components (5)

#FoodGramsPM$SC_fiber<-NA
#FoodGramsPM$SC_fiber<-ifelse(fiber=="Q4", 5, NA)
#FoodGramsPM$SC_fiber<-ifelse(fiber=="Q3", 3, FoodGramsPM$SC_fiber)
#FoodGramsPM$SC_fiber<-ifelse(fiber=="Q2", 2, FoodGramsPM$SC_fiber)
#FoodGramsPM$SC_fiber<-ifelse(fiber=="Q1", 1, FoodGramsPM$SC_fiber)

FoodGramsPM$SC_fiber<-NA
FoodGramsPM$SC_fiber<-ifelse(fiber=="Q2", 5, NA)
FoodGramsPM$SC_fiber<-ifelse(fiber=="Q1", 1, FoodGramsPM$SC_fiber)

FoodGramsPM$SC_nuts<-NA
FoodGramsPM$SC_nuts<-ifelse(nuts=="Q3", 5, NA)
FoodGramsPM$SC_nuts<-ifelse(nuts=="Q2", 3, FoodGramsPM$SC_nuts)
FoodGramsPM$SC_nuts<-ifelse(nuts=="Q1", 1, FoodGramsPM$SC_nuts)

FoodGramsPM$SC_fruits<-NA
FoodGramsPM$SC_fruits<-ifelse(fruits=="Q5", 5, NA)
FoodGramsPM$SC_fruits<-ifelse(fruits=="Q4", 4, FoodGramsPM$SC_fruits)
FoodGramsPM$SC_fruits<-ifelse(fruits=="Q3", 3, FoodGramsPM$SC_fruits)
FoodGramsPM$SC_fruits<-ifelse(fruits=="Q2", 2, FoodGramsPM$SC_fruits)
FoodGramsPM$SC_fruits<-ifelse(fruits=="Q1", 1, FoodGramsPM$SC_fruits)

#FoodGramsPM$SC_coffee<-NA
#FoodGramsPM$SC_coffee<-ifelse(coffee=="Q4", 5, NA)
#FoodGramsPM$SC_coffee<-ifelse(coffee=="Q3", 3, FoodGramsPM$SC_coffee)
#FoodGramsPM$SC_coffee<-ifelse(coffee=="Q2", 2, FoodGramsPM$SC_coffee)
#FoodGramsPM$SC_coffee<-ifelse(coffee=="Q1", 1, FoodGramsPM$SC_coffee)

FoodGramsPM$SC_coffee<-NA
FoodGramsPM$SC_coffee<-ifelse(coffee=="Q3", 5, NA)
FoodGramsPM$SC_coffee<-ifelse(coffee=="Q2", 3, FoodGramsPM$SC_coffee)
FoodGramsPM$SC_coffee<-ifelse(coffee=="Q1", 1, FoodGramsPM$SC_coffee)


FoodGramsPM$SC_ratio<-NA
FoodGramsPM$SC_ratio<-ifelse(ratio=="Q5", 5, NA)
FoodGramsPM$SC_ratio<-ifelse(ratio=="Q4", 4, FoodGramsPM$SC_ratio)
FoodGramsPM$SC_ratio<-ifelse(ratio=="Q3", 3, FoodGramsPM$SC_ratio)
FoodGramsPM$SC_ratio<-ifelse(ratio=="Q2", 2, FoodGramsPM$SC_ratio)
FoodGramsPM$SC_ratio<-ifelse(ratio=="Q1", 1, FoodGramsPM$SC_ratio)

#negative components (4)

FoodGramsPM$SC_gi<-NA
FoodGramsPM$SC_gi<-ifelse(gi=="Q5", 1, NA)
FoodGramsPM$SC_gi<-ifelse(gi=="Q4", 2, FoodGramsPM$SC_gi)
FoodGramsPM$SC_gi<-ifelse(gi=="Q3", 3, FoodGramsPM$SC_gi)
FoodGramsPM$SC_gi<-ifelse(gi=="Q2", 4, FoodGramsPM$SC_gi)
FoodGramsPM$SC_gi<-ifelse(gi=="Q1", 5, FoodGramsPM$SC_gi)

#FoodGramsPM$SC_gi<-NA
#FoodGramsPM$SC_gi<-ifelse(gi=="Q4", 1, NA)
#FoodGramsPM$SC_gi<-ifelse(gi=="Q3", 2, FoodGramsPM$SC_gi)
#FoodGramsPM$SC_gi<-ifelse(gi=="Q2", 3, FoodGramsPM$SC_gi)
#FoodGramsPM$SC_gi<-ifelse(gi=="Q1", 5, FoodGramsPM$SC_gi)


FoodGramsPM$SC_trans<-NA
FoodGramsPM$SC_trans<-ifelse(trans=="Q5", 1, NA)
FoodGramsPM$SC_trans<-ifelse(trans=="Q4", 2, FoodGramsPM$SC_trans)
FoodGramsPM$SC_trans<-ifelse(trans=="Q3", 3, FoodGramsPM$SC_trans)
FoodGramsPM$SC_trans<-ifelse(trans=="Q2", 4, FoodGramsPM$SC_trans)
FoodGramsPM$SC_trans<-ifelse(trans=="Q1", 5, FoodGramsPM$SC_trans)

FoodGramsPM$SC_meats<-NA
FoodGramsPM$SC_meats<-ifelse(meats=="Q5", 1, NA)
FoodGramsPM$SC_meats<-ifelse(meats=="Q4", 2, FoodGramsPM$SC_meats)
FoodGramsPM$SC_meats<-ifelse(meats=="Q3", 3, FoodGramsPM$SC_meats)
FoodGramsPM$SC_meats<-ifelse(meats=="Q2", 4, FoodGramsPM$SC_meats)
FoodGramsPM$SC_meats<-ifelse(meats=="Q1", 5, FoodGramsPM$SC_meats)

#FoodGramsPM$SC_juices<-NA
#FoodGramsPM$SC_juices<-ifelse(juices=="Q1", 5, NA)
#FoodGramsPM$SC_juices<-ifelse(juices=="Q2", 3.5, FoodGramsPM$SC_juices)
#FoodGramsPM$SC_juices<-ifelse(juices=="Q3", 2.5, FoodGramsPM$SC_juices)
#FoodGramsPM$SC_juices<-ifelse(juices=="Q4", 1, FoodGramsPM$SC_juices)


FoodGramsPM$SC_juices<-NA
FoodGramsPM$SC_juices<-ifelse(juices=="Q1", 5, NA)
FoodGramsPM$SC_juices<-ifelse(juices=="Q2", 3, FoodGramsPM$SC_juices)
FoodGramsPM$SC_juices<-ifelse(juices=="Q3", 1, FoodGramsPM$SC_juices)

#FoodGramsPM$SC_juices<-NA
#FoodGramsPM$SC_juices<-ifelse(juices=="Q5", 1, NA)
#FoodGramsPM$SC_juices<-ifelse(juices=="Q4", 2, FoodGramsPM$SC_juices)
#FoodGramsPM$SC_juices<-ifelse(juices=="Q3", 3, FoodGramsPM$SC_juices)
#FoodGramsPM$SC_juices<-ifelse(juices=="Q2", 4, FoodGramsPM$SC_juices)
#FoodGramsPM$SC_juices<-ifelse(juices=="Q1", 5, FoodGramsPM$SC_juices)


# the final score

#FoodGrams$SCORE<-FoodGrams$SC_fiber+FoodGrams$SC_nuts+FoodGrams$SC_fruits+FoodGrams$SC_coffee+
#                 FoodGrams$SC_ratio+FoodGrams$SC_gi+FoodGrams$SC_trans+FoodGrams$SC_meats+FoodGrams$SC_juices


#FoodGramas.Group$SCORE<-FoodGrams$SCORE

#FoodGramas.Group$SC_fiber<-FoodGrams$SC_fiber
#FoodGramas.Group$SC_nuts<-FoodGrams$SC_nuts
#FoodGramas.Group$SC_fruits<-FoodGrams$SC_fruits
#FoodGramas.Group$SC_coffee<-FoodGrams$SC_coffee
#FoodGramas.Group$SC_ratio<-FoodGrams$SC_ratio
#FoodGramas.Group$SC_gi<-FoodGrams$SC_gi
#FoodGramas.Group$SC_trans<-FoodGrams$SC_trans
#FoodGramas.Group$SC_meats<-FoodGrams$SC_meats
#FoodGramas.Group$SC_juices<-FoodGrams$SC_juice


# same for all:


FoodGramsPM$SCORE<-FoodGramsPM$SC_fiber+FoodGramsPM$SC_nuts+FoodGramsPM$SC_fruits+FoodGramsPM$SC_coffee+FoodGramsPM$SC_ratio+FoodGramsPM$SC_gi+FoodGramsPM$SC_trans+FoodGramsPM$SC_meats+FoodGramsPM$SC_juices

#FoodGramsPM$SCORE<-FoodGramsPM$SC_fiber+FoodGramsPM$SC_nuts+FoodGramsPM$SC_fruits+FoodGramsPM$SC_ratio+FoodGramsPM$SC_trans+FoodGramsPM$SC_meats+FoodGramsPM$SC_juices

FoodGramsPM.Group$SCORE<-FoodGramsPM$SCORE

FoodGramsPM.Group$SC_fiber<-FoodGramsPM$SC_fiber
FoodGramsPM.Group$SC_nuts<-FoodGramsPM$SC_nuts
FoodGramsPM.Group$SC_fruits<-FoodGramsPM$SC_fruits
FoodGramsPM.Group$SC_coffee<-FoodGramsPM$SC_coffee
FoodGramsPM.Group$SC_ratio<-FoodGramsPM$SC_ratio
FoodGramsPM.Group$SC_gi<-FoodGramsPM$SC_gi
FoodGramsPM.Group$SC_trans<-FoodGramsPM$SC_trans
FoodGramsPM.Group$SC_meats<-FoodGramsPM$SC_meats
FoodGramsPM.Group$SC_juices<-FoodGramsPM$SC_juices

summary(FoodGramsPM.Group$SCORE)

# also add the components

FoodGramsPM.Group$fruits<-medfruits
FoodGramsPM.Group$vege<-medvege
FoodGramsPM.Group$leg<-medleg
FoodGramsPM.Group$cer<-medcer
FoodGramsPM.Group$fish<-medfish
FoodGramsPM.Group$oil<-medoil
FoodGramsPM.Group$meat<-medmeat
FoodGramsPM.Group$fish<-medfish

FoodGramsPM$fruits<-medfruits
FoodGramsPM$vege<-medvege
FoodGramsPM$leg<-medleg
FoodGramsPM$cer<-medcer
FoodGramsPM$fish<-medfish
FoodGramsPM$oil<-medoil
FoodGramsPM$meat<-medmeat
FoodGramsPM$fish<-medfish



# descriptives:

library(compareGroups)
library(kableExtra)

# A higher score should lead to lower T2D risk, and in turn, to a lower PDAC risk

# Verify is associated with case-control

meta<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Microbiome_metadata/NEW UPDATE ALL VARIABLES/14052018_metadata_AllMicrobiome.csv", header=TRUE, sep=";")
metaPM<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/Esther Molina/EMBL_STAY/Imputation/ImputationOnAllData/01062018_imputeddataNew.csv", header=TRUE, sep=";") # ONLY FOR THE SPANISH DATA

sc<-FoodGramsPM[,c("subject", "SCORE", "SC_fiber",      "SC_nuts",      
                  "SC_fruits",     "SC_coffee",     "SC_ratio",      "SC_gi",         "SC_trans",     
                 "SC_meats",      "SC_juices", "TIPO", "fiber",         "nuts",          "fruits",        "ratio",         "gi",           
                 "trans",         "meats",         "juices")]

meta<-merge(metaPM, sc, by="subject")

sc<-NutrPM[,c("subject", "kcal", "Sugar.g")]

meta<-merge(meta, sc, by="subject")


table(meta$final_eligibility)

meta<-meta[meta$casecontrol!=2,]

#meta<-meta[meta$final_eligibility=="ELIGIBLE",]

#### DESCRIPTIVES:

res<-compareGroups(casecontrol~., data=FoodGramsPM.Group[FoodGramsPM.Group$casecontrol!=2,], ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab


res<-compareGroups(alldiab~., data=meta[meta$casecontrol==0 & meta$diabcat!=1,c(4,11,44:54)], ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab
export2md(restab, strip = TRUE, first.strip = TRUE) 


res<-compareGroups(alldiab~., data=meta[meta$casecontrol==0,c(4,11,44:54)], ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab
export2md(restab, strip = TRUE, first.strip = TRUE) 


res<-compareGroups(obese~., data=meta[meta$casecontrol==0,c(4,11,13,44:54)], ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab
export2md(restab, strip = TRUE, first.strip = TRUE) 


res<-compareGroups(casecontrol~., data=meta[meta$diabcat!=1,c(4,11,13,44:54)], ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab
export2md(restab, strip = TRUE, first.strip = TRUE) 



## RESCTRICTED TO THE MICOBIOME STUDY:

res<-compareGroups(casecontrol~., data=meta[meta$diabcat!=1 & meta$MetaG==1,c(4,11,13,44:54)], ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab
export2md(restab, strip = TRUE, first.strip = TRUE) 


##########################################
### Associations:

# with diabetes:

fit<-glm(alldiab~SCORE, data=meta[meta$casecontrol==0 & meta$diabcat!=1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))


fit<-glm(alldiab~SCORE+agec+factor(sex)+factor(center)+kcal+factor(cpy1t)+factor(obese), data=meta[meta$casecontrol==0 & meta$diabcat!=1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))


# with obesity:

fit<-glm(obese~SCORE, data=meta[meta$casecontrol==0 & meta$diabcat!=1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))

fit<-glm(obese~SCORE+agec+factor(sex)+factor(center)+kcal+factor(cpy1t)+factor(alldiab), data=meta[meta$casecontrol==0 & meta$diabcat!=1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))


# with PDAC:


fit<-glm(casecontrol~SCORE, data=meta, family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))


fit<-glm(casecontrol~SCORE+agec+factor(sex)+factor(center)+kcal+factor(cpy1t)+factor(FHPDAC)+factor(diabcat), data=meta, family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))


#restricted to LSD
fit<-glm(casecontrol~SCORE+agec+factor(sex)+factor(center)+kcal+factor(cpy1t)+factor(FHPDAC), data=meta[meta$diabcat!=1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))


## restricted to the microbiome study:

fit<-glm(casecontrol~SCORE, data=meta[meta$MetaG==1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))


fit<-glm(casecontrol~SCORE+agec+factor(sex)+factor(center)+kcal+factor(cpy1t)+factor(FHPDAC) + factor(diabcat), data=meta[meta$MetaG==1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))

fit<-glm(casecontrol~SCORE+agec+factor(sex)+factor(center)+kcal+factor(cpy1t)+factor(FHPDAC), data=meta[meta$MetaG==1 & meta$diabcat!=1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))


############################################################
############################################################
## MEDITERRANEAN DIET:


# The relative Mediterranean diet score (rMED), as previously
# applied in other EPIC studies (Buckland et al, 2010, 2013) and similar
# in concept to the original MD score (Trichopoulou et al, 2003), was
# used to estimate level of conformity to theMD. It is an 18-point scale
# that incorporates nine selected components of the MD. Each
# component was calculated as a function of energy density (g per1000 kcal per day), 
# using the nutrient density model (Willet el al, 1997), 
# and then divided into country-specific tertiles of intakes (except
# for olive oil). For the six components presumed to fit the MD; fruits
# (including nuts and seeds), vegetables (excluding potatoes), legumes, fish
# (including seafood), olive oil and cereals (white and nonwhite), a
# score of 0-2 points was assigned to the first (0 points), second (1
#  point) and third (2 points) tertile of intake, respectively.

# The scoring
# scheme for olive oil consisted of assigning 0 points to non-consumers,
# 1 point for participants below the median of intake and 2 points for
# levels of intake equal or above this median. For the 2 components
# presumed not to fit MD, meat (including meat products) and
# dairy products, the scoring was reversed (first, second and third tertile:
# 2, 1 and 0 points, respectively). Because alcohol consumption has been
# potentially associated with pancreatic cancer (Maisonneuve and
# Lowenfels, 2015), the alcohol component (the ninth component) was
# removed from the score and the non-alcohol MD score (arMED) was
# used instead (Buckland et al, 2013). Thus, the range of the arMED
# score contained eight components and the point scale ranks from 0 to
# , whereby 0 represents the lowest adherence to the MD pattern and
#  the highest adherence. The arMED score was further classified into
# low (0-5 points), medium (6-9 points) or high (10-16 points)
# adherence levels based on the previously published cutoff points
# (Buckland et al, 2013).


# COMPONENET 1: Fruits including nuts and seeds

FoodGramsPM.Group$medfru<-FoodGramsPM.Group$gra6200+FoodGramsPM.Group$gra6300+FoodGramsPM.Group$gra6400+FoodGramsPM.Group$gra6500+FoodGramsPM.Group$gra7000+FoodGramsPM.Group$gra7200+FoodGramsPM.Group$gra6600+FoodGramsPM.Group$gra7300+FoodGramsPM.Group$gra6700+FoodGramsPM.Group$gra7600+FoodGramsPM.Group$gra7800+FoodGramsPM.Group$gra7900
FoodGramsGroupPM_cc$medfru<-FoodGramsGroupPM_cc$gra6200+FoodGramsGroupPM_cc$gra6300+FoodGramsGroupPM_cc$gra6400+FoodGramsGroupPM_cc$gra6500+FoodGramsGroupPM_cc$gra7000+FoodGramsGroupPM_cc$gra7200+FoodGramsGroupPM_cc$gra6600+FoodGramsGroupPM_cc$gra7300+FoodGramsGroupPM_cc$gra6700+FoodGramsGroupPM_cc$gra7600+FoodGramsGroupPM_cc$gra7800+FoodGramsGroupPM_cc$gra7900

FoodGramsPM.Group$medfru<-(FoodGramsPM.Group$medfru*1000)/NutrPM$kcal
FoodGramsGroupPM_cc$medfru<-(FoodGramsGroupPM_cc$medfru*1000)/NutrPM_cc$kcal


#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(FoodGramsGroupPM_cc$medfru, probs = seq(0, 1, by = 0.333))), 
      labels=c("Q1","Q2","Q3"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
medfruits <- sapply(FoodGramsPM.Group$medfru, ApplyQuintiles)

table(medfruits, useNA="always")

summary(FoodGramsPM.Group$medfru)
summary(FoodGramsGroupPM_cc$medfru)

medfruits<-ifelse(FoodGramsPM.Group$medfru>=800, 3, medfruits)


medfruits<- factor(medfruits,
               levels = c(1,2,3),
               labels = c("Q1", "Q2", "Q3"))

table(medfruits, useNA="always")


# COMPONENET 2: Vegetables excluding potatoes

summary(FoodGramsPM.Group$gra_gallveg)
summary(FoodGramsGroup_cc$gra_gallveg)

#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(FoodGramsGroup_cc$gra_gallveg, probs = seq(0, 1, by = 0.333))), 
      labels=c("Q1","Q2","Q3"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
medvege <- sapply(FoodGramsPM.Group$gra_gallveg, ApplyQuintiles)

table(medvege, useNA="always")


medvege<-ifelse(FoodGramsPM.Group$gra_gallveg>=615, 3, medvege)
medvege<-ifelse(FoodGramsPM.Group$gra_gallveg<=42.75, 1, medvege)


medvege<- factor(medvege,
                   levels = c(1,2,3),
                   labels = c("Q1", "Q2", "Q3"))

table(medvege, useNA="always")


# COMPONENET 3: Legumes (group gra5810)

summary(FoodGramsGroupPM_cc$gra5810)
summary(FoodGramsPM.Group$gra5810)

#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(FoodGramsGroup_cc$gra5810, probs = seq(0, 1, by = 0.333))), 
      labels=c("Q1","Q2","Q3"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
medleg <- sapply(FoodGramsPM.Group$gra5810, ApplyQuintiles)

table(medleg, useNA="always")

medleg<-ifelse(FoodGramsPM.Group$gra5810>=100, 3, medleg)

medleg<- factor(medleg,
                 levels = c(1,2,3),
                 labels = c("Q1", "Q2", "Q3"))

table(medleg, useNA="always")


# COMPONENET 4: Fish including seafood gra_gallsea

summary(FoodGramsGroupPM_cc$gra_gallsea)
summary(FoodGramsPM.Group$gra_gallsea)

#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(FoodGramsGroup_cc$gra_gallsea, probs = seq(0, 1, by = 0.333))), 
      labels=c("Q1","Q2","Q3"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
medfish <- sapply(FoodGramsPM.Group$gra_gallsea, ApplyQuintiles)

table(medfish, useNA="always")

medfish<-ifelse(FoodGramsPM.Group$gra_gallsea>=300, 3, medfish)

medfish<- factor(medfish,
                levels = c(1,2,3),
                labels = c("Q1", "Q2", "Q3"))

table(medfish, useNA="always")


# COMPONENET 5: Cereals (white and non-white) gra_gallcer


summary(FoodGramsGroupPM_cc$gra_gallcer)
summary(FoodGramsPM.Group$gra_gallcer)

#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(FoodGramsGroup_cc$gra_gallcer, probs = seq(0, 1, by = 0.333))), 
      labels=c("Q1","Q2","Q3"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
medcer <- sapply(FoodGramsPM.Group$gra_gallcer, ApplyQuintiles)

table(medcer, useNA="always")


medcer<-ifelse(FoodGramsPM.Group$gra_gallcer>=400, 3, medcer)
medcer<-ifelse(FoodGramsPM.Group$gra_gallcer<=40, 1, medcer)

medcer<- factor(medcer,
                 levels = c(1,2,3),
                 labels = c("Q1", "Q2", "Q3"))

table(medcer, useNA="always")


# COMPONENET 6: Olive oil fre11700

#The scoring scheme for olive oil consisted of assigning 0 points to non-consumers,
#1 point for participants below the median of intake and 2 points for
#levels of intake equal or above this median.

summary(FoodGramsGroupPM_cc$gra11700)
summary(FoodGramsPM.Group$gra11700)

# median among controls is 10.00

medoil<-ifelse(FoodGramsPM.Group$gra11700<=0, 1, NA)
medoil<-ifelse(FoodGramsPM.Group$gra11700>0 & FoodGramsPM.Group$gra11700<=10.0, 2, medoil)
medoil<-ifelse(FoodGramsPM.Group$gra11700>10.0, 3, medoil)

table(medoil, useNA="always")

medoil<- factor(medoil,
                levels = c(1,2,3),
                labels = c("Q1", "Q2", "Q3"))

table(medoil, useNA="always")

# BETTER by tertiles:

#ApplyQuintiles <- function(x) {
#  cut(x, breaks=c(quantile(FoodGramsGroup_cc$gra11700, probs = seq(0, 1, by = 0.333))), 
#      labels=c("Q1","Q2","Q3"), include.lowest = TRUE)
#}

#Add the quintile to the dataframe
#medoil <- sapply(FoodGramsPM.Group$gra11700, ApplyQuintiles)

table(medoil, useNA="always")



### NEGATIVE COMPONENTS

# COMPONENET 7: Meat and meat products gra_gallmeat


summary(FoodGramsGroupPM_cc$gra_gallmeat)
summary(FoodGramsPM.Group$gra_gallmeat)

#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(FoodGramsGroup_cc$gra_gallmeat, probs = seq(0, 1, by = 0.333))), 
      labels=c("Q1","Q2","Q3"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
medmeat <- sapply(FoodGramsPM.Group$gra_gallmeat, ApplyQuintiles)

table(medmeat, useNA="always")


medmeat<-ifelse(FoodGramsPM.Group$gra_gallmeat<=0, 1, medmeat)
medmeat<-ifelse(FoodGramsPM.Group$gra_gallmeat>=250, 3, medmeat)
medmeat<-ifelse(FoodGramsPM.Group$gra_gallmeat<=20, 1, medmeat)

medmeat<- factor(medmeat,
                levels = c(1,2,3),
                labels = c("Q1", "Q2", "Q3"))

table(medmeat, useNA="always")


# COMPONENET 8: Dairy products gra_galldairy


summary(FoodGramsGroupPM_cc$gra_galldairy)
summary(FoodGramsPM.Group$gra_galldairy)

#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(FoodGramsGroup_cc$gra_galldairy, probs = seq(0, 1, by = 0.333))), 
      labels=c("Q1","Q2","Q3"), include.lowest = TRUE)
}

#Add the quintile to the dataframe
meddai <- sapply(FoodGramsPM.Group$gra_galldairy, ApplyQuintiles)

table(meddai, useNA="always")

meddai<-ifelse(FoodGramsPM.Group$gra_galldairy<=0, 1, meddai)
meddai<-ifelse(FoodGramsPM.Group$gra_galldairy>=650, 3, meddai)
meddai<-ifelse(FoodGramsPM.Group$gra_galldairy<=30, 1, meddai)

meddai<- factor(meddai,
                 levels = c(1,2,3),
                 labels = c("Q1", "Q2", "Q3"))

table(meddai, useNA="always")


# COMPONENET 9: Alcohol

#alcohol intake (non-drinkers, drinkers of 0-6 g per day, 
#46-12 g per day, 412-24 g per day, 424-60 g per day, 
#women drinkers of: 460 g per day, men drinkers of: 
#460-96 g per day, 496 g per day),

# not to be done!!


# MAKE THE SCORE;

FoodGramsPM$medfruits<-NA
FoodGramsPM$medfruits<-ifelse(medfruits=="Q3", 2, NA)
FoodGramsPM$medfruits<-ifelse(medfruits=="Q2", 1, FoodGramsPM$medfruits)
FoodGramsPM$medfruits<-ifelse(medfruits=="Q1", 0, FoodGramsPM$medfruits)

FoodGramsPM$medvege<-NA
FoodGramsPM$medvege<-ifelse(medvege=="Q3", 2, NA)
FoodGramsPM$medvege<-ifelse(medvege=="Q2", 1, FoodGramsPM$medvege)
FoodGramsPM$medvege<-ifelse(medvege=="Q1", 0, FoodGramsPM$medvege)

FoodGramsPM$medleg<-NA
FoodGramsPM$medleg<-ifelse(medleg=="Q3", 2, NA)
FoodGramsPM$medleg<-ifelse(medleg=="Q2", 1, FoodGramsPM$medleg)
FoodGramsPM$medleg<-ifelse(medleg=="Q1", 0, FoodGramsPM$medleg)

FoodGramsPM$medcer<-NA
FoodGramsPM$medcer<-ifelse(medcer=="Q3", 2, NA)
FoodGramsPM$medcer<-ifelse(medcer=="Q2", 1, FoodGramsPM$medcer)
FoodGramsPM$medcer<-ifelse(medcer=="Q1", 0, FoodGramsPM$medcer)

FoodGramsPM$medfish<-NA
FoodGramsPM$medfish<-ifelse(medfish=="Q3", 2, NA)
FoodGramsPM$medfish<-ifelse(medfish=="Q2", 1, FoodGramsPM$medfish)
FoodGramsPM$medfish<-ifelse(medfish=="Q1", 0, FoodGramsPM$medfish)

FoodGramsPM$medoil<-NA
FoodGramsPM$medoil<-ifelse(medoil=="Q3", 2, NA)
FoodGramsPM$medoil<-ifelse(medoil=="Q2", 1, FoodGramsPM$medoil)
FoodGramsPM$medoil<-ifelse(medoil=="Q1", 0, FoodGramsPM$medoil)


# negative components:

FoodGramsPM$medmeat<-NA
FoodGramsPM$medmeat<-ifelse(medmeat=="Q3", 0, NA)
FoodGramsPM$medmeat<-ifelse(medmeat=="Q2", 1, FoodGramsPM$medmeat)
FoodGramsPM$medmeat<-ifelse(medmeat=="Q1", 2, FoodGramsPM$medmeat)

FoodGramsPM$meddai<-NA
FoodGramsPM$meddai<-ifelse(meddai=="Q3", 0, NA)
FoodGramsPM$meddai<-ifelse(meddai=="Q2", 1, FoodGramsPM$meddai)
FoodGramsPM$meddai<-ifelse(meddai=="Q1", 2, FoodGramsPM$meddai)


# same for all:


FoodGramsPM$MDSCORE<-FoodGramsPM$medfruits+FoodGramsPM$medvege+FoodGramsPM$medleg+FoodGramsPM$medcer+FoodGramsPM$medfish+FoodGramsPM$medoil+FoodGramsPM$medmeat+FoodGramsPM$meddai

#FoodGramsPM$SCORE<-FoodGramsPM$SC_fiber+FoodGramsPM$SC_nuts+FoodGramsPM$SC_fruits+FoodGramsPM$SC_ratio+FoodGramsPM$SC_trans+FoodGramsPM$SC_meats+FoodGramsPM$SC_juices

FoodGramsPM.Group$MDSCORE<-FoodGramsPM$MDSCORE

FoodGramsPM.Group$medfruits<-FoodGramsPM$medfruits
FoodGramsPM.Group$medvege<-FoodGramsPM$medvege
FoodGramsPM.Group$medleg<-FoodGramsPM$medleg
FoodGramsPM.Group$medcer<-FoodGramsPM$medcer
FoodGramsPM.Group$medfish<-FoodGramsPM$medfish
FoodGramsPM.Group$medoil<-FoodGramsPM$medoil
FoodGramsPM.Group$medmeat<-FoodGramsPM$medmeat
FoodGramsPM.Group$meddai<-FoodGramsPM$meddai

summary(FoodGramsPM$MDSCORE)

summary(FoodGramsPM.Group$MDSCORE)


# also add the components

FoodGramsPM.Group$fruits<-medfruits
FoodGramsPM.Group$vege<-medvege
FoodGramsPM.Group$leg<-medleg
FoodGramsPM.Group$cer<-medcer
FoodGramsPM.Group$fish<-medfish
FoodGramsPM.Group$oil<-medoil
FoodGramsPM.Group$meat<-medmeat
FoodGramsPM.Group$fish<-medfish

FoodGramsPM$fruits<-medfruits
FoodGramsPM$vege<-medvege
FoodGramsPM$leg<-medleg
FoodGramsPM$cer<-medcer
FoodGramsPM$fish<-medfish
FoodGramsPM$oil<-medoil
FoodGramsPM$meat<-medmeat
FoodGramsPM$fish<-medfish


# descriptives:

library(compareGroups)
library(kableExtra)

# A higher score should lead to lower T2D risk, and in turn, to a lower PDAC risk

# Verify is associated with case-control

meta<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Microbiome_metadata/NEW UPDATE ALL VARIABLES/14052018_metadata_AllMicrobiome.csv", header=TRUE, sep=";")
metaPM<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/Esther Molina/EMBL_STAY/Imputation/ImputationOnAllData/01062018_imputeddataNew.csv", header=TRUE, sep=";") # ONLY FOR THE SPANISH DATA

sc<-FoodGramsPM[,c("subject", "MDSCORE", "medfruits",    "medvege",   "medleg",        "medcer",        "medfish",       "medoil",        "medmeat", "meddai")]

meta<-merge(metaPM, sc, by="subject")

sc<-NutrPM[,c("subject", "kcal", "Sugar.g")]

meta<-merge(meta, sc, by="subject")


table(meta$final_eligibility)

meta<-meta[meta$casecontrol!=2,]

# descriptives:


#### DESCRIPTIVES:

res<-compareGroups(casecontrol~., data=FoodGramsPM.Group[FoodGramsPM.Group$casecontrol!=2,], ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab


res<-compareGroups(alldiab~., data=meta[meta$casecontrol==0 & meta$diabcat!=1,c(4,11,44:54)], ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab
export2md(restab, strip = TRUE, first.strip = TRUE) 


res<-compareGroups(alldiab~., data=meta[meta$casecontrol==0,c(4,11,44:54)], ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab
export2md(restab, strip = TRUE, first.strip = TRUE) 


res<-compareGroups(obese~., data=meta[meta$casecontrol==0,c(4,11,13,44:54)], ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab
export2md(restab, strip = TRUE, first.strip = TRUE) 


res<-compareGroups(casecontrol~., data=meta[meta$diabcat!=1,c(4,11,13,44:54)], ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab
export2md(restab, strip = TRUE, first.strip = TRUE) 


res<-compareGroups(casecontrol~., data=meta[,c(4,11,13,44:54)], ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab
export2md(restab, strip = TRUE, first.strip = TRUE) 



## RESCTRICTED TO THE MICOBIOME STUDY:

res<-compareGroups(casecontrol~., data=meta[meta$diabcat!=1 & meta$MetaG==1,c(4,11,13,44:54)], ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab
export2md(restab, strip = TRUE, first.strip = TRUE) 

# association study:

##########################################
### Associations:

# with diabetes:

fit<-glm(alldiab~MDSCORE, data=meta[meta$casecontrol==0 & meta$diabcat!=1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))
exp(cbind(OR=coef(fit), confint(fit)))^2


fit<-glm(alldiab~MDSCORE+agec+factor(sex)+factor(center)+kcal+factor(cpy1t)+factor(obese), data=meta[meta$casecontrol==0 & meta$diabcat!=1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))
exp(cbind(OR=coef(fit), confint(fit)))^2


# with obesity:

fit<-glm(obese~MDSCORE, data=meta[meta$casecontrol==0 & meta$diabcat!=1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))

fit<-glm(obese~MDSCORE+agec+factor(sex)+factor(center)+kcal+factor(cpy1t)+factor(alldiab), data=meta[meta$casecontrol==0 & meta$diabcat!=1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))


# with PDAC:


fit<-glm(casecontrol~MDSCORE, data=meta, family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))
exp(cbind(OR=coef(fit), confint(fit)))^2

fit<-glm(casecontrol~MDSCORE+agec+factor(sex)+factor(center)+kcal+factor(cpy1t)+factor(FHPDAC)+factor(diabcat) + factor(obese), data=meta, family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))
exp(cbind(OR=coef(fit), confint(fit)))^2


#restricted to non-T2D
fit<-glm(casecontrol~MDSCORE+agec+factor(sex)+factor(center)+kcal+factor(cpy1t)+factor(FHPDAC) + factor(obese), data=meta[meta$diabcat!=0,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))
exp(cbind(OR=coef(fit), confint(fit)))^2

#restricted to obese
fit<-glm(casecontrol~MDSCORE+agec+factor(sex)+factor(center)+kcal+factor(cpy1t)+factor(FHPDAC) + factor(diabcat), data=meta[meta$obese==1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))
exp(cbind(OR=coef(fit), confint(fit)))^2

# restricted to sex

fit<-glm(casecontrol~MDSCORE+agec+factor(obese)+factor(center)+kcal+factor(cpy1t)+factor(FHPDAC) + factor(diabcat), data=meta[meta$sex==1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))
exp(cbind(OR=coef(fit), confint(fit)))^2


## restricted to the microbiome study:

fit<-glm(casecontrol~MDSCORE, data=meta[meta$MetaG==1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))

fit<-glm(casecontrol~MDSCORE+agec+factor(sex)+factor(center)+kcal+factor(cpy1t)+factor(FHPDAC), data=meta[meta$MetaG==1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))

fit<-glm(casecontrol~MDSCORE+agec+factor(sex)+factor(center)+kcal+factor(cpy1t)+factor(FHPDAC) + factor(diabcat), data=meta[meta$MetaG==1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))

fit<-glm(casecontrol~MDSCORE+agec+factor(sex)+factor(center)+kcal+factor(cpy1t)+factor(FHPDAC), data=meta[meta$MetaG==1 & meta$diabcat!=1,], family="binomial")
summary(fit)
exp(coef(fit))
exp(confint(fit))


