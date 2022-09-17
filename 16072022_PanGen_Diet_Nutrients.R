

#########################################
#### ------------------------------- ####
#### NUTRIENTS/DAY COMPILATION       ####
#### 16-23 March 2017                ####
#### Noelia and Esther               ####
#### ------------------------------- ####
#########################################


# LOAD THE DATASETS WE WILL USE:


setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodFreq")

CuestFreq<-read.csv("14072022_FoodFreqDay with Zeros.csv", header=T, sep=",", na.strings="NA")## Open PanGen dieta data


setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodNutrients")
## Change them one by one, depending on whether you want to generate nutrients for both sexes combined or considering sex-specific nutrients
## TCAgrac<-read.csv("30032017_TCA_BEDCA&PANGEN_CALC_SERVING.csv", header=T, sep=";", na.strings="NA")## 
## TCAgrac<-read.csv("30032017_TCA_BEDCA&PANGEN_CALC_WOMEN.csv", header=T, sep=";", na.strings="NA")## 
TCAgrac<-read.csv("30032017_TCA_BEDCA&PANGEN_CALC_SERVING.csv", header=T, sep=";", na.strings="NA")## this is the example for men. Once finished, to it for women and then combine the data (see end of the code)
# altertanively do it in one step if you want to generate directly nutrients for the same serving sizes in men and women (combined)

TCAgrac[is.na(TCAgrac)]<-0


###################################################################################################
###################################################################################################

##-----------------------------------------------------
## the example:
## here I multiply every column in mymat by the vector and then sum up all the values 

##myvec <- c(1:3)

##mymat <- as.matrix(cbind(a = 6:15, b = 16:25, c= 26:35))

##bb<-mymat %*% myvec
##-----------------------------------------------------

###################################################################################################
###################################################################################################

### Apply it the same way to the pangen data

# extract the variables needed for the data compilation in the same order as they appear in the TCA database:

data<-c("fre3100", "fre_bfish_tot", "fre_c_2900",  "fre_chi_tot", "fre_lam_tot",
        "fre_por_tot", "fre_vea_tot", "fre_wfish_tot", "fre10200", "fre10300", "fre10400", "fre10500",
        "fre11000", "fre11300", "fre11400", "fre11610", "fre11630", "fre11700", "fre11800", "fre1200",
        "fre12200", "fre12500", "fre12900", "fre1300", "fre13100", "fre13200", "fre13500", "fre1400",      
        "fre14300", "fre14400", "fre14500", "fre14700", "fre1500", "fre15201", "fre1800", "fre2100", "fre2200", 
        "fre2300", "fre2500", "fre2700", "fre2800", "fre300", "fre3200", "fre3400", "fre3500",
        "fre3600", "fre3700", "fre3800", "fre4100", "fre4200", "fre4300", "fre4500", "fre4700", "fre5000",
        "fre5100", "fre5200", "fre5810", "fre600", "fre6200", "fre6300", "fre6400",  "fre6500", "fre6600",
        "fre6700", "fre700", "fre7000", "fre7200", "fre7300", "fre7600", "fre7800", "fre7900", "fre8100", "fre8200", "fre8500",      
        "fre8600", "fre8800", "fre9000", "fre9200", "fre9400", "fre9600", "fre9700", "fre9800")

MyDiet<-CuestFreq[data] 

# change all NAs to ceros; this is so needed to sum up and avoid problems with NAs; since we will deal with imputed data, there won´t be any problem with NAs. This is therefore not needed
#MyDiet[is.na(MyDiet)] <- 0 # to avoid problems

#TCAgrac[is.na(TCAgrac)] <- 0 # to avoid problems


MyDiet<-as.matrix(sapply(MyDiet, as.numeric))  

# these are my Mg values for all the 97 food items; my vector
# diet is my matrix with the 97 food items in columns and the 1351 observations
# Let?s multiply the vector by the matrix so as to sum up Mg values for every subject across all the 97 food items


kJmean<-TCAgrac$kJmean 
kJ<-MyDiet %*% kJmean


kcalmean<-TCAgrac$kcalmean
kcal<-MyDiet %*% kcalmean


Fat..gmean<-TCAgrac$Fat..gmean 
Lipids.g<-MyDiet %*% Fat..gmean


Proteins..gmean<-TCAgrac$Proteins..gmean 
Proteins.g<-MyDiet %*% Proteins..gmean


Humidity..gmean<-TCAgrac$Humidity..gmean 
Humidity.g<-MyDiet %*% Humidity..gmean


Carbs..gmean<-TCAgrac$Carbs..gmean 
Carbs.g<-MyDiet %*% Carbs..gmean


Sucrose..gmean<-TCAgrac$Sucrose..gmean 
Sucrose.g<-MyDiet %*% Sucrose..gmean


Fiber..gmean<-TCAgrac$Fiber..gmean 
Fiber.g<-MyDiet %*% Fiber..gmean


Starch..gmean<-TCAgrac$Starch..gmean 
Starch.g<-MyDiet %*% Starch..gmean


Sugar..gmean<-TCAgrac$Sugar..gmean 
Sugar.g<-MyDiet %*% Sugar..gmean


Cholesterol..mgmean<-TCAgrac$Cholesterol..mgmean 
Cholesterol.mg<-MyDiet %*% Cholesterol..mgmean


Equivalentes.de.retinol..ug<-TCAgrac$Equivalentes.de.retinol..ug 
VitA.ug<-MyDiet %*% Equivalentes.de.retinol..ug


Vitamina.D..ug<-TCAgrac$Vitamina.D..ug 
VitD.ug<-MyDiet %*% Vitamina.D..ug


Vitamina.E..mgmean<-TCAgrac$Vitamina.E..mgmean 
VitE.mg<-MyDiet %*% Vitamina.E..mgmean


Vitamina.B8..ugmean<-TCAgrac$Vitamina.B8..ugmean 
VitB8.ug<-MyDiet %*% Vitamina.B8..ugmean


Vitamina.B9..ugmean<-TCAgrac$Vitamina.B9..ugmean 
VitB9.ug<-MyDiet %*% Vitamina.B9..ugmean


Vitamina.B3..mgmean<-TCAgrac$Vitamina.B3..mgmean 
VitB3.mg<-MyDiet %*% Vitamina.B3..mgmean


Vitamina.B5..mgmean<-TCAgrac$Vitamina.B5..mgmean 
VitB5.mg<-MyDiet %*% Vitamina.B5..mgmean


Vitamina.B2..mgmean<-TCAgrac$Vitamina.B2..mgmean 
VitB2.mg<-MyDiet %*% Vitamina.B2..mgmean


Vitamina.B1..mgmean<-TCAgrac$Vitamina.B1..mgmean 
VitB1.mg<-MyDiet %*% Vitamina.B1..mgmean


Vitamina.B12..ugmean<-TCAgrac$Vitamina.B12..ugmean 
VitB12.ug<-MyDiet %*% Vitamina.B12..ugmean


Vitamina.B6..mgmean<-TCAgrac$Vitamina.B6..mgmean 
VitB6.mg<-MyDiet %*% Vitamina.B6..mgmean


Vitamina.C..mgmean<-TCAgrac$Vitamina.C..mgmean 
VitC.mg<-MyDiet %*% Vitamina.C..mgmean


Calcium..mgmean<-TCAgrac$Calcium..mgmean 
Calcium.mg<-MyDiet %*% Calcium..mgmean


Iron..mgmean<-TCAgrac$Iron..mgmean 
Iron.mg<-MyDiet %*% Iron..mgmean


Potasium..mgmean<-TCAgrac$Potasium..mgmean 
Potasium.mg<-MyDiet %*% Potasium..mgmean


Magnesium..mgmean<-TCAgrac$Magnesium..mgmean 
Magnesium.mg<-MyDiet %*% Magnesium..mgmean


Sodium..mgmean<-TCAgrac$Sodium..mgmean 
Sodium.mg<-MyDiet %*% Sodium..mgmean


f.sforo..mg.mean<-TCAgrac$f.sforo..mg.mean 
Phosphorus.mg<-MyDiet %*% f.sforo..mg.mean


Copper..mgmean<-TCAgrac$Copper..mgmean 
Copper.mg<-MyDiet %*% Copper..mgmean


Iodide..ugmean<-TCAgrac$Iodide..ugmean 
Iodide.ug<-MyDiet %*% Iodide..ugmean


Selenium..ugmean<-TCAgrac$Selenium..ugmean 
Selenium.ug<-MyDiet %*% Selenium..ugmean


Zinc..mgmean<-TCAgrac$Zinc..mgmean 
Zinc.mg<-MyDiet %*% Zinc..mgmean


Linoleic..gmean<-TCAgrac$Linoleic..gmean 
Linoleic.g<-MyDiet %*% Linoleic..gmean


Linolenic..mgmean<-TCAgrac$Linolenic..mgmean 
Linolenic.mg<-MyDiet %*% Linolenic..mgmean


Araquidonic..gmean<-TCAgrac$Araquidonic..gmean 
Araquidonic.gm<-MyDiet %*% Araquidonic..gmean


DHA..gmean<-TCAgrac$DHA..gmean 
DHA.g<-MyDiet %*% DHA..gmean


EPA..gmean<-TCAgrac$EPA..gmean 
EPA.g<-MyDiet %*% EPA..gmean


Estearic..gmean<-TCAgrac$Estearic..gmean 
Estearic.g<-MyDiet %*% Estearic..gmean


Lauric..gmean<-TCAgrac$Lauric..gmean 
Lauric.g<-MyDiet %*% Lauric..gmean


Miristic..gmean<-TCAgrac$Miristic..gmean 
Miristic.g<-MyDiet %*% Miristic..gmean


Polyunsaturated..gmean<-TCAgrac$Polyunsaturated..gmean 
Polyunsaturated.g<-MyDiet %*% Polyunsaturated..gmean


Saturated..gmean<-TCAgrac$Saturated..gmean 
Saturated.g<-MyDiet %*% Saturated..gmean

Trans..gmean<-TCAgrac$Trans..gmean 
Trans.g<-MyDiet %*% Trans..gmean

MFA..gmean<-TCAgrac$MFA..gmean 
MFA.g<-MyDiet %*% MFA..gmean 

GI<-TCAgrac$GI
GI.g<-MyDiet %*% GI

## To extract personal data


Data1<-c("subject")

subject<-CuestFreq[Data1] 


Data2<-c("section")

section<-CuestFreq[Data2] 





## To combine the columns with cbind

PanGen_DietMatrix<-cbind(subject, section,
  kJ,
  kcal,
  Lipids.g,
  Proteins.g,
  Humidity.g,
  Carbs.g,
  GI.g,
  Sucrose.g,
  Fiber.g,
  Starch.g,
  Sugar.g,
  Cholesterol.mg,
  VitA.ug,
  VitD.ug,
  VitE.mg,
  VitB8.ug,
  VitB9.ug,
  VitB3.mg,
  VitB5.mg,
  VitB2.mg,
  VitB1.mg,
  VitB12.ug,
  VitB6.mg,
  VitC.mg,
  Calcium.mg,
  Iron.mg,
  Potasium.mg,
  Magnesium.mg,
  Sodium.mg,
  Phosphorus.mg,
  Copper.mg,
  Iodide.ug,
  Selenium.ug,
  Zinc.mg,
  Linoleic.g,
  Linolenic.mg,
  Araquidonic.gm,
  DHA.g,
  EPA.g,
  Estearic.g,
  Lauric.g,
  Miristic.g,
  Polyunsaturated.g,
  Saturated.g,
  Trans.g,
  MFA.g)

setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodNutrients") # change the path, if so needed
#write.csv(PanGen_DietMatrix, "20042017_PanGen_Nutrients_StandarServing.csv")## 
#write.csv(PanGen_DietMatrix, "05042017_PanGen_Nutrients_WomenServing.csv")## 
write.csv(PanGen_DietMatrix, "20072022_Nutrients_Serving with GI.csv")## 





