
#########################################
#### ------------------------------- ####
#### GRAMS/DAY                       ####
#### 15-27 March 2017                ####
#### Noelia and Esther               ####
#### ------------------------------- ####
#########################################

# LOAD THE DATASETS WE WILL USE:
# this regards the frequency/day dataset

setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME")

dieta<-read.csv("13072022_FoodFreqDay.csv", header=T, sep=",", na.strings="NA")## Open freqency data (transpose)

# load the serving size database:

setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/FoodGrams/ServingsData")

Serve<-read.csv("31032017_TCA_FoodMean_sex.csv", header=T, sep=";", na.strings="NA")## Open grams per serve and grams per serve / 100


# extract the variables needed for the data compilation in the same order as they appear in the TCA database:


MyFreDiet<-dieta[,c("fre3100", "fre_bfish_tot", "fre_c_2900",  "fre_chi_tot", "fre_lam_tot",  
                    "fre_por_tot", "fre_vea_tot", "fre_wfish_tot", "fre10200", "fre10300", "fre10400", "fre10500",       
                    "fre11000", "fre11300", "fre11400", "fre11610", "fre11630", "fre11700", "fre11800", "fre1200",      
                    "fre12200", "fre12500", "fre12900", "fre1300", "fre13100", "fre13200", "fre13500", "fre1400",      
                    "fre14300", "fre14400", "fre14500", "fre14700", "fre1500", "fre15201",      
                    "fre1800", "fre2100", "fre2200", "fre2300", "fre2500", "fre2700", "fre2800", "fre300", "fre3200",      
                    "fre3400", "fre3500", "fre3600", "fre3700", "fre3800", "fre4100", "fre4200", "fre4300",      
                    "fre4500", "fre4700", "fre5000",  "fre5100", "fre5200", "fre5810",      
                    "fre600", "fre6200", "fre6300", "fre6400",  "fre6500",  "fre6600",  "fre6700",  "fre700", "fre7000",      
                    "fre7200", "fre7300", "fre7600", "fre7800", "fre7900",  "fre8100",  "fre8200",  "fre9400", "fre8500",      
                    "fre8600", "fre8800", "fre9000", "fre9200", "fre9600", "fre9700", "fre9800")] 



# variable not contained in this microbiome dataset: "fre8400"

# change all NAs to ceros; this is so needed to sum up and avoid problems with NAs
MyFreDiet[is.na(MyFreDiet)] <- 0 # to avoid problems


MyFreDiet<-as.matrix(sapply(MyFreDiet, as.numeric))  

# g.food<-Serve$Serving.S # these are servings sizes for all, men and women combined (non-specific serving sizes), not active
# g.food<-Serve$Serving.W # these are servins sizes for Women (specific), not active
g.food<-Serve$Serving.M # these are servings sizes for Men (specific), now active

# diet is my matrix with the 97 food items in columns and the 1351 observations
# Lets multiply the vector by the matrix for every subject across all the 97 food items

FoodGrams<-MyFreDiet %*% diag(g.food) # this way we generate grams/day of every food item considering servings sizes

#save this for men:

write.csv(FoodGrams, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodGrams/GramsDayMen.csv")

# repeate the same for women:
g.food<-Serve$Serving.W
FoodGrams<-MyFreDiet %*% diag(g.food)
# make sure that the replacement of values is well performed. Otherwise, calculate first grams/day for men and grams/day for women
# and then join the data with cbind: see the code at the end

write.csv(FoodGrams, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodGrams/GramsDayWomen.csv")


# in case you need to generate the non-specifc serving sizes, you will need to run:
g.food<-Serve$Serving.S
FoodGrams<-MyFreDiet %*% diag(g.food)

Nada de trabajos, pero realizó TODAS las tareas
################
#### RENAME ####
################

library(data.table)
nms <- c("gra3100", "gra_bfish_tot", "gra_c_2900",  "gra_chi_tot", "gra_lam_tot",  
         "gra_por_tot", "gra_vea_tot", "gra_wfish_tot", "gra10200", "gra10300", "gra10400", "gra10500",       
         "gra11000", "gra11300", "gra11400", "gra11610", "gra11630", "gra11700", "gra11800", "gra1200",      
         "gra12200", "gra12500", "gra12900", "gra1300", "gra13100", "gra13200", "gra13500", "gra1400",      
         "gra14300", "gra14400", "gra14500", "gra14700", "gra1500", "gra15201",      
         "gra1800", "gra2100", "gra2200", "gra2300", "gra2500", "gra2700", "gra2800", "gra300", "gra3200",      
         "gra3400", "gra3500", "gra3600", "gra3700", "gra3800", "gra4100", "gra4200", "gra4300",      
         "gra4500", "gra4700", "gra5000",  "gra5100", "gra5200", "gra5810",      
         "gra600", "gra6200", "gra6300", "gra6400",  "gra6500",  "gra6600",  "gra6700",  "gra700", "gra7000",      
         "gra7200", "gra7300", "gra7600", "gra7800", "gra7900",  "gra8100",  "gra8200",  "gra9400", "gra8500",      
         "gra8600", "gra8800", "gra9000", "gra9200", "gra9600", "gra9700", "gra9800")

FoodGrams<-as.data.frame(FoodGrams)
FoodGrams$subject<-NULL
FoodGrams<-setnames(FoodGrams, nms)



##################################
#### ADD PERSONAL INFORMATION "subject variable" ####
##################################


## To merged grams information and personal data


FoodGrams$subject<-foodfq$subject

# should this not work, make sure that Food.Grams is a data.frame
# otherwise, use the code below:

#Datax<-c("subject")
#subject<-TCA.TOT[Datax] 
#IDsDietGrams<-cbind(subject, Food.Grams)
#MyGramsPers<-merge(PanGen_GramsMatrix, IDsDietGrams, by.x="subject", by.y="subject") # Total dataset related to PanGen data


## To save data

write.csv(FoodGrams, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodGrams/13072022_GramsDayAll.csv")


# write.csv(FoodGrams, "20042017_PanGen_Grams_StandardServing.csv")## 
# write.csv(FoodGrams, "20042017_PanGen_Grams_WomanServing.csv")## 
# write.csv(FoodGrams, "20042017_PanGen_Grams_MenServing.csv")## 

#########################################################################
#########################################################
## to join rows of sex servings (women and men servings)
#########################################################
#########################################################################

setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/Esther Molina/NOELIA_DIETA_PanGen/PANGEN/FoodGrams") # change the path if so needed

ME<-read.csv("20042017_PanGen_Grams_MenServing.csv", header=T, sep=",", na.strings="NA") ##Add PanGen variables
WO<-read.csv("20042017_PanGen_Grams_WomanServing.csv", header=T, sep=",", na.strings="NA") ##Add PanGen variables

ME1<-subset(ME, sex =="1")
WO1<-subset(WO, sex =="0")

SEX <- rbind(ME1, WO1) 

setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/Esther Molina/NOELIA_DIETA_PanGen/PANGEN/FoodGrams") # change the path if so needed
write.csv(SEX, "20042017_PanGen_Diet_Grams_SEXSERVING.csv") ## Save data

#############################################################################
#### Reorder the variables according to master file PANGEN ORDER
#############################################################################

DATA<-c("subject",
               "section",
               "gra8200",
               "gra8100",
               "gra8600",
               "gra8500",
               "gra8800",
               "gra9000",
               "gra9200",
               "gra9400",
               "gra300",
               "gra_chi_tot",
               "gra_vea_tot",
               "gra_por_tot",
               "gra_lam_tot",
               "gra1200",
               "gra1300",
               "gra1400",
               "gra1500",
               "gra_wfish_tot",
               "gra_bfish_tot",
               "gra3100",
               "gra3200",
               "gra1800",
               "gra600",
               "gra700",
               "gra2500",
               "gra2100",
               "gra2200",
               "gra2300",
               "gra2700",
               "gra2800",
               "gra_c_2900",
               "gra3500",
               "gra4500",
               "gra3400",
               "gra3600",
               "gra3800",
               "gra4200",
               "gra4300",
               "gra5000",
               "gra3700",
               "gra4700",
               "gra5200",
               "gra4100",
               "gra5100",
               "gra5810",
               "gra6200",
               "gra6300", 
               "gra6400",
               "gra6500",
               "gra7000",
               "gra7200",
               "gra6600",
               "gra7300",
               "gra6700",
               "gra7600",
               "gra7800",
               "gra7900",
               "gra9600",
               "gra9700",
               "gra9800",
               "gra10300",
               "gra10200",
               "gra10400",
               "gra10500",
               "gra11000",
               "gra11700",
               "gra11800",
               "gra12200",
               "gra12500",
               "gra12900",
               "gra13100",
               "gra13200",
               "gra11300",
               "gra11400",
               "gra11610",
               "gra11630",
               "gra13500",
               "gra14300",
               "gra14400",
               "gra14500",
               "gra14700",
               "gra15201",   "gra_coffee",    "gra_decoffee",  "gra_tea",      "beer2",         "wine2",        
        "fortified2",    "sprits2")

#MyEND<-FoodGrams[DATA]

#setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/Esther Molina/NOELIA_DIETA_PanGen/PANGEN/FoodGrams")
# write.csv(MyEND, "20042017_PanGen_Diet_Grams_StandardServing.csv") ## Save data
#write.csv(MyEND, "20042017_PanGen_Diet_Grams_SEXSERVING.csv") ## Save data




### NEED TO INCORPORATE COFFEE AND ALCOHOL BEVERAGES TRANSFORMED

dieta<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodFreq/13072022_FoodFreqDay.csv", header=TRUE)


# take 200 ml as standard serving for coffee and tea

FoodGrams$gra_coffee<-dieta$fre_coffee * 200
FoodGrams$gra_decoffee<-dieta$fre_decoffee * 200
FoodGrams$gra_tea<-dieta$fre_tea *200

#Reference values to be considered for conversion later on: 
#  Vino: 1 vaso o copa (125mls)
#  Cerveza: 1 caña o quinto (200ml)
#  Vermut, Martini, O Porto, etc: 1 copa (50 ml)
#  Coñac, ginebra, ron, whisky, etc: 1 medida de bar o 25mls

FoodGrams$beer2<-dieta$beer2 * 200
FoodGrams$wine2<-dieta$wine2 * 125
FoodGrams$fortified2<-dieta$fortified2 * 50
FoodGrams$spirits2<-dieta$sprits2 * 25

## Put as NAs those whod did not have information collected on diet

#FoodGrams<-FoodGrams[,c(83,87,1:82,84,85,86,89:91)]


aa<-FoodGrams[,c(3:86)]
rownames(aa)<-FoodGrams$subject

aa[rownames(aa)=="3109131",]<-NA
aa[rownames(aa)=="3109136",]<-NA
aa[rownames(aa)=="3109140",]<-NA
aa[rownames(aa)=="3109231",]<-NA

aa$subject<-FoodGrams$subject
aa$section<-FoodGrams$section
aa$beer2<-FoodGrams$beer2
aa$wine2<-FoodGrams$wine2
aa$fortified2<-FoodGrams$fortified2
aa$spirits2<-FoodGrams$spirits2

FoodGrams<-aa

#FoodGrams$section<-dieta$section

# Reorder the data according to pangen:

FoodGrams<-FoodGrams[,c("subject",
  "section",
  "gra8200",
  "gra8100",
  "gra8600",
  "gra8500",
  "gra8800",
  "gra9000",
  "gra9200",
  "gra9400",
  "gra300",
  "gra_chi_tot",
  "gra_vea_tot",
  "gra_por_tot",
  "gra_lam_tot",
  "gra1200",
  "gra1300",
  "gra1400",
  "gra1500",
  "gra_wfish_tot",
  "gra_bfish_tot",
  "gra3100",
  "gra3200",
  "gra1800",
  "gra600",
  "gra700",
  "gra2500",
  "gra2100",
  "gra2200",
  "gra2300",
  "gra2700",
  "gra2800",
  "gra_c_2900",
  "gra3500",
  "gra4500",
  "gra3400",
  "gra3600",
  "gra3800",
  "gra4200",
  "gra4300",
  "gra5000",
  "gra3700",
  "gra4700",
  "gra5200",
  "gra4100",
  "gra5100",
  "gra5810",
  "gra6200",
  "gra6300", 
  "gra6400",
  "gra6500",
  "gra7000",
  "gra7200",
  "gra6600",
  "gra7300",
  "gra6700",
  "gra7600",
  "gra7800",
  "gra7900",
  "gra9600",
  "gra9700",
  "gra9800",
  "gra10300",
  "gra10200",
  "gra10400",
  "gra10500",
  "gra11000",
  "gra11700",
  "gra11800",
  "gra12200",
  "gra12500",
  "gra12900",
  "gra13100",
  "gra13200",
  "gra11300",
  "gra11400",
  "gra11610",
  "gra11630",
  "gra13500",
  "gra14300",
  "gra14400",
  "gra14500",
  "gra14700",
  "gra15201",
  "gra_coffee",    "gra_decoffee",  "gra_tea",      "beer2",         "wine2",        
 "fortified2",    "spirits2")]


# some descriptives:


res<-compareGroups(~., data=FoodGrams, ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab
export2md(restab, strip = TRUE, first.strip = TRUE) 


write.csv(FoodGrams, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodGrams/13072022_GramsDayAll.csv")

names(FoodGrams)

