#############################################
####      GROUPS AND SUBGROUPS GRAMS     ####
####            21 April 2017            ####
####          Noelia and Esther          ####
#############################################

setwd("R:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/FoodGrams")

MyGrams<-read.csv("20042017_PanGen_Diet_Grams_StandardServing.csv", header=T, sep=";", na.strings="NA") ## PanGen imputed dataset by standardized servings
# MyGrams<-read.csv("20042017_PanGen_Diet_Grams_SEXSERVING.csv", header=T, sep=";", na.strings="NA") ## PanGen imputed dataset by sex diferenciated servings


myvars<-aa # with zero assigned; for Food Grams


## Groups (gra_g...) and subgroups (gra_sg...)

## Dairy products

myvars$gra_galldairy<-myvars$gra8200+myvars$gra8100+myvars$gra8600+myvars$gra8500+myvars$gra8800+myvars$gra9000+myvars$gra9200+myvars$gra9400

myvars$gra_sgmilkyogurt<-myvars$gra8200+myvars$gra8100+myvars$gra8600+myvars$gra8500+myvars$gra9400
myvars$gra_sgcheesse<-myvars$gra8800+myvars$gra9000
myvars$gra_sgdairydessert<-myvars$gra9200

## Meats

myvars$gra_gallmeat<-myvars$gra_chi_tot+myvars$gra_vea_tot+myvars$gra_por_tot+myvars$gra_lam_tot+myvars$gra1200+myvars$gra1300+myvars$gra1400+myvars$gra1500+myvars$gra600+myvars$gra700+myvars$gra2500+myvars$gra2100+myvars$gra2200+myvars$gra2300

myvars$gra_sgwhitemeat<-myvars$gra_chi_tot+myvars$gra1200
myvars$gra_sgredmeat<-myvars$gra_vea_tot+myvars$gra_por_tot+myvars$gra_lam_tot
myvars$gra_sgorganmeat<-myvars$gra1300+myvars$gra1400
myvars$gra_sgcuredmeat<-myvars$gra2100+myvars$gra2200+myvars$gra2300
myvars$gra_sgprocessedmeat<-myvars$gra600+myvars$gra700+myvars$gra2500
myvars$gra_sgcuredprocessedmeat<-myvars$gra2100+myvars$gra2200+myvars$gra2300+myvars$gra600+myvars$gra700+myvars$gra2500

## Seafood

myvars$gra_gallsea<-myvars$gra_wfish_tot+myvars$gra_bfish_tot+myvars$gra3200+myvars$gra3100+myvars$gra1800

myvars$gra_sgfish<-myvars$gra_wfish_tot+myvars$gra_bfish_tot
myvars$gra_sgothersea<-myvars$gra3200+myvars$gra3100+myvars$gra1800

## SEAFOOD+MEAT

myvars$gra_meatseafood<-myvars$gra_chi_tot+myvars$gra_vea_tot+myvars$gra_por_tot+myvars$gra_lam_tot+myvars$gra1200+myvars$gra1300+myvars$gra1400+myvars$gra1500+myvars$gra600+myvars$gra700+myvars$gra2500+myvars$gra2100+myvars$gra2200+myvars$gra2300+myvars$gra_wfish_tot+myvars$gra_bfish_tot+myvars$gra3200+myvars$gra3100+myvars$gra1800

## Already-to-eat dishes

myvars$gra_gallready<-myvars$gra2700+myvars$gra2800+myvars$gra_c_2900+myvars$gra13500

## Vegetables and legumes

myvars$gra_gallveg<-myvars$gra3500+myvars$gra4500+myvars$gra3400+myvars$gra3600+myvars$gra3800+myvars$gra4200+myvars$gra4300+myvars$gra5000+myvars$gra3700+myvars$gra4700+myvars$gra5200+myvars$gra4100+myvars$gra5100+myvars$gra11630+myvars$gra10300+myvars$gra10200

myvars$gra_sg1leafyveg<-myvars$gra3500+myvars$gra3400
myvars$gra_sg1starchveg<-myvars$gra4200+myvars$gra10300+myvars$gra10200
myvars$gra_sg1sgfruitingveg<-myvars$gra3600+myvars$gra3800+myvars$gra3700+myvars$gra4700+myvars$gra5200+myvars$gra4100+myvars$gra11630
myvars$gra_sg1sggrainsveg<-myvars$gra4300+myvars$gra5000+myvars$gra5100
myvars$gra_sg2redyellveg<-myvars$gra3600+myvars$gra4200+myvars$gra5100
myvars$gra_sg2greenveg<-myvars$gra3500+myvars$gra3400+myvars$gra3800+myvars$gra4300+myvars$gra5000+myvars$gra3700+myvars$gra4700+myvars$gra5200
myvars$gra_sg2whiteveg<-myvars$gra3800+myvars$gra11630+myvars$gra4500+myvars$gra4100

myvars$gra_gleg<-myvars$gra5810 ##only one

## Fruits and nuts

myvars$gra_gallfru<-myvars$gra6200+myvars$gra6300+myvars$gra6400+myvars$gra6500+myvars$gra7000+myvars$gra7200+myvars$gra6600+myvars$gra7300+myvars$gra6700+myvars$gra7600

myvars$gra_golives<-myvars$gra7800

myvars$gra_gnuts<-myvars$gra7900

## Bread and cereals

myvars$gra_gallcer<-myvars$gra9600+myvars$gra9700+myvars$gra9800+myvars$gra10400+myvars$gra10500+myvars$gra11000

myvars$gra_sgbread<-myvars$gra9600+myvars$gra9700
myvars$gra_sgricepasta<-myvars$gra10400+myvars$gra10500
myvars$gra_gallfats<-myvars$gra11700+myvars$gra11800+myvars$gra11300

## Cakes and sweets

myvars$gra_gflour<-myvars$gra12200+myvars$gra12500

myvars$gra_gchoc<-myvars$gra12900

myvars$gra_gsugar<-myvars$gra13100+myvars$gra13200

myvars$gra_gsauces<-myvars$gra11400+myvars$gra11610

## Non alcoholic beberages and juices

myvars$gra_gnonalc<-myvars$gra14300+myvars$gra14400+myvars$gra14500+myvars$gra14700+myvars$gra15201

myvars$gra_sgsugbev<-myvars$gra14300+myvars$gra14500+myvars$gra14700
myvars$gra_sgsoftdr<-myvars$gra14300+myvars$gra14400
myvars$gra_sgjuice<-myvars$gra14500+myvars$gra14700


#setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/Esther Molina/NOELIA_DIETA_PanGen/PANGEN/FoodGrams") # change the path, if so needed
write.csv(myvars, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodGrams/14072022_GramsDayall_StandardServing(GROUPS).csv") ## Save data
# write.csv(myvars, "20042017_PanGen_Diet_Grams_SEXSERVING(GROUPS).csv") ## Save data


