

######################################
### 20 July 2022 #####################
### Merge the data for Pangen & Microbiome ###
### Define carbohydrate rich foods within both studies ###
#####################################


# Need to combine the data for freq/day, grams/d and by groups, and nutrients/day
# Supplemental variables are not merged since this was collected for the microbiome study only

# 1) Load the pangen and microbiome data:
# a) Frequencies:

foodfq_micro<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodFreq/14072022_FoodFreqDay with Zeros.csv", header=TRUE)
foodfq_pangen<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/FoodFrequency/20042017_PanGen_Diet_Freq.csv", header=TRUE)

data<-c("subject", "TIPO", "fre8200",       "fre8100",       "fre8600",       "fre8500",       "fre8800",       "fre9000",       "fre9200",     
        "fre8400",      "fre300",        "fre_chi_tot",   "fre_vea_tot",   "fre_por_tot",   "fre_lam_tot",   "fre1200",      
        "fre1300",       "fre1400",      "fre1500",       "fre_wfish_tot", "fre_bfish_tot", "fre3100",       "fre3200",      
        "fre1800",       "fre600",      "fre700",        "fre2500",       "fre2100",       "fre2200",       "fre2300",     
        "fre2700",      "fre2800",       "fre_c_2900",    "fre3500",       "fre4500",       "fre3400",       "fre3600",     
        "fre3800",       "fre4200",       "fre4300",       "fre5000",       "fre3700",       "fre4700",       "fre5200",     
        "fre4100",       "fre5100",       "fre5810",       "fre6200",       "fre6300",       "fre6400",       "fre6500",      
        "fre7000",       "fre7200",       "fre6600",       "fre7300",       "fre6700",       "fre7600",       "fre7800",      
        "fre7900",       "fre9600",       "fre9700",       "fre9800",       "fre10300",      "fre10200",      "fre10400",     
        "fre10500",      "fre11000",      "fre11700",      "fre11800",      "fre12200",      "fre12500",      "fre12900",     
        "fre13100",      "fre13200",      "fre11300",      "fre11400",      "fre11610",      "fre11630",      "fre13500",     
        "fre14300",      "fre14400",      "fre14500",      "fre14700",      "fre15201",      "fre_coffee",    "fre_tea",      
 "beer",          "wine",          "spirits",       "fortified")

pangen<-foodfq_pangen[data]

colnames(pangen)<-c("subject", "TIPO", "fre8200",       "fre8100",       "fre8600",       "fre8500",       "fre8800",       "fre9000",       "fre9200",     
                    "fre9400",      "fre300",        "fre_chi_tot",   "fre_vea_tot",   "fre_por_tot",   "fre_lam_tot",   "fre1200",      
                    "fre1300",       "fre1400",      "fre1500",       "fre_wfish_tot", "fre_bfish_tot", "fre3100",       "fre3200",      
                    "fre1800",       "fre600",      "fre700",        "fre2500",       "fre2100",       "fre2200",       "fre2300",     
                    "fre2700",      "fre2800",       "fre_c_2900",    "fre3500",       "fre4500",       "fre3400",       "fre3600",     
                    "fre3800",       "fre4200",       "fre4300",       "fre5000",       "fre3700",       "fre4700",       "fre5200",     
                    "fre4100",       "fre5100",       "fre5810",       "fre6200",       "fre6300",       "fre6400",       "fre6500",      
                    "fre7000",       "fre7200",       "fre6600",       "fre7300",       "fre6700",       "fre7600",       "fre7800",      
                    "fre7900",       "fre9600",       "fre9700",       "fre9800",       "fre10300",      "fre10200",      "fre10400",     
                    "fre10500",      "fre11000",      "fre11700",      "fre11800",      "fre12200",      "fre12500",      "fre12900",     
                    "fre13100",      "fre13200",      "fre11300",      "fre11400",      "fre11610",      "fre11630",      "fre13500",     
                    "fre14300",      "fre14400",      "fre14500",      "fre14700",      "fre15201",      "fre_coffee",    "fre_tea",      
                    "beer2",          "wine2",          "sprits2",       "fortified2")


pangen$fre_decoffee<-NA

foodfq_micro$TIPO<-NA

data<-c("subject", "TIPO", "fre8200",       "fre8100",       "fre8600",       "fre8500",       "fre8800",       "fre9000",       "fre9200",     
        "fre9400",      "fre300",        "fre_chi_tot",   "fre_vea_tot",   "fre_por_tot",   "fre_lam_tot",   "fre1200",      
        "fre1300",       "fre1400",      "fre1500",       "fre_wfish_tot", "fre_bfish_tot", "fre3100",       "fre3200",      
        "fre1800",       "fre600",      "fre700",        "fre2500",       "fre2100",       "fre2200",       "fre2300",     
        "fre2700",      "fre2800",       "fre_c_2900",    "fre3500",       "fre4500",       "fre3400",       "fre3600",     
        "fre3800",       "fre4200",       "fre4300",       "fre5000",       "fre3700",       "fre4700",       "fre5200",     
        "fre4100",       "fre5100",       "fre5810",       "fre6200",       "fre6300",       "fre6400",       "fre6500",      
        "fre7000",       "fre7200",       "fre6600",       "fre7300",       "fre6700",       "fre7600",       "fre7800",      
        "fre7900",       "fre9600",       "fre9700",       "fre9800",       "fre10300",      "fre10200",      "fre10400",     
        "fre10500",      "fre11000",      "fre11700",      "fre11800",      "fre12200",      "fre12500",      "fre12900",     
        "fre13100",      "fre13200",      "fre11300",      "fre11400",      "fre11610",      "fre11630",      "fre13500",     
        "fre14300",      "fre14400",      "fre14500",      "fre14700",      "fre15201",      "fre_coffee",    "fre_decoffee", "fre_tea",      
        "beer2",          "wine2",          "sprits2",       "fortified2")


micro<-foodfq_micro[data]

names(micro)
names(pangen)

# oder them similarly:

micro<-micro[,c("subject", "TIPO", "fre8200",       "fre8100",       "fre8600",       "fre8500",       "fre8800",       "fre9000",       "fre9200",     
                "fre9400",      "fre300",        "fre_chi_tot",   "fre_vea_tot",   "fre_por_tot",   "fre_lam_tot",   "fre1200",      
                "fre1300",       "fre1400",      "fre1500",       "fre_wfish_tot", "fre_bfish_tot", "fre3100",       "fre3200",      
                "fre1800",       "fre600",      "fre700",        "fre2500",       "fre2100",       "fre2200",       "fre2300",     
                "fre2700",      "fre2800",       "fre_c_2900",    "fre3500",       "fre4500",       "fre3400",       "fre3600",     
                "fre3800",       "fre4200",       "fre4300",       "fre5000",       "fre3700",       "fre4700",       "fre5200",     
                "fre4100",       "fre5100",       "fre5810",       "fre6200",       "fre6300",       "fre6400",       "fre6500",      
                "fre7000",       "fre7200",       "fre6600",       "fre7300",       "fre6700",       "fre7600",       "fre7800",      
                "fre7900",       "fre9600",       "fre9700",       "fre9800",       "fre10300",      "fre10200",      "fre10400",     
                "fre10500",      "fre11000",      "fre11700",      "fre11800",      "fre12200",      "fre12500",      "fre12900",     
                "fre13100",      "fre13200",      "fre11300",      "fre11400",      "fre11610",      "fre11630",      "fre13500",     
                "fre14300",      "fre14400",      "fre14500",      "fre14700",      "fre15201",      "fre_coffee",    "fre_decoffee", "fre_tea",      
                "beer2",          "wine2",          "sprits2",       "fortified2")]

pangen<-pangen[,c("subject", "TIPO", "fre8200",       "fre8100",       "fre8600",       "fre8500",       "fre8800",       "fre9000",       "fre9200",     
                "fre9400",      "fre300",        "fre_chi_tot",   "fre_vea_tot",   "fre_por_tot",   "fre_lam_tot",   "fre1200",      
                "fre1300",       "fre1400",      "fre1500",       "fre_wfish_tot", "fre_bfish_tot", "fre3100",       "fre3200",      
                "fre1800",       "fre600",      "fre700",        "fre2500",       "fre2100",       "fre2200",       "fre2300",     
                "fre2700",      "fre2800",       "fre_c_2900",    "fre3500",       "fre4500",       "fre3400",       "fre3600",     
                "fre3800",       "fre4200",       "fre4300",       "fre5000",       "fre3700",       "fre4700",       "fre5200",     
                "fre4100",       "fre5100",       "fre5810",       "fre6200",       "fre6300",       "fre6400",       "fre6500",      
                "fre7000",       "fre7200",       "fre6600",       "fre7300",       "fre6700",       "fre7600",       "fre7800",      
                "fre7900",       "fre9600",       "fre9700",       "fre9800",       "fre10300",      "fre10200",      "fre10400",     
                "fre10500",      "fre11000",      "fre11700",      "fre11800",      "fre12200",      "fre12500",      "fre12900",     
                "fre13100",      "fre13200",      "fre11300",      "fre11400",      "fre11610",      "fre11630",      "fre13500",     
                "fre14300",      "fre14400",      "fre14500",      "fre14700",      "fre15201",      "fre_coffee",    "fre_decoffee", "fre_tea",      
                "beer2",          "wine2",          "sprits2",       "fortified2")]


pangen.micro<-rbind(pangen, micro)

# save the data:

write.csv(pangen.micro, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME_PANGEN/FoodFreq/20072022_FoodFreqPM.csv")


# b) grams:

food_micro<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodGrams/14072022_GramsDayAll with Zeros.csv", header=TRUE)
food_pangen<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/FoodGrams/20042017_PanGen_Diet_Grams_StandardServing.csv", header=TRUE)

identical(food_pangen$subject, pangen$subject)

food_pangen$fre_coffee<-pangen$fre_coffee
food_pangen$fre_decoffee<-pangen$fre_decoffee
food_pangen$fre_tea<-pangen$fre_tea
food_pangen$beer2<-pangen$beer2
food_pangen$wine2<-pangen$wine2
food_pangen$sprits2<-pangen$sprits2
food_pangen$fortified2<-pangen$fortified2

data<-c("subject", "TIPO", "fre8200",       "fre8100",       "fre8600",       "fre8500",       "fre8800",       "fre9000",       "fre9200",     
        "fre8400",      "fre300",        "fre_chi_tot",   "fre_vea_tot",   "fre_por_tot",   "fre_lam_tot",   "fre1200",      
        "fre1300",       "fre1400",      "fre1500",       "fre_wfish_tot", "fre_bfish_tot", "fre3100",       "fre3200",      
        "fre1800",       "fre600",      "fre700",        "fre2500",       "fre2100",       "fre2200",       "fre2300",     
        "fre2700",      "fre2800",       "fre_c_2900",    "fre3500",       "fre4500",       "fre3400",       "fre3600",     
        "fre3800",       "fre4200",       "fre4300",       "fre5000",       "fre3700",       "fre4700",       "fre5200",     
        "fre4100",       "fre5100",       "fre5810",       "fre6200",       "fre6300",       "fre6400",       "fre6500",      
        "fre7000",       "fre7200",       "fre6600",       "fre7300",       "fre6700",       "fre7600",       "fre7800",      
        "fre7900",       "fre9600",       "fre9700",       "fre9800",       "fre10300",      "fre10200",      "fre10400",     
        "fre10500",      "fre11000",      "fre11700",      "fre11800",      "fre12200",      "fre12500",      "fre12900",     
        "fre13100",      "fre13200",      "fre11300",      "fre11400",      "fre11610",      "fre11630",      "fre13500",     
        "fre14300",      "fre14400",      "fre14500",      "fre14700",      "fre15201")

pangen<-food_pangen[data]

colnames(pangen)<-c("subject", "TIPO", "gra8200",       "gra8100",       "gra8600",       "gra8500",       "gra8800",       "gra9000",       "gra9200",     
                    "gra9400",      "gra300",        "gra_chi_tot",   "gra_vea_tot",   "gra_por_tot",   "gra_lam_tot",   "gra1200",      
                    "gra1300",       "gra1400",      "gra1500",       "gra_wfish_tot", "gra_bfish_tot", "gra3100",       "gra3200",      
                    "gra1800",       "gra600",      "gra700",        "gra2500",       "gra2100",       "gra2200",       "gra2300",     
                    "gra2700",      "gra2800",       "gra_c_2900",    "gra3500",       "gra4500",       "gra3400",       "gra3600",     
                    "gra3800",       "gra4200",       "gra4300",       "gra5000",       "gra3700",       "gra4700",       "gra5200",     
                    "gra4100",       "gra5100",       "gra5810",       "gra6200",       "gra6300",       "gra6400",       "gra6500",      
                    "gra7000",       "gra7200",       "gra6600",       "gra7300",       "gra6700",       "gra7600",       "gra7800",      
                    "gra7900",       "gra9600",       "gra9700",       "gra9800",       "gra10300",      "gra10200",      "gra10400",     
                    "gra10500",      "gra11000",      "gra11700",      "gra11800",      "gra12200",      "gra12500",      "gra12900",     
                    "gra13100",      "gra13200",      "gra11300",      "gra11400",      "gra11610",      "gra11630",      "gra13500",     
                    "gra14300",      "gra14400",      "gra14500",      "gra14700",      "gra15201")



food_micro$TIPO<-NA

data<-c("subject", "TIPO", "gra8200",       "gra8100",       "gra8600",       "gra8500",       "gra8800",       "gra9000",       "gra9200",     
                    "gra9400",      "gra300",        "gra_chi_tot",   "gra_vea_tot",   "gra_por_tot",   "gra_lam_tot",   "gra1200",      
                    "gra1300",       "gra1400",      "gra1500",       "gra_wfish_tot", "gra_bfish_tot", "gra3100",       "gra3200",      
                    "gra1800",       "gra600",      "gra700",        "gra2500",       "gra2100",       "gra2200",       "gra2300",     
                    "gra2700",      "gra2800",       "gra_c_2900",    "gra3500",       "gra4500",       "gra3400",       "gra3600",     
                    "gra3800",       "gra4200",       "gra4300",       "gra5000",       "gra3700",       "gra4700",       "gra5200",     
                    "gra4100",       "gra5100",       "gra5810",       "gra6200",       "gra6300",       "gra6400",       "gra6500",      
                    "gra7000",       "gra7200",       "gra6600",       "gra7300",       "gra6700",       "gra7600",       "gra7800",      
                    "gra7900",       "gra9600",       "gra9700",       "gra9800",       "gra10300",      "gra10200",      "gra10400",     
                    "gra10500",      "gra11000",      "gra11700",      "gra11800",      "gra12200",      "gra12500",      "gra12900",     
                    "gra13100",      "gra13200",      "gra11300",      "gra11400",      "gra11610",      "gra11630",      "gra13500",     
                    "gra14300",      "gra14400",      "gra14500",      "gra14700",      "gra15201")


micro<-food_micro[data]


pangen.micro<-rbind(pangen, micro)

# save the data:

write.csv(pangen.micro, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME_PANGEN/FoodGrams/20072022_FoodGramsPM.csv")


# generate here the food groups:

myvars<-pangen.micro

# run the script of food groups

#save the data

write.csv(myvars, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME_PANGEN/FoodGrams/20072022_FoodGramsPM(GROUPS).csv")


# c) nutrients:


nut_micro<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodNutrients/20072022_Nutrients_Serving.csv", header=TRUE)
nut_pangen<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/FoodNutrient/20042017_PanGen_Nutrients_StandarServing.csv", header=TRUE)


data<-c("subject", "TIPO", "kJ",                "kcal",              "Lipids.g",          "Proteins.g",
        "Humidity.g",  "Carbs.g",           "Sucrose.g",         "Fiber.g",           "Starch.g",          "Sugar.g",          
   "Cholesterol.mg",    "VitA.ug",           "VitD.ug",           "VitE.mg",         "VitB8.ug",         
   "VitB9.ug",          "VitB3.mg",          "VitB5.mg",          "VitB2.mg",          "VitB1.mg",         
   "VitB12.ug",         "VitB6.mg",          "VitC.mg",           "Calcium.mg",        "Iron.mg",          
   "Potasium.mg",       "Magnesium.mg",      "Sodium.mg",         "Phosphorus.mg",     "Copper.mg",        
   "Iodide.ug",         "Selenium.ug",       "Zinc.mg",           "Linoleic.g",        "Linolenic.mg",     
   "Araquidonic.gm",    "DHA.g",             "EPA.g",             "Estearic.g",        "Lauric.g",         
  "Miristic.g",        "Polyunsaturated.g", "Saturated.g",       "Trans.g",           "MFA.g")


pangen<-nut_pangen[data]

nut_micro$TIPO<-NA
 See
micro<-nut_micro[data]


pangen.micro<-rbind(pangen, micro)


write.csv(pangen.micro, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME_PANGEN/FoodNutrients/20072022_FoodNutrientsPM.csv")


##########################################################################################
### FOR THE GLYCEMIC INDEX:


#diary products

names(micro)[names(micro) == "leche_entera_sola_o_con_ca"] <- "fre8200"
names(micro)[names(micro) == "leche_semi_desnatada_o_des"] <- "fre8100"
names(micro)[names(micro) == "yogur_entero_uno"] <- "fre8600"
names(micro)[names(micro) == "yogur_desnatado_uno_125_gr"] <- "fre8500"
names(micro)[names(micro) == "queso_blanco_o_fresco_troz"] <- "fre8800"
names(micro)[names(micro) == "queso_curado_semi_curado_c"] <- "fre9000"
names(micro)[names(micro) == "natillas_flan_p_din"] <- "fre9200"
names(micro)[names(micro) == "helados_un_cucurucho_o_vas"] <- "fre9400"

## Vegetables and legumes

names(micro)[names(micro) == "cebolla_una_mediana"] <- "fre3800"
names(micro)[names(micro) == "zanahoria_calabaza_una_o_p"] <- "fre4200"
names(micro)[names(micro) == "jud_as_verdes_cocinadas_1"] <- "fre4300"
names(micro)[names(micro) == "guisantes_1_plato"] <- "fre5000"
names(micro)[names(micro) == "ma_z_hervido_plato_o_lata"] <- "fre5100"
names(micro)[names(micro) == "legumbres_lentejas_garbanz"] <- "fre5810"


## Fruits

names(micro)[names(micro) == "naranjas_mandarinas_una"] <- "fre6200"
names(micro)[names(micro) == "pl_tano_uno"] <- "fre6300"
names(micro)[names(micro) == "manzana_una_mediana"] <- "fre6400"
names(micro)[names(micro) == "pera_una_mediana"] <- "fre6500"
names(micro)[names(micro) == "melocot_n_nectarina_albari"] <- "fre7000"
names(micro)[names(micro) == "sand_a_mel_n_1_tajada_o_ca"] <- "fre7200"
names(micro)[names(micro) == "uvas_un_racimo_mediano_o_p"] <- "fre6600"
names(micro)[names(micro) == "prunas_ciruelas_frescas_se"] <- "fre7300"

names(micro)[names(micro) == "kiwi_una_unidad"] <- "fre6700"
names(micro)[names(micro) == "frutas_en_alm_bar_melocot"] <- "fre7600"
names(micro)[names(micro) == "aceitunas_un_platito_o_tap"] <- "fre7800"
names(micro)[names(micro) == "frutos_secos_almendras_cac"] <- "fre7900"

## Bread, cereals and potatoes

names(micro)[names(micro) == "pan_blanco_una_pieza_peque"] <- "fre9600"
names(micro)[names(micro) == "pan_integral_pieza_peque_a"] <- "fre9700"
names(micro)[names(micro) == "cereales_desayuno_30_g_en"] <- "fre9800"

names(micro)[names(micro) == "patatas_fritas_no_chips_1"] <- "fre10300"
names(micro)[names(micro) == "patatas_cocidas_asadas_1_p"] <- "fre10200"

names(micro)[names(micro) == "arroz_cocinado_1_plato_med"] <- "fre10400"

names(micro)[names(micro) == "pastas_espaguetis_fideos_m"] <- "fre10500"
names(micro)[names(micro) == "pizza_1_porci_n_o_raci_n_2"] <- "fre11000"



## Sweets and others

names(micro)[names(micro) == "galletas_tipo_mar_a_o_gall"] <- "fre12200"
names(micro)[names(micro) == "boller_a_croissant_donut_m"] <- "fre12500"
names(micro)[names(micro) == "chocolate_bombones_y_simil"] <- "fre12900"
names(micro)[names(micro) == "mermeladas_miel_1_cucharad"] <- "fre13100"
names(micro)[names(micro) == "az_car_ej_en_el_caf_postre"] <- "fre13200"


names(micro)[names(micro) == "bolsa_de_patatas_fritas_o"] <- "fre13500"

## Non alcoholic beverages and juices

names(micro)[names(micro) == "refrescos_normales_de_cola"] <- "fre14300"
names(micro)[names(micro) == "refrescos_sin_az_car_cola"] <- "fre14400"

names(micro)[names(micro) == "zumo_de_naranja_natural_1"] <- "fre14500"
names(micro)[names(micro) == "zumo_de_frutas_envasado_1"] <- "fre14700"
names(micro)[names(micro) == "bebidas_de_cereales_o_suce"] <- "fre15201"




