

#### SCRIPT FOR MICROBIOME DATA COMPILATION ####


base_dir1 <- '//bespin/Human_Cancer_Genetics/Genetic_and_Molecular_Epidemiology/Backup/NOELIA_DIETA_PanGen/'

base_dir2 <- '//bespin/Human_Cancer_Genetics/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet'



#micro<- read.csv(file.path(base_dir2, "MICROBIOME", "Microbioma_DATA_LABELS_2019-09-30_1431.csv", header=T, sep=";", na.strings=c("9999", "8888", "NA")))## Open the PanGen questionnaire data

micro<- read.csv(file.path(base_dir2, "MICROBIOME", "Microbioma_DATA_2019-09-30_1431.csv"))## Open the PanGen questionnaire data


# transform text of frequency of intake into categorical data; already done by loading the correct data without labels

# rename all the variables to homogeneize

names(micro)[names(micro) == "redcap_event_name"] <- "section"
micro$section<-"Diet"

#diary products

names(micro)[names(micro) == "leche_entera_sola_o_con_ca"] <- "fre8200"
names(micro)[names(micro) == "leche_semi_desnatada_o_des"] <- "fre8100"
names(micro)[names(micro) == "yogur_entero_uno"] <- "fre8600"
names(micro)[names(micro) == "yogur_desnatado_uno_125_gr"] <- "fre8500"
names(micro)[names(micro) == "queso_blanco_o_fresco_troz"] <- "fre8800"
names(micro)[names(micro) == "queso_curado_semi_curado_c"] <- "fre9000"
names(micro)[names(micro) == "natillas_flan_p_din"] <- "fre9200"
names(micro)[names(micro) == "helados_un_cucurucho_o_vas"] <- "fre9400"

## Fowl and eggs


names(micro)[names(micro) == "aves_pollo_pavo_pato_codor"] <- "fre_chi_tot"
names(micro)[names(micro) == "huevos_de_gallina_uno"] <- "fre300"
names(micro)[names(micro) == "a_la_plancha_o_sart_n"] <- "fre100"
names(micro)[names(micro) == "frito_en_mucho_aceite"] <- "fre130"
names(micro)[names(micro) == "guisado_estofado_en_salsa"] <- "fre140"
names(micro)[names(micro) == "asado_o_al_horno"] <- "fre110"
names(micro)[names(micro) == "a_la_brasa_o_barbacoa"] <- "fre150"

names(micro)[names(micro) == "otro_especificar"] <- "fre160"

## Beef


names(micro)[names(micro) == "carne_de_ternera_buey_vaca"] <- "fre_vea_tot"
names(micro)[names(micro) == "a_la_plancha_vaca"] <- "fre500"
names(micro)[names(micro) == "frita_en_mucho_aceite"] <- "fre520"
names(micro)[names(micro) == "guisada_estofada_en_salsa"] <- "fre530"
names(micro)[names(micro) == "asada_o_al_horno"] <- "fre540"
names(micro)[names(micro) == "a_la_brasa_o_barbacoa_vaca"] <- "fre550"
names(micro)[names(micro) == "otro_especificar_vaca"] <- "fre560"

## Pork

names(micro)[names(micro) == "carne_de_cerdo_excluya_sal"] <- "fre_por_tot"
names(micro)[names(micro) == "a_la_plancha_cerdo"] <- "fre400"
names(micro)[names(micro) == "frito_en_cerdo"] <- "fre440"
names(micro)[names(micro) == "guisado_estofado_cerdo"] <- "fre420"
names(micro)[names(micro) == "asado_o_al_horno_cerdo"] <- "fre410"
names(micro)[names(micro) == "a_la_brasa_cerdo"] <- "fre430"
names(micro)[names(micro) == "otro_especificar_cerdo"] <- "fre460"

## Lamb and goat

names(micro)[names(micro) == "carne_de_cordero_o_cabra_1"] <- "fre_lam_tot"

names(micro)[names(micro) == "a_la_plancha_cordero"] <- "fre1100"
names(micro)[names(micro) == "frito_con_cordero"] <- "fre1110"
names(micro)[names(micro) == "guisado_cordero"] <- "fre1130"
names(micro)[names(micro) == "asada_cordero"] <- "fre1140"
names(micro)[names(micro) == "a_la_brasa_cordero"] <- "fre1150"
names(micro)[names(micro) == "otro_especificar_cordero"] <- "fre1160"

## Other kind of meat


names(micro)[names(micro) == "conejo_o_liebre_1_pieza_o"] <- "fre1200"
names(micro)[names(micro) == "h_gado_de_ternera_cerdo_o"] <- "fre1300"
names(micro)[names(micro) == "otras_v_sceras_sesos_molle"] <- "fre1400"
names(micro)[names(micro) == "bac_n_tocino_o_panceta_2_l"] <- "fre1500"

## Fish

names(micro)[names(micro) == "pescado_blanco_fresco_cong"] <- "fre_wfish_tot"
names(micro)[names(micro) == "pescado_azul_fresco_congel"] <- "fre_bfish_tot"
names(micro)[names(micro) == "a_la_plancha_pescado"] <- "fre1600"
names(micro)[names(micro) == "frito_pescado"] <- "fre1610"
names(micro)[names(micro) == "guisado_pescado"] <- "fre1630"
names(micro)[names(micro) == "asada_pescado"] <- "fre1640"
names(micro)[names(micro) == "a_la_brasa_pescado"] <- "fre1650"
names(micro)[names(micro) == "otro_especificar_pescado"] <- "fre1660"

names(micro)[names(micro) == "una_lata_peque_a_de_conser"] <- "fre3100"
names(micro)[names(micro) == "pescados_en_salaz_n_y_o_ah"] <- "fre3200"
names(micro)[names(micro) == "calamares_chipirones_sepia"] <- "fre1800"

## Ready-to-eat dishes

names(micro)[names(micro) == "salchichas_frescas_butifar"] <- "fre600"
names(micro)[names(micro) == "hamburguesas_alb_ndigas_un"] <- "fre700"
names(micro)[names(micro) == "frankfurt_hot_dog_uno"] <- "fre2500"
names(micro)[names(micro) == "jam_n_dulce_york_o_cocido"] <- "fre2100"
names(micro)[names(micro) == "jam_n_salado_serrano_pa_s"] <- "fre2200"
names(micro)[names(micro) == "salami_salchich_n_fuet_cho"] <- "fre2300"
names(micro)[names(micro) == "croquetas_de_pollo_jam_n_u"] <- "fre2700"
names(micro)[names(micro) == "croquetas_palitos_o_delici"] <- "fre2800"

## Vegetables and legumes

names(micro)[names(micro) == "espinacas_o_acelgas_cocina"] <- "fre3500"
names(micro)[names(micro) == "col_coliflor_br_colis_coci"] <- "fre4500"
names(micro)[names(micro) == "lechuga_endibias_escarola"] <- "fre3400"
names(micro)[names(micro) == "tomate_crudo_uno_mediano"] <- "fre3600"
names(micro)[names(micro) == "cebolla_una_mediana"] <- "fre3800"
names(micro)[names(micro) == "zanahoria_calabaza_una_o_p"] <- "fre4200"
names(micro)[names(micro) == "jud_as_verdes_cocinadas_1"] <- "fre4300"
names(micro)[names(micro) == "guisantes_1_plato"] <- "fre5000"
names(micro)[names(micro) == "berenjenas_calabacines_pep"] <- "fre3700"
names(micro)[names(micro) == "pimientos_rojos_y_verdes_u"] <- "fre4700"
names(micro)[names(micro) == "alcachofas_una_raci_n_o_pl"] <- "fre5200"
names(micro)[names(micro) == "esp_rragos_una_raci_n_o_pl"] <- "fre4100"
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

## Oils 

names(micro)[names(micro) == "aceite_de_oliva_a_adido_en"] <- "fre11700"
names(micro)[names(micro) == "otros_aceites_vegetales_de"] <- "fre11800"

## Sweets and others

names(micro)[names(micro) == "galletas_tipo_mar_a_o_gall"] <- "fre12200"
names(micro)[names(micro) == "boller_a_croissant_donut_m"] <- "fre12500"
names(micro)[names(micro) == "chocolate_bombones_y_simil"] <- "fre12900"
names(micro)[names(micro) == "mermeladas_miel_1_cucharad"] <- "fre13100"
names(micro)[names(micro) == "az_car_ej_en_el_caf_postre"] <- "fre13200"
names(micro)[names(micro) == "mayonesa_1_cucharada_soper"] <- "fre11300"
names(micro)[names(micro) == "salsa_de_tomate_media_taza"] <- "fre11400"
names(micro)[names(micro) == "ketchup_1_cucharada_sopera"] <- "fre11610"
names(micro)[names(micro) == "ajo_1_diente"] <- "fre11630"

names(micro)[names(micro) == "bolsa_de_patatas_fritas_o"] <- "fre13500"

## Non alcoholic beverages and juices

names(micro)[names(micro) == "refrescos_normales_de_cola"] <- "fre14300"
names(micro)[names(micro) == "refrescos_sin_az_car_cola"] <- "fre14400"

names(micro)[names(micro) == "zumo_de_naranja_natural_1"] <- "fre14500"
names(micro)[names(micro) == "zumo_de_frutas_envasado_1"] <- "fre14700"
names(micro)[names(micro) == "bebidas_de_cereales_o_suce"] <- "fre15201"

# ready dishes II
names(micro)[names(micro) == "lasa_a_canelones_pasteles"] <- "fre_c_2900"

# tea and coffee:  

names(micro)[names(micro) == "t_negro_verde_o_de_hiertas"] <- "fre_tea"
names(micro)[names(micro) == "caf_con_o_sin_leche"] <- "fre_coffee"
names(micro)[names(micro) == "caf_descafeinado_con_o_sin"] <- "fre_decoffee" # this variables does not exist in pangen?

# culinary processes:

names(micro)[names(micro) == "despu_cocinar_liqu_sarten"] <- "Q1oth" 
names(micro)[names(micro) == "frecuencia_tostada_quemada"] <- "Q2oth" 
names(micro)[names(micro) == "grasa_visible"] <- "Q3oth" 

names(micro)[names(micro) == "si_otro_especificar"] <- "Oth160" 
names(micro)[names(micro) == "cuando_com_a_pollo_u_otras"] <- "Q3oth" 
names(micro)[names(micro) == "pescado_otro_especificar"] <- "Oth1660" 
names(micro)[names(micro) == "cerdo_cocinado_otro_es"] <- "Oth460" 
names(micro)[names(micro) == "cordero_otro_especificar"] <- "Oth1160" 
names(micro)[names(micro) == "otros_especificar"] <- "Oth560" 

names(micro)[names(micro) == "c_mo_tomaba_normalmente_de"] <- "Q1chi"
names(micro)[names(micro) == "c_mo_tomaba_norm_cerdo"] <- "Q1por"
names(micro)[names(micro) == "carne_vaca_cocinada"] <- "Q1vea"
names(micro)[names(micro) == "c_mo_tomaba_cordero"] <- "Q1lam"

# questions on changes in habits:

names(micro)[names(micro) == "l_cteos"] <- "M104.1.1_dairy"
names(micro)[names(micro) == "carne"] <- "M105.1.1_meat"
names(micro)[names(micro) == "pescado"] <- "M106.1.1_fish"
names(micro)[names(micro) == "precocinados_preelaborados"] <- "M107.1.1_ready"
names(micro)[names(micro) == "verduras_legumbres"] <- "M108.1.1_vege"
names(micro)[names(micro) == "frutas"] <- "M109.1.1_fruits"
names(micro)[names(micro) == "pan_cereales"] <- "M110.1.1_cereals"
names(micro)[names(micro) == "aceites_y_grasas"] <- "M103.1.1_fats" #does not exist in pangen
names(micro)[names(micro) == "dulces_aperitivos_y_otros"] <- "M111.1.1_sugars"
names(micro)[names(micro) == "bebidas"] <- "M112.1.1_beve"
names(micro)[names(micro) == "ha_hecho_alguna_vez_alg_n"] <- "M103.1.1_change"

# rename ID variable

names(micro)[names(micro) == "ï..record_id"] <- "subject"


micro$multivitamin_F<-micro$complejos_polivitam_nicos___1
micro$multivitamin_F<-ifelse(micro$vitaminas==1 & micro$complejos_polivitam_nicos___2==1, 2, micro$multivitamin_F)
table(micro$multivitamin_F)
micro$multivitamin_F<-ifelse(micro$vitaminas==1 & micro$complejos_polivitam_nicos___3==1, 3, micro$multivitamin_F)
table(micro$multivitamin_F)
micro$multivitamin_F<-ifelse(micro$vitaminas==1 & micro$complejos_polivitam_nicos___4==1, 4, micro$multivitamin_F)
table(micro$multivitamin_F)

micro$multivitamin_D<-micro$complejos_polivitam_nicos___5
micro$multivitamin_D<-ifelse(micro$vitaminas==1 & micro$complejos_polivitam_nicos___6==1, 2, micro$multivitamin_D)
table(micro$multivitamin_D)
micro$multivitamin_D<-ifelse(micro$vitaminas==1 & micro$complejos_polivitam_nicos___7==1, 3, micro$multivitamin_D)
table(micro$multivitamin_D)
micro$multivitamin_D<-ifelse(micro$vitaminas==1 & micro$complejos_polivitam_nicos___8==1, 4, micro$multivitamin_D)
table(micro$multivitamin_D)
micro$multivitamin_D<-ifelse(micro$vitaminas==1 & micro$complejos_polivitam_nicos___9==1, 5, micro$multivitamin_D)
table(micro$multivitamin_D)

micro$vitaminC_F<-micro$vitamina_c_como_por_ejempl___1
micro$vitaminC_F<-ifelse(micro$vitaminas==1 & micro$vitamina_c_como_por_ejempl___2==1, 2, micro$vitaminC_F)
table(micro$vitaminC_F)
micro$vitaminC_F<-ifelse(micro$vitaminas==1 & micro$vitamina_c_como_por_ejempl___3==1, 3, micro$vitaminC_F)
table(micro$vitaminC_F)
micro$vitaminC_F<-ifelse(micro$vitaminas==1 & micro$vitamina_c_como_por_ejempl___4==1, 4, micro$vitaminC_F)
table(micro$vitaminC_F)


micro$vitaminC_D<-micro$vitamina_c_como_por_ejempl___5
micro$vitaminC_D<-ifelse(micro$vitaminas==1 & micro$vitamina_c_como_por_ejempl___6==1, 2, micro$vitaminC_D)
table(micro$vitaminC_D)
micro$vitaminC_D<-ifelse(micro$vitaminas==1 & micro$vitamina_c_como_por_ejempl___7==1, 3, micro$vitaminC_D)
table(micro$vitaminC_D)
micro$vitaminC_D<-ifelse(micro$vitaminas==1 & micro$vitamina_c_como_por_ejempl___8==1, 4, micro$vitaminC_D)
table(micro$vitaminC_D)
micro$vitaminC_D<-ifelse(micro$vitaminas==1 & micro$vitamina_c_como_por_ejempl___9==1, 5, micro$vitaminC_D)
table(micro$vitaminC_D)



micro$vitaminB_F<-micro$vitaminas_complejo_b___1
micro$vitaminB_F<-ifelse(micro$vitaminas==1 & micro$vitaminas_complejo_b___2==1, 2, micro$vitaminB_F)
table(micro$vitaminB_F)
micro$vitaminB_F<-ifelse(micro$vitaminas==1 & micro$vitaminas_complejo_b___3==1, 3, micro$vitaminB_F)
table(micro$vitaminB_F)
micro$vitaminB_F<-ifelse(micro$vitaminas==1 & micro$vitaminas_complejo_b___4==1, 4, micro$vitaminB_F)
table(micro$vitaminB_F)


micro$vitaminB_D<-micro$vitaminas_complejo_b___5
micro$vitaminB_D<-ifelse(micro$vitaminas==1 & micro$vitaminas_complejo_b___6==1, 2, micro$vitaminB_D)
table(micro$vitaminB_D)
micro$vitaminB_D<-ifelse(micro$vitaminas==1 & micro$vitaminas_complejo_b___7==1, 3, micro$vitaminB_D)
table(micro$vitaminB_D)
micro$vitaminB_D<-ifelse(micro$vitaminas==1 & micro$vitaminas_complejo_b___8==1, 4, micro$vitaminB_D)
table(micro$vitaminB_D)
micro$vitaminB_D<-ifelse(micro$vitaminas==1 & micro$vitaminas_complejo_b___9==1, 5, micro$vitaminB_D)
table(micro$vitaminB_D)



micro$vitaminA_F<-micro$vitamina_a___1
micro$vitaminA_F<-ifelse(micro$vitaminas==1 & micro$vitamina_a___2==1, 2, micro$vitaminA_F)
table(micro$vitaminA_F)
micro$vitaminA_F<-ifelse(micro$vitaminas==1 & micro$vitamina_a___3==1, 3, micro$vitaminA_F)
table(micro$vitaminA_F)
micro$vitaminA_F<-ifelse(micro$vitaminas==1 & micro$vitamina_a___4==1, 4, micro$vitaminA_F)
table(micro$vitaminA_F)


micro$vitaminA_D<-micro$vitamina_a___5
micro$vitaminA_D<-ifelse(micro$vitaminas==1 & micro$vitamina_a___6==1, 2, micro$vitaminA_D)
table(micro$vitaminA_D)
micro$vitaminA_D<-ifelse(micro$vitaminas==1 & micro$vitamina_a___7==1, 3, micro$vitaminA_D)
table(micro$vitaminA_D)
micro$vitaminA_D<-ifelse(micro$vitaminas==1 & micro$vitamina_a___8==1, 4, micro$vitaminA_D)
table(micro$vitaminA_D)
micro$vitaminA_D<-ifelse(micro$vitaminas==1 & micro$vitamina_a___9==1, 5, micro$vitaminA_D)
table(micro$vitaminA_D)





micro$iron_F<-micro$hierro___1
micro$iron_F<-ifelse(micro$vitaminas==1 & micro$hierro___2==1, 2, micro$iron_F)
table(micro$iron_F)
micro$iron_F<-ifelse(micro$vitaminas==1 & micro$hierro___3==1, 3, micro$iron_F)
table(micro$iron_F)
micro$iron_F<-ifelse(micro$vitaminas==1 & micro$hierro___4==1, 4, micro$iron_F)
table(micro$iron_F)


micro$iron_D<-micro$hierro___5
micro$iron_D<-ifelse(micro$vitaminas==1 & micro$hierro___6==1, 2, micro$iron_D)
table(micro$iron_D)
micro$iron_D<-ifelse(micro$vitaminas==1 & micro$hierro___7==1, 3, micro$iron_D)
table(micro$iron_D)
micro$iron_D<-ifelse(micro$vitaminas==1 & micro$hierro___8==1, 4, micro$iron_D)
table(micro$iron_D)
micro$iron_D<-ifelse(micro$vitaminas==1 & micro$hierro___9==1, 5, micro$iron_D)
table(micro$iron_D)




micro$cavitD_F<-micro$calcio_calcio_vitamina_d___1
micro$cavitD_F<-ifelse(micro$vitaminas==1 & micro$calcio_calcio_vitamina_d___2==1, 2, micro$cavitD_F)
table(micro$cavitD_F)
micro$cavitD_F<-ifelse(micro$vitaminas==1 & micro$calcio_calcio_vitamina_d___3==1, 3, micro$cavitD_F)
table(micro$cavitD_F)
micro$cavitD_F<-ifelse(micro$vitaminas==1 & micro$calcio_calcio_vitamina_d___4==1, 4, micro$cavitD_F)
table(micro$cavitD_F)


micro$cavitD_D<-micro$calcio_calcio_vitamina_d___5
micro$cavitD_D<-ifelse(micro$vitaminas==1 & micro$calcio_calcio_vitamina_d___6==1, 2, micro$cavitD_D)
table(micro$cavitD_D)
micro$cavitD_D<-ifelse(micro$vitaminas==1 & micro$calcio_calcio_vitamina_d___7==1, 3, micro$cavitD_D)
table(micro$cavitD_D)
micro$cavitD_D<-ifelse(micro$vitaminas==1 & micro$calcio_calcio_vitamina_d___8==1, 4, micro$cavitD_D)
table(micro$cavitD_D)
micro$cavitD_D<-ifelse(micro$vitaminas==1 & micro$calcio_calcio_vitamina_d___9==1, 5, micro$cavitD_D)
table(micro$cavitD_D)



micro$othervit_F<-micro$vit_otros_especificar___1
micro$othervit_F<-ifelse(micro$vitaminas==1 & micro$vit_otros_especificar___2==1, 2, micro$othervit_F)
table(micro$othervit_F)
micro$othervit_F<-ifelse(micro$vitaminas==1 & micro$vit_otros_especificar___3==1, 3, micro$othervit_F)
table(micro$othervit_F)
micro$othervit_F<-ifelse(micro$vitaminas==1 & micro$vit_otros_especificar___4==1, 4, micro$othervit_F)
table(micro$othervit_F)


micro$othervit_D<-micro$vit_otros_especificar___5
micro$othervit_D<-ifelse(micro$vitaminas==1 & micro$vit_otros_especificar___6==1, 2, micro$othervit_D)
table(micro$othervit_D)
micro$othervit_D<-ifelse(micro$vitaminas==1 & micro$vit_otros_especificar___7==1, 3, micro$othervit_D)
table(micro$othervit_D)
micro$othervit_D<-ifelse(micro$vitaminas==1 & micro$vit_otros_especificar___8==1, 4, micro$othervit_D)
table(micro$othervit_D)
micro$othervit_D<-ifelse(micro$vitaminas==1 & micro$vit_otros_especificar___9==1, 5, micro$othervit_D)
table(micro$othervit_D)


# probiotic yogurt

micro$pro_yog_F<-micro$yogur_activia_danone_otros___1
micro$pro_yog_F<-ifelse(micro$vitaminas==1 & micro$yogur_activia_danone_otros___2==1, 2, micro$pro_yog_F)
table(micro$pro_yog_F)
micro$pro_yog_F<-ifelse(micro$vitaminas==1 & micro$yogur_activia_danone_otros___3==1, 3, micro$pro_yog_F)
table(micro$pro_yog_F)
micro$pro_yog_F<-ifelse(micro$vitaminas==1 & micro$yogur_activia_danone_otros___4==1, 4, micro$pro_yog_F)
table(micro$pro_yog_F)


micro$pro_yog_D<-micro$yogur_activia_danone_otros___5
micro$pro_yog_D<-ifelse(micro$vitaminas==1 & micro$yogur_activia_danone_otros___6==1, 2, micro$pro_yog_D)
table(micro$pro_yog_D)
micro$pro_yog_D<-ifelse(micro$vitaminas==1 & micro$yogur_activia_danone_otros___7==1, 3, micro$pro_yog_D)
table(micro$pro_yog_D)
micro$pro_yog_D<-ifelse(micro$vitaminas==1 & micro$yogur_activia_danone_otros___8==1, 4, micro$pro_yog_D)
table(micro$pro_yog_D)
micro$pro_yog_D<-ifelse(micro$vitaminas==1 & micro$yogur_activia_danone_otros___9==1, 5, micro$pro_yog_D)
table(micro$pro_yog_D)



micro$pro_actimel_F<-micro$bebidas_actimel_danone_otr___1
micro$pro_actimel_F<-ifelse(micro$vitaminas==1 & micro$bebidas_actimel_danone_otr___2==1, 2, micro$pro_actimel_F)
table(micro$pro_actimel_F)
micro$pro_actimel_F<-ifelse(micro$vitaminas==1 & micro$bebidas_actimel_danone_otr___3==1, 3, micro$pro_actimel_F)
table(micro$pro_actimel_F)
micro$pro_actimel_F<-ifelse(micro$vitaminas==1 & micro$bebidas_actimel_danone_otr___4==1, 4, micro$pro_actimel_F)
table(micro$pro_actimel_F)


micro$pro_actimel_D<-micro$bebidas_actimel_danone_otr___5
micro$pro_actimel_D<-ifelse(micro$vitaminas==1 & micro$bebidas_actimel_danone_otr___6==1, 2, micro$pro_actimel_D)
table(micro$pro_actimel_D)
micro$pro_actimel_D<-ifelse(micro$vitaminas==1 & micro$bebidas_actimel_danone_otr___7==1, 3, micro$pro_actimel_D)
table(micro$pro_actimel_D)
micro$pro_actimel_D<-ifelse(micro$vitaminas==1 & micro$bebidas_actimel_danone_otr___8==1, 4, micro$pro_actimel_D)
table(micro$pro_actimel_D)
micro$pro_actimel_D<-ifelse(micro$vitaminas==1 & micro$bebidas_actimel_danone_otr___9==1, 5, micro$pro_actimel_D)
table(micro$pro_actimel_D)




micro$art_sweet_F<-micro$edulcorantes_artificiales___1
micro$art_sweet_F<-ifelse(micro$vitaminas==1 & micro$edulcorantes_artificiales___2==1, 2, micro$art_sweet_F)
table(micro$art_sweet_F)
micro$art_sweet_F<-ifelse(micro$vitaminas==1 & micro$edulcorantes_artificiales___3==1, 3, micro$art_sweet_F)
table(micro$art_sweet_F)
micro$art_sweet_F<-ifelse(micro$vitaminas==1 & micro$edulcorantes_artificiales___4==1, 4, micro$art_sweet_F)
table(micro$art_sweet_F)

micro$art_sweet_D<-micro$edulcorantes_artificiales___5
micro$art_sweet_D<-ifelse(micro$vitaminas==1 & micro$edulcorantes_artificiales___6==1, 2, micro$art_sweet_D)
table(micro$art_sweet_D)
micro$art_sweet_D<-ifelse(micro$vitaminas==1 & micro$edulcorantes_artificiales___7==1, 3, micro$art_sweet_D)
table(micro$art_sweet_D)
micro$art_sweet_D<-ifelse(micro$vitaminas==1 & micro$edulcorantes_artificiales___8==1, 4, micro$art_sweet_D)
table(micro$art_sweet_D)
micro$art_sweet_D<-ifelse(micro$vitaminas==1 & micro$edulcorantes_artificiales___9==1, 5, micro$art_sweet_D)
table(micro$art_sweet_D)



micro$fiber_F<-micro$fibra_o_suplementos_metamu___1
micro$fiber_F<-ifelse(micro$vitaminas==1 & micro$fibra_o_suplementos_metamu___2==1, 2, micro$fiber_F)
table(micro$fiber_F)
micro$fiber_F<-ifelse(micro$vitaminas==1 & micro$fibra_o_suplementos_metamu___3==1, 3, micro$fiber_F)
table(micro$fiber_F)
micro$fiber_F<-ifelse(micro$vitaminas==1 & micro$fibra_o_suplementos_metamu___4==1, 4, micro$fiber_F)
table(micro$fiber_F)


micro$fiber_D<-micro$fibra_o_suplementos_metamu___5
micro$fiber_D<-ifelse(micro$vitaminas==1 & micro$fibra_o_suplementos_metamu___6==1, 2, micro$fiber_D)
table(micro$fiber_D)
micro$fiber_D<-ifelse(micro$vitaminas==1 & micro$fibra_o_suplementos_metamu___7==1, 3, micro$fiber_D)
table(micro$fiber_D)
micro$fiber_D<-ifelse(micro$vitaminas==1 & micro$fibra_o_suplementos_metamu___8==1, 4, micro$fiber_D)
table(micro$fiber_D)
micro$fiber_D<-ifelse(micro$vitaminas==1 & micro$fibra_o_suplementos_metamu___9==1, 5, micro$fiber_D)
table(micro$fiber_D)

# others pro other prebiotics not included since there are few respondents


names(micro)[names(micro) == "le_ha_ayudado_a_responder"] <- "M112.1.1.1"
names(micro)[names(micro) == "si_su_respuesta_es_s_quien"] <- "M113.1.1.1"
names(micro)[names(micro) == "d_nde_ha_contestado_el_cue"] <- "M114.1.1.1"
#names(micro)[names(micro) == "observaciones_del_entrevis"] <- "observations1"
#names(micro)[names(micro) == "la_cooperaci_n_de_la_pe"] <- "cooperation"
#names(micro)[names(micro) == "la_calidad_de_esta_entr"] <- "quality"
#names(micro)[names(micro) == "observaciones"] <- "observations2"



 # remove the LONG format supplement variables

suppl<-micro[,c(132:252)]

micro<-micro[,-c(132:252)]

suppl$subject<-micro$subject

suppl.clean<-micro[,c(151:172)]

# save the supplement data:
write.csv(suppl, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/Supplements_raw.csv")

write.csv(suppl.clean, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/Supplements.csv")


# save the foods in frequency format only

foodfq<-micro[,c(1:130)]

write.csv(foodfq, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodFreq.csv")


# rename the final questions



# save the whole data:

write.csv(micro, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/DietQes_microbiome_clean.csv")


## Information on water was not collected in the microbiome study
## Alcoholic beverages to be incorporated!

# ** for the alcoholic beverages variables, another set of variables was developed accounting for the most recent alcoholic beverage consumption. 
#See 24042017_WaterAlcoholicBeverages_improved(freq).csv
#Variables are here names as:
#  wine_2
#fortified_2
#spirtis_2
#beer_2

# The section on alcohol consumption in the microbiome study was adapted.

alcohol<- read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/Microbioma_DATA_2019-Alcohol.csv", header=TRUE)## Open the Alcohol questionnaire data

# keep only datos: arm 1 rows

alcohol<-alcohol[alcohol$redcap_event_name=="datos_arm_1",]

head(alcohol)

names(alcohol)[names(alcohol) == "record_id"] <- "subject"
alcohol$redcap_event_name<-NULL

names(alcohol)

# the most recent alcohol consumption by type will be considered, since the dietary data also refers to habitual intake in the previous two years
# also, this more recent alcohol consumption is more likely to exert an effect on the microbiome
# the microbiome questionnaire accounts for actual alcohol intake and for the intake in young adulthood (20-29 years)

# variables to be considered are: NUMBER OF DRINKS; FREQUENCY

#ii_cantidad_cerveza_ahora; ii_qu_cantidad_de_cerveza
#ii_cantidad_brandy_ahora; "ii_cantid_brandy_d_a_sem"
#ii_cantidad_vino_ahora; "ii_cantidad_vino_dia_ahora"
#cantidad_vermut_ahora; "de_vermut_ahora_dia"

# In variables of dia/semana, 1 means 1 per day (or 1); 2 means 1 per week (or 1/7), 3 means 1 per month (or 1/30), and 4 means 1 per year (or 1/365)
# Tranform everything in frequencies per day

alcohol$beer2<-ifelse(alcohol$ii_qu_cantidad_de_cerveza==1, alcohol$ii_cantidad_cerveza_ahora, NA)
alcohol$beer2<-ifelse(alcohol$ii_qu_cantidad_de_cerveza==2, alcohol$ii_cantidad_cerveza_ahora/7, alcohol$beer2)
alcohol$beer2<-ifelse(alcohol$ii_qu_cantidad_de_cerveza==3, alcohol$ii_cantidad_cerveza_ahora/30, alcohol$beer2)
alcohol$beer2<-ifelse(alcohol$ii_qu_cantidad_de_cerveza==4, alcohol$ii_cantidad_cerveza_ahora/365, alcohol$beer2)

alcohol$beer2


alcohol$wine2<-ifelse(alcohol$ii_cantidad_vino_dia_ahora==1, alcohol$ii_cantidad_vino_ahora, NA)
alcohol$wine2<-ifelse(alcohol$ii_cantidad_vino_dia_ahora==2, alcohol$ii_cantidad_vino_ahora/7, alcohol$wine2)
alcohol$wine2<-ifelse(alcohol$ii_cantidad_vino_dia_ahora==3, alcohol$ii_cantidad_vino_ahora/30, alcohol$wine2)
alcohol$wine2<-ifelse(alcohol$ii_cantidad_vino_dia_ahora==4, alcohol$ii_cantidad_vino_ahora/365, alcohol$wine2)

alcohol$wine2




alcohol$fortified2<-ifelse(alcohol$ii_cantid_brandy_d_a_sem==1, alcohol$ii_cantidad_brandy_ahora, NA)
alcohol$fortified2<-ifelse(alcohol$ii_cantid_brandy_d_a_sem==2, alcohol$ii_cantidad_brandy_ahora/7, alcohol$fortified2)
alcohol$fortified2<-ifelse(alcohol$ii_cantid_brandy_d_a_sem==3, alcohol$ii_cantidad_brandy_ahora/30, alcohol$fortified2)
alcohol$fortified2<-ifelse(alcohol$ii_cantid_brandy_d_a_sem==4, alcohol$ii_cantidad_brandy_ahora/365, alcohol$fortified2)

alcohol$fortified2



alcohol$sprits2<-ifelse(alcohol$de_vermut_ahora_dia==1, alcohol$cantidad_vermut_ahora, NA)
alcohol$sprits2<-ifelse(alcohol$de_vermut_ahora_dia==2, alcohol$cantidad_vermut_ahora/7, alcohol$sprits2)
alcohol$sprits2<-ifelse(alcohol$de_vermut_ahora_dia==3, alcohol$cantidad_vermut_ahora/30, alcohol$sprits2)
alcohol$sprits2<-ifelse(alcohol$de_vermut_ahora_dia==4, alcohol$cantidad_vermut_ahora/365, alcohol$sprits2)

alcohol$sprits2

alcohol.clean<-alcohol[,c(1,31:34)]

# save the alcohol variables:

write.csv(alcohol.clean, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/alcohol_clean.csv")

# merge with the microbiome data file

micro2<-merge(micro, alcohol.clean, by="subject")


# save the whole data:

write.csv(micro2, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/DietQes_microbiome_alcohol_clean.csv")


#Reference values to be considered for conversion later on: 
#  Vino: 1 vaso o copa (125mls)
#  Cerveza: 1 caña o quinto (200ml)
#  Vermut, Martini, O Porto, etc: 1 copa (50 ml)
#  Coñac, ginebra, ron, whisky, etc: 1 medida de bar o 25mls



