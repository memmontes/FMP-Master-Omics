
##########################################
#### FRECUENCY TRANSFORMATION Microbiome items
#### 12 July 2022
#### Esther Molina
###########################################

load("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/DietMicrobiome.RData")

## apply conversion factors to transform dietary information into frequency/day
dieta<-foodfq # dietary database with transformed names


# transform all into factor variables

dieta<-lapply(dieta[,c(2:130)], as.factor)
dieta<-as.data.frame(dieta)
dieta$subject<-foodfq$subject
#dieta<-dieta[]

# first check the data

library(compareGroups)
library(kableExtra)

res<-compareGroups(~., data=dieta, ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab
export2md(restab, strip = TRUE, first.strip = TRUE) 

# there are 4 missings in almost all variables:

miss<-dieta[is.na(dieta$fre8100),]


## Dairy

dieta$fre8100 <- with(dieta, ((dieta$fre8100==1) *0.00 + (dieta$fre8100==2)*0.07 +(dieta$fre8100==3)*0.14 + (dieta$fre8100==4)*0.43 + (dieta$fre8100==5)*0.79 + (dieta$fre8100==6)*1.00 + (dieta$fre8100==7)*2.5 + (dieta$fre8100==8)*4.5 + (dieta$fre8100==9)*6.5))

dieta$fre8200 <- with(dieta, ((dieta$fre8200==1) *0.00 + (dieta$fre8200==2)*0.07 +(dieta$fre8200==3)*0.14 + (dieta$fre8200==4)*0.43 + (dieta$fre8200==5)*0.79 + (dieta$fre8200==6)*1.00 + (dieta$fre8200==7)*2.5 + (dieta$fre8200==8)*4.5 + (dieta$fre8200==9)*6.5))

dieta$fre8600 <- with(dieta, ((dieta$fre8600==1) *0.00 + (dieta$fre8600==2)*0.07 +(dieta$fre8600==3)*0.14 + (dieta$fre8600==4)*0.43 + (dieta$fre8600==5)*0.79 + (dieta$fre8600==6)*1.00 + (dieta$fre8600==7)*2.5 + (dieta$fre8600==8)*4.5 + (dieta$fre8600==9)*6.5))

dieta$fre8500 <- with(dieta, ((dieta$fre8500==1) *0.00 + (dieta$fre8500==2)*0.07 +(dieta$fre8500==3)*0.14 + (dieta$fre8500==4)*0.43 + (dieta$fre8500==5)*0.79 + (dieta$fre8500==6)*1.00 + (dieta$fre8500==7)*2.5 + (dieta$fre8500==8)*4.5 + (dieta$fre8500==9)*6.5))

dieta$fre8800 <- with(dieta, ((dieta$fre8800==1) *0.00 + (dieta$fre8800==2)*0.07 +(dieta$fre8800==3)*0.14 + (dieta$fre8800==4)*0.43 + (dieta$fre8800==5)*0.79 + (dieta$fre8800==6)*1.00 + (dieta$fre8800==7)*2.5 + (dieta$fre8800==8)*4.5 + (dieta$fre8800==9)*6.5))

dieta$fre9000 <- with(dieta, ((dieta$fre9000==1) *0.00 + (dieta$fre9000==2)*0.07 +(dieta$fre9000==3)*0.14 + (dieta$fre9000==4)*0.43 + (dieta$fre9000==5)*0.79 + (dieta$fre9000==6)*1.00 + (dieta$fre9000==7)*2.5 + (dieta$fre9000==8)*4.5 + (dieta$fre9000==9)*6.5))

dieta$fre9200 <- with(dieta, ((dieta$fre9200==1) *0.00 + (dieta$fre9200==2)*0.07 +(dieta$fre9200==3)*0.14 + (dieta$fre9200==4)*0.43 + (dieta$fre9200==5)*0.79 + (dieta$fre9200==6)*1.00 + (dieta$fre9200==7)*2.5 + (dieta$fre9200==8)*4.5 + (dieta$fre9200==9)*6.5))

dieta$fre9400 <- with(dieta, ((dieta$fre9400==1) *0.00 + (dieta$fre9400==2)*0.07 +(dieta$fre9400==3)*0.14 + (dieta$fre9400==4)*0.43 + (dieta$fre9400==5)*0.79 + (dieta$fre9400==6)*1.00 + (dieta$fre9400==7)*2.5 + (dieta$fre9400==8)*4.5 + (dieta$fre9400==9)*6.5))


## Fowl and eggs

dieta$fre300 <- with(dieta, ((dieta$fre300==1) *0.00 + (dieta$fre300==2)*0.07 +(dieta$fre300==3)*0.14 + (dieta$fre300==4)*0.43 + (dieta$fre300==5)*0.79 + (dieta$fre300==6)*1.00 + (dieta$fre300==7)*2.5 + (dieta$fre300==8)*4.5 + (dieta$fre300==9)*6.5))

dieta$fre_chi_tot <- with(dieta, ((dieta$fre_chi_tot==1) *0.00 + (dieta$fre_chi_tot==2)*0.07 +(dieta$fre_chi_tot==3)*0.14 + (dieta$fre_chi_tot==4)*0.43 + (dieta$fre_chi_tot==5)*0.79 + (dieta$fre_chi_tot==6)*1.00 + (dieta$fre_chi_tot==7)*2.5 + (dieta$fre_chi_tot==8)*4.5 + (dieta$fre_chi_tot==9)*6.5))

dieta$fre100 <- with(dieta, ((dieta$fre100==1) *0.00 + (dieta$fre100==2)*0.25 +(dieta$fre100==3)*0.50 + (dieta$fre100==4)*0.75 + (dieta$fre100==5)*1.00))

dieta$fre130 <- with(dieta, ((dieta$fre130==1) *0.00 + (dieta$fre130==2)*0.25 +(dieta$fre130==3)*0.50 + (dieta$fre130==4)*0.75 + (dieta$fre130==5)*1.00))

dieta$fre140 <- with(dieta, ((dieta$fre140==1) *0.00 + (dieta$fre140==2)*0.25 +(dieta$fre140==3)*0.50 + (dieta$fre140==4)*0.75 + (dieta$fre140==5)*1.00))

dieta$fre110 <- with(dieta, ((dieta$fre110==1) *0.00 + (dieta$fre110==2)*0.25 +(dieta$fre110==3)*0.50 + (dieta$fre110==4)*0.75 + (dieta$fre110==5)*1.00))

dieta$fre150 <- with(dieta, ((dieta$fre150==1) *0.00 + (dieta$fre150==2)*0.25 +(dieta$fre150==3)*0.50 + (dieta$fre150==4)*0.75 + (dieta$fre150==5)*1.00))

dieta$fre160 <- with(dieta, ((dieta$fre160==1) *0.00 + (dieta$fre160==2)*0.25 +(dieta$fre160==3)*0.50 + (dieta$fre160==4)*0.75 + (dieta$fre160==5)*1.00))

## Beef

dieta$fre_vea_tot <- with(dieta, ((dieta$fre_vea_tot==1) *0.00 + (dieta$fre_vea_tot==2)*0.07 +(dieta$fre_vea_tot==3)*0.14 + (dieta$fre_vea_tot==4)*0.43 + (dieta$fre_vea_tot==5)*0.79 + (dieta$fre_vea_tot==6)*1.00 + (dieta$fre_vea_tot==7)*2.5 + (dieta$fre_vea_tot==8)*4.5 + (dieta$fre_vea_tot==9)*6.5))

dieta$fre500 <- with(dieta, ((dieta$fre500==1) *0.00 + (dieta$fre500==2)*0.25 +(dieta$fre500==3)*0.50 + (dieta$fre500==4)*0.75 + (dieta$fre500==5)*1.00))

dieta$fre520 <- with(dieta, ((dieta$fre520==1) *0.00 + (dieta$fre520==2)*0.25 +(dieta$fre520==3)*0.50 + (dieta$fre520==4)*0.75 + (dieta$fre520==5)*1.00))

dieta$fre530 <- with(dieta, ((dieta$fre530==1) *0.00 + (dieta$fre530==2)*0.25 +(dieta$fre530==3)*0.50 + (dieta$fre530==4)*0.75 + (dieta$fre530==5)*1.00))

dieta$fre540 <- with(dieta, ((dieta$fre540==1) *0.00 + (dieta$fre540==2)*0.25 +(dieta$fre540==3)*0.50 + (dieta$fre540==4)*0.75 + (dieta$fre540==5)*1.00))

dieta$fre550 <- with(dieta, ((dieta$fre550==1) *0.00 + (dieta$fre550==2)*0.25 +(dieta$fre550==3)*0.50 + (dieta$fre550==4)*0.75 + (dieta$fre550==5)*1.00))

dieta$fre560 <- with(dieta, ((dieta$fre560==1) *0.00 + (dieta$fre560==2)*0.25 +(dieta$fre560==3)*0.50 + (dieta$fre560==4)*0.75 + (dieta$fre560==5)*1.00))

## Pork

dieta$fre_por_tot <- with(dieta, ((dieta$fre_por_tot==1) *0.00 + (dieta$fre_por_tot==2)*0.07 +(dieta$fre_por_tot==3)*0.14 + (dieta$fre_por_tot==4)*0.43 + (dieta$fre_por_tot==5)*0.79 + (dieta$fre_por_tot==6)*1.00 + (dieta$fre_por_tot==7)*2.5 + (dieta$fre_por_tot==8)*4.5 + (dieta$fre_por_tot==9)*6.5))

dieta$fre400 <- with(dieta, ((dieta$fre400==1) *0.00 + (dieta$fre400==2)*0.25 +(dieta$fre400==3)*0.50 + (dieta$fre400==4)*0.75 + (dieta$fre400==5)*1.00))

dieta$fre440 <- with(dieta, ((dieta$fre440==1) *0.00 + (dieta$fre440==2)*0.25 +(dieta$fre440==3)*0.50 + (dieta$fre440==4)*0.75 + (dieta$fre440==5)*1.00))

dieta$fre420 <- with(dieta, ((dieta$fre420==1) *0.00 + (dieta$fre420==2)*0.25 +(dieta$fre420==3)*0.50 + (dieta$fre420==4)*0.75 + (dieta$fre420==5)*1.00))

dieta$fre410 <- with(dieta, ((dieta$fre410==1) *0.00 + (dieta$fre410==2)*0.25 +(dieta$fre410==3)*0.50 + (dieta$fre410==4)*0.75 + (dieta$fre410==5)*1.00))

dieta$fre430 <- with(dieta, ((dieta$fre430==1) *0.00 + (dieta$fre430==2)*0.25 +(dieta$fre430==3)*0.50 + (dieta$fre430==4)*0.75 + (dieta$fre430==5)*1.00))

dieta$fre460 <- with(dieta, ((dieta$fre460==1) *0.00 + (dieta$fre460==2)*0.25 +(dieta$fre460==3)*0.50 + (dieta$fre460==4)*0.75 + (dieta$fre460==5)*1.00))

## Lamb and goat

dieta$fre_lam_tot <- with(dieta, ((dieta$fre_lam_tot==1) *0.00 + (dieta$fre_lam_tot==2)*0.07 +(dieta$fre_lam_tot==3)*0.14 + (dieta$fre_lam_tot==4)*0.43 + (dieta$fre_lam_tot==5)*0.79 + (dieta$fre_lam_tot==6)*1.00 + (dieta$fre_lam_tot==7)*2.5 + (dieta$fre_lam_tot==8)*4.5 + (dieta$fre_lam_tot==9)*6.5))

dieta$fre1100 <- with(dieta, ((dieta$fre1100==1) *0.00 + (dieta$fre1100==2)*0.25 +(dieta$fre1100==3)*0.50 + (dieta$fre1100==4)*0.75 + (dieta$fre1100==5)*1.00))

dieta$fre1110 <- with(dieta, ((dieta$fre1110==1) *0.00 + (dieta$fre1110==2)*0.25 +(dieta$fre1110==3)*0.50 + (dieta$fre1110==4)*0.75 + (dieta$fre1110==5)*1.00))

dieta$fre1130 <- with(dieta, ((dieta$fre1130==1) *0.00 + (dieta$fre1130==2)*0.25 +(dieta$fre1130==3)*0.50 + (dieta$fre1130==4)*0.75 + (dieta$fre1130==5)*1.00))

dieta$fre1140 <- with(dieta, ((dieta$fre1140==1) *0.00 + (dieta$fre1140==2)*0.25 +(dieta$fre1140==3)*0.50 + (dieta$fre1140==4)*0.75 + (dieta$fre1140==5)*1.00))

dieta$fre1150 <- with(dieta, ((dieta$fre1150==1) *0.00 + (dieta$fre1150==2)*0.25 +(dieta$fre1150==3)*0.50 + (dieta$fre1150==4)*0.75 + (dieta$fre1150==5)*1.00))

dieta$fre1160 <- with(dieta, ((dieta$fre1160==1) *0.00 + (dieta$fre1160==2)*0.25 +(dieta$fre1160==3)*0.50 + (dieta$fre1160==4)*0.75 + (dieta$fre1160==5)*1.00))

## Other kind of meat

dieta$fre1200 <- with(dieta, ((dieta$fre1200==1) *0.00 + (dieta$fre1200==2)*0.25 +(dieta$fre1200==3)*0.50 + (dieta$fre1200==4)*0.75 + (dieta$fre1200==5)*1.00))

dieta$fre1300 <- with(dieta, ((dieta$fre1300==1) *0.00 + (dieta$fre1300==2)*0.25 +(dieta$fre1300==3)*0.50 + (dieta$fre1300==4)*0.75 + (dieta$fre1300==5)*1.00))

dieta$fre1400 <- with(dieta, ((dieta$fre1400==1) *0.00 + (dieta$fre1400==2)*0.25 +(dieta$fre1400==3)*0.50 + (dieta$fre1400==4)*0.75 + (dieta$fre1400==5)*1.00))

dieta$fre1500 <- with(dieta, ((dieta$fre1500==1) *0.00 + (dieta$fre1500==2)*0.25 +(dieta$fre1500==3)*0.50 + (dieta$fre1500==4)*0.75 + (dieta$fre1500==5)*1.00))

## Pescado

dieta$fre_wfish_tot <- with(dieta, ((dieta$fre_wfish_tot==1) *0.00 + (dieta$fre_wfish_tot==2)*0.07 +(dieta$fre_wfish_tot==3)*0.14 + (dieta$fre_wfish_tot==4)*0.43 + (dieta$fre_wfish_tot==5)*0.79 + (dieta$fre_wfish_tot==6)*1.00 + (dieta$fre_wfish_tot==7)*2.5 + (dieta$fre_wfish_tot==8)*4.5 + (dieta$fre_wfish_tot==9)*6.5))

dieta$fre_bfish_tot <- with(dieta, ((dieta$fre_bfish_tot==1) *0.00 + (dieta$fre_bfish_tot==2)*0.07 +(dieta$fre_bfish_tot==3)*0.14 + (dieta$fre_bfish_tot==4)*0.43 + (dieta$fre_bfish_tot==5)*0.79 + (dieta$fre_bfish_tot==6)*1.00 + (dieta$fre_bfish_tot==7)*2.5 + (dieta$fre_bfish_tot==8)*4.5 + (dieta$fre_bfish_tot==9)*6.5))

dieta$fre1600 <- with(dieta, ((dieta$fre1600==1) *0.00 + (dieta$fre1600==2)*0.25 +(dieta$fre1600==3)*0.50 + (dieta$fre1600==4)*0.75 + (dieta$fre1600==5)*1.00))

dieta$fre1610 <- with(dieta, ((dieta$fre1610==1) *0.00 + (dieta$fre1610==2)*0.25 +(dieta$fre1610==3)*0.50 + (dieta$fre1610==4)*0.75 + (dieta$fre1610==5)*1.00))

dieta$fre1630 <- with(dieta, ((dieta$fre1630==1) *0.00 + (dieta$fre1630==2)*0.25 +(dieta$fre1630==3)*0.50 + (dieta$fre1630==4)*0.75 + (dieta$fre1630==5)*1.00))

dieta$fre1640 <- with(dieta, ((dieta$fre1640==1) *0.00 + (dieta$fre1640==2)*0.25 +(dieta$fre1640==3)*0.50 + (dieta$fre1640==4)*0.75 + (dieta$fre1640==5)*1.00))

dieta$fre1650 <- with(dieta, ((dieta$fre1650==1) *0.00 + (dieta$fre1650==2)*0.25 +(dieta$fre1650==3)*0.50 + (dieta$fre1650==4)*0.75 + (dieta$fre1650==5)*1.00))

dieta$fre1660 <- with(dieta, ((dieta$fre1660==1) *0.00 + (dieta$fre1660==2)*0.25 +(dieta$fre1660==3)*0.50 + (dieta$fre1660==4)*0.75 + (dieta$fre1660==5)*1.00))

dieta$fre3100 <- with(dieta, ((dieta$fre3100==1) *0.00 + (dieta$fre3100==2)*0.07 +(dieta$fre3100==3)*0.14 + (dieta$fre3100==4)*0.43 + (dieta$fre3100==5)*0.79 + (dieta$fre3100==6)*1.00 + (dieta$fre3100==7)*2.5 + (dieta$fre3100==8)*4.5 + (dieta$fre3100==9)*6.5))

dieta$fre3200 <- with(dieta, ((dieta$fre3200==1) *0.00 + (dieta$fre3200==2)*0.07 +(dieta$fre3200==3)*0.14 + (dieta$fre3200==4)*0.43 + (dieta$fre3200==5)*0.79 + (dieta$fre3200==6)*1.00 + (dieta$fre3200==7)*2.5 + (dieta$fre3200==8)*4.5 + (dieta$fre3200==9)*6.5))

dieta$fre1800 <- with(dieta, ((dieta$fre1800==1) *0.00 + (dieta$fre1800==2)*0.07 +(dieta$fre1800==3)*0.14 + (dieta$fre1800==4)*0.43 + (dieta$fre1800==5)*0.79 + (dieta$fre1800==6)*1.00 + (dieta$fre1800==7)*2.5 + (dieta$fre1800==8)*4.5 + (dieta$fre1800==9)*6.5))

## Ready-to-eat dishes

dieta$fre600 <- with(dieta, ((dieta$fre600==1) *0.00 + (dieta$fre600==2)*0.07 +(dieta$fre600==3)*0.14 + (dieta$fre600==4)*0.43 + (dieta$fre600==5)*0.79 + (dieta$fre600==6)*1.00 + (dieta$fre600==7)*2.5 + (dieta$fre600==8)*4.5 + (dieta$fre600==9)*6.5))

dieta$fre700 <- with(dieta, ((dieta$fre700==1) *0.00 + (dieta$fre700==2)*0.07 +(dieta$fre700==3)*0.14 + (dieta$fre700==4)*0.43 + (dieta$fre700==5)*0.79 + (dieta$fre700==6)*1.00 + (dieta$fre700==7)*2.5 + (dieta$fre700==8)*4.5 + (dieta$fre700==9)*6.5))

dieta$fre2500 <- with(dieta, ((dieta$fre2500==1) *0.00 + (dieta$fre2500==2)*0.07 +(dieta$fre2500==3)*0.14 + (dieta$fre2500==4)*0.43 + (dieta$fre2500==5)*0.79 + (dieta$fre2500==6)*1.00 + (dieta$fre2500==7)*2.5 + (dieta$fre2500==8)*4.5 + (dieta$fre2500==9)*6.5))

dieta$fre2100 <- with(dieta, ((dieta$fre2100==1) *0.00 + (dieta$fre2100==2)*0.07 +(dieta$fre2100==3)*0.14 + (dieta$fre2100==4)*0.43 + (dieta$fre2100==5)*0.79 + (dieta$fre2100==6)*1.00 + (dieta$fre2100==7)*2.5 + (dieta$fre2100==8)*4.5 + (dieta$fre2100==9)*6.5))

dieta$fre2200 <- with(dieta, ((dieta$fre2200==1) *0.00 + (dieta$fre2200==2)*0.07 +(dieta$fre2200==3)*0.14 + (dieta$fre2200==4)*0.43 + (dieta$fre2200==5)*0.79 + (dieta$fre2200==6)*1.00 + (dieta$fre2200==7)*2.5 + (dieta$fre2200==8)*4.5 + (dieta$fre2200==9)*6.5))

dieta$fre2300 <- with(dieta, ((dieta$fre2300==1) *0.00 + (dieta$fre2300==2)*0.07 +(dieta$fre2300==3)*0.14 + (dieta$fre2300==4)*0.43 + (dieta$fre2300==5)*0.79 + (dieta$fre2300==6)*1.00 + (dieta$fre2300==7)*2.5 + (dieta$fre2300==8)*4.5 + (dieta$fre2300==9)*6.5))

dieta$fre2700 <- with(dieta, ((dieta$fre2700==1) *0.00 + (dieta$fre2700==2)*0.07 +(dieta$fre2700==3)*0.14 + (dieta$fre2700==4)*0.43 + (dieta$fre2700==5)*0.79 + (dieta$fre2700==6)*1.00 + (dieta$fre2700==7)*2.5 + (dieta$fre2700==8)*4.5 + (dieta$fre2700==9)*6.5))

dieta$fre2800 <- with(dieta, ((dieta$fre2800==1) *0.00 + (dieta$fre2800==2)*0.07 +(dieta$fre2800==3)*0.14 + (dieta$fre2800==4)*0.43 + (dieta$fre2800==5)*0.79 + (dieta$fre2800==6)*1.00 + (dieta$fre2800==7)*2.5 + (dieta$fre2800==8)*4.5 + (dieta$fre2800==9)*6.5))

dieta$fre_c_2900 <- with(dieta, ((dieta$fre_c_2900==1) *0.00 + (dieta$fre_c_2900==2)*0.07 +(dieta$fre_c_2900==3)*0.14 + (dieta$fre_c_2900==4)*0.43 + (dieta$fre_c_2900==5)*0.79 + (dieta$fre_c_2900==6)*1.00 + (dieta$fre_c_2900==7)*2.5 + (dieta$fre_c_2900==8)*4.5 + (dieta$fre_c_2900==9)*6.5))

## Vegetables and legumes

dieta$fre3500 <- with(dieta, ((dieta$fre3500==1) *0.00 + (dieta$fre3500==2)*0.07 +(dieta$fre3500==3)*0.14 + (dieta$fre3500==4)*0.43 + (dieta$fre3500==5)*0.79 + (dieta$fre3500==6)*1.00 + (dieta$fre3500==7)*2.5 + (dieta$fre3500==8)*4.5 + (dieta$fre3500==9)*6.5))

dieta$fre4500 <- with(dieta, ((dieta$fre4500==1) *0.00 + (dieta$fre4500==2)*0.07 +(dieta$fre4500==3)*0.14 + (dieta$fre4500==4)*0.43 + (dieta$fre4500==5)*0.79 + (dieta$fre4500==6)*1.00 + (dieta$fre4500==7)*2.5 + (dieta$fre4500==8)*4.5 + (dieta$fre4500==9)*6.5))

dieta$fre3400 <- with(dieta, ((dieta$fre3400==1) *0.00 + (dieta$fre3400==2)*0.07 +(dieta$fre3400==3)*0.14 + (dieta$fre3400==4)*0.43 + (dieta$fre3400==5)*0.79 + (dieta$fre3400==6)*1.00 + (dieta$fre3400==7)*2.5 + (dieta$fre3400==8)*4.5 + (dieta$fre3400==9)*6.5))

dieta$fre3600 <- with(dieta, ((dieta$fre3600==1) *0.00 + (dieta$fre3600==2)*0.07 +(dieta$fre3600==3)*0.14 + (dieta$fre3600==4)*0.43 + (dieta$fre3600==5)*0.79 + (dieta$fre3600==6)*1.00 + (dieta$fre3600==7)*2.5 + (dieta$fre3600==8)*4.5 + (dieta$fre3600==9)*6.5))

dieta$fre3800 <- with(dieta, ((dieta$fre3800==1) *0.00 + (dieta$fre3800==2)*0.07 +(dieta$fre3800==3)*0.14 + (dieta$fre3800==4)*0.43 + (dieta$fre3800==5)*0.79 + (dieta$fre3800==6)*1.00 + (dieta$fre3800==7)*2.5 + (dieta$fre3800==8)*4.5 + (dieta$fre3800==9)*6.5))

dieta$fre4200 <- with(dieta, ((dieta$fre4200==1) *0.00 + (dieta$fre4200==2)*0.07 +(dieta$fre4200==3)*0.14 + (dieta$fre4200==4)*0.43 + (dieta$fre4200==5)*0.79 + (dieta$fre4200==6)*1.00 + (dieta$fre4200==7)*2.5 + (dieta$fre4200==8)*4.5 + (dieta$fre4200==9)*6.5))

dieta$fre4300 <- with(dieta, ((dieta$fre4300==1) *0.00 + (dieta$fre4300==2)*0.07 +(dieta$fre4300==3)*0.14 + (dieta$fre4300==4)*0.43 + (dieta$fre4300==5)*0.79 + (dieta$fre4300==6)*1.00 + (dieta$fre4300==7)*2.5 + (dieta$fre4300==8)*4.5 + (dieta$fre4300==9)*6.5))

dieta$fre5000 <- with(dieta, ((dieta$fre5000==1) *0.00 + (dieta$fre5000==2)*0.07 +(dieta$fre5000==3)*0.14 + (dieta$fre5000==4)*0.43 + (dieta$fre5000==5)*0.79 + (dieta$fre5000==6)*1.00 + (dieta$fre5000==7)*2.5 + (dieta$fre5000==8)*4.5 + (dieta$fre5000==9)*6.5))

dieta$fre3700 <- with(dieta, ((dieta$fre3700==1) *0.00 + (dieta$fre3700==2)*0.07 +(dieta$fre3700==3)*0.14 + (dieta$fre3700==4)*0.43 + (dieta$fre3700==5)*0.79 + (dieta$fre3700==6)*1.00 + (dieta$fre3700==7)*2.5 + (dieta$fre3700==8)*4.5 + (dieta$fre3700==9)*6.5))

dieta$fre4700 <- with(dieta, ((dieta$fre4700==1) *0.00 + (dieta$fre4700==2)*0.07 +(dieta$fre4700==3)*0.14 + (dieta$fre4700==4)*0.43 + (dieta$fre4700==5)*0.79 + (dieta$fre4700==6)*1.00 + (dieta$fre4700==7)*2.5 + (dieta$fre4700==8)*4.5 + (dieta$fre4700==9)*6.5))

dieta$fre5200 <- with(dieta, ((dieta$fre5200==1) *0.00 + (dieta$fre5200==2)*0.07 +(dieta$fre5200==3)*0.14 + (dieta$fre5200==4)*0.43 + (dieta$fre5200==5)*0.79 + (dieta$fre5200==6)*1.00 + (dieta$fre5200==7)*2.5 + (dieta$fre5200==8)*4.5 + (dieta$fre5200==9)*6.5))

dieta$fre4100 <- with(dieta, ((dieta$fre4100==1) *0.00 + (dieta$fre4100==2)*0.07 +(dieta$fre4100==3)*0.14 + (dieta$fre4100==4)*0.43 + (dieta$fre4100==5)*0.79 + (dieta$fre4100==6)*1.00 + (dieta$fre4100==7)*2.5 + (dieta$fre4100==8)*4.5 + (dieta$fre4100==9)*6.5))

dieta$fre5100 <- with(dieta, ((dieta$fre5100==1) *0.00 + (dieta$fre5100==2)*0.07 +(dieta$fre5100==3)*0.14 + (dieta$fre5100==4)*0.43 + (dieta$fre5100==5)*0.79 + (dieta$fre5100==6)*1.00 + (dieta$fre5100==7)*2.5 + (dieta$fre5100==8)*4.5 + (dieta$fre5100==9)*6.5))

dieta$fre5810 <- with(dieta, ((dieta$fre5810==1) *0.00 + (dieta$fre5810==2)*0.07 +(dieta$fre5810==3)*0.14 + (dieta$fre5810==4)*0.43 + (dieta$fre5810==5)*0.79 + (dieta$fre5810==6)*1.00 + (dieta$fre5810==7)*2.5 + (dieta$fre5810==8)*4.5 + (dieta$fre5810==9)*6.5))



## Fruits

dieta$fre6200 <- with(dieta, ((dieta$fre6200==1) *0.00 + (dieta$fre6200==2)*0.07 +(dieta$fre6200==3)*0.14 + (dieta$fre6200==4)*0.43 + (dieta$fre6200==5)*0.79 + (dieta$fre6200==6)*1.00 + (dieta$fre6200==7)*2.5 + (dieta$fre6200==8)*4.5 + (dieta$fre6200==9)*6.5))

dieta$fre6300 <- with(dieta, ((dieta$fre6300==1) *0.00 + (dieta$fre6300==2)*0.07 +(dieta$fre6300==3)*0.14 + (dieta$fre6300==4)*0.43 + (dieta$fre6300==5)*0.79 + (dieta$fre6300==6)*1.00 + (dieta$fre6300==7)*2.5 + (dieta$fre6300==8)*4.5 + (dieta$fre6300==9)*6.5))

dieta$fre6400 <- with(dieta, ((dieta$fre6400==1) *0.00 + (dieta$fre6400==2)*0.07 +(dieta$fre6400==3)*0.14 + (dieta$fre6400==4)*0.43 + (dieta$fre6400==5)*0.79 + (dieta$fre6400==6)*1.00 + (dieta$fre6400==7)*2.5 + (dieta$fre6400==8)*4.5 + (dieta$fre6400==9)*6.5))

dieta$fre6500 <- with(dieta, ((dieta$fre6500==1) *0.00 + (dieta$fre6500==2)*0.07 +(dieta$fre6500==3)*0.14 + (dieta$fre6500==4)*0.43 + (dieta$fre6500==5)*0.79 + (dieta$fre6500==6)*1.00 + (dieta$fre6500==7)*2.5 + (dieta$fre6500==8)*4.5 + (dieta$fre6500==9)*6.5))

dieta$fre7000 <- with(dieta, ((dieta$fre7000==1) *0.00 + (dieta$fre7000==2)*0.07 +(dieta$fre7000==3)*0.14 + (dieta$fre7000==4)*0.43 + (dieta$fre7000==5)*0.79 + (dieta$fre7000==6)*1.00 + (dieta$fre7000==7)*2.5 + (dieta$fre7000==8)*4.5 + (dieta$fre7000==9)*6.5))

dieta$fre7200 <- with(dieta, ((dieta$fre7200==1) *0.00 + (dieta$fre7200==2)*0.07 +(dieta$fre7200==3)*0.14 + (dieta$fre7200==4)*0.43 + (dieta$fre7200==5)*0.79 + (dieta$fre7200==6)*1.00 + (dieta$fre7200==7)*2.5 + (dieta$fre7200==8)*4.5 + (dieta$fre7200==9)*6.5))

dieta$fre6600 <- with(dieta, ((dieta$fre6600==1) *0.00 + (dieta$fre6600==2)*0.07 +(dieta$fre6600==3)*0.14 + (dieta$fre6600==4)*0.43 + (dieta$fre6600==5)*0.79 + (dieta$fre6600==6)*1.00 + (dieta$fre6600==7)*2.5 + (dieta$fre6600==8)*4.5 + (dieta$fre6600==9)*6.5))

dieta$fre7300 <- with(dieta, ((dieta$fre7300==1) *0.00 + (dieta$fre7300==2)*0.07 +(dieta$fre7300==3)*0.14 + (dieta$fre7300==4)*0.43 + (dieta$fre7300==5)*0.79 + (dieta$fre7300==6)*1.00 + (dieta$fre7300==7)*2.5 + (dieta$fre7300==8)*4.5 + (dieta$fre7300==9)*6.5))

dieta$fre6700 <- with(dieta, ((dieta$fre6700==1) *0.00 + (dieta$fre6700==2)*0.07 +(dieta$fre6700==3)*0.14 + (dieta$fre6700==4)*0.43 + (dieta$fre6700==5)*0.79 + (dieta$fre6700==6)*1.00 + (dieta$fre6700==7)*2.5 + (dieta$fre6700==8)*4.5 + (dieta$fre6700==9)*6.5))

dieta$fre7600 <- with(dieta, ((dieta$fre7600==1) *0.00 + (dieta$fre7600==2)*0.07 +(dieta$fre7600==3)*0.14 + (dieta$fre7600==4)*0.43 + (dieta$fre7600==5)*0.79 + (dieta$fre7600==6)*1.00 + (dieta$fre7600==7)*2.5 + (dieta$fre7600==8)*4.5 + (dieta$fre7600==9)*6.5))

dieta$fre7800 <- with(dieta, ((dieta$fre7800==1) *0.00 + (dieta$fre7800==2)*0.07 +(dieta$fre7800==3)*0.14 + (dieta$fre7800==4)*0.43 + (dieta$fre7800==5)*0.79 + (dieta$fre7800==6)*1.00 + (dieta$fre7800==7)*2.5 + (dieta$fre7800==8)*4.5 + (dieta$fre7800==9)*6.5))

dieta$fre7900 <- with(dieta, ((dieta$fre7900==1) *0.00 + (dieta$fre7900==2)*0.07 +(dieta$fre7900==3)*0.14 + (dieta$fre7900==4)*0.43 + (dieta$fre7900==5)*0.79 + (dieta$fre7900==6)*1.00 + (dieta$fre7900==7)*2.5 + (dieta$fre7900==8)*4.5 + (dieta$fre7900==9)*6.5))


## Bread, cereals and potatoes

dieta$fre9600 <- with(dieta, ((dieta$fre9600==1) *0.00 + (dieta$fre9600==2)*0.07 +(dieta$fre9600==3)*0.14 + (dieta$fre9600==4)*0.43 + (dieta$fre9600==5)*0.79 + (dieta$fre9600==6)*1.00 + (dieta$fre9600==7)*2.5 + (dieta$fre9600==8)*4.5 + (dieta$fre9600==9)*6.5))

dieta$fre9700 <- with(dieta, ((dieta$fre9700==1) *0.00 + (dieta$fre9700==2)*0.07 +(dieta$fre9700==3)*0.14 + (dieta$fre9700==4)*0.43 + (dieta$fre9700==5)*0.79 + (dieta$fre9700==6)*1.00 + (dieta$fre9700==7)*2.5 + (dieta$fre9700==8)*4.5 + (dieta$fre9700==9)*6.5))

dieta$fre9800 <- with(dieta, ((dieta$fre9800==1) *0.00 + (dieta$fre9800==2)*0.07 +(dieta$fre9800==3)*0.14 + (dieta$fre9800==4)*0.43 + (dieta$fre9800==5)*0.79 + (dieta$fre9800==6)*1.00 + (dieta$fre9800==7)*2.5 + (dieta$fre9800==8)*4.5 + (dieta$fre9800==9)*6.5))

dieta$fre10300 <- with(dieta, ((dieta$fre10300==1) *0.00 + (dieta$fre10300==2)*0.07 +(dieta$fre10300==3)*0.14 + (dieta$fre10300==4)*0.43 + (dieta$fre10300==5)*0.79 + (dieta$fre10300==6)*1.00 + (dieta$fre10300==7)*2.5 + (dieta$fre10300==8)*4.5 + (dieta$fre10300==9)*6.5))

dieta$fre10200 <- with(dieta, ((dieta$fre10200==1) *0.00 + (dieta$fre10200==2)*0.07 +(dieta$fre10200==3)*0.14 + (dieta$fre10200==4)*0.43 + (dieta$fre10200==5)*0.79 + (dieta$fre10200==6)*1.00 + (dieta$fre10200==7)*2.5 + (dieta$fre10200==8)*4.5 + (dieta$fre10200==9)*6.5))

dieta$fre10400 <- with(dieta, ((dieta$fre10400==1) *0.00 + (dieta$fre10400==2)*0.07 +(dieta$fre10400==3)*0.14 + (dieta$fre10400==4)*0.43 + (dieta$fre10400==5)*0.79 + (dieta$fre10400==6)*1.00 + (dieta$fre10400==7)*2.5 + (dieta$fre10400==8)*4.5 + (dieta$fre10400==9)*6.5))

dieta$fre10500 <- with(dieta, ((dieta$fre10500==1) *0.00 + (dieta$fre10500==2)*0.07 +(dieta$fre10500==3)*0.14 + (dieta$fre10500==4)*0.43 + (dieta$fre10500==5)*0.79 + (dieta$fre10500==6)*1.00 + (dieta$fre10500==7)*2.5 + (dieta$fre10500==8)*4.5 + (dieta$fre10500==9)*6.5))

dieta$fre11000 <- with(dieta, ((dieta$fre11000==1) *0.00 + (dieta$fre11000==2)*0.07 +(dieta$fre11000==3)*0.14 + (dieta$fre11000==4)*0.43 + (dieta$fre11000==5)*0.79 + (dieta$fre11000==6)*1.00 + (dieta$fre11000==7)*2.5 + (dieta$fre11000==8)*4.5 + (dieta$fre11000==9)*6.5))

## Oils 

dieta$fre11700 <- with(dieta, ((dieta$fre11700==1) *0.00 + (dieta$fre11700==2)*0.07 +(dieta$fre11700==3)*0.14 + (dieta$fre11700==4)*0.43 + (dieta$fre11700==5)*0.79 + (dieta$fre11700==6)*1.00 + (dieta$fre11700==7)*2.5 + (dieta$fre11700==8)*4.5 + (dieta$fre11700==9)*6.5))

dieta$fre11800 <- with(dieta, ((dieta$fre11800==1) *0.00 + (dieta$fre11800==2)*0.07 +(dieta$fre11800==3)*0.14 + (dieta$fre11800==4)*0.43 + (dieta$fre11800==5)*0.79 + (dieta$fre11800==6)*1.00 + (dieta$fre11800==7)*2.5 + (dieta$fre11800==8)*4.5 + (dieta$fre11800==9)*6.5))

## Sweets and others

dieta$fre12200 <- with(dieta, ((dieta$fre12200==1) *0.00 + (dieta$fre12200==2)*0.07 +(dieta$fre12200==3)*0.14 + (dieta$fre12200==4)*0.43 + (dieta$fre12200==5)*0.79 + (dieta$fre12200==6)*1.00 + (dieta$fre12200==7)*2.5 + (dieta$fre12200==8)*4.5 + (dieta$fre12200==9)*6.5))

dieta$fre12500 <- with(dieta, ((dieta$fre12500==1) *0.00 + (dieta$fre12500==2)*0.07 +(dieta$fre12500==3)*0.14 + (dieta$fre12500==4)*0.43 + (dieta$fre12500==5)*0.79 + (dieta$fre12500==6)*1.00 + (dieta$fre12500==7)*2.5 + (dieta$fre12500==8)*4.5 + (dieta$fre12500==9)*6.5))

dieta$fre12900 <- with(dieta, ((dieta$fre12900==1) *0.00 + (dieta$fre12900==2)*0.07 +(dieta$fre12900==3)*0.14 + (dieta$fre12900==4)*0.43 + (dieta$fre12900==5)*0.79 + (dieta$fre12900==6)*1.00 + (dieta$fre12900==7)*2.5 + (dieta$fre12900==8)*4.5 + (dieta$fre12900==9)*6.5))

dieta$fre13100 <- with(dieta, ((dieta$fre13100==1) *0.00 + (dieta$fre13100==2)*0.07 +(dieta$fre13100==3)*0.14 + (dieta$fre13100==4)*0.43 + (dieta$fre13100==5)*0.79 + (dieta$fre13100==6)*1.00 + (dieta$fre13100==7)*2.5 + (dieta$fre13100==8)*4.5 + (dieta$fre13100==9)*6.5))

dieta$fre13200 <- with(dieta, ((dieta$fre13200==1) *0.00 + (dieta$fre13200==2)*0.07 +(dieta$fre13200==3)*0.14 + (dieta$fre13200==4)*0.43 + (dieta$fre13200==5)*0.79 + (dieta$fre13200==6)*1.00 + (dieta$fre13200==7)*2.5 + (dieta$fre13200==8)*4.5 + (dieta$fre13200==9)*6.5))

dieta$fre11300 <- with(dieta, ((dieta$fre11300==1) *0.00 + (dieta$fre11300==2)*0.07 +(dieta$fre11300==3)*0.14 + (dieta$fre11300==4)*0.43 + (dieta$fre11300==5)*0.79 + (dieta$fre11300==6)*1.00 + (dieta$fre11300==7)*2.5 + (dieta$fre11300==8)*4.5 + (dieta$fre11300==9)*6.5))

dieta$fre11400 <- with(dieta, ((dieta$fre11400==1) *0.00 + (dieta$fre11400==2)*0.07 +(dieta$fre11400==3)*0.14 + (dieta$fre11400==4)*0.43 + (dieta$fre11400==5)*0.79 + (dieta$fre11400==6)*1.00 + (dieta$fre11400==7)*2.5 + (dieta$fre11400==8)*4.5 + (dieta$fre11400==9)*6.5))

dieta$fre11610 <- with(dieta, ((dieta$fre11610==1) *0.00 + (dieta$fre11610==2)*0.07 +(dieta$fre11610==3)*0.14 + (dieta$fre11610==4)*0.43 + (dieta$fre11610==5)*0.79 + (dieta$fre11610==6)*1.00 + (dieta$fre11610==7)*2.5 + (dieta$fre11610==8)*4.5 + (dieta$fre11610==9)*6.5))

dieta$fre11630 <- with(dieta, ((dieta$fre11630==1) *0.00 + (dieta$fre11630==2)*0.07 +(dieta$fre11630==3)*0.14 + (dieta$fre11630==4)*0.43 + (dieta$fre11630==5)*0.79 + (dieta$fre11630==6)*1.00 + (dieta$fre11630==7)*2.5 + (dieta$fre11630==8)*4.5 + (dieta$fre11630==9)*6.5))

dieta$fre13500 <- with(dieta, ((dieta$fre13500==1) *0.00 + (dieta$fre13500==2)*0.07 +(dieta$fre13500==3)*0.14 + (dieta$fre13500==4)*0.43 + (dieta$fre13500==5)*0.79 + (dieta$fre13500==6)*1.00 + (dieta$fre13500==7)*2.5 + (dieta$fre13500==8)*4.5 + (dieta$fre13500==9)*6.5))

## Non alcoholic beverages and juices

dieta$fre14300 <- with(dieta, ((dieta$fre14300==1) *0.00 + (dieta$fre14300==2)*0.07 +(dieta$fre14300==3)*0.14 + (dieta$fre14300==4)*0.43 + (dieta$fre14300==5)*0.79 + (dieta$fre14300==6)*1.00 + (dieta$fre14300==7)*2.5 + (dieta$fre14300==8)*4.5 + (dieta$fre14300==9)*6.5))

dieta$fre14400 <- with(dieta, ((dieta$fre14400==1) *0.00 + (dieta$fre14400==2)*0.07 +(dieta$fre14400==3)*0.14 + (dieta$fre14400==4)*0.43 + (dieta$fre14400==5)*0.79 + (dieta$fre14400==6)*1.00 + (dieta$fre14400==7)*2.5 + (dieta$fre14400==8)*4.5 + (dieta$fre14400==9)*6.5))

dieta$fre14500 <- with(dieta, ((dieta$fre14500==1) *0.00 + (dieta$fre14500==2)*0.07 +(dieta$fre14500==3)*0.14 + (dieta$fre14500==4)*0.43 + (dieta$fre14500==5)*0.79 + (dieta$fre14500==6)*1.00 + (dieta$fre14500==7)*2.5 + (dieta$fre14500==8)*4.5 + (dieta$fre14500==9)*6.5))

dieta$fre14700 <- with(dieta, ((dieta$fre14700==1) *0.00 + (dieta$fre14700==2)*0.07 +(dieta$fre14700==3)*0.14 + (dieta$fre14700==4)*0.43 + (dieta$fre14700==5)*0.79 + (dieta$fre14700==6)*1.00 + (dieta$fre14700==7)*2.5 + (dieta$fre14700==8)*4.5 + (dieta$fre14700==9)*6.5))

dieta$fre15201 <- with(dieta, ((dieta$fre15201==1) *0.00 + (dieta$fre15201==2)*0.07 +(dieta$fre15201==3)*0.14 + (dieta$fre15201==4)*0.43 + (dieta$fre15201==5)*0.79 + (dieta$fre15201==6)*1.00 + (dieta$fre15201==7)*2.5 + (dieta$fre15201==8)*4.5 + (dieta$fre15201==9)*6.5))


# tea and coffee within the microbiome study only

dieta$fre_tea <- with(dieta, ((dieta$fre_tea==1) *0.00 + (dieta$fre_tea==2)*0.07 +(dieta$fre_tea==3)*0.14 + (dieta$fre_tea==4)*0.43 + (dieta$fre_tea==5)*0.79 + (dieta$fre_tea==6)*1.00 + (dieta$fre_tea==7)*2.5 + (dieta$fre_tea==8)*4.5 + (dieta$fre_tea==9)*6.5))

dieta$fre_coffee <- with(dieta, ((dieta$fre_coffee==1) *0.00 + (dieta$fre_coffee==2)*0.07 +(dieta$fre_coffee==3)*0.14 + (dieta$fre_coffee==4)*0.43 + (dieta$fre_coffee==5)*0.79 + (dieta$fre_coffee==6)*1.00 + (dieta$fre_coffee==7)*2.5 + (dieta$fre_coffee==8)*4.5 + (dieta$fre_coffee==9)*6.5))

dieta$fre_decoffee <- with(dieta, ((dieta$fre_decoffee==1) *0.00 + (dieta$fre_decoffee==2)*0.07 +(dieta$fre_decoffee==3)*0.14 + (dieta$fre_decoffee==4)*0.43 + (dieta$fre_decoffee==5)*0.79 + (dieta$fre_decoffee==6)*1.00 + (dieta$fre_decoffee==7)*2.5 + (dieta$fre_decoffee==8)*4.5 + (dieta$fre_decoffee==9)*6.5))


# water is not included in the microbiome study

# for alcoholic beverages, we need to include beer2, wine2, ferm2, spritis2; this is already in frequency per day

dieta<-merge(dieta, alcohol.clean, by="subject")

# for these beverages we need to assign zero to those who reported that they do not consume currently these beverages

summary(dieta$beer2)

dieta$beer2[is.na(dieta$beer2)]<-0

dieta$sprits2[is.na(dieta$sprits2)]<-0

dieta$wine2[is.na(dieta$wine2)]<-0

dieta$fortified2[is.na(dieta$fortified2)]<-0

# culinary process variables:

culinary<-foodfq[,c("Q1oth", 
"Q2oth", "Q3oth", 
"Oth160",  "Q3oth",
"Oth1660",  "Oth460", 
"Oth1160",  "Oth560", "Q1chi",
"Q1por", "Q1vea",
"Q1lam", "Q3oth.1", "fre160", "fre560", "fre460", "fre1160", "fre1660")]


dieta<-subset(dieta, select = -c(Q1oth, Q2oth, Q3oth, Oth160, Q3oth, Oth1660, Oth460, 
                 Oth1160, Oth560,  Q1chi, Q1por,
                 Q1vea, Q1lam, Q3oth.1))

dieta<-subset(dieta, select = -c(Q3oth.1))



# some descriptives

res<-compareGroups(~., data=dieta, ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab
export2md(restab, strip = TRUE, first.strip = TRUE) 



# save the data:

write.csv(dieta, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/13072022_FoodFreqDay.csv")## To save the file

write.csv(culinary, "Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/13072022_CulinaryVar.csv")## To save the file



