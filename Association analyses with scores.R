
##### Association analyses with scores:

load("C:/Ordenador_German/MASTER_OMICS/TFM/Para Fran/28 Agosto/RScoreNutrients.Rdata")

meta<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Microbiome_metadata/NEW UPDATE ALL VARIABLES/14052018_metadata_AllMicrobiome.csv", header=TRUE, sep=";")


names(scores)

# take r.score

names(alpha)

# take: Richness and Shannon

# linear regression analyses:


aa<-as.data.frame(Foods.i)

aa<-merge(aa, meta, by.x="row.names", by.y="subject")

aa<-merge(aa, scores, by.x="Row.names", by.y="subject_id")

categorical <-  aa[,c(2:90)] # place in a new data set the specific variables you want to try

str(categorical)

bb<-as.data.frame(Nutrients.i)

aa$kcal<-bb$kcal

j=1
Results<-matrix(NA,89,6)
for(i in 1:ncol(categorical))
{
  print(i)
  fit<- try(lm(log(aa$r.score+0.00001) ~ scale(categorical[,i]) + factor(aa$sex) + factor(aa$center) + aa$agec + factor(aa$diabcat) + factor(aa$FHPDAC) + aa$cpy1 + aa$kcal))
  Results[j,1]<-round(summary(fit)$coefficients[2,1],4)
  Results[j,2]<-round(summary(fit)$coefficients[2,2],4)   
  Results[j,3]<-summary(fit)$coefficients[2,4]
  Results[j,4]<-round(summary(fit)$coefficients[2,1]-1.96*summary(fit)$coefficients[2,2], 3)#CI1_low
  Results[j,5]<-round(summary(fit)$coefficients[2,1]+1.96*summary(fit)$coefficients[2,2], 4) #CI1_high
  Results[j,6]<-round(summary(fit)$r.squared, 4)       
  j<-j+1
}
rownames(Results)<-colnames(categorical) #Don?t include the first column (subject)
colnames(Results)<-c("coeff" ,"SE", "p-value", "CILow", "CIHigh", "R-squared")
Results


#to adjust for multiple test
p.bh<-p.adjust(Results[,3], "BH") #False discovery rate (Benjamini Hochberg)
Results.adj=data.frame(cbind(Results,p.bh))
Results.adj



# for nutrients:


aa<-as.data.frame(Nutrients.i)

aa<-merge(aa, meta, by.x="row.names", by.y="subject")

aa<-merge(aa, scores, by.x="Row.names", by.y="subject_id")

categorical <-  aa[,c(3:46)] # place in a new data set the specific variables you want to try




j=1
Results<-matrix(NA,44,6)
for(i in 1:ncol(categorical))
{
  print(i)
  fit<- try(lm((aa$r.score+0.00001) ~ scale(categorical[,i]) + factor(aa$sex) + factor(aa$center) + aa$agec + aa$cpy1 + factor(aa$diabcat) + factor(aa$FHPDAC) + aa$kcal))
  Results[j,1]<-round(summary(fit)$coefficients[2,1],4)
  Results[j,2]<-round(summary(fit)$coefficients[2,2],4)   
  Results[j,3]<-summary(fit)$coefficients[2,4]
  Results[j,4]<-round(summary(fit)$coefficients[2,1]-1.96*summary(fit)$coefficients[2,2], 3)#CI1_low
  Results[j,5]<-round(summary(fit)$coefficients[2,1]+1.96*summary(fit)$coefficients[2,2], 4) #CI1_high
  Results[j,6]<-round(summary(fit)$r.squared, 4)       
  j<-j+1
}
rownames(Results)<-colnames(categorical) #Don?t include the first column (subject)
colnames(Results)<-c("coeff" ,"SE", "p-value", "CILow", "CIHigh", "R-squared")
Results


#to adjust for multiple test
p.bh<-p.adjust(Results[,3], "BH") #False discovery rate (Benjamini Hochberg)
Results.adj=data.frame(cbind(Results,p.bh))
Results.adj





j=1
Results<-matrix(NA,44,6)
for(i in 1:ncol(categorical))
{
  print(i)
  fit<- try(lm(log(aa$r.score+0.00001) ~ scale(categorical[,i]) + factor(aa$sex) + factor(aa$center) + aa$agec + factor(aa$obese) + factor(aa$diabcat) + factor(aa$FHPDAC)))
  Results[j,1]<-round(summary(fit)$coefficients[2,1],4)
  Results[j,2]<-round(summary(fit)$coefficients[2,2],4)   
  Results[j,3]<-summary(fit)$coefficients[2,4]
  Results[j,4]<-round(summary(fit)$coefficients[2,1]-1.96*summary(fit)$coefficients[2,2], 3)#CI1_low
  Results[j,5]<-round(summary(fit)$coefficients[2,1]+1.96*summary(fit)$coefficients[2,2], 4) #CI1_high
  Results[j,6]<-round(summary(fit)$r.squared, 4)       
  j<-j+1
}
rownames(Results)<-colnames(categorical) #Don?t include the first column (subject)
colnames(Results)<-c("coeff" ,"SE", "p-value", "CILow", "CIHigh", "R-squared")
Results


#to adjust for multiple test
p.bh<-p.adjust(Results[,3], "BH") #False discovery rate (Benjamini Hochberg)
Results.adj=data.frame(cbind(Results,p.bh))
Results.adj


## Food groups:




aa<-as.data.frame(Food.groups.i)

aa<-merge(aa, meta, by="subject")

aa<-merge(aa, scores, by.x="subject", by.y="subject_id")

aa$kcal<-bb$kcal

categorical <-  aa[,c(2:48)] # place in a new data set the specific variables you want to try




j=1
Results<-matrix(NA,47,6)
for(i in 1:ncol(categorical))
{
  print(i)
  fit<- try(lm(log(aa$r.score+0.00001) ~ scale(categorical[,i]) + factor(aa$sex) + factor(aa$center) + aa$agec + factor(aa$obese) + factor(aa$diabcat) + factor(aa$FHPDAC) + aa$kcal))
  Results[j,1]<-round(summary(fit)$coefficients[2,1],4)
  Results[j,2]<-round(summary(fit)$coefficients[2,2],4)   
  Results[j,3]<-summary(fit)$coefficients[2,4]
  Results[j,4]<-round(summary(fit)$coefficients[2,1]-1.96*summary(fit)$coefficients[2,2], 3)#CI1_low
  Results[j,5]<-round(summary(fit)$coefficients[2,1]+1.96*summary(fit)$coefficients[2,2], 4) #CI1_high
  Results[j,6]<-round(summary(fit)$r.squared, 4)       
  j<-j+1
}
rownames(Results)<-colnames(categorical) #Don?t include the first column (subject)
colnames(Results)<-c("coeff" ,"SE", "p-value", "CILow", "CIHigh", "R-squared")
Results


#to adjust for multiple test
p.bh<-p.adjust(Results[,3], "BH") #False discovery rate (Benjamini Hochberg)
Results.adj=data.frame(cbind(Results,p.bh))
Results.adj







categorical <-  aa[,c(2:41)] # place in a new data set the specific variables you want to try




j=1
Results<-matrix(NA,40,6)
for(i in 1:ncol(categorical))
{
  print(i)
  fit<- try(lm(log(aa$r.score+0.00001) ~ scale(categorical[,i]) + factor(aa$sex) + factor(aa$center) + aa$agec + aa$cpy1 + factor(aa$diabcat) + factor(aa$FHPDAC) + aa$kcal))
  Results[j,1]<-round(summary(fit)$coefficients[2,1],4)
  Results[j,2]<-round(summary(fit)$coefficients[2,2],4)   
  Results[j,3]<-summary(fit)$coefficients[2,4]
  Results[j,4]<-round(summary(fit)$coefficients[2,1]-1.96*summary(fit)$coefficients[2,2], 3)#CI1_low
  Results[j,5]<-round(summary(fit)$coefficients[2,1]+1.96*summary(fit)$coefficients[2,2], 4) #CI1_high
  Results[j,6]<-round(summary(fit)$r.squared, 4)       
  j<-j+1
}
rownames(Results)<-colnames(categorical) #Don?t include the first column (subject)
colnames(Results)<-c("coeff" ,"SE", "p-value", "CILow", "CIHigh", "R-squared")
Results


#to adjust for multiple test
p.bh<-p.adjust(Results[,3], "BH") #False discovery rate (Benjamini Hochberg)
Results.adj=data.frame(cbind(Results,p.bh))
Results.adj


#### For the richness scores:


## Foods:

aa<-as.data.frame(Foods.i)

aa<-merge(aa, alpha, by.x="row.names", by.y="subject_id")

#aa<-merge(aa, scores, by.x="Row.names", by.y="subject_id")

categorical <-  aa[,c(2:90)] # place in a new data set the specific variables you want to try

str(categorical)

bb<-as.data.frame(Nutrients.i)

aa$kcal<-bb$kcal


j=1
Results<-matrix(NA,89,6)
for(i in 1:ncol(categorical))
{
  print(i)
  fit<- try(lm(log(aa$exp.Shannon.+0.00001) ~ scale(categorical[,i]) + factor(aa$gender) + factor(aa$Center) + aa$age_years + factor(aa$diabcat) + factor(aa$FHPDAC) + factor(aa$cpy1t) + factor(aa$obese) + aa$kcal))
  Results[j,1]<-round(summary(fit)$coefficients[2,1],4)
  Results[j,2]<-round(summary(fit)$coefficients[2,2],4)   
  Results[j,3]<-summary(fit)$coefficients[2,4]
  Results[j,4]<-round(summary(fit)$coefficients[2,1]-1.96*summary(fit)$coefficients[2,2], 3)#CI1_low
  Results[j,5]<-round(summary(fit)$coefficients[2,1]+1.96*summary(fit)$coefficients[2,2], 4) #CI1_high
  Results[j,6]<-round(summary(fit)$r.squared, 4)       
  j<-j+1
}
rownames(Results)<-colnames(categorical) #Don?t include the first column (subject)
colnames(Results)<-c("coeff" ,"SE", "p-value", "CILow", "CIHigh", "R-squared")
Results


#to adjust for multiple test
p.bh<-p.adjust(Results[,3], "BH") #False discovery rate (Benjamini Hochberg)
Results.adj=data.frame(cbind(Results,p.bh))
Results.adj


# further adjustment by probiotics



j=1
Results<-matrix(NA,89,6)
for(i in 1:ncol(categorical))
{
  print(i)
  fit<- try(lm(log(aa$exp.Shannon.+0.00001) ~ scale(categorical[,i]) + factor(aa$gender) + factor(aa$Center) + aa$age_years + factor(aa$diabcat) + factor(aa$FHPDAC) + factor(aa$cpy1t) + factor(aa$obese) + aa$kcal + factor(aa$probiot)))
  Results[j,1]<-round(summary(fit)$coefficients[2,1],4)
  Results[j,2]<-round(summary(fit)$coefficients[2,2],4)   
  Results[j,3]<-summary(fit)$coefficients[2,4]
  Results[j,4]<-round(summary(fit)$coefficients[2,1]-1.96*summary(fit)$coefficients[2,2], 3)#CI1_low
  Results[j,5]<-round(summary(fit)$coefficients[2,1]+1.96*summary(fit)$coefficients[2,2], 4) #CI1_high
  Results[j,6]<-round(summary(fit)$r.squared, 4)       
  j<-j+1
}
rownames(Results)<-colnames(categorical) #Don?t include the first column (subject)
colnames(Results)<-c("coeff" ,"SE", "p-value", "CILow", "CIHigh", "R-squared")
Results


#to adjust for multiple test
p.bh<-p.adjust(Results[,3], "BH") #False discovery rate (Benjamini Hochberg)
Results.adj=data.frame(cbind(Results,p.bh))
Results.adj





## Food groups:




aa<-as.data.frame(Food.groups.i)

#aa<-merge(aa, meta, by="subject")

aa<-merge(aa, alpha, by.x="subject", by.y="subject_id")

aa$kcal<-bb$kcal

categorical <-  aa[,c(2:48)] # place in a new data set the specific variables you want to try





j=1
Results<-matrix(NA,47,6)
for(i in 1:ncol(categorical))
{
  print(i)
  fit<- try(lm(log(aa$exp.Shannon.+0.00001) ~ scale(categorical[,i]) + factor(aa$gender) + factor(aa$Center) + aa$age_years + factor(aa$diabcat) + factor(aa$FHPDAC) + factor(aa$cpy1t) + factor(aa$obese) + aa$kcal))
  Results[j,1]<-round(summary(fit)$coefficients[2,1],4)
  Results[j,2]<-round(summary(fit)$coefficients[2,2],4)   
  Results[j,3]<-summary(fit)$coefficients[2,4]
  Results[j,4]<-round(summary(fit)$coefficients[2,1]-1.96*summary(fit)$coefficients[2,2], 3)#CI1_low
  Results[j,5]<-round(summary(fit)$coefficients[2,1]+1.96*summary(fit)$coefficients[2,2], 4) #CI1_high
  Results[j,6]<-round(summary(fit)$r.squared, 4)       
  j<-j+1
}
rownames(Results)<-colnames(categorical) #Don?t include the first column (subject)
colnames(Results)<-c("coeff" ,"SE", "p-value", "CILow", "CIHigh", "R-squared")
Results


#to adjust for multiple test
p.bh<-p.adjust(Results[,3], "BH") #False discovery rate (Benjamini Hochberg)
Results.adj=data.frame(cbind(Results,p.bh))
Results.adj


### Nutrients:


aa<-as.data.frame(Nutrients.i)

#aa<-merge(aa, meta, by.x="row.names", by.y="subject")

aa<-merge(aa, alpha, by.x="row.names", by.y="subject_id")

categorical <-  aa[,c(3:46)] # place in a new data set the specific variables you want to try



j=1
Results<-matrix(NA,44,6)
for(i in 1:ncol(categorical))
{
  print(i)
  fit<- try(lm(log(aa$exp.Shannon.+0.00001) ~ scale(categorical[,i]) + factor(aa$gender) + aa$kcal + factor(aa$Center) + aa$age_years + factor(aa$diabcat) + factor(aa$FHPDAC) + factor(aa$cpy1t) + factor(aa$obese)))
  Results[j,1]<-round(summary(fit)$coefficients[2,1],4)
  Results[j,2]<-round(summary(fit)$coefficients[2,2],4)   
  Results[j,3]<-summary(fit)$coefficients[2,4]
  Results[j,4]<-round(summary(fit)$coefficients[2,1]-1.96*summary(fit)$coefficients[2,2], 3)#CI1_low
  Results[j,5]<-round(summary(fit)$coefficients[2,1]+1.96*summary(fit)$coefficients[2,2], 4) #CI1_high
  Results[j,6]<-round(summary(fit)$r.squared, 4)       
  j<-j+1
}
rownames(Results)<-colnames(categorical) #Don?t include the first column (subject)
colnames(Results)<-c("coeff" ,"SE", "p-value", "CILow", "CIHigh", "R-squared")
Results


#to adjust for multiple test
p.bh<-p.adjust(Results[,3], "BH") #False discovery rate (Benjamini Hochberg)
Results.adj=data.frame(cbind(Results,p.bh))
Results.adj


#########################################################
# plots:


## burbles for plotting:



MRS<-c(-0.0003,
       -0.0006,
       -0.0006,
       -0.0001,
       0.0001,
       0.0004,
       -0.0001,
       -0.0002,
       -0.0001,
       -0.0008,
       -0.0007,
       -0.0009,
       -0.0003,
       -0.0003,
       -0.0003,
       -0.0004,
       -0.0004,
       -0.0004,
       0.0000,
       -0.0006,
       0.0000,
       -0.0001,
       -0.0001,
       -0.0004,
       -0.0003,
       -0.0003,
       -0.0003,
       -0.0004,
       -0.0004,
       -0.0003,
       -0.0006,
       -0.0006,
       -0.0001,
       -0.0001,
       -0.0006,
       -0.0006,
       -0.0006,
       -0.0005,
       0.0000,
       -0.0005,
       -0.0006,
       -0.0003,
       -0.0008,
       -0.0005
)

rS<-c(0.0212,
      -0.0365,
      -0.0426,
      0.1174,
      0.0657,
      0.0438,
      0.0649,
      -0.0496,
      0.0047,
      -0.0476,
      -0.0085,
      -0.0294,
      0.0529,
      -0.036,
      0.0451,
      -0.002,
      -0.0168,
      -0.013,
      0.0383,
      -0.0231,
      -0.0254,
      0.0548,
      0.0327,
      -0.0011,
      0.1063,
      0.0426,
      -0.0358,
      -0.019,
      0.0007,
      -0.0233,
      -0.0652,
      -0.0367,
      0.0323,
      0.0331,
      -0.0196,
      -0.0282,
      -0.0277,
      -0.0665,
      -0.0554,
      -0.0017,
      -0.0138,
      -0.0136,
      -0.0471,
      -0.0313
)

SS<-c(0.0185,
      -0.0494,
      -0.0319,
      0.137,
      0.0739,
      0.0497,
      0.0603,
      -0.0608,
      0.0069,
      -0.0406,
      -0.0026,
      -0.0176,
      0.0527,
      -0.048,
      0.0493,
      0.0121,
      -0.0091,
      -0.0058,
      0.0402,
      -0.0162,
      -0.0365,
      0.0538,
      0.0303,
      0.007,
      0.1151,
      0.0375,
      -0.0352,
      -0.0123,
      -0.0037,
      -0.0118,
      -0.0565,
      -0.0346,
      0.0262,
      0.0271,
      -0.0097,
      -0.0166,
      -0.0154,
      -0.0679,
      -0.0519,
      0.009,
      -0.0233,
      -0.0133,
      -0.0582,
      -0.0462
)


bb<-cbind(MRS, rS, SS)
bb<-as.data.frame(bb)
rownames(bb)<-c("kcal",
                "Lipids.g",
                "Proteins.g",
                "Humidity.g",
                "Carbs.g",
                "Sucrose.g",
                "Fiber.g",
                "Starch.g",
                "Sugar.g",
                "Cholesterol.mg",
                "VitA.ug",
                "VitD.ug",
                "VitE.mg",
                "VitB8.ug",
                "VitB9.ug",
                "VitB3.mg",
                "VitB5.mg",
                "VitB2.mg",
                "VitB1.mg",
                "VitB12.ug",
                "VitB6.mg",
                "VitC.mg",
                "Calcium.mg",
                "Iron.mg",
                "Potasium.mg",
                "Magnesium.mg",
                "Sodium.mg",
                "Phosphorus.mg",
                "Copper.mg",
                "Iodide.ug",
                "Selenium.ug",
                "Zinc.mg",
                "Linoleic.g",
                "Linolenic.mg",
                "Araquidonic.gm",
                "DHA.g",
                "EPA.g",
                "Estearic.g",
                "Lauric.g",
                "Miristic.g",
                "Polyunsaturated.g",
                "Saturated.g",
                "Trans.g",
                "MFA.g"
)

#bb<-as.data.frame(lapply(bb, as.numeric))

library(ggpubr)

#colnames(bb)<-c("Breakfast skipping vs non-skipping", "Short time (< 30 minutes) to breakfast vs other", "Lunch at later afternoon (>14 h) vs earlier", "Dinner at later evening (>21 h) vs earlier", "Less than five meals vs more meals")

p2<-ggballoonplot(bb, fill = "value", color = "lightgray", shape = 22,
                  size = 5, show.label = FALSE, rotate.x.text=TRUE)+
  gradient_fill(c("blue","white","red"))


# make p1 for MRS and foods



MRS<-c(0.0001,
       0.0001,
       0.0003,
       -0.0004,
       0,
       0,
       0.0003,
       -0.0007,
       -0.0001,
       0.0008,
       0.0004,
       -0.0007,
       -0.0002,
       -0.0015,
       -0.0004,
       0.0005,
       -0.0005,
       0.0001,
       -0.0009,
       -0.0002,
       -0.0007,
       -0.0004,
       -0.0003,
       -0.0003,
       -0.0002,
       0.0007,
       -0.0006,
       -0.0002,
       0.0007,
       0.0004,
       0.0001,
       -0.0001,
       -0.0001,
       0.0006,
       0.0007,
       0.0008,
       0,
       -0.0002,
       0,
       0,
       0.0006,
       0.0004,
       -0.0001,
       0.0006,
       0,
       0.0006,
       0.0001
)



bb<-cbind(MRS)
bb<-as.data.frame(bb)
rownames(bb)<-c("galldairy",
                "sgmilkyogurt",
                "sgcheesse",
                "sgdairydessert",
                "gallmeat",
                "sgwhitemeat",
                "sgredmeat",
                "sgorganmeat",
                "sgcuredmeat",
                "sgprocessedmeat",
                "sgcuredprocessedmeat",
                "gallsea",
                "sgfish",
                "sgothersea",
                "meatseafood",
                "gallready",
                "gallveg",
                "sg1leafyveg",
                "sg1starchveg",
                "sg1sgfruitingveg",
                "sg1sggrainsveg",
                "sg2redyellveg",
                "sg2greenveg",
                "sg2whiteveg",
                "glegumes",
                "gallfruits",
                "golives",
                "gnuts",
                "gallcereals",
                "sgbread",
                "sgricepasta",
                "gallfats",
                "gflour",
                "gchocolate",
                "gsugar",
                "gsauces",
                "gbeverages",
                "sgsugbeverages",
                "sgartificialbev",
                "sgjuice",
                "coffee",
                "decoffee",
                "tea",
                "beer",
                "wine",
                "spirits",
                "fortified")

#bb<-as.data.frame(lapply(bb, as.numeric))

library(ggpubr)

#colnames(bb)<-c("Breakfast skipping vs non-skipping", "Short time (< 30 minutes) to breakfast vs other", "Lunch at later afternoon (>14 h) vs earlier", "Dinner at later evening (>21 h) vs earlier", "Less than five meals vs more meals")

p1<-ggballoonplot(bb, fill = "value", color = "lightgray", shape = 23,
                  size = 5, show.label = FALSE, rotate.x.text=TRUE)+
  gradient_fill(c("blue","white","red"))



# make the plot for food nutrients only:



MRS<-c(-0.0003,
       -0.0006,
       -0.0006,
       -0.0001,
       0.0001,
       0.0004,
       -0.0001,
       -0.0002,
       -0.0001,
       -0.0008,
       -0.0007,
       -0.0009,
       -0.0003,
       -0.0003,
       -0.0003,
       -0.0004,
       -0.0004,
       -0.0004,
       0.0000,
       -0.0006,
       0.0000,
       -0.0001,
       -0.0001,
       -0.0004,
       -0.0003,
       -0.0003,
       -0.0003,
       -0.0004,
       -0.0004,
       -0.0003,
       -0.0006,
       -0.0006,
       -0.0001,
       -0.0001,
       -0.0006,
       -0.0006,
       -0.0006,
       -0.0005,
       0.0000,
       -0.0005,
       -0.0006,
       -0.0003,
       -0.0008,
       -0.0005)


bb<-cbind(MRS)
bb<-as.data.frame(bb)
rownames(bb)<-c("kcal",
                "Lipids.g",
                "Proteins.g",
                "Humidity.g",
                "Carbs.g",
                "Sucrose.g",
                "Fiber.g",
                "Starch.g",
                "Sugar.g",
                "Cholesterol.mg",
                "VitA.ug",
                "VitD.ug",
                "VitE.mg",
                "VitB8.ug",
                "VitB9.ug",
                "VitB3.mg",
                "VitB5.mg",
                "VitB2.mg",
                "VitB1.mg",
                "VitB12.ug",
                "VitB6.mg",
                "VitC.mg",
                "Calcium.mg",
                "Iron.mg",
                "Potasium.mg",
                "Magnesium.mg",
                "Sodium.mg",
                "Phosphorus.mg",
                "Copper.mg",
                "Iodide.ug",
                "Selenium.ug",
                "Zinc.mg",
                "Linoleic.g",
                "Linolenic.mg",
                "Araquidonic.gm",
                "DHA.g",
                "EPA.g",
                "Estearic.g",
                "Lauric.g",
                "Miristic.g",
                "Polyunsaturated.g",
                "Saturated.g",
                "Trans.g",
                "MFA.g")

#bb<-as.data.frame(lapply(bb, as.numeric))

library(ggpubr)

#colnames(bb)<-c("Breakfast skipping vs non-skipping", "Short time (< 30 minutes) to breakfast vs other", "Lunch at later afternoon (>14 h) vs earlier", "Dinner at later evening (>21 h) vs earlier", "Less than five meals vs more meals")

p3<-ggballoonplot(bb, fill = "value", color = "lightgray", shape = 22,
                  size = 5, show.label = FALSE, rotate.x.text=TRUE)+
  gradient_fill(c("blue","white","red"))



## associations between the scores with PC risk:

fit1<-glm(aa$casecontrol~scale(aa$r.score) + aa$agec + aa$center + aa$sex, family="binomial")
summary(fit1)

fit1<-glm(aa$subject_disease_status~scale(aa$Richness) + aa$age_years + aa$Center + aa$gender, family="binomial")
summary(fit1)

fit1<-glm(aa$subject_disease_status~scale(aa$exp.Shannon.) + aa$age_years + aa$Center + aa$gender, family="binomial")
summary(fit1)





# All combined:

library(ggpubr)


tiff("C:/Ordenador_German/MASTER_OMICS/TFM/Figures/MRScore.tif",width = 12, height = 10, units = "in", res = 600)   

lay = rbind(c(1,2,3,3))
print(grid.arrange(p1, p3, p2, ncol=3, layout_matrix=lay))
print(grid.arrange(arrangeGrob(p1, left = text_grob("A", x = unit(1, "npc"), 
                                                  y = unit(0.95, "npc"))), 
               arrangeGrob(p3, left =text_grob("B", x = unit(1, "npc"), 
                                               y = unit(0.95, "npc"))),
               arrangeGrob(p2, left=text_grob("C", x = unit(1, "npc"), 
                                        y = unit(0.95, "npc"))),
                   layout_matrix = lay))

dev.off()

###################################################
## Final tests: association between the MRS with the dietary scores:

diet.scores<-read.csv("C:/Ordenador_German/MASTER_OMICS/TFM/Section Diet/SCORES/020822_scores.csv")


aa<-as.data.frame(Foods.i)

aa<-merge(aa, diet.scores, by.x="row.names", by.y="subject")

aa<-merge(aa, scores, by.x="Row.names", by.y="subject_id")

aa<-merge(aa, meta, by.x="Row.names", by.y="subject")

#categorical <-  aa[,c(2:90)] # place in a new data set the specific variables you want to try

#str(categorical)

bb<-as.data.frame(Nutrients.i)

identical(as.character(aa$Row.names), rownames(bb))

aa$kcal<-bb$kcal


fit1<-glm(log(aa$DRRDSCORE+0.0001)~scale(aa$r.score) + aa$agec + aa$center.x + aa$sex)
summary(fit1)
#scale(aa$r.score)  0.036730   0.020111   1.826   0.0710 .

fit1<-glm(log(aa$DRRDSCORE+0.0001)~scale(aa$r.score) + aa$agec + factor(aa$center.x) + factor(aa$sex) + factor(aa$diabcat) + factor(aa$FHPDAC) + aa$kcal + aa$cpy1)
summary(fit1)
#3.645e-02  2.094e-02   1.740   0.0854 .

fit1<-glm(log(aa$MDSCORE+0.0001)~scale(aa$r.score) + aa$agec + factor(aa$center.x) + factor(aa$sex) + factor(aa$diabcat) + factor(aa$FHPDAC) + aa$kcal + aa$cpy1)
summary(fit1)
#-2.407e-02  3.257e-02  -0.739  0.46188 

fit1<-glm(log(aa$MDSCORE+0.0001)~scale(aa$r.score) + aa$agec + factor(aa$center.x) + factor(aa$sex))
summary(fit1)
#-0.025517   0.034520  -0.739    0.462 

aa<-as.data.frame(Foods.i)

aa<-merge(aa, diet.scores, by.x="row.names", by.y="subject")

aa<-merge(aa, alpha, by.x="Row.names", by.y="subject_id")

aa<-merge(aa, meta, by.x="Row.names", by.y="subject")

#categorical <-  aa[,c(2:90)] # place in a new data set the specific variables you want to try

#str(categorical)

bb<-as.data.frame(Nutrients.i)

identical(as.character(aa$Row.names), rownames(bb))

aa$kcal<-bb$kcal


fit1<-glm(log(aa$DRRDSCORE+0.0001)~scale(aa$Richness) + aa$agec + aa$center.x + aa$sex)
summary(fit1)
#-0.001986   0.019214  -0.103   0.9179

fit1<-glm(log(aa$DRRDSCORE+0.0001)~scale(aa$Richness) + aa$agec + factor(aa$center.x) + factor(aa$sex) + factor(aa$diabcat.x) + factor(aa$FHPDAC.x) + aa$kcal + aa$cpy1)
summary(fit1)
#8.210e-03  1.924e-02   0.427   0.6706

fit1<-glm(log(aa$MDSCORE+0.0001)~scale(aa$Richness) + aa$agec + factor(aa$center.x) + factor(aa$sex))
summary(fit1)
#0.012247   0.032483   0.377    0.707

fit1<-glm(log(aa$MDSCORE+0.0001)~scale(aa$Richness) + aa$agec + factor(aa$center.x) + factor(aa$sex) + factor(aa$diabcat.x) + factor(aa$FHPDAC.x) + aa$kcal + aa$cpy1)
summary(fit1)
#1.603e-02  3.410e-02   0.470    0.639