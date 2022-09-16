

## Multivariate analyses on the association between dietary factors and PC risk

# load the data from Microbiome:

setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodGrams")
FoodGrams<-read.csv("14072022_GramsDayAll with Zeros.csv")
FoodGramas.Group<-read.csv("14072022_GramsDayall_StandardServing(GROUPS).csv")

# load the data from Microbiome and pangen:

setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME_PANGEN/FoodGrams")
FoodGramsPM<-read.csv("20072022_FoodGramsPM_complete.csv")
FoodGramsPM.Group<-read.csv("20072022_FoodGramsPM(GROUPS).csv")

#FoodGramsPM.Group<-read.csv("C:/Ordenador_German/MASTER_OMICS/TFM/Section Diet/SCORES/020822_FoodGramsPMGroup.csv")

# load the data for nutrients:
setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME/FoodNutrients")
Nutr<-read.csv("20072022_Nutrients_Serving with GI.csv")
setwd("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Secciones trabajadas/Section Diet/MICROBIOME_PANGEN/FoodNutrients")
NutrPM<-read.csv("20072022_FoodNutrientsPM.csv")

# metadata:
meta<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/PanGen/QES/Data/Microbiome_metadata/NEW UPDATE ALL VARIABLES/14052018_metadata_AllMicrobiome.csv", header=TRUE, sep=";")
metaPM<-read.csv("Z:/Genetic_and_Molecular_Epidemiology/Backup/Esther Molina/EMBL_STAY/Imputation/ImputationOnAllData/01062018_imputeddataNew.csv", header=TRUE, sep=";") # ONLY FOR THE SPANISH DATA



# join metadata with Foods, groups and nutrients: to be done among all cases and controls

aa<-merge(metaPM, NutrPM, by="subject")

aa<-aa[aa$casecontrol!=2,]

###########################################
# Logistic regression models: NUTRIENTS

categorical <-  aa[,c(47:90)] # place in a new data set the specific variables you want to try

str(categorical)


#Create a loop to run one by one all the variables you want to test

j=1
Results<-matrix(NA,44,4) #Number of rows is the number of variables you want to try, # 4 is the number of columns
for (i in 1:ncol(categorical)){
  print(i)
  mod1a<- try(glm (factor(aa$casecontrol) ~ scale(categorical[,i]) + factor(aa$sex) + factor(aa$center) + aa$agec + factor(aa$diabcat) + factor(aa$cpy1t) + factor(aa$FHPDAC) + aa$kcal, family=binomial))
  print(i) 
  Results[j,1]<-round(exp(summary(mod1a)$coeff[2,1]),2)          #OR1
  Results[j,2]<-round(exp(summary(mod1a)$coeff[2,1]-1.96*summary(mod1a)$coeff[2,2]),2)   #CI1_low
  Results[j,3]<-round(exp(summary(mod1a)$coeff[2,1]+1.96*summary(mod1a)$coeff[2,2]),2)   #CI1_high
  Results[j,4]<-round(summary(mod1a)$coeff[2,4],2)          #P1
  j<-j+1
}
colnames(Results)<-c("OR" ,"LCI", "HCI", "P.value")

rownames(Results)<- colnames(aa) #In order place all the variables from data

Results



table1<-as.data.frame(Results)
table1$a <- " ["; table1$b <- ";"; table1$c <- "] "
table1$a
table1 <- table1[,c("OR","a", "LCI","b","HCI","c","P.value")]
table1 = unite(table1, "OR (95%CI)", c(OR, a, LCI, b, HCI, c, P.value), sep ="", remove=T)
#View(table1)
table1

#to adjust for multiple test
p.bh<-p.adjust(Results[,4], "BH") #False discovery rate (Benjamini Hochberg)
Results.adj=data.frame(cbind(Results,p.bh))
Results.adj

rownames(Results.adj)<-colnames(categorical)

Results.adj




###########################################
# Logistic regression models: FOODS


bb<-aa

aa<-merge(metaPM, FoodGramsPM, by="subject")

aa<-aa[aa$casecontrol!=2,]


categorical <-  aa[,c(46:134)] # place in a new data set the specific variables you want to try

str(categorical)

identical(aa$subject, bb$subject)

#Create a loop to run one by one all the variables you want to test

j=1
Results<-matrix(NA,89,4) #Number of rows is the number of variables you want to try, # 4 is the number of columns
for (i in 1:ncol(categorical)){
  print(i)
  mod1a<- try(glm (factor(aa$casecontrol) ~ scale(categorical[,i]) + factor(aa$sex) + factor(aa$center) + aa$agec + factor(aa$diabcat) + factor(aa$cpy1t) + factor(aa$FHPDAC) + bb$kcal, family=binomial))
  print(i) 
  Results[j,1]<-round(exp(summary(mod1a)$coeff[2,1]),2)          #OR1
  Results[j,2]<-round(exp(summary(mod1a)$coeff[2,1]-1.96*summary(mod1a)$coeff[2,2]),2)   #CI1_low
  Results[j,3]<-round(exp(summary(mod1a)$coeff[2,1]+1.96*summary(mod1a)$coeff[2,2]),2)   #CI1_high
  Results[j,4]<-round(summary(mod1a)$coeff[2,4],2)          #P1
  j<-j+1
}
colnames(Results)<-c("OR" ,"LCI", "HCI", "P.value")

rownames(Results)<- colnames(aa) #In order place all the variables from data

Results



table1<-as.data.frame(Results)
table1$a <- " ["; table1$b <- ";"; table1$c <- "] "
table1$a
table1 <- table1[,c("OR","a", "LCI","b","HCI","c","P.value")]
table1 = unite(table1, "OR (95%CI)", c(OR, a, LCI, b, HCI, c, P.value), sep ="", remove=T)
#View(table1)
table1

#to adjust for multiple test
p.bh<-p.adjust(Results[,4], "BH") #False discovery rate (Benjamini Hochberg)
Results.adj=data.frame(cbind(Results,p.bh))
Results.adj

rownames(Results.adj)<-colnames(categorical)

Results.adj



###########################################
# Logistic regression models: FOOD GROUPS


aa<-merge(metaPM, FoodGramsPM.Group, by="subject")

aa<-aa[aa$casecontrol!=2,]


categorical <-  aa[,c(128:167)] # place in a new data set the specific variables you want to try

str(categorical)

identical(aa$subject, bb$subject)


#Create a loop to run one by one all the variables you want to test

j=1
Results<-matrix(NA,40,4) #Number of rows is the number of variables you want to try, # 4 is the number of columns
for (i in 1:ncol(categorical)){
  print(i)
  mod1a<- try(glm (factor(aa$casecontrol) ~ scale(categorical[,i]) + factor(aa$sex) + factor(aa$center) + aa$agec + factor(aa$diabcat) + factor(aa$cpy1t) + factor(aa$FHPDAC) + bb$kcal + aa$obese, family=binomial))
  print(i) 
  Results[j,1]<-round(exp(summary(mod1a)$coeff[2,1]),2)          #OR1
  Results[j,2]<-round(exp(summary(mod1a)$coeff[2,1]-1.96*summary(mod1a)$coeff[2,2]),2)   #CI1_low
  Results[j,3]<-round(exp(summary(mod1a)$coeff[2,1]+1.96*summary(mod1a)$coeff[2,2]),2)   #CI1_high
  Results[j,4]<-round(summary(mod1a)$coeff[2,4],2)          #P1
  j<-j+1
}
colnames(Results)<-c("OR" ,"LCI", "HCI", "P.value")

rownames(Results)<- colnames(aa) #In order place all the variables from data

Results



table1<-as.data.frame(Results)
table1$a <- " ["; table1$b <- ";"; table1$c <- "] "
table1$a
table1 <- table1[,c("OR","a", "LCI","b","HCI","c","P.value")]
table1 = unite(table1, "OR (95%CI)", c(OR, a, LCI, b, HCI, c, P.value), sep ="", remove=T)
#View(table1)
table1

#to adjust for multiple test
p.bh<-p.adjust(Results[,4], "BH") #False discovery rate (Benjamini Hochberg)
Results.adj=data.frame(cbind(Results,p.bh))
Results.adj

rownames(Results.adj)<-colnames(categorical)

Results.adj



############# Associations with the diet scores ####################
############# Results are in R markdown files   ####################

# scripts for plots:

### ODDS RATIO PLOTS:

library(ggplot2)
library(gridExtra)

# Create labels
boxLabels = c("overall crude",
              "overall crude among non-T2D",
              "overall adjusted",
              "overall adjusted + T2D",
              "overall adjusted among non-T2D",
              "overall adjusted among T2D", "non-obese adjusted + T2D", "obese adjusted + T2D", "women adjusted + T2D", "men adjusted + T2D", "non-T2D women adjusted", "non-T2D men adjusted")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.

df <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(1.060,
              1.048,
              1.067,
              1.057,
              1.036,
              1.255,
              1.059,
              1.098,
              1.126,
              1.003,
              1.116,
              0.976),
  boxCILow = c(1.004,
               0.989,
               1.007,
               1.000,
               0.974,
               1.063,
               0.991,
               0.957,
               1.034,
               0.922,
               1.019,
               0.891),
  boxCIHigh = c(1.119,
                1.111,
                1.132,
                1.118,
                1.102,
                1.500,
                1.132,
                1.263,
                1.228,
                1.092,
                1.224,
                1.070)
)


df<-as.data.frame(df)
df$cyl<-c("orangered","wheat","orangered", "wheat","orangered", "wheat", "blue", "red", "blue", "red", "blue", "red")


p1 <- ggplot(df, aes(x = boxOdds, y = yAxis, color=cyl))
p1<-p1 + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") + 
  geom_point(size = 4.5, color = df$cyl) +  
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = df$yAxis, labels = boxLabels) +
  scale_x_continuous(breaks = seq(0,7,1) ) +
  #coord_trans(x = "log10") +
  ylab("") +
  xlab("") +
  annotate(geom = "text", y =1.1, x = 1.5, label =" ", size = 3.5, hjust = 0) + ggtitle("A")


## DRRD by components

# Create labels
boxLabels = c("crude but adjusted for fruits",
              "crude but adjusted for fatty acids ratio",
              "crude but adjusted for GI",
              "crude but adjusted for trans-fats",
              "crude but adjusted for fiber",
              "crude but adjusted for juices",
              "crude but adjusted for meats",
              "crude but adjusted for nuts",
              "crude but adjusted for coffee",
              "crude but adjusted for energy intake")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.

df <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(1.067,
              1.074,
              1.071,
              1.093,
              1.084,
              1.076,
              1.086,
              1.059,
              1.062,
              1.096),
  boxCILow = c(1.012,
               1.016,
               1.017,
               1.031,
               1.021,
               1.017,
               1.025,
               1.001,
               1.006,
               1.039),
  boxCIHigh = c(1.124,
                1.135,
                1.129,
                1.158,
                1.153,
                1.139,
                1.152,
                1.121,
                1.121,
                1.157)
)


df<-as.data.frame(df)
df$cyl<-c("black", "black", "black", "black", "black", "black", "black", "black", "black", "black")


p2 <- ggplot(df, aes(x = boxOdds, y = yAxis, color=cyl))
p2<-p2 + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") + 
  geom_point(size = 4.5, color = df$cyl) +  
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = df$yAxis, labels = boxLabels) +
  scale_x_continuous(breaks = seq(0,7,1) ) +
  #coord_trans(x = "log10") +
  ylab("") +
  xlab("") +
  annotate(geom = "text", y =1.1, x = 1.5, label =" ", size = 3.5, hjust = 0) + ggtitle("B")



# MD cores:



# Create labels
boxLabels = c("crude model",
              "adjusted",
              "among non-T2D adjusted",
              "among T2D adjusted",
              "among non obese adjusted",
              "among obese adjusted",
              "among women adjusted",
              "among men adjusted")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.

df <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(1.078,
              1.051,
              1.037,
              1.142,
              1.045,
              1.008,
              1.038,
              1.032),
  boxCILow = c(0.966,
               0.929,
               0.903,
               0.871,
               0.909,
               0.761,
               0.857,
               0.874),
  boxCIHigh = c(1.205,
                1.189,
                1.193,
                1.502,
                1.203,
                1.335,
                1.257,
                1.219)
)


df<-as.data.frame(df)
df$cyl<-c("black", "black", "green", "orange", "green", "orange", "green", "orange")


p3 <- ggplot(df, aes(x = boxOdds, y = yAxis, color=cyl))
p3<-p3 + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") + 
  geom_point(size = 4.5, color = df$cyl) +  
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = df$yAxis, labels = boxLabels) +
  scale_x_continuous(breaks = seq(0,7,1) ) +
  #coord_trans(x = "log10") +
  ylab("") +
  xlab("") +
  annotate(geom = "text", y =1.1, x = 1.5, label =" ", size = 3.5, hjust = 0) + ggtitle("C")



# All combined:

library(ggpubr)


tiff("C:/Ordenador_German/MASTER_OMICS/TFM/Figures/DietScoresPC risk.tif",width = 12, height = 10, units = "in", res = 600)   

lay = rbind(c(1,1,1,1),
            c(2,2,3,3))
print(grid.arrange(p1, p2, p3, nrow=2, layout_matrix=lay))
#print(grid.arrange(arrangeGrob(p1, left = textGrob("a)", x = unit(1, "npc"), 
#                                                   y = unit(0.95, "npc"))), 
#                   arrangeGrob(p2, left =textGrob("b)", x = unit(1, "npc"), 
#                                                  y = unit(0.95, "npc"))),
#                   arrangeGrob(p3, left=textGrob("b)", x = unit(1, "npc"), 
#                                                 y = unit(0.95, "npc"))),
#                   layout_matrix = lay))

dev.off()

res<-compareGroups(casecontrol.y~., data=aa[,c(141,143:175)], ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab
