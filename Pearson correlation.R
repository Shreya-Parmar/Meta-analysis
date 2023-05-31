#load library:
library("robumeta")
library("metafor")
library("dplyr")
library("esc")
library("esvis")
library("ggplot2")
library("ggpubr")

setwd("C:/Users/Asus/OneDrive/Desktop/Shreya/Results/Excel")
#load the csv
dat_1<- read.csv("Studies with seperate gender.csv")
dat_1 <- replace(dat_1, is.na(dat_1), 0)
#dat_1<- dat_1[-c(4),]

#########################################################################################################################
#MALE

#1 Pearson correlation:

#BMI 
res <- cor.test(dat_1$AGE_M,dat_1$BMI_M, 
                method = "pearson")
res
Age<- dat_1$AGE_M
BMI<- dat_1$BMI_M

# Draw scatter plot with pearson correlation 
# coefficient
ggplot( dat_1, aes( x=Age, y=BMI ))+
  geom_point()+
  stat_cor(method = "pearson", label.x = -5, label.y = 30)

####################################################################################################################################
#HbA1c

res <- cor.test(dat_1$AGE_M,dat_1$HbA1C_M, 
                method = "pearson")
res
Age<- dat_1$AGE_M
HbA1c<- dat_1$HbA1C_M

# Draw scatter plot with pearson correlation 
# coefficient
ggplot( dat_1, aes( x=Age, y=HbA1c ))+
  geom_point()+
  stat_cor(method = "pearson", label.x = -5, label.y = 10)

#############################################################################################
#TC

res <- cor.test(dat_1$AGE_M,dat_1$TOT.CHOLESTEROL_M, 
                method = "pearson")
res
Age<- dat_1$AGE_M
TC<- dat_1$TOT.CHOLESTEROL_M

# Draw scatter plot with pearson correlation 
# coefficient
ggplot( dat_1, aes( x=Age, y=TC))+
  geom_point()+
  stat_cor(method = "pearson", label.x = -5, label.y = 10)

############################################################################################
#TG
res <- cor.test(dat_1$AGE_M,dat_1$TGs_M, 
                method = "pearson")
res
Age<- dat_1$AGE_M
TG<- dat_1$TGs_M

# Draw scatter plot with pearson correlation 
# coefficient
ggplot( dat_1, aes( x=Age, y=TG))+
  geom_point()+
  stat_cor(method = "pearson", label.x = -5, label.y = 10)

########################################################################################################
#HDL
res <- cor.test(dat_1$AGE_M,dat_1$HDL_M, 
                method = "pearson")
res
Age<- dat_1$AGE_M
HDL<- dat_1$HDL_M

# Draw scatter plot with pearson correlation 
# coefficient
ggplot( dat_1, aes( x=Age, y=HDL))+
  geom_point()+
  stat_cor(method = "pearson", label.x = -5, label.y = 10)

#########################################################################################
#Insulin Dose
res <- cor.test(dat_1$AGE_M,dat_1$INSULIN.DOSE_M, 
                method = "pearson")
res
Age<- dat_1$AGE_M
Insulin_Dose<- dat_1$INSULIN.DOSE_M

# Draw scatter plot with pearson correlation 
# coefficient
ggplot( dat_1, aes( x=Age, y=Insulin_Dose))+
  geom_point()+
  stat_cor(method = "pearson", label.x = -5, label.y = 2)

###########################################################################################
########################################################################################

############################################################################################
##############################################################################################
#FEMALE:


#BMI 
res <- cor.test(dat_1$AGE_F,dat_1$BMI_F, 
                method = "pearson")
res
Age<- dat_1$AGE_F
BMI<- dat_1$BMI_F

# Draw scatter plot with pearson correlation 
# coefficient
ggplot( dat_1, aes( x=Age, y=BMI ))+
  geom_point()+
  stat_cor(method = "pearson", label.x = -5, label.y = 30)

###########################################################################33333333333333333
#HbA1c

res <- cor.test(dat_1$AGE_F,dat_1$HbA1C_F, 
                method = "pearson")
res
Age<- dat_1$AGE_F
HbA1c<- dat_1$HbA1C_F

# Draw scatter plot with pearson correlation 
# coefficient
ggplot( dat_1, aes( x=Age, y=HbA1c ))+
  geom_point()+
  stat_cor(method = "pearson", label.x = -5, label.y = 10)

#############################################################################################
#TC

res <- cor.test(dat_1$AGE_F,dat_1$TOT.CHOLESTEROL_F, 
                method = "pearson")
res
Age<- dat_1$AGE_F
TC<- dat_1$TOT.CHOLESTEROL_F

# Draw scatter plot with pearson correlation 
# coefficient
ggplot( dat_1, aes( x=Age, y=TC))+
  geom_point()+
  stat_cor(method = "pearson", label.x = -5, label.y = 10)

############################################################################################
#TG
res <- cor.test(dat_1$AGE_F,dat_1$TGs_F, 
                method = "pearson")
res
Age<- dat_1$AGE_F
TG<- dat_1$TGs_F

# Draw scatter plot with pearson correlation 
# coefficient
ggplot( dat_1, aes( x=Age, y=TG))+
  geom_point()+
  stat_cor(method = "pearson", label.x = -5, label.y = 10)

########################################################################################################
#HDL
res <- cor.test(dat_1$AGE_F,dat_1$HDL_F, 
                method = "pearson")
res
Age<- dat_1$AGE_F
HDL<- dat_1$HDL_F

# Draw scatter plot with pearson correlation 
# coefficient
ggplot( dat_1, aes( x=Age, y=HDL))+
  geom_point()+
  stat_cor(method = "pearson", label.x = -5, label.y = 10)

#########################################################################################
#Insulin Dose
res <- cor.test(dat_1$AGE_F,dat_1$INSULIN.DOSE_F, 
                method = "pearson")
res
Age<- dat_1$AGE_F
Insulin_Dose<- dat_1$INSULIN.DOSE_F

# Draw scatter plot with pearson correlation 
# coefficient
ggplot( dat_1, aes( x=Age, y=Insulin_Dose))+
  geom_point()+
  stat_cor(method = "pearson", label.x = -5, label.y = 2)












