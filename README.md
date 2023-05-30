# Meta-analysis for parameters of Insulin Resitance in T1D.
#load library:
library("robumeta")
library("metafor")
library("dplyr")
library("esc")
library("esvis")
library("ggplot2")
setwd("C:/Users/Asus/OneDrive/Desktop")
#load the csv
dat_1<- read.csv("Studies with seperate gender.csv")
dat_1 <- replace(dat_1, is.na(dat_1), 0)
#dat_1<- dat_1[-c(3),]
**#FOR HbA1c**
dat_HbA1c<-esc_mean_sd(grp1m = dat_1$HbA1C_M, grp1sd = dat_1$HbA1C_M_SD, grp1n = dat_1$MALE_No,
                       grp2m = dat_1$HbA1C_F, grp2sd = dat_1$HbA1C_F_SD, grp2n = dat_1$FEMALE_No,
                       es.type = "g", study = dat_1$Author.and.year)
res_HbA1c<- rma(yi =dat_HbA1c$es,vi =dat_HbA1c$var, method="HE")
res_HbA1c
forest(res_HbA1c, at=log(c(.05, .25, 1, 4)), xlim=c(-10,6),
       ilab.xpos=c(-6.0,-5.3,-4.5,-3.5),
       ilab=cbind(dat_1$HbA1C_M, dat_1$HbA1C_M_SD,dat_1$HbA1C_F,dat_1$HbA1C_F_SD),
       cex=.75, header="Author(s) and years",slab = dat_HbA1c$study, xlab = "Hedge's g", showweights = TRUE,
       mlab="")
op <- par(cex=.75, font=2)
text(c(-6.7,-5.5,-4.3,-3.0),res_HbA1c$k+2, c("HbA1c_M", "HbA1c_SD", "HbA1c_M", "HbA1c_SD"))
text(c(-5.7,-3.7), res_HbA1c$b+14,c("Male", "Female"))
par(op)
### add text with Q-value, dfs, p-value, and I^2 statistic
text(-10, -0.5, pos=4, cex=0.75, bquote(paste(bold("RE Model"))))

text(-10, -1, pos=4, cex=0.77, bquote(paste(bold("Heterogeneity: "), "Q = ",
                                            .(formatC(res_HbA1c$QE, digits=2, format="f")), ", df = ", .(res_HbA1c$k -res_HbA1c$p),
                                            ", p = ", .(formatC(res_HbA1c$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res_HbA1c$I2, digits=1, format="f")), "%")))
### add text with test of theta
text(-10, -1.7, pos=4, cex=0.77 , bquote(paste(bold("Test for θ:"), "zval =",
                                               .(formatC(res_HbA1c$zval, digits=2, format="f")), 
                                               ", pval = ", .(formatC(res_HbA1c$pval, digits=2, format="f")))))

funnel(res_HbA1c, main="HbA1c", label=TRUE)

**#INSULIN DOSE**
dat_ID<-esc_mean_sd(grp1m = dat_1$INSULIN.DOSE_M, grp1sd = dat_1$INSULIN.DOSE_M_SD, grp1n = dat_1$MALE_No,
                    grp2m = dat_1$INSULIN.DOSE_F, grp2sd = dat_1$INSULIN.DOSE_F_SD, grp2n = dat_1$FEMALE_No,
                    es.type = "g", study = dat_1$Author.and.year)
res_ID<- rma(yi =dat_ID$es,vi =dat_ID$var, method ="HE")
res_ID
forest(res_ID, at=log(c(.05, .25, 1, 4)), xlim=c(-10,6),
       ilab.xpos=c(-5.5,-4.7,-3.5,-2.5),
       ilab=cbind(dat_1$INSULIN.DOSE_M, dat_1$INSULIN.DOSE_M_SD,dat_1$INSULIN.DOSE_F,dat_1$INSULIN.DOSE_F_SD),
       cex=.75, header="Author(s) and years",slab = dat_ID$study, xlab = "Hedge's g", showweights = TRUE,
       mlab="")
op <- par(cex=.75, font=2)
text(c(-5.5,-4.7,-3.5,-2.5),res_ID$k+2, c("ID_M", "ID_SD", "ID_M", "ID_SD"))
text(c(-5.5,-3.5), res_ID$b+10,c("Male", "Female"))
par(op)

### add text with Q-value, dfs, p-value, and I^2 statistic
text(-10, -0.5, pos=4, cex=0.75, bquote(paste(bold("RE Model"))))

text(-10, -1, pos=4, cex=0.77, bquote(paste(bold("Heterogeneity: "), "Q = ",
                                            .(formatC(res_ID$QE, digits=2, format="f")), ", df = ", .(res_ID$k - res_ID$p),
                                            ", p = ", .(formatC(res_ID$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res_ID$I2, digits=1, format="f")), "%")))
### add text with test of theta
text(-10, -1.7, pos=4, cex=0.77 , bquote(paste(bold("Test for θ:"), "zval =",
                                               .(formatC(res_ID$zval, digits=2, format="f")), 
                                               ", pval = ", .(formatC(res_ID$pval, digits=2, format="f")))))

funnel(res_ID, main="Insulin dose",label=TRUE)

**#FOR BMI**
dat_BMI<-esc_mean_sd(grp1m = dat_1$BMI_M, grp1sd = dat_1$BMI_M_SD, grp1n = dat_1$MALE_No,
                     grp2m = dat_1$BMI_F, grp2sd = dat_1$BMI_F_SD, grp2n = dat_1$FEMALE_No,
                     es.type = "g", study = dat_1$Author.and.year)
res_BMI<- rma(yi =dat_BMI$es,vi =dat_BMI$var,  method = "HE")
res_BMI
 ## forest plot with extra annotations
forest(res_BMI, at=log(c(.05, .25, 1, 4)), xlim=c(-12,6),
       ilab.xpos=c(-5.5,-4.5,-3.5,-2.5),
       ilab=cbind(dat_1$BMI_M, dat_1$BMI_M_SD,dat_1$BMI_F,dat_1$BMI_F_SD),
       cex=.85, header="Author(s) and years",slab = dat_BMI$study, xlab = "Hedge's g", showweights = TRUE,
       mlab="")
op <- par(cex=.75, font=2)
text(c(-5.9,-4.7,-3.2,-2),res_BMI$k+2, c("BMI_M", "BMI_SD", "BMI_M", "BMI_SD"))
text(c(-5.5,-3.5), res_BMI$b+14,c("Male", "Female"))
par(op)
### add text with Q-value, dfs, p-value, and I^2 statistic
text(-12, -0.5, pos=4, cex=0.75, bquote(paste(bold("RE Model"))))
text(-12, -1, pos=4, cex=0.77, bquote(paste(bold("Heterogeneity: "), "Q = ",
.(formatC(res_BMI$QE, digits=2, format="f")), ", df = ", .(res_BMI$k - res_BMI$p),
", p = ", .(formatC(res_BMI$QEp, digits=2, format="f")), "; ", I^2, " = ",
.(formatC(res_BMI$I2, digits=1, format="f")), "%")))
### add text with test of theta
text(-12, -1.7, pos=4, cex=0.77 , bquote(paste(bold("Test for θ:"), "zval =",
.(formatC(res_BMI$zval, digits=2, format="f")), 
", pval = ", .(formatC(res_BMI$pval, digits=2, format="f")))))

 funnel(res_BMI, main="BMI",label=TRUE)

**#for HDL**
dat_HDL<-esc_mean_sd(grp1m = dat_1$HDL_M, grp1sd = dat_1$HDL_M_SD, grp1n = dat_1$MALE_No,
                     grp2m = dat_1$HDL_F, grp2sd = dat_1$HDL_F_SD, grp2n = dat_1$FEMALE_No,
                     es.type = "g", study = dat_1$Author.and.year)
res_HDL<- rma(yi =dat_HDL$es,vi =dat_HDL$var, method ="HE")
res_HDL
forest(res_HDL, at=log(c(.05, .25, 1, 4)), xlim=c(-10,6),
       ilab.xpos=c(-5.5,-4.7,-3.5,-2.5),
       ilab=cbind(dat_1$HDL_M, dat_1$HDL_M_SD ,dat_1$HDL_F,dat_1$HDL_F_SD),
       cex=.75, header="Author(s) and years",slab = dat_HDL$study, xlab = "Hedge's g", showweights = TRUE,
       mlab="")
op <- par(cex=.75, font=2)
text(c(-5.5,-4.6,-3.5,-2.5),res_HDL$k+2, c("HDL_M", "HDL_SD", "HDL_M", "HDL_SD"))
text(c(-5.5,-3.5), res_HDL$b+10,c("Male", "Female"))
par(op)
### add text with Q-value, dfs, p-value, and I^2 statistic
text(-10, -0.5, pos=4, cex=0.75, bquote(paste(bold("RE Model"))))
text(-10, -1, pos=4, cex=0.77, bquote(paste(bold("Heterogeneity: "), "Q = ",
                                            .(formatC(res_HDL$QE, digits=2, format="f")), ", df = ", .(res_HDL$k -res_HDL$p),
                                            ", p = ", .(formatC(res_HDL$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res_HDL$I2, digits=1, format="f")), "%")))
### add text with test of theta
text(-10, -1.7, pos=4, cex=0.77 , bquote(paste(bold("Test for θ:"), "zval =",
                                               .(formatC(res_HDL$zval, digits=2, format="f")), 
                                               ", pval = ", .(formatC(res_HDL$pval, digits=2, format="f")))))

funnel(res_HDL, main="HDL",label=TRUE)

**#for TOTAL CHOLESTEROL**
dat_TC<-esc_mean_sd(grp1m = dat_1$TOT.CHOLESTEROL_M, grp1sd = dat_1$TOT.CHOLESTEROL_M_SD, grp1n = dat_1$MALE_No,
                    grp2m = dat_1$TOT.CHOLESTEROL_F, grp2sd = dat_1$TOT.CHOLESTEROL_F_SD, grp2n = dat_1$FEMALE_No,
                    es.type = "g", study = dat_1$Author.and.year)
res_TC<- rma(yi=dat_TC$es,vi=dat_TC$var, method="HE")
res_TC
forest(res_TC, at=log(c(.05, .25, 1, 4)), xlim=c(-10,6),
       ilab.xpos=c(-5.5,-4.7,-3.5,-2.5),
       ilab=cbind(dat_1$TOT.CHOLESTEROL_M, dat_1$TOT.CHOLESTEROL_M_SD ,dat_1$TOT.CHOLESTEROL_F,dat_1$TOT.CHOLESTEROL_F_SD),
       cex=.75, header="Author(s) and years",slab = dat_TC$study, xlab = "Hedge's g", showweights = TRUE,
       mlab="")
op <- par(cex=.75, font=2)
text(c(-5.5,-4.7,-3.5,-2.5),res_TC$k+2, c("TC_M", "TC_SD", "TC_M", "TC_SD"))
text(c(-5.5,-3.5), res_TC$b+11,c("Male", "Female"))
par(op)
### add text with Q-value, dfs, p-value, and I^2 statistic
text(-10, -0.5, pos=4, cex=0.75, bquote(paste(bold("RE Model"))))
text(-10, -1, pos=4, cex=0.77, bquote(paste(bold("Heterogeneity: "), "Q = ",
                                            .(formatC(res_TC$QE, digits=2, format="f")), ", df = ", .(res_TC$k - res_TC$p),
                                            ", p = ", .(formatC(res_TC$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res_TC$I2, digits=1, format="f")), "%")))
### add text with test of theta
text(-10, -1.7, pos=4, cex=0.77 , bquote(paste(bold("Test for θ:"), "zval =",
                                               .(formatC(res_TC$zval, digits=2, format="f")), 
                                               ", pval = ", .(formatC(res_TC$pval, digits=2, format="f")))))
funnel(res_TC, main="Total Cholestrol",label=TRUE)

**FOR TRIGLYCERIDE**
dat_TGs<-esc_mean_sd(grp1m = dat_1$TGs_M, grp1sd = dat_1$TGs_M_SD, grp1n = dat_1$MALE_No,
                     grp2m = dat_1$TGs_F, grp2sd = dat_1$TGs_F_SD, grp2n = dat_1$FEMALE_No,
                     es.type = "g", study = dat_1$Author.and.year)
res_TGs<- rma(yi =dat_TGs$es,vi =dat_TGs$var, method="HE")
res_TGs
forest(res_TGs, at=log(c(.05, .25, 1, 4)), xlim=c(-10,6),
       ilab.xpos=c(-5.5,-4.7,-3.5,-2.5),
       ilab=cbind(dat_1$TGs_M, dat_1$TGs_M_SD ,dat_1$TGs_F,dat_1$TGs_F_SD),
       cex=.75, header="Author(s) and years",slab = dat_TGs$study, xlab = "Hedge's g", showweights = TRUE,
       mlab="")
op <- par(cex=.75, font=2)
text(c(-5.5,-4.7,-3.5,-2.5),res_TGs$k+2, c("TG_M", "TG_SD", "TG_M", "TG_SD"))
text(c(-4.9,-3.7), res_TGs$b+8.5,c("Male", "Female"))
par(op)
### add text with Q-value, dfs, p-value, and I^2 statistic
text(-10, -0.5, pos=4, cex=0.75, bquote(paste(bold("RE Model"))))
text(-10, -1, pos=4, cex=0.77, bquote(paste(bold("Heterogeneity: "), "Q = ",
                                            .(formatC(res_TGs$QE, digits=2, format="f")), ", df = ", .(res_TGs$k -res_TGs$p),
                                            ", p = ", .(formatC(res_TGs$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res_TGs$I2, digits=1, format="f")), "%")))
### add text with test of theta
text(-10, -1.7, pos=4, cex=0.77 , bquote(paste(bold("Test for θ:"), "zval =",
                                               .(formatC(res_TGs$zval, digits=2, format="f")), 
                                               ", pval = ", .(formatC(res_TGs$pval, digits=2, format="f")))))
funnel(res_TGs, main="Triglycerides",label=TRUE)






