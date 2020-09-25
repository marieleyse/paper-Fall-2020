#Marie-Elyse Lafaille-Magnan, PhD
#marie-elyse.lafaille-magnan@mail.mcgill.ca

setwd("/Users/Marie-Elyse/Downloads")
NEW = read.csv("MAVAN_48M_and_up_jun2020_new.csv")

#CALLED MEAN_CENTERED BUT ZSCORE

NEW$mean_centered_ADHD = c(scale(NEW$ADHD))
NEW$mean_centered_PRS_0_001_adhd_child = c(scale(NEW$PRS_0_001_adhd_child))
NEW$mean_centered_auc_post_cesd = c(scale(NEW$auc_post_cesd))
NEW$mean_centered_Pren_CESD = c(scale(NEW$Pren_CESD))
NEW$mean_centered_PC1 = c(scale(NEW$PC1))
NEW$mean_centered_PC2 = c(scale(NEW$PC2)) 
NEW$mean_centered_PC3 = c(scale(NEW$PC3)) 
NEW$mean_centered_mom_age_birth = c(scale(NEW$mom_age_birth))
NEW$mean_centered_Smoking_During_Pregnancy = c(scale(NEW$Smoking_During_Pregnancy)) 
NEW$mean_centered_Pren_income4 = c(scale(NEW$Pren_income4)) 
NEW$mean_centered_conners_mother_hyperactivity_score.72m = c(scale(NEW$conners_mother_hyperactivity_score.72m)) 
NEW$gender_male <- to_factor(NEW$gender_male)

options("jtools-digits" = 2)                

sink('fit_model_4_3_way_simple_slope.txt')
fit4 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd*gender_male + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + Hamilton, data=NEW)

sim_slopes(fit4, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, mod2 = gender_male)
sim_slopes(fit4, pred = mean_centered_auc_post_cesd, modx = mean_centered_Pren_CESD, mod2 = gender_male)
simple_slopes(fit4)

sink()

sink('fit_model_4_3_way_simple_slope_rawdata.txt')
fit4 <-lm(ADHD ~ Pren_CESD*auc_post_cesd*gender_male + PRS_0_001_adhd_child + PC1 + PC2 + PC3 + mom_age_birth + above_college + Hamilton, data=NEW)

sim_slopes(fit4, pred = Pren_CESD, modx = auc_post_cesd, mod2 = gender_male)
sim_slopes(fit4, pred = auc_post_cesd, modx = Pren_CESD, mod2 = gender_male)
simple_slopes(fit4)
modelEffectSizes(fit4)
options(max.print=999999)
simple_slopes(fit4, levels=list(auc_post_cesd=c(0:54), Pren_CESD=c(0:54), gender_male=c(0, 1, 'sstest')))
simpleSlope(fit4, pred = Pren_CESD, modx = auc_post_cesd, mod2 = gender_male)
sink()

fit4 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd*gender_male + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + Hamilton, data=NEW)

#png(filename = 'nj_plot.png', width=1000, height=400)
pdf("nj_plot.pdf", width=10, height=4)
sim_slopes(fit4, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, mod2 = gender_male, jnplot = TRUE,
           mod2.labels = c("Girls","Boys"),
           modx.labels = "Postnatal depression",
           y.labels = "Slope of Prenatal depression",
           main.title = "Johnson-Neyman plot")
dev.off()


#png(filename = 'nj_plot_version2.png', width=1000, height=400)
pdf("nj_plot_version2.pdf", width=10, height=4)
sim_slopes(fit4, pred = mean_centered_auc_post_cesd, modx = mean_centered_Pren_CESD, mod2 = gender_male, jnplot = TRUE,
           mod2.labels = c("Girls","Boys"),
           modx.labels = "Postnatal depression",
           y.labels = "Slope of Prenatal depression",
           main.title = "Johnson-Neyman plot")
dev.off()