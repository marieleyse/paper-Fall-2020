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
NEW$gender_male <- to_factor(NEW$gender_male)
NEW$above_college <- to_factor(NEW$above_college)
NEW$Hamilton <- to_factor(NEW$Hamilton)
NEW$mean_centered_Smoking_During_Pregnancy = c(scale(NEW$Smoking_During_Pregnancy)) 
NEW$mean_centered_Pren_income4 = c(scale(NEW$Pren_income4)) 
NEW$mean_centered_conners_mother_hyperactivity_score.72m = c(scale(NEW$conners_mother_hyperactivity_score.72m)) 

pdf("interaction_plot_pre_post.pdf", width=10, height=4)
fit4 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + gender_male + Hamilton, data=NEW)
P = interact_plot(fit4, pred = mean_centered_auc_post_cesd, modx = mean_centered_Pren_CESD, interval = TRUE, plot.points = FALSE,colors = c("blue", "orange"),
                  x.label = "Postnatal depression",
                  y.label = "ADHD",
                  modx.labels = c(" Low "," Moderate", " High"),
                  pred.labels = "Postnatal depression",
                  main.title = NULL,
                  legend.main = "Prenatal depression")
P + 
  drop_gridlines() 
dev.off()

# pdf("interaction_plot_pre_post.pdf", width=10, height=4)
# par(mfrow=c(1,3)) # 
# fit4 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + gender_male + Hamilton, data=NEW)
# 
# equation1=function(x){coef(fit)[2]*x+coef(fit)[1]}
# equation2=function(x){coef(fit)[2]*x+coef(fit)[1]+coef(fit)[3]}
# 
# ggplot(NEW,aes(mean_centered_ADHD,x=mean_centered_auc_post_cesd,color=gender_male))+geom_point()+
#  facet_wrap(vars(mean_centered_Pren_CESD), ncol = 3) +
#   stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
#   stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])


pdf("interaction_plot_pre_post2.pdf", width=10, height=4)
fit4 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + gender_male + Hamilton, data=NEW)
P = interact_plot(fit4, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, interval = TRUE, plot.points = FALSE,colors = c("blue", "orange"),
                  x.label = "Prenatal depression",
                  y.label = "ADHD",
                  modx.labels = c(" Low "," Moderate", " High"),
                  pred.labels = "Prenatal depression",
                  main.title = NULL,
                  legend.main = "Postnatal depression")
P + 
  drop_gridlines() 
dev.off()

pdf("interaction_plot_pre_post_point.pdf", width=10, height=4)
fit4 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + gender_male + Hamilton, data=NEW)
P = interact_plot(fit4, pred = mean_centered_auc_post_cesd, modx = mean_centered_Pren_CESD, interval = TRUE, plot.points = TRUE,colors = c("blue", "orange"),
                  x.label = "Postnatal depression",
                  y.label = "ADHD",
                  modx.labels = c(" Low "," Moderate", " High"),
                  pred.labels = "Postnatal depression",
                  main.title = NULL,
                  legend.main = "Prenatal depression")
P + 
  drop_gridlines() 
dev.off()

pdf("interaction_plot_pre_post2_point.pdf", width=10, height=4)
fit4 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + gender_male + Hamilton, data=NEW)
P = interact_plot(fit4, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, interval = TRUE, plot.points = TRUE,colors = c("blue", "orange"),
                  x.label = "Prenatal depression",
                  y.label = "ADHD",
                  modx.labels = c(" Low "," Moderate", " High"),
                  pred.labels = "Prenatal depression",
                  main.title = NULL,
                  legend.main = "Postnatal depression")
P + 
  drop_gridlines() 
dev.off()


#readr::write_csv(NEW, "/Users/Marie-Elyse/Downloads/MAVAN_SEP_PAPER_2020.csv")

options("jtools-digits" = 2)  

fit4 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd*gender_male + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + Hamilton, data=NEW)
#png(filename = 'interaction_pre_points.png', width=1000, height=400)
pdf("interaction_pre_points.pdf", width=10, height=4)
P=interact_plot(fit4, pred = mean_centered_Pren_CESD, modx = gender_male, mod2 = mean_centered_auc_post_cesd, interval = TRUE, plot.points = FALSE,colors = c("blue", "orange"),
                x.label = "Prenatal depression",
                y.label = "ADHD",
                pred.labels = "Prenatal depression",
                modx.labels = c("Girls","Boys"),
                mod2.labels = c(" Low "," Moderate", " High"),
                main.title = NULL,
                legend.main = "Postnatal depression")
P + 
  drop_gridlines()
dev.off()

#######################

#png(filename = 'interaction_pre_wopoints.png', width=1000, height=400)
pdf("interaction_pre_wopoints.pdf", width=10, height=4)

P=interact_plot(fit4, pred = mean_centered_Pren_CESD, modx = gender_male, mod2 = mean_centered_auc_post_cesd, interval = TRUE, plot.points = FALSE,colors = c("blue", "orange"),
                x.label = "Prenatal depression",
                y.label = "ADHD",
                pred.labels = "Prenatal depression",
                modx.labels = c("Girls","Boys"),
                mod2.labels = c(" Low "," Moderate", " High"),
                main.title = NULL,
                legend.main = "Postnatal depression")
P + 
  drop_gridlines()
dev.off()

#######################
#######################
#png(filename = 'interaction_plot_post_auc_cesd_test_points_BY_SEX_SWITCH.png', width=1000, height=400)
pdf("interaction_plot_post_auc_cesd_test_points_BY_SEX_SWITCH.pdf", width=10, height=4)
P = interact_plot(fit4, pred = mean_centered_auc_post_cesd, modx = mean_centered_Pren_CESD, mod2 = gender_male, interval = TRUE, plot.points = TRUE,colors = c("blue", "orange"),
                  x.label = "Postnatal depression",
                  y.label = "ADHD",
                  pred.labels = "Postnatal depression",
                  mod2.labels = c("Girls","Boys"),
                  modx.labels = c(" Low "," Moderate", " High"),
                  main.title = NULL,
                  legend.main = "Prenatal depression")
P + 
  drop_gridlines() 
dev.off()

####################### 

#png(filename = 'interaction_plot_points_BY_SEX.png', width=1000, height=400)
pdf("interaction_plot_points_BY_SEX.pdf", width=10, height=4)
P = interact_plot(fit4, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, mod2 = gender_male, interval = TRUE, plot.points = TRUE,colors = c("blue", "orange"),
                  x.label = "Prenatal depression",
                  y.label = "ADHD",
                  pred.labels = "Prenatal depression",
                  mod2.labels = c("Girls","Boys"),
                  modx.labels = c(" Low "," Moderate", " High"),
                  main.title = NULL,
                  legend.main = "Postnatal depression")
P + 
  drop_gridlines() 
dev.off()

#######################

#png(filename = 'interaction_plot_post_points.png', width=1000, height=400)
pdf("interaction_plot_post_points.pdf", width=10, height=4)
P=interact_plot(fit4, pred = mean_centered_auc_post_cesd,modx = gender_male, mod2 = mean_centered_Pren_CESD, interval = TRUE, plot.points = TRUE, colors = c("blue", "orange"),
              x.label = "Postnatal depression",
              y.label = "ADHD",
              pred.labels = "Postnatal depression",
              mod2.labels = c(" Low "," Moderate", " High"),
              modx.labels = c("Girls","Boys"),
              main.title = NULL,
              legend.main = "Sex")
P + 
  drop_gridlines() 
dev.off()

#######################

#png(filename = 'interaction_plot_post_wopoints.png', width=1000, height=400)
pdf("interaction_plot_post_wopoints.pdf", width=10, height=4)
P=interact_plot(fit4, pred = mean_centered_auc_post_cesd,modx = gender_male, mod2 = mean_centered_Pren_CESD, interval = TRUE, plot.points = FALSE, colors = c("blue", "orange"),
              x.label = "Postnatal depression",
              y.label = "ADHD",
              pred.labels = "Postnatal depression",
              mod2.labels = c(" Low "," Moderate", " High"),
              modx.labels = c("Girls","Boys"),
              main.title = NULL,
              legend.main = "Sex")
P + 
  drop_gridlines() 
dev.off()



