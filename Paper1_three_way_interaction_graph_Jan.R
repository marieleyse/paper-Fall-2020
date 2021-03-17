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

#readr::write_csv(NEW, "/Users/Marie-Elyse/Downloads/MAVAN_SEP_PAPER_2020.csv")

options("jtools-digits" = 2)  

fit4 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd*gender_male + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + Hamilton, data=NEW)

png(filename = 'interaction_pre_points.png', width=1000, height=400)
P=interact_plot(fit4, pred = mean_centered_Pren_CESD, modx = gender_male, mod2 = mean_centered_auc_post_cesd, interval = TRUE, plot.points = FALSE,colors = c("orange", "blue"),
                x.label = "Prenatal depression",
                y.label = "ADHD",
                pred.labels = "Prenatal depression",
                modx.labels = c("Girls", "Boys"),
                mod2.labels = c("i) Low postnatal depression","ii) Average postnatal depression", "iii) High postnatal depression"),
                main.title = NULL,
                legend.main = "Sex")
P + 
  drop_gridlines() +
  # facet_grid(cond ~ mod2) +
  geom_vline(aes(xintercept=-0.06),   
             color="black", linetype="dotted", size=1) +
  geom_vline(aes(xintercept= 1.74),   
           color="black", linetype="dotted", size=1) +
  # geom_vline(aes(xintercept=(mean_centered_Pren_CESD==1.74)),   
  #            color="red", linetype="dashed", size=1) + 
  geom_rect(xmin=-0.06, xmax=1.74, ymin=-2, ymax=4,fill = "gray", colour = NA, alpha=0.02)
dev.off()


#######################

png(filename = 'interaction_pre_wopoints.png', width=1000, height=400)
P=interact_plot(fit4, pred = mean_centered_Pren_CESD, modx = gender_male, mod2 = mean_centered_auc_post_cesd, interval = TRUE, plot.points = FALSE,colors = c("blue", "orange"),
                x.label = "Prenatal depression",
                y.label = "ADHD",
                pred.labels = "Prenatal depression",
                modx.labels = c("Girls"=0,"Boys"=1),
                mod2.labels = c("A) Low postnatal depression","B) Average postnatal depression", "C) High postnatal depression"),
                main.title = NULL,
                legend.main = "Sex")
P + 
  drop_gridlines() +
  # facet_grid(cond ~ mod2) +
  geom_vline(aes(xintercept=-0.06),   
             color="black", linetype="dotted", size=1) +
  geom_vline(aes(xintercept= 1.74),   
             color="black", linetype="dotted", size=1) +
  # geom_vline(aes(xintercept=(mean_centered_Pren_CESD==1.74)),   
  #            color="red", linetype="dashed", size=1) + 
  geom_rect(xmin=-0.06, xmax=1.74, ymin=-2, ymax=4,fill = "gray", colour = NA, alpha=0.02)
dev.off()

#######################
#######################

png(filename = 'interaction_plot_post_auc_cesd_test_points_BY_SEX_SWITCH.png', width=1000, height=400)
P = interact_plot(fit4, pred = mean_centered_auc_post_cesd, modx = mean_centered_Pren_CESD, mod2 = gender_male, interval = TRUE, plot.points = TRUE,colors = c("blue", "orange"),
                  x.label = "Postnatal depression",
                  y.label = "ADHD",
                  pred.labels = "Postnatal depression",
                  mod2.labels = c("Girls","Boys"),
                  modx.labels = c(" Low "," Average", " High"),
                  main.title = NULL,
                  legend.main = "Prenatal depression")
P + 
  drop_gridlines() +
  # facet_grid(cond ~ mod2) +
  geom_vline(aes(xintercept=-0.06),   
             color="black", linetype="dotted", size=1) +
  geom_vline(aes(xintercept= 1.74),   
             color="black", linetype="dotted", size=1) +
  # geom_vline(aes(xintercept=(mean_centered_Pren_CESD==1.74)),   
  #            color="red", linetype="dashed", size=1) + 
  geom_rect(xmin=-0.06, xmax=1.74, ymin=-2, ymax=4,fill = "gray", colour = NA, alpha=0.02)
dev.off()

####################### 

png(filename = 'interaction_plot_points_BY_SEX.png', width=1000, height=400)
P = interact_plot(fit4, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, mod2 = gender_male, interval = TRUE, plot.points = TRUE,colors = c("blue", "orange"),
                  x.label = "Prenatal depression",
                  y.label = "ADHD",
                  pred.labels = "Prenatal depression",
                  mod2.labels = c("Girls","Boys"),
                  modx.labels = c(" Low "," Average", " High"),
                  main.title = NULL,
                  legend.main = "Postnatal depression")
P + 
  drop_gridlines() +
  # facet_grid(cond ~ mod2) +
  geom_vline(aes(xintercept=-0.06),   
             color="black", linetype="dotted", size=1) +
  geom_vline(aes(xintercept= 1.74),   
             color="black", linetype="dotted", size=1) +
  # geom_vline(aes(xintercept=(mean_centered_Pren_CESD==1.74)),   
  #            color="red", linetype="dashed", size=1) + 
  geom_rect(xmin=-0.06, xmax=1.74, ymin=-2, ymax=4,fill = "gray", colour = NA, alpha=0.02)
dev.off()

#######################

png(filename = 'interaction_plot_post_points.png', width=1000, height=400)
P=interact_plot(fit4, pred = mean_centered_auc_post_cesd,modx = gender_male, mod2 = mean_centered_Pren_CESD, interval = TRUE, plot.points = TRUE, colors = c("blue", "orange"),
              x.label = "Postnatal depression",
              y.label = "ADHD",
              pred.labels = "Postnatal depression",
              mod2.labels = c("A) Low prenatal depression ","B) Average prenatal depression", "C) High prenatal depression"),
              modx.labels = c("Girls"=0,"Boys"=1),
              main.title = NULL,
              legend.main = "Sex")
P + 
  drop_gridlines() +
  # facet_grid(cond ~ mod2) +
  geom_vline(aes(xintercept=-0.06),   
             color="black", linetype="dotted", size=1) +
  geom_vline(aes(xintercept= 1.74),   
             color="black", linetype="dotted", size=1) +
  # geom_vline(aes(xintercept=(mean_centered_Pren_CESD==1.74)),   
  #            color="red", linetype="dashed", size=1) + 
  geom_rect(xmin=-0.06, xmax=1.74, ymin=-2, ymax=4,fill = "gray", colour = NA, alpha=0.02)
dev.off()

#######################

png(filename = 'interaction_plot_post_wopoints.png', width=1000, height=400)
P=interact_plot(fit4, pred = mean_centered_auc_post_cesd,modx = gender_male, mod2 = mean_centered_Pren_CESD, interval = TRUE, plot.points = FALSE, colors = c("blue", "orange"),
              x.label = "Postnatal depression",
              y.label = "ADHD",
              pred.labels = "Postnatal depression",
              mod2.labels = c("A) Low prenatal depression ","B) Average prenatal depression", "C) High prenatal depression"),
              modx.labels = c("Girls"=0,"Boys"=1),
              main.title = NULL,
              legend.main = "Sex")
P + 
  drop_gridlines() +
  # facet_grid(cond ~ mod2) +
  geom_vline(aes(xintercept=-0.06),   
             color="black", linetype="dotted", size=1) +
  geom_vline(aes(xintercept= 1.74),   
             color="black", linetype="dotted", size=1) +
  # geom_vline(aes(xintercept=(mean_centered_Pren_CESD==1.74)),   
  #            color="red", linetype="dashed", size=1) + 
  geom_rect(xmin=-0.06, xmax=1.74, ymin=-2, ymax=4,fill = "gray", colour = NA, alpha=0.02)
dev.off()

#######################
#######################

# png(filename = 'interaction_plot_post_auc_cesd_test2_wopoints.png', width=1000, height=400)
# interact_plot(fit4, pred = mean_centered_Pren_CESD, modx = gender_male, mod2 = mean_centered_auc_post_cesd, interval = TRUE, plot.points = FALSE, colors ="Rainbow",theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
# dev.off()

# P +
#   drop_gridlines() +
#   geom_vline(mean_centered_auc_post_cesd = c(-0.95, -0.95), xintercept=c(-0.06, 1.74), linetype='dashed', size=1) +
#   geom_hline(mean_centered_auc_post_cesd = c(-0.95, -0.95), yintercept=c(-0.14, 1.33), linetype='solid', size=1)
# dev.off()
# 
# data_vline <- DF(
#   xintercept = c(-0.06, 1.74),
#   mean_centered_auc_post_cesd = c(-0.95, -0.95)
# )
# data_hline <- DF(
#   yintercept = c(-0.14, 1.33),
#   mean_centered_auc_post_cesd = c(-0.95, -0.95)
# )
# P +
#   drop_gridlines() +
#   geom_vline(data = data_vline, aes(xintercept = xintercept), linetype='dashed', size=1) +
#   geom_hline(data = data_hline, aes(yintercept = yintercept), linetype='solid', size=1) +
#   facet_wrap(~mean_centered_auc_post_cesd)
# dev.off()
# 
#   geom_vline(data = data_vline, aes(xintercept = xintercept), linetype='dashed', size=1) +
#   geom_hline(data = data_hline, aes(yintercept = yintercept), linetype='solid', size=1)
#   facet_wrap(~mean_centered_auc_post_cesd)
#   
#   geom_vline(xintercept = c(-0.06, 1.74), mod2 = c(-0.95, -0.95), aes(xintercept = xintercept), linetype='dashed', size=1) 
#   
# 
# data_vline <- DF(
#   xintercept = c(-0.06, 1.74),
#   mean_centered_auc_post_cesd = c(-0.95, -0.95)
# )
# data_hline <- DF(
#   yintercept = c(-0.14, 1.33),
#   mean_centered_auc_post_cesd = c(-0.95, -0.95)
# )
# 
# P +
#   drop_gridlines() +
#   geom_vline(xintercept=c(-0.06, 1.74), linetype='dashed', size=1) +
#   geom_hline(yintercept=c(-0.14, 1.33), linetype='solid', size=1)
# dev.off()
# 
# 
# 
# P + geom_vline(xintercept=c(-0.06, 1.74), linetype='dashed', size=.1)
# P + drop_gridlines()
# 
# mod2.values="minus", 
# 
# P + geom_vline(xintercept=c(-0.06, 1.74), linetype='dashed', size=.1)
# 
# mod2.values = "terciles",
# mod2.values("-1 SD"),
# mod2.values("-1"), xintercept=c(-0.06, 1.74)
# mod2_group=c("Low M","High M"), xintercept=c(1,3)
# + geom_vline(data=test,aes(xintercept=xintercept))
# 
# test <- data.frame(mod2_group=1,xintercept=c(-1.24))
# P + geom_vline(data=test,aes(xintercept=xintercept))
# 
# + geom_line()
# + drop_gridlines()
# + geom_vline(xintercept = -1.24)
# # p + geom_vline(xintercept = -1.24)
# # p + geom_vline(xintercept = 3.74)
# # p  

# P + geom_vline(xintercept=c(-0.06, 1.74))

# 
# geom_vline(xintercept=c(-0.06, 1.74), linetype='solid', size=1) + 
#   geom_hline(yintercept=c(-0.14, 1.33), linetype='dashed', size=1) 

