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
# NEW$gender_male <- to_factor(NEW$gender_male)
# NEW$above_college <- to_factor(NEW$above_college)
# NEW$Hamilton <- to_factor(NEW$Hamilton)
NEW$mean_centered_Smoking_During_Pregnancy = c(scale(NEW$Smoking_During_Pregnancy)) 
NEW$mean_centered_Pren_income4 = c(scale(NEW$Pren_income4)) 
NEW$mean_centered_conners_mother_hyperactivity_score.72m = c(scale(NEW$conners_mother_hyperactivity_score.72m)) 

fit5 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd*gender_male + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + Hamilton, data=NEW)

pdf("nj_plot.pdf", width=10, height=4)
png(filename = 'nj_plot.png', width=1000, height=400)
sim_slopes(fit5, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, mod2 = gender_male, jnplot = TRUE,
           mod2.labels = c("Girls","Boys"),
           modx.labels = "Postnatal depression",
           y.labels = "Slope of Prenatal depression",
           main.title = "Johnson-Neyman plot")
dev.off()

png(filename = 'probe_interaction_plot.png', width=1000, height=400)
probe_interaction(fit5, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, mod2 = gender_male)
dev.off()

png(filename = 'interaction_plot_datapoints.png', width=1000, height=400)
interact_plot(fit5, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, mod2 = gender_male, interval = TRUE, plot.points = TRUE)
dev.off()

png(filename = 'interaction_plot_nopoints_nointervals.png', width=1000, height=400)
pdf("interaction_plot_nopoints_nointervals.pdf", width=10, height=4)
P = interact_plot(fit5, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, mod2 = gender_male, interval = FALSE, plot.points = FALSE, 
                  x.label = "Prenatal depression",
                  y.label = "ADHD",
                  pred.labels = "Prenatal depression",
                  modx.labels = c("i) Low","ii) Moderate", "iii) High"),
                  mod2.labels = c("Girls","Boys"),
                  legend.main = "Postnatal depression")  
P + 
  drop_gridlines() +
  xlim(-1.5, 3) +
  geom_vline(aes(xintercept=-0.06), color= c("black"), 
             linetype= c("dotted"), size=1) +
  geom_vline(aes(xintercept=1.74), color= c("black"), 
           linetype= c("dotted"), size=1) 

dev.off()

png(filename = 'interaction_plot_nopoints_nointervals_novlines.png', width=1000, height=400)
pdf("interaction_plot_nopoints_nointervals_novlines.pdf", width=10, height=4)
P = interact_plot(fit5, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, mod2 = gender_male, interval = FALSE, plot.points = FALSE, 
                  x.label = "Prenatal depression",
                  y.label = "ADHD",
                  pred.labels = "Prenatal depression",
                  modx.labels = c("i) Low","ii) Moderate", "iii) High"),
                  mod2.labels = c("Girls","Boys"),
                  legend.main = "Postnatal depression")  
P + 
  drop_gridlines() +
  xlim(-1.5, 3) 
dev.off()


#                   theme(plot.title = element_text(hjust = 0.5))
# boys <- subset(NEW, gender_male == 1) 
#subset(gender_male==1), 
P2 <- P + 
         drop_gridlines() +
         xlim(-1, 3)
plot(P2)
# boys= subset(NEW, gender_male==1) 

P3 <- P2 +
      geom_vline(aes(xintercept=-0.06), color= c("black"), 
      linetype= c("dotted"), size=1) 
plot(P3)
  #           +
  # geom_vline(aes(xintercept= 1.74),   
  #            color="black", linetype="dotted", size=1) 
dev.off()

png(filename = 'interaction_pre_wopoints.png', width=1000, height=400)
pdf("interaction_pre_wopoints.pdf", width=10, height=4)
fit5 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd*gender_male + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + Hamilton, data=NEW)
P=interact_plot(fit5, pred = mean_centered_Pren_CESD, modx = gender_male, mod2 = mean_centered_auc_post_cesd, interval = TRUE, plot.points = FALSE,colors = c("orange", "blue"),
                x.label = "Prenatal depression",
                y.label = "ADHD",
                pred.labels = "Prenatal depression",
                modx.labels = c("Girls","Boys"),
                mod2.labels = c("i) Low","ii) Moderate", "iii) High"),
                main.title = "Postnatal depression",
                legend.main = "Sex")+
  theme(plot.title = element_text(hjust = 0.5))

P + 
  drop_gridlines() +
  geom_vline(aes(xintercept=-0.06),   
             color="black", linetype="dotted", size=1) +
  geom_vline(aes(xintercept= 1.74),   
             color="black", linetype="dotted", size=1) 
dev.off()

pdf("nj_plot2.pdf", width=10, height=4)
sim_slopes(fit5, pred = mean_centered_auc_post_cesd, modx = mean_centered_Pren_CESD, mod2 = gender_male, jnplot = TRUE,
           mod2.labels = c("Girls","Boys"),
           modx.labels = "Prenatal depression",
           y.labels = "Slope of Postnatal depression",
           main.title = "Johnson-Neyman plot")
dev.off()

png(filename = 'probe_interaction_plot.png', width=1000, height=400)
probe_interaction(fit4, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, mod2 = gender_male)
dev.off()

png(filename = 'interaction_plot_datapoints.png', width=1000, height=400)
interact_plot(fit4, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, mod2 = gender_male, interval = TRUE, plot.points = TRUE)
dev.off()

png(filename = 'interaction_plot_nopoints_nointervals.png', width=1000, height=400)
interact_plot(fit4, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, mod2 = gender_male, interval = FALSE, plot.points = FALSE)
dev.off()

png(filename = 'interaction_plot_nopoints_nointervalsb.png', width=1000, height=400)
pdf("interaction_plot_nopoints_nointervalsb.pdf", width=10, height=4)
P = interact_plot(fit5, pred = mean_centered_auc_post_cesd, modx= mean_centered_Pren_CESD, mod2 = gender_male, interval = FALSE, plot.points = FALSE, 
                  x.label = "Postnatal depression",
                  y.label = "ADHD",
                  pred.labels = "Postnatal depression",
                  modx.labels = c("i) Low","ii) Moderate", "iii) High"),
                  mod2.labels = c("Girls","Boys"),
                  legend.main = "Prenatal depression")  
P + 
  drop_gridlines() +
  xlim(-1.5, 3) +
  geom_vline(aes(xintercept=-0.14), color= c("black"), 
             linetype= c("dotted"), size=1) +
  geom_vline(aes(xintercept=1.33), color= c("black"), 
             linetype= c("dotted"), size=1) 

dev.off()

 

png(filename = 'interaction_plot_nopoints_nointervals_novlinesb.png', width=1000, height=400)
pdf("interaction_plot_nopoints_nointervals_novlinesb.pdf", width=10, height=4)
P = interact_plot(fit5, pred = mean_centered_auc_post_cesd, modx= mean_centered_Pren_CESD, mod2 = gender_male, interval = FALSE, plot.points = FALSE, 
                  x.label = "Postnatal depression",
                  y.label = "ADHD",
                  pred.labels = "Postnatal depression",
                  modx.labels = c("i) Low","ii) Moderate", "iii) High"),
                  mod2.labels = c("Girls","Boys"),
                  legend.main = "Prenatal depression")  
P + 
  drop_gridlines() +
  xlim(-1.5, 3) 
dev.off()



png(filename = 'interaction_plot_post_wopoints.png', width=1000, height=400)
P=interact_plot(fit5, pred = mean_centered_auc_post_cesd,modx = gender_male, mod2 = mean_centered_Pren_CESD, interval = TRUE, plot.points = FALSE, colors = c("orange", "blue"),
                x.label = "Postnatal depression",
                y.label = "ADHD",
                pred.labels = "Postnatal depression",
                mod2.labels = c("i) Low","ii) Moderate", "iii) High"),
                modx.labels = c("Girls","Boys"),
                main.title = "Prenatal depression",
                legend.main = "Sex")+
  theme(plot.title = element_text(hjust = 0.5))
P + 
  drop_gridlines() +
  # facet_grid(cond ~ mod2) +
  geom_vline(aes(xintercept=-0.14),   
             color="black", linetype="dotted", size=1) +
  geom_vline(aes(xintercept=1.33),   
             color="black", linetype="dotted", size=1) 
dev.off()


