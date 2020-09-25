#Marie-Elyse Lafaille-Magnan, PhD
#marie-elyse.lafaille-magnan@mail.mcgill.ca

setwd("/Users/Marie-Elyse/Downloads")
NEW = read.csv("MAVAN_48M_and_up_jun2020_new.csv")

#CALLED MEAN_CENTERED BUT ZSCORE

# NEW$gender_male <- to_factor(NEW$gender_male)
# NEW$above_college <- to_factor(NEW$above_college)
# NEW$Hamilton <- to_factor(NEW$Hamilton)

fit1 <-lm(scale(ADHD) ~ scale(PRS_0_001_adhd_child) + scale(auc_post_cesd) + scale(PC1) + scale(PC2) + scale(PC3) + scale(mom_age_birth) + above_college +  scale(Pren_CESD) + gender_male + Hamilton, data=NEW)
fit2 <-lm(scale(ADHD) ~ scale(Pren_CESD)*gender_male + scale(PRS_0_001_adhd_child) + scale(PC1) + scale(PC2) + scale(PC3) + scale(mom_age_birth) + above_college + scale(auc_post_cesd) + Hamilton, data=NEW)
fit3 <-lm(scale(ADHD) ~ scale(auc_post_cesd)*gender_male + scale(PRS_0_001_adhd_child) + scale(PC1) + scale(PC2) + scale(PC3) + scale(mom_age_birth) + above_college + scale(Pren_CESD) + Hamilton, data=NEW)
fit4 <-lm(scale(ADHD) ~ scale(Pren_CESD)*scale(auc_post_cesd)*gender_male + scale(PRS_0_001_adhd_child) + scale(PC1) + scale(PC2) + scale(PC3) + scale(mom_age_birth) + above_college + Hamilton, data=NEW)

#png(filename = 'forest_plot_model_prs_auc_post_cesd_summary.png', width=1000, height=400)
pdf("forest_plot_model_prs_auc_post_cesd_summary.pdf", width=10, height=4, compress = FALSE)

coef_names <- c("PC3" = "scale(PC3)","PC2" = "scale(PC2)","PC1" = "scale(PC1)",  "PRS child ADHD" = "scale(PRS_0_001_adhd_child)", "Maternal age" = "scale(mom_age_birth)", "Education" = "above_college", "Site" = "Hamilton","Pre X Post depression X Sex" = "scale(Pren_CESD):scale(auc_post_cesd):gender_male", "Pre X Post depression" = "scale(Pren_CESD):scale(auc_post_cesd)", "Post depression X Sex" = "scale(auc_post_cesd):gender_male", "Pre depression X Sex" = "scale(Pren_CESD):gender_male", "Sex" = "gender_male","Postnatal depression" = "scale(auc_post_cesd)", "Prenanal depression" = "scale(Pren_CESD)",  "Constant" = "(Intercept)")

jtools::plot_summs(fit1, fit2, fit3, fit4,  plot.distributions = TRUE, models.names=c("1", "2", "3", "4"), coefs = coef_names)

dev.off()

#################################

fit1 <-lm(scale(ADHD) ~ scale(auc_post_cesd) + scale(mom_age_birth) + above_college +  scale(Pren_CESD) + gender_male + Hamilton, data=NEW)
fit2 <-lm(scale(ADHD) ~ scale(Pren_CESD)*gender_male + scale(mom_age_birth) + above_college + scale(auc_post_cesd) + Hamilton, data=NEW)
fit3 <-lm(scale(ADHD) ~ scale(auc_post_cesd)*gender_male + scale(mom_age_birth) + above_college + scale(Pren_CESD) + Hamilton, data=NEW)
fit4 <-lm(scale(ADHD) ~ scale(Pren_CESD)*scale(auc_post_cesd)*gender_male + scale(mom_age_birth) + above_college + Hamilton, data=NEW)

#png(filename = 'forest_plot_model_auc_post_cesd_summary_noprs.png', width=1000, height=400)
pdf("forest_plot_model_auc_post_cesd_summary_noprs.pdf", width=10, height=4, compress = FALSE)

# jtools::plot_summs(fit1, fit2, fit3, fit4,  plot.distributions = TRUE, models.names=c("1", "2", "3", "4"))
# dev.off()

coef_names <- c("Maternal age" = "scale(mom_age_birth)", "Education" = "above_college", "Site" = "Hamilton","Pre X Post depression X Sex" = "scale(Pren_CESD):scale(auc_post_cesd):gender_male", "Pre X Post depression" = "scale(Pren_CESD):scale(auc_post_cesd)", "Post depression X Sex" = "scale(auc_post_cesd):gender_male", "Pre depression X Sex" = "scale(Pren_CESD):gender_male", "Sex" = "gender_male","Postnatal depression" = "scale(auc_post_cesd)", "Prenanal depression" = "scale(Pren_CESD)",  "Constant" = "(Intercept)")

jtools::plot_summs(fit1, fit2, fit3, fit4,  plot.distributions = TRUE, models.names=c("1", "2", "3", "4"), coefs = coef_names)
dev.off()

#################################
#################################
#################################

fit1 <-lm(scale(conners_mother_hyperactivity_score.72m) ~ scale(PRS_0_001_adhd_child) + scale(auc_post_cesd) + scale(PC1) + scale(PC2) + scale(PC3) + scale(mom_age_birth) + above_college +  scale(Pren_CESD) + gender_male + Hamilton, data=NEW)
fit2 <-lm(scale(conners_mother_hyperactivity_score.72m) ~ scale(Pren_CESD)*gender_male + scale(PRS_0_001_adhd_child) + scale(PC1) + scale(PC2) + scale(PC3) + scale(mom_age_birth) + above_college + scale(auc_post_cesd) + Hamilton, data=NEW)
fit3 <-lm(scale(conners_mother_hyperactivity_score.72m) ~ scale(auc_post_cesd)*gender_male + scale(PRS_0_001_adhd_child) + scale(PC1) + scale(PC2) + scale(PC3) + scale(mom_age_birth) + above_college + scale(Pren_CESD) + Hamilton, data=NEW)
fit4 <-lm(scale(conners_mother_hyperactivity_score.72m) ~ scale(Pren_CESD)*scale(auc_post_cesd)*gender_male + scale(PRS_0_001_adhd_child) + scale(PC1) + scale(PC2) + scale(PC3) + scale(mom_age_birth) + above_college + Hamilton, data=NEW)

#png(filename = 'forest_plot_Conners_model_prs_auc_post_cesd_summary.png', width=1000, height=400)
pdf("forest_plot_Conners_model_prs_auc_post_cesd_summary.pdf", width=10, height=4, compress = FALSE)

coef_names <- c("PC3" = "scale(PC3)","PC2" = "scale(PC2)","PC1" = "scale(PC1)",  "PRS child ADHD" = "scale(PRS_0_001_adhd_child)", "Maternal age" = "scale(mom_age_birth)", "Education" = "above_college", "Site" = "Hamilton","Pre X Post depression X Sex" = "scale(Pren_CESD):scale(auc_post_cesd):gender_male", "Pre X Post depression" = "scale(Pren_CESD):scale(auc_post_cesd)", "Post depression X Sex" = "scale(auc_post_cesd):gender_male", "Pre depression X Sex" = "scale(Pren_CESD):gender_male", "Sex" = "gender_male","Postnatal depression" = "scale(auc_post_cesd)", "Prenanal depression" = "scale(Pren_CESD)",  "Constant" = "(Intercept)")

jtools::plot_summs(fit1, fit2, fit3, fit4,  plot.distributions = TRUE, models.names=c("1", "2", "3", "4"), coefs = coef_names)
dev.off()

#################################

fit1 <-lm(scale(conners_mother_hyperactivity_score.72m) ~ scale(auc_post_cesd) + scale(mom_age_birth) + above_college +  scale(Pren_CESD) + gender_male + Hamilton, data=NEW)
fit2 <-lm(scale(conners_mother_hyperactivity_score.72m) ~ scale(Pren_CESD)*gender_male + scale(mom_age_birth) + above_college + scale(auc_post_cesd) + Hamilton, data=NEW)
fit3 <-lm(scale(conners_mother_hyperactivity_score.72m) ~ scale(auc_post_cesd)*gender_male + scale(mom_age_birth) + above_college + scale(Pren_CESD) + Hamilton, data=NEW)
fit4 <-lm(scale(conners_mother_hyperactivity_score.72m) ~ scale(Pren_CESD)*scale(auc_post_cesd)*gender_male + scale(mom_age_birth) + above_college + Hamilton, data=NEW)

#png(filename = 'forest_plot_Conners_model_prs_auc_post_cesd_summary_noprs.png', width=1000, height=400)
pdf("forest_plot_Conners_model_prs_auc_post_cesd_summary_noprs.png", width=10, height=4, compress = FALSE)

coef_names <- c("Maternal age" = "scale(mom_age_birth)", "Education" = "above_college", "Site" = "Hamilton","Pre X Post depression X Sex" = "scale(Pren_CESD):scale(auc_post_cesd):gender_male", "Pre X Post depression" = "scale(Pren_CESD):scale(auc_post_cesd)", "Post depression X Sex" = "scale(auc_post_cesd):gender_male", "Pre depression X Sex" = "scale(Pren_CESD):gender_male", "Sex" = "gender_male","Postnatal depression" = "scale(auc_post_cesd)", "Prenanal depression" = "scale(Pren_CESD)",  "Constant" = "(Intercept)")

jtools::plot_summs(fit1, fit2, fit3, fit4,  plot.distributions = TRUE, models.names=c("1", "2", "3", "4"), coefs = coef_names)
dev.off()

#################################

#print(fit4$coefficients)
# coef_names <- coef_names[1:3]
#"Pre X Post depression X Sex" = "scale(Pren_CESD):scale(auc_post_cesd):gender_male", "Pre X Post depression" = "scale(Pren_CESD):scale(auc_post_cesd)", "Post depression X Sex" = "scale(auc_post_cesd):gender_male", "Pre depression X Sex" = "scale(Pren_CESD):gender_male",
# coef_names <- c("Pre X Post depression X Sex" = "scale(Pren_CESD):scale(auc_post_cesd):gender_male", "Pre X Post depression" = "scale(Pren_CESD):scale(auc_post_cesd)", "Post depression X Sex" = "scale(auc_post_cesd):gender_male", "Pre depression X Sex" = "scale(Pren_CESD):gender_male","Prenanal depression" = "scale(Pren_CESD)", "Postnatal depression" = "scale(auc_post_cesd)","Sex" = "gender_male", "Maternal age" = "scale(mom_age_birth)", "Site" = "Hamilton", "Education" = "above_college", "PC1" = "scale(PC1)","PC2" = "scale(PC2)","PC3" = "scale(PC3)")
#coef_names <- c("Pre X Post depression X Sex" = "scale(Pren_CESD):scale(auc_post_cesd):gender_male", "Pre X Post depression" = "scale(Pren_CESD):scale(auc_post_cesd)", "Post depression X Sex" = "scale(auc_post_cesd):gender_male", "Pre depression X Sex" = "scale(Pren_CESD):gender_male", "PC3" = "scale(PC3)","PC2" = "scale(PC2)","PC1" = "scale(PC1)",  "Maternal age" = "scale(mom_age_birth)", "Education" = "above_college", "Site" = "Hamilton","Sex" = "gender_male","Postnatal depression" = "scale(auc_post_cesd)", "Prenanal depression" = "scale(Pren_CESD)", "PRS child ADHD" = "scale(PRS_0_001_adhd_child)",  "Constant" = "(Intercept)")
#coef_names <- c("Pre X Post depression X Sex" = "scale(Pren_CESD):scale(auc_post_cesd):gender_male", "Pre X Post depression" = "scale(Pren_CESD):scale(auc_post_cesd)", "Post depression X Sex" = "scale(auc_post_cesd):gender_male", "Pre depression X Sex" = "scale(Pren_CESD):gender_male", "PC3" = "scale(PC3)","PC2" = "scale(PC2)","PC1" = "scale(PC1)", "Site" = "Hamilton", "Education" = "above_college",  "Maternal age" = "scale(mom_age_birth)","Sex" = "gender_male","Postnatal depression" = "scale(auc_post_cesd)", "Prenanal depression" = "scale(Pren_CESD)", "Constant" = "(Intercept)")
