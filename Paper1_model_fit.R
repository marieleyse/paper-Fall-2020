#Marie-Elyse Lafaille-Magnan, PhD
#marie-elyse.lafaille-magnan@mail.mcgill.ca

setwd("/Users/Marie-Elyse/Downloads")
NEW = read.csv("MAVAN_48M_and_up_jun2020_new.csv")

#REFERED TO MEAN_CENTERED BUT ZSCORE

# NEW$gender_male <- to_factor(NEW$gender_male)
# NEW$above_college <- to_factor(NEW$above_college)
# NEW$Hamilton <- to_factor(NEW$Hamilton)

options("jtools-digits" = 4)  

sink('fit_model_scale_prs_0_001_adhd_child_auc_post_cesd_summary.txt')

fit1 <-lm(scale(ADHD) ~ scale(PRS_0_001_adhd_child) + scale(auc_post_cesd) + scale(PC1) + scale(PC2) + scale(PC3) + scale(mom_age_birth) + above_college +  scale(Pren_CESD) + gender_male + Hamilton, data=NEW)
fit2 <-lm(scale(ADHD) ~ scale(Pren_CESD)*gender_male + scale(PRS_0_001_adhd_child) + scale(PC1) + scale(PC2) + scale(PC3) + scale(mom_age_birth) + above_college + scale(auc_post_cesd) + Hamilton, data=NEW)
fit3 <-lm(scale(ADHD) ~ scale(auc_post_cesd)*gender_male + scale(PRS_0_001_adhd_child) + scale(PC1) + scale(PC2) + scale(PC3) + scale(mom_age_birth) + above_college + scale(Pren_CESD) + Hamilton, data=NEW)
fit4 <-lm(scale(ADHD) ~ scale(Pren_CESD)*scale(auc_post_cesd)*gender_male + scale(PRS_0_001_adhd_child) + scale(PC1) + scale(PC2) + scale(PC3) + scale(mom_age_birth) + above_college + Hamilton, data=NEW)

lmtest::bptest(fit1) #studentized Breusch-Pagan test
lmtest::bptest(fit2) #studentized Breusch-Pagan test
lmtest::bptest(fit3) #studentized Breusch-Pagan test
lmtest::bptest(fit4) #studentized Breusch-Pagan test

print(summary(fit1))
print(summary(fit2))
print(summary(fit3))
print(summary(fit4))

summ(fit1, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)
summ(fit2, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)
summ(fit3, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)
summ(fit4, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)

# print("coefficient covariance matrix")
# vcov(fit4)
# print("heteroscedasticity-corrected asymptotic covariance matrix")
# hccm(fit4, type="hc0")

sink()

sink('table_model_prs_auc_post_cesd_summary.txt')

jtools::export_summs(fit1, fit2, fit3, fit4, 
                     error_format = "[{conf.low}, {conf.high}]", statistics = c(N = "nobs", R2 = "r.squared", "logLik", "df", "AIC",  "BIC"))
#robust=FALSE
# chisq=(-2*-309.6764)-(-2*-316.1413)
# pchisq(chisq, df = 3, lower.tail=FALSE)
# print(pschisq)        
# jtools::export_summs(fit1, fit2, fit3, fit4, statistics = all)

sink()

sink('fit_model_scale_auc_post_cesd_summary_noprs.txt')

fit1 <-lm(scale(ADHD) ~ scale(auc_post_cesd) + scale(mom_age_birth) + above_college +  scale(Pren_CESD) + gender_male + Hamilton, data=NEW)
fit2 <-lm(scale(ADHD) ~ scale(Pren_CESD)*gender_male + scale(mom_age_birth) + above_college + scale(auc_post_cesd) + Hamilton, data=NEW)
fit3 <-lm(scale(ADHD) ~ scale(auc_post_cesd)*gender_male + scale(mom_age_birth) + above_college + scale(Pren_CESD) + Hamilton, data=NEW)
fit4 <-lm(scale(ADHD) ~ scale(Pren_CESD)*scale(auc_post_cesd)*gender_male + scale(mom_age_birth) + above_college + Hamilton, data=NEW)

lmtest::bptest(fit1) #studentized Breusch-Pagan test
lmtest::bptest(fit2) #studentized Breusch-Pagan test
lmtest::bptest(fit3) #studentized Breusch-Pagan test
lmtest::bptest(fit4) #studentized Breusch-Pagan test

print(summary(fit1))
print(summary(fit2))
print(summary(fit3))
print(summary(fit4))

summ(fit1, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)
summ(fit2, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)
summ(fit3, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)
summ(fit4, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)

# print("coefficient covariance matrix")
# vcov(fit4)
# print("heteroscedasticity-corrected asymptotic covariance matrix")
# hccm(fit4, type="hc0")
# sink()

sink('table_model_prs_auc_post_cesd_summary_noprs.txt')
jtools::export_summs(fit1, fit2, fit3, fit4, 
                     error_format = "[{conf.low}, {conf.high}]", statistics = c(N = "nobs", R2 = "r.squared", "logLik", "df", "AIC",  "BIC"))
sink()

sink('Conners_fit_model_scale_prs_0_001_child_auc_post_cesd_summary.txt')

fit1 <-lm(scale(conners_mother_hyperactivity_score.72m) ~ scale(PRS_0_001_adhd_child) + scale(auc_post_cesd) + scale(PC1) + scale(PC2) + scale(PC3) + scale(mom_age_birth) + above_college +  scale(Pren_CESD) + gender_male + Hamilton, data=NEW)
fit2 <-lm(scale(conners_mother_hyperactivity_score.72m) ~ scale(Pren_CESD)*gender_male + scale(PRS_0_001_adhd_child) + scale(PC1) + scale(PC2) + scale(PC3) + scale(mom_age_birth) + above_college + scale(auc_post_cesd) + Hamilton, data=NEW)
fit3 <-lm(scale(conners_mother_hyperactivity_score.72m) ~ scale(auc_post_cesd)*gender_male + scale(PRS_0_001_adhd_child) + scale(PC1) + scale(PC2) + scale(PC3) + scale(mom_age_birth) + above_college + scale(Pren_CESD) + Hamilton, data=NEW)
fit4 <-lm(scale(conners_mother_hyperactivity_score.72m) ~ scale(Pren_CESD)*scale(auc_post_cesd)*gender_male + scale(PRS_0_001_adhd_child) + scale(PC1) + scale(PC2) + scale(PC3) + scale(mom_age_birth) + above_college + Hamilton, data=NEW)

lmtest::bptest(fit1) #studentized Breusch-Pagan test
lmtest::bptest(fit2) #studentized Breusch-Pagan test
lmtest::bptest(fit3) #studentized Breusch-Pagan test
lmtest::bptest(fit4) #studentized Breusch-Pagan test

print(summary(fit1))
print(summary(fit2))
print(summary(fit3))
print(summary(fit4))

summ(fit1, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)
summ(fit2, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)
summ(fit3, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)
summ(fit4, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)

# print("coefficient covariance matrix")
# vcov(fit4)
# print("heteroscedasticity-corrected asymptotic covariance matrix")
# hccm(fit4, type="hc0")

sink()

sink('table_model_Conners_prs_auc_post_cesd_summary.txt')
jtools::export_summs(fit1, fit2, fit3, fit4, 
                     error_format = "[{conf.low}, {conf.high}]", statistics = c(N = "nobs", R2 = "r.squared", "logLik", "df", "AIC",  "BIC"))
sink()


sink('fit_model_conners_scale_auc_post_cesd_summary_noprs.txt')

fit1 <-lm(scale(conners_mother_hyperactivity_score.72m) ~ scale(auc_post_cesd) + scale(mom_age_birth) + above_college +  scale(Pren_CESD) + gender_male + Hamilton, data=NEW)
fit2 <-lm(scale(conners_mother_hyperactivity_score.72m) ~ scale(Pren_CESD)*gender_male + scale(mom_age_birth) + above_college + scale(auc_post_cesd) + Hamilton, data=NEW)
fit3 <-lm(scale(conners_mother_hyperactivity_score.72m) ~ scale(auc_post_cesd)*gender_male + scale(mom_age_birth) + above_college + scale(Pren_CESD) + Hamilton, data=NEW)
fit4 <-lm(scale(conners_mother_hyperactivity_score.72m) ~ scale(Pren_CESD)*scale(auc_post_cesd)*gender_male + scale(mom_age_birth) + above_college + Hamilton, data=NEW)

lmtest::bptest(fit1) #studentized Breusch-Pagan test
lmtest::bptest(fit2) #studentized Breusch-Pagan test
lmtest::bptest(fit3) #studentized Breusch-Pagan test
lmtest::bptest(fit4) #studentized Breusch-Pagan test

print(summary(fit1))
print(summary(fit2))
print(summary(fit3))
print(summary(fit4))

summ(fit1, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)
summ(fit2, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)
summ(fit3, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)
summ(fit4, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)

# print("coefficient covariance matrix")
# vcov(fit4)
# print("heteroscedasticity-corrected asymptotic covariance matrix")
# hccm(fit4, type="hc0")

sink()

sink('table_model_conners_prs_auc_post_cesd_summary_noprs.txt')
jtools::export_summs(fit1, fit2, fit3, fit4, 
                     error_format = "[{conf.low}, {conf.high}]", statistics = c(N = "nobs", R2 = "r.squared", "logLik", "df", "AIC",  "BIC"))
sink()

