setwd("/Users/Marie-Elyse/Downloads")
NEW = read.csv("FEB2021.csv")

#Subset data according to who has at least one item present based on the updated (e.g. parecellated) number of items.
subsample_one_item_present1 <- NEW[which((rowMeans(is.na(NEW[c("CBCL48_sc6raw.x", "SDQ60_mother_hyperactivity","SDQ60_father_hyperactivity", "CBCL60_sc6raw.x","conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", "conners_teacher_adhd_score.72m", "PAPA_p4nadhd", "Dominic72_ADHD","SDQ72_mother_hyperactivity", "SDQ72_father_hyperactivity", "SDQ72_teacher_hyperactivity")]))) <= 13/14),]
dim(subsample_one_item_present1)

ADHDfactor.data1 <- subsample_one_item_present1[c("CBCL48_sc6raw.x", "SDQ60_mother_hyperactivity","SDQ60_father_hyperactivity", "CBCL60_sc6raw.x","conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", "conners_teacher_adhd_score.72m", "PAPA_p4nadhd", "Dominic72_ADHD","SDQ72_mother_hyperactivity", "SDQ72_father_hyperactivity", "SDQ72_teacher_hyperactivity")]
str(ADHDfactor.data1)

sink('bi_adhd_model_womother_withpapa.txt')
ADHD.model1 <- '
ADHD_womother_withpapa =~ SDQ60_father_hyperactivity + conners_father_adhd_score.60m + conners_father_adhd_score.72m + conners_teacher_adhd_score.72m + PAPA_p4nadhd + Dominic72_ADHD + SDQ72_father_hyperactivity + SDQ72_teacher_hyperactivity
Father_womother_withpapa =~ SDQ60_father_hyperactivity + conners_father_adhd_score.60m + conners_father_adhd_score.72m + SDQ72_father_hyperactivity 
Teacher_womother_withpapa =~ conners_teacher_adhd_score.72m + SDQ72_teacher_hyperactivity'

ADHD.fit1 <- lavaan::cfa(ADHD.model1, data = ADHDfactor.data1, estimator = "MLR", missing = "FIML", std.ov = TRUE, std.lv = TRUE, orthogonal = T, verbose = T)

o <- lavInspect(ADHD.fit1, what ='fit')
capture.output(o, file = "fit_indices_ADHD_model_fiml_ADHD_rater_womother_withpapa.txt")
s <- summary(ADHD.fit1, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_ADHD_model_fiml__ADHD_rater_womother_withpapa.txt")
sink()

sink('omega_adhd_model_womother_withpapa.txt')
semTools::reliability(ADHD.fit1, return.total = TRUE, dropSingle = TRUE,
                      omit.imps = c("no.conv", "no.se"))
lavaan::lavInspect(ADHD.fit1, "cov.lv")
sink()

sink('bi_adhd_model_womotherquestionnaires_womother_wopapa.txt')
ADHD.model2 <- '
ADHD_womother =~ SDQ60_father_hyperactivity + conners_father_adhd_score.60m + conners_father_adhd_score.72m + conners_teacher_adhd_score.72m + Dominic72_ADHD + SDQ72_father_hyperactivity + SDQ72_teacher_hyperactivity
Father_womother =~ SDQ60_father_hyperactivity + conners_father_adhd_score.60m + conners_father_adhd_score.72m + SDQ72_father_hyperactivity 
Teacher_womother =~ conners_teacher_adhd_score.72m + SDQ72_teacher_hyperactivity'

ADHD.fit2 <- lavaan::cfa(ADHD.model2, data = ADHDfactor.data1, estimator = "MLR", missing = "FIML", std.ov = TRUE, std.lv = TRUE, orthogonal = T, verbose = T)

o <- lavInspect(ADHD.fit2, what ='fit')
capture.output(o, file = "fit_indices_ADHD_model_fiml_ADHD_rater_womother.txt")
s <- summary(ADHD.fit2, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_ADHD_model_fiml_ADHD_rater_womother.txt")
sink()

sink('omega_adhd_model_womother.txt')
semTools::reliability(ADHD.fit2, return.total = TRUE, dropSingle = TRUE,
                      omit.imps = c("no.conv", "no.se"))
lavaan::lavInspect(ADHD.fit2, "cov.lv")
sink()

ADHD_scores_womother_wpapa <- lavaan::lavPredict(ADHD.fit1)
ADHD_scores_womother <- lavaan::lavPredict(ADHD.fit2)

NEW2 <- cbind(NEW, ADHDfactor.data1)

NEW3 <- cbind(NEW2, ADHD_scores_womother_wpapa)

NEW4 <- cbind(NEW3, ADHD_scores_womother)

readr::write_csv(NEW4, file = "/Users/Marie-Elyse/Downloads/womothertest.csv")

NEW = read.csv("womothertest.csv")

NEW$mean_centered_ADHD_womother = c(scale(NEW$ADHD_womother))
NEW$mean_centered_ADHD_womother_withpapa = c(scale(NEW$ADHD_womother_withpapa))
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
NEW$mean_centered_conners_mother_hyperactivity_score.72m = c(scale(NEW$conners_mother_hyperactivity_score.72m)) 

sink('model_womother.txt')
fit1 <-lm(mean_centered_ADHD_womother_withpapa ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd*gender_male + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + Hamilton, data=NEW)
print(summary(fit1))
summ(fit1, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)

simple_slopes(fit1)
modelEffectSizes(fit1)
options(max.print=999999)
simple_slopes(fit1, levels=list(auc_post_cesd=c(0:54), Pren_CESD=c(0:54), gender_male=c(0, 1, 'sstest')))

fit2 <-lm(mean_centered_ADHD_womother ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd*gender_male + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + Hamilton, data=NEW)
print(summary(fit2))
summ(fit2, robust = "HC1", confint = TRUE, digits = 2, vifs = TRUE)

simple_slopes(fit2)
modelEffectSizes(fit2)
options(max.print=999999)
simple_slopes(fit2, levels=list(auc_post_cesd=c(0:54), Pren_CESD=c(0:54), gender_male=c(0, 1, 'sstest')))

jtools::export_summs(fit1, fit2,
                     error_format = "[{conf.low}, {conf.high}]", statistics = c(N = "nobs", R2 = "r.squared", "logLik", "df", "AIC",  "BIC"))


sink()

