#Marie-Elyse Lafaille-Magnan, PhD
#marie-elyse.lafaille-magnan@mail.mcgill.ca

setwd("/Users/Marie-Elyse/Downloads")
NEW = read.csv("MAVAN_DATA_APR2020_fix_wo_nan.csv")

names(NEW)[1] <- "ID"

########################################

#Subset data according to who has at least one item present based on the updated (e.g. parecellated) number of items.
subsample_one_item_present <- NEW[which((rowMeans(is.na(NEW[c("CBCL48_sc6raw", "SDQ60_mother_hyperactivity","SDQ60_father_hyperactivity", "CBCL60_sc6raw","conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", "conners_teacher_adhd_score.72m", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD","SDQ72_mother_hyperactivity", "SDQ72_father_hyperactivity", "SDQ72_teacher_hyperactivity")]))) <= 14/15),]
dim(subsample_one_item_present)

#Save file
write.csv(subsample_one_item_present, file = "mom_ADHD_factor_one_item_present.csv", row.names = FALSE) 

ADHDfactor.data <- subsample_one_item_present[c("CBCL48_sc6raw", "SDQ60_mother_hyperactivity","SDQ60_father_hyperactivity", "CBCL60_sc6raw","conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", "conners_teacher_adhd_score.72m", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD","SDQ72_mother_hyperactivity", "SDQ72_father_hyperactivity", "SDQ72_teacher_hyperactivity")]
str(ADHDfactor.data)

#sex_gender_group

#Subset data according to who has at least one item present based on the updated (e.g. parecellated) number of items.
subsample_two_item_present <- NEW[which((rowMeans(is.na(NEW[c("gender_male", "CBCL48_sc6raw", "SDQ60_mother_hyperactivity","SDQ60_father_hyperactivity", "CBCL60_sc6raw","conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", "conners_teacher_adhd_score.72m", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD","SDQ72_mother_hyperactivity", "SDQ72_father_hyperactivity", "SDQ72_teacher_hyperactivity")]))) <= 14/16),]
dim(subsample_two_item_present)

#Save file.
write.csv(subsample_two_item_present, file = "mom_ADHD_factor_two_item_present_sex.csv", row.names = FALSE) 

ADHDfactor.data2 <- subsample_two_item_present[c("gender_male","CBCL48_sc6raw", "SDQ60_mother_hyperactivity","SDQ60_father_hyperactivity", "CBCL60_sc6raw","conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", "conners_teacher_adhd_score.72m", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD","SDQ72_mother_hyperactivity", "SDQ72_father_hyperactivity", "SDQ72_teacher_hyperactivity")]
str(ADHDfactor.data2)

###########
#simple#
###########

sink('basic_adhd_model.txt')
ADHD.model <- '
ADHD =~ CBCL48_sc6raw + SDQ60_mother_hyperactivity + SDQ60_father_hyperactivity + CBCL60_sc6raw + conners_mother_adhd_score.60m + conners_father_adhd_score.60m + conners_mother_adhd_score.72m + conners_father_adhd_score.72m + conners_teacher_adhd_score.72m + PAPA_p4nadhd + Dominic72_ADHD + SDQ72_mother_hyperactivity + SDQ72_father_hyperactivity + SDQ72_teacher_hyperactivity'

ADHD.fit <- cfa(ADHD.model, data = ADHDfactor.data, estimator = "MLR", missing = "FIML", std.ov = TRUE, std.lv = TRUE, orthogonal = T, verbose = T)

o <- lavInspect(ADHD.fit, what ='fit')
capture.output(o, file = "fit_indices_ADHD_model_fiml_onefactor.txt")
s <- summary(ADHD.fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_ADHD_model_fiml_onefactor.txt")
sink()

###########
#bi
###########

sink('bi_adhd_model.txt')
ADHD.model <- '
ADHD =~ CBCL48_sc6raw + SDQ60_mother_hyperactivity + SDQ60_father_hyperactivity + CBCL60_sc6raw + conners_mother_adhd_score.60m + conners_father_adhd_score.60m + conners_mother_adhd_score.72m + conners_father_adhd_score.72m + conners_teacher_adhd_score.72m + PAPA_p4nadhd + Dominic72_ADHD + SDQ72_mother_hyperactivity + SDQ72_father_hyperactivity + SDQ72_teacher_hyperactivity
Mother =~ CBCL48_sc6raw + SDQ60_mother_hyperactivity + CBCL60_sc6raw + conners_mother_adhd_score.60m + conners_mother_adhd_score.72m + PAPA_p4nadhd + SDQ72_mother_hyperactivity
Father =~ SDQ60_father_hyperactivity + conners_father_adhd_score.60m + conners_father_adhd_score.72m + SDQ72_father_hyperactivity 
Teacher =~ conners_teacher_adhd_score.72m + SDQ72_teacher_hyperactivity'

ADHD.fit <- cfa(ADHD.model, data = ADHDfactor.data, estimator = "MLR", missing = "FIML", std.ov = TRUE, std.lv = TRUE, orthogonal = T, verbose = T)

o <- lavInspect(ADHD.fit, what ='fit')
capture.output(o, file = "fit_indices_ADHD_model_fiml_ADHD_rater.txt")
s <- summary(ADHD.fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_ADHD_model_fiml__ADHD_rater.txt")
sink()

#########do this by gender 
sink('bi_adhd_model_sex.txt')
ADHD.model2 <- '
ADHD =~ CBCL48_sc6raw + SDQ60_mother_hyperactivity + SDQ60_father_hyperactivity + CBCL60_sc6raw + conners_mother_adhd_score.60m + conners_father_adhd_score.60m + conners_mother_adhd_score.72m + conners_father_adhd_score.72m + conners_teacher_adhd_score.72m + PAPA_p4nadhd + Dominic72_ADHD + SDQ72_mother_hyperactivity + SDQ72_father_hyperactivity + SDQ72_teacher_hyperactivity
Mother =~ CBCL48_sc6raw + SDQ60_mother_hyperactivity + CBCL60_sc6raw + conners_mother_adhd_score.60m + conners_mother_adhd_score.72m + PAPA_p4nadhd + SDQ72_mother_hyperactivity
Father =~ SDQ60_father_hyperactivity + conners_father_adhd_score.60m + conners_father_adhd_score.72m + SDQ72_father_hyperactivity 
Teacher =~ conners_teacher_adhd_score.72m + SDQ72_teacher_hyperactivity'

ADHD.fit2 <- cfa(ADHD.model2, data = ADHDfactor.data2, estimator = "MLR", missing = "FIML", std.ov = TRUE, std.lv = TRUE, orthogonal = T, verbose = T)

o <- lavInspect(ADHD.fit2, what ='fit')
capture.output(o, file = "fit_indices_ADHD_model_fiml_ADHD_rater_sex.txt")
s <- summary(ADHD.fit2, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_ADHD_model_fiml_ADHD_rater_sex.txt")

measurementInvariance(model= ADHD.fit2, 
                      data = ADHDfactor.data2,
                      estimator = "MLR", 
                      missing = "FIML", 
                      std.ov = TRUE, 
                      std.lv = TRUE, 
                      orthogonal = T, 
                      verbose = T,  
                      group = "gender_male")

measurement_invariance <- lavTestLRT(ADHD.fit2)
capture.output(measurement_invariance, file = "model_differences_ADHD_model_fiml_ADHD_rater_sex.txt")

fit <- cfa(ADHD.fit2, 
           data = ADHDfactor.data2, 
           estimator = "MLR", 
           missing = "FIML", 
           std.ov = TRUE, 
           std.lv = TRUE, 
           orthogonal = T, 
           verbose = T , 
           group = "gender_male")
summary(fit)

#did not converge
sink()


reliability(ADHD.fit)

#Extract adhd-factor scores.
ADHD_scores <- predict(ADHD.fit) ###what to do how to impute missing data some scores are of 0 I'm guess because there is no info so I think I need to remove those
# You could later get scores for imputed datasets:
# for (i in 1:Nimp){
#   list_of_imputed_datasets[[i]] <- cbind(list_of_imputed_datasets[[i]], predict(ADHD.fit, newdata = list_of_imputed_datasets[[i]]))
# }
#make sure same variable names as model and implist
#ignore error message Warning message:
#In lav_object_post_check(object) :
 # lavaan WARNING: some estimated ov variances are negative

# Removing zeros (which are in fact NA)
for (i in 1:NCOL(ADHD_scores)){
  index_to_remove = ADHD_scores[,i]==0
  ADHD_scores[index_to_remove,i] = NA
}

#Attach factor scores to maternal psychopathology data
NEW2 <- cbind(ADHDfactor.data, ADHD_scores)
NEW = read.csv("MAVAN_DATA_APR2020_fix_wo_nan.csv")

NEW9 <- cbind(NEW, NEW2)

write.csv(NEW9, file = "ADHD_factorscores_MAVAN_fix_wo_nan2.csv", row.names = FALSE)

png(filename = 'adhd_factors_vs_old_adhd_factor.png')

mydata.cor = cor(NEW9[, c("ADHD", "Mother",	"Father",	"Teacher")],  method ="kendall",  use = "pairwise")
res1 <- cor.mtest(NEW9[, c("ADHD",	"Mother",	"Father",	"Teacher")], conf.level = .95, method ="kendall",  alternative = "two.sided", exact=FALSE, use = "pairwise")

corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

dev.off()

corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "lower",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "lower", order="FPC",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper", order="FPC",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

