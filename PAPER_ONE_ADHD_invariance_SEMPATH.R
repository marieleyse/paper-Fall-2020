#Marie-Elyse Lafaille-Magnan, PhD
#marie-elyse.lafaille-magnan@mail.mcgill.ca

setwd("/Users/Marie-Elyse/Downloads")
NEW <- read.csv("MAVAN_48M_and_up_jun2020.csv")

#Subset data according to who has at least one item present based on the updated (e.g. parecellated) number of items.
subsample_one_item_present <- NEW[which((rowMeans(is.na(NEW[c("CBCL48_sc6raw.y", "SDQ60_mother_hyperactivity","SDQ60_father_hyperactivity", "CBCL60_sc6raw.y","conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", "conners_teacher_adhd_score.72m", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD","SDQ72_mother_hyperactivity", "SDQ72_father_hyperactivity", "SDQ72_teacher_hyperactivity")]))) <= 14/15),]
dim(subsample_one_item_present)

ADHDfactor.data <- subsample_one_item_present[c("CBCL48_sc6raw.y", "SDQ60_mother_hyperactivity","SDQ60_father_hyperactivity", "CBCL60_sc6raw.y","conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", "conners_teacher_adhd_score.72m", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD","SDQ72_mother_hyperactivity", "SDQ72_father_hyperactivity", "SDQ72_teacher_hyperactivity")]
str(ADHDfactor.data)

sink('simple_adhd_model.txt')
ADHD.model <- '
ADHD =~ CBCL48_sc6raw.y + SDQ60_mother_hyperactivity + SDQ60_father_hyperactivity + CBCL60_sc6raw.y + conners_mother_adhd_score.60m + conners_father_adhd_score.60m + conners_mother_adhd_score.72m + conners_father_adhd_score.72m + conners_teacher_adhd_score.72m + PAPA_p4nadhd + Dominic72_ADHD + SDQ72_mother_hyperactivity + SDQ72_father_hyperactivity + SDQ72_teacher_hyperactivity'

ADHD.fit <- cfa(ADHD.model, data = ADHDfactor.data, estimator = "MLR", missing = "FIML", std.ov = TRUE, std.lv = TRUE, orthogonal = T, verbose = T)

o <- lavInspect(ADHD.fit, what='fit')
capture.output(o, file = "simplecfafit_indices_ADHD_model_fiml_ADHD_rater.txt")
s <- summary(ADHD.fit, standardized = T, fit.measures = T)
capture.output(s, file = "simplecfamodel_output_ADHD_model_fiml__ADHD_rater.txt")
sink()

sink('bi_adhd_model.txt')
ADHD.model <- '
ADHD =~ CBCL48_sc6raw.y + SDQ60_mother_hyperactivity + SDQ60_father_hyperactivity + CBCL60_sc6raw.y + conners_mother_adhd_score.60m + conners_father_adhd_score.60m + conners_mother_adhd_score.72m + conners_father_adhd_score.72m + conners_teacher_adhd_score.72m + PAPA_p4nadhd + Dominic72_ADHD + SDQ72_mother_hyperactivity + SDQ72_father_hyperactivity + SDQ72_teacher_hyperactivity
Mother =~ CBCL48_sc6raw.y + SDQ60_mother_hyperactivity + CBCL60_sc6raw.y + conners_mother_adhd_score.60m + conners_mother_adhd_score.72m + PAPA_p4nadhd + SDQ72_mother_hyperactivity
Father =~ SDQ60_father_hyperactivity + conners_father_adhd_score.60m + conners_father_adhd_score.72m + SDQ72_father_hyperactivity 
Teacher =~ conners_teacher_adhd_score.72m + SDQ72_teacher_hyperactivity'

ADHD.fit <- cfa(ADHD.model, data = ADHDfactor.data, estimator = "MLR", missing = "FIML", std.ov = TRUE, std.lv = TRUE, orthogonal = T, verbose = T)

o <- lavInspect(ADHD.fit, what ='fit')
capture.output(o, file = "fit_indices_ADHD_model_fiml_ADHD_rater.txt")
s <- summary(ADHD.fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_ADHD_model_fiml_ADHD_rater.txt")
sink()

semPaths(ADHD.fit, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)
semPaths(ADHD.fit, "std", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)


#png(filename = 'adhd_latent_factor.png', width=1000, height=400)
pdf("adhd_latent_factor.pdf", width=10, height=4, compress = FALSE)
# cols <- wes_palette(name = "Zissou1", n = 5, type = "discrete")
# colorlist <- list(man = cols[2], lat = cols[5])
cols <- wes_palette(name = "FantasticFox1", n = 5, type = "discrete")
colorlist <- list(man = cols[3], lat = cols[4])
semPaths(ADHD.fit, what = "model", bifactor = "ADHD", whatLabels = "std", layout = "tree2",  
         color = colorlist, nCharNodes = 7, sizeMan = 7, sizeMan2 = 4,rotation=4,
         shapeMan = "rectangle", intercepts =FALSE, residuals=FALSE, curve=0.5, fade = F, curvePivot = TRUE, arrows = 1,
         edge.label.cex=0.5, edge.label.position=0.75, exoCov = FALSE, nodeLabels =c("Dominic 72"," CBCL 48  ", "  SDQ 60  ", " CBCL 60  ", "Conners 60", "Conners 72", " PAPA 72  ",
                                                                                     "  SDQ 72  ", "  SDQ 60  ", "Conners 60", "Conners 72", "  SDQ 72  ", "Conners 72", "  SDQ 72  ", "ADHD", "Mother", "Father", "Teacher"))
dev.off()

nodeLabels =c("Dominic","CBCL 48", "SDQ 60", "CBCL 60", "Conners 60", "Conners 72", "PAPA",
              "SDQ 72", "SDQ 60", "Conners 60", "Conners 72", "SDQ 72", "Conners 72", "SDQ 72", "ADHD", "Mother", "Father", "Teacher")

NEW <- read.csv("MAVAN_48M_and_up_jun2020.csv")
NEW <- filter(NEW,gender_male == "0")

#Subset data according to who has at least one item present based on the updated (e.g. parecellated) number of items.
subsample_one_item_present <- NEW[which((rowMeans(is.na(NEW[c("CBCL48_sc6raw.y", "SDQ60_mother_hyperactivity","SDQ60_father_hyperactivity", "CBCL60_sc6raw.y","conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", "conners_teacher_adhd_score.72m", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD","SDQ72_mother_hyperactivity", "SDQ72_father_hyperactivity", "SDQ72_teacher_hyperactivity")]))) <= 14/15),]
dim(subsample_one_item_present)

ADHDfactor.data <- subsample_one_item_present[c("CBCL48_sc6raw.y", "SDQ60_mother_hyperactivity","SDQ60_father_hyperactivity", "CBCL60_sc6raw.y","conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", "conners_teacher_adhd_score.72m", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD","SDQ72_mother_hyperactivity", "SDQ72_father_hyperactivity", "SDQ72_teacher_hyperactivity")]
str(ADHDfactor.data)

sink('bi_adhd_model_girl.txt')
ADHD.model <- '
ADHD =~ CBCL48_sc6raw.y + SDQ60_mother_hyperactivity + SDQ60_father_hyperactivity + CBCL60_sc6raw.y + conners_mother_adhd_score.60m + conners_father_adhd_score.60m + conners_mother_adhd_score.72m + conners_father_adhd_score.72m + conners_teacher_adhd_score.72m + PAPA_p4nadhd + Dominic72_ADHD + SDQ72_mother_hyperactivity + SDQ72_father_hyperactivity + SDQ72_teacher_hyperactivity
Mother =~ CBCL48_sc6raw.y + SDQ60_mother_hyperactivity + CBCL60_sc6raw.y + conners_mother_adhd_score.60m + conners_mother_adhd_score.72m + PAPA_p4nadhd + SDQ72_mother_hyperactivity
Father =~ SDQ60_father_hyperactivity + conners_father_adhd_score.60m + conners_father_adhd_score.72m + SDQ72_father_hyperactivity 
Teacher =~ conners_teacher_adhd_score.72m + SDQ72_teacher_hyperactivity'

ADHD.fit <- cfa(ADHD.model, data = ADHDfactor.data, estimator = "MLR", missing = "FIML", std.ov = TRUE, std.lv = TRUE, orthogonal = T, verbose = T)

#Warning messages:
#1: In lav_model_vcov(lavmodel = lavmodel2, lavsamplestats = lavsamplestats,  :
                     #   lavaan WARNING:
                     #   The variance-covariance matrix of the estimated parameters (vcov)
                     # does not appear to be positive definite! The smallest eigenvalue
                     # (= -1.831710e-07) is smaller than zero. This may be a symptom that
                     # the model is not identified.
                     # 2: In lav_object_post_check(object) :
                     #   lavaan WARNING: some estimated ov variances are negative

o <- lavInspect(ADHD.fit, what ='fit')
capture.output(o, file = "fit_indices_ADHD_model_fiml_ADHD_rater_girl.txt")
s <- summary(ADHD.fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_ADHD_model_fiml__ADHD_rater_girl.txt")
sink()

# png(filename = 'adhd_latent_factor_girls.png', width=1000, height=400)
pdf("adhd_latent_factor_girls.pdf", width=10, height=4, compress = FALSE)
# cols <- wes_palette(name = "Zissou1", n = 5, type = "discrete")
# colorlist <- list(man = cols[2], lat = cols[5])
cols <- wes_palette(name = "FantasticFox1", n = 5, type = "discrete")
colorlist <- list(man = cols[3], lat = cols[4])
semPaths(ADHD.fit, what = "model", bifactor = "ADHD", whatLabels = "std", layout = "tree2",  
         color = colorlist, nCharNodes = 7, sizeMan = 7, sizeMan2 = 4,rotation=2,
         shapeMan = "rectangle", intercepts =FALSE, residuals=FALSE, curve=0.5, fade = F, curvePivot = TRUE, arrows = 1,
         edge.label.cex=0.5, edge.label.position=0.75, exoCov = FALSE, nodeLabels =c("Dominic 72"," CBCL 48  ", "  SDQ 60  ", " CBCL 60  ", "Conners 60", "Conners 72", " PAPA 72  ",
                                                                                     "  SDQ 72  ", "  SDQ 60  ", "Conners 60", "Conners 72", "  SDQ 72  ", "Conners 72", "  SDQ 72  ", "ADHD", "Mother", "Father", "Teacher"))
dev.off()

NEW <- read.csv("MAVAN_48M_and_up_jun2020.csv")
NEW <- filter(NEW,gender_male == "1")

#Subset data according to who has at least one item present based on the updated (e.g. parecellated) number of items.
subsample_one_item_present <- NEW[which((rowMeans(is.na(NEW[c("CBCL48_sc6raw.y", "SDQ60_mother_hyperactivity","SDQ60_father_hyperactivity", "CBCL60_sc6raw.y","conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", "conners_teacher_adhd_score.72m", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD","SDQ72_mother_hyperactivity", "SDQ72_father_hyperactivity", "SDQ72_teacher_hyperactivity")]))) <= 14/15),]
dim(subsample_one_item_present)

ADHDfactor.data <- subsample_one_item_present[c("CBCL48_sc6raw.y", "SDQ60_mother_hyperactivity","SDQ60_father_hyperactivity", "CBCL60_sc6raw.y","conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", "conners_teacher_adhd_score.72m", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD","SDQ72_mother_hyperactivity", "SDQ72_father_hyperactivity", "SDQ72_teacher_hyperactivity")]
str(ADHDfactor.data)

sink('bi_adhd_model_boys.txt')
ADHD.model <- '
ADHD =~ CBCL48_sc6raw.y + SDQ60_mother_hyperactivity + SDQ60_father_hyperactivity + CBCL60_sc6raw.y + conners_mother_adhd_score.60m + conners_father_adhd_score.60m + conners_mother_adhd_score.72m + conners_father_adhd_score.72m + conners_teacher_adhd_score.72m + PAPA_p4nadhd + Dominic72_ADHD + SDQ72_mother_hyperactivity + SDQ72_father_hyperactivity + SDQ72_teacher_hyperactivity
Mother =~ CBCL48_sc6raw.y + SDQ60_mother_hyperactivity + CBCL60_sc6raw.y + conners_mother_adhd_score.60m + conners_mother_adhd_score.72m + PAPA_p4nadhd + SDQ72_mother_hyperactivity
Father =~ SDQ60_father_hyperactivity + conners_father_adhd_score.60m + conners_father_adhd_score.72m + SDQ72_father_hyperactivity 
Teacher =~ conners_teacher_adhd_score.72m + SDQ72_teacher_hyperactivity'

ADHD.fit <- cfa(ADHD.model, data = ADHDfactor.data, estimator = "MLR", missing = "FIML", std.ov = TRUE, std.lv = TRUE, orthogonal = T, verbose = T)

o <- lavInspect(ADHD.fit, what ='fit')
capture.output(o, file = "fit_indices_ADHD_model_fiml_ADHD_rater_boys.txt")
s <- summary(ADHD.fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_ADHD_model_fiml__ADHD_rater_boys.txt")
sink()

#png(filename = 'adhd_latent_factor_boys.png', width=1000, height=400)
pdf("adhd_latent_factor_boys.pdf", width=10, height=4, compress = FALSE)
# cols <- wes_palette(name = "Zissou1", n = 5, type = "discrete")
# colorlist <- list(man = cols[2], lat = cols[5])
cols <- wes_palette(name = "FantasticFox1", n = 5, type = "discrete")
colorlist <- list(man = cols[3], lat = cols[4])
semPaths(ADHD.fit, what = "model", bifactor = "ADHD", whatLabels = "std", layout = "tree2",  
         color = colorlist, nCharNodes = 7, sizeMan = 7, sizeMan2 = 4,rotation=2,
         shapeMan = "rectangle", intercepts =FALSE, residuals=FALSE, curve=0.5, fade = F, curvePivot = TRUE, arrows = 1,
         edge.label.cex=0.5, edge.label.position=0.75, exoCov = FALSE, nodeLabels =c("Dominic 72"," CBCL 48  ", "  SDQ 60  ", " CBCL 60  ", "Conners 60", "Conners 72", " PAPA 72  ",
                                                                                     "  SDQ 72  ", "  SDQ 60  ", "Conners 60", "Conners 72", "  SDQ 72  ", "Conners 72", "  SDQ 72  ", "ADHD", "Mother", "Father", "Teacher"))
dev.off()

################
# NEW <- read.csv("MAVAN_48M_and_up_jun2020.csv")
# #Subset data according to who has at least one item present based on the updated (e.g. parecellated) number of items.
# subsample_two_item_present <- NEW[which((rowMeans(is.na(NEW[c("gender_male", "CBCL48_sc6raw.y", "SDQ60_mother_hyperactivity","SDQ60_father_hyperactivity", "CBCL60_sc6raw.y","conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", "conners_teacher_adhd_score.72m", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD","SDQ72_mother_hyperactivity", "SDQ72_father_hyperactivity", "SDQ72_teacher_hyperactivity")]))) <= 14/16),]
# dim(subsample_two_item_present)
# 
# ADHDfactor.data2 <- subsample_two_item_present[c("gender_male","CBCL48_sc6raw.y", "SDQ60_mother_hyperactivity","SDQ60_father_hyperactivity", "CBCL60_sc6raw.y","conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", "conners_teacher_adhd_score.72m", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD","SDQ72_mother_hyperactivity", "SDQ72_father_hyperactivity", "SDQ72_teacher_hyperactivity")]
# str(ADHDfactor.data2)
# 
# sink('correlated_bi_adhd_model_sex.txt')
# ADHD.model2 <- '
# ADHD =~ CBCL48_sc6raw.y + SDQ60_mother_hyperactivity + SDQ60_father_hyperactivity + CBCL60_sc6raw.y + conners_mother_adhd_score.60m + conners_father_adhd_score.60m + conners_mother_adhd_score.72m + conners_father_adhd_score.72m + conners_teacher_adhd_score.72m + PAPA_p4nadhd + Dominic72_ADHD + SDQ72_mother_hyperactivity + SDQ72_father_hyperactivity + SDQ72_teacher_hyperactivity
# Mother =~ CBCL48_sc6raw.y + SDQ60_mother_hyperactivity + CBCL60_sc6raw.y + conners_mother_adhd_score.60m + conners_mother_adhd_score.72m + PAPA_p4nadhd + SDQ72_mother_hyperactivity
# Father =~ SDQ60_father_hyperactivity + conners_father_adhd_score.60m + conners_father_adhd_score.72m + SDQ72_father_hyperactivity 
# Teacher =~ conners_teacher_adhd_score.72m + SDQ72_teacher_hyperactivity
# Mother ~~ Father
# Mother ~~ Teacher
# Father ~~ Teacher'
# 
# ADHD.fit2 <- cfa(ADHD.model2, data = ADHDfactor.data2, estimator = "MLR", missing = "FIML", std.ov = TRUE, std.lv = TRUE, orthogonal = T, verbose = T)
# 
# o <- lavInspect(ADHD.fit2, what ='fit')
# capture.output(o, file = "fit_indices_ADHD_model_fiml_ADHD_rater_sex.txt")
# s <- summary(ADHD.fit2, standardized = T, fit.measures = T)
# capture.output(s, file = "model_output_ADHD_model_fiml_ADHD_rater_sex.txt")
# 
# measurementInvariance(model= ADHD.fit2, 
#                       data = ADHDfactor.data2,
#                       estimator = "MLR", 
#                       missing = "FIML", 
#                       std.ov = TRUE, 
#                       std.lv = TRUE, 
#                       orthogonal = T, 
#                       verbose = T,  
#                       group = "gender_male")
# 
# measurement_invariance <- lavTestLRT(ADHD.fit2)
# capture.output(measurement_invariance, file = "model_differences_ADHD_model_fiml_ADHD_rater_sex.txt")
# 
# fit <- cfa(ADHD.fit2, 
#            data = ADHDfactor.data2, 
#            estimator = "MLR", 
#            missing = "FIML", 
#            std.ov = TRUE, 
#            std.lv = TRUE, 
#            orthogonal = T, 
#            verbose = T , 
#            group = "gender_male")
# summary(fit)
# 
# fit <- cfa(ADHD.fit2, 
#            data = ADHDfactor.data2,
#            group = "gender_male",
#            group.equal = c("loadings"))
# 
# fit <- cfa(ADHD.fit2, 
#            data = ADHDfactor.data2,
#            group = "gender_male",
#            group.equal = c("loadings", "intercepts"))
# summary(fit)
# 
# semTools::measurementInvariance(ADHD.fit2, 
#                       data = ADHDfactor.data2,
#                       group = "gender_male")
# 
# summary(fit)
# 
# fit <-measEq.syntax(ADHD.fit2, 
#               data = ADHDfactor.data2, ID.fac = "std.lv",
#               ID.cat = "Wu.Estabrook.2016", ID.thr = c(1L, 2L), group = "gender_male",
#               group.equal = "", group.partial = "", longFacNames = list(),
#               longIndNames = list(), long.equal = "", long.partial = "",
#               auto = "all", warn = TRUE, debug = FALSE, return.fit = TRUE)
# 
# fit <-measEq.syntax(ADHD.fit2, 
#                     data = ADHDfactor.data2, ID.cat = "Wu.Estabrook.2016", ID.fac = "std.lv", group = "gender_male",
#                     group.equal = "", group.partial = "", longFacNames = list(),
#                     longIndNames = list(), long.equal = "", long.partial = "",
#                     auto = "all", warn = TRUE, debug = FALSE, return.fit = TRUE)
# cat(as.character(fit))
# o <- as.character(fit, package = "lavaan")
# capture.output(o, file = "model_invariance_sex_output.txt")
# # fit <- lavaan(ADHD.fit2, ...) 
# s <-summary(fit, verbose = TRUE)
# capture.output(s, file = "model_invariance_sex_summary.txt")
# 
# o <- lavInspect(fit, what ='fit')
# capture.output(o, file = "model_invariance_sex_output.txt")
# s <- summary(fit, standardized = T, fit.measures = T)
# capture.output(s, file = "model_invariance_sex_summary.txt")
# 
# 
# #TRYING STD.LV =FALSE
# 
# measurementInvariance(model= ADHD.fit2, 
#                       data = ADHDfactor.data2,
#                       estimator = "MLR", 
#                       missing = "FIML", 
#                       std.ov = TRUE, 
#                       std.lv = FALSE, 
#                       orthogonal = T, 
#                       verbose = T,  
#                       group = "gender_male")
# 
# measurement_invariance <- lavTestLRT(ADHD.fit2)
# capture.output(measurement_invariance, file = "model_differences_ADHD_model_fiml_ADHD_rater_sex.txt")
# 
# fit <- cfa(ADHD.fit2, 
#            data = ADHDfactor.data2, 
#            estimator = "MLR", 
#            missing = "FIML", 
#            std.ov = TRUE, 
#            std.lv = FALSE, 
#            orthogonal = T, 
#            verbose = T , 
#            group = "gender_male")
# summary(fit)
# 
# #measurementInvarianceCat()
# 
# #did not converge
# sink()
# 

NEW <- read.csv("MAVAN_48M_and_up_jun2020.csv")
#Subset data according to who has at least one item present based on the updated (e.g. parecellated) number of items.
subsample_two_item_present <- NEW[which((rowMeans(is.na(NEW[c("gender_male", "CBCL48_sc6raw.y", "SDQ60_mother_hyperactivity","SDQ60_father_hyperactivity", "CBCL60_sc6raw.y","conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", "conners_teacher_adhd_score.72m", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD","SDQ72_mother_hyperactivity", "SDQ72_father_hyperactivity", "SDQ72_teacher_hyperactivity")]))) <= 14/16),]
dim(subsample_two_item_present)

ADHDfactor.data2 <- subsample_two_item_present[c("gender_male","CBCL48_sc6raw.y", "SDQ60_mother_hyperactivity","SDQ60_father_hyperactivity", "CBCL60_sc6raw.y","conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", "conners_teacher_adhd_score.72m", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD","SDQ72_mother_hyperactivity", "SDQ72_father_hyperactivity", "SDQ72_teacher_hyperactivity")]
str(ADHDfactor.data2)

sink('bi_adhd_model_invariance.txt')
ADHD.model2 <- '
ADHD =~ CBCL48_sc6raw.y + SDQ60_mother_hyperactivity + SDQ60_father_hyperactivity + CBCL60_sc6raw.y + conners_mother_adhd_score.60m + conners_father_adhd_score.60m + conners_mother_adhd_score.72m + conners_father_adhd_score.72m + conners_teacher_adhd_score.72m + PAPA_p4nadhd + Dominic72_ADHD + SDQ72_mother_hyperactivity + SDQ72_father_hyperactivity + SDQ72_teacher_hyperactivity
Mother =~ CBCL48_sc6raw.y + SDQ60_mother_hyperactivity + CBCL60_sc6raw.y + conners_mother_adhd_score.60m + conners_mother_adhd_score.72m + PAPA_p4nadhd + SDQ72_mother_hyperactivity
Father =~ SDQ60_father_hyperactivity + conners_father_adhd_score.60m + conners_father_adhd_score.72m + SDQ72_father_hyperactivity 
Teacher =~ conners_teacher_adhd_score.72m + SDQ72_teacher_hyperactivity'

ADHD.fit2 <- cfa(ADHD.model2, data = ADHDfactor.data2, estimator = "MLR", missing = "FIML", std.ov = TRUE, std.lv = TRUE, orthogonal = T, verbose = T)

sink('bi_adhd_invariance_test.txt')
# measurementInvariance(ADHD.model2, 
#                       data = ADHDfactor.data2, 
#                       estimator = "MLR",
#                       missing = "FIML",
#                       std.ov = TRUE,
#                       std.lv = TRUE,
#                       orthogonal = T,
#                       verbose = T ,
#                       group = "gender_male")
# sink()
#deprecated

sink('bi_adhd_model_invariance_configural.txt')
fit <- cfa(ADHD.model2, 
           data = ADHDfactor.data2,
           estimator = "MLR",
           missing = "FIML",
           std.ov = TRUE,
           std.lv = TRUE,
           orthogonal = T,
           verbose = T ,
           group = "gender_male")
summary(fit)
options(max.print=999999)
o <- lavInspect(fit, what ='fit')
capture.output(o, file = "fit_indices_bi_adhd_model_invariance_configural.txt")
s <- summary(fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_bi_adhd_model_invariance_configural.txt")
sink()

sink('bi_adhd_model_invariance_loadings.txt')
fit <- cfa(ADHD.model2,
           data = ADHDfactor.data2,
           estimator = "MLR",
           missing = "FIML",
           std.ov = TRUE,
           std.lv = TRUE,
           orthogonal = T,
           verbose = T ,
           group = "gender_male",
           group.equal = c("loadings"))
summary(fit)
options(max.print=999999)
o <- lavInspect(fit, what ='fit')
capture.output(o, file = "fit_indices_bi_adhd_model_invariance_loadings.txt")
s <- summary(fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_bi_adhd_model_invariance_loadings.txt")
sink()

sink('bi_adhd_model_invariance_loadings_intercepts.txt')
fit <- cfa(ADHD.model2, 
            data = ADHDfactor.data2,
           estimator = "MLR",
           missing = "FIML",
           std.ov = TRUE,
           std.lv = TRUE,
           orthogonal = T,
           verbose = T ,
           group = "gender_male",
            group.equal = c("loadings", "intercepts"))
summary(fit)
options(max.print=999999)
o <- lavInspect(fit, what ='fit')
capture.output(o, file = "fit_indices_bi_adhd_model_invariance_loadings_intercepts.txt")
s <- summary(fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_bi_adhd_model_invariance_loadings_intercepts.txt")
sink()

sink('bi_adhd_model_invariance_loadings_intercepts_means.txt')
fit <- cfa(ADHD.model2, 
           data = ADHDfactor.data2,
           estimator = "MLR",
           missing = "FIML",
           std.ov = TRUE,
           std.lv = TRUE,
           orthogonal = T,
           verbose = T ,
           group = "gender_male",
           group.equal = c("loadings", "intercepts", "means"))
summary(fit)
options(max.print=999999)
o <- lavInspect(fit, what ='fit')
capture.output(o, file = "fit_indices_bi_adhd_model_invariance_loadings_intercepts_means.txt")
s <- summary(fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_bi_adhd_model_invariance_loadings_intercepts_means.txt")
sink()

sink('bi_adhd_model_invariance_loadings_intercepts_residuals.txt')
fit <- cfa(ADHD.model2, 
           data = ADHDfactor.data2,
           estimator = "MLR",
           missing = "FIML",
           std.ov = TRUE,
           std.lv = TRUE,
           orthogonal = T,
           verbose = T ,
           group = "gender_male",
           group.equal = c("loadings", "intercepts", "residuals"),
           optim.method = "em", em.iter.max = 20000)
summary(fit)
options(max.print=999999)
o <- lavInspect(fit, what ='fit')
capture.output(o, file = "fit_indices_bi_adhd_model_invariance_loadings_intercepts_residuals.txt")
s <- summary(fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_bi_adhd_model_invariance_loadings_intercepts_residuals.txt")
sink()


sink('bi_adhd_model_invariance_loadings_intercepts_residuals_lv_var_covar.txt')
fit <- cfa(ADHD.model2, 
           data = ADHDfactor.data2,
           estimator = "MLR",
           missing = "FIML",
           std.ov = TRUE,
           std.lv = TRUE,
           orthogonal = T,
           verbose = T ,
           group = "gender_male",
           group.equal = c("loadings", "intercepts", "residuals", "lv.variances",
                           "lv.covariances"))
summary(fit)
options(max.print=999999)
o <- lavInspect(fit, what ='fit')
capture.output(o, file = "fit_indices_bi_adhd_model_invariance_loadings_intercepts_residuals_lv_var_covar.txt")
s <- summary(fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_bi_adhd_model_invariance_loadings_intercepts_residuals_lv_var_covar.txt")
sink()

sink('bi_adhd_model_non_ortho.txt')
ADHD.model <- '
ADHD =~ CBCL48_sc6raw.y + SDQ60_mother_hyperactivity + SDQ60_father_hyperactivity + CBCL60_sc6raw.y + conners_mother_adhd_score.60m + conners_father_adhd_score.60m + conners_mother_adhd_score.72m + conners_father_adhd_score.72m + conners_teacher_adhd_score.72m + PAPA_p4nadhd + Dominic72_ADHD + SDQ72_mother_hyperactivity + SDQ72_father_hyperactivity + SDQ72_teacher_hyperactivity
Mother =~ CBCL48_sc6raw.y + SDQ60_mother_hyperactivity + CBCL60_sc6raw.y + conners_mother_adhd_score.60m + conners_mother_adhd_score.72m + PAPA_p4nadhd + SDQ72_mother_hyperactivity
Father =~ SDQ60_father_hyperactivity + conners_father_adhd_score.60m + conners_father_adhd_score.72m + SDQ72_father_hyperactivity 
Teacher =~ conners_teacher_adhd_score.72m + SDQ72_teacher_hyperactivity'

ADHD.fit <- cfa(ADHD.model, data = ADHDfactor.data, estimator = "MLR", missing = "FIML", std.ov = TRUE, std.lv = TRUE, orthogonal = F, verbose = T)

o <- lavInspect(ADHD.fit, what ='fit')
capture.output(o, file = "fit_indices_ADHD_model_fiml_ADHD_rater_non_ortho.txt")
s <- summary(ADHD.fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_ADHD_model_fiml__ADHD_rater_non_ortho.txt")
sink()

sink('bi_adhd_model.txt')

sink('bi_adhd_model_invariance_configural_non_ortho.txt')
fit <- cfa(ADHD.model2, 
           data = ADHDfactor.data2,
           estimator = "MLR",
           missing = "FIML",
           std.ov = TRUE,
           std.lv = TRUE,
           orthogonal = F,
           verbose = T ,
           group = "gender_male")
summary(fit)
options(max.print=999999)
o <- lavInspect(fit, what ='fit')
capture.output(o, file = "fit_indices_bi_adhd_model_invariance_configural_non_ortho.txt")
s <- summary(fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_bi_adhd_model_invariance_configural_non_ortho.txt")
sink()

sink('bi_adhd_model_invariance_loadings_non_ortho.txt')
fit <- cfa(ADHD.model2,
           data = ADHDfactor.data2,
           estimator = "MLR",
           missing = "FIML",
           std.ov = TRUE,
           std.lv = TRUE,
           orthogonal = F,
           verbose = T ,
           group = "gender_male",
           group.equal = c("loadings"))
summary(fit)
options(max.print=999999)
o <- lavInspect(fit, what ='fit')
capture.output(o, file = "fit_indices_bi_adhd_model_invariance_loadings_non_ortho.txt")
s <- summary(fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_bi_adhd_model_invariance_loadings_non_ortho.txt")
sink()

sink('bi_adhd_model_invariance_loadings_intercepts_non_ortho.txt')
fit <- cfa(ADHD.model2, 
           data = ADHDfactor.data2,
           estimator = "MLR",
           missing = "FIML",
           std.ov = TRUE,
           std.lv = TRUE,
           orthogonal = F,
           verbose = T ,
           group = "gender_male",
           group.equal = c("loadings", "intercepts"))
summary(fit)
options(max.print=999999)
o <- lavInspect(fit, what ='fit')
capture.output(o, file = "fit_indices_bi_adhd_model_invariance_loadings_intercepts_non_ortho.txt")
s <- summary(fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_bi_adhd_model_invariance_loadings_intercepts_non_ortho.txt")
modindices(fit)
sink()

sink('bi_adhd_model_invariance_loadings_intercepts_means_non_ortho.txt')
fit <- cfa(ADHD.model2, 
           data = ADHDfactor.data2,
           estimator = "MLR",
           missing = "FIML",
           std.ov = TRUE,
           std.lv = TRUE,
           orthogonal = F,
           verbose = T ,
           group = "gender_male",
           group.equal = c("loadings", "intercepts", "means"))
summary(fit)
options(max.print=999999)
o <- lavInspect(fit, what ='fit')
capture.output(o, file = "fit_indices_bi_adhd_model_invariance_loadings_intercepts_means_non_ortho.txt")
s <- summary(fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_bi_adhd_model_invariance_loadings_intercepts_means_non_ortho.txt")
sink()

sink('bi_adhd_model_invariance_loadings_intercepts_residuals_non_ortho.txt')
fit <- cfa(ADHD.model2, 
           data = ADHDfactor.data2,
           estimator = "MLR",
           missing = "FIML",
           std.ov = TRUE,
           std.lv = TRUE,
           orthogonal = F,
           verbose = T ,
           group = "gender_male",
           group.equal = c("loadings", "intercepts", "residuals"))
summary(fit)
options(max.print=999999)
o <- lavInspect(fit, what ='fit')
capture.output(o, file = "fit_indices_bi_adhd_model_invariance_loadings_intercepts_residuals_non_ortho.txt")
s <- summary(fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_bi_adhd_model_invariance_loadings_intercepts_residuals_non_ortho.txt")
sink()

sink('bi_adhd_model_invariance_loadings_intercepts_residuals_lv_var_covar_non_ortho.txt')
fit <- cfa(ADHD.model2, 
           data = ADHDfactor.data2,
           estimator = "MLR",
           missing = "FIML",
           std.ov = TRUE,
           std.lv = TRUE,
           orthogonal = F,
           verbose = T ,
           group = "gender_male",
           group.equal = c("loadings", "intercepts", "residuals", "lv.variances",
                           "lv.covariances"))
summary(fit)
options(max.print=999999)
o <- lavInspect(fit, what ='fit')
capture.output(o, file = "fit_indices_bi_adhd_model_invariance_loadings_intercepts_residuals_lv_var_covar_non_ortho.txt")
s <- summary(fit, standardized = T, fit.measures = T)
capture.output(s, file = "model_output_bi_adhd_model_invariance_loadings_intercepts_residuals_lv_var_covar_non_ortho.txt")
sink()


sink('correlated_bi_adhd_model_sex.txt')
ADHD.model2 <- '
ADHD =~ CBCL48_sc6raw.y + SDQ60_mother_hyperactivity + SDQ60_father_hyperactivity + CBCL60_sc6raw.y + conners_mother_adhd_score.60m + conners_father_adhd_score.60m + conners_mother_adhd_score.72m + conners_father_adhd_score.72m + conners_teacher_adhd_score.72m + PAPA_p4nadhd + Dominic72_ADHD + SDQ72_mother_hyperactivity + SDQ72_father_hyperactivity + SDQ72_teacher_hyperactivity
Mother =~ CBCL48_sc6raw.y + SDQ60_mother_hyperactivity + CBCL60_sc6raw.y + conners_mother_adhd_score.60m + conners_mother_adhd_score.72m + PAPA_p4nadhd + SDQ72_mother_hyperactivity
Father =~ SDQ60_father_hyperactivity + conners_father_adhd_score.60m + conners_father_adhd_score.72m + SDQ72_father_hyperactivity
Teacher =~ conners_teacher_adhd_score.72m + SDQ72_teacher_hyperactivity'
Mother ~~ Father
Mother ~~ Teacher
Father ~~ Teacher'

ADHD.fit2 <- cfa(ADHD.model2, data = ADHDfactor.data2, estimator = "MLR", missing = "FIML", std.ov = TRUE, std.lv = TRUE, orthogonal = T, verbose = T)


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

fit <- cfa(ADHD.fit2,
data = ADHDfactor.data2,
group = "gender_male",
group.equal = c("loadings"))

fit <- cfa(ADHD.fit2, 
data = ADHDfactor.data2,
group = "gender_male",
group.equal = c("loadings", "intercepts"))
summary(fit)

sink()

