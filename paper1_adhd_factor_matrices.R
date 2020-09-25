#Marie-Elyse Lafaille-Magnan, PhD
#marie-elyse.lafaille-magnan@mail.mcgill.ca

setwd("/Users/Marie-Elyse/Downloads")
NEW = read.csv("MAVAN_48M_and_up_jun2020.csv")

library(corrplot)

pdf("adhd_factors.pdf", width=5, height=4, compress=FALSE)
mydata.cor = cor(NEW[, c("ADHD", "Mother",	"Father",	"Teacher")],  method ="kendall",  use = "pairwise")
res1 <- cor.mtest(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher")], conf.level = .95, method ="kendall",  alternative = "two.sided", exact=FALSE, use = "pairwise")

corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

dev.off()

#####PANEL A

pdf("adhd_factors_questionnaires.pdf", width=6, height=4, compress=FALSE)


mydata.cor = stats::cor(NEW[, c("ADHD", "Mother",	"Father",	"Teacher", "gender_male",
                                "PAPA_p4nadhd", "PAPA_p4_adhd",
                                "Dominic72_ADHD", 
                                "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                                "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                                "conners_teacher_adhd_score.72m",
                                "CBCL48_sc6raw.y", "CBCL60_sc6raw.y", 
                                "SDQ60_mother_hyperactivity", 
                                "SDQ60_father_hyperactivity", 
                                "SDQ72_mother_hyperactivity", 
                                "SDQ72_father_hyperactivity", 
                                "SDQ72_teacher_hyperactivity")],  method ="kendall",  use = "pairwise")


res1 <- corrplot::cor.mtest(NEW[, c("ADHD", "Mother",	"Father",	"Teacher", "gender_male",
                                    "PAPA_p4nadhd", "PAPA_p4_adhd",
                                    "Dominic72_ADHD", 
                                    "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                                    "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                                    "conners_teacher_adhd_score.72m",
                                    "CBCL48_sc6raw.y", "CBCL60_sc6raw.y", 
                                    "SDQ60_mother_hyperactivity", 
                                    "SDQ60_father_hyperactivity", 
                                    "SDQ72_mother_hyperactivity", 
                                    "SDQ72_father_hyperactivity", 
                                    "SDQ72_teacher_hyperactivity")], conf.level = .95, method ="kendall",  alternative = "two.sided", exact=FALSE, use = "pairwise")

colnames(mydata.cor) <-c("ADHD",	"Mother",	"Father",	"Teacher", "Male",
                         "PAPA ADHD number of symptoms", "PAPA ADHD diagnosis",
                         "Dominic ADHD", 
                         "Conners mother ADHD 60m", "Conners father ADHD 60m", 
                         "Conners mother ADHD 72m", "Conners father ADHD 72m", 
                         "Conners teacher ADHD 72m",
                         "CBCL hyperactivity 48m", "CBCL hyperactivity 60m", 
                         "SDQ mother hyperactivity 60m", 
                         "SDQ father hyperactivity 60m", 
                         "SDQ mother hyperactivity 72m", 
                         "SDQ father hyperactivity 72m", 
                         "SDQ teacher hyperactivity 72m")

rownames(mydata.cor) <-c("ADHD",	"Mother",	"Father",	"Teacher", "Male",
                         "PAPA ADHD number of symptoms", "PAPA ADHD diagnosis",
                         "Dominic ADHD", 
                         "Conners mother ADHD 60m", "Conners father ADHD 60m", 
                         "Conners mother ADHD 72m", "Conners father ADHD 72m", 
                         "Conners teacher ADHD 72m",
                         "CBCL hyperactivity 48m", "CBCL hyperactivity 60m", 
                         "SDQ mother hyperactivity 60m", 
                         "SDQ father hyperactivity 60m", 
                         "SDQ mother hyperactivity 72m", 
                         "SDQ father hyperactivity 72m", 
                         "SDQ teacher hyperactivity 72m")

corrplot::corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper",
                   sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
                   insig = "label_sig", pch.col = "black", tl.col = "black")

dev.off()

########PANEL B
pdf("adhd_factors_risk_factors.pdf", width=5, height=4, compress=FALSE)

mydata.cor = stats::cor(NEW[, c("ADHD", "Mother", "Father",	"Teacher", "gender_male",
                                "mom_age_birth", "above_college", "Pren_income4", 
                                "Pren_life_events", 
                                "birth_wt_grams",	
                                "gestation_age_wks")],  method ="kendall",  use = "pairwise")

res1 <- corrplot::cor.mtest(NEW[, c("ADHD", "Mother",	"Father",	"Teacher", "gender_male",
                                    "mom_age_birth", "above_college", "Pren_income4", 
                                    "Pren_life_events",  
                                    "birth_wt_grams",	
                                    "gestation_age_wks")],  method ="kendall",  use = "pairwise")

colnames(mydata.cor) <- c("ADHD", "Mother",	"Father",	"Teacher", "Male",
                          "Maternal age at birth", "Maternal education", "Prenatal income", 
                          "Prenatal adverse life events",  
                          "Birth weight grams",	
                          "Gestational age weeks")

rownames(mydata.cor) <- c("ADHD", "Mother",	"Father",	"Teacher", "Male",
                          "Maternal age at birth", "Maternal education", "Prenatal income", 
                          "Prenatal adverse life events",  
                          "Birth weight grams",	
                          "Gestational age weeks")

corrplot::corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper",
                   sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
                   insig = "label_sig", pch.col = "black", tl.col = "black")

dev.off()

######PANEL C
# png(filename = 'adhd_factors_depression_rater.png')
pdf("adhd_factors_depression_rater.pdf", width=10, height=4, compress=FALSE)



mydata.cor = stats::cor(NEW[, c("ADHD", "Mother",	"Father",	"Teacher", "gender_male", "Pren_CESD", "HWB6_CESD",	"HWB12_CESD", 
                                "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "auc_post_cesd")],  method ="kendall",  use = "pairwise")

res1 <- corrplot::cor.mtest(NEW[, c("ADHD", "Mother",	"Father",	"Teacher", "gender_male", 
                                    "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                                    "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "auc_post_cesd")],  method ="kendall",  use = "pairwise")

colnames(mydata.cor) <- c("ADHD", "Mother",	"Father",	"Teacher", "Male", 
                           "Prenatal CESD", "CESD 6m",	"CESD 12 m",	
                          "CESD 24m",	"CESD 36m",	"CESD 48m",	"CESD 60m",	"CESD 72m", "Postnatal depression")
  
rownames(mydata.cor) <- c("ADHD", "Mother",	"Father",	"Teacher", "Male", 
                          "Prenatal CESD", "CESD 6m",	"CESD 12 m",	
                          "CESD 24m",	"CESD 36m",	"CESD 48m",	"CESD 60m",	"CESD 72m", "Postnatal depression")

corrplot::corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper",
                   sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
                   insig = "label_sig", pch.col = "black", tl.col = "black")

dev.off()

# 
# png(filename = 'adhd_factors_external_factors.png', width=1000, height=400, res=300)
######PANEL D
pdf("adhd_factors_external_factors.pdf", width=7, height=4, compress=FALSE)

mydata.cor = stats::cor(NEW[, c("ADHD", "Mother",	"Father",	"Teacher", "CBCL48_sc5raw.y", "CBCL60_sc5raw.y",
                         "Conduct_Disorder", "ODD","Dominic72_cd", "Dominic72_odd",
                         "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD", 
                         "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                         "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                         "conners_teacher_hyperactivity_score.72m", "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                         "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                         "conners_teacher_cognitive_score.72m")],  method ="kendall",  use = "pairwise")
res1 <- corrplot::cor.mtest(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher", "CBCL48_sc5raw.y", "CBCL60_sc5raw.y",
                          "Conduct_Disorder", "ODD","Dominic72_cd", "Dominic72_odd",
                          "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                          "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                          "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                          "conners_teacher_hyperactivity_score.72m","conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                          "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                          "conners_teacher_cognitive_score.72m")], conf.level = .95, method ="kendall",  alternative = "two.sided", exact=FALSE, use = "pairwise")

colnames(mydata.cor) <- c("ADHD",	"Mother",	"Father",	"Teacher", "CBCL 48 sleep problems", "CBCL 60 sleep problems",
                          "Conduct Disorder", "ODD","Dominic 72 CD", "Dominic 72 ODD",
                          "Comorbidities Number", "Comorbidity",
                          "Conners mother hyperactivity score 60m", "Conners father hyperactivity score 60m", 
                          "Conners mother hyperactivity score 72m", "Conners father hyperactivity score 72m", 
                          "Conners teacher hyperactivity score 72m","Conners mother cognitive score 60m", "conners father cognitive score 60m", 
                          "Conners mother cognitive score 72m", "Conners father cognitive score 72m", 
                          "Conners teacher cognitive score 72m")
rownames(mydata.cor) <- c("ADHD",	"Mother",	"Father",	"Teacher", "CBCL 48 sleep problems", "CBCL 60 sleep problems",
                          "Conduct Disorder", "ODD","Dominic 72 CD", "Dominic 72 ODD",
                          "Comorbidities Number", "Comorbidity",
                          "Conners mother hyperactivity score 60m", "Conners father hyperactivity score 60m", 
                          "Conners mother hyperactivity score 72m", "Conners father hyperactivity score 72m", 
                          "Conners teacher hyperactivity score 72m","Conners mother cognitive score 60m", "conners father cognitive score 60m", 
                          "Conners mother cognitive score 72m", "Conners father cognitive score 72m", 
                          "Conners teacher cognitive score 72m")

corrplot::corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

dev.off()

# png(filename = 'adhd_factors_vs_adhd_prs.png', width=1000, height=400)

#####PANEL E
pdf("adhd_factors_vs_adhd_prs.pdf", width=5, height=4, compress=FALSE)


mydata.cor = stats::cor(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher", 
                                "PRS_0_5_adhd_child", "PRS_0_2_adhd_child", 
                                "PRS_0_1_adhd_child", "PRS_0_05_adhd_child",
                                "PRS_0_01_adhd_child", "PRS_0_001_adhd_child", 
                                "PRS_0_0001_adhd_child")],  method ="kendall",  use = "pairwise")

res1 <- corrplot::cor.mtest(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher",  
                                    "PRS_0_5_adhd_child", "PRS_0_2_adhd_child", 
                                    "PRS_0_1_adhd_child", "PRS_0_05_adhd_child",
                                    "PRS_0_01_adhd_child", "PRS_0_001_adhd_child", 
                                    "PRS_0_0001_adhd_child")], conf.level = .95, method ="kendall",  alternative = "two.sided", exact=FALSE, use = "pairwise")

rownames(mydata.cor) <-c("ADHD",	"Mother",	"Father",	"Teacher",  
             "Child PRS .5 ADHD", "Child PRS .2 ADHD", 
             "Child PRS .1 ADHD", "Child PRS .05",
             "Child PRS .01 ADHD", "Child PRS .001 ADHD", 
             "Child PRS .0001 ADHD")

colnames(mydata.cor) <-c("ADHD",	"Mother",	"Father",	"Teacher",  
             "Child PRS .5 ADHD", "Child PRS .2 ADHD", 
             "Child PRS .1 ADHD", "Child PRS .05",
             "Child PRS .01 ADHD", "Child PRS .001 ADHD", 
             "Child PRS .0001 ADHD")

corrplot::corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper",
                   sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
                   insig = "label_sig", pch.col = "black", tl.col = "black")

dev.off()

#########OTHER OPTIONS
#TO VIEW CORRELATIONS
View(mydata.cor)
#TO VIEW P-FACTORS
View(res1$p)

corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "lower",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "lower", order="FPC",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper", order="FPC",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")



# png(filename = 'adhd_factors_vs_adhd_factor_demographics.png', width=1000, height=400)
pdf("adhd_factors_vs_adhd_factor_demographics.pdf", width=5, height=4, compress=FALSE)


mydata.cor = stats::cor(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher", "PAPA_p4nadhd", "PAPA_p4_adhd",
                          "gender_male", "GEN",
                          "mom_age_birth", "above_college", "Pren_income4", 
                          "Pren_life_events", "QuintMat_w", "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                          "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD")],  method ="kendall",  use = "pairwise")


res1 <- corrplot::cor.mtest(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher", 
                           "PAPA_p4nadhd", "PAPA_p4_adhd",
                           "gender_male", "GEN",
                           "mom_age_birth", "above_college", "Pren_income4", 
                           "Pren_life_events", "QuintMat_w", "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                           "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD")], conf.level = .95, method ="kendall",  alternative = "two.sided", exact=FALSE, use = "pairwise")

mydata.cor = stats::cor(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher", 
                         "Male", "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                         "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD")],  method ="kendall",  use = "pairwise")


res1 <- corrplot::cor.mtest(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher", 
                          "Male", "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                          "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD")], conf.level = .95, method ="kendall",  alternative = "two.sided", exact=FALSE, use = "pairwise")


corrplot::corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

dev.off()


View(mydata.cor)

View(res1$p)

# png(filename = 'adhd_factors_vs_snps.png', width=1000, height=400)
pdf("adhd_factors_vs_snps.pdf", width=10, height=4, compress=FALSE)


mydata.cor = stats::cor(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher",
                         "gender_male", "B_HTTLPR_2",	"B_OXT_pep1",	"B_TPH2",	"B_HTR1A",	"B_HTR1B_best",	"B_HTR2A_alt_r",	
                         "B_DRD4",	"B_DAT",	"B_DRD2",	 "B_DRD2_rs1799978",	"B_DRD3_rs6280", "B_GR_rs10052957",	"B_COMTB_COMT_rs165599", "B_BDNF_r")],  method ="kendall",  use = "pairwise")


res1 <- corrplot::cor.mtest(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher",
                          "gender_male", "B_HTTLPR_2",	"B_OXT_pep1",	"B_TPH2",	"B_HTR1A",	"B_HTR1B_best",	"B_HTR2A_alt_r",	
                          "B_DRD4",	"B_DAT",	"B_DRD2",	 "B_DRD2_rs1799978",	"B_DRD3_rs6280", "B_GR_rs10052957",	"B_COMTB_COMT_rs165599", "B_BDNF_r")], conf.level = .95, method ="kendall",  alternative = "two.sided", exact=FALSE, use = "pairwise")

corrplot::corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

dev.off()

setwd("/Users/Marie-Elyse/Downloads")
NEW = read.csv("MAVAN_48M_and_up_april2020.csv")

# png(filename = 'adhd_factors_vs_snps.png', width=1000, height=400)
pdf("adhd_factors_vs_snps.pdf", width=10, height=4, compress=FALSE)


mydata.cor = stats::cor(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher",
                         "gender_male", "B_HTTLPR_2",	"B_OXT_pep1",	"B_TPH2",	"B_HTR1A",
                         "B_HTR1B_best",	"B_HTR2A_alt_r",	"B_5HTR1B_rs130058",	
                         "B_DRD4", "B_DRD4_78",	"B_DAT",
                         "B_DRD2",	"B_DRD1_hap",
                         "B_DRD2_rs1799978", "B_DRD3_rs6280", "B_GR_rs10052957", "B_COMT", "B_COMT_cat",
                         "B_BDNF_r")],  method ="kendall",  use = "pairwise")

res1 <- corrplot::cor.mtest(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher",
                          "gender_male", "B_HTTLPR_2",	"B_OXT_pep1",	"B_TPH2",	"B_HTR1A",
                          "B_HTR1B_best",	"B_HTR2A_alt_r",	"B_5HTR1B_rs130058",	
                          "B_DRD4", "B_DRD4_78",	"B_DAT",
                          "B_DRD2",	"B_DRD1_hap",
                          "B_DRD2_rs1799978", "B_DRD3_rs6280", "B_GR_rs10052957", "B_COMT", "B_COMT_cat",
                          "B_BDNF_r")], conf.level = .95, method ="kendall",  alternative = "two.sided", exact=FALSE, use = "pairwise")

corrplot::corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

dev.off()

View(mydata.cor)

View(res1$p)

# png(filename = 'adhd_factors_questionnaires.png')


# png(filename = 'adhd_factors_questionnaires_including_conners_cog_hyper.png')
pdf("adhd_factors_questionnaires_including_conners_cog_hyper.pdf", width=10, height=4, compress=FALSE)

mydata.cor = stats::cor(NEW[, c("ADHD", "Mother",	"Father",	"Teacher", 
                         "PAPA_p4nadhd", "PAPA_p4_adhd",
                         "Dominic72_ADHD", 
                         "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                         "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                         "conners_teacher_adhd_score.72m",
                         "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                         "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                         "conners_teacher_cognitive_score.72m",
                         "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                         "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                         "conners_teacher_hyperactivity_score.72m",
                         "CBCL48_sc6raw.y", "CBCL60_sc6raw.y", 
                         "SDQ60_mother_hyperactivity", 
                         "SDQ60_father_hyperactivity", 
                         "SDQ72_mother_hyperactivity", 
                         "SDQ72_father_hyperactivity", 
                         "SDQ72_teacher_hyperactivity" , "gender_male")],  method ="kendall",  use = "pairwise")

res1 <- corrplot::cor.mtest(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                          "PAPA_p4nadhd", "PAPA_p4_adhd",
                          "Dominic72_ADHD", 
                          "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                          "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                          "conners_teacher_adhd_score.72m",
                          "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                          "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                          "conners_teacher_cognitive_score.72m",
                          "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                          "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                          "conners_teacher_hyperactivity_score.72m",
                          "CBCL48_sc6raw.y", "CBCL60_sc6raw.y", 
                          "SDQ60_mother_hyperactivity", 
                          "SDQ60_father_hyperactivity", 
                          "SDQ72_mother_hyperactivity", 
                          "SDQ72_father_hyperactivity", 
                          "SDQ72_teacher_hyperactivity", "gender_male")], conf.level = .95, method ="kendall",  alternative = "two.sided", exact=FALSE, use = "pairwise")

corrplot::corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

dev.off()




png(filename = 'adhd_factors_questionnaires.png')

mydata.cor = stats::cor(NEW[, c("ADHD", "Mother",	"Father",	"Teacher", 
                         "PAPA_p4nadhd", "PAPA_p4_adhd",
                         "Dominic72_ADHD", 
                         "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                         "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                         "conners_teacher_adhd_score.72m",
                         "CBCL48_sc6raw.y", "CBCL60_sc6raw.y", 
                         "SDQ60_mother_hyperactivity", 
                         "SDQ60_father_hyperactivity", 
                         "SDQ72_mother_hyperactivity", 
                         "SDQ72_father_hyperactivity", 
                         "SDQ72_teacher_hyperactivity" , "gender_male")],  method ="kendall",  use = "pairwise")


res1 <- corrplot::cor.mtest(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                          "PAPA_p4nadhd", "PAPA_p4_adhd",
                          "Dominic72_ADHD", 
                          "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                          "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                          "conners_teacher_adhd_score.72m",
                          "CBCL48_sc6raw.y", "CBCL60_sc6raw.y", 
                          "SDQ60_mother_hyperactivity", 
                          "SDQ60_father_hyperactivity", 
                          "SDQ72_mother_hyperactivity", 
                          "SDQ72_father_hyperactivity", 
                          "SDQ72_teacher_hyperactivity", "gender_male")], conf.level = .95, method ="kendall",  alternative = "two.sided", exact=FALSE, use = "pairwise")

corrplot::corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

dev.off()



png(filename = 'adhd_factors_vs_adhd_factor_demographics.png')

mydata.cor = stats::cor(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher", "PAPA_p4nadhd", "PAPA_p4_adhd",
                          "gender_male", "GEN",
                          "mom_age_birth", "above_college", "Pren_income4", 
                          "Pren_life_events", "QuintMat_w", "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                          "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD")],  method ="kendall",  use = "pairwise")


res1 <- corrplot::cor.mtest(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher", 
                           "PAPA_p4nadhd", "PAPA_p4_adhd",
                           "gender_male", "GEN",
                           "mom_age_birth", "above_college", "Pren_income4", 
                           "Pren_life_events", "QuintMat_w", "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                           "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD")], conf.level = .95, method ="kendall",  alternative = "two.sided", exact=FALSE, use = "pairwise")

corrplot::corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

dev.off()



View(mydata.cor)

View(res1$p)

#for paper 2

png(filename = 'adhd_factors_vs_snps.png', res=300)

mydata.cor = stats::cor(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher",
                         "gender_male", "B_HTTLPR_2",	"B_OXT_pep1",	"B_TPH2",	"B_HTR1A",	"B_HTR1B_best",	"B_HTR2A_alt_r",	
                         "B_DRD4",	"B_DAT",	"B_DRD2",	 "B_DRD2_rs1799978",	"B_DRD3_rs6280", "B_GR_rs10052957",	"B_COMTB_COMT_rs165599", "B_BDNF_r")],  method ="kendall",  use = "pairwise")


res1 <- corrplot::cor.mtest(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher",
                          "gender_male", "B_HTTLPR_2",	"B_OXT_pep1",	"B_TPH2",	"B_HTR1A",	"B_HTR1B_best",	"B_HTR2A_alt_r",	
                          "B_DRD4",	"B_DAT",	"B_DRD2",	 "B_DRD2_rs1799978",	"B_DRD3_rs6280", "B_GR_rs10052957",	"B_COMTB_COMT_rs165599", "B_BDNF_r")], conf.level = .95, method ="kendall",  alternative = "two.sided", exact=FALSE, use = "pairwise")

corrplot::corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

dev.off()

png(filename = 'adhd_factors_vs_snps.png', res=300)

mydata.cor = stats::cor(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher",
                         "gender_male", "B_HTTLPR_2",	"B_OXT_pep1",	"B_TPH2",	"B_HTR1A",
                         "B_HTR1B_best",	"B_HTR2A_alt_r",	"B_5HTR1B_rs130058",	
                         "B_DRD4", "B_DRD4_78",	"B_DAT",
                         "B_DRD2",	"B_DRD1_hap",
                         "B_DRD2_rs1799978", "B_DRD3_rs6280", "B_GR_rs10052957", "B_COMT", "B_COMT_cat",
                         "B_BDNF_r")],  method ="kendall",  use = "pairwise")

res1 <- corrplot::cor.mtest(NEW[, c("ADHD",	"Mother",	"Father",	"Teacher",
                          "gender_male", "B_HTTLPR_2",	"B_OXT_pep1",	"B_TPH2",	"B_HTR1A",
                          "B_HTR1B_best",	"B_HTR2A_alt_r",	"B_5HTR1B_rs130058",	
                          "B_DRD4", "B_DRD4_78",	"B_DAT",
                          "B_DRD2",	"B_DRD1_hap",
                          "B_DRD2_rs1799978", "B_DRD3_rs6280", "B_GR_rs10052957", "B_COMT", "B_COMT_cat",
                          "B_BDNF_r")], conf.level = .95, method ="kendall",  alternative = "two.sided", exact=FALSE, use = "pairwise")

corrplot::corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

dev.off()

View(mydata.cor)

View(res1$p)

