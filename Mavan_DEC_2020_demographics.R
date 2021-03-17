setwd("/Users/Marie-Elyse/Downloads")
NEW = read.csv("MAVAN_48M_and_up_jun2020.csv")

sink('characteristic_description.txt')
print(count(NEW$Pren_CESD))
print(count(NEW$HWB6_CESD))
print(count(NEW$HWB12_CESD))
print(count(NEW$HWB24_CESD))
print(count(NEW$HWB36_CESD))
print(count(NEW$HWB48_CESD))
print(count(NEW$HWB60_CESD))
print(count(NEW$HWB72_CESD))
# ", "HWB6_CESD",	"HWB12_CESD",	
# "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD")]))
sink()

sink('characteristic_description.txt')
print(summary(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                      "gender_male",
                      "mom_age_birth", "above_college", "Pren_income4", 
                      "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                      "Alcohol_During_Pregnancy",
                      "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                      "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                      "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                      "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                      "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                      "conners_teacher_cognitive_score.72m",
                      "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                      "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                      "conners_teacher_hyperactivity_score.72m",
                      "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                      "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                      "SDQ60_mother_hyperactivity", 
                      "SDQ60_father_hyperactivity", 
                      "SDQ72_mother_hyperactivity", 
                      "SDQ72_father_hyperactivity", 
                      "SDQ72_teacher_hyperactivity",
                      "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                      "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                      "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                      "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                      "gestation_age_wks", "Dominic72_cd","Dominic72_odd", "auc_post_cesd")]))

describeVEC <-Hmisc::describe(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                                      "gender_male",
                                      "mom_age_birth", "above_college", "Pren_income4", 
                                      "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                                      "Alcohol_During_Pregnancy",
                                      "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                                      "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                                      "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                                      "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                                      "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                                      "conners_teacher_cognitive_score.72m",
                                      "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                                      "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                                      "conners_teacher_hyperactivity_score.72m",
                                      "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                                      "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                                      "SDQ60_mother_hyperactivity", 
                                      "SDQ60_father_hyperactivity", 
                                      "SDQ72_mother_hyperactivity", 
                                      "SDQ72_father_hyperactivity", 
                                      "SDQ72_teacher_hyperactivity",
                                      "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                                      "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                                      "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                                      "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                                      "gestation_age_wks", "Dominic72_cd","Dominic72_odd", "auc_post_cesd")], transpose = TRUE)
print(describeVEC)

library(pastecs)
stat.describeVEC <-pastecs::stat.desc(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                                              "gender_male",
                                              "mom_age_birth", "above_college", "Pren_income4", 
                                              "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                                              "Alcohol_During_Pregnancy",
                                              "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                                              "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                                              "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                                              "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                                              "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                                              "conners_teacher_cognitive_score.72m",
                                              "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                                              "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                                              "conners_teacher_hyperactivity_score.72m",
                                              "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                                              "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                                              "SDQ60_mother_hyperactivity", 
                                              "SDQ60_father_hyperactivity", 
                                              "SDQ72_mother_hyperactivity", 
                                              "SDQ72_father_hyperactivity", 
                                              "SDQ72_teacher_hyperactivity",
                                              "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                                              "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                                              "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                                              "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                                              "gestation_age_wks", "Dominic72_cd","Dominic72_odd", "auc_post_cesd")])
print(stat.describeVEC)

###best
psych.describeVEC <-psych::describe(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                                            "gender_male",
                                            "mom_age_birth", "above_college", "Pren_income4", 
                                            "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                                            "Alcohol_During_Pregnancy",
                                            "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                                            "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                                            "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                                            "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                                            "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                                            "conners_teacher_cognitive_score.72m",
                                            "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                                            "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                                            "conners_teacher_hyperactivity_score.72m",
                                            "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                                            "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                                            "SDQ60_mother_hyperactivity", 
                                            "SDQ60_father_hyperactivity", 
                                            "SDQ72_mother_hyperactivity", 
                                            "SDQ72_father_hyperactivity", 
                                            "SDQ72_teacher_hyperactivity",
                                            "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                                            "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                                            "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                                            "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                                            "gestation_age_wks", "Dominic72_cd","Dominic72_odd", "auc_post_cesd")])
print(psych.describeVEC)

print(summary(NEW$mom_age_birth, na.rm = TRUE))
print(summary(NEW$above_college, na.rm = TRUE))
print(summary(NEW$gender_male, na.rm = TRUE))
print(summary(NEW$ADHD, na.rm = TRUE))
print(summary(NEW$Pren_income4, na.rm = TRUE))
print(summary(NEW$Pren_life_events, na.rm = TRUE))
print(summary(NEW$QuintMat_w, na.rm = TRUE))
print(summary(NEW$Smoking_During_Pregnancy, na.rm = TRUE))
print(summary(NEW$AGE_BY_SITE_corr, na.rm = TRUE))
print(summary(NEW$QuintSoc_w, na.rm = TRUE))
print(summary(NEW$birth_size_percent2_x, na.rm = TRUE))
print(summary(NEW$birth_wt_grams, na.rm = TRUE))
print(summary(NEW$gestation_age_wks, na.rm = TRUE))
print(summary(NEW$Sibling, na.rm = TRUE))
print(summary(NEW$PAPA_p4nadhd, na.rm = TRUE))
print(summary(NEW$PAPA_p4_adhd, na.rm = TRUE)) 
print(summary(NEW$Dominic72_ADHD, na.rm = TRUE))
print(summary(NEW$conners_mother_adhd_score.60m, na.rm = TRUE)) 
print(summary(NEW$conners_father_adhd_score.60m, na.rm = TRUE))
print(summary(NEW$conners_mother_adhd_score.72m, na.rm = TRUE))
print(summary(NEW$conners_father_adhd_score.72m, na.rm = TRUE))
print(summary(NEW$conners_teacher_adhd_score.72m, na.rm = TRUE))
print(summary(NEW$Mother, na.rm = TRUE))
print(summary(NEW$Father, na.rm = TRUE))
print(summary(NEW$Teacher, na.rm = TRUE))
print(summary(NEW$Pren_CESD, na.rm = TRUE))
print(summary(NEW$HWB6_CESD, na.rm = TRUE))
print(summary(NEW$HWB12_CESD, na.rm = TRUE))
print(summary(NEW$HWB24_CESD, na.rm = TRUE))
print(summary(NEW$HWB36_CESD, na.rm = TRUE))
print(summary(NEW$HWB48_CESD, na.rm = TRUE))
print(summary(NEW$HWB60_CESD, na.rm = TRUE))
print(summary(NEW$HWB72_CESD, na.rm = TRUE))
print(summary(NEW$auc_post_cesd, na.rm = TRUE))
print(summary(NEW$Dominic72_cd, na.rm = TRUE))
print(summary(NEW$Dominic72_odd, na.rm = TRUE))
print(summary(NEW$Conduct_Disorder, na.rm = TRUE))
print(summary(NEW$ODD, na.rm = TRUE))
print(summary(NEW$Number_Disorder_exclu_ADHD, na.rm = TRUE))
print(summary(NEW$Disorder_no_ADHD, na.rm = TRUE))
print(summary(NEW$CBCL48_sc5raw.y, na.rm = TRUE))
print(summary(NEW$CBCL60_sc5raw.y, na.rm = TRUE))
print(summary(NEW$conners_mother_cognitive_score.60m, na.rm = TRUE))
print(summary(NEW$conners_father_cognitive_score.60m, na.rm = TRUE))
print(summary(NEW$conners_mother_cognitive_score.72m, na.rm = TRUE))
print(summary(NEW$conners_father_cognitive_score.72m, na.rm = TRUE))
print(summary(NEW$conners_teacher_cognitive_score.72m, na.rm = TRUE))
print(summary(NEW$conners_mother_hyperactivity_score.60m, na.rm = TRUE))
print(summary(NEW$conners_father_hyperactivity_score.60m, na.rm = TRUE))
print(summary(NEW$conners_mother_hyperactivity_score.72m, na.rm = TRUE))
print(summary(NEW$conners_father_hyperactivity_score.72m, na.rm = TRUE))
print(summary(NEW$conners_teacher_hyperactivity_score.72m, na.rm = TRUE))
print(summary(NEW$CBCL48_sc6raw.y, na.rm = TRUE))
print(summary(NEW$CBCL60_sc6raw.y, na.rm = TRUE))
print(summary(NEW$SDQ60_mother_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ60_father_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ72_mother_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ72_father_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ72_teacher_hyperactivity, na.rm = TRUE))
print(summary(NEW$PRS_0_001_adhd_child, na.rm = TRUE))
print(summary(NEW$PC1, na.rm = TRUE))
print(summary(NEW$PC2, na.rm = TRUE))
print(summary(NEW$PC3, na.rm = TRUE))

print(IQR(NEW$mom_age_birth, na.rm = TRUE))
print(IQR(NEW$above_college, na.rm = TRUE))
print(IQR(NEW$gender_male, na.rm = TRUE))
print(IQR(NEW$ADHD, na.rm = TRUE))
print(IQR(NEW$Pren_income4, na.rm = TRUE))
print(IQR(NEW$Pren_life_events, na.rm = TRUE))
print(IQR(NEW$QuintMat_w, na.rm = TRUE))
print(IQR(NEW$Smoking_During_Pregnancy, na.rm = TRUE))
print(IQR(NEW$AGE_BY_SITE_corr, na.rm = TRUE))
print(IQR(NEW$QuintSoc_w, na.rm = TRUE))
print(IQR(NEW$birth_size_percent2_x, na.rm = TRUE))
print(IQR(NEW$birth_wt_grams, na.rm = TRUE))
print(IQR(NEW$gestation_age_wks, na.rm = TRUE))
print(IQR(NEW$Sibling, na.rm = TRUE))
print(IQR(NEW$PAPA_p4nadhd, na.rm = TRUE))
print(IQR(NEW$PAPA_p4_adhd, na.rm = TRUE)) 
print(IQR(NEW$Dominic72_ADHD, na.rm = TRUE))
print(IQR(NEW$conners_mother_adhd_score.60m, na.rm = TRUE)) 
print(IQR(NEW$conners_father_adhd_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_mother_adhd_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_father_adhd_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_teacher_adhd_score.72m, na.rm = TRUE))
print(IQR(NEW$Mother, na.rm = TRUE))
print(IQR(NEW$Father, na.rm = TRUE))
print(IQR(NEW$Teacher, na.rm = TRUE))
print(IQR(NEW$Pren_CESD, na.rm = TRUE))
print(IQR(NEW$HWB6_CESD, na.rm = TRUE))
print(IQR(NEW$HWB12_CESD, na.rm = TRUE))
print(IQR(NEW$HWB24_CESD, na.rm = TRUE))
print(IQR(NEW$HWB36_CESD, na.rm = TRUE))
print(IQR(NEW$HWB48_CESD, na.rm = TRUE))
print(IQR(NEW$HWB60_CESD, na.rm = TRUE))
print(IQR(NEW$HWB72_CESD, na.rm = TRUE))
print(IQR(NEW$auc_post_cesd, na.rm = TRUE))
print(IQR(NEW$Dominic72_cd, na.rm = TRUE))
print(IQR(NEW$Dominic72_odd, na.rm = TRUE))
print(IQR(NEW$Conduct_Disorder, na.rm = TRUE))
print(IQR(NEW$ODD, na.rm = TRUE))
print(IQR(NEW$Number_Disorder_exclu_ADHD, na.rm = TRUE))
print(IQR(NEW$Disorder_no_ADHD, na.rm = TRUE))
print(IQR(NEW$CBCL48_sc5raw.y, na.rm = TRUE))
print(IQR(NEW$CBCL60_sc5raw.y, na.rm = TRUE))
print(IQR(NEW$conners_mother_cognitive_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_father_cognitive_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_mother_cognitive_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_father_cognitive_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_teacher_cognitive_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_mother_hyperactivity_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_father_hyperactivity_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_mother_hyperactivity_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_father_hyperactivity_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_teacher_hyperactivity_score.72m, na.rm = TRUE))
print(IQR(NEW$CBCL48_sc6raw.y, na.rm = TRUE))
print(IQR(NEW$CBCL60_sc6raw.y, na.rm = TRUE))
print(IQR(NEW$SDQ60_mother_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ60_father_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ72_mother_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ72_father_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ72_teacher_hyperactivity, na.rm = TRUE))
print(IQR(NEW$PRS_0_001_adhd_child, na.rm = TRUE))
print(IQR(NEW$PC1, na.rm = TRUE))
print(IQR(NEW$PC2, na.rm = TRUE))
print(IQR(NEW$PC3, na.rm = TRUE))

print(IQR(NEW$birth_wt_grams, na.rm = TRUE))	
print(IQR(NEW$PRS_0_001_adhd_child, na.rm = TRUE))


sink()


sink('characteristic_description_by_groups.txt')
# NEW %>%
#   tab_cells("ADHD", "Mother",	"Father",	"Teacher",
#             "gender_male",
#             "mom_age_birth", "above_college", "Pren_income4", 
#             "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
#             "Alcohol_During_Pregnancy",
#             "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
#             "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
#             "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
#             "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
#             "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
#             "conners_teacher_cognitive_score.72m",
#             "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
#             "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
#             "conners_teacher_hyperactivity_score.72m",
#             "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
#             "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
#             "SDQ60_mother_hyperactivity", 
#             "SDQ60_father_hyperactivity", 
#             "SDQ72_mother_hyperactivity", 
#             "SDQ72_father_hyperactivity", 
#             "SDQ72_teacher_hyperactivity",
#             "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
#             "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
#             "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
#             "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
#             "gestation_age_wks", "Dominic72_cd","Dominic72_odd", "auc_post_cesd") %>%
#   #tab_cols(total(), NEW$gender_male, LONG$gender_male_fixed) %>% 
#   tab_cols(total(), NEW$gender_male) %>% 
#   tab_stat_mean_sd_n() %>%
#   tab_last_sig_means(subtable_marks = "both") %>% 
#   tab_pivot() %>% 
#   set_caption("Table with summary statistics and significance marks.")  
# 
# NEW %>%
#   tab_cells("ADHD", "Mother",	"Father",	"Teacher",
#             "gender_male",
#             "mom_age_birth", "above_college", "Pren_income4", 
#             "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
#             "Alcohol_During_Pregnancy",
#             "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
#             "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
#             "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
#             "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
#             "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
#             "conners_teacher_cognitive_score.72m",
#             "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
#             "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
#             "conners_teacher_hyperactivity_score.72m",
#             "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
#             "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
#             "SDQ60_mother_hyperactivity", 
#             "SDQ60_father_hyperactivity", 
#             "SDQ72_mother_hyperactivity", 
#             "SDQ72_father_hyperactivity", 
#             "SDQ72_teacher_hyperactivity",
#             "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
#             "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
#             "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
#             "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
#             "gestation_age_wks", "Dominic72_cd","Dominic72_odd", "auc_post_cesd") %>%
#   #tab_cols(total(), NEW$gender_male, LONG$gender_male_fixed) %>% 
#   tab_cols(total(), NEW$Hamilton) %>% 
#   tab_stat_mean_sd_n() %>%
#   tab_last_sig_means(subtable_marks = "both") %>% 
#   tab_pivot() %>% 
#   set_caption("Table with summary statistics and significance marks.") 
# 
# NEW %>%
#   tab_cells("ADHD", "Mother",	"Father",	"Teacher",
#             "gender_male",
#             "mom_age_birth", "above_college", "Pren_income4", 
#             "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
#             "Alcohol_During_Pregnancy",
#             "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
#             "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
#             "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
#             "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
#             "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
#             "conners_teacher_cognitive_score.72m",
#             "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
#             "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
#             "conners_teacher_hyperactivity_score.72m",
#             "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
#             "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
#             "SDQ60_mother_hyperactivity", 
#             "SDQ60_father_hyperactivity", 
#             "SDQ72_mother_hyperactivity", 
#             "SDQ72_father_hyperactivity", 
#             "SDQ72_teacher_hyperactivity",
#             "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
#             "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
#             "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
#             "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
#             "gestation_age_wks", "Dominic72_cd","Dominic72_odd", "auc_post_cesd") %>%
#   #tab_cols(total(), NEW$gender_male, LONG$gender_male_fixed) %>% 
#   tab_cols(total(), NEW$Sibling) %>% 
#   tab_stat_mean_sd_n() %>%
#   tab_last_sig_means(subtable_marks = "both") %>% 
#   tab_pivot() %>% 
#   set_caption("Table with summary statistics and significance marks.")  
# 
# NEW$ADHD_d<-dicho(
#   NEW$ADHD,
#   dich.by = "median",
#   as.num = TRUE,
#   var.label = NULL,
#   val.labels = NULL,
#   append = TRUE,
#   suffix = "_d"
# )
# head(NEW$ADHD_d)
# 
# NEW %>%
#   tab_cells("ADHD", "Mother",	"Father",	"Teacher",
#             "gender_male",
#             "mom_age_birth", "above_college", "Pren_income4", 
#             "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
#             "Alcohol_During_Pregnancy",
#             "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
#             "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
#             "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
#             "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
#             "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
#             "conners_teacher_cognitive_score.72m",
#             "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
#             "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
#             "conners_teacher_hyperactivity_score.72m",
#             "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
#             "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
#             "SDQ60_mother_hyperactivity", 
#             "SDQ60_father_hyperactivity", 
#             "SDQ72_mother_hyperactivity", 
#             "SDQ72_father_hyperactivity", 
#             "SDQ72_teacher_hyperactivity",
#             "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
#             "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
#             "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
#             "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
#             "gestation_age_wks", "Dominic72_cd","Dominic72_odd", "auc_post_cesd") %>%
#   #tab_cols(total(), NEW$gender_male, LONG$gender_male_fixed) %>% 
#   tab_cols(total(), NEW$ADHD_d) %>% 
#   tab_stat_mean_sd_n() %>%
#   tab_last_sig_means(subtable_marks = "both") %>% 
#   tab_pivot() %>% 
#   set_caption("Table with summary statistics and significance marks.") 

#psych::describe.by(mydata, group,...)
sink('characteristic_description_by_sex.txt')
psych.describeVEC.sex <-psych::describeBy(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                                            "gender_male",
                                            "mom_age_birth", "above_college", "Pren_income4", 
                                            "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                                            "Alcohol_During_Pregnancy",
                                            "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                                            "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                                            "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                                            "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                                            "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                                            "conners_teacher_cognitive_score.72m",
                                            "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                                            "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                                            "conners_teacher_hyperactivity_score.72m",
                                            "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                                            "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                                            "SDQ60_mother_hyperactivity", 
                                            "SDQ60_father_hyperactivity", 
                                            "SDQ72_mother_hyperactivity", 
                                            "SDQ72_father_hyperactivity", 
                                            "SDQ72_teacher_hyperactivity",
                                            "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                                            "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                                            "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                                            "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                         "gestation_age_wks", "Dominic72_cd","Dominic72_odd", "auc_post_cesd")], NEW$gender_male)
print(psych.describeVEC.sex)

sink()

sink('characteristic_description_by_site.txt')
psych.describeVEC.site <-psych::describeBy(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                                                   "gender_male",
                                                   "mom_age_birth", "above_college", "Pren_income4", 
                                                   "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                                                   "Alcohol_During_Pregnancy",
                                                   "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                                                   "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                                                   "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                                                   "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                                                   "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                                                   "conners_teacher_cognitive_score.72m",
                                                   "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                                                   "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                                                   "conners_teacher_hyperactivity_score.72m",
                                                   "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                                                   "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                                                   "SDQ60_mother_hyperactivity", 
                                                   "SDQ60_father_hyperactivity", 
                                                   "SDQ72_mother_hyperactivity", 
                                                   "SDQ72_father_hyperactivity", 
                                                   "SDQ72_teacher_hyperactivity",
                                                   "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                                                   "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                                                   "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                                                   "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                         "gestation_age_wks", "Dominic72_cd","Dominic72_odd", "auc_post_cesd")], NEW$Hamilton)
print(psych.describeVEC.site)
sink()

sink('characteristic_description_by_adhd_dx.txt')
psych.describeVEC.ADHD.DX <-psych::describeBy(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                                                    "gender_male",
                                                    "mom_age_birth", "above_college", "Pren_income4", 
                                                    "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                                                    "Alcohol_During_Pregnancy",
                                                    "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                                                    "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                                                    "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                                                    "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                                                    "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                                                    "conners_teacher_cognitive_score.72m",
                                                    "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                                                    "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                                                    "conners_teacher_hyperactivity_score.72m",
                                                    "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                                                    "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                                                    "SDQ60_mother_hyperactivity", 
                                                    "SDQ60_father_hyperactivity", 
                                                    "SDQ72_mother_hyperactivity", 
                                                    "SDQ72_father_hyperactivity", 
                                                    "SDQ72_teacher_hyperactivity",
                                                    "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                                                    "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                                                    "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                                                    "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                         "gestation_age_wks", "Dominic72_cd","Dominic72_odd", "auc_post_cesd")], NEW$PAPA_p4_adhd)
print(psych.describeVEC.ADHD.DX)
sink()

sink('characteristic_description_by_sibling.txt')
psych.describeVEC.Sibling <-psych::describeBy(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                                                       "gender_male",
                                                       "mom_age_birth", "above_college", "Pren_income4", 
                                                       "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                                                       "Alcohol_During_Pregnancy",
                                                       "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                                                       "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                                                       "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                                                       "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                                                       "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                                                       "conners_teacher_cognitive_score.72m",
                                                       "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                                                       "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                                                       "conners_teacher_hyperactivity_score.72m",
                                                       "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                                                       "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                                                       "SDQ60_mother_hyperactivity", 
                                                       "SDQ60_father_hyperactivity", 
                                                       "SDQ72_mother_hyperactivity", 
                                                       "SDQ72_father_hyperactivity", 
                                                       "SDQ72_teacher_hyperactivity",
                                                       "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                                                       "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                                                       "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                                                       "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                         "gestation_age_wks", "Dominic72_cd","Dominic72_odd", "auc_post_cesd")], NEW$Sibling)
print(psych.describeVEC.Sibling)
sink()

# psych.describeVEC.ADHD <-psych::describeBy(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
#                                                        "gender_male",
#                                                        "mom_age_birth", "above_college", "Pren_income4", 
#                                                        "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
#                                                        "Alcohol_During_Pregnancy",
#                                                        "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
#                                                        "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
#                                                        "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
#                                                        "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
#                                                        "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
#                                                        "conners_teacher_cognitive_score.72m",
#                                                        "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
#                                                        "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
#                                                        "conners_teacher_hyperactivity_score.72m",
#                                                        "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
#                                                        "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
#                                                        "SDQ60_mother_hyperactivity", 
#                                                        "SDQ60_father_hyperactivity", 
#                                                        "SDQ72_mother_hyperactivity", 
#                                                        "SDQ72_father_hyperactivity", 
#                                                        "SDQ72_teacher_hyperactivity",
#                                                        "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
#                                                        "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
#                                                        "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
#                                                        "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
#                          "gestation_age_wks", "Dominic72_cd","Dominic72_odd", "auc_post_cesd")], NEW$ADHD)
# print(psych.describeVEC.ADHD)

sink('characteristic_description_by_adhd_median.txt')
NEW$ADHD_d<-sjmisc::dicho(
     NEW$ADHD,
     dich.by = "median",
     as.num = TRUE,
     var.label = NULL,
     val.labels = NULL,
     append = TRUE,
     suffix = "_d"
   )

psych.describeVEC.ADHD <-psych::describeBy(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                                                   "gender_male",
                                                   "mom_age_birth", "above_college", "Pren_income4", 
                                                   "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                                                   "Alcohol_During_Pregnancy",
                                                   "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                                                   "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                                                   "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                                                   "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                                                   "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                                                   "conners_teacher_cognitive_score.72m",
                                                   "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                                                   "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                                                   "conners_teacher_hyperactivity_score.72m",
                                                   "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                                                   "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                                                   "SDQ60_mother_hyperactivity", 
                                                   "SDQ60_father_hyperactivity", 
                                                   "SDQ72_mother_hyperactivity", 
                                                   "SDQ72_father_hyperactivity", 
                                                   "SDQ72_teacher_hyperactivity",
                                                   "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                                                   "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                                                   "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                                                   "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                                                   "gestation_age_wks", "Dominic72_cd","Dominic72_odd", "auc_post_cesd")], NEW$ADHD_d)
print(psych.describeVEC.ADHD)

sink()

################
################
################
NEW <- read.csv("MAVAN_48M_and_up_jun2020.csv")
# NEW <- filter(gender_male > 1)
NEW <- filter(NEW,gender_male == "1")

sink('characteristic_description_boys.txt')
print(summary(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                      "gender_male",
                      "mom_age_birth", "above_college", "Pren_income4", 
                      "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                      "Alcohol_During_Pregnancy",
                      "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                      "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                      "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                      "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                      "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                      "conners_teacher_cognitive_score.72m",
                      "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                      "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                      "conners_teacher_hyperactivity_score.72m",
                      "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                      "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                      "SDQ60_mother_hyperactivity", 
                      "SDQ60_father_hyperactivity", 
                      "SDQ72_mother_hyperactivity", 
                      "SDQ72_father_hyperactivity", 
                      "SDQ72_teacher_hyperactivity",
                      "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                      "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                      "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                      "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                         "gestation_age_wks", na.rm = TRUE)]))
describeVEC <-Hmisc::describe(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                                      "gender_male",
                                      "mom_age_birth", "above_college", "Pren_income4", 
                                      "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                                      "Alcohol_During_Pregnancy",
                                      "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                                      "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                                      "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                                      "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                                      "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                                      "conners_teacher_cognitive_score.72m",
                                      "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                                      "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                                      "conners_teacher_hyperactivity_score.72m",
                                      "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                                      "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                                      "SDQ60_mother_hyperactivity", 
                                      "SDQ60_father_hyperactivity", 
                                      "SDQ72_mother_hyperactivity", 
                                      "SDQ72_father_hyperactivity", 
                                      "SDQ72_teacher_hyperactivity",
                                      "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                                      "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                                      "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                                      "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                         "gestation_age_wks")], transpose = TRUE, na.rm = TRUE)
print(describeVEC)

print(summary(NEW$mom_age_birth, na.rm = TRUE))
print(summary(NEW$above_college, na.rm = TRUE))
print(summary(NEW$gender_male, na.rm = TRUE))
print(summary(NEW$ADHD, na.rm = TRUE))
print(summary(NEW$Pren_income4, na.rm = TRUE))
print(summary(NEW$Pren_life_events, na.rm = TRUE))
print(summary(NEW$QuintMat_w, na.rm = TRUE))
print(summary(NEW$Smoking_During_Pregnancy, na.rm = TRUE))
print(summary(NEW$AGE_BY_SITE_corr, na.rm = TRUE))
print(summary(NEW$QuintSoc_w, na.rm = TRUE))
print(summary(NEW$birth_size_percent2_x, na.rm = TRUE))
print(summary(NEW$birth_wt_grams, na.rm = TRUE))
print(summary(NEW$gestation_age_wks, na.rm = TRUE))
print(summary(NEW$Sibling, na.rm = TRUE))
print(summary(NEW$PAPA_p4nadhd, na.rm = TRUE))
print(summary(NEW$PAPA_p4_adhd, na.rm = TRUE)) 
print(summary(NEW$Dominic72_ADHD, na.rm = TRUE))
print(summary(NEW$conners_mother_adhd_score.60m, na.rm = TRUE)) 
print(summary(NEW$conners_father_adhd_score.60m, na.rm = TRUE))
print(summary(NEW$conners_mother_adhd_score.72m, na.rm = TRUE))
print(summary(NEW$conners_father_adhd_score.72m, na.rm = TRUE))
print(summary(NEW$conners_teacher_adhd_score.72m, na.rm = TRUE))
print(summary(NEW$Mother, na.rm = TRUE))
print(summary(NEW$Father, na.rm = TRUE))
print(summary(NEW$Teacher, na.rm = TRUE))
print(summary(NEW$Pren_CESD, na.rm = TRUE))
print(summary(NEW$HWB6_CESD, na.rm = TRUE))
print(summary(NEW$HWB12_CESD, na.rm = TRUE))
print(summary(NEW$HWB24_CESD, na.rm = TRUE))
print(summary(NEW$HWB36_CESD, na.rm = TRUE))
print(summary(NEW$HWB48_CESD, na.rm = TRUE))
print(summary(NEW$HWB60_CESD, na.rm = TRUE))
print(summary(NEW$HWB72_CESD, na.rm = TRUE))
print(summary(NEW$auc_post_cesd, na.rm = TRUE))
print(summary(NEW$Dominic72_cd, na.rm = TRUE))
print(summary(NEW$Dominic72_odd, na.rm = TRUE))
print(summary(NEW$Conduct_Disorder, na.rm = TRUE))
print(summary(NEW$ODD, na.rm = TRUE))
print(summary(NEW$Number_Disorder_exclu_ADHD, na.rm = TRUE))
print(summary(NEW$Disorder_no_ADHD, na.rm = TRUE))
print(summary(NEW$CBCL48_sc5raw.y, na.rm = TRUE))
print(summary(NEW$CBCL60_sc5raw.y, na.rm = TRUE))
print(summary(NEW$conners_mother_cognitive_score.60m, na.rm = TRUE))
print(summary(NEW$conners_father_cognitive_score.60m, na.rm = TRUE))
print(summary(NEW$conners_mother_cognitive_score.72m, na.rm = TRUE))
print(summary(NEW$conners_father_cognitive_score.72m, na.rm = TRUE))
print(summary(NEW$conners_teacher_cognitive_score.72m, na.rm = TRUE))
print(summary(NEW$conners_mother_hyperactivity_score.60m, na.rm = TRUE))
print(summary(NEW$conners_father_hyperactivity_score.60m, na.rm = TRUE))
print(summary(NEW$conners_mother_hyperactivity_score.72m, na.rm = TRUE))
print(summary(NEW$conners_father_hyperactivity_score.72m, na.rm = TRUE))
print(summary(NEW$conners_teacher_hyperactivity_score.72m, na.rm = TRUE))
print(summary(NEW$CBCL48_sc6raw.y, na.rm = TRUE))
print(summary(NEW$CBCL60_sc6raw.y, na.rm = TRUE))
print(summary(NEW$SDQ60_mother_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ60_father_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ72_mother_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ72_father_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ72_teacher_hyperactivity, na.rm = TRUE))
print(summary(NEW$PRS_0_001_adhd_child, na.rm = TRUE))
print(summary(NEW$PC1, na.rm = TRUE))
print(summary(NEW$PC2, na.rm = TRUE))
print(summary(NEW$PC3, na.rm = TRUE))

print(IQR(NEW$mom_age_birth, na.rm = TRUE))
print(IQR(NEW$above_college, na.rm = TRUE))
print(IQR(NEW$gender_male, na.rm = TRUE))
print(IQR(NEW$ADHD, na.rm = TRUE))
print(IQR(NEW$Pren_income4, na.rm = TRUE))
print(IQR(NEW$Pren_life_events, na.rm = TRUE))
print(IQR(NEW$QuintMat_w, na.rm = TRUE))
print(IQR(NEW$Smoking_During_Pregnancy, na.rm = TRUE))
print(IQR(NEW$AGE_BY_SITE_corr, na.rm = TRUE))
print(IQR(NEW$QuintSoc_w, na.rm = TRUE))
print(IQR(NEW$birth_size_percent2_x, na.rm = TRUE))
print(IQR(NEW$birth_wt_grams, na.rm = TRUE))
print(IQR(NEW$gestation_age_wks, na.rm = TRUE))
print(IQR(NEW$Sibling, na.rm = TRUE))
print(IQR(NEW$PAPA_p4nadhd, na.rm = TRUE))
print(IQR(NEW$PAPA_p4_adhd, na.rm = TRUE)) 
print(IQR(NEW$Dominic72_ADHD, na.rm = TRUE))
print(IQR(NEW$conners_mother_adhd_score.60m, na.rm = TRUE)) 
print(IQR(NEW$conners_father_adhd_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_mother_adhd_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_father_adhd_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_teacher_adhd_score.72m, na.rm = TRUE))
print(IQR(NEW$Mother, na.rm = TRUE))
print(IQR(NEW$Father, na.rm = TRUE))
print(IQR(NEW$Teacher, na.rm = TRUE))
print(IQR(NEW$Pren_CESD, na.rm = TRUE))
print(IQR(NEW$HWB6_CESD, na.rm = TRUE))
print(IQR(NEW$HWB12_CESD, na.rm = TRUE))
print(IQR(NEW$HWB24_CESD, na.rm = TRUE))
print(IQR(NEW$HWB36_CESD, na.rm = TRUE))
print(IQR(NEW$HWB48_CESD, na.rm = TRUE))
print(IQR(NEW$HWB60_CESD, na.rm = TRUE))
print(IQR(NEW$HWB72_CESD, na.rm = TRUE))
print(IQR(NEW$auc_post_cesd, na.rm = TRUE))
print(IQR(NEW$Dominic72_cd, na.rm = TRUE))
print(IQR(NEW$Dominic72_odd, na.rm = TRUE))
print(IQR(NEW$Conduct_Disorder, na.rm = TRUE))
print(IQR(NEW$ODD, na.rm = TRUE))
print(IQR(NEW$Number_Disorder_exclu_ADHD, na.rm = TRUE))
print(IQR(NEW$Disorder_no_ADHD, na.rm = TRUE))
print(IQR(NEW$CBCL48_sc5raw.y, na.rm = TRUE))
print(IQR(NEW$CBCL60_sc5raw.y, na.rm = TRUE))
print(IQR(NEW$conners_mother_cognitive_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_father_cognitive_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_mother_cognitive_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_father_cognitive_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_teacher_cognitive_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_mother_hyperactivity_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_father_hyperactivity_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_mother_hyperactivity_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_father_hyperactivity_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_teacher_hyperactivity_score.72m, na.rm = TRUE))
print(IQR(NEW$CBCL48_sc6raw.y, na.rm = TRUE))
print(IQR(NEW$CBCL60_sc6raw.y, na.rm = TRUE))
print(IQR(NEW$SDQ60_mother_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ60_father_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ72_mother_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ72_father_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ72_teacher_hyperactivity, na.rm = TRUE))
print(IQR(NEW$PRS_0_001_adhd_child, na.rm = TRUE))
print(IQR(NEW$PC1, na.rm = TRUE))
print(IQR(NEW$PC2, na.rm = TRUE))
print(IQR(NEW$PC3, na.rm = TRUE))

print(IQR(NEW$birth_wt_grams, na.rm = TRUE))	
print(IQR(NEW$PRS_0_001_adhd_child, na.rm = TRUE))

sink()

NEW <- read.csv("MAVAN_48M_and_up_jun2020.csv")
NEW <- filter(NEW,gender_male == "0")

sink('characteristic_description_girls.txt')
print(summary(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                      "gender_male",
                      "mom_age_birth", "above_college", "Pren_income4", 
                      "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                      "Alcohol_During_Pregnancy",
                      "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                      "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                      "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                      "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                      "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                      "conners_teacher_cognitive_score.72m",
                      "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                      "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                      "conners_teacher_hyperactivity_score.72m",
                      "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                      "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                      "SDQ60_mother_hyperactivity", 
                      "SDQ60_father_hyperactivity", 
                      "SDQ72_mother_hyperactivity", 
                      "SDQ72_father_hyperactivity", 
                      "SDQ72_teacher_hyperactivity",
                      "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                      "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                      "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                      "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                         "gestation_age_wks")]))
describeVEC <-Hmisc::describe(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                                      "gender_male",
                                      "mom_age_birth", "above_college", "Pren_income4", 
                                      "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                                      "Alcohol_During_Pregnancy",
                                      "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                                      "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                                      "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                                      "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                                      "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                                      "conners_teacher_cognitive_score.72m",
                                      "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                                      "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                                      "conners_teacher_hyperactivity_score.72m",
                                      "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                                      "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                                      "SDQ60_mother_hyperactivity", 
                                      "SDQ60_father_hyperactivity", 
                                      "SDQ72_mother_hyperactivity", 
                                      "SDQ72_father_hyperactivity", 
                                      "SDQ72_teacher_hyperactivity",
                                      "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                                      "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                                      "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                                      "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                         "gestation_age_wks")], transpose = TRUE)
print(describeVEC)
print(summary(NEW$mom_age_birth, na.rm = TRUE))
print(summary(NEW$above_college, na.rm = TRUE))
print(summary(NEW$gender_male, na.rm = TRUE))
print(summary(NEW$ADHD, na.rm = TRUE))
print(summary(NEW$Pren_income4, na.rm = TRUE))
print(summary(NEW$Pren_life_events, na.rm = TRUE))
print(summary(NEW$QuintMat_w, na.rm = TRUE))
print(summary(NEW$Smoking_During_Pregnancy, na.rm = TRUE))
print(summary(NEW$AGE_BY_SITE_corr, na.rm = TRUE))
print(summary(NEW$QuintSoc_w, na.rm = TRUE))
print(summary(NEW$birth_size_percent2_x, na.rm = TRUE))
print(summary(NEW$birth_wt_grams, na.rm = TRUE))
print(summary(NEW$gestation_age_wks, na.rm = TRUE))
print(summary(NEW$Sibling, na.rm = TRUE))
print(summary(NEW$PAPA_p4nadhd, na.rm = TRUE))
print(summary(NEW$PAPA_p4_adhd, na.rm = TRUE)) 
print(summary(NEW$Dominic72_ADHD, na.rm = TRUE))
print(summary(NEW$conners_mother_adhd_score.60m, na.rm = TRUE)) 
print(summary(NEW$conners_father_adhd_score.60m, na.rm = TRUE))
print(summary(NEW$conners_mother_adhd_score.72m, na.rm = TRUE))
print(summary(NEW$conners_father_adhd_score.72m, na.rm = TRUE))
print(summary(NEW$conners_teacher_adhd_score.72m, na.rm = TRUE))
print(summary(NEW$Mother, na.rm = TRUE))
print(summary(NEW$Father, na.rm = TRUE))
print(summary(NEW$Teacher, na.rm = TRUE))
print(summary(NEW$Pren_CESD, na.rm = TRUE))
print(summary(NEW$HWB6_CESD, na.rm = TRUE))
print(summary(NEW$HWB12_CESD, na.rm = TRUE))
print(summary(NEW$HWB24_CESD, na.rm = TRUE))
print(summary(NEW$HWB36_CESD, na.rm = TRUE))
print(summary(NEW$HWB48_CESD, na.rm = TRUE))
print(summary(NEW$HWB60_CESD, na.rm = TRUE))
print(summary(NEW$HWB72_CESD, na.rm = TRUE))
print(summary(NEW$auc_post_cesd, na.rm = TRUE))
print(summary(NEW$Dominic72_cd, na.rm = TRUE))
print(summary(NEW$Dominic72_odd, na.rm = TRUE))
print(summary(NEW$Conduct_Disorder, na.rm = TRUE))
print(summary(NEW$ODD, na.rm = TRUE))
print(summary(NEW$Number_Disorder_exclu_ADHD, na.rm = TRUE))
print(summary(NEW$Disorder_no_ADHD, na.rm = TRUE))
print(summary(NEW$CBCL48_sc5raw.y, na.rm = TRUE))
print(summary(NEW$CBCL60_sc5raw.y, na.rm = TRUE))
print(summary(NEW$conners_mother_cognitive_score.60m, na.rm = TRUE))
print(summary(NEW$conners_father_cognitive_score.60m, na.rm = TRUE))
print(summary(NEW$conners_mother_cognitive_score.72m, na.rm = TRUE))
print(summary(NEW$conners_father_cognitive_score.72m, na.rm = TRUE))
print(summary(NEW$conners_teacher_cognitive_score.72m, na.rm = TRUE))
print(summary(NEW$conners_mother_hyperactivity_score.60m, na.rm = TRUE))
print(summary(NEW$conners_father_hyperactivity_score.60m, na.rm = TRUE))
print(summary(NEW$conners_mother_hyperactivity_score.72m, na.rm = TRUE))
print(summary(NEW$conners_father_hyperactivity_score.72m, na.rm = TRUE))
print(summary(NEW$conners_teacher_hyperactivity_score.72m, na.rm = TRUE))
print(summary(NEW$CBCL48_sc6raw.y, na.rm = TRUE))
print(summary(NEW$CBCL60_sc6raw.y, na.rm = TRUE))
print(summary(NEW$SDQ60_mother_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ60_father_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ72_mother_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ72_father_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ72_teacher_hyperactivity, na.rm = TRUE))
print(summary(NEW$PRS_0_001_adhd_child, na.rm = TRUE))
print(summary(NEW$PC1, na.rm = TRUE))
print(summary(NEW$PC2, na.rm = TRUE))
print(summary(NEW$PC3, na.rm = TRUE))

print(IQR(NEW$mom_age_birth, na.rm = TRUE))
print(IQR(NEW$above_college, na.rm = TRUE))
print(IQR(NEW$gender_male, na.rm = TRUE))
print(IQR(NEW$ADHD, na.rm = TRUE))
print(IQR(NEW$Pren_income4, na.rm = TRUE))
print(IQR(NEW$Pren_life_events, na.rm = TRUE))
print(IQR(NEW$QuintMat_w, na.rm = TRUE))
print(IQR(NEW$Smoking_During_Pregnancy, na.rm = TRUE))
print(IQR(NEW$AGE_BY_SITE_corr, na.rm = TRUE))
print(IQR(NEW$QuintSoc_w, na.rm = TRUE))
print(IQR(NEW$birth_size_percent2_x, na.rm = TRUE))
print(IQR(NEW$birth_wt_grams, na.rm = TRUE))
print(IQR(NEW$gestation_age_wks, na.rm = TRUE))
print(IQR(NEW$Sibling, na.rm = TRUE))
print(IQR(NEW$PAPA_p4nadhd, na.rm = TRUE))
print(IQR(NEW$PAPA_p4_adhd, na.rm = TRUE)) 
print(IQR(NEW$Dominic72_ADHD, na.rm = TRUE))
print(IQR(NEW$conners_mother_adhd_score.60m, na.rm = TRUE)) 
print(IQR(NEW$conners_father_adhd_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_mother_adhd_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_father_adhd_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_teacher_adhd_score.72m, na.rm = TRUE))
print(IQR(NEW$Mother, na.rm = TRUE))
print(IQR(NEW$Father, na.rm = TRUE))
print(IQR(NEW$Teacher, na.rm = TRUE))
print(IQR(NEW$Pren_CESD, na.rm = TRUE))
print(IQR(NEW$HWB6_CESD, na.rm = TRUE))
print(IQR(NEW$HWB12_CESD, na.rm = TRUE))
print(IQR(NEW$HWB24_CESD, na.rm = TRUE))
print(IQR(NEW$HWB36_CESD, na.rm = TRUE))
print(IQR(NEW$HWB48_CESD, na.rm = TRUE))
print(IQR(NEW$HWB60_CESD, na.rm = TRUE))
print(IQR(NEW$HWB72_CESD, na.rm = TRUE))
print(IQR(NEW$auc_post_cesd, na.rm = TRUE))
print(IQR(NEW$Dominic72_cd, na.rm = TRUE))
print(IQR(NEW$Dominic72_odd, na.rm = TRUE))
print(IQR(NEW$Conduct_Disorder, na.rm = TRUE))
print(IQR(NEW$ODD, na.rm = TRUE))
print(IQR(NEW$Number_Disorder_exclu_ADHD, na.rm = TRUE))
print(IQR(NEW$Disorder_no_ADHD, na.rm = TRUE))
print(IQR(NEW$CBCL48_sc5raw.y, na.rm = TRUE))
print(IQR(NEW$CBCL60_sc5raw.y, na.rm = TRUE))
print(IQR(NEW$conners_mother_cognitive_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_father_cognitive_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_mother_cognitive_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_father_cognitive_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_teacher_cognitive_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_mother_hyperactivity_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_father_hyperactivity_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_mother_hyperactivity_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_father_hyperactivity_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_teacher_hyperactivity_score.72m, na.rm = TRUE))
print(IQR(NEW$CBCL48_sc6raw.y, na.rm = TRUE))
print(IQR(NEW$CBCL60_sc6raw.y, na.rm = TRUE))
print(IQR(NEW$SDQ60_mother_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ60_father_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ72_mother_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ72_father_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ72_teacher_hyperactivity, na.rm = TRUE))
print(IQR(NEW$PRS_0_001_adhd_child, na.rm = TRUE))
print(IQR(NEW$PC1, na.rm = TRUE))
print(IQR(NEW$PC2, na.rm = TRUE))
print(IQR(NEW$PC3, na.rm = TRUE))

print(IQR(NEW$birth_wt_grams, na.rm = TRUE))	
print(IQR(NEW$PRS_0_001_adhd_child, na.rm = TRUE))
sink()

NEW <- read.csv("MAVAN_48M_and_up_jun2020.csv")
NEW <- filter(NEW,Sibling == "1")

sink('characteristic_description_first_child.txt')
print(summary(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                      "gender_male",
                      "mom_age_birth", "above_college", "Pren_income4", 
                      "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                      "Alcohol_During_Pregnancy",
                      "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                      "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                      "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                      "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                      "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                      "conners_teacher_cognitive_score.72m",
                      "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                      "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                      "conners_teacher_hyperactivity_score.72m",
                      "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                      "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                      "SDQ60_mother_hyperactivity", 
                      "SDQ60_father_hyperactivity", 
                      "SDQ72_mother_hyperactivity", 
                      "SDQ72_father_hyperactivity", 
                      "SDQ72_teacher_hyperactivity",
                      "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                      "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                      "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                      "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                         "gestation_age_wks")]))
describeVEC <-Hmisc::describe(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                                      "gender_male",
                                      "mom_age_birth", "above_college", "Pren_income4", 
                                      "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                                      "Alcohol_During_Pregnancy",
                                      "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                                      "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                                      "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                                      "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                                      "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                                      "conners_teacher_cognitive_score.72m",
                                      "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                                      "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                                      "conners_teacher_hyperactivity_score.72m",
                                      "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                                      "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                                      "SDQ60_mother_hyperactivity", 
                                      "SDQ60_father_hyperactivity", 
                                      "SDQ72_mother_hyperactivity", 
                                      "SDQ72_father_hyperactivity", 
                                      "SDQ72_teacher_hyperactivity",
                                      "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                                      "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                                      "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                                      "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                         "gestation_age_wks")], transpose = TRUE)
print(describeVEC)
print(summary(NEW$mom_age_birth, na.rm = TRUE))
print(summary(NEW$above_college, na.rm = TRUE))
print(summary(NEW$gender_male, na.rm = TRUE))
print(summary(NEW$ADHD, na.rm = TRUE))
print(summary(NEW$Pren_income4, na.rm = TRUE))
print(summary(NEW$Pren_life_events, na.rm = TRUE))
print(summary(NEW$QuintMat_w, na.rm = TRUE))
print(summary(NEW$Smoking_During_Pregnancy, na.rm = TRUE))
print(summary(NEW$AGE_BY_SITE_corr, na.rm = TRUE))
print(summary(NEW$QuintSoc_w, na.rm = TRUE))
print(summary(NEW$birth_size_percent2_x, na.rm = TRUE))
print(summary(NEW$birth_wt_grams, na.rm = TRUE))
print(summary(NEW$gestation_age_wks, na.rm = TRUE))
print(summary(NEW$Sibling, na.rm = TRUE))
print(summary(NEW$PAPA_p4nadhd, na.rm = TRUE))
print(summary(NEW$PAPA_p4_adhd, na.rm = TRUE)) 
print(summary(NEW$Dominic72_ADHD, na.rm = TRUE))
print(summary(NEW$conners_mother_adhd_score.60m, na.rm = TRUE)) 
print(summary(NEW$conners_father_adhd_score.60m, na.rm = TRUE))
print(summary(NEW$conners_mother_adhd_score.72m, na.rm = TRUE))
print(summary(NEW$conners_father_adhd_score.72m, na.rm = TRUE))
print(summary(NEW$conners_teacher_adhd_score.72m, na.rm = TRUE))
print(summary(NEW$Mother, na.rm = TRUE))
print(summary(NEW$Father, na.rm = TRUE))
print(summary(NEW$Teacher, na.rm = TRUE))
print(summary(NEW$Pren_CESD, na.rm = TRUE))
print(summary(NEW$HWB6_CESD, na.rm = TRUE))
print(summary(NEW$HWB12_CESD, na.rm = TRUE))
print(summary(NEW$HWB24_CESD, na.rm = TRUE))
print(summary(NEW$HWB36_CESD, na.rm = TRUE))
print(summary(NEW$HWB48_CESD, na.rm = TRUE))
print(summary(NEW$HWB60_CESD, na.rm = TRUE))
print(summary(NEW$HWB72_CESD, na.rm = TRUE))
print(summary(NEW$auc_post_cesd, na.rm = TRUE))
print(summary(NEW$Dominic72_cd, na.rm = TRUE))
print(summary(NEW$Dominic72_odd, na.rm = TRUE))
print(summary(NEW$Conduct_Disorder, na.rm = TRUE))
print(summary(NEW$ODD, na.rm = TRUE))
print(summary(NEW$Number_Disorder_exclu_ADHD, na.rm = TRUE))
print(summary(NEW$Disorder_no_ADHD, na.rm = TRUE))
print(summary(NEW$CBCL48_sc5raw.y, na.rm = TRUE))
print(summary(NEW$CBCL60_sc5raw.y, na.rm = TRUE))
print(summary(NEW$conners_mother_cognitive_score.60m, na.rm = TRUE))
print(summary(NEW$conners_father_cognitive_score.60m, na.rm = TRUE))
print(summary(NEW$conners_mother_cognitive_score.72m, na.rm = TRUE))
print(summary(NEW$conners_father_cognitive_score.72m, na.rm = TRUE))
print(summary(NEW$conners_teacher_cognitive_score.72m, na.rm = TRUE))
print(summary(NEW$conners_mother_hyperactivity_score.60m, na.rm = TRUE))
print(summary(NEW$conners_father_hyperactivity_score.60m, na.rm = TRUE))
print(summary(NEW$conners_mother_hyperactivity_score.72m, na.rm = TRUE))
print(summary(NEW$conners_father_hyperactivity_score.72m, na.rm = TRUE))
print(summary(NEW$conners_teacher_hyperactivity_score.72m, na.rm = TRUE))
print(summary(NEW$CBCL48_sc6raw.y, na.rm = TRUE))
print(summary(NEW$CBCL60_sc6raw.y, na.rm = TRUE))
print(summary(NEW$SDQ60_mother_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ60_father_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ72_mother_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ72_father_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ72_teacher_hyperactivity, na.rm = TRUE))
print(summary(NEW$PRS_0_001_adhd_child, na.rm = TRUE))
print(summary(NEW$PC1, na.rm = TRUE))
print(summary(NEW$PC2, na.rm = TRUE))
print(summary(NEW$PC3, na.rm = TRUE))

print(IQR(NEW$mom_age_birth, na.rm = TRUE))
print(IQR(NEW$above_college, na.rm = TRUE))
print(IQR(NEW$gender_male, na.rm = TRUE))
print(IQR(NEW$ADHD, na.rm = TRUE))
print(IQR(NEW$Pren_income4, na.rm = TRUE))
print(IQR(NEW$Pren_life_events, na.rm = TRUE))
print(IQR(NEW$QuintMat_w, na.rm = TRUE))
print(IQR(NEW$Smoking_During_Pregnancy, na.rm = TRUE))
print(IQR(NEW$AGE_BY_SITE_corr, na.rm = TRUE))
print(IQR(NEW$QuintSoc_w, na.rm = TRUE))
print(IQR(NEW$birth_size_percent2_x, na.rm = TRUE))
print(IQR(NEW$birth_wt_grams, na.rm = TRUE))
print(IQR(NEW$gestation_age_wks, na.rm = TRUE))
print(IQR(NEW$Sibling, na.rm = TRUE))
print(IQR(NEW$PAPA_p4nadhd, na.rm = TRUE))
print(IQR(NEW$PAPA_p4_adhd, na.rm = TRUE)) 
print(IQR(NEW$Dominic72_ADHD, na.rm = TRUE))
print(IQR(NEW$conners_mother_adhd_score.60m, na.rm = TRUE)) 
print(IQR(NEW$conners_father_adhd_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_mother_adhd_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_father_adhd_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_teacher_adhd_score.72m, na.rm = TRUE))
print(IQR(NEW$Mother, na.rm = TRUE))
print(IQR(NEW$Father, na.rm = TRUE))
print(IQR(NEW$Teacher, na.rm = TRUE))
print(IQR(NEW$Pren_CESD, na.rm = TRUE))
print(IQR(NEW$HWB6_CESD, na.rm = TRUE))
print(IQR(NEW$HWB12_CESD, na.rm = TRUE))
print(IQR(NEW$HWB24_CESD, na.rm = TRUE))
print(IQR(NEW$HWB36_CESD, na.rm = TRUE))
print(IQR(NEW$HWB48_CESD, na.rm = TRUE))
print(IQR(NEW$HWB60_CESD, na.rm = TRUE))
print(IQR(NEW$HWB72_CESD, na.rm = TRUE))
print(IQR(NEW$auc_post_cesd, na.rm = TRUE))
print(IQR(NEW$Dominic72_cd, na.rm = TRUE))
print(IQR(NEW$Dominic72_odd, na.rm = TRUE))
print(IQR(NEW$Conduct_Disorder, na.rm = TRUE))
print(IQR(NEW$ODD, na.rm = TRUE))
print(IQR(NEW$Number_Disorder_exclu_ADHD, na.rm = TRUE))
print(IQR(NEW$Disorder_no_ADHD, na.rm = TRUE))
print(IQR(NEW$CBCL48_sc5raw.y, na.rm = TRUE))
print(IQR(NEW$CBCL60_sc5raw.y, na.rm = TRUE))
print(IQR(NEW$conners_mother_cognitive_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_father_cognitive_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_mother_cognitive_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_father_cognitive_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_teacher_cognitive_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_mother_hyperactivity_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_father_hyperactivity_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_mother_hyperactivity_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_father_hyperactivity_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_teacher_hyperactivity_score.72m, na.rm = TRUE))
print(IQR(NEW$CBCL48_sc6raw.y, na.rm = TRUE))
print(IQR(NEW$CBCL60_sc6raw.y, na.rm = TRUE))
print(IQR(NEW$SDQ60_mother_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ60_father_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ72_mother_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ72_father_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ72_teacher_hyperactivity, na.rm = TRUE))
print(IQR(NEW$PRS_0_001_adhd_child, na.rm = TRUE))
print(IQR(NEW$PC1, na.rm = TRUE))
print(IQR(NEW$PC2, na.rm = TRUE))
print(IQR(NEW$PC3, na.rm = TRUE))

print(IQR(NEW$birth_wt_grams, na.rm = TRUE))	
print(IQR(NEW$PRS_0_001_adhd_child, na.rm = TRUE))

sink()

NEW <- read.csv("MAVAN_48M_and_up_jun2020.csv")
NEW <- filter(NEW,Sibling == "0")

sink('characteristic_description_sibling.txt')
print(summary(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                      "gender_male",
                      "mom_age_birth", "above_college", "Pren_income4", 
                      "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                      "Alcohol_During_Pregnancy",
                      "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                      "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                      "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                      "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                      "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                      "conners_teacher_cognitive_score.72m",
                      "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                      "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                      "conners_teacher_hyperactivity_score.72m",
                      "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                      "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                      "SDQ60_mother_hyperactivity", 
                      "SDQ60_father_hyperactivity", 
                      "SDQ72_mother_hyperactivity", 
                      "SDQ72_father_hyperactivity", 
                      "SDQ72_teacher_hyperactivity",
                      "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                      "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                      "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                      "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                         "gestation_age_wks")]))
describeVEC <-Hmisc::describe(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                                      "gender_male",
                                      "mom_age_birth", "above_college", "Pren_income4", 
                                      "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                                      "Alcohol_During_Pregnancy",
                                      "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                                      "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                                      "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                                      "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                                      "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                                      "conners_teacher_cognitive_score.72m",
                                      "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                                      "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                                      "conners_teacher_hyperactivity_score.72m",
                                      "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                                      "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                                      "SDQ60_mother_hyperactivity", 
                                      "SDQ60_father_hyperactivity", 
                                      "SDQ72_mother_hyperactivity", 
                                      "SDQ72_father_hyperactivity", 
                                      "SDQ72_teacher_hyperactivity",
                                      "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                                      "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                                      "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                                      "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                         "gestation_age_wks")], transpose = TRUE)
print(describeVEC)
print(summary(NEW$mom_age_birth, na.rm = TRUE))
print(summary(NEW$above_college, na.rm = TRUE))
print(summary(NEW$gender_male, na.rm = TRUE))
print(summary(NEW$ADHD, na.rm = TRUE))
print(summary(NEW$Pren_income4, na.rm = TRUE))
print(summary(NEW$Pren_life_events, na.rm = TRUE))
print(summary(NEW$QuintMat_w, na.rm = TRUE))
print(summary(NEW$Smoking_During_Pregnancy, na.rm = TRUE))
print(summary(NEW$AGE_BY_SITE_corr, na.rm = TRUE))
print(summary(NEW$QuintSoc_w, na.rm = TRUE))
print(summary(NEW$birth_size_percent2_x, na.rm = TRUE))
print(summary(NEW$birth_wt_grams, na.rm = TRUE))
print(summary(NEW$gestation_age_wks, na.rm = TRUE))
print(summary(NEW$Sibling, na.rm = TRUE))
print(summary(NEW$PAPA_p4nadhd, na.rm = TRUE))
print(summary(NEW$PAPA_p4_adhd, na.rm = TRUE)) 
print(summary(NEW$Dominic72_ADHD, na.rm = TRUE))
print(summary(NEW$conners_mother_adhd_score.60m, na.rm = TRUE)) 
print(summary(NEW$conners_father_adhd_score.60m, na.rm = TRUE))
print(summary(NEW$conners_mother_adhd_score.72m, na.rm = TRUE))
print(summary(NEW$conners_father_adhd_score.72m, na.rm = TRUE))
print(summary(NEW$conners_teacher_adhd_score.72m, na.rm = TRUE))
print(summary(NEW$Mother, na.rm = TRUE))
print(summary(NEW$Father, na.rm = TRUE))
print(summary(NEW$Teacher, na.rm = TRUE))
print(summary(NEW$Pren_CESD, na.rm = TRUE))
print(summary(NEW$HWB6_CESD, na.rm = TRUE))
print(summary(NEW$HWB12_CESD, na.rm = TRUE))
print(summary(NEW$HWB24_CESD, na.rm = TRUE))
print(summary(NEW$HWB36_CESD, na.rm = TRUE))
print(summary(NEW$HWB48_CESD, na.rm = TRUE))
print(summary(NEW$HWB60_CESD, na.rm = TRUE))
print(summary(NEW$HWB72_CESD, na.rm = TRUE))
print(summary(NEW$auc_post_cesd, na.rm = TRUE))
print(summary(NEW$Dominic72_cd, na.rm = TRUE))
print(summary(NEW$Dominic72_odd, na.rm = TRUE))
print(summary(NEW$Conduct_Disorder, na.rm = TRUE))
print(summary(NEW$ODD, na.rm = TRUE))
print(summary(NEW$Number_Disorder_exclu_ADHD, na.rm = TRUE))
print(summary(NEW$Disorder_no_ADHD, na.rm = TRUE))
print(summary(NEW$CBCL48_sc5raw.y, na.rm = TRUE))
print(summary(NEW$CBCL60_sc5raw.y, na.rm = TRUE))
print(summary(NEW$conners_mother_cognitive_score.60m, na.rm = TRUE))
print(summary(NEW$conners_father_cognitive_score.60m, na.rm = TRUE))
print(summary(NEW$conners_mother_cognitive_score.72m, na.rm = TRUE))
print(summary(NEW$conners_father_cognitive_score.72m, na.rm = TRUE))
print(summary(NEW$conners_teacher_cognitive_score.72m, na.rm = TRUE))
print(summary(NEW$conners_mother_hyperactivity_score.60m, na.rm = TRUE))
print(summary(NEW$conners_father_hyperactivity_score.60m, na.rm = TRUE))
print(summary(NEW$conners_mother_hyperactivity_score.72m, na.rm = TRUE))
print(summary(NEW$conners_father_hyperactivity_score.72m, na.rm = TRUE))
print(summary(NEW$conners_teacher_hyperactivity_score.72m, na.rm = TRUE))
print(summary(NEW$CBCL48_sc6raw.y, na.rm = TRUE))
print(summary(NEW$CBCL60_sc6raw.y, na.rm = TRUE))
print(summary(NEW$SDQ60_mother_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ60_father_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ72_mother_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ72_father_hyperactivity, na.rm = TRUE))
print(summary(NEW$SDQ72_teacher_hyperactivity, na.rm = TRUE))
print(summary(NEW$PRS_0_001_adhd_child, na.rm = TRUE))
print(summary(NEW$PC1, na.rm = TRUE))
print(summary(NEW$PC2, na.rm = TRUE))
print(summary(NEW$PC3, na.rm = TRUE))

print(IQR(NEW$mom_age_birth, na.rm = TRUE))
print(IQR(NEW$above_college, na.rm = TRUE))
print(IQR(NEW$gender_male, na.rm = TRUE))
print(IQR(NEW$ADHD, na.rm = TRUE))
print(IQR(NEW$Pren_income4, na.rm = TRUE))
print(IQR(NEW$Pren_life_events, na.rm = TRUE))
print(IQR(NEW$QuintMat_w, na.rm = TRUE))
print(IQR(NEW$Smoking_During_Pregnancy, na.rm = TRUE))
print(IQR(NEW$AGE_BY_SITE_corr, na.rm = TRUE))
print(IQR(NEW$QuintSoc_w, na.rm = TRUE))
print(IQR(NEW$birth_size_percent2_x, na.rm = TRUE))
print(IQR(NEW$birth_wt_grams, na.rm = TRUE))
print(IQR(NEW$gestation_age_wks, na.rm = TRUE))
print(IQR(NEW$Sibling, na.rm = TRUE))
print(IQR(NEW$PAPA_p4nadhd, na.rm = TRUE))
print(IQR(NEW$PAPA_p4_adhd, na.rm = TRUE)) 
print(IQR(NEW$Dominic72_ADHD, na.rm = TRUE))
print(IQR(NEW$conners_mother_adhd_score.60m, na.rm = TRUE)) 
print(IQR(NEW$conners_father_adhd_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_mother_adhd_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_father_adhd_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_teacher_adhd_score.72m, na.rm = TRUE))
print(IQR(NEW$Mother, na.rm = TRUE))
print(IQR(NEW$Father, na.rm = TRUE))
print(IQR(NEW$Teacher, na.rm = TRUE))
print(IQR(NEW$Pren_CESD, na.rm = TRUE))
print(IQR(NEW$HWB6_CESD, na.rm = TRUE))
print(IQR(NEW$HWB12_CESD, na.rm = TRUE))
print(IQR(NEW$HWB24_CESD, na.rm = TRUE))
print(IQR(NEW$HWB36_CESD, na.rm = TRUE))
print(IQR(NEW$HWB48_CESD, na.rm = TRUE))
print(IQR(NEW$HWB60_CESD, na.rm = TRUE))
print(IQR(NEW$HWB72_CESD, na.rm = TRUE))
print(IQR(NEW$auc_post_cesd, na.rm = TRUE))
print(IQR(NEW$Dominic72_cd, na.rm = TRUE))
print(IQR(NEW$Dominic72_odd, na.rm = TRUE))
print(IQR(NEW$Conduct_Disorder, na.rm = TRUE))
print(IQR(NEW$ODD, na.rm = TRUE))
print(IQR(NEW$Number_Disorder_exclu_ADHD, na.rm = TRUE))
print(IQR(NEW$Disorder_no_ADHD, na.rm = TRUE))
print(IQR(NEW$CBCL48_sc5raw.y, na.rm = TRUE))
print(IQR(NEW$CBCL60_sc5raw.y, na.rm = TRUE))
print(IQR(NEW$conners_mother_cognitive_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_father_cognitive_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_mother_cognitive_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_father_cognitive_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_teacher_cognitive_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_mother_hyperactivity_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_father_hyperactivity_score.60m, na.rm = TRUE))
print(IQR(NEW$conners_mother_hyperactivity_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_father_hyperactivity_score.72m, na.rm = TRUE))
print(IQR(NEW$conners_teacher_hyperactivity_score.72m, na.rm = TRUE))
print(IQR(NEW$CBCL48_sc6raw.y, na.rm = TRUE))
print(IQR(NEW$CBCL60_sc6raw.y, na.rm = TRUE))
print(IQR(NEW$SDQ60_mother_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ60_father_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ72_mother_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ72_father_hyperactivity, na.rm = TRUE))
print(IQR(NEW$SDQ72_teacher_hyperactivity, na.rm = TRUE))
print(IQR(NEW$PRS_0_001_adhd_child, na.rm = TRUE))
print(IQR(NEW$PC1, na.rm = TRUE))
print(IQR(NEW$PC2, na.rm = TRUE))
print(IQR(NEW$PC3, na.rm = TRUE))

print(IQR(NEW$birth_wt_grams, na.rm = TRUE))	
print(IQR(NEW$PRS_0_001_adhd_child, na.rm = TRUE))

sink()

setwd("/Users/Marie-Elyse/Downloads")
NEW = read.csv("MAVAN_48M_and_up_jun2020.csv")

sink('sex_differences_kk_adhd_demographics.txt')
kruskal.test(conners_mother_adhd_score.60m ~ gender_male, data=NEW)
kruskal.test(conners_mother_adhd_score.72m ~ gender_male, data=NEW)
kruskal.test(conners_father_adhd_score.60m ~ gender_male, data=NEW)
kruskal.test(conners_father_adhd_score.72m ~ gender_male, data=NEW)
kruskal.test(SDQ60_mother_hyperactivity ~ gender_male, data=NEW)
kruskal.test(SDQ72_mother_hyperactivity ~ gender_male, data=NEW)
kruskal.test(SDQ60_father_hyperactivity ~ gender_male, data=NEW)
kruskal.test(SDQ72_father_hyperactivity ~ gender_male, data=NEW)
kruskal.test(Dominic72_ADHD ~ gender_male, data=NEW)
kruskal.test(CBCL48_sc6raw.y ~ gender_male, data=NEW)
kruskal.test(CBCL60_sc6raw.y ~ gender_male, data=NEW)
kruskal.test(PAPA_p4nadhd ~ gender_male, data=NEW)
kruskal.test(ADHD ~ gender_male, data=NEW)
kruskal.test(Mother ~ gender_male, data=NEW)
kruskal.test(Father ~ gender_male, data=NEW)
kruskal.test(Teacher ~ gender_male, data=NEW)
kruskal.test(mom_age_birth ~ gender_male, data=NEW)
kruskal.test(Pren_income4 ~ gender_male, data=NEW)
kruskal.test(Pren_life_events ~ gender_male, data=NEW)
kruskal.test(QuintMat_w ~ gender_male, data=NEW)
kruskal.test(Smoking_During_Pregnancy ~ gender_male, data=NEW)
kruskal.test(AGE_BY_SITE_corr~ gender_male, data=NEW)
kruskal.test(QuintSoc_w ~ gender_male, data=NEW)
kruskal.test(birth_size_percent2_x ~ gender_male, data=NEW)
kruskal.test(birth_wt_grams ~ gender_male, data=NEW)
kruskal.test(gestation_age_wks ~ gender_male, data=NEW)
kruskal.test(Pren_CESD ~ gender_male, data=NEW)
kruskal.test(HWB6_CESD ~ gender_male, data=NEW)
kruskal.test(HWB12_CESD ~ gender_male, data=NEW)
kruskal.test(HWB24_CESD ~ gender_male, data=NEW)
kruskal.test(HWB36_CESD ~ gender_male, data=NEW)
kruskal.test(HWB48_CESD ~ gender_male, data=NEW)
kruskal.test(HWB60_CESD ~ gender_male, data=NEW)
kruskal.test(HWB72_CESD ~ gender_male, data=NEW)
kruskal.test(auc_post_cesd ~ gender_male, data=NEW)
kruskal.test(Dominic72_cd ~ gender_male, data=NEW)
kruskal.test(Dominic72_odd ~ gender_male, data=NEW)
kruskal.test(Number_Disorder_exclu_ADHD ~ gender_male, data=NEW)
kruskal.test(CBCL48_sc5raw.y ~ gender_male, data=NEW)
kruskal.test(CBCL60_sc5raw.y ~ gender_male, data=NEW)
kruskal.test(PRS_0_001_adhd_child ~ gender_male, data=NEW)
kruskal.test(PC1 ~ gender_male, data=NEW)
kruskal.test(PC2 ~ gender_male, data=NEW)
kruskal.test(PC3 ~ gender_male, data=NEW)
kruskal.test(birth_wt_grams ~ gender_male, data=NEW)
kruskal.test(gestation_age_wks ~ gender_male, data=NEW)
# kruskal.test(Conduct_Disorder ~ gender_male, data=NEW)
# kruskal.test(ODD ~ gender_male, data=NEW)
# kruskal.test(Disorder_no_ADHD ~ gender_male, data=NEW)

chisq.test(NEW$above_college, NEW$gender_male, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Sibling, NEW$gender_male, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$PAPA_p4_adhd, NEW$gender_male, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Conduct_Disorder, NEW$gender_male, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$ODD, NEW$gender_male, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Disorder_no_ADHD, NEW$gender_male, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

sink()


sink('ADHD_differences_kk_adhd_demographics.txt')
kruskal.test(conners_mother_adhd_score.60m ~ PAPA_p4_adhd, data=NEW)
kruskal.test(conners_mother_adhd_score.72m ~ PAPA_p4_adhd, data=NEW)
kruskal.test(conners_father_adhd_score.60m ~ PAPA_p4_adhd, data=NEW)
kruskal.test(conners_father_adhd_score.72m ~ PAPA_p4_adhd, data=NEW)
kruskal.test(SDQ60_mother_hyperactivity ~ PAPA_p4_adhd, data=NEW)
kruskal.test(SDQ72_mother_hyperactivity ~ PAPA_p4_adhd, data=NEW)
kruskal.test(SDQ60_father_hyperactivity ~ PAPA_p4_adhd, data=NEW)
kruskal.test(SDQ72_father_hyperactivity ~ PAPA_p4_adhd, data=NEW)
kruskal.test(Dominic72_ADHD ~ PAPA_p4_adhd, data=NEW)
kruskal.test(CBCL48_sc6raw.y ~ PAPA_p4_adhd, data=NEW)
kruskal.test(CBCL60_sc6raw.y ~ PAPA_p4_adhd, data=NEW)
kruskal.test(PAPA_p4nadhd ~ PAPA_p4_adhd, data=NEW)
kruskal.test(ADHD ~ PAPA_p4_adhd, data=NEW)
kruskal.test(Mother ~ PAPA_p4_adhd, data=NEW)
kruskal.test(Father ~ PAPA_p4_adhd, data=NEW)
kruskal.test(Teacher ~ PAPA_p4_adhd, data=NEW)
kruskal.test(mom_age_birth ~ PAPA_p4_adhd, data=NEW)
kruskal.test(Pren_income4 ~ PAPA_p4_adhd, data=NEW)
kruskal.test(Pren_life_events ~ PAPA_p4_adhd, data=NEW)
kruskal.test(QuintMat_w ~ PAPA_p4_adhd, data=NEW)
kruskal.test(Smoking_During_Pregnancy ~ PAPA_p4_adhd, data=NEW)
kruskal.test(AGE_BY_SITE_corr~ PAPA_p4_adhd, data=NEW)
kruskal.test(QuintSoc_w ~ PAPA_p4_adhd, data=NEW)
kruskal.test(birth_size_percent2_x ~ PAPA_p4_adhd, data=NEW)
kruskal.test(birth_wt_grams ~ PAPA_p4_adhd, data=NEW)
kruskal.test(gestation_age_wks ~ PAPA_p4_adhd, data=NEW)
kruskal.test(Pren_CESD ~ PAPA_p4_adhd, data=NEW)
kruskal.test(HWB6_CESD ~ PAPA_p4_adhd, data=NEW)
kruskal.test(HWB12_CESD ~ PAPA_p4_adhd, data=NEW)
kruskal.test(HWB24_CESD ~ PAPA_p4_adhd, data=NEW)
kruskal.test(HWB36_CESD ~ PAPA_p4_adhd, data=NEW)
kruskal.test(HWB48_CESD ~ PAPA_p4_adhd, data=NEW)
kruskal.test(HWB60_CESD ~ PAPA_p4_adhd, data=NEW)
kruskal.test(HWB72_CESD ~ PAPA_p4_adhd, data=NEW)
kruskal.test(auc_post_cesd ~ PAPA_p4_adhd, data=NEW)
kruskal.test(Dominic72_cd ~ PAPA_p4_adhd, data=NEW)
kruskal.test(Dominic72_odd ~ PAPA_p4_adhd, data=NEW)
kruskal.test(Number_Disorder_exclu_ADHD ~ PAPA_p4_adhd, data=NEW)
kruskal.test(CBCL48_sc5raw.y ~ PAPA_p4_adhd, data=NEW)
kruskal.test(CBCL60_sc5raw.y ~ PAPA_p4_adhd, data=NEW)
kruskal.test(PRS_0_001_adhd_child ~ PAPA_p4_adhd, data=NEW)
kruskal.test(PC1 ~ PAPA_p4_adhd, data=NEW)
kruskal.test(PC2 ~ PAPA_p4_adhd, data=NEW)
kruskal.test(PC3 ~ PAPA_p4_adhd, data=NEW)
kruskal.test(birth_wt_grams ~ PAPA_p4_adhd, data=NEW)
kruskal.test(gestation_age_wks ~ PAPA_p4_adhd, data=NEW)

chisq.test(NEW$above_college, NEW$PAPA_p4_adhd, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$gender_male, NEW$PAPA_p4_adhd, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Sibling, NEW$PAPA_p4_adhd, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Conduct_Disorder, NEW$PAPA_p4_adhd, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$ODD, NEW$PAPA_p4_adhd, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Disorder_no_ADHD, NEW$PAPA_p4_adhd, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

sink()

sink('Sibling_differences_kk_adhd_demographics.txt')
kruskal.test(conners_mother_adhd_score.60m ~ Sibling, data=NEW)
kruskal.test(conners_mother_adhd_score.72m ~ Sibling, data=NEW)
kruskal.test(conners_father_adhd_score.60m ~ Sibling, data=NEW)
kruskal.test(conners_father_adhd_score.72m ~ Sibling, data=NEW)
kruskal.test(SDQ60_mother_hyperactivity ~ Sibling, data=NEW)
kruskal.test(SDQ72_mother_hyperactivity ~ Sibling, data=NEW)
kruskal.test(SDQ60_father_hyperactivity ~ Sibling, data=NEW)
kruskal.test(SDQ72_father_hyperactivity ~ Sibling, data=NEW)
kruskal.test(Dominic72_ADHD ~ Sibling, data=NEW)
kruskal.test(CBCL48_sc6raw.y ~ Sibling, data=NEW)
kruskal.test(CBCL60_sc6raw.y ~ Sibling, data=NEW)
kruskal.test(PAPA_p4nadhd ~ Sibling, data=NEW)
kruskal.test(ADHD ~ Sibling, data=NEW)
kruskal.test(Mother ~ Sibling, data=NEW)
kruskal.test(Father ~ Sibling, data=NEW)
kruskal.test(Teacher ~ Sibling, data=NEW)
kruskal.test(mom_age_birth ~ Sibling, data=NEW)
kruskal.test(Pren_income4 ~ Sibling, data=NEW)
kruskal.test(Pren_life_events ~ Sibling, data=NEW)
kruskal.test(QuintMat_w ~ Sibling, data=NEW)
kruskal.test(Smoking_During_Pregnancy ~ Sibling, data=NEW)
kruskal.test(AGE_BY_SITE_corr~ Sibling, data=NEW)
kruskal.test(QuintSoc_w ~ Sibling, data=NEW)
kruskal.test(birth_size_percent2_x ~ Sibling, data=NEW)
kruskal.test(birth_wt_grams ~ Sibling, data=NEW)
kruskal.test(gestation_age_wks ~ Sibling, data=NEW)
kruskal.test(Pren_CESD ~ Sibling, data=NEW)
kruskal.test(HWB6_CESD ~ Sibling, data=NEW)
kruskal.test(HWB12_CESD ~ Sibling, data=NEW)
kruskal.test(HWB24_CESD ~ Sibling, data=NEW)
kruskal.test(HWB36_CESD ~ Sibling, data=NEW)
kruskal.test(HWB48_CESD ~ Sibling, data=NEW)
kruskal.test(HWB60_CESD ~ Sibling, data=NEW)
kruskal.test(HWB72_CESD ~ Sibling, data=NEW)
kruskal.test(auc_post_cesd ~ Sibling, data=NEW)
kruskal.test(Dominic72_cd ~ Sibling, data=NEW)
kruskal.test(Dominic72_odd ~ Sibling, data=NEW)
kruskal.test(Number_Disorder_exclu_ADHD ~ Sibling, data=NEW)
kruskal.test(CBCL48_sc5raw.y ~ Sibling, data=NEW)
kruskal.test(CBCL60_sc5raw.y ~ Sibling, data=NEW)
kruskal.test(PRS_0_001_adhd_child ~ Sibling, data=NEW)
kruskal.test(PC1 ~ Sibling, data=NEW)
kruskal.test(PC2 ~ Sibling, data=NEW)
kruskal.test(PC3 ~ Sibling, data=NEW)
kruskal.test(birth_wt_grams ~ Sibling, data=NEW)
kruskal.test(gestation_age_wks ~ Sibling, data=NEW)
# kruskal.test(Conduct_Disorder ~ Sibling, data=NEW)
# kruskal.test(ODD ~ Sibling, data=NEW)
# kruskal.test(Disorder_no_ADHD ~ Sibling, data=NEW)

chisq.test(NEW$above_college, NEW$Sibling, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$gender_male, NEW$Sibling, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$PAPA_p4_adhd, NEW$Sibling, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Conduct_Disorder, NEW$Sibling, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$ODD, NEW$Sibling, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Disorder_no_ADHD, NEW$Sibling, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

sink()

sink('Site_differences_kk_adhd_demographics.txt')
kruskal.test(conners_mother_adhd_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_mother_adhd_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_father_adhd_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_father_adhd_score.72m ~ Hamilton, data=NEW)
kruskal.test(SDQ60_mother_hyperactivity ~ Hamilton, data=NEW)
kruskal.test(SDQ72_mother_hyperactivity ~ Hamilton, data=NEW)
kruskal.test(SDQ60_father_hyperactivity ~ Hamilton, data=NEW)
kruskal.test(SDQ72_father_hyperactivity ~ Hamilton, data=NEW)
kruskal.test(Dominic72_ADHD ~ Hamilton, data=NEW)
kruskal.test(CBCL48_sc6raw.y ~ Hamilton, data=NEW)
kruskal.test(CBCL60_sc6raw.y ~ Hamilton, data=NEW)
kruskal.test(PAPA_p4nadhd ~ Hamilton, data=NEW)
kruskal.test(ADHD ~ Hamilton, data=NEW)
kruskal.test(Mother ~ Hamilton, data=NEW)
kruskal.test(Father ~ Hamilton, data=NEW)
kruskal.test(Teacher ~ Hamilton, data=NEW)
kruskal.test(mom_age_birth ~ Hamilton, data=NEW)
kruskal.test(Pren_income4 ~ Hamilton, data=NEW)
kruskal.test(Pren_life_events ~ Hamilton, data=NEW)
kruskal.test(QuintMat_w ~ Hamilton, data=NEW)
kruskal.test(Smoking_During_Pregnancy ~ Hamilton, data=NEW)
kruskal.test(AGE_BY_SITE_corr~ Hamilton, data=NEW)
kruskal.test(QuintSoc_w ~ Hamilton, data=NEW)
kruskal.test(birth_size_percent2_x ~ Hamilton, data=NEW)
kruskal.test(birth_wt_grams ~ Hamilton, data=NEW)
kruskal.test(gestation_age_wks ~ Hamilton, data=NEW)
kruskal.test(Pren_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB6_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB12_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB24_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB36_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB48_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB60_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB72_CESD ~ Hamilton, data=NEW)
kruskal.test(auc_post_cesd ~ Hamilton, data=NEW)
kruskal.test(Dominic72_cd ~ Hamilton, data=NEW)
kruskal.test(Dominic72_odd ~ Hamilton, data=NEW)
kruskal.test(Number_Disorder_exclu_ADHD ~ Hamilton, data=NEW)
kruskal.test(CBCL48_sc5raw.y ~ Hamilton, data=NEW)
kruskal.test(CBCL60_sc5raw.y ~ Hamilton, data=NEW)
kruskal.test(PRS_0_001_adhd_child ~ Hamilton, data=NEW)
kruskal.test(PC1 ~ Hamilton, data=NEW)
kruskal.test(PC2 ~ Hamilton, data=NEW)
kruskal.test(PC3 ~ Hamilton, data=NEW)
kruskal.test(birth_wt_grams ~ Hamilton, data=NEW)
kruskal.test(gestation_age_wks ~ Hamilton, data=NEW)
# kruskal.test(Conduct_Disorder ~ Sibling, data=NEW)
# kruskal.test(ODD ~ Sibling, data=NEW)
# kruskal.test(Disorder_no_ADHD ~ Sibling, data=NEW)

chisq.test(NEW$above_college, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$gender_male, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$PAPA_p4_adhd, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Conduct_Disorder, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$ODD, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Disorder_no_ADHD, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

sink()

NEW$ADHD_d<-dicho(
               NEW$ADHD,
               dich.by = "median",
               as.num = TRUE,
               var.label = NULL,
               val.labels = NULL,
               append = TRUE,
               suffix = "_d"
               )
head(NEW$ADHD_d)

sink('ADHD_factor_differences_kk_adhd_demographics.txt')
kruskal.test(conners_mother_adhd_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_mother_adhd_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_adhd_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_adhd_score.72m ~ ADHD_d, data=NEW)
kruskal.test(SDQ60_mother_hyperactivity ~ ADHD_d, data=NEW)
kruskal.test(SDQ72_mother_hyperactivity ~ ADHD_d, data=NEW)
kruskal.test(SDQ60_father_hyperactivity ~ ADHD_d, data=NEW)
kruskal.test(SDQ72_father_hyperactivity ~ ADHD_d, data=NEW)
kruskal.test(Dominic72_ADHD ~ ADHD_d, data=NEW)
kruskal.test(CBCL48_sc6raw.y ~ ADHD_d, data=NEW)
kruskal.test(CBCL60_sc6raw.y ~ ADHD_d, data=NEW)
kruskal.test(PAPA_p4nadhd ~ ADHD_d, data=NEW)
kruskal.test(ADHD ~ ADHD_d, data=NEW)
kruskal.test(Mother ~ ADHD_d, data=NEW)
kruskal.test(Father ~ ADHD_d, data=NEW)
kruskal.test(Teacher ~ ADHD_d, data=NEW)
kruskal.test(mom_age_birth ~ ADHD_d, data=NEW)
kruskal.test(Pren_income4 ~ ADHD_d, data=NEW)
kruskal.test(Pren_life_events ~ ADHD_d, data=NEW)
kruskal.test(QuintMat_w ~ ADHD_d, data=NEW)
kruskal.test(Smoking_During_Pregnancy ~ ADHD_d, data=NEW)
kruskal.test(AGE_BY_SITE_corr~ ADHD_d, data=NEW)
kruskal.test(QuintSoc_w ~ ADHD_d, data=NEW)
kruskal.test(birth_size_percent2_x ~ ADHD_d, data=NEW)
kruskal.test(birth_wt_grams ~ ADHD_d, data=NEW)
kruskal.test(gestation_age_wks ~ ADHD_d, data=NEW)
kruskal.test(Pren_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB6_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB12_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB24_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB36_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB48_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB60_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB72_CESD ~ ADHD_d, data=NEW)
kruskal.test(auc_post_cesd ~ ADHD_d, data=NEW)
kruskal.test(Dominic72_cd ~ ADHD_d, data=NEW)
kruskal.test(Dominic72_odd ~ ADHD_d, data=NEW)
kruskal.test(Number_Disorder_exclu_ADHD ~ ADHD_d, data=NEW)
kruskal.test(CBCL48_sc5raw.y ~ ADHD_d, data=NEW)
kruskal.test(CBCL60_sc5raw.y ~ ADHD_d, data=NEW)
kruskal.test(PRS_0_001_adhd_child ~ ADHD_d, data=NEW)
kruskal.test(PC1 ~ ADHD_d, data=NEW)
kruskal.test(PC2 ~ ADHD_d, data=NEW)
kruskal.test(PC3 ~ ADHD_d, data=NEW)
kruskal.test(birth_wt_grams ~ ADHD_d, data=NEW)
kruskal.test(gestation_age_wks ~ ADHD_d, data=NEW)
# kruskal.test(Conduct_Disorder ~ ADHD_d, data=NEW)
# kruskal.test(ODD ~ ADHD_d, data=NEW)
# kruskal.test(Disorder_no_ADHD ~ ADHD_d, data=NEW)

chisq.test(NEW$above_college, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Sibling, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$PAPA_p4_adhd, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Conduct_Disorder, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$ODD, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Disorder_no_ADHD, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

sink()

sink('kk_extra_demographics.txt')

NEW$ADHD_d<-dicho(
               NEW$ADHD,
               dich.by = "median",
               as.num = TRUE,
               var.label = NULL,
               val.labels = NULL,
               append = TRUE,
               suffix = "_d"
               )
head(NEW$ADHD_d)

kruskal.test(Alcohol_During_Pregnancy ~ ADHD_d, data=NEW)
kruskal.test(Alcohol_During_Pregnancy ~ Hamilton, data=NEW)
kruskal.test(Alcohol_During_Pregnancy ~ PAPA_p4_adhd, data=NEW)
kruskal.test(Alcohol_During_Pregnancy ~ Sibling, data=NEW)
kruskal.test(Alcohol_During_Pregnancy ~ gender_male, data=NEW)

kruskal.test(SDQ72_teacher_hyperactivity ~ ADHD_d, data=NEW)
kruskal.test(SDQ72_teacher_hyperactivity ~ Hamilton, data=NEW)
kruskal.test(SDQ72_teacher_hyperactivity ~ PAPA_p4_adhd, data=NEW)
kruskal.test(SDQ72_teacher_hyperactivity ~ Sibling, data=NEW)
kruskal.test(SDQ72_teacher_hyperactivity ~ gender_male, data=NEW)

chisq.test(NEW$gender_male, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$gender_male, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$gender_male, NEW$PAPA_p4_adhd, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$gender_male, NEW$Sibling, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

kruskal.test(conners_teacher_adhd_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_teacher_adhd_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_teacher_adhd_score.72m ~ PAPA_p4_adhd, data=NEW)
kruskal.test(conners_teacher_adhd_score.72m ~ Sibling, data=NEW)
kruskal.test(conners_teacher_adhd_score.72m ~ gender_male, data=NEW)

kruskal.test(conners_mother_cognitive_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_mother_cognitive_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_mother_cognitive_score.60m ~ PAPA_p4_adhd, data=NEW)
kruskal.test(conners_mother_cognitive_score.60m~ Sibling, data=NEW)
kruskal.test(conners_mother_cognitive_score.60m ~ gender_male, data=NEW)

kruskal.test(conners_father_cognitive_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_cognitive_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_father_cognitive_score.60m ~ PAPA_p4_adhd, data=NEW)
kruskal.test(conners_father_cognitive_score.60m ~ Sibling, data=NEW)
kruskal.test(conners_father_cognitive_score.60m ~ gender_male, data=NEW)

kruskal.test(conners_mother_cognitive_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_mother_cognitive_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_mother_cognitive_score.72m ~ PAPA_p4_adhd, data=NEW)
kruskal.test(conners_mother_cognitive_score.72m ~ Sibling, data=NEW)
kruskal.test(conners_mother_cognitive_score.72m ~ gender_male, data=NEW)

kruskal.test(conners_father_cognitive_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_cognitive_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_father_cognitive_score.72m ~ PAPA_p4_adhd, data=NEW)
kruskal.test(conners_father_cognitive_score.72m ~ Sibling, data=NEW)
kruskal.test(conners_father_cognitive_score.72m ~ gender_male, data=NEW)

kruskal.test(conners_teacher_cognitive_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_teacher_cognitive_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_teacher_cognitive_score.72m ~ PAPA_p4_adhd, data=NEW)
kruskal.test(conners_teacher_cognitive_score.72m ~ Sibling, data=NEW)
kruskal.test(conners_teacher_cognitive_score.72m ~ gender_male, data=NEW)

kruskal.test(conners_mother_hyperactivity_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.60m ~ PAPA_p4_adhd, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.60m ~ Sibling, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.60m ~ gender_male, data=NEW)

kruskal.test(conners_father_hyperactivity_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_hyperactivity_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_father_hyperactivity_score.60m ~ PAPA_p4_adhd, data=NEW)
kruskal.test(conners_father_hyperactivity_score.60m ~ Sibling, data=NEW)
kruskal.test(conners_father_hyperactivity_score.60m ~ gender_male, data=NEW)

kruskal.test(conners_mother_hyperactivity_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.72m ~ PAPA_p4_adhd, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.72m ~ Sibling, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.72m ~ gender_male, data=NEW)

kruskal.test(conners_father_hyperactivity_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_hyperactivity_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_father_hyperactivity_score.72m ~ PAPA_p4_adhd, data=NEW)
kruskal.test(conners_father_hyperactivity_score.72m ~ Sibling, data=NEW)
kruskal.test(conners_father_hyperactivity_score.72m ~ gender_male, data=NEW)

kruskal.test(conners_teacher_hyperactivity_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_teacher_hyperactivity_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_teacher_hyperactivity_score.72m ~ PAPA_p4_adhd, data=NEW)
kruskal.test(conners_teacher_hyperactivity_score.72m ~ Sibling, data=NEW)
kruskal.test(conners_teacher_hyperactivity_score.72m ~ gender_male, data=NEW)

sink()

Dominic72_cd
Dominic72_odd

# group_by(NEW, gender_male) %>%
#   dplyr::summarise(
#     count = sum(n()),
#     mean = mean(conners_mother_adhd_score.60m, na.rm = TRUE),
#     sd = sd(conners_mother_adhd_score.60m, na.rm = TRUE),
#     median = median(conners_mother_adhd_score.60m, na.rm = TRUE),
#     IQR = IQR(conners_mother_adhd_score.60m, na.rm = TRUE)
#   )
# 
# group_by(NEW, gender_male) %>%
#   dplyr::summarise(
#     count = sum(n()),
#     mean = mean(conners_mother_adhd_score.72m, na.rm = TRUE),
#     sd = sd(conners_mother_adhd_score.72m, na.rm = TRUE),
#     median = median(conners_mother_adhd_score.72m, na.rm = TRUE),
#     IQR = IQR(conners_mother_adhd_score.72m, na.rm = TRUE)
#   )
# 
# group_by(NEW, gender_male) %>%
#   dplyr::summarise(
#     count = sum(n()),
#     mean = mean(conners_father_adhd_score.60m, na.rm = TRUE),
#     sd = sd(conners_father_adhd_score.60m, na.rm = TRUE),
#     median = median(conners_father_adhd_score.60m, na.rm = TRUE),
#     IQR = IQR(conners_father_adhd_score.60m, na.rm = TRUE)
#   )
# 
# group_by(NEW, gender_male) %>%
#   dplyr::summarise(
#     count = sum(n()),
#     mean = mean(conners_father_adhd_score.72m, na.rm = TRUE),
#     sd = sd(conners_father_adhd_score.72m, na.rm = TRUE),
#     median = median(conners_father_adhd_score.72m, na.rm = TRUE),
#     IQR = IQR(conners_father_adhd_score.72m, na.rm = TRUE)
#   )
# 
# group_by(NEW, gender_male) %>%
#   dplyr::summarise(
#     count = sum(n()),
#     mean = mean(SDQ60_mother_hyperactivity, na.rm = TRUE),
#     sd = sd(SDQ60_mother_hyperactivity, na.rm = TRUE),
#     median = median(SDQ60_mother_hyperactivity, na.rm = TRUE),
#     IQR = IQR(SDQ60_mother_hyperactivity, na.rm = TRUE)
#   )
# 
# group_by(NEW, gender_male) %>%
#   dplyr::summarise(
#     count = sum(n()),
#     mean = mean(SDQ72_mother_hyperactivity, na.rm = TRUE),
#     sd = sd(SDQ72_mother_hyperactivity, na.rm = TRUE),
#     median = median(SDQ72_mother_hyperactivity, na.rm = TRUE),
#     IQR = IQR(SDQ72_mother_hyperactivity, na.rm = TRUE)
#   )
# 
# group_by(NEW, gender_male) %>%
#   dplyr::summarise(
#     count = sum(n()),
#     mean = mean(SDQ60_father_hyperactivity, na.rm = TRUE),
#     sd = sd(SDQ60_father_hyperactivity, na.rm = TRUE),
#     median = median(SDQ60_father_hyperactivity, na.rm = TRUE),
#     IQR = IQR(SDQ60_father_hyperactivity, na.rm = TRUE)
#   )
# 
# group_by(NEW, gender_male) %>%
#   dplyr::summarise(
#     count = sum(n()),
#     mean = mean(SDQ72_father_hyperactivity, na.rm = TRUE),
#     sd = sd(SDQ72_father_hyperactivity, na.rm = TRUE),
#     median = median(SDQ72_father_hyperactivity, na.rm = TRUE),
#     IQR = IQR(SDQ72_father_hyperactivity, na.rm = TRUE)
#   )
# 
# group_by(NEW, gender_male) %>%
#   dplyr::summarise(
#     count = sum(n()),
#     mean = mean(Dominic72_ADHD, na.rm = TRUE),
#     sd = sd(Dominic72_ADHD, na.rm = TRUE),
#     median = median(Dominic72_ADHD, na.rm = TRUE),
#     IQR = IQR(Dominic72_ADHD, na.rm = TRUE)
#   )
# 
# group_by(NEW, gender_male) %>%
#   dplyr::summarise(
#     count = sum(n()),
#     mean = mean(CBCL48_sc6raw, na.rm = TRUE),
#     sd = sd(CBCL48_sc6raw, na.rm = TRUE),
#     median = median(CBCL48_sc6raw, na.rm = TRUE),
#     IQR = IQR(CBCL48_sc6raw, na.rm = TRUE)
#   )
# 
# group_by(NEW, gender_male) %>%
#   dplyr::summarise(
#     count = sum(n()),
#     mean = mean(CBCL60_sc6raw, na.rm = TRUE),
#     sd = sd(CBCL60_sc6raw, na.rm = TRUE),
#     median = median(CBCL60_sc6raw, na.rm = TRUE),
#     IQR = IQR(CBCL60_sc6raw, na.rm = TRUE)
#   )
# 
# group_by(NEW, gender_male) %>%
#   dplyr::summarise(
#     count = sum(n()),
#     mean = mean(PAPA_p4nadhd, na.rm = TRUE),
#     sd = sd(PAPA_p4nadhd, na.rm = TRUE),
#     median = median(PAPA_p4nadhd, na.rm = TRUE),
#     IQR = IQR(PAPA_p4nadhd, na.rm = TRUE)
#   )
# 
# sink()
# 
# lmm <- lmer(conners_mother_adhd_score.60m ~ Sconners_father_adhd_score.60m + (1 | gender_male), data = NEW,
#             REML = FALSE)
# summary(lmm) #estimate of the variance explained by the random effect. This number is important, because if it's indistinguishable from zero, then your random effect probably doesn't matter and you can go ahead and do a regular linear model instead. 
# Anova(lmm) #Anova function does a Wald test how confident we are of our estimate of the effect of child sex on adhd score
# 
# table(NEW$conners_mother_adhd_score.60m, NEW$gender_male)  
# addmargins(table(NEW$conners_mother_adhd_score.60m, NEW$gender_male))
# 
# chisq.test(NEW$, LONG$gender_male, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
# 
# group_by(LONG, gender_male) %>%
#   dplyr::summarise(
#     count = sum(n()),
#     mean = mean(conners_60, na.rm = TRUE),
#     sd = sd(conners_60, na.rm = TRUE),
#     median = median(conners_60, na.rm = TRUE),
#     IQR = IQR(conners_60, na.rm = TRUE)
#   )
# Two Way Factorial Design
# fit <- aov(conners_60 ~ conners_parent_60*gender_male_fixed, data=LONG) # same thing
# #plot(fit) # diagnostic plots
# summary(fit) # display Type I ANOVA table
# #TukeyHSD(fit) # where fit comes from aov()
# drop1(fit,~.,test="F") # type III SS and F Tests
# summary.aov(fit)

sink('extra_demographics.txt')
kruskal.test(mom_age_birth ~ Hamilton, data=NEW)
kruskal.test(Pren_income4 ~ Hamilton, data=NEW)
kruskal.test(birth_wt_grams ~ Hamilton, data=NEW)
kruskal.test(gestation_age_wks ~ Hamilton, data=NEW)
kruskal.test(ADHD ~ Hamilton, data=NEW)
kruskal.test(CBCL48_sc6raw.y ~ Hamilton, data=NEW)
kruskal.test(PC1 ~ Hamilton, data=NEW)
kruskal.test(PC2 ~ Hamilton, data=NEW)
kruskal.test(PC3 ~ Hamilton, data=NEW)
sink()      

NEW$ADHD_d<-sjmisc::dicho(NEW$ADHD,
                          dich.by = "median",
                          as.num = TRUE,
                          var.label = NULL,
                          val.labels = NULL,
                          append = TRUE,
                          suffix = "_d"
)

sink('extra_demographics.txt')
kruskal.test(mom_age_birth ~ Hamilton, data=NEW)
chisq.test(NEW$above_college, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
kruskal.test(Pren_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB6_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB12_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB24_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB36_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB48_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB60_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB72_CESD ~ ADHD_d, data=NEW)
kruskal.test(auc_post_cesd ~ ADHD_d, data=NEW)

kruskal.test(PAPA_p4nadhd ~ ADHD_d, data=NEW)
chisq.test(NEW$PAPA_p4_adhd, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
kruskal.test(Dominic72_ADHD ~ ADHD_d, data=NEW)

kruskal.test(conners_mother_adhd_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_mother_adhd_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_adhd_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_adhd_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_teacher_adhd_score.72m ~ ADHD_d, data=NEW)

kruskal.test(conners_mother_cognitive_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_mother_cognitive_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_cognitive_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_cognitive_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_teacher_cognitive_score.72m ~ ADHD_d, data=NEW)

kruskal.test(conners_mother_hyperactivity_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_hyperactivity_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_hyperactivity_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_teacher_hyperactivity_score.72m ~ ADHD_d, data=NEW)

kruskal.test(CBCL48_sc6raw.y ~ ADHD_d, data=NEW)
kruskal.test(CBCL60_sc6raw.y ~ ADHD_d, data=NEW)

kruskal.test(SDQ60_mother_hyperactivity ~ ADHD_d, data=NEW)
kruskal.test(SDQ72_mother_hyperactivity ~ ADHD_d, data=NEW)
kruskal.test(SDQ60_father_hyperactivity ~ ADHD_d, data=NEW)
kruskal.test(SDQ72_father_hyperactivity ~ ADHD_d, data=NEW)
kruskal.test(SDQ72_teacher_hyperactivity ~ ADHD_d, data=NEW)

kruskal.test(Mother ~ ADHD_d, data=NEW)
kruskal.test(Father ~ ADHD_d, data=NEW)
kruskal.test(Teacher ~ ADHD_d, data=NEW)

kruskal.test(Dominic72_odd ~ ADHD_d, data=NEW)
chisq.test(NEW$ODD, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

kruskal.test(Dominic72_cd ~ ADHD_d, data=NEW)
chisq.test(NEW$Conduct_Disorder, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

kruskal.test(Number_Disorder_exclu_ADHD ~ ADHD_d, data=NEW)
chisq.test(NEW$Disorder_no_ADHD, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

kruskal.test(CBCL48_sc5raw.y ~ ADHD_d, data=NEW)
kruskal.test(CBCL60_sc5raw.y ~ ADHD_d, data=NEW)

sink() 

NEW <- read.csv("MAVAN_48M_and_up_jun2020.csv")
NEW <-NEW[!is.na(NEW$PRS_0_001_adhd_child), ]

sink('characteristic_description_by_site_only_those_with_genetics.txt')
psych.describeVEC.site <-psych::describeBy(NEW[, c("ADHD", "Mother",	"Father",	"Teacher",
                                                   "gender_male",
                                                   "mom_age_birth", "above_college", "Pren_income4", 
                                                   "Pren_life_events", "QuintMat_w", "Smoking_During_Pregnancy",
                                                   "Alcohol_During_Pregnancy",
                                                   "AGE_BY_SITE_corr",  "QuintSoc_w", "birth_size_percent2_x", "PAPA_p4nadhd", "PAPA_p4_adhd", "Dominic72_ADHD",
                                                   "conners_mother_adhd_score.60m", "conners_father_adhd_score.60m", 
                                                   "conners_mother_adhd_score.72m", "conners_father_adhd_score.72m", 
                                                   "conners_teacher_adhd_score.72m",  "conners_mother_cognitive_score.60m", "conners_father_cognitive_score.60m", 
                                                   "conners_mother_cognitive_score.72m", "conners_father_cognitive_score.72m", 
                                                   "conners_teacher_cognitive_score.72m",
                                                   "conners_mother_hyperactivity_score.60m", "conners_father_hyperactivity_score.60m", 
                                                   "conners_mother_hyperactivity_score.72m", "conners_father_hyperactivity_score.72m", 
                                                   "conners_teacher_hyperactivity_score.72m",
                                                   "CBCL48_sc6raw.y", "CBCL60_sc6raw.y",
                                                   "CBCL48_sc7raw.y", "CBCL60_sc7raw.y",
                                                   "SDQ60_mother_hyperactivity", 
                                                   "SDQ60_father_hyperactivity", 
                                                   "SDQ72_mother_hyperactivity", 
                                                   "SDQ72_father_hyperactivity", 
                                                   "SDQ72_teacher_hyperactivity",
                                                   "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
                                                   "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "Conduct_Disorder", "ODD",
                                                   "Number_Disorder_exclu_ADHD", "Disorder_no_ADHD",
                                                   "CBCL48_sc5raw.y", "CBCL60_sc5raw.y", "PRS_0_001_adhd_child", "PC1", "PC2", "PC3", "birth_wt_grams",	
                                                   "gestation_age_wks", "Dominic72_cd","Dominic72_odd", "auc_post_cesd")], NEW$Hamilton)
print(psych.describeVEC.site)
sink()   

sink()

sink('Site_differences_kk_adhd_demographics_only_those_with_genetics.txt')

NEW$ADHD_d<-dicho(
  NEW$ADHD,
  dich.by = "median",
  as.num = TRUE,
  var.label = NULL,
  val.labels = NULL,
  append = TRUE,
  suffix = "_d"
)
head(NEW$ADHD_d)

kruskal.test(conners_mother_adhd_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_mother_adhd_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_father_adhd_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_father_adhd_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_teacher_adhd_score.72m ~ Hamilton, data=NEW)
kruskal.test(SDQ60_mother_hyperactivity ~ Hamilton, data=NEW)
kruskal.test(SDQ72_mother_hyperactivity ~ Hamilton, data=NEW)
kruskal.test(SDQ60_father_hyperactivity ~ Hamilton, data=NEW)
kruskal.test(SDQ72_father_hyperactivity ~ Hamilton, data=NEW)
kruskal.test(SDQ72_teacher_hyperactivity ~ Hamilton, data=NEW)
kruskal.test(Dominic72_ADHD ~ Hamilton, data=NEW)
kruskal.test(CBCL48_sc6raw.y ~ Hamilton, data=NEW)
kruskal.test(CBCL60_sc6raw.y ~ Hamilton, data=NEW)
kruskal.test(PAPA_p4nadhd ~ Hamilton, data=NEW)
kruskal.test(ADHD ~ Hamilton, data=NEW)
kruskal.test(Mother ~ Hamilton, data=NEW)
kruskal.test(Father ~ Hamilton, data=NEW)
kruskal.test(Teacher ~ Hamilton, data=NEW)
kruskal.test(mom_age_birth ~ Hamilton, data=NEW)
kruskal.test(Pren_income4 ~ Hamilton, data=NEW)
kruskal.test(Pren_life_events ~ Hamilton, data=NEW)
kruskal.test(QuintMat_w ~ Hamilton, data=NEW)
kruskal.test(Smoking_During_Pregnancy ~ Hamilton, data=NEW)
kruskal.test(AGE_BY_SITE_corr~ Hamilton, data=NEW)
kruskal.test(QuintSoc_w ~ Hamilton, data=NEW)
kruskal.test(birth_size_percent2_x ~ Hamilton, data=NEW)
kruskal.test(birth_wt_grams ~ Hamilton, data=NEW)
kruskal.test(gestation_age_wks ~ Hamilton, data=NEW)
kruskal.test(Pren_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB6_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB12_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB24_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB36_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB48_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB60_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB72_CESD ~ Hamilton, data=NEW)
kruskal.test(auc_post_cesd ~ Hamilton, data=NEW)
kruskal.test(Dominic72_cd ~ Hamilton, data=NEW)
kruskal.test(Dominic72_odd ~ Hamilton, data=NEW)
kruskal.test(Number_Disorder_exclu_ADHD ~ Hamilton, data=NEW)
kruskal.test(CBCL48_sc5raw.y ~ Hamilton, data=NEW)
kruskal.test(CBCL60_sc5raw.y ~ Hamilton, data=NEW)
kruskal.test(PRS_0_001_adhd_child ~ Hamilton, data=NEW)
kruskal.test(PC1 ~ Hamilton, data=NEW)
kruskal.test(PC2 ~ Hamilton, data=NEW)
kruskal.test(PC3 ~ Hamilton, data=NEW)
kruskal.test(birth_wt_grams ~ Hamilton, data=NEW)
kruskal.test(gestation_age_wks ~ Hamilton, data=NEW)
# kruskal.test(Conduct_Disorder ~ Sibling, data=NEW)
# kruskal.test(ODD ~ Sibling, data=NEW)
# kruskal.test(Disorder_no_ADHD ~ Sibling, data=NEW)

chisq.test(NEW$above_college, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$gender_male, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$PAPA_p4_adhd, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Conduct_Disorder, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$ODD, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Disorder_no_ADHD, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

kruskal.test(mom_age_birth ~ Hamilton, data=NEW)
kruskal.test(Pren_income4 ~ Hamilton, data=NEW)
kruskal.test(birth_wt_grams ~ Hamilton, data=NEW)
kruskal.test(gestation_age_wks ~ Hamilton, data=NEW)
kruskal.test(ADHD ~ Hamilton, data=NEW)
kruskal.test(CBCL48_sc6raw.y ~ Hamilton, data=NEW)
kruskal.test(Alcohol_During_Pregnancy ~ Hamilton, data=NEW)

kruskal.test(conners_mother_cognitive_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_father_cognitive_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_mother_cognitive_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_father_cognitive_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_teacher_cognitive_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_father_hyperactivity_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_father_hyperactivity_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_teacher_hyperactivity_score.72m ~ Hamilton, data=NEW)

sink()
