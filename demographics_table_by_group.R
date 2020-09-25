#Marie-Elyse Lafaille-Magnan, PhD
#marie-elyse.lafaille-magnan@mail.mcgill.ca

setwd("/Users/Marie-Elyse/Downloads")
NEW = read.csv("MAVAN_48M_and_up_jun2020.csv")

NEW$ADHD_d<-sjmisc::dicho(NEW$ADHD,
     dich.by = "median",
     as.num = TRUE,
     var.label = NULL,
     val.labels = NULL,
     append = TRUE,
     suffix = "_d"
   )
head(NEW$ADHD_d)

NEW = apply_labels(NEW,
                    gender_male = "biological sex of child",
                    c("Girls" = 0,
                      "Boys"=1),
                    Hamilton = "site",
                    c("Montreal" = 0,
                     "Hamilton"=1),
                    mom_age_birth = "Maternal age at birth", 
                    above_college = "Maternal education", 
                    Pren_income4 = "Prenatal income per year",
                    Pren_life_events = "Prenatal adverse life events",
                    gestation_age_wks = "gestation age in weeks",
                    birth_wt_grams = "birth weight in grams",
                    Sibling = "Multiple children", 
                    c("Second and Third child"=0, 
                     "First child"=1),
                    Pren_CESD = "Prenatal depression", 
                    HWB6_CESD =	"Postnatal depression at 6 months",
                    HWB12_CESD = "Postnatal depression at 12 months",	
                    HWB24_CESD = "Postnatal depression at 24 months",
                    HWB36_CESD = "Postnatal depression at 36 months", 
                    HWB48_CESD = "Postnatal depression at 48 months",
                    HWB60_CESD = "Postnatal depression at 60 months",
                    HWB72_CESD = "Postnatal depression at 72 months",
                    auc_post_cesd = "Total postnatal depression",
                    PRS_0_001_adhd_child = "PRS 0.001 ADHD in child", 
                    PC1 = "PC1", 
                    PC2 = "PC2", 
                    PC3 = "PC3", 
                    conners_mother_adhd_score.60m = "Conners mother ADHD at 60 months",
                    conners_mother_adhd_score.72m = "Conners mother ADHD at 72 months",
                    conners_mother_cognitive_score.60m = "Conners mother Cognitive at 60 months",
                    conners_mother_cognitive_score.72m = "Conners mother Cognitive at 72 months",
                    conners_mother_hyperactivity_score.60m = "Conners mother Hyperactivity at 60 months", 
                    conners_mother_hyperactivity_score.72m = "Conners mother Hyperactivity at 72 months",  
                    SDQ60_mother_hyperactivity = "SDQ mother hyperactivity at 60 months",
                    SDQ72_mother_hyperactivity = "SDQ mother hyperactivity at 72 months", 
                    conners_father_adhd_score.60m = "Conners father ADHD at 60 months",
                    conners_father_adhd_score.72m = "Conners father ADHD at 72 months",
                    conners_father_cognitive_score.60m = "Conners father cognitive at 60 months",
                    conners_father_cognitive_score.72m = "Conners father cognitive at 72 months",
                    conners_father_hyperactivity_score.60m = "Conners father hyperactivity at 60 months",
                    conners_father_hyperactivity_score.72m = "Conners father hyperactivity at 72 months",
                    SDQ60_father_hyperactivity = "SDQ father hyperactivity at 60 months",
                    SDQ72_father_hyperactivity = "SDQ father hyperactivity at 72 months",
                    conners_teacher_adhd_score.72m = "Conners teacher ADHD at 72 months",
                    conners_teacher_cognitive_score.72m = "Conners teacher cognitive at 72 months", 
                    conners_teacher_hyperactivity_score.72m = "Conners teacher hyperactivity at 72 months",
                    SDQ72_teacher_hyperactivity = "SDQ teacher hyperactivity at 72 months",
                    CBCL48_sc5raw.y = "CBCL sleep problems at 48 months",
                    CBCL60_sc5raw.y = "CBCL sleep problems at 60 months",
                    CBCL48_sc6raw.y = "CBCL attention problems at 48 months",
                    CBCL60_sc6raw.y = "CBCL attention problems at 60 months",
                    CBCL48_sc7raw.y = "CBCL agressivity problems at 48 months",
                    CBCL60_sc7raw.y = "CBCL agressivity problems at 60 months",
                    Dominic72_ADHD = "Dominic ADHD",
                    Dominic72_cd = "Dominic CD",
                    Dominic72_odd = "Dominic ODD", 
                    PAPA_p4nadhd = "PAPA number of symptoms",
                    PAPA_p4_adhd = "PAPA diagnosis", 
                    ODD = "PAPA ODD",
                    Conduct_Disorder = "PAPA CD",
                    Number_Disorder_exclu_ADHD = "PAPA number of comorbid disorder excluding ADHD",
                    Disorder_no_ADHD = "PAPA having a disorder excluding ADHD",
                    ADHD = "ADHD", 
                    Mother = "Mother",	
                    Father = "Father",	
                    Teacher = "Teacher",
                    ADHD_d = "ADHD factor groups",
                    c("Low" = 0,
                      "High"=1)
)

sink('demographics_table.txt')

NEW %>%
  tab_cells(NEW$gender_male,
            NEW$Hamilton,
            NEW$mom_age_birth, 
            NEW$above_college, 
            NEW$Pren_income4,
            NEW$Pren_life_events,
            NEW$gestation_age_wks,
            NEW$birth_wt_grams,
            NEW$Sibling,
            NEW$Pren_CESD, 
            NEW$HWB6_CESD,
            NEW$HWB12_CESD,	
            NEW$HWB24_CESD,
            NEW$HWB36_CESD, 
            NEW$HWB48_CESD,
            NEW$HWB60_CESD,
            NEW$HWB72_CESD,
            NEW$auc_post_cesd,
            NEW$PRS_0_001_adhd_child, 
            NEW$PC1, 
            NEW$PC2, 
            NEW$PC3, 
            NEW$conners_mother_adhd_score.60m,
            NEW$conners_mother_adhd_score.72m,
            NEW$conners_mother_cognitive_score.60m,
            NEW$conners_mother_cognitive_score.72m,
            NEW$conners_mother_hyperactivity_score.60m, 
            NEW$conners_mother_hyperactivity_score.72m,  
            NEW$SDQ60_mother_hyperactivity,
            NEW$SDQ72_mother_hyperactivity, 
            NEW$conners_father_adhd_score.60m,
            NEW$conners_father_adhd_score.72m,
            NEW$conners_father_cognitive_score.60m,
            NEW$conners_father_cognitive_score.72m,
            NEW$conners_father_hyperactivity_score.60m,
            NEW$conners_father_hyperactivity_score.72m,
            NEW$SDQ60_father_hyperactivity,
            NEW$SDQ72_father_hyperactivity,
            NEW$conners_teacher_adhd_score.72m,
            NEW$conners_teacher_cognitive_score.72m, 
            NEW$conners_teacher_hyperactivity_score.72m,
            NEW$SDQ72_teacher_hyperactivity,
            NEW$CBCL48_sc5raw.y,
            NEW$CBCL60_sc5raw.y,
            NEW$CBCL48_sc6raw.y,
            NEW$CBCL60_sc6raw.y,
            NEW$CBCL48_sc7raw.y,
            NEW$CBCL60_sc7raw.y,
            NEW$Dominic72_ADHD,
            NEW$Dominic72_cd,
            NEW$Dominic72_odd, 
            NEW$PAPA_p4nadhd,
            NEW$PAPA_p4_adhd, 
            NEW$ODD,
            NEW$Conduct_Disorder,
            NEW$Number_Disorder_exclu_ADHD,
            NEW$Disorder_no_ADHD,
            NEW$ADHD, 
            NEW$Mother,	
            NEW$Father,	
            NEW$Teacher,
            NEW$ADHD_d) %>%
  tab_cols() %>% 
  tab_stat_mean_sd_n() %>%
  tab_last_sig_means(subtable_marks = "both") %>% 
  tab_pivot() %>% 
  set_caption("Table with summary statistics and significance marks")  

sink()


sink('demographics_table_by_sex.txt')

NEW %>%
  tab_cells(NEW$gender_male,
            NEW$Hamilton,
            NEW$mom_age_birth, 
            NEW$above_college, 
            NEW$Pren_income4,
            NEW$Pren_life_events,
            NEW$gestation_age_wks,
            NEW$birth_wt_grams,
            NEW$Sibling,
            NEW$Pren_CESD, 
            NEW$HWB6_CESD,
            NEW$HWB12_CESD,	
            NEW$HWB24_CESD,
            NEW$HWB36_CESD, 
            NEW$HWB48_CESD,
            NEW$HWB60_CESD,
            NEW$HWB72_CESD,
            NEW$auc_post_cesd,
            NEW$PRS_0_001_adhd_child, 
            NEW$PC1, 
            NEW$PC2, 
            NEW$PC3, 
            NEW$conners_mother_adhd_score.60m,
            NEW$conners_mother_adhd_score.72m,
            NEW$conners_mother_cognitive_score.60m,
            NEW$conners_mother_cognitive_score.72m,
            NEW$conners_mother_hyperactivity_score.60m, 
            NEW$conners_mother_hyperactivity_score.72m,  
            NEW$SDQ60_mother_hyperactivity,
            NEW$SDQ72_mother_hyperactivity, 
            NEW$conners_father_adhd_score.60m,
            NEW$conners_father_adhd_score.72m,
            NEW$conners_father_cognitive_score.60m,
            NEW$conners_father_cognitive_score.72m,
            NEW$conners_father_hyperactivity_score.60m,
            NEW$conners_father_hyperactivity_score.72m,
            NEW$SDQ60_father_hyperactivity,
            NEW$SDQ72_father_hyperactivity,
            NEW$conners_teacher_adhd_score.72m,
            NEW$conners_teacher_cognitive_score.72m, 
            NEW$conners_teacher_hyperactivity_score.72m,
            NEW$SDQ72_teacher_hyperactivity,
            NEW$CBCL48_sc5raw.y,
            NEW$CBCL60_sc5raw.y,
            NEW$CBCL48_sc6raw.y,
            NEW$CBCL60_sc6raw.y,
            NEW$CBCL48_sc7raw.y,
            NEW$CBCL60_sc7raw.y,
            NEW$Dominic72_ADHD,
            NEW$Dominic72_cd,
            NEW$Dominic72_odd, 
            NEW$PAPA_p4nadhd,
            NEW$PAPA_p4_adhd, 
            NEW$ODD,
            NEW$Conduct_Disorder,
            NEW$Number_Disorder_exclu_ADHD,
            NEW$Disorder_no_ADHD,
            NEW$ADHD, 
            NEW$Mother,	
            NEW$Father,	
            NEW$Teacher,
            NEW$ADHD_d) %>%
  tab_cols(total(), NEW$gender_male
          ) %>% 
  tab_stat_mean_sd_n() %>%
  tab_last_sig_means(subtable_marks = "both") %>% 
  tab_pivot() %>% 
  set_caption("Table with summary statistics and significance marks by sex")  

sink()

sink('demographics_table_by_site.txt')

NEW %>%
  tab_cells(NEW$gender_male,
            NEW$Hamilton,
            NEW$mom_age_birth, 
            NEW$above_college, 
            NEW$Pren_income4,
            NEW$Pren_life_events,
            NEW$gestation_age_wks,
            NEW$birth_wt_grams,
            NEW$Sibling,
            NEW$Pren_CESD, 
            NEW$HWB6_CESD,
            NEW$HWB12_CESD,	
            NEW$HWB24_CESD,
            NEW$HWB36_CESD, 
            NEW$HWB48_CESD,
            NEW$HWB60_CESD,
            NEW$HWB72_CESD,
            NEW$auc_post_cesd,
            NEW$PRS_0_001_adhd_child, 
            NEW$PC1, 
            NEW$PC2, 
            NEW$PC3, 
            NEW$conners_mother_adhd_score.60m,
            NEW$conners_mother_adhd_score.72m,
            NEW$conners_mother_cognitive_score.60m,
            NEW$conners_mother_cognitive_score.72m,
            NEW$conners_mother_hyperactivity_score.60m, 
            NEW$conners_mother_hyperactivity_score.72m,  
            NEW$SDQ60_mother_hyperactivity,
            NEW$SDQ72_mother_hyperactivity, 
            NEW$conners_father_adhd_score.60m,
            NEW$conners_father_adhd_score.72m,
            NEW$conners_father_cognitive_score.60m,
            NEW$conners_father_cognitive_score.72m,
            NEW$conners_father_hyperactivity_score.60m,
            NEW$conners_father_hyperactivity_score.72m,
            NEW$SDQ60_father_hyperactivity,
            NEW$SDQ72_father_hyperactivity,
            NEW$conners_teacher_adhd_score.72m,
            NEW$conners_teacher_cognitive_score.72m, 
            NEW$conners_teacher_hyperactivity_score.72m,
            NEW$SDQ72_teacher_hyperactivity,
            NEW$CBCL48_sc5raw.y,
            NEW$CBCL60_sc5raw.y,
            NEW$CBCL48_sc6raw.y,
            NEW$CBCL60_sc6raw.y,
            NEW$CBCL48_sc7raw.y,
            NEW$CBCL60_sc7raw.y,
            NEW$Dominic72_ADHD,
            NEW$Dominic72_cd,
            NEW$Dominic72_odd, 
            NEW$PAPA_p4nadhd,
            NEW$PAPA_p4_adhd, 
            NEW$ODD,
            NEW$Conduct_Disorder,
            NEW$Number_Disorder_exclu_ADHD,
            NEW$Disorder_no_ADHD,
            NEW$ADHD, 
            NEW$Mother,	
            NEW$Father,	
            NEW$Teacher,
            NEW$ADHD_d) %>%
  tab_cols(total(), NEW$Hamilton
  ) %>% 
  tab_stat_mean_sd_n() %>%
  tab_last_sig_means(subtable_marks = "both") %>% 
  tab_pivot() %>% 
  set_caption("Table with summary statistics and significance marks by site")  

sink()

sink('demographics_table_by_sibling.txt')

NEW %>%
  tab_cells(NEW$gender_male,
            NEW$Hamilton,
            NEW$mom_age_birth, 
            NEW$above_college, 
            NEW$Pren_income4,
            NEW$Pren_life_events,
            NEW$gestation_age_wks,
            NEW$birth_wt_grams,
            NEW$Sibling,
            NEW$Pren_CESD, 
            NEW$HWB6_CESD,
            NEW$HWB12_CESD,	
            NEW$HWB24_CESD,
            NEW$HWB36_CESD, 
            NEW$HWB48_CESD,
            NEW$HWB60_CESD,
            NEW$HWB72_CESD,
            NEW$auc_post_cesd,
            NEW$PRS_0_001_adhd_child, 
            NEW$PC1, 
            NEW$PC2, 
            NEW$PC3, 
            NEW$conners_mother_adhd_score.60m,
            NEW$conners_mother_adhd_score.72m,
            NEW$conners_mother_cognitive_score.60m,
            NEW$conners_mother_cognitive_score.72m,
            NEW$conners_mother_hyperactivity_score.60m, 
            NEW$conners_mother_hyperactivity_score.72m,  
            NEW$SDQ60_mother_hyperactivity,
            NEW$SDQ72_mother_hyperactivity, 
            NEW$conners_father_adhd_score.60m,
            NEW$conners_father_adhd_score.72m,
            NEW$conners_father_cognitive_score.60m,
            NEW$conners_father_cognitive_score.72m,
            NEW$conners_father_hyperactivity_score.60m,
            NEW$conners_father_hyperactivity_score.72m,
            NEW$SDQ60_father_hyperactivity,
            NEW$SDQ72_father_hyperactivity,
            NEW$conners_teacher_adhd_score.72m,
            NEW$conners_teacher_cognitive_score.72m, 
            NEW$conners_teacher_hyperactivity_score.72m,
            NEW$SDQ72_teacher_hyperactivity,
            NEW$CBCL48_sc5raw.y,
            NEW$CBCL60_sc5raw.y,
            NEW$CBCL48_sc6raw.y,
            NEW$CBCL60_sc6raw.y,
            NEW$CBCL48_sc7raw.y,
            NEW$CBCL60_sc7raw.y,
            NEW$Dominic72_ADHD,
            NEW$Dominic72_cd,
            NEW$Dominic72_odd, 
            NEW$PAPA_p4nadhd,
            NEW$PAPA_p4_adhd, 
            NEW$ODD,
            NEW$Conduct_Disorder,
            NEW$Number_Disorder_exclu_ADHD,
            NEW$Disorder_no_ADHD,
            NEW$ADHD, 
            NEW$Mother,	
            NEW$Father,	
            NEW$Teacher,
            NEW$ADHD_d) %>%
  tab_cols(total(), NEW$Sibling 
  ) %>% 
  tab_stat_mean_sd_n() %>%
  tab_last_sig_means(subtable_marks = "both") %>% 
  tab_pivot() %>% 
  set_caption("Table with summary statistics and significance marks by sibling")  

sink()

sink('demographics_table_by_adhd.txt')

NEW %>%
  tab_cells(NEW$gender_male,
            NEW$Hamilton,
            NEW$mom_age_birth, 
            NEW$above_college, 
            NEW$Pren_income4,
            NEW$Pren_life_events,
            NEW$gestation_age_wks,
            NEW$birth_wt_grams,
            NEW$Sibling,
            NEW$Pren_CESD, 
            NEW$HWB6_CESD,
            NEW$HWB12_CESD,	
            NEW$HWB24_CESD,
            NEW$HWB36_CESD, 
            NEW$HWB48_CESD,
            NEW$HWB60_CESD,
            NEW$HWB72_CESD,
            NEW$auc_post_cesd,
            NEW$PRS_0_001_adhd_child, 
            NEW$PC1, 
            NEW$PC2, 
            NEW$PC3, 
            NEW$conners_mother_adhd_score.60m,
            NEW$conners_mother_adhd_score.72m,
            NEW$conners_mother_cognitive_score.60m,
            NEW$conners_mother_cognitive_score.72m,
            NEW$conners_mother_hyperactivity_score.60m, 
            NEW$conners_mother_hyperactivity_score.72m,  
            NEW$SDQ60_mother_hyperactivity,
            NEW$SDQ72_mother_hyperactivity, 
            NEW$conners_father_adhd_score.60m,
            NEW$conners_father_adhd_score.72m,
            NEW$conners_father_cognitive_score.60m,
            NEW$conners_father_cognitive_score.72m,
            NEW$conners_father_hyperactivity_score.60m,
            NEW$conners_father_hyperactivity_score.72m,
            NEW$SDQ60_father_hyperactivity,
            NEW$SDQ72_father_hyperactivity,
            NEW$conners_teacher_adhd_score.72m,
            NEW$conners_teacher_cognitive_score.72m, 
            NEW$conners_teacher_hyperactivity_score.72m,
            NEW$SDQ72_teacher_hyperactivity,
            NEW$CBCL48_sc5raw.y,
            NEW$CBCL60_sc5raw.y,
            NEW$CBCL48_sc6raw.y,
            NEW$CBCL60_sc6raw.y,
            NEW$CBCL48_sc7raw.y,
            NEW$CBCL60_sc7raw.y,
            NEW$Dominic72_ADHD,
            NEW$Dominic72_cd,
            NEW$Dominic72_odd, 
            NEW$PAPA_p4nadhd,
            NEW$PAPA_p4_adhd, 
            NEW$ODD,
            NEW$Conduct_Disorder,
            NEW$Number_Disorder_exclu_ADHD,
            NEW$Disorder_no_ADHD,
            NEW$ADHD, 
            NEW$Mother,	
            NEW$Father,	
            NEW$Teacher,
            NEW$ADHD_d) %>%
  tab_cols(total(), NEW$ADHD_d 
  ) %>% 
  tab_stat_mean_sd_n() %>%
  tab_last_sig_means(subtable_marks = "both") %>% 
  tab_pivot() %>% 
  set_caption("Table with summary statistics and significance marks by ADHD factor")  

sink()

# png(filename = 'sex_differences_characteristics.png', width=1000, height=400)
# par(mfrow=c(1,4))
# myBox <- function(x, y, ylab, xlab) boxplot(x ~ y, col=c("#FF6600", "#0000CC"), names=c("Girls", "Boys"),ylab=ylab, xlab=xlab)
# mapply(myBox,
#        x = NEW[,c("Pren_CESD", "HWB6_CESD", "HWB12_CESD", "HWB24_CESD", "HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "auc_post_cesd")],
#        y = list(NEW$gender_male), # we make this a list so it has length(1)
#        ylab = c("Pren_CESD", "HWB6_CESD", "HWB12_CESD", "HWB24_CESD", "HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "auc_post_cesd"),
#        xlab = "" # empty x-lab
# )
# dev.off()

sink('sex_differences_kk_adhd_demographics.txt')

# NEW$gender_male,
chisq.test(NEW$gender_male, NEW$gender_male, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Hamilton, NEW$gender_male, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
kruskal.test(mom_age_birth ~ gender_male, data=NEW)
chisq.test(NEW$above_college, NEW$gender_male, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
kruskal.test(Pren_income4 ~ gender_male, data=NEW)
kruskal.test(Pren_life_events ~ gender_male, data=NEW)
kruskal.test(gestation_age_wks ~ gender_male, data=NEW)
kruskal.test(birth_wt_grams ~ gender_male, data=NEW)
chisq.test(NEW$Sibling, NEW$gender_male, correct=FALSE)
kruskal.test(Pren_CESD ~ gender_male, data=NEW)
kruskal.test(HWB6_CESD ~ gender_male, data=NEW)
kruskal.test(HWB12_CESD ~ gender_male, data=NEW)
kruskal.test(HWB24_CESD ~ gender_male, data=NEW)
kruskal.test(HWB36_CESD ~ gender_male, data=NEW)
kruskal.test(HWB48_CESD ~ gender_male, data=NEW)
kruskal.test(HWB60_CESD ~ gender_male, data=NEW)
kruskal.test(HWB72_CESD ~ gender_male, data=NEW)
kruskal.test(auc_post_cesd ~ gender_male, data=NEW)
kruskal.test(PRS_0_001_adhd_child ~ gender_male, data=NEW)
kruskal.test(PC1 ~ gender_male, data=NEW)
kruskal.test(PC2 ~ gender_male, data=NEW)
kruskal.test(PC3 ~ gender_male, data=NEW)
kruskal.test(conners_mother_adhd_score.60m ~ gender_male, data=NEW)
kruskal.test(conners_mother_adhd_score.72m ~ gender_male, data=NEW)
kruskal.test(conners_mother_cognitive_score.60m ~ gender_male, data=NEW)
kruskal.test(conners_mother_cognitive_score.72m ~ gender_male, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.60m ~ gender_male, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.72m ~ gender_male, data=NEW)
kruskal.test(SDQ60_mother_hyperactivity ~ gender_male, data=NEW)
kruskal.test(SDQ72_mother_hyperactivity ~ gender_male, data=NEW)
kruskal.test(conners_father_adhd_score.60m ~ gender_male, data=NEW)
kruskal.test(conners_father_adhd_score.72m ~ gender_male, data=NEW)
kruskal.test(conners_father_cognitive_score.60m ~ gender_male, data=NEW)
kruskal.test(conners_father_cognitive_score.72m ~ gender_male, data=NEW)
kruskal.test(conners_father_hyperactivity_score.60m ~ gender_male, data=NEW)
kruskal.test(conners_father_hyperactivity_score.72m ~ gender_male, data=NEW)
kruskal.test(SDQ60_father_hyperactivity ~ gender_male, data=NEW)
kruskal.test(SDQ72_father_hyperactivity ~ gender_male, data=NEW)
kruskal.test(conners_teacher_adhd_score.72m ~ gender_male, data=NEW)
kruskal.test(conners_teacher_cognitive_score.72m ~ gender_male, data=NEW)
kruskal.test(conners_teacher_hyperactivity_score.72m ~ gender_male, data=NEW)
kruskal.test(SDQ72_teacher_hyperactivity ~ gender_male, data=NEW)
kruskal.test(CBCL48_sc5raw.y ~ gender_male, data=NEW)
kruskal.test(CBCL60_sc5raw.y ~ gender_male, data=NEW)
kruskal.test(CBCL48_sc6raw.y ~ gender_male, data=NEW)
kruskal.test(CBCL60_sc6raw.y ~ gender_male, data=NEW)
kruskal.test(CBCL48_sc7raw.y ~ gender_male, data=NEW)
kruskal.test(CBCL60_sc7raw.y ~ gender_male, data=NEW)
kruskal.test(Dominic72_ADHD ~ gender_male, data=NEW)
kruskal.test(Dominic72_cd ~ gender_male, data=NEW)
kruskal.test(Dominic72_odd ~ gender_male, data=NEW)
kruskal.test(PAPA_p4nadhd ~ gender_male, data=NEW)
chisq.test(NEW$PAPA_p4_adhd, NEW$gender_male, correct=FALSE)
chisq.test(NEW$ODD, NEW$gender_male, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Conduct_Disorder, NEW$gender_male, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
kruskal.test(Number_Disorder_exclu_ADHD ~ gender_male, data=NEW)
chisq.test(NEW$Disorder_no_ADHD, NEW$gender_male, correct=FALSE) 
kruskal.test(ADHD ~ gender_male, data=NEW)
kruskal.test(Mother ~ gender_male, data=NEW)
kruskal.test(Father ~ gender_male, data=NEW)
kruskal.test(Teacher ~ gender_male, data=NEW)
chisq.test(NEW$ADHD_d, NEW$gender_male, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

sink()

sink('Sibling_differences_kk_adhd_demographics.txt')

# NEW$gender_male,
chisq.test(NEW$gender_male, NEW$Sibling, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Hamilton, NEW$Sibling, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
kruskal.test(mom_age_birth ~ Sibling, data=NEW)
chisq.test(NEW$above_college, NEW$Sibling, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
kruskal.test(Pren_income4 ~ Sibling, data=NEW)
kruskal.test(Pren_life_events ~ Sibling, data=NEW)
kruskal.test(gestation_age_wks ~ Sibling, data=NEW)
kruskal.test(birth_wt_grams ~ Sibling, data=NEW)
chisq.test(NEW$Sibling, NEW$Sibling, correct=FALSE)
kruskal.test(Pren_CESD ~ Sibling, data=NEW)
kruskal.test(HWB6_CESD ~ Sibling, data=NEW)
kruskal.test(HWB12_CESD ~ Sibling, data=NEW)
kruskal.test(HWB24_CESD ~ Sibling, data=NEW)
kruskal.test(HWB36_CESD ~ Sibling, data=NEW)
kruskal.test(HWB48_CESD ~ Sibling, data=NEW)
kruskal.test(HWB60_CESD ~ Sibling, data=NEW)
kruskal.test(HWB72_CESD ~ Sibling, data=NEW)
kruskal.test(auc_post_cesd ~ Sibling, data=NEW)
kruskal.test(PRS_0_001_adhd_child ~ Sibling, data=NEW)
kruskal.test(PC1 ~ Sibling, data=NEW)
kruskal.test(PC2 ~ Sibling, data=NEW)
kruskal.test(PC3 ~ Sibling, data=NEW)
kruskal.test(conners_mother_adhd_score.60m ~ Sibling, data=NEW)
kruskal.test(conners_mother_adhd_score.72m ~ Sibling, data=NEW)
kruskal.test(conners_mother_cognitive_score.60m ~ Sibling, data=NEW)
kruskal.test(conners_mother_cognitive_score.72m ~ Sibling, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.60m ~ Sibling, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.72m ~ Sibling, data=NEW)
kruskal.test(SDQ60_mother_hyperactivity ~ Sibling, data=NEW)
kruskal.test(SDQ72_mother_hyperactivity ~ Sibling, data=NEW)
kruskal.test(conners_father_adhd_score.60m ~ Sibling, data=NEW)
kruskal.test(conners_father_adhd_score.72m ~ Sibling, data=NEW)
kruskal.test(conners_father_cognitive_score.60m ~ Sibling, data=NEW)
kruskal.test(conners_father_cognitive_score.72m ~ Sibling, data=NEW)
kruskal.test(conners_father_hyperactivity_score.60m ~ Sibling, data=NEW)
kruskal.test(conners_father_hyperactivity_score.72m ~ Sibling, data=NEW)
kruskal.test(SDQ60_father_hyperactivity ~ Sibling, data=NEW)
kruskal.test(SDQ72_father_hyperactivity ~ Sibling, data=NEW)
kruskal.test(conners_teacher_adhd_score.72m ~ Sibling, data=NEW)
kruskal.test(conners_teacher_cognitive_score.72m ~ Sibling, data=NEW)
kruskal.test(conners_teacher_hyperactivity_score.72m ~ Sibling, data=NEW)
kruskal.test(SDQ72_teacher_hyperactivity ~ Sibling, data=NEW)
kruskal.test(CBCL48_sc5raw.y ~ Sibling, data=NEW)
kruskal.test(CBCL60_sc5raw.y ~ Sibling, data=NEW)
kruskal.test(CBCL48_sc6raw.y ~ Sibling, data=NEW)
kruskal.test(CBCL60_sc6raw.y ~ Sibling, data=NEW)
kruskal.test(CBCL48_sc7raw.y ~ Sibling, data=NEW)
kruskal.test(CBCL60_sc7raw.y ~ Sibling, data=NEW)
kruskal.test(Dominic72_ADHD ~ Sibling, data=NEW)
kruskal.test(Dominic72_cd ~ Sibling, data=NEW)
kruskal.test(Dominic72_odd ~ Sibling, data=NEW)
kruskal.test(PAPA_p4nadhd ~ Sibling, data=NEW)
chisq.test(NEW$PAPA_p4_adhd, NEW$Sibling, correct=FALSE)
chisq.test(NEW$ODD, NEW$Sibling, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Conduct_Disorder, NEW$Sibling, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
kruskal.test(Number_Disorder_exclu_ADHD ~ Sibling, data=NEW)
chisq.test(NEW$Disorder_no_ADHD, NEW$Sibling, correct=FALSE) 
kruskal.test(ADHD ~ Sibling, data=NEW)
kruskal.test(Mother ~ Sibling, data=NEW)
kruskal.test(Father ~ Sibling, data=NEW)
kruskal.test(Teacher ~ Sibling, data=NEW)
chisq.test(NEW$ADHD_d, NEW$Sibling, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

sink()

sink('Site_differences_kk_adhd_demographics.txt')
chisq.test(NEW$gender_male, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Hamilton, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
kruskal.test(mom_age_birth ~ Hamilton, data=NEW)
chisq.test(NEW$above_college, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
kruskal.test(Pren_income4 ~ Hamilton, data=NEW)
kruskal.test(Pren_life_events ~ Hamilton, data=NEW)
kruskal.test(gestation_age_wks ~ Hamilton, data=NEW)
kruskal.test(birth_wt_grams ~ Hamilton, data=NEW)
chisq.test(NEW$Sibling, NEW$Hamilton, correct=FALSE)
kruskal.test(Pren_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB6_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB12_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB24_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB36_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB48_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB60_CESD ~ Hamilton, data=NEW)
kruskal.test(HWB72_CESD ~ Hamilton, data=NEW)
kruskal.test(auc_post_cesd ~ Hamilton, data=NEW)
kruskal.test(PRS_0_001_adhd_child ~ Hamilton, data=NEW)
kruskal.test(PC1 ~ Hamilton, data=NEW)
kruskal.test(PC2 ~ Hamilton, data=NEW)
kruskal.test(PC3 ~ Hamilton, data=NEW)
kruskal.test(conners_mother_adhd_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_mother_adhd_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_mother_cognitive_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_mother_cognitive_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.72m ~ Hamilton, data=NEW)
kruskal.test(SDQ60_mother_hyperactivity ~ Hamilton, data=NEW)
kruskal.test(SDQ72_mother_hyperactivity ~ Hamilton, data=NEW)
kruskal.test(conners_father_adhd_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_father_adhd_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_father_cognitive_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_father_cognitive_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_father_hyperactivity_score.60m ~ Hamilton, data=NEW)
kruskal.test(conners_father_hyperactivity_score.72m ~ Hamilton, data=NEW)
kruskal.test(SDQ60_father_hyperactivity ~ Hamilton, data=NEW)
kruskal.test(SDQ72_father_hyperactivity ~ Hamilton, data=NEW)
kruskal.test(conners_teacher_adhd_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_teacher_cognitive_score.72m ~ Hamilton, data=NEW)
kruskal.test(conners_teacher_hyperactivity_score.72m ~ Hamilton, data=NEW)
kruskal.test(SDQ72_teacher_hyperactivity ~ Hamilton, data=NEW)
kruskal.test(CBCL48_sc5raw.y ~ Hamilton, data=NEW)
kruskal.test(CBCL60_sc5raw.y ~ Hamilton, data=NEW)
kruskal.test(CBCL48_sc6raw.y ~ Hamilton, data=NEW)
kruskal.test(CBCL60_sc6raw.y ~ Hamilton, data=NEW)
kruskal.test(CBCL48_sc7raw.y ~ Hamilton, data=NEW)
kruskal.test(CBCL60_sc7raw.y ~ Hamilton, data=NEW)
kruskal.test(Dominic72_ADHD ~ Hamilton, data=NEW)
kruskal.test(Dominic72_cd ~ Hamilton, data=NEW)
kruskal.test(Dominic72_odd ~ Hamilton, data=NEW)
kruskal.test(PAPA_p4nadhd ~ Hamilton, data=NEW)
chisq.test(NEW$PAPA_p4_adhd, NEW$Hamilton, correct=FALSE)
chisq.test(NEW$ODD, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Conduct_Disorder, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
kruskal.test(Number_Disorder_exclu_ADHD ~ Hamilton, data=NEW)
chisq.test(NEW$Disorder_no_ADHD, NEW$Hamilton, correct=FALSE) 
kruskal.test(ADHD ~ Hamilton, data=NEW)
kruskal.test(Mother ~ Hamilton, data=NEW)
kruskal.test(Father ~ Hamilton, data=NEW)
kruskal.test(Teacher ~ Hamilton, data=NEW)
chisq.test(NEW$ADHD_d, NEW$Hamilton, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

sink()

sink('ADHD_factor_differences_kk_adhd_demographics.txt')

chisq.test(NEW$gender_male, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Hamilton, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
kruskal.test(mom_age_birth ~ ADHD_d, data=NEW)
chisq.test(NEW$above_college, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
kruskal.test(Pren_income4 ~ ADHD_d, data=NEW)
kruskal.test(Pren_life_events ~ ADHD_d, data=NEW)
kruskal.test(gestation_age_wks ~ ADHD_d, data=NEW)
kruskal.test(birth_wt_grams ~ ADHD_d, data=NEW)
chisq.test(NEW$Sibling, NEW$ADHD_d, correct=FALSE)
kruskal.test(Pren_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB6_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB12_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB24_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB36_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB48_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB60_CESD ~ ADHD_d, data=NEW)
kruskal.test(HWB72_CESD ~ ADHD_d, data=NEW)
kruskal.test(auc_post_cesd ~ ADHD_d, data=NEW)
kruskal.test(PRS_0_001_adhd_child ~ ADHD_d, data=NEW)
kruskal.test(PC1 ~ ADHD_d, data=NEW)
kruskal.test(PC2 ~ ADHD_d, data=NEW)
kruskal.test(PC3 ~ ADHD_d, data=NEW)
kruskal.test(conners_mother_adhd_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_mother_adhd_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_mother_cognitive_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_mother_cognitive_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_mother_hyperactivity_score.72m ~ ADHD_d, data=NEW)
kruskal.test(SDQ60_mother_hyperactivity ~ ADHD_d, data=NEW)
kruskal.test(SDQ72_mother_hyperactivity ~ ADHD_d, data=NEW)
kruskal.test(conners_father_adhd_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_adhd_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_cognitive_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_cognitive_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_hyperactivity_score.60m ~ ADHD_d, data=NEW)
kruskal.test(conners_father_hyperactivity_score.72m ~ ADHD_d, data=NEW)
kruskal.test(SDQ60_father_hyperactivity ~ ADHD_d, data=NEW)
kruskal.test(SDQ72_father_hyperactivity ~ ADHD_d, data=NEW)
kruskal.test(conners_teacher_adhd_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_teacher_cognitive_score.72m ~ ADHD_d, data=NEW)
kruskal.test(conners_teacher_hyperactivity_score.72m ~ ADHD_d, data=NEW)
kruskal.test(SDQ72_teacher_hyperactivity ~ ADHD_d, data=NEW)
kruskal.test(CBCL48_sc5raw.y ~ ADHD_d, data=NEW)
kruskal.test(CBCL60_sc5raw.y ~ ADHD_d, data=NEW)
kruskal.test(CBCL48_sc6raw.y ~ ADHD_d, data=NEW)
kruskal.test(CBCL60_sc6raw.y ~ ADHD_d, data=NEW)
kruskal.test(CBCL48_sc7raw.y ~ ADHD_d, data=NEW)
kruskal.test(CBCL60_sc7raw.y ~ ADHD_d, data=NEW)
kruskal.test(Dominic72_ADHD ~ ADHD_d, data=NEW)
kruskal.test(Dominic72_cd ~ ADHD_d, data=NEW)
kruskal.test(Dominic72_odd ~ ADHD_d, data=NEW)
kruskal.test(PAPA_p4nadhd ~ ADHD_d, data=NEW)
chisq.test(NEW$PAPA_p4_adhd, NEW$ADHD_d, correct=FALSE)
chisq.test(NEW$ODD, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
chisq.test(NEW$Conduct_Disorder, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant
kruskal.test(Number_Disorder_exclu_ADHD ~ ADHD_d, data=NEW)
chisq.test(NEW$Disorder_no_ADHD, NEW$ADHD_d, correct=FALSE) 
kruskal.test(ADHD ~ ADHD_d, data=NEW)
kruskal.test(Mother ~ ADHD_d, data=NEW)
kruskal.test(Father ~ ADHD_d, data=NEW)
kruskal.test(Teacher ~ ADHD_d, data=NEW)
chisq.test(NEW$ADHD_d, NEW$ADHD_d, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

sink()



