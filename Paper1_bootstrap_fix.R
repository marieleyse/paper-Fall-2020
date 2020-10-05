#Marie-Elyse Lafaille-Magnan, PhD
#marie-elyse.lafaille-magnan@mail.mcgill.ca
#Alexia Jolicoeur-Martineau, PhD (c)

setwd("/Users/Marie-Elyse/Downloads")
NEW = read.csv("MAVAN_48M_and_up_jun2020_new.csv")

#REFFERED TO MEAN_CENTERED BUT ZSCORE

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

# data(NEW)
# attach(NEW)

set.seed(1234)

fit4 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd*gender_male + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + Hamilton, data=NEW)
lboot1 <- lm.boot(fit4, R = 5000)
sink('fit_model_scale_prs_0_001_adhd_child_auc_post_cesd_summary_bootstrap.txt')

# get_rsquare_mean = function(boot_list){
#   rsquare_mean = 0
#   for (i in 1:length(boot_list)){
#     print(boot_list[[i]]$rsquare)
#     rsquare_mean =+ boot_list[[i]]$rsquare
#   }
#   return(rsquare_mean/length(boot_list))
# }

get_rsquare_mean = function(boot_list){
  rsquare_mean = 0
  for (i in 1:length(boot_list)){
    rsquare_mean = rsquare_mean + boot_list[[i]]$rsquare
  }
  return(rsquare_mean/length(boot_list))
}

get_coef_mean = function(boot_list){
  coef_mean = rep(0, 15)
  for (i in 1:length(boot_list)){
    for(j in 1:length(boot_list[[i]]$coef)){
      coef_mean[j] = coef_mean[j] + boot_list[[i]]$coef[[j]]
    }
  }
  return(coef_mean/length(boot_list))
}

get_rsquare_mean(lboot1$boot.list)
get_coef_mean(lboot1$boot.list)
print(summary(lboot1))
sink()

####CI by hand with mean estimate plus minus 1.96x(bootstrap standard deviation)

####

set.seed(1234)
fit4 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd*gender_male + mean_centered_mom_age_birth + above_college + Hamilton, data=NEW)
lboot1 <- lm.boot(fit4, R = 5000)
sink('fit_model_scale_auc_post_cesd_summary_bootstrap.txt')
get_rsquare_mean = function(boot_list){
  rsquare_mean = 0
  for (i in 1:length(boot_list)){
    rsquare_mean = rsquare_mean + boot_list[[i]]$rsquare
  }
  return(rsquare_mean/length(boot_list))
}

get_coef_mean = function(boot_list){
  coef_mean = rep(0, 15)
  for (i in 1:length(boot_list)){
    for(j in 1:length(boot_list[[i]]$coef)){
      coef_mean[j] = coef_mean[j] + boot_list[[i]]$coef[[j]]
    }
  }
  return(coef_mean/length(boot_list))
}
get_rsquare_mean(lboot1$boot.list)
get_coef_mean(lboot1$boot.list)
print(summary(lboot1))
sink()

####

data(NEW)
attach(NEW)
set.seed(1234)
fit4 <-lm(mean_centered_conners_mother_hyperactivity_score.72m ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd*gender_male + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + Hamilton, data=NEW)
lboot1 <- lm.boot(fit4, R = 5000)
sink('fit_model_scale_Conners_prs_0_001_adhd_child_auc_post_cesd_summary_bootstrap.txt')
get_rsquare_mean = function(boot_list){
  rsquare_mean = 0
  for (i in 1:length(boot_list)){
    rsquare_mean = rsquare_mean + boot_list[[i]]$rsquare
  }
  return(rsquare_mean/length(boot_list))
}

get_coef_mean = function(boot_list){
  coef_mean = rep(0, 15)
  for (i in 1:length(boot_list)){
    for(j in 1:length(boot_list[[i]]$coef)){
      coef_mean[j] = coef_mean[j] + boot_list[[i]]$coef[[j]]
    }
  }
  return(coef_mean/length(boot_list))
}
get_rsquare_mean(lboot1$boot.list)
get_coef_mean(lboot1$boot.list)
print(summary(lboot1))
sink()

####
data(NEW)
attach(NEW)
set.seed(1234)
fit4 <-lm(mean_centered_conners_mother_hyperactivity_score.72m ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd*gender_male + mean_centered_mom_age_birth + above_college + Hamilton, data=NEW)
lboot1 <- lm.boot(fit4, R = 5000)
sink('fit_model_conners_scale_auc_post_cesd_summary_bootstrap.txt')
get_rsquare_mean = function(boot_list){
  rsquare_mean = 0
  for (i in 1:length(boot_list)){
    rsquare_mean = rsquare_mean + boot_list[[i]]$rsquare
  }
  return(rsquare_mean/length(boot_list))
}

get_coef_mean = function(boot_list){
  coef_mean = rep(0, 15)
  for (i in 1:length(boot_list)){
    for(j in 1:length(boot_list[[i]]$coef)){
      coef_mean[j] = coef_mean[j] + boot_list[[i]]$coef[[j]]
    }
  }
  return(coef_mean/length(boot_list))
}
get_rsquare_mean(lboot1$boot.list)
get_coef_mean(lboot1$boot.list)
print(summary(lboot1))
sink()

