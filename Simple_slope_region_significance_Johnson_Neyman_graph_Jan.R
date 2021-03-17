#Marie-Elyse Lafaille-Magnan, PhD
#marie-elyse.lafaille-magnan@mail.mcgill.ca

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
NEW$mean_centered_Smoking_During_Pregnancy = c(scale(NEW$Smoking_During_Pregnancy)) 
NEW$mean_centered_Pren_income4 = c(scale(NEW$Pren_income4)) 
NEW$mean_centered_conners_mother_hyperactivity_score.72m = c(scale(NEW$conners_mother_hyperactivity_score.72m)) 
NEW$gender_male <- to_factor(NEW$gender_male)

options("jtools-digits" = 2)                

sink('fit_model_4_3_way_simple_slope.txt')
fit5 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd*gender_male + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + Hamilton, data=NEW)
fit2 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*gender_male + mean_centered_auc_post_cesd + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + Hamilton, data=NEW)
fit3 <-lm(mean_centered_ADHD ~ mean_centered_auc_post_cesd*gender_male + mean_centered_Pren_CESD + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + Hamilton, data=NEW)


# sim_slopes(fit4, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, mod2 = gender_male)
# sim_slopes(fit4, pred = mean_centered_auc_post_cesd, modx = mean_centered_Pren_CESD, mod2 = gender_male)
sim_slopes(fit2, pred = mean_centered_Pren_CESD, modx = gender_male)
sim_slopes(fit3, pred = mean_centered_auc_post_cesd, modx = gender_male)
sim_slopes(fit4, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd)
sim_slopes(fit4, pred = mean_centered_auc_post_cesd, modx = mean_centered_Pren_CESD)
simple_slopes(fit2)
simple_slopes(fit3)

sink('fit_model_prepost_2_way_simple_slope.txt')
fit4 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + above_college + Hamilton, data=NEW)
sim_slopes(fit4, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd)
sim_slopes(fit4, pred = mean_centered_auc_post_cesd, modx = mean_centered_Pren_CESD)
simple_slopes(fit4)
sink()

sink('fit_model_prepost_2_way_simple_slope.txt')
fit4 <-lm(mean_centered_ADHD ~ Pren_CESD*auc_post_cesd + PRS_0_001_adhd_child + PC1 + PC2 + PC3 + mean_centered_mom_age_birth + above_college + above_college + Hamilton, data=NEW)
sim_slopes(fit4, pred =Pren_CESD, modx = auc_post_cesd)
sim_slopes(fit4, pred = auc_post_cesd, modx = Pren_CESD)
simple_slopes(fit4)
sink()

sink('fit_model_4_3_way_simple_slope_rawdata.txt')
fit4 <-lm(ADHD ~ Pren_CESD*auc_post_cesd*gender_male + PRS_0_001_adhd_child + PC1 + PC2 + PC3 + mom_age_birth + above_college + Hamilton, data=NEW)
interactions::sim_slopes(fit4, pred = Pren_CESD, modx = auc_post_cesd, mod2 = gender_male)
interactions::sim_slopes(fit4, pred = auc_post_cesd, modx = Pren_CESD, mod2 = gender_male)
reghelper::simple_slopes(fit4)
sink()

sink('fit_model_4_3_way_simple_slope_rawdata_part2.txt')
modelEffectSizes(fit4)
options(max.print=999999)
simple_slopes(fit4, levels=list(auc_post_cesd=c(0:2000), Pren_CESD=c(0:54), gender_male=c(0, 1, 'sstest')))
simpleSlope(fit4, pred = Pren_CESD, modx = auc_post_cesd, mod2 = gender_male)
sink()

sink('/Users/Marie-Elyse/Downloads/fit_model_4_3_way_simple_slope_centereddata.txt')
fit5 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd*gender_male + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + Hamilton, data=NEW)
interactions::sim_slopes(fit5, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, mod2 = gender_male)
interactions::sim_slopes(fit5, pred = mean_centered_auc_post_cesd, modx = mean_centered_Pren_CESD, mod2 = gender_male)
reghelper::simple_slopes(fit5)
sink()

print(summary(NEW$mean_centered_auc_post_cesd))
print(summary(NEW$mean_centered_Pren_CESD))
sink('/Users/Marie-Elyse/Downloads/fit_model_4_3_way_simple_slope_centereddata_part2.txt')
fit5 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_auc_post_cesd*gender_male + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + Hamilton, data=NEW)
lmSupport::modelEffectSizes(fit5)
options(max.print=999999)
reghelper::simple_slopes(fit5, levels=list(mean_centered_auc_post_cesd=c(-1.4668:3.9161), mean_centered_Pren_CESD=c(-1.2380:3.7450), gender_male=c(0, 1, 'sstest')))
pequod::simpleSlope(fit5, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, mod2 = gender_male)
sink()

print(fit5$coefficients)
coef_names <- coef_names[1:14]
# (Intercept)                                                     mean_centered_Pren_CESD 
# -0.3560845973                                                    -0.0062396804 
# mean_centered_auc_post_cesd                                                     gender_male1 
# 0.1392320870                                                     0.5241239516 
# mean_centered_PRS_0_001_adhd_child                                                mean_centered_PC1 
# -0.0451191642                                                     0.0565969770 
# mean_centered_PC2                                                mean_centered_PC3 
# 0.0185709335                                                     0.0499225428 
# mean_centered_mom_age_birth                                                         Hamilton 
# -0.2157769412                                                     0.2980639233 
# mean_centered_Pren_CESD:mean_centered_auc_post_cesd                             mean_centered_Pren_CESD:gender_male1 
# 0.0005998688                                                     0.2127044560 
# mean_centered_auc_post_cesd:gender_male1 mean_centered_Pren_CESD:mean_centered_auc_post_cesd:gender_male1 
# 0.0673789866                                                    -0.3441356671

pdf("interaction_plot_pre_post_sex.pdf", width=10, height=4)
par(mfrow=c(1,3)) # 

library(sjPlot)
library(sjmisc)
library(ggplot2)
library(sjlabelled)

theme_set(theme_sjplot())

fit5 <-lm(mean_centered_ADHD ~ mean_centered_Pren_CESD*mean_centered_Pren_CESD*gender_male + mean_centered_PRS_0_001_adhd_child + mean_centered_PC1 + mean_centered_PC2 + mean_centered_PC3 + mean_centered_mom_age_birth + above_college + gender_male + Hamilton, data=NEW)

# get_model_data(fit5, type = "int", title = "", colors = "bw")
# write.csv2(print(get_model_data(fit5, type = "int", title = "", colors = "bw")), file = "Three3-way-interaction.csv")
# data.graph <- read.csv(file.choose("Three3-way-interaction.csv"), header=TRUE, sep = ";", dec = ",")
# 
# p <- ggplot(data = data.graph, aes(x = mean_centered_Pren_CESD, y = PredictedMeans, group = mean_centered_auc_post_cesd:gender_male)) + geom_line(size=0.8, aes(linetype=mean_centered_auc_post_cesd, color=gender_male)) + geom_point(size=2, aes(shape=gender_male, color=gender_male))
# p + scale_linetype_manual(values=c("dotdash", "solid")) + scale_color_manual(values=c('#000000','#848484')) + theme_classic() + theme(axis.text=element_text(size=12, color="black"), axis.title=element_text(size=14,face="bold", color="black")) + labs(y="Outcome") + ylim(2.75, 3.5)
# ggsave("3-way.jpg", width = 5, height = 5 , dpi=1200)

# try <- ggpredict(fit5, terms = c("mean_centered_Pren_CESD", "mean_centered_Pren_CESD", "gender_male"))
# ggplot(try, aes(x, predicted, colour = group)) + geom_line()

# #fit <- lm(conners_mother_adhd_score.72m ~ PRS_0_05_sdq_child*gender_male + PC1 +PC2 + PC3, data=NEW8)
# ggplot(NEW8,aes(conners_mother_adhd_score.72m,x=PRS_0_05_sdq_child,color=gender_male))+geom_point()+
#   stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
#   stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])
# 
# equation1=function(x){((coef(fit5)[2]*x)*coef(fit5)[3]*x)*coef(fit5)[4])+(coef(fit5)[3]*x)*coef(fit5)[4]))+coef(fit5)[1]+coef(fit5)[4]+coef(fit5)[5]*x+coef(fit5)[6]+coef(fit5)[7]+coef(fit5)[8]+coef(fit5)[9]+coef(fit5)[10]}
# equation2=function(x){((coef(fit5)[2]*x)*coef(fit5)[3]*x)*coef(fit5)[4])+(coef(fit5)[3]*x)*coef(fit5)[4]))+coef(fit5)[1]+coef(fit5)[4]+coef(fit5)[5]*x+coef(fit5)[6]+coef(fit5)[7]+coef(fit5)[8]+coef(fit5)[9]+coef(fit5)[10]}
# 
# ggplot(NEW,aes(mean_centered_ADHD,x=mean_centered_auc_post_cesd,color=gender_male))+geom_point()+
#   stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
#   stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])

# pdf("data_group_color_size.pdf", width=10, height=4)
# ggplot(data=NEW, aes(x=mean_centered_Pren_CESD, y=mean_centered_ADHD, colour=gender_male, size=mean_centered_auc_post_cesd)) +
#   geom_point()
# dev.off()

#png(filename = 'nj_plot.png', width=1000, height=400)
pdf("nj_plot.pdf", width=10, height=4)
sim_slopes(fit5, pred = mean_centered_Pren_CESD, modx = mean_centered_auc_post_cesd, mod2 = gender_male, jnplot = TRUE,
           mod2.labels = c("Girls","Boys"),
           modx.labels = "Postnatal depression",
           y.labels = "Slope of Prenatal depression",
           main.title = "Johnson-Neyman plot")
dev.off()

pdf("nj_plot2.pdf", width=10, height=4)
sim_slopes(fit5, pred = mean_centered_auc_post_cesd, modx = mean_centered_Pren_CESD, mod2 = gender_male, jnplot = TRUE,
           mod2.labels = c("Girls","Boys"),
           modx.labels = "Prenatal depression",
           y.labels = "Slope of Postnatal depression",
           main.title = "Johnson-Neyman plot")
dev.off()

modelEffectSizes(fit5)
options(max.print=999999)
simple_slopes(fit5, levels=list(auc_post_cesd=c(0:54), Pren_CESD=c(0:54), gender_male=c(0, 1, 'sstest')))
simpleSlope(fit5, pred = Pren_CESD, modx = auc_post_cesd, mod2 = gender_male)
