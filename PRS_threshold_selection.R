#Marie-Elyse Lafaille-Magnan, PhD
#marie-elyse.lafaille-magnan@mail.mcgill.ca

setwd("/Users/Marie-Elyse/Downloads")
NEW = read.csv("MAVAN_48M_and_up_jun2020_new.csv")

sink('threshold_selection.txt')

fit1 <- lm(ADHD ~ PRS_0_5_adhd_child + PC1 +PC2 +PC3, data=NEW)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(fit1)
lmtest::bptest(fit1) #studentized Breusch-Pagan test

fit2 <- lm(ADHD ~ PRS_0_2_adhd_child + PC1 +PC2 +PC3, data=NEW)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(fit2)
lmtest::bptest(fit2) #studentized Breusch-Pagan test

fit3 <- lm(ADHD ~ PRS_0_1_adhd_child + PC1 +PC2 +PC3, data=NEW)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(fit3)
lmtest::bptest(fit3) #studentized Breusch-Pagan test

fit4 <- lm(ADHD ~ PRS_0_05_adhd_child + PC1 +PC2 +PC3, data=NEW)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(fit4)
lmtest::bptest(fit4) #studentized Breusch-Pagan test

fit5 <- lm(ADHD ~ PRS_0_01_adhd_child + PC1 +PC2 +PC3, data=NEW)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(fit5)
lmtest::bptest(fit5) #studentized Breusch-Pagan test

fit6 <- lm(ADHD ~ PRS_0_001_adhd_child + PC1 +PC2 +PC3, data=NEW)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(fit6)
lmtest::bptest(fit6) #studentized Breusch-Pagan test

fit7 <- lm(ADHD ~ PRS_0_0001_adhd_child + PC1 +PC2 +PC3, data=NEW)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(fit7)
lmtest::bptest(fit7) #studentized Breusch-Pagan test

summary(fit1)
y1 <-summary(fit1)$r.squared
summary(fit1)$adj.r.squared
summary(fit2)
y2 <- summary(fit2)$r.squared
summary(fit2)$adj.r.squared
summary(fit3)
y3 <-summary(fit3)$r.squared
summary(fit3)$adj.r.squared
summary(fit4)
y4 <-summary(fit4)$r.squared
summary(fit4)$adj.r.squared
summary(fit5)
y5 <-summary(fit5)$r.squared
summary(fit5)$adj.r.squared
summary(fit6)
y6 <-summary(fit6)$r.squared
summary(fit6)$adj.r.squared
summary(fit7)
y7 <-summary(fit7)$r.squared
summary(fit7)$adj.r.squared

AIC <- AIC(fit1, fit2, fit3, fit4, fit5, fit6, fit7)
BIC <- BIC(fit1, fit2, fit3, fit4, fit5, fit6, fit7)

r.squared <- c(y1, y2,  y3,  y4,  y5,  y6,  y7)
PRS.threshold <- c(0.5,  0.2,  0.1,  0.05,  0.01,  0.001, 0.0001)

print(AIC)
print(BIC)

print(r.squared)
print(PRS.threshold)

# install.packages("broom")
# library(broom)

glance(fit1) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)
glance(fit2) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)
glance(fit3) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)
glance(fit4) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)
glance(fit5) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)
glance(fit6) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)
glance(fit7) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)
sink()
