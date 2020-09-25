#Marie-Elyse Lafaille-Magnan, PhD
#marie-elyse.lafaille-magnan@mail.mcgill.ca

setwd("/Users/Marie-Elyse/Downloads")
#NEW = read.csv("MAVAN_48M_and_up_april2020.csv")
NEW <- read.csv("MAVAN_48M_and_up_jun2020.csv")

# "Pren_CESD", "HWB6_CESD",	"HWB12_CESD",	
# "HWB24_CESD",	"HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD"

# AUC - Timepoints w/out prenatal
t1 = c(6,12,24,36,48)
t1 = c(6,12,24,36,48, 60, 72)

# initialize new AUC variable
NEW$auc_post_cesd = rep(0,NROW(NEW))
NEW$auc_post_cesd2 = rep(0,NROW(NEW))

for (i in 1:NROW(NEW)){
  y = NEW[i,c("HWB6_CESD","HWB12_CESD","HWB24_CESD","HWB36_CESD","HWB48_CESD", "HWB60_CESD", "HWB72_CESD")]
  print(y)
  whichismissing = is.na(y)
  print(whichismissing)
  print(sum(whichismissing))
  print(!whichismissing)
  print(t1[!whichismissing])
  print(y[!whichismissing])
  print('----------')
  if(sum(whichismissing) < 4) NEW$auc_post_cesd2[i] = DescTools::AUC(t1[!whichismissing],y[!whichismissing])
  else NEW$auc_post_cesd2[i] = NA
}

mydata.cor = cor(NEW[, c("auc_post_cesd","auc_post_cesd2")],  method ="kendall",  use = "pairwise")

t2 = c(0, 6,12,24,36,48, 60, 72)

# initialize new AUC variable
NEW$auc_cesd = rep(0,NROW(NEW))

for (i in 1:NROW(NEW)){
  y = NEW[i,c("Pren_CESD","HWB6_CESD","HWB12_CESD","HWB24_CESD","HWB36_CESD","HWB48_CESD", "HWB60_CESD", "HWB72_CESD")]
  print(y)
  whichismissing = is.na(y)
  print(whichismissing)
  print(sum(whichismissing))
  print(!whichismissing)
  print(t1[!whichismissing])
  print(y[!whichismissing])
  print('----------')
  if(sum(whichismissing) < 5) NEW$auc_cesd[i] = DescTools::AUC(t2[!whichismissing],y[!whichismissing])
  else NEW$auc_cesd[i] = NA
}

t3 = c(0, 6,12,24,36,48)

# initialize new AUC variable
NEW$auc_cesd_48 = rep(0,NROW(NEW))

for (i in 1:NROW(NEW)){
  y = NEW[i,c("Pren_CESD","HWB6_CESD","HWB12_CESD","HWB24_CESD","HWB36_CESD","HWB48_CESD")]
  print(y)
  whichismissing = is.na(y)
  print(whichismissing)
  print(sum(whichismissing))
  print(!whichismissing)
  print(t1[!whichismissing])
  print(y[!whichismissing])
  print('----------')
  if(sum(whichismissing) < 3) NEW$auc_cesd_48[i] = DescTools::AUC(t3[!whichismissing],y[!whichismissing])
  else NEW$auc_cesd_48[i] = NA
}

t4 = c(6,12,24,36,48)

# initialize new AUC variable
NEW$auc_post_cesd_48 = rep(0,NROW(NEW))

for (i in 1:NROW(NEW)){
  y = NEW[i,c("HWB6_CESD","HWB12_CESD","HWB24_CESD","HWB36_CESD","HWB48_CESD")]
  print(y)
  whichismissing = is.na(y)
  print(whichismissing)
  print(sum(whichismissing))
  print(!whichismissing)
  print(t1[!whichismissing])
  print(y[!whichismissing])
  print('----------')
  if(sum(whichismissing) < 2) NEW$auc_post_cesd_48[i] = DescTools::AUC(t4[!whichismissing],y[!whichismissing])
  else NEW$auc_post_cesd_48[i] = NA
}

# initialize new AUC variable
NEW$auc_post_cesd_flex = rep(0,NROW(NEW))

for (i in 1:NROW(NEW)){
  y = NEW[i,c("HWB6_CESD","HWB12_CESD","HWB24_CESD","HWB36_CESD","HWB48_CESD", "HWB60_CESD", "HWB72_CESD")]
  print(y)
  whichismissing = is.na(y)
  print(whichismissing)
  print(sum(whichismissing))
  print(!whichismissing)
  print(t1[!whichismissing])
  print(y[!whichismissing])
  print('----------')
  if(sum(whichismissing) < 5) NEW$auc_post_cesd_flex[i] = DescTools::AUC(t1[!whichismissing],y[!whichismissing])
  else NEW$auc_post_cesd_flex[i] = NA
}

# plot(t1,NEW$auc_post_cesd_flex)

# initialize new AUC variable
NEW$auc_cesd_flex = rep(0,NROW(NEW))

for (i in 1:NROW(NEW)){
  y = NEW[i,c("Pren_CESD","HWB6_CESD","HWB12_CESD","HWB24_CESD","HWB36_CESD","HWB48_CESD", "HWB60_CESD", "HWB72_CESD")]
  print(y)
  whichismissing = is.na(y)
  print(whichismissing)
  print(sum(whichismissing))
  print(!whichismissing)
  print(t1[!whichismissing])
  print(y[!whichismissing])
  print('----------')
  if(sum(whichismissing) <6) NEW$auc_cesd_flex[i] = DescTools::AUC(t2[!whichismissing],y[!whichismissing])
  else NEW$auc_cesd_flex[i] = NA
}

# initialize new AUC variable
NEW$auc_cesd_48_flex = rep(0,NROW(NEW))

for (i in 1:NROW(NEW)){
  y = NEW[i,c("Pren_CESD","HWB6_CESD","HWB12_CESD","HWB24_CESD","HWB36_CESD","HWB48_CESD")]
  print(y)
  whichismissing = is.na(y)
  print(whichismissing)
  print(sum(whichismissing))
  print(!whichismissing)
  print(t1[!whichismissing])
  print(y[!whichismissing])
  print('----------')
  if(sum(whichismissing) < 4) NEW$auc_cesd_48_flex[i] = DescTools::AUC(t3[!whichismissing],y[!whichismissing])
  else NEW$auc_cesd_48_flex[i] = NA
}

# initialize new AUC variable
NEW$auc_post_cesd_48_flex = rep(0,NROW(NEW))

for (i in 1:NROW(NEW)){
  y = NEW[i,c("HWB6_CESD","HWB12_CESD","HWB24_CESD","HWB36_CESD","HWB48_CESD")]
  print(y)
  whichismissing = is.na(y)
  print(whichismissing)
  print(sum(whichismissing))
  print(!whichismissing)
  print(t1[!whichismissing])
  print(y[!whichismissing])
  print('----------')
  if(sum(whichismissing) < 3) NEW$auc_post_cesd_48_flex[i] = DescTools::AUC(t4[!whichismissing],y[!whichismissing])
  else NEW$auc_post_cesd_48_flex[i] = NA
}

mydata.cor = cor(NEW[, c("auc_post_cesd",	"auc_cesd",	"auc_cesd_48",	"auc_post_cesd_48","auc_post_cesd_flex", "auc_cesd_flex",	"auc_cesd_48_flex",	"auc_post_cesd_48_flex")],  method ="kendall",  use = "pairwise")
res1 <- cor.mtest(NEW[, c("auc_post_cesd",	"auc_cesd",	"auc_cesd_48",	"auc_post_cesd_48", "auc_post_cesd_flex",	"auc_cesd_flex",	"auc_cesd_48_flex",	"auc_post_cesd_48_flex")], conf.level = .95, method ="kendall",  alternative = "two.sided", exact=FALSE, use = "pairwise")


corrplot(mydata.cor, p.mat = res1$p, method = "color", type = "upper",
         sig.level = c(.001, .01), pch.cex = .8, tl.cex = .6,  tl.srt = 45,
         insig = "label_sig", pch.col = "black", tl.col = "black")

#will go with auc from 3 time points instead of 4 time points since adds observation and correlation appears high

write.csv(NEW, file = "MAVAN_48M_and_up_april2020.csv", row.names = FALSE)
