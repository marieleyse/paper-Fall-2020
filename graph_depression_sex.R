#Marie-Elyse Lafaille-Magnan, PhD
#marie-elyse.lafaille-magnan@mail.mcgill.ca

setwd("/Users/Marie-Elyse/Downloads")
NEW = read.csv("MAVAN_48M_and_up_jun2020.csv")

# NEW %>%
#   tab_cells(NEW$Pren_CESD, NEW$HWB6_CESD, NEW$HWB12_CESD, NEW$HWB24_CESD) %>%
#   #tab_cols(total(), NEW$gender_male, LONG$gender_male_fixed) %>% 
#   tab_cols(total(), NEW$gender_male) %>% 
#   tab_stat_mean_sd_n() %>%
#   tab_last_sig_means(subtable_marks = "both") %>% 
#   tab_pivot() %>% 
#   set_caption("Table with summary statistics and significance marks.")  

# png(filename = 'sex_differences_maternal_depression.png', width=1000, height=400)
pdf("sex_differences_maternal_depression.pdf", width=10, height=4, compress=FALSE)

par(mfrow=c(1,4))
myBox <- function(x, y, ylab, xlab) boxplot(x ~ y, col=c("#FF6600", "#0000CC"), names=c("Girls", "Boys"),ylab=ylab, xlab=xlab)
mapply(myBox,
       x = NEW[,c("Pren_CESD", "HWB6_CESD", "HWB12_CESD", "HWB24_CESD", "HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "auc_post_cesd")],
       y = list(NEW$gender_male), # we make this a list so it has length(1)
       ylab = c("Pren_CESD", "HWB6_CESD", "HWB12_CESD", "HWB24_CESD", "HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "auc_post_cesd"),
       xlab = "" # empty x-lab
)
dev.off()

# png(filename = 'sex_differences_maternal_depression_preschool.png', width=1000, height=400)
pdf("sex_differences_maternal_depression_preschool.pdf", width=10, height=4, compress=FALSE)
par(mfrow=c(1,5))
myBox <- function(x, y, ylab, xlab) boxplot(x ~ y, col=rainbow(2), names=c("Girls", "Boys"),ylab=ylab, xlab=xlab)
mapply(myBox,
       x = NEW[,c("HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "auc_post_cesd")],
       y = list(NEW$gender_male), # we make this a list so it has length(1)
       ylab = c("HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "auc_post_cesd"),
       xlab = "" # empty x-lab
)
dev.off()

# png(filename = 'sex_differences_all_maternal_depression.png', width=1000, height=400)
pdf("sex_differences_all_maternal_depression.pdf", width=14, height=4, compress=FALSE)
par(mfrow=c(1,9))
myBox <- function(x, y, ylab, xlab) boxplot(x ~ y, col=c("#FF6600", "#0000CC"), names=c("Girls", "Boys"),ylab=ylab, xlab=xlab)
mapply(myBox,
       x = NEW[,c("Pren_CESD", "HWB6_CESD", "HWB12_CESD", "HWB24_CESD", "HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "auc_post_cesd")],
       y = list(NEW$gender_male), # we make this a list so it has length(1)
       ylab = c("Prenatal CESD", "CESD 6m", "CESD 12m", "CESD 24m", "CESD 36m",	"CESD 48m",	"CESD 60m",	"CESD 72m", "Total postnatal depression (AUC)"),
       xlab = "" # empty x-lab
)
dev.off()

NEW = read.csv("MAVAN_48M_and_up_jun2020.csv")

median(NEW$ADHD)
#DEFAULT IS MEDIAN

NEW$ADHD_d = 0
NEW$ADHD_d <- dicho(
    NEW$ADHD,
    dich.by(),
    as.num = TRUE,
    var.label = "ADHD (dichotomized)",
    val.labels = c("low", "high"),
    append = TRUE,
    suffix = "_d"
)
head(NEW$ADHD_d)

NEW$ADHD_d<- sjmisc::dicho(
                  NEW$ADHD,
                  dich.by = "median",
                  as.num = TRUE,
                  var.label = NULL,
                  val.labels = NULL,
                  append = TRUE,
                  suffix = "_d"
)

# png(filename = 'low_and_high_differences_all_maternal_depression.png', width=1000, height=400)
pdf("low_and_high_differences_all_maternal_depression.pdf", width=14, height=4, compress=FALSE)
par(mfrow=c(1,9))
myBox <- function(x, y, ylab, xlab) boxplot(x ~ y, col=c("#FF6600", "#0000CC"), names=c("Low", "High"),ylab=ylab, xlab=xlab)
mapply(myBox,
       x = NEW[,c("Pren_CESD", "HWB6_CESD", "HWB12_CESD", "HWB24_CESD", "HWB36_CESD",	"HWB48_CESD",	"HWB60_CESD",	"HWB72_CESD", "auc_post_cesd")],
       y = list(NEW$ADHD_d), # we make this a list so it has length(1)
       ylab = c("Prenatal CESD", "CESD 6m", "CESD 12m", "CESD 24m", "CESD 36m",	"CESD 48m",	"CESD 60m",	"CESD 72m", "Total postnatal experience of depression (AUC)"),
       xlab = "ADHD" # empty x-lab
)
dev.off()

sink()

png(filename = 'DEPRESSION.png', width=1000, height=400)
ggplot(NEW, aes(Pren_CESD,auc_post_cesd)) +
  geom_jitter(
    position = position_jitter(0.2)
  ) + 
  geom_smooth(
    method = "lm"
  )
dev.off()

# png(filename = 'DEPRESSION_by_sex.png', width=1000, height=400)
pdf("DEPRESSION_by_sex.pdf", width=10, height=4, compress=FALSE)

ggplot(NEW, aes(Pren_CESD,auc_post_cesd)) +
  geom_jitter(
    aes(color = gender_male),
    position = position_jitter(0.2)
  ) + 
  geom_smooth(
    method = "lm",
    aes(color=gender_male)
    )
dev.off()

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

# png(filename = 'DEPRESSION_by_ADHD.png', width=1000, height=400)
pdf("DEPRESSION_by_ADHD.pdf", width=10, height=4, compress=FALSE)
ggplot(NEW, aes(Pren_CESD,auc_post_cesd)) +
  geom_jitter(
    aes(color = ADHD_d),
    position = position_jitter(0.2)
  ) + 
  geom_smooth(
    method = "lm",
    aes(color=ADHD_d)
  )
dev.off()
