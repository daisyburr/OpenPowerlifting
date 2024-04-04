#SET UP####

#libs
library(tidyverse)
library(jtools)
library(ggpubr)
library(lme4)
library(lmerTest)
library(car)
library(interactions)


setwd("~/Dropbox/openpowerlifting-2024-03-30")

d <- read_csv('openpowerlifting-2024-03-30-bc1cac9c.csv')

#*str####
#factor
d$Name <- as.factor(d$Name)
d$Sex <- as.factor(d$Sex)
d$Event <- as.factor(d$Event)
d$Equipment <- as.factor(d$Equipment)
d$AgeClass <- as.factor(d$AgeClass)
d$WeightClassKg <- as.factor(d$WeightClassKg)
d$Division <- as.factor(d$Division)
d$Tested <- as.factor(d$Tested)
d$MeetState <- as.factor(d$MeetState)
d$MeetCountry <- as.factor(d$MeetCountry)
d$Federation <- as.factor(d$Federation)
d$ParentFederation <- as.factor(d$ParentFederation)

#numeric
d$BodyweightKg <- as.numeric(d$BodyweightKg)
d$Age <- as.numeric(d$Age)
d$Squat1Kg <- as.numeric(d$Squat1Kg)
d$Squat2Kg <- as.numeric(d$Squat2Kg)
d$Squat3Kg <- as.numeric(d$Squat3Kg)
d$Bench1Kg <- as.numeric(d$Bench1Kg)
d$Bench2Kg <- as.numeric(d$Bench2Kg)
d$Bench3Kg <- as.numeric(d$Bench3Kg)
d$Deadlift1Kg <- as.numeric(d$Deadlift1Kg)
d$Deadlift2Kg <- as.numeric(d$Deadlift2Kg)
d$Deadlift3Kg <- as.numeric(d$Deadlift3Kg)
d$TotalKg <- as.numeric(d$TotalKg)
d$Dots <- as.numeric(d$Dots)
d$Best3SquatKg <- as.numeric(d$Best3SquatKg)
d$Best3BenchKg <- as.numeric(d$Best3BenchKg)
d$Best3DeadliftKg <- as.numeric(d$Best3DeadliftKg)

#date
d$Date <- format(as.POSIXct(d$Date, format='Y-M-D'))
range(d$Date) #"1964-09-05" "2024-03-24"

#*trans####
#full power only, raw
d2 <- d[d$Event=="SBD" & d$Equipment=="Raw",]

#have full age data
d3 <- d2[!is.na(d2$Age),]

#have full weight data
d4 <- d3[!is.na(d3$BodyweightKg),]

#have full sex data
d5 <- d4[d4$Sex=="F" | d4$Sex=="M",]
d5$Sex <- fct_drop(d5$Sex)

#change Tested NA to "No
d5$Tested <- as.factor(ifelse(d5$Tested=="Yes", "Yes", "No"))
d5$Tested <- fct_na_value_to_level(d5$Tested, "No")

#bombed out = pos number in all 3 
d6 <- d5[!is.na(d5$Best3SquatKg) & !is.na(d5$Best3BenchKg) & !is.na(d5$Best3DeadliftKg),]

#fix age groups
levels(d6$AgeClass)
d7 <- d6[d6$AgeClass!="5-12" & d6$AgeClass!="80-999",]
d7$AgeClass <- fct_drop(d7$AgeClass)
d7$AgeClass <- fct_collapse(d7$AgeClass,
                            '<18' = c('13-15','16-17'),
                            '18-23' = c("18-19",'20-23'))
d7$AgeClass <- fct_drop(d7$AgeClass)

#fix weight groups
d7$WeightClassKg_new <- cut(d7$BodyweightKg,
                                 breaks=c(-Inf, 44, 48, 52, 56, 60, 67.5, 75, 82.5, 90, 100, Inf), 
                                 labels=c("44kg","48kg","52kg", "52kg", "60kg", "67.5kg", "75kg", "82.5kg", "90kg", "100kg", "100+kg"))


#bad lift is negative 
#didn't bomb out
d8 <- d7[d7$Best3SquatKg>0 & d7$Best3BenchKg>0 & d7$Best3DeadliftKg>0,]

#prop
d8$propS <- d8$Best3SquatKg/d8$TotalKg
d8$propB <- d8$Best3BenchKg/d8$TotalKg
d8$propDl <- d8$Best3DeadliftKg/d8$TotalKg

#failed lifts
#number of failed lifts per lifter within meet 
#Recode negatives as NA
d8$Squat1Kg <- ifelse(d8$Squat1Kg <0, NA, d8$Squat1Kg)
d8$Squat2Kg <- ifelse(d8$Squat2Kg <0, NA, d8$Squat2Kg)
d8$Squat3Kg <- ifelse(d8$Squat3Kg <0, NA, d8$Squat3Kg)

d8$Bench1Kg <- ifelse(d8$Bench1Kg <0, NA, d8$Bench1Kg)
d8$Bench2Kg <- ifelse(d8$Bench2Kg <0, NA, d8$Bench2Kg)
d8$Bench3Kg <- ifelse(d8$Bench3Kg <0, NA, d8$Bench3Kg)

d8$Deadlift1Kg <- ifelse(d8$Deadlift1Kg <0, NA, d8$Deadlift1Kg)
d8$Deadlift2Kg <- ifelse(d8$Deadlift2Kg <0, NA, d8$Deadlift2Kg)
d8$Deadlift3Kg <- ifelse(d8$Deadlift3Kg <0, NA, d8$Deadlift3Kg)

#number NAs lift
d9 <-  d8 %>%
  rowwise() %>%
  mutate(nFailedS= sum(is.na(c_across(all_of(c("Squat1Kg", "Squat2Kg", "Squat3Kg")))))) %>%
  mutate(nFailedB= sum(is.na(c_across(all_of(c("Bench1Kg", "Bench2Kg", "Bench3Kg")))))) %>%
  mutate(nFailedDl= sum(is.na(c_across(all_of(c("Deadlift1Kg", "Deadlift2Kg", "Deadlift3Kg")))))) %>%
  mutate(nFailTotal = sum(nFailedS,nFailedB, nFailedDl))


#DESC####
#age
ggplot(d9, aes(x=Sex, y = TotalKg, fill = Tested)) +
  geom_boxplot() + facet_grid(.~AgeClass) + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Best Totals by Sex and Age") + theme(plot.title = element_text(hjust = 0.5))
ggsave('sex_total_age_tested.png')

ggplot(d9, aes(x=Sex, y = Best3SquatKg, fill = Tested)) +
  geom_boxplot() + facet_grid(.~AgeClass) + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "none") +
  ggtitle("Best Squat by Sex and Age") + theme(plot.title = element_text(hjust = 0.5))
ggsave('sex_s_age_tested.png')

ggplot(d9, aes(x=Sex, y = Best3BenchKg, fill = Tested)) +
  geom_boxplot() + facet_grid(.~AgeClass) + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "none")  +
  ggtitle("Best Bench by Sex and Age") + theme(plot.title = element_text(hjust = 0.5))
ggsave('sex_b_age_tested.png')

ggplot(d9, aes(x=Sex, y = Best3DeadliftKg, fill = Tested)) +
  geom_boxplot() + facet_grid(.~AgeClass) + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Best Deadlift by Sex and Age") + theme(plot.title = element_text(hjust = 0.5)) 
ggsave('sex_dl_age_tested.png')



#weight class
ggplot(d9, aes(x=Sex, y = TotalKg, fill = Tested)) +
  geom_boxplot() + facet_grid(.~WeightClassKg_new) + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Best Totals by Sex and Weight Class") + theme(plot.title = element_text(hjust = 0.5))
ggsave('sex_total_WeightC_tested.png')

ggplot(d9, aes(x=Sex, y = Best3SquatKg, fill = Tested)) +
  geom_boxplot() + facet_grid(.~WeightClassKg_new) + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "none") +
  ggtitle("Best Squat by Sex and Weight Class") + theme(plot.title = element_text(hjust = 0.5))
ggsave('sex_s_WeightC_tested.png')

ggplot(d9, aes(x=Sex, y = Best3BenchKg, fill = Tested)) +
  geom_boxplot() + facet_grid(.~WeightClassKg_new) + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "none")  +
  ggtitle("Best Bench by Sex and Weight Class") + theme(plot.title = element_text(hjust = 0.5))
ggsave('sex_b_WeightC_tested.png')

ggplot(d9, aes(x=Sex, y = Best3DeadliftKg, fill = Tested)) +
  geom_boxplot() + facet_grid(.~WeightClassKg_new) + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Best Deadlift by Sex and Weight Class") + theme(plot.title = element_text(hjust = 0.5)) 
ggsave('sex_dl_WeightC_tested.png')



#weight
ggplot(d9, aes(x=Sex, y = BodyweightKg, colour = Sex)) +
  geom_boxplot() + theme_classic() + facet_grid(.~AgeClass) +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Body Weight by Sex and Age") + theme(plot.title = element_text(hjust = 0.5))
ggsave('sex_bw_age_tested.png')


#weight class percentiles
ggplot(d9, aes(x = BodyweightKg, group = Sex, colour = Sex)) +
  geom_boxplot() + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Body Weight by Sex and Age") + theme(plot.title = element_text(hjust = 0.5))

#weight class percentiles
ggplot(d9, aes(x = BodyweightKg, group = Sex, colour = Sex)) +
  geom_boxplot() + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Body Weight by Sex") + theme(plot.title = element_text(hjust = 0.5))


#weight class percentiles
ggplot(d9, aes(x = BodyweightKg, group = Sex, colour = Sex)) +
  geom_boxplot() + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Body Weight by Sex") + theme(plot.title = element_text(hjust = 0.5))

#prop
ggplot(d9, aes(x=Sex, y = propS, fill = Tested)) +
  geom_boxplot() + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Proportion of total accounted for by squat") + theme(plot.title = element_text(hjust = 0.5))
ggsave('PropS.png')

ggplot(d9, aes(x=Sex, y = propB, fill = Tested)) +
  geom_boxplot() + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "none") +
  ggtitle("Proportion of total accounted for by bench") + theme(plot.title = element_text(hjust = 0.5))
ggsave('PropB.png')

ggplot(d9, aes(x=Sex, y = propDl, fill = Tested)) +
  geom_boxplot() + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "none")  +
  ggtitle("Proportion of total accounted for by deadlift") + theme(plot.title = element_text(hjust = 0.5))
ggsave('propDL.png')


#Fed
d9 %>%
  filter(Federation=="USPA" | Federation == "USAPL" | 
           Federation == "WRPF") %>%
  ggplot(aes(x=Federation, y = TotalKg, colour = Tested)) +
  geom_boxplot() + theme_classic() + 
  facet_grid(.~Sex) +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom")  +
  ggtitle("Totals by Sex and Federation") + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('totals_sex_fed_tested.png')

#Country
d9 %>%
  ggplot(aes(x=Country, y = TotalKg)) +
  geom_boxplot() + theme_classic() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size =8)) + 
  ggtitle("Totals by Country") + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('totals_country.png', width = 24, height = 12)


#failed lifts
d9 %>%
  filter(Federation=="USPA" | Federation == "USAPL" | 
           Federation == "WRPF") %>%
  ggplot(aes(x=Sex, y = nFailTotal, fill = Tested)) +
  geom_boxplot() + facet_grid(.~Federation) + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Failed Lifts by Sex and Federation") + theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,6)
ggsave('Sex_Fed_failedL.png')

d9 %>%
  filter(Federation=="USPA" | Federation == "USAPL" | 
           Federation == "WRPF") %>%
  ggplot(aes(x=Sex, y = nFailedS, fill = Tested)) +
  geom_boxplot() + facet_grid(.~Federation) + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Failed Squats by Sex and Federation") + theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,3)
ggsave('Sex_Fed_failedS.png')

d9 %>%
  filter(Federation=="USPA" | Federation == "USAPL" | 
           Federation == "WRPF") %>%
  ggplot(aes(x=Sex, y = nFailedB, fill = Tested)) +
  geom_boxplot() + facet_grid(.~Federation) + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Failed Bench by Sex and Federation") + theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,3)
ggsave('Sex_Fed_failedB.png')

d9 %>%
  filter(Federation=="USPA" | Federation == "USAPL" | 
           Federation == "WRPF") %>%
  ggplot(aes(x=Sex, y = nFailedDl, fill = Tested)) +
  geom_boxplot() + facet_grid(.~Federation) + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Failed Deadlifts by Sex and Federation") + theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,3)
ggsave('Sex_Fed_failedDl.png')


d9 %>%
  filter(Federation=="USPA" | Federation == "USAPL" | 
           Federation == "WRPF") %>%
  ggplot(aes(x=Sex, y = nFailTotal, fill = Tested)) +
  geom_boxplot() + facet_grid(.~WeightClassKg_new) + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Failed Lifts by Sex and Federation") + theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,6)
ggsave('Sex_Fed_failed_weight.png')

d9 %>%
  filter(Federation=="USPA" | Federation == "USAPL" | 
           Federation == "WRPF") %>%
  ggplot(aes(x=Sex, y = nFailedS, fill = Tested)) +
  geom_boxplot() + facet_grid(.~WeightClassKg_new) + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Failed Squats by Sex and Federation") + theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,3)
ggsave('Sex_Fed_failedS_weight.png')

d9 %>%
  filter(Federation=="USPA" | Federation == "USAPL" | 
           Federation == "WRPF") %>%
  ggplot(aes(x=Sex, y = nFailedB, fill = Tested)) +
  geom_boxplot() + facet_grid(.~WeightClassKg_new) + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Failed Bench by Sex and Federation") + theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,3)
ggsave('Sex_Fed_failedB_weight.png')

d9 %>%
  filter(Federation=="USPA" | Federation == "USAPL" | 
           Federation == "WRPF") %>%
  ggplot(aes(x=Sex, y = nFailedDl, fill = Tested)) +
  geom_boxplot() + facet_grid(.~WeightClassKg_new) + theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Failed Deadlifts by Sex and Federation") + theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,3)
ggsave('Sex_Fed_failedDl_weight.png')

#dots total
ggplot(d9, aes(x=Dots, y = TotalKg, group = Sex, colour = Sex)) +
  geom_point() + theme_classic() + facet_grid(.~WeightClassKg_new) +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Dots x Total") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0))
ggsave('dotsTotalSex_WC.png', width = 8)

ggplot(d9, aes(x=Dots, y = TotalKg, group = Sex, colour = Sex)) +
  geom_point() + theme_classic() + facet_grid(.~AgeClass) +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Dots x Total") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0))
ggsave('dotsTotalSex_AC.png', width = 8)


#weight classes by weight
ggplot(d9, aes(BodyweightKg, fill = Sex)) +
  geom_histogram()  +
  geom_vline(aes(xintercept=44)) + 
  geom_vline(aes(xintercept=48)) +
  geom_vline(aes(xintercept=52)) +
  geom_vline(aes(xintercept=60)) +
  geom_vline(aes(xintercept=67.5)) +
  geom_vline(aes(xintercept=75)) +
  geom_vline(aes(xintercept=82.5)) +
  geom_vline(aes(xintercept=90)) +
  geom_vline(aes(xintercept=100)) +
  theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Body Weight Distribution versus Weight Classes") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0)) +
  scale_x_continuous(breaks = seq(0, 225, 10))
ggsave('weightVweightC.png')

#weight classes by weight
ggplot(d9, aes(Age, fill = Sex)) +
  geom_histogram()  +
  geom_vline(aes(xintercept=18)) + 
  geom_vline(aes(xintercept=23)) +
  geom_vline(aes(xintercept=34)) +
  geom_vline(aes(xintercept=40)) +
  geom_vline(aes(xintercept=45)) +
  geom_vline(aes(xintercept=50)) +
  geom_vline(aes(xintercept=55)) +
  geom_vline(aes(xintercept=60)) +
  geom_vline(aes(xintercept=65)) +
  geom_vline(aes(xintercept=70)) +
  geom_vline(aes(xintercept=75)) +
  theme_classic() +
  theme(strip.text.x = element_text(size = 8, angle = 45), legend.position = "bottom") +
  ggtitle("Age Distribution versus Age Classes") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0)) +
  scale_x_continuous(breaks = seq(0, 80, 5))
ggsave('AgeVAgeC.png')


#total
m1 <- lm(TotalKg ~ Federation + Tested + Sex + Age + BodyweightKg, data = d9)
#props
m2 <- lmer(propS ~ Federation + Tested + Sex + Age + BodyweightKg + (1| Name) + (1 |MeetName), data = d9)

m3 <- lmer(propB ~ Federation + Tested + Sex + Age + BodyweightKg + (1| Name) + (1 |MeetName), data = d9)

m4 <- lmer(propDL ~ Federation + Tested + Sex + Age + BodyweightKg + (1| Name) + (1 |MeetName), data = d9)

#failed
d9_fed <- d9[d9$Federation=="USPA" | d9$Federation=="USAPL" | d9$Federation=="WRPF",]
d9_fed$Federation <- fct_drop(d9_fed$Federation)

m5 <- lm(nFailTotal ~ Federation:Tested + Federation:Sex + WeightClassKg_new + AgeClass, data = d9_fed)
summ(m5, scale = T)
Anova(m5)

cat_plot(m5, pred = Federation, modx = Tested) + ggtitle("nFailTotal ~ Federation:Tested") +
  theme(plot.title = element_text(face = "plain", hjust = 0.5))
ggsave('nFail_fed.png')

cat_plot(m5, pred = Federation, modx = Sex) + ggtitle("nFailTotal ~ Federation:Sex") +
  theme(plot.title = element_text(face = "plain", hjust = 0.5))
ggsave('nFail_fed_sex.png')

effect_plot(m5, pred = WeightClassKg_new) + ggtitle("nFailTotal ~ Weight Class") +
  theme(plot.title = element_text(face = "plain", hjust = 0.5))
ggsave('nFail_WC.png')

effect_plot(m5, pred = AgeClass) + ggtitle("nFailTotal ~ Age Class") +
  theme(plot.title = element_text(face = "plain", hjust = 0.5))
ggsave('nFail_Age.png')




m6 <- lm(nFailedS ~ Federation:Tested + Federation:Sex + WeightClassKg_new + AgeClass, data = d9_fed)
summ(m6, scale = T)
Anova(m6)

cat_plot(m6, pred = Federation, modx = Tested) + ggtitle("nFailSquat ~ Federation:Tested") +
  theme(plot.title = element_text(face = "plain", hjust = 0.5))
ggsave('nFailS_fed.png')

cat_plot(m6, pred = Federation, modx = Sex) + ggtitle("nFailSquat ~ Federation:Sex") +
  theme(plot.title = element_text(face = "plain", hjust = 0.5))
ggsave('nFailS_fed_sex.png')





m7 <- lm(nFailedB ~ Federation:Tested + Federation:Sex + WeightClassKg_new + AgeClass, data = d9_fed)
summ(m7, scale = T)
Anova(m7)

cat_plot(m7, pred = Federation, modx = Tested) + ggtitle("nFailBench ~ Federation:Tested") +
  theme(plot.title = element_text(face = "plain", hjust = 0.5))
ggsave('nFailB_fed.png')

cat_plot(m7, pred = Federation, modx = Sex) + ggtitle("nFailBench ~ Federation:Sex") +
  theme(plot.title = element_text(face = "plain", hjust = 0.5))
ggsave('nFailB_fed_sex.png')



m8 <- lm(nFailedDl ~ Federation:Tested + Federation:Sex + WeightClassKg_new + AgeClass, data = d9_fed)
summ(m8, scale = T)
Anova(m8)

cat_plot(m8, pred = Federation, modx = Tested) + ggtitle("nFailDeadlift ~ Federation:Tested") +
  theme(plot.title = element_text(face = "plain", hjust = 0.5))
ggsave('nFailDl_fed.png')

cat_plot(m8, pred = Federation, modx = Sex) + ggtitle("nFailDeadlift ~ Federation:Sex") +
  theme(plot.title = element_text(face = "plain", hjust = 0.5))
ggsave('nFailDl_fed_sex.png')



