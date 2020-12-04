#----QUESTION_1----
png("UMQ1.png", height = 800, width = 700, res = 180)
hist(catrate$cat.rate, 
     main = "Salamander Catastrophic\nReproduction Rates",
     xlab = "Catastrophic rate")
dev.off()

#----QUESTION_2----
shapiro.test(catrate$cat.rate)
#----QUESTION_3----
# Q1: null hyp = data is sampled from norm dist
# Q2: p-value = 0.04097 --> reject null, but barely


#----QUESTION_4----
t.test(catrate$cat.rate, mu = obs_late_fill)
# Q1: null hyp = obs pop mean is not different than a value 
# that we can specify but has a default of 0
#----QUESTION_5----
# Q1: p-value = 0.01193 
# Q2: CI = 0.3526250 0.7261295; does not include 0
# Q3: p < 0.05 --> reject null


#----QUESTION_6----
wilcox.test(catrate$cat.rate, mu = 2 / 7)
#----QUESTION_7----
# Q1: p-value = 0.006275
# Q2: p < 0.05 --> reject null


#----QUESTION_8----
# Q1: Wilcoxon's test is better bc shapiro.test revealed that 
#     data is not normally distributed and Wilcox is for non-norm
# Q2: alt hyp is obs mean =! expected mean for both but Wilcox
#     has a much smaller p value -> stronger rejection of null    


#----QUESTION_9----
dat_adelie <- subset(penguin_dat, species == "Adelie")
dat_chin <- subset(penguin_dat, species == "Chinstrap")

shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chin$flipper_length_mm)
# Q1: Adelie p-value = 0.72 --> data is norm dist
#     Chinstrap p-value = 0.8106 --> data is norm dist


#----QUESTION_10----
png("UMQ10.png", height = 800, width = 800, res = 180)

par(mfrow = c(1,2), mar = c(4.5, 4, 3, 1.5))
hist(dat_adelie$flipper_length_mm,
     main = "Adelie Penguin\nHistogram",
     xlab = "Flipper length (mm)",
     ylab = "Frequency")
hist(dat_chin$flipper_length_mm,
     main = "Chinstrap Penguin\nHistogram",
     xlab = "Flipper length (mm)",
     ylab = "Frequency")

dev.off()


#----QUESTION_11----
t.test(penguin_dat$flipper_length_mm ~ penguin_dat$species)

# Q1: alt hyp: the two species have different mean flipper lengths


