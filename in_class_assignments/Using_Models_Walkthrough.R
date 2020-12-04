require(here)
catrate <- read.csv(here("data/catrate.csv"))
summary(catrate)

hist(catrate$cat.rate)


#----ONE_SAMPLE_TESTS----
#alternate hypothesis:
# is obs mean cast rate statist. diff from expected late filling rate?
#   aka is cast rate solely a result of late filling?
obs_mean_cr <- mean(catrate$cat.rate)
obs_late_fill <- 2/7

#check for normality with shapiro.test
#null hyp for s.t = data is sampled from norm dist
#small p val(< 0.05) = reject null
shapiro.test(catrate$cat.rate)
# p-value = 0.04097 --> reject null (not norm dist)

#null hyp: pop mean is not different than [value]
#2 side alt hyp: obs mean =! expected mean
t.test(catrate$cat.rate, mu = obs_late_fill)
#p-value = 0.01193 --> reject null, obs =! expected
#interpretation: cat rate is not solely a result of late pond filling

#1 side alt hyp: obs mean > expected mean
t.test(catrate$cat.rate, mu = obs_late_fill, alternative = "greater")
#p-value = 0.005966 --> reject null
#   stronger evidence than 2 sided t.test bc p is <<


#Wilcoxon's signed rank test for non-norm dist sample
#2 tailed
wilcox.test(catrate$cat.rate, mu = 2 / 7)
#p-value = 0.006275

#1 tailed
wilcox.test(catrate$cat.rate, mu = 2 / 7, alternative = "greater")
#p-value = 0.003137


#----TWO_SAMPLE_TESTS----
#null hyp: no diff in mean btwn 2 groups
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, 
                                species != "Gentoo"))
boxplot(flipper_length_mm ~ species, data = penguin_dat)

#subset species to test normality (shapiro.test) for each
dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chin = subset(penguin_dat, species == "Chinstrap")
#test normality 
shapiro.test(dat_adelie$flipper_length_mm)
#p-value = 0.72 --> data is norm dist
shapiro.test(dat_chin$flipper_length_mm)
#p-value = 0.8106 --> data is norm dist

t.test(penguin_dat$flipper_length_mm ~ penguin_dat$species)
#p-value = 6.049e-08

#for 1 tailed hyp in two-sample tests:find which group is "base level"
levels(penguin_dat$species) #first listed is base (Adelie)

t.test(penguin_dat$flipper_length_mm ~ penguin_dat$species,
       alternative = "less")
#p-value = 3.025e-08 (smaller than 2 tailed)


#Wilcoxon's signed rank test (for non-norm dist)
wilcox.test(penguin_dat$flipper_length_mm ~ penguin_dat$species)
#p-value = 3.028e-08 (similar to 1 tailed t.test)
wilcox.test(penguin_dat$flipper_length_mm ~ penguin_dat$species,
            alternative = "less")
#p-value = 1.514e-08

