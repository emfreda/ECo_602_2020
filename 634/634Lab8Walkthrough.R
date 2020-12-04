require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

#alternative hypothesis: Adelie flippers shorter than Chinstrap
t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")

require(boot)
install.packages("simpleboot")
require(simpleboot)

#----TWO_SAMPLE_BOOTSTRAP----
#hist of bootstrapped mean diff flipper length

pen_boot <- two.boot(subset(penguin_dat, species == "Adelie")$flipper_length_mm,
         subset(penguin_dat, species == "Chinstrap")$flipper_length_mm, 
         FUN = mean, R = 10, na.rm = T)
hist(pen_boot$t)
str(pen_boot)
#do it again on diff data
require(here)
veg <- read.csv(here("data/vegdata.csv"))
boxplot(pine ~ treatment, dat = veg)
#new df w clipped and control treatments only
dat_tree <- droplevels(subset(veg, treatment %in% c("control", "clipped")))
boxplot(pine ~ treatment, dat = dat_tree)
#use table to find # obs per treatment type
#arguments are hard, may be wrong but gets right answer
table(dat_tree$treatment)
#what is p value for OG data?
t.test(pine ~ treatment, data = dat_tree)

tree_boot = two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean, R = 10000, na.rm = TRUE)
#sum(tree_boot$t >= 0)
#sum(tree_boot$t < 0)
boot.ci(tree_boot)
quantile(tree_boot$t, 0.025)

#----RESAMPLING:LINEAR_REGRESSION----
dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))

dat_all = merge(dat_bird, dat_habitat,
  by = c("basin", "sub"))
#b.sidi = diversity index for breeding birds
#s.sidi = diversity index for vegetation cover types

#simple linear regression
fit_1 <- lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
#save slope coefficient (s.sidi,aka predictor variable) for later
slope_observed <- coef(fit_1)[2]
#add regression line to a plot
plot(b.sidi ~ s.sidi, data = dat_all,
     main = "Simpson's diversity indices",
     xlab = "Vegetation cover diversity",
     ylab = "Bird diversity")
abline(fit_1)

#testing the slope coefficient to see if relationship is by chance
#simplify data first
dat_1 <- subset(dat_all,select = c(b.sidi, s.sidi))

#create a resampled dataset by randomly generating row indicies
#then use index vectors to create resampled s.sidi/b.sidi vectors
index_1 <- sample(nrow(dat_1), replace = TRUE)
index_2 <- sample(nrow(dat_1), replace = TRUE)

dat_resampled_i <- data.frame(
    b.sidi <- dat_1$b.sidi[index_1],
    s.sidi <- dat_1$s.sidi[index_2])

fit_resampled_i <- lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i <- coef(fit_resampled_i)[2]

print(slope_resampled_i)
#plot resampled linear fit over resampled vectors
plot(b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

#use a loop to repeat this process 
#(est sampling dist of null hypothesis)

m = 10000 
result = numeric(m) 

for(i in 1:m) {
  index_1 <- sample(nrow(dat_1), replace = TRUE)
  index_2 <- sample(nrow(dat_1), replace = TRUE)

  dat_resampled_i <- data.frame(
    b.sidi <- dat_1$b.sidi[index_1],
    s.sidi <- dat_1$s.sidi[index_2])

  fit_resampled_i <- lm(b.sidi ~ s.sidi, data = dat_resampled_i)  
  result[i] = coef(fit_resampled_i)[2]
} 
#plot hist of slope values w a line for the observed data fit line
hist(result, main = "Null Distribution of Regression Slope",
     xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
#find 5th percentile of null dist slopes
#compare to observed slope
quantile(result, c(.05))
