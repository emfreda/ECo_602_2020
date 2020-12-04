require(palmerpenguins)
require(boot)
require(simpleboot)
require(here)
penguin_dat <- droplevels(subset(penguins, species != "Gentoo"))

veg <- read.csv(here("data/vegdata.csv"))
dat_tree <- droplevels(subset(veg, treatment %in% c("control", "clipped")))


dat_bird <- read.csv(here("data", "bird.sub.csv"))
dat_habitat <- read.csv(here("data", "hab.sub.csv"))
dat_all <- merge(dat_bird, dat_habitat,
                 by = c("basin", "sub"))
simp_dat <- subset(dat_all,select = c(b.sidi, s.sidi))


#----QUESTION_1----
pen_boot <- two.boot(subset(penguin_dat, species == "Adelie")$flipper_length_mm,
                     subset(penguin_dat, species == "Chinstrap")$flipper_length_mm, 
                     FUN = mean, R = 10000, na.rm = T)
png("L8Q1.png", height = 800, width = 700, res = 180)
hist(pen_boot$t, main = "two.boot Histogram for \nChinstrap and Adelie")
dev.off()


#----QUESTION_2----
quantile(pen_boot$t, c(0.025, 0.975))
#  2.5%      97.5% 
# -7.869490 -3.907289 


#----QUESTION_3----
pen_ecdf <- ecdf(pen_boot$t)


#----QUESTION_4----
#The null hypothesis is that there is no difference
#in mean flipper length between Adelie and Chinstrap


#----QUESTION_5----
pen_ecdf(-4.5)
# 0.9085
1 - pen_ecdf(0)


#----QUESTION_6----
wilcox.test(pine ~ treatment, 
       data = dat_tree, alternative = "greater")
#p-value = 0.05026


#----QUESTION_7----
tree_boot <- two.boot(
  subset(dat_tree, treatment == "clipped")$pine,
  subset(dat_tree, treatment == "control")$pine,
  FUN = mean, R = 10000, na.rm = TRUE)
# boxplot(dat_tree$pine ~ dat_tree$treatment)

quantile(tree_boot$t, c(0.025, 0.975))
# 2.5%  97.5% 
# 3.875 29.875

agg_means <- aggregate(pine ~ treatment, 
                       data = dat_tree, FUN = mean, 
                       na.rm = TRUE)
diff_observed <- diff(agg_means[, 2])
agg_means
diff_observed  # -16

#Q1: CI endpoints = 3.875, 29.875
#Q2: Observed difference: -16; falls outside 95% CI


#----QUESTION_8----
m = 10000 
result = numeric(m) 

for(i in 1:m) {
  index_1 <- sample(nrow(simp_dat), replace = TRUE)
  index_2 <- sample(nrow(simp_dat), replace = TRUE)
  
  dat_resampled_i <- data.frame(
    b.sidi <- simp_dat$b.sidi[index_1],
    s.sidi <- simp_dat$s.sidi[index_2])
  
  fit_resampled_i <- lm(b.sidi ~ s.sidi, data = dat_resampled_i)  
  result[i] = coef(fit_resampled_i)[2]
} 


#----QUESTION_9----
crit_value <- quantile(result, c(.05)) #critical value; -0.01306674 

fit_1 <- lm(b.sidi ~ s.sidi, data = simp_dat)
slope_observed <- coef(fit_1)[2]
slope_observed

png("L8Q9.png", height = 800, width = 700, res = 180)
hist(result, main = "1000 MC Resampled\nRegression Slopes",
     xlab = "Slope Parameter",
     xlim = c(-0.033,0.032))
abline(v = slope_observed, lty = 1, col = "blue", lwd = 2)
abline(v = crit_value, lty = 2, col = "red", lwd = 2)
dev.off()


#----QUESTION_10----
# crit_value <- quantile(result, c(.05)) 
# Q1: critical value = -0.01306674
#     slope_observed = -0.02437131, less than crit

# Q2: since the observed slope is less than the
#     critical value the null hypothesis is rejected.
#     This means there is a stat sig neg relationship
#     btwn veg cover & bird diversity



