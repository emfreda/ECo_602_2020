require(palmerpenguins)
penguins <- palmerpenguins::penguins

#----Question_1----

sse_mean <- function(x)
{
  sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))
}
#should = 0.1067846
sse_mean(penguins$bill_depth_mm)

#----Question_2----

#remove Gentoo species from data for now
dat_pen <- droplevels(subset(penguins, species != "Gentoo"))
#table of the count of each species in dat_pen
table(dat_pen$species)
#use aggregate() to find flipper length mean by species
agg_means <- aggregate(flipper_length_mm ~ species, 
                      data = dat_pen, FUN = mean, 
                      na.rm = TRUE)
#use diff() to find difference of the values in agg_means
diff_observed <- diff(agg_means[, 2])
agg_means
diff_observed

#function to resample flipper length & find diff of means
two_group_resample <- function(x, n_1, n_2)
{
  dat_1 <- sample(x, n_1, replace = TRUE)
  dat_2 <- sample(x, n_2, replace = TRUE)
  diff_simulated <- mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  print(diff_simulated)
  }

two_group_resample(dat_pen$flipper_length_mm, 68, 152)

#----Question_3----

n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
png("Q3.png", height = 800, width = 800, res = 180)
hist(mean_differences,
     main = "Mean Differences Histogram",
     xlab = "Mean Differences")
dev.off()

#----Question_4----

#how many times is the resampled mean difference > observed mean difference?
sum(abs(mean_differences) >= diff_observed)

#----Question_5----
#####answer in Moodle#####

#----Question_6----

png("Q6.png", height = 800, width = 600, res = 180)
boxplot(bill_length_mm ~ species, data = dat_pen,
        main = "Bill Length by Species",
        xlab = "Species",
        ylab = "Bill Length (mm)")
dev.off()

#calc group means and diff btwn groups
diff_crit_agg <- aggregate(bill_length_mm ~ species, 
                       data = dat_pen, FUN = mean, 
                       na.rm = TRUE)
diff_crit <- diff(diff_crit_agg[, 2])
diff_crit_agg
diff_crit

#----Question_7----

#conduct t test, observe p value
t.test(dat_pen$bill_length_mm ~ dat_pen$species)

#----Question_8----

n = 1000
bill_mean_differences = c()
for (i in 1:n)
{
  bill_mean_differences = c(
    bill_mean_differences,
    two_group_resample(dat_pen$bill_length_mm, 68, 152)
  )
}
png("Q8.png", height = 800, width = 800, res = 180)
hist(bill_mean_differences,
     main = "Bill Mean Differences Histogram",
     xlab = "Mean Differences")
dev.off()

sum(abs(bill_mean_differences) >= diff_crit)

