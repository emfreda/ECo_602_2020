dat_bird <- read.csv(here::here("data", "bird.sta.csv"))
dat_habitat <- read.csv(here::here("data", "hab.sta.csv"))
dat_all <- merge(dat_bird, dat_habitat)

#----Question_1----
wiwa_counts = c(2, 6)
sum(log(dpois(x = wiwa_counts, lambda = 4.0)))
#lambda 4 maximizes sum of log likelihood

#----Question_2----
WIWR_count <- dat_all$WIWR
hist(dpois(x = WIWR_count, lambda = 1.6))
#lambda = 1.6 maximizes the number of counts 
#with as high of a dpois value as possible

png("Q2.png", height = 800, width = 800, res = 180)
hist(dat_all$WIWR, breaks = 0:7 - .5,
     main = "Winter Wren Count Histogram",
     xlab = "Count")
dev.off()

sum(log(dpois(x = dat_all$WIWA, lambda = 1.5)))
max(WIWR_count)
#prod(dpois(x = wiwa_counts, lambda = 4.5))
#sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))


#----Question_3----
max(WIWR_count)
hist(dbinom(WIWR_count, 6, 0.2))


