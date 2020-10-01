###############QUESTION 1###############
m <- 10.4
sd <- 2.4
norm_17 <- rnorm(17, mean = m, sd = sd)
norm_30 <- rnorm(30, mean = m, sd = sd)
norm_300 <- rnorm(300, mean = m, sd = sd)

###############QUESTION 2###############
png( filename = "lab_04_hist_01.png",
     width = 700,
     height = 1400,
     res = 180)
par(mfrow = c(3,1))
hist(norm_17, main = "Histogram of 17 random points")
hist(norm_30, main = "Histogram of 30 random points")
hist(norm_300, main = "Histogram of 300 random points")
dev.off()

###############QUESTION 3###############
#writing answers in Moodle

###############QUESTION 4###############
#make a vector of x values
x = seq(0, 21, length.out = 1000)
#make a probability density function
y = dnorm(x, mean = m, sd = sd)
#save file as png
png(filename = "norm_1.png",
    width = 700,
    height = 700,
    res = 130)
#plot the function
plot(x, y,
     main = "Normal PDF, mean of 10.4 & \n standard deviation of 2.4",
     type = "l")
dev.off()
#adds a line at y = 0
abline(h = 0)

###############QUESTION 5###############
png("sim_data_scatterplots.png",
    height = 900,
    width = 900,
    res = 100)
#mai is border spacing; c(bottom, left, top, right)
par(mfrow = c(2,2), mai = c(0.7, 0.5, 0.3, 0.3))

set.seed(1)
#create a df with random variables
n_pts = 10
x_min = 1
x_max = 10
x1 = runif(n = n_pts, min = x_min, max = x_max)
dat1 = data.frame(x = x1, y_observed = rnorm(n_pts))
#plot it
plot(y_observed ~ x1, data = dat1, pch = 15)

set.seed(2)
#create a df with random variables
n_pts = 20
x_min = 1
x_max = 10
x2 = runif(n = n_pts, min = x_min, max = x_max)
dat2 = data.frame(x = x2, y_observed = rnorm(n_pts))
plot(y_observed ~ x2, data = dat2, pch = 14)

set.seed(3)
#create a df with random variables
n_pts = 50
x_min = 1
x_max = 10
x3 = runif(n = n_pts, min = x_min, max = x_max)
dat3 = data.frame(x = x3, y_observed = rnorm(n_pts))
plot(y_observed ~ x3, data = dat3, pch = 5)

set.seed(4)
#create a df with random variables
n_pts = 100
x_min = 1
x_max = 10
x4 = runif(n = n_pts, min = x_min, max = x_max)
dat4 = data.frame(x = x4, y_observed = rnorm(n_pts))
plot(y_observed ~ x4, data = dat4, pch = 12)
dev.off()

###############QUESTION 6###############
#reset par to 1 plot/normal margins w dev.off()
dev.off()
#add linear function to script
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
#this is the dataset being used
set.seed(3)
#create a df with random variables
n_pts = 50
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)
dat3 = data.frame(x = x, y_observed = rnorm(n_pts))

#these parameters input into the line fxn
guess_x = 5
guess_y = 0
guess_slope = -0.4
png("Q6 Plot.png",
    height = 600,
    width = 600,
    res = 100)
plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
dev.off()

##############QUESTION 7###############

set.seed(3)
#create a df with random variables
n_pts = 50
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)
dat3 = data.frame(x = x, y_observed = rnorm(n_pts))
dat3
#create a column of predicted y-values
dat3$y_predicted <- line_point_slope(x, guess_x, guess_y, guess_slope)

#create a column of residuals
dat3$resids <- dat3$y_predicted - dat3$y_observed
dat3

###############messing around, not part of lab###############
#exploring the set.seed() conceptually
#a seed is an index on the string of random numbers R references
#when you call the rnorm function
set.seed(3)
one <- rnorm(4)
set.seed(3)
two <- rnorm(5)
one
two

#exploring the diff btwn sum of resids vs sum of abs(resids)
#abs(resids) is much bigger bc the pos/neg values dont cancel
dat3$residsAbs <- abs(dat3$resids)
sum(dat3$residsAbs)
dat3  
  
  