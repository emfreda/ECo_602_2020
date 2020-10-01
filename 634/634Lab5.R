########## QUESTION 1 ########## exponential functions
exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}
png("Q1.png", height = 800, width = 800, res = 180)
curve(exp_fun(x, 1.9, 0.1), 
      from = 0, to = 40, add = FALSE, 
      main = "4 Exponential functions",
      ylab = "f(x)", xlab = "x",
      col = "black", lty = 1)
curve(exp_fun(x, 1.9, 0.3), 
      col = "black", lty = 3, add = TRUE)
curve(exp_fun(x, 1.2, 0.2), 
      col = "red", lty = 1, add = TRUE)
curve(exp_fun(x, 1.2, 0.4), 
      col = "red", lty = 3, add = TRUE)
dev.off()
########## QUESTION 2 ##########
  ##### answer in Moodle #####
########## QUESTION 3 ########## Ricker functions
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
png("Q3.png", height = 800, width = 800, res = 180)
curve(ricker_fun(x, 25, 0.1), 
      from = 0, to = 50, add = TRUE,
      main = "6 Ricker functions",
      ylab = "f(x)", xlab = "x",
      col = "black", lty = 1)
curve(ricker_fun(x, 20, 0.2), 
      col = "black", lty = 3, add = TRUE)
curve(ricker_fun(x, 10, 0.2), 
      col = "black", lty = 3, add = TRUE)
curve(ricker_fun(x, 75, 0.3), 
      col = "red", lty = 1, add = TRUE)
curve(ricker_fun(x, 50, 0.3), 
      col = "red", lty = 3, add = TRUE)
curve(ricker_fun(x, 40, 0.3), 
      col = "red", lty = 3, add = TRUE)
dev.off()
########## QUESTION 4 ##########
  ##### answer in Moodle #####
########## QUESTION 5 ########## scatterplot
require(here)
dat_dispersal <- data.frame(read.csv(here("data/salamander_dispersal.csv")))

png("Q5.png", height = 800, width = 800, res = 180)
plot(x = dat_dispersal$dist.class, y = dat_dispersal$disp.rate.ftb,
     main = "First Time Breeding\n Salamander Dispersal",
     xlab = "Distance Class",
     ylab = "Dispersal Rate")
dev.off()

########## QUESTION 6 ########## linear fitting
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  linear = function(x, yint, slope) 
      return(yint + x * slope)
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
png("Q6.png", height = 800, width = 1000, res = 180)
plot(x = dat_dispersal$dist.class, y = dat_dispersal$disp.rate.ftb,
     xlim = c(0,1400),
     main = "First Time Breeding\n Salamander Dispersal\n Linear Model",
     xlab = "Distance Class",
     ylab = "Dispersal Rate")
curve(line_point_slope(x, 700, 0.3, -0.0005), add = TRUE)
dev.off()

########## QUESTION 7 ########## exponential fitting
png("Q7.png", height = 800, width = 1000, res = 180)
plot(x = dat_dispersal$dist.class, y = dat_dispersal$disp.rate.ftb,
     xlim = c(100,1550),
     main = "First Time Breeding\n Salamander Dispersal\n Exponential Model",
     xlab = "Distance Class",
     ylab = "Dispersal Rate")
curve(exp_fun(x, 1.7, .003), add = TRUE)
dev.off()

########## QUESTION 8 ########## Ricker fitting
png("Q8.png", height = 800, width = 800, res = 180)
plot(x = dat_dispersal$dist.class, y = dat_dispersal$disp.rate.ftb,
     xlim = c(10,1550),
     main = "First Time Breeding\n Salamander Dispersal\n Ricker Model",
     xlab = "Distance Class",
     ylab = "Dispersal Rate")
curve(ricker_fun(x, .0085, .0045), add = TRUE)
dev.off()

########## QUESTION 9 ########## linear residuals
#make a vector of residual values, use our data points for x argument in line fuction
#so that the vector is only the number of values from the function that we want
y_predicted_linear <- c(line_point_slope(dat_dispersal$dist.class, 700, 0.3, -0.0005))
#add the predicted y values to the dataframe (good data mgmt)
dat_dispersal <- cbind(dat_dispersal, y_predicted_linear)
#calc resudials, store as a vector for graphing
resids_linear <- c(dat_dispersal$disp.rate.ftb - dat_dispersal$y_predicted_linear)

png("Q9.png", height = 700, width = 700, res = 180)
hist(resids_linear,
     main = "Linear Residuals Histogram",
     xlab = "Residual")
dev.off()
########## QUESTION 10 ########## exponential residuals
y_predicted_exp <- c(exp_fun(dat_dispersal$dist.class, 1.7, .003))
dat_dispersal <- cbind(dat_dispersal, y_predicted_exp)
resids_exp <- c(dat_dispersal$disp.rate.ftb - dat_dispersal$y_predicted_exp)

png("Q10.png", height = 700, width = 700, res = 180)
hist(resids_exp,
     main = "Exponential Residuals Histogram",
     xlab = "Residual")
dev.off()

########## QUESTION 11 ########## Ricker residuals
y_predicted_Ricker <- c(ricker_fun(dat_dispersal$dist.class, .0085, .0045))
dat_dispersal <- cbind(dat_dispersal, y_predicted_Ricker)
resids_Ricker <- c(dat_dispersal$disp.rate.ftb - dat_dispersal$y_predicted_Ricker)

png("Q11.png", height = 700, width = 700, res = 180)
hist(resids_Ricker,
     main = "Ricker Residuals Histogram",
     xlab = "Residual")
dev.off()

