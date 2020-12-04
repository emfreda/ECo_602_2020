require(here)
bird <- read.csv(here("data/bird.sub.csv"))
habitat <- read.csv(here("data/hab.sub.csv"))

#----WALKTHROUGH----
birdhab <- merge(bird, habitat, by = c("basin", "sub"))
dim(birdhab)

length(which(birdhab$ls == 100))

plot(BRCR ~ ls, data = birdhab)
fit_1 <- lm(BRCR ~ ls, data = birdhab)
abline(fit_1)
summary(fit_1)

#building deterministic model component
#function to find linear function based off input values
linear <- function(x, y_int, slope) {
  return(x * slope + y_int)
}
#stochastic model component
rnorm(mean = 0, sd = 1, n = length(birdhab$BRCR))

#----LINEAR_SIMULATOR_FXN----
linear_simulator <- function(x, y_int, slope, st_dev) {
  y_values <- linear(x, y_int, slope)
  errors <- rnorm(n = length(x), mean = 0, sd = st_dev)
  return(y_values + errors)
}

#test function
n = 200
par(mfrow = c(2, 2))
for (i in 1:4) {
  x = runif(n = n)
  plot(x, linear_simulator(x, 1, 4.5, 0.1),
       main = "", xlab = "x", ylab = "y",
       pch = 16, col = rgb(0, 0.2, 0, 0.2))
}
#test function again
n = 400
par(mfrow = c(2, 2))
for (i in 1:4) {
  x = runif(n = n)
  plot(x, linear_simulator(x, 10, slope = -6.5, st_dev = 1.1),
       main = "", xlab = "x", ylab = "y",
       pch = 16, col = rgb(0, 0.2, 0, 0.2))
}

#retrieve y_int, slope, and stdev from model fit to the data
#y_int and slope are in coeff table
fit_1_coefs <- coefficients(fit_1)
str(fit_1_coefs)

int_obs <- fit_1_coefs["(Intercept)"]
slope_obs <- fit_1_coefs["ls"]
#use summary to find stdev parameter
fit_1_summary <- summary(fit_1)

sd_obs <- fit_1_summary$sigma

#simulate data, plot on OG data
dev.off()
plot(birdhab$ls, birdhab$BRCR, 
     xlab = "late-successional forest extent",
     ylab = "Brown Creeper abundance",pch = 19)
points(x = birdhab$ls, 
       y = linear_simulator(x = birdhab$ls,
                            y_int = int_obs,
                            slope = slope_obs,
                            st_dev = sd_obs),
       col = adjustcolor("red", alpha = 0.3),
       pch = 16)


#----POWER_ANALYSIS----
#used for prob of correctly rejecting null hyp
#----SINGLE_SIMULATION----
y_sim <- linear_simulator(x = birdhab$ls,
                         y_int = int_obs,
                         slope = slope_obs,
                         st_dev = sd_obs)
fit_sim <- lm(y_sim ~ birdhab$ls)
summary(fit_sim)$coefficients

#----REPEATING_SIMULATIONS----
n_sims = 1000
p_vals = numeric(n_sims)
for(i in 1:n_sims) {
  y_sim = linear_simulator(x = birdhab$ls,
                           y_int = int_obs,
                           slope = slope_obs,
                           st_dev = sd_obs)
  fit_sim = lm(y_sim ~ birdhab$ls)
  
  p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
}
sum(p_vals < 0.05) / n_sims

#----SIMULATION_LOOP_FUNCTION----
#function to make simulation loops (above) simpler
linear_sim_fit <- function(x, y_int, slope, st_dev) {
  y_sim <- linear_simulator(x = x, y_int = y_int,
                           slope = slope,
                           st_dev = st_dev)
  return(lm(y_sim ~ x))
}


#----SIMULATING_EFFECT_SIZES----
#create sequence of fx (aka slope values) to try
#OG code nested in a loop that iterates over fx sizes

alpha <- 0.05
n_sims <- 50
p_vals <- numeric(n_sims)

n_effect_sizes <- 10
effect_sizes_1 <- seq(-.01, .01, length.out = n_effect_sizes)

effect_size_powers <- numeric(n_effect_sizes)

for(j in 1:n_effect_sizes) {
  for(i in 1:n_sims) {
    fit_sim <- linear_sim_fit(x = birdhab$ls,
                             y_int = int_obs,
                             slope = effect_sizes_1[j],
                             st_dev = sd_obs)
    #p-values calculated in inner loop, used to find stat power below
    p_vals[i] <- summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  #powers calculated and stored in effect_size_powers
  effect_size_powers[j] <- sum(p_vals < alpha) / n_sims
}
#results saved as a DF, fx size and stat power are columns
sim_effect_size <- data.frame(power = effect_size_powers,
                              effect_size = effect_sizes_1)
#plot the result w vert line to show OG slope
plot(power ~ effect_size, data = sim_effect_size,
     type = 'l', xlab = 'Effect size', ylab = 'Power')
abline(v = coef(fit_1)[2], lty = 2, col = 'red')

#----SIMULATING_SAMPLE_SIZES----
#test power over a gradient of sample sizes (?)
alpha <- 0.05
n_sims <- 10
p_vals <- numeric(n_sims)

sample_sizes <- seq(5, 100)
sample_size_powers <- numeric(length(sample_sizes))

for(j in 1:length(sample_sizes)) {
  x_vals <- seq(0, 100, length.out = sample_sizes[j])
  for(i in 1:n_sims) {
    fit_sim <- linear_sim_fit(x = x_vals,
                             y_int = int_obs,
                             slope = slope_obs,
                             st_dev = sd_obs)
    p_vals[i] <- summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sample_size_powers[j] <- sum(p_vals < alpha) / n_sims
}

sim_sample_size <- data.frame(power = sample_size_powers,
                             sample_size = sample_sizes)
#plot the results w OG sample size as a line
plot(power ~ sample_size, data = sim_sample_size,
     type = 'l', xlab = 'Sample size', ylab = 'Power')
abline(v = nrow(birdhab), lty = 2, col = 'red')


#----BIVARIATE_POWER_ANALYSIS----
alpha <- 0.01
n_sims <- 50
p_vals <- numeric(n_sims)

n_effect_sizes <- 20
effect_sizes <- seq(-.01, .01, length.out = n_effect_sizes)
sample_sizes <- seq(10, 50)
#use matrix to store data bc 2 variables
sim_output_2 <- matrix(nrow = length(effect_sizes),
                       ncol = length(sample_sizes))

for(k in 1:length(effect_sizes)) {
  effect_size <- effect_sizes[k]
  for(j in 1:length(sample_sizes)) {
    x_vals <- seq(0, 100, length.out = sample_sizes[j])
    for(i in 1:n_sims) {
      fit_sim <- linear_sim_fit(x = x_vals,
                               y_int = int_obs,
                               slope = effect_size,
                               st_dev = sd_obs)
      p_vals[i] <- summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_2[k, j] <- sum(p_vals < alpha) / n_sims
  }
  print(paste0("computing effect size ", k," of ", length(effect_sizes)))
}

sim_n_effect_size <- list(power = sim_output_2,
                         effect_size = effect_sizes,
                         sample_size = sample_sizes)
#use image() to plot a matrix as if it were a raster
image(sim_n_effect_size$power)





#----3D_PLOTTING----
#CONTOUR PLOT
#x-axis = effect size, y-axis = sample size
#z coordinates are the matrix storing above values
contour(x = effect_sizes, y = sample_sizes, z = sim_output_2)

#STATIC PLOT
persp(x = sim_n_effect_size$effect_size,
      y = sim_n_effect_size$sample_size,
      z = sim_n_effect_size$power,
      xlab = "beta", ylab = "n", zlab = "power",
      col = 'lightblue',
      theta = 30, phi = 30, expand = .75,
      ticktype = 'detailed')

#INTERACTIVE PLOT
#install rgl package
install.packages("rgl")
require(rgl)
#same syntax as persp() for persp3d()
persp3d(x = sim_n_effect_size$effect_size,
        y = sim_n_effect_size$sample_size,
        z = sim_n_effect_size$power,
        xlab = "beta", ylab = "n", zlab = "power",
        col = 'lightblue',
        theta = 30, phi = 30, expand = .75,
        ticktype = 'detailed')

#----SAVING_FILES----
#saving interactive plot
rgl::writeWebGL(dir = here::here("docs", "webGL"), 
                filename = here::here("docs", "webGL",
                           "n_effect_size_power_sim_plot.html"),
                width = 1200, height = 1200)
#saving simulation results
save(sim_n_effect_size,
     file = here::here("data", "lab_11_n_effect_sizes.Rdata"))
#to load the data back into R:
load(file = here::here("data", "lab_11_n_effect_sizes.Rdata")))


