require(here)
moths <- read.csv(here("data", "moths.csv"))


#PARAMETRIC CONFIDENCE INTERVAL for mean standardized abundance
###use a student t distribution for the sample mean
###useful when pop variance is unknown (must est from sample)
alpha = 0.05
anst = moths$anst
n = sum(!is.na(anst))
t_crit = abs(qt(alpha / 2, df = n - 1))

sse = sd(anst) / sqrt(n)

sample_mean <- mean(anst)
ci_parametric <- sse * t_crit

confidence_intervals <- data.frame(
  technique <- c("parametric: t-dist"),
  mean <- sample_mean,
  ci_radius <- sse * t_crit,
  lower <- sample_mean - ci_parametric,
  upper <- sample_mean + ci_parametric)
confidence_intervals
#if CI does not contain 0 -> mean didnt come from a dist w a mean=0



#BOOTSTRAP (NONPARAMETRIC) CONFIDENCE INTERVALS
###more robust for small sample sizes
###works when we dont know if the pop is norm dist

#BOOTSTRAP BY HAND
#resample 1000 times from the 24 entries we have
m <- 10000
result <- numeric(m)
#numeric makes a vector of zeros of m length 

for(i in 1:m) {
  result[i] <- mean(sample(anst, replace=TRUE)) }

mean(result) #find the mean 
quantile(result,c(0.025, 0.975)) #find the 2.5% / 97.5% quantiles

#bootstrap CI are skewed bc data is skewed
#parametric CI are symmetric bc based off a dist (t-dist)

#BOOTSTRAP W BOOT PACKAGE
install.packages("boot")
require(boot)
#boot(data, statistic function, # resamplings)

#need a custom mean function to use boot() w our data
boot_mean = function(x, i) {
  return(mean(x[i], na.rm = TRUE)) }
#bootstrap 10000 iterations
myboot <- boot(anst, boot_mean, 10000)
print(myboot)
#original = og mean of the whole sample (mean(anst))
#bias = diff btwn og mean & bootstrapped samples mean
#std.error = stdev of bootstrapped values

#str() lets u see what attributes u can call w $
str(myboot)
#extracting CI
quantile(myboot$t, c(0.025, 0.975))



#RAREFACTION CURVE
#as sampling intensity inc -> species richness inc
###rarefaction adjusts species richness among 
###samples w varying sampling intensity

#run to clear envi & make sure function works
rm(list = ls())
moths = read.csv(here("data", "moths.csv"))

#will create a matrix where each row is an iteration
#and each column is a diff sample size
rarefaction_sampler <- function(input_dat, n_iterations) {
  n_input_rows <- nrow(input_dat)
  results_out <- matrix(nrow = n_iterations, ncol = n_input_rows)

  #OUTER LOOP: runs once for each bootstrap iteration; index variable = i
  for(i in 1:n_iterations) {
    #INNER LOOP: simulates increasing sampling intensity
    #sampling intensity ranges from 1 site -> complete # of sites in input data (n)
    for(j in 1:n_input_rows) {
      #sample the input data row indices, with replacement
      rows_j <- sample(n_input_rows, size = j, replace=TRUE)
      #creates a new data matrix
      t1 <- input_dat[rows_j, ]
      #calculates the column sums
      t2 <- apply(t1, 2, sum)
      #counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }}
  return(results_out) }

#this should run if function works
rarefact <- rarefaction_sampler(moths[,-1], 10000)
head(rarefact)

rare_mean <- apply(rarefact, 2, mean)
rare_quant <- apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
#put in dataframe w rows as sampling intensity, col as mean/quantiles
rare = t(rbind(rare_mean, rare_quant))

matplot(rare, type='l',
  xlab='Number of sampling plots',
  ylab='Species richness', 
  main='Rarefaction Curve')

legend('bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3), col=c(1,2,3), inset=c(.1,.1))
