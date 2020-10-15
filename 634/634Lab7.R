rm(list = ls())
#----Question_1----
# find parametric 95% CI for mean bill length of Gentoo
penguins <- palmerpenguins::penguins

dat_gen <- subset(penguins, species == "Gentoo")
#dat_pen <- droplevels(subset(penguins, species != "Adelie"))
#dat_gen_2 <- droplevels(subset(dat_pen, species != "Chinstrap"))

alpha = 0.05
gentBL <- dat_gen$bill_length_mm
n = sum(!is.na(gentBL))
t_crit = abs(qt(alpha / 2, df = n - 1))

sse = sd(gentBL, na.rm = T) / sqrt(n)
sample_mean <- mean(gentBL, na.rm = T)
ci_parametric <- sse * t_crit

confidence_intervals <- data.frame(
  technique = c("parametric: t-dist"),
  mean = sample_mean,
  ci_radius = sse * t_crit,
  lower <- sample_mean - ci_parametric,
  upper <- sample_mean + ci_parametric)
confidence_intervals



#----Question_2----
# find bootstrap 95% CI for mean bill length of Gentoo w boot()
require(boot)

boot_mean = function(x, i) {
  return(mean(x[i], na.rm = TRUE)) }
#bootstrap 10000 iterations
gentboot <- boot(gentBL, boot_mean, 124)
quantile(gentboot$t, c(0.025, 0.975))



#----Question_3----
# rarefaction sampler function
rarefaction_sampler <- function(input_dat, n_iterations) {
  n_input_rows <- nrow(input_dat)
  results_out <- matrix(nrow = n_iterations, ncol = n_input_rows)
  
  #OUTER LOOP: runs 1x per bootstrap iteration; index variable = i
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



#----Question_4----
# plot rarefaction curve
moths = read.csv(here("data", "moths.csv"))
rarefact <- rarefaction_sampler(moths[,-1], 10000)
rare_mean <- apply(rarefact, 2, mean)
rare_quant <- apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

png("Lab7Rarefaction.png", height = 700, width = 700, res = 180)
matplot(rare, type='l',
        xlab='Number of sampling plots',
        ylab='Species richness', 
        main='Rarefaction Curve')

legend('bottomright',
       legend=c('mean','2.5%','97.5%'),
       lty=c(1,2,3), col=c(1,2,3), inset=c(.1,.1))
dev.off()

#ignore unless needed
gent.matrix <- matrix(gentBL, length(gentBL), 1)
gent.matrix

gentrarefact <- rarefaction_sampler(gent.matrix, 1000)

