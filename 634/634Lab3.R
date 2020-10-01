#require psych package and test it on iris dataset
require(psych)
pairs.panels(iris)
#load here and make df for bird & habitat csvs
require(here)
here()
#read in and make a dataframe of the habitat data
dat_habitat <- data.frame(read.csv(here("data/hab.sta.csv")))
#read in and make a dataframe of the bird data
dat_birds <- data.frame(read.csv(here("data/bird.sta.csv")))

#use merge fxn to make a dataframe of both
dat_all <- merge(dat_birds, dat_habitat)
#test the merge w this plot
plot(ba.tot ~ elev, data = dat_all)

#make boolean vector of presence/absence for a species
cewa_pres_abs <- dat_all$CEWA >= 1
cewa_asnumeric <- as.numeric(cewa_pres_abs)
cewa_asnumeric
#test the boolean vector
plot(x = dat_all$elev, y = cewa_asnumeric)

#calculate a logistic parameter given a slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}

# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slopoe and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

#plot the logistic curve
plot(x = dat_all$ba.tot, y = amgo_pres_abs,
     xlab = "Basal Area",
     ylab = "Presence/Absence")
curve(logistic_midpoint_slope(x, midpoint = 50, slope = -.7), add = TRUE)


#find total number of Gray Jays in all sites
sum(dat_all$GRJA)

#find the total number of sites with Gray Jays
grja_pres <- dat_all$GRJA >= 1
grja_asnum <- as.numeric(grja_pres)
sum(grja_asnum)





