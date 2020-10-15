require(palmerpenguins)
require(boot)
require(simpleboot)
require(here)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
veg <- read.csv(here("data/vegdata.csv"))



