require(palmerpenguins)
penguins <- palmerpenguins::penguins

#----QUESTION_1----
bartlett.test(body_mass_g ~ species, data = penguins)
#p-value: 0.0500

#----QUESTION_2----
bartlett.test(body_mass_g ~ sex, data = penguins)
#p-value: 0.0319

#----QUESTION_3----
species_sex <- aggregate(body_mass_g ~ sex * species, 
                         data = penguins, FUN = c)
str(species_sex)
#use str() to look at this list to find which object within it
#you want to run the test on
bartlett.test(species_sex$body_mass_g)
#p-value: 0.1741

#----QUESTION_4----
require(here)
birds <- read.csv(here("data", "bird.sta.csv"), header=TRUE)
hab <- read.csv(here("data", "hab.sta.csv"), header=TRUE)
birdhab <- merge(birds, hab, by=c("basin", "sub", "sta"))

br_creeper_table <- table(birdhab$s.edge, 
                          birdhab$BRCR > 0)[, 2:1]
chisq.test(br_creeper_table)
#p-value: 1.386e-6
#reject null of no sig diff btwn obs and expected freqs; aka theyre independent
#the proportions are signifigantly different; aka not independent








