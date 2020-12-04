#no final r script for this assignment; answers in google docs

require(palmerpenguins)
penguins <- palmerpenguins::penguins

#----T.TEST_RECAP----
#####one sample two tailed
t.test(subset(penguins, species == "Gentoo")$flipper_length_mm)
#reject null
#change null from = 0 to = 218
t.test(x = subset(penguins, species == "Gentoo")$flipper_length_mm,
       mu = 218)
#no sig evidence to reject null

#####one sample one tailed; null = less than 218
t.test(x = subset(penguins, species == "Gentoo")$flipper_length_mm,
       mu = 218, alternative = "less")
#no sig evidence to reject null

#####two sample two tailed
t.test(flipper_length_mm ~ species,
       data = subset(penguins, species != "Chinstrap"))
#reject null


#----ANOVA----
#do graphical/numerical exploration
#fit linear model using lm()
#use summary() to examine model coeff table
#use anova()

par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, 
     main = "histogram of body mass", 
     xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE),
     main = "density plot of body mass")
dev.off()
#try a conditional boxplot; good for categorical vars
boxplot(body_mass_g ~ species, data = penguins)

#test if within group data are norm dist
#extract each species, find mean body mass, do shapiro.test
peng_agg <- aggregate(body_mass_g ~ species, 
                      data = penguins, FUN = mean)
shapiro.test(peng_agg$body_mass_g)

#fit linear model
fit_species <- lm(body_mass_g ~ species, data = penguins)
#look at model coefficients
summary(fit_species)
#conduct ANOVA on the linear model
anova(fit_species)
#see interpretation summary section of walkthrough


#----TWO_WAY_ANOVA----
boxplot(body_mass_g ~ sex + species, data = penguins)
fit_both <- lm(body_mass_g ~ sex * species, data = penguins)
summary(fit_both)






