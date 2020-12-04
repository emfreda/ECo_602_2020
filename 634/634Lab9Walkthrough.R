require(here)
catrate = read.csv(here("data", "catrate.csv"))

#----TESTING_PROPORTIONS----
#BINOMIAL TEST for signifigance of ratios; p(success)=0.5
success <- sum(catrate$success)
years <- sum(catrate$years)
binom.test(success, years)
#change expected prob to 5/7, ie success w no late pond filling
binom.test(success, years, p = 5/7) 
#change to one sided test; obs rate < late fill rate
binom.test(success, years, p = 5/7, alternative = 'less')


#----COMPARING_VARIANCES----
#F-TEST for if the sample variances are sig diff
#F-statistic is 1 if variances are the same
#assumes norm dist
veg <- read.csv(here("data", "vegdata.csv"), header = TRUE)
#visualize data
boxplot(pine ~ treatment, data = veg)

var.test(pine ~ treatment, data = veg,
         subset = treatment %in% c('control','clipped'))
#tiny p-vlaue, reject null (ie. data ratio is not 1)

shapiro.test(veg$pine[veg$treatment=="control"])
shapiro.test(veg$pine[veg$treatment=="clipped"])
#data are not norm so F-test isn't good bc it assumes norm

#FLINGER-KILLEEN TEST if F-test for non-norm dist
fligner.test(pine ~ treatment, data = veg, 
             subset = treatment %in% c('control','clipped'))
#tiny p-vlaue, reject null (ie. data ratio is not 1)

#BARTLETT'S TEST for k-sample (F-stat for more than two groups)
#assumes norm dist and sensitive to outliers (like F-test)
bartlett.test(pine ~ treatment, data = veg)
#use Flinger-Killeen Test for non-norm dist; can have n groups
fligner.test(pine ~ treatment, data = veg)


#----COMAPRING_SAMPLE_MEANS----
#t-test: indep. samples, constant variances, norm dist errors
t.test(pine~treatment, data = veg,
       subset = treatment %in% c('control','clipped'),
       conf.int = TRUE)
#conf.int defalut in 95%; if incudes 0 = sample means not sig diff
#p-value > 0.05 -> dont reject null

#Wilcoxon: indep. samples, non-norm dist errors
wilcox.test(pine~treatment, data = veg,
            subset = treatment %in% c('control','clipped'),
            conf.int = TRUE)
#p-value >> 0.05 -> dont reject null

#PAIRED T-TEST if suspect correlation btwn 2 measurements
#if pos covariance -> decrease var of mean diff -> easier to find sig diff in means
#need seperate vectors for using paired t.test
control <- veg$pine[veg$treatment=='control']
clipped <- veg$pine[veg$treatment=='clipped']
t.test(control, clipped, paired = TRUE)
#p-value > 0.05 -> dont reject null

wilcox.test(control, clipped, paired = TRUE)
#reject null


#----TESTING_CORRELATION----
disp = read.csv(here("data", "dispersal.csv"), header = TRUE)

cor.test(disp$disp.rate.ftb, disp$disp.rate.eb,
         use = 'complete.obs')
#last argument is bc there are NA values

#SPEARMANS RANK CORRELATION uses a diff statistic
#good for data w/o bivariate norm dist
cor.test(disp$disp.rate.ftb, disp$disp.rate.eb,
         use ='complete.obs', method ='spearman')


#----COMPARING_TWO_DISTRIBUTIONS----
#KOLMOGOROV-SMIRNOV TEST comapres ecdfs, are they sig diff?
#look at ecdf first
plot(ecdf(disp$disp.rate.ftb), verticals = TRUE)
plot(ecdf(disp$disp.rate.eb), verticals = TRUE, lty = 3, add = TRUE)

ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)
#large p-value -> null true (distributions are same)

#----COMPARING_TWO_OR_MORE_PROPORTIONS----
#proportions test w specified vectors 
#vec 1 = mortalities for fem and male c(4,16)
#vec 2 = total fem and male c(40,250)
prop.test(c(4,16),c(40,250))
#p-value >> 0.05 -> proportions are not stat sig diff


#----CONTINGENCY_TABLES----
#shows counts of # times each contingency happened in a sample
owls <- matrix(c(16, 9, 4, 11), nrow = 2)
rownames(owls) <- c("present", "absent")
colnames(owls) <- c("old", "young")
owls

#CHI-SQUARE TEST
#tests to see if expected freqs and obs freqs are sig diff
#contingency table values have to be > 4 or 5 to use
chisq.test(owls)
#p-value ~ 0.05, could reject null of obs = expected

#FISHER'S ECAXT TEST
#chi-squared but for small numbers (< 4 or 5)
fisher.test(owls)
#p-value < 0.05 -> reject null


birds <- read.csv(here("data", "bird.sta.csv"), header=TRUE)
hab <- read.csv(here("data", "hab.sta.csv"), header=TRUE)
birdhab <- merge(birds, hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper pres/abs
#set presence to be in first column bc some fxns expect this order
br_creeper_table <- table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]

