# Start with a clean workspspace:
rm(list = ls())

# Library imports
#install.packages('sm', dependcies = T)     # etc, if not installed
library(sm)
library(mgcv) # for gam()

# Preprocessing steps
# Changed original file to read only (> chmod a-w)
# Made copy, 'data.csv' to edit

# Filled blanks with NA 
                                        # moved 'revenue' (response var) to first field to be consistent with R
# commands, e.g. lm(y ~)

data <- read.csv('~/Desktop/cbtnuggetsbusinessanalyticsproject/data.csv')
dim(data) # 117 records, 42 vars
summary(data)

                                        # Eyeball data visually, 9 variables at a time, to check for potential
# outliers, data distributions, etc.
par(mfrow = c(3, 3))
for (i in 1:dim(data)[2]) {
  plot(data[, i], main = names(data)[i])
  if (i %% 9 == 0 ) { pause() }
}

par(mfrow = c(3, 3))
n <- 0
  
for (i in 1:dim(data)[2]) {
  if (class(data[, i]) != 'factor') {
    hist(data[, i], main = names(data)[i])
    n <- n + 1
    print(n)
  }
  
  if (n %% 9 == 0 ) { pause() }
}


# Observations:
# These are time series data, where time is clearly an important element
# Each record is a week's data; ordered temporally (top dates = most recent)

# Binary variables (change to categorical vars):
# highseason, holiday, autoenroll, socialperson*, promoteposts*, learner
# *one value in each not = 0 or 1; presumed to be an input error

# Potential outliers in:
# displayimp, displaylag, brandpaid, socialpostimp, Fblikes*, Fbengage* 
# *possibly just very skewed data

# Many data are highly non-normal, though revenue is not so bad
shapiro.test(data$revenue) # not signif different from normal at alpha of 0.05
# Other
# emailsent, emailopen, and emailclick show two distinct trends, with
# transition occurring near index 55

# Replace data with NA values: 
# For the (apparently) binary variables, the values that are not 0/1 are
# assumed to be input errors, and replaced with NAs
par(mfrow = c(1, 1))
hist(data$socialperson)
abline(h = 1, col = 2) # verify that it is just one erroneous value
hist(data$promoteposts)
abline(h = 1, col = 2)

data$socialperson[data$socialperson == 11] <- NA
data$promoteposts[data$promoteposts == 10] <- NA


# Convert binary data to categorical 
data$highseason <- as.factor(data$highseason)
data$holiday <- as.factor(data$holiday)
data$autoenroll <- as.factor(data$autoenroll)
data$socialperson <- as.factor(data$socialperson)
data$learner <- as.factor(data$learner)


# Convert 'Week' to Date type
data$Week <- as.Date(data$Week, format = '%m/%d/%y')


# Because revenue is a time series, explore temporal effects further
revenue.ts <- as.ts(data$revenue, start = '2014-02-02', frequency = 52)
# Check for temporal autocorrelation in revenue values
acf(data$revenue)
# check for autocorrelation in variance
acf((data$revenue - mean(data$revenue))^2) # slight at best
plot(revenue.ts)
abline(v = c(52, 104), col = 2) # divisions between years

plot(diff(data$revenue), type = 'l')
acf(diff(data$revenue))
hist(diff(data$revenue))
shapiro.test(diff(data$revenue))


# Explore relationships between predictors and response
par(mfrow = c(3, 3))
for (i in 2:dim(data)[2]) {
  plot(data[, 1] ~ data[, i], xlab = names(data)[i], ylab = 'revenue')
  lines(lowess(data[, 1][!is.na(data[, i])] ~ data[, i][!is.na(data[, i])]),
        col = 2)
  if ((i - 1) %% 9 == 0 ) { pause() }
}

# Potentially significant nonlinear trends in:
# brandpaid, fbposts, Twpostimp, twengage, twengagelag, blogsignup

































# Explore with LM
dim(data[complete.cases(data), ])
rev.lm <- lm(revenue ~ . + 
             c(NA, diff(data$revenue)) + 
             c(rep(NA, 2), diff(data$revenue, difference = 2)) +
             c(rep(NA, 3), diff(data$revenue, difference = 3)) +
             c(rep(NA, 4), diff(data$revenue, difference = 4)) +
             c(rep(NA, 5), diff(data$revenue, difference = 5)), 
             data = data)
rev.lm <- update(rev.lm, .~. - weekno - Fbpostimplag - Twpostimp - Fbpageimp)
rev.lm <- update(rev.lm, .~. - emailopen)
rev.lm <- update(rev.lm, .~. - twposts - blogvisits)
rev.lm <- update(rev.lm, .~. - promoteposts)
rev.lm <- update(rev.lm, .~. - holiday - nonbrandpaid - youtube - blogrefernew)
rev.lm <- update(rev.lm, .~. - displayimp)
rev.lm <- update(rev.lm, .~. - Fblikes - blognewusers)
rev.lm <- update(rev.lm, .~. - emailclick - blogsignup)
rev.lm <- update(rev.lm, .~. - socialpostimp - twengage - Fbpostimp)
rev.lm <- update(rev.lm, .~. - displaylag - blogrefer)
rev.lm <- update(rev.lm, .~. - learner - organicnew)
rev.lm <- update(rev.lm, .~. - socialimp)
rev.lm <- update(rev.lm, .~. - brandpaid)
rev.lm <- update(rev.lm, .~. - socialimplag)
rev.lm <- update(rev.lm, .~. - emailsent)
rev.lm <- step(rev.lm, direction = 'both')
summary(rev.lm)

# Explore with GAM
rev.gam <- gam(
  revenue ~ Week + highseason + brandclicks + nonbrandclicks + youtubelag +
  socialpostlag + s(fbposts) + Fbengage + fbengagelag + TWimpLag +
  s(twengagelag) +
  autoenroll + socialperson + 
  c(NA, diff(data$revenue)) + 
  c(rep(NA, 2), diff(data$revenue, difference = 2)) + 
  c(rep(NA, 3), diff(data$revenue, difference = 3)) + 
  c(rep(NA, 4), diff(data$revenue, difference = 4)) + 
  c(rep(NA, 5), diff(data$revenue, difference = 5)) + 
  s(brandpaid) + s(blogsignup) + s(fbposts) + s(Twpostimp) + s(twengage), 
  data = data)
summary(rev.gam)								# GCV = 5.1917e+07
anova(rev.gam)

#rev.gam1 <- update(rev.gam, . ~ . - highseason); summary(rev.gam1)	#GCV = 5.0332e+07
#rev.gam1 <- update(rev.gam, . ~ . - Fbengage); summary(rev.gam1)	#GCV = 4.9177e+07
rev.gam1 <- update(rev.gam, . ~ . - fbengagelag); summary(rev.gam1) #GCV = 4.9085e+07
anova(rev.gam1)

#rev.gam2 <- update(rev.gam1, . ~ . - highseason); summary(rev.gam2) #GCV = 4.7623e+07
rev.gam2 <- update(rev.gam1, . ~ . - Fbengage); summary(rev.gam2)    #GCV = 4.6561e+07
anova(rev.gam2)

rev.gam3 <- update(rev.gam2, . ~ . - highseason); summary(rev.gam3) #GCV = 4.5078e+07
anova(rev.gam3)

#rev.gam4 <- update(rev.gam3, . ~ . - autoenroll); summary(rev.gam4) #GCV = 4.5143e+07
#rev.gam4 <- update(rev.gam3, . ~ . - 
#                   c(rep(NA, 5), diff(data$revenue, difference = 5))); summary(rev.gam4) 													#GCV = 4.5078e+07
#rev.gam4 <- update(rev.gam3, . ~ . - s(twengagelag)); summary(rev.gam4) #

par(mfrow = c(2, 3))
plot(rev.gam3)

# Consider nonlinear terms for 
# fbposts, brandpaid, Twpostimp and twengage

# NEXT:
# Switch to python and sklearn:  Hold out a test set.
# Use lasso to thin out the set of potential predictors

