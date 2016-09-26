#============================================================#
#                                                            #
# General exploration of the data and preprocessing          #
#                                                            #
# @author Damian Satterthwaite-Phillips <damiansp@gmail.com> #
# @version 21 Sep 2016                                       #
#                                                            #
#============================================================#

# Start with a clean workspspace:
rm(list = ls())

# Library imports
#install.packages('sm', dependcies = T)     # etc, if not installed
library(sm)

# Preprocessing prior to this:
#   Changed original file to read only (> chmod a-w)
#   Made copy, 'data.csv' to edit
#   Filled blanks with NA 
#   moved 'revenue' (response var) to first field to be consistent with R
#   commands, e.g. lm(y ~)

data <- read.csv('~/Desktop/cbtnuggetsbusinessanalyticsproject/data.csv')
dim(data) # 117 records, 42 vars
# summary(data)

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
  }
  
  if (n %% 9 == 0 ) { pause() }
}


# Observations:
# These are time series, and time is clearly an important element
# Each record is a week's data; ordered temporally (top dates = oldest)

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
data$promoteposts <- as.factor(data$promoteposts)
data$learner <- as.factor(data$learner)


# Convert 'Week' to Date type
data$Week <- as.Date(data$Week, format = '%m/%d/%y')



# Explore temporal correlation:

# Because revenue is a time series, explore temporal effects further
# Check for temporal autocorrelation in revenue values
acf(data$revenue) # yes signif for about 5 weeks lag
# check for autocorrelation in variance
acf((data$revenue - mean(data$revenue))^2) # slight at best
plot(data$revenue, type = 'l')
abline(v = c(52, 104), col = 2) # divisions every 52 weeks (~1 yr)
# With just over 2 years of data, it is not very clear, but there may be 
# a seasonal effect: lower revenue in summer, higher near year ends

# How well does differencing remove autocorrelation:
plot(diff(data$revenue), type = 'l')
abline(h = 0, col = 'grey')
acf(diff(data$revenue))
# pretty well, but note that the differenced distribution has fat tails
hist(diff(data$revenue))
shapiro.test(diff(data$revenue))

# Add some potentially useful variables to deal with temporal AC
data$rev1weekAgo <- c(NA,         data$revenue[1:116])
data$rev2weekAgo <- c(rep(NA, 2), data$revenue[1:115])
data$rev3weekAgo <- c(rep(NA, 3), data$revenue[1:114])
data$rev4weekAgo <- c(rep(NA, 4), data$revenue[1:113])


# Make sure all weeks are evenly spaced (no gaps)
diff(data$Week)
# Appx 52 weeks per year; create new variable telling which week of the year 
# it is (e.g., so first week of Mar 2015 can be grouped with first week of 
# Mar 2016 to check for seasonal effects)
# NOTE: data begin on Feb 2
weekOfYear <- (data$weekno + 4) %% 52
weekOfYear[weekOfYear == 0] <- 52
data$weekOfYear <- weekOfYear
# Verify correct alignment
data[47:49, c('Week', 'weekOfYear')]
data[52 + 47:49, c('Week', 'weekOfYear')]

# Visualize
plot(data$revenue, type = 'l')
lines(80000 + 100 * weekOfYear, col = 4)
abline(v = which(weekOfYear == 1), col = 2) # first week of each yr


# Add potentially useful transformations of the variables 
# (See nuggetAnalysis.R for explanation)
data$fbengage3 <- data$fbengage^3
data$logSocialpostlag <- (data$socialpostlag)
data$organicnew2 <- data$organicnew^2
data$logOrganicnew <- log(data$organicnew)
data$blognewusers2 <- data$blognewusers^2
data$blognewusersRecip <- 1 / data$blognewusers
data$Fbpostimplag2 <- data$Fbpostimplag^2
data$weekOfYear2 <- data$weekOfYear^2
data$sinWeekOfYear <- sin(pi * (data$weekOfYear + 13) / 26)
data$Twpostimp2 <- data$Twpostimp^2
data$logTwpostimp <- log(data$Twpostimp)

# Store cleaned data
write.csv(data, '~/Desktop/cbtnuggetsbusinessanalyticsproject/dataClean.csv')

# For "outliers" that may be legitimate data (i.e., not input error), we will 
# want to test models both with the data (assuming that they are correct), and 
# with them removed (assuming they are errors), to compare which performs 
# better.
# Replace potential outliers with NA and save as a separate file

# Potential outliers were in:
# displayimp, displaylag, brandpaid, socialpostimp, Fblikes*, Fbengage* 
# *possibly just very skewed data
hist(data$displayimp)
hist(data$displayimp[data$displayimp < 1e+08])
data$displayimp[data$displayimp > 1e+08] <- NA

hist(data$displaylag)
hist(data$displaylag[data$displaylag < 1e+08])
data$displaylag[data$displaylag > 1e+08] <- NA

hist(data$brandpaid)
hist(data$brandpaid[data$brandpaid < 150000])
data$brandpaid[data$brandpaid > 150000] <- NA

hist(data$socialpostimp)
hist(data$socialpostimp[data$socialpostimp < 5e+6])
data$socialpostimp[data$socialpostimp > 5e+6] <- NA

# Given the distributions, the following are more likely to be real extreme 
# values, and not input errors.  Leave alone for now.
hist(data$Fblikes)
plot(data$Fblikes)
hist(data$Fbengage)
plot(data$Fbengage)

write.csv(
  data, 
  '~/Desktop/cbtnuggetsbusinessanalyticsproject/dataCleanNoOutlier.csv')








# Further Exploration

# Explore relationships between predictors and response
par(mfrow = c(3, 3))
for (i in 2:dim(data)[2]) {
  plot(data[, 1] ~ data[, i], xlab = names(data)[i], ylab = 'revenue')
  lines(lowess(data[, 1][!is.na(data[, i])] ~ data[, i][!is.na(data[, i])]),
        col = 2)
  if ((i - 1) %% 9 == 0 ) { pause() }
}

# Potentially significant nonlinear trends in, most of which can be
# approximated  with a quadratic term:
# brandpaid, fbposts, Twpostimp, twengage, twengagelag, blogsignup
