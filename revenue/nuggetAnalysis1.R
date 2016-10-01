#=============================================================#
#                                                             #
# Analyze the CBT Nuggets Test Data Set - "Outliers" in place #
# @author: Damian Satterthwaite-Phillips <damiansp@gmail.com> #
# @version: 21 Sep 2016                                       #
#                                                             #
#=============================================================#

# Clear workspace
rm(list = ls())

# Load libraries (assumes they have been installed)
library(mgcv) # for gam() 

# Load any data stored from previous session
load('~/Desktop/cbtnuggetsbusinessanalyticsproject/nuggets.RData')

data <- read.csv('~/Desktop/cbtnuggetsbusinessanalyticsproject/dataClean.csv')
  
# Seed random number generator
set.seed(11)


# Convert binary data to categorical 
data$highseason   <- as.factor(data$highseason)
data$holiday      <- as.factor(data$holiday)
data$autoenroll   <- as.factor(data$autoenroll)
data$socialperson <- as.factor(data$socialperson)
data$promoteposts <- as.factor(data$promoteposts)
data$learner      <- as.factor(data$learner)

# Convert 'Week' to Date type
data$Week <- as.Date(data$Week, format = '%m/%d/%y')


# Scale numerical data; DO NOT SCALE RESPONSE
# Rescale so all values are on the interval [0, 1]
rescale <- function(x) {
	x <- x - min(x, na.rm = T)
	x <- x / max(x, na.rm = T)  
	return (x)
}

for (i in 3:ncol(data)) {
  if (class(data[, i]) != 'factor') {
    data[, i] <- rescale(data[, i])
  }
}

# Split data into train and test sets
# Put 29 rows (~25%) aside in test set
test.inds <- sort(sample(1:nrow(data), size = 29))

# In the test set, in order to be able to make predictions if there are NAs,
# replace NA's with values; Since these are time series with clear temporal 
# autocorrelation, simplify by filling with the data from the previous date
for (j in 2:ncol(data)) {
  for (i in 1:nrow(data)) {
  	if (i %in% test.inds & is.na(data[i, j])) {
  	  if (i == 1) {
  	  	# first row has no prior date; use following
  	  	data[i, j] <- data[i + 1, j]
  	  } else {
  	    data[i, j] <- data[i - 1, j]
  	  }
  	} 	
  }
}


test <- data[test.inds, ]
train <- data[-test.inds, ]

# Fill remaining NAs
test$rev2weekAgo[1:2] <- test$rev2weekAgo[3]
test$rev3weekAgo[1:2] <- test$rev3weekAgo[3]
test$rev4weekAgo[1:2] <- test$rev4weekAgo[3]

# Just as a baseline, do a simple linear model and check its predictions on the 
# test set as a "score-to-beat"
# (Ignore data2/test2/train2 for now)
mod.lm1 <- lm(revenue ~ ., data = train) 
summary(mod.lm1)

# Remove linearly dependent variables, in order to be able to use predict
mod.lm1 <- update(mod.lm1, 
                  . ~ . - weekno - Fbpageimp - Twpostimp - logSocialpostlag - 
                  blognewusersRecip - Fbpostimplag2 - weekOfYear2 - sinWeekOfYear - 
                  Twpostimp2 - logTwpostimp)
summary(mod.lm1)
mod.lm1.p <- predict(mod.lm1, newdata = test)

# Compare predictions to actual values
cbind(mod.lm1.p, test$revenue)

# Metric for quantifying fit (sum of squared error)
sse <- function(preds, actual) {
  return (sum((preds - actual)^2))
}

(mod.lm1.sse <- sse(mod.lm1.p, test$revenue)) # 428381890439  (4.2828e+11)

# To think about this number in terms of our response variable, we can calculate the
# average error:
sqrt(mod.lm1.sse / nrow(test))
mean(data$revenue)

# i.e., the average error in the predictions is $121,539.30; just less than the mean 
# value we are trying to predict.  Not a great fit.

# Track current best model, and sse of current best
currentBest <- list(mod = 'mod.lm1', sse = mod.lm1.sse)

# Reduce model to prevent overfitting
# NOTE: here the step function will not complete due to NAs changing the 
# available amount of data.  Model was updated at each iteration then update
# was run again.  Strictly speaking, AICs are not directly comparable in this 
# way because the number of underlying data going into the computation changes,
# but at this point the goal is simply to thin out the model predictor space
mod.lm2 <- step(mod.lm1, direction = 'both')
mod.lm2 <- update(mod.lm1, . ~ . - rev1weekAgo - learner - rev4weekAgo)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, 
                  . ~ . - fbengagelag - weekOfYear - blognewusers - rev2weekAgo - 
                  youtubelag)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - Week - fbposts)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - Fbpostimp - brandclicks)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, 
                  . ~ . - displaylag - emailclick - organicnew2 - blogrefernew)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - autoenroll - twengagelag)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - twengage - blognewusers2)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - socialpostimp - twposts - displayimp)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - organicnew - brandpaid)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - promoteposts)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - nonbrandclicks)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - rev3weekAgo - emailsent)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - emailopen)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - socialimp)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - Fbengage - nonbrandpaid)
mod.lm2 <- step(mod.lm2, direction = 'both')
summary(mod.lm2)
mod.lm2.p <- predict(mod.lm2, newdata = test)
(mod.lm2.sse <- sse(mod.lm2.p, test$revenue)) # 13722360108 (1.3722e+10)
sqrt(mod.lm2.sse / nrow(test))

# Check against current best
currentBest$sse

# Much better already... Replace
currentBest <- list(mod = 'mod.lm2', sse = mod.lm2.sse)

# Add in the variables that could not be explored in the initial model
mod.lm3 <- update(mod.lm2, 
                  . ~ . + weekno + Fbpageimp + Twpostimp + logSocialpostlag + 
                  blognewusersRecip + Fbpostimplag2 + weekOfYear2 + sinWeekOfYear + 
                  Twpostimp2 + logTwpostimp)
# And reduce again
mod.lm3 <- step(mod.lm3, direction = 'both')
mod.lm3 <- update(mod.lm3, . ~ . - Fbpageimp - weekno - logTwpostimp - blogvisits)
mod.lm3 <- step(mod.lm3, direction = 'both')
mod.lm3 <- update(mod.lm3, 
                  . ~ . - sinWeekOfYear - Fbpostimplag2 - weekOfYear2 - fbEngage3)
mod.lm3 <- step(mod.lm3, direction = 'both')
summary(mod.lm3)

mod.lm3.p <- predict(mod.lm3, newdata = test)
(mod.lm3.sse <- sse(mod.lm3.p, test$revenue)) # 1.3464e+10
currentBest$sse  # A slight improvement
currentBest <- list(mod = 'mod.lm3', sse = mod.lm3.sse)

# Explore possible non-linear relationships between predictors and response

# Approach: 
# Model as additive model; because additive models consume a large number of 
# degrees of freedom, with a data set this size, it is not possible to model
# all predictors at once.  We can madel about 7 or 8 smoothed predictors with the 
# df available, e.g.:
mod.gam1 <- gam(revenue ~ s(Week) + s(weekno) + s(displayimp) + s(displaylag) +
                s(brandpaid) + s(brandclicks) + s(nonbrandpaid) + 
                s(nonbrandclicks), data = train)
summary(mod.gam1)

# Perform a random search for the best model using smooths of 8 predictors
predictors <- names(train)[-1]

# Do not include binary variables at this point
binVars <- c(
  'highseason', 'holiday', 'autoenroll', 'socialperson', 'promoteposts', 'learner')
predictors <- predictors[-which(predictors %in% binVars)]

# Also exclude Week, weekno from predictors here, as these will always be unique
# values (never repeated), so we don't want extrapolate 
predictors <- predictors[-c(1, 2)]

np <- 7
iterations <- 100 # reduced here to knit file, actually ran several thousand

# Keep track of the best 3 models
# Uncomment the following 2 lines for first run only
#bestGCV <- c(Inf, Inf, Inf)
#bestMods <-  c('', '', '')


for (i in 1:iterations) {
  sampP <- sample(predictors, np)
  form <- 'revenue ~'

  for (p in sampP) { 
    form <- paste(form, ' + s(', p, ')', sep = '')
  }

  mod <- gam(as.formula(form), data = train)

  if (mod$gcv.ubre < bestGCV[3]) {
  	if (mod$gcv.ubre < bestGCV[2]) {
      if (mod$gcv.ubre < bestGCV[1]) {
  	    # insert in first place, bump others
  	    bestGCV[3] <- bestGCV[2]
  	    bestGCV[2] <- bestGCV[1]
  	    bestGCV[1] <- mod$gcv.ubre
  	    bestMods[3] <- bestMods[2]
  	    bestMods[2] <- bestMods[1]
  	    bestMods[1] <- form
  	  } else { 
  	    # insert in second place, bump other
  	    bestGCV[3] <- bestGCV[2]
  	    bestGCV[2] <- mod$gcv.ubre
  	    bestMods[3] <- bestMods[2]
  	    bestMods[2] <- form
  	  } 
  	} else {
  	  # insert in third place; no bumping
  	  bestGCV[3] <- mod$gcv.ubre
  	  bestMods[3] <- form
  	}
  }
}

bestGCV

bestMod3 <- gam(as.formula(bestMods[3]), data = train)
bestMod2 <- gam(as.formula(bestMods[2]), data = train)
bestMod1 <- gam(as.formula(bestMods[1]), data = train)
summary(bestMod3)
par(mfrow = c(3, 3))
par(mar = c(4, 3, 0, 0))
plot(bestMod3)

summary(bestMod2)
quartz()
par(mfrow = c(3, 3))
par(mar = c(4, 3, 0, 0))
plot(bestMod2)


summary(bestMod1)
quartz()
par(mfrow = c(3, 3))
par(mar = c(4, 3, 0, 0))
plot(bestMod1)

# If we look at the predictors found to be significant in these top models, a few 
# non-linear relationships are suggested
# • fbengage: approximately cubic
# • socialpostlag: early increase then basically flat
# • organicnew: roughly quadratic or log-like
# • blognewusers: roughly quadratic or reciprocal (though it seems unlikely that an 
#   increase in new blog users would lead to a decrease in revenue)
# • Fbpostimp:  This looks suspiciously overfitted, and there are not a lot of data 
#   points contributing to the area of high curvature.  At the left end of the 
#   graph, where most of the data lie, there is a roughly quadratic fit, but this 
#   does not fit the remaining data points.
# • Fbpostimplag: roughly quadratic
# • weekOfYear: as mentioned when exploring the temporal correlation (in 
#   nuggetPreprocess.R), these models also suggest that revenue is lowest at mid 
#   year, and highest in the winter; could be approximated with a quadratic or 
#   perhaps sinusoid
# • Twpostimp: quadratic or log

# Construct new variables:
# • fbengage^3 • socialpostlag / (socialpostlag + 1) • organicnew^2 • log(organicnew)
# • blognewusers^2 • 1 / blognewusers • Fbpostimplag^2 • weekOfYear^2
# • sin(pi(weekOfYear + 13) / 26) • Twpostimp^2 • log(Twpostimp)
# **** Done in nuggetsPreprocess.R ****


# Explore potential interaction terms in a similar manner.  It is generally - though 
# not always - the case that when interaction terms are significant, so are the main
# effects.  In creating random models we initially require that if the x:y 
# interaction term is present, so are x and y
dim(train)

#bestAIC <- c(Inf, Inf, Inf)
#bestInteractionMods <-  c('', '', '')
iterations <- 1000000

for (i in 1:iterations) {
  sampP <- c()
  dfRemaining <- sample(50, 1)

  while (dfRemaining > 0) { 
  	preds <- sort(sample(predictors, 2))
  	preds[3] <- paste(preds[1], preds[2], sep = ':')
  	
  	for (p in preds) {
  	  if (!(p %in% sampP)) {
  	  	sampP <- c(sampP, p)
  	  	dfRemaining <- dfRemaining - 1	
  	  }	
  	}
  }
  
  if (length(sampP) > 50) {
  	sampP <- sampP[-c(50:length(sampP))]
  }

  form <- 'revenue ~'

  for (p in sampP) { 
    form <- paste(form, p, sep = ' + ')
  }

  mod <- lm(as.formula(form), data = train)
  summary(mod)
  aic <- extractAIC(mod)[2]
  
  if (aic == -Inf) { 
  	# bad model
  	continue 
  } else {
    if (aic < bestAIC[3]) {
  	  if (aic < bestAIC[2]) {
        if (aic < bestAIC[1]) {
  	      # insert in first place, bump others
  	      bestAIC[3] <- bestAIC[2]
  	      bestAIC[2] <- bestAIC[1]
  	      bestAIC[1] <- aic
  	      bestInteractionMods[3] <- bestInteractionMods[2]
  	      bestInteractionMods[2] <- bestInteractionMods[1]
  	      bestInteractionMods[1] <- form
  	    } else { 
  	      # insert in second place, bump other
  	      bestAIC[3] <- bestAIC[2]
  	      bestAIC[2] <- aic
  	      bestInteractionMods[3] <- bestInteractionMods[2]
  	      bestInteractionMods[2] <- form
  	    } 
  	  } else {
  	    # insert in third place; no bumping
  	    bestAIC[3] <- aic
  	    bestInteractionMods[3] <- form
  	  }
    }
  }
}

bestAIC
bestInteractionMods


bestInteractionMod3 <- lm(as.formula(bestInteractionMods[3]), data = train)
bestInteractionMod2 <- lm(as.formula(bestInteractionMods[2]), data = train)
bestInteractionMod1 <- lm(as.formula(bestInteractionMods[1]), data = train)

# Reduce
bestInteractionMod3 <- step(bestInteractionMod3)
bestInteractionMod2 <- step(bestInteractionMod2)
bestInteractionMod1 <- step(bestInteractionMod1)

summary(bestInteractionMod3)
summary(bestInteractionMod2)
summary(bestInteractionMod1)

bestInteractionMod3.p <- predict(bestInteractionMod3, test)
bestInteractionMod2.p <- predict(bestInteractionMod2, test)
bestInteractionMod1.p <- predict(bestInteractionMod1, test)
(bestInteractionMod3.sse <- sse(bestInteractionMod3.p, test$revenue)) # 3.8471e+11
(bestInteractionMod2.sse <- sse(bestInteractionMod2.p, test$revenue)) # 2.2417e+12
(bestInteractionMod1.sse <- sse(bestInteractionMod1.p, test$revenue)) # 1.0008e+12
currentBest$sse  # NO improvement                                     # 1.3464e+10

# At this point, there is a good indication that this relatively simplistic approach is not working well.  The best interaction model above (based on AIC) has a p value of 0.001, and and adjusted R-squared value of 0.97, indicating that the model fits the training data extraordinarily well, but it is not generalizing to good predictions on the test data, suggesting overfitting.  It is also probable that outliers and the temporal correlation are contributing to the overfitting.

# Pause here to repeat some of these initial tests with data set 2 (outliers replaced with NAs), to determine if there are considerable differences so far.















save.image('~/Desktop/cbtnuggetsbusinessanalyticsproject/nuggets.RData')