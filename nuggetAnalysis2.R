#=============================================================#
#                                                             #
# Analyze the CBT Nuggets Test Data Set - Outliers removed    #
# @author: Damian Satterthwaite-Phillips <damiansp@gmail.com> #
# @version: 23 Sep 2016                                       #
#                                                             #
#=============================================================#

# Clear workspace
rm(list = ls())

# Load libraries (assumes they have been installed)
library(mgcv) # for gam() 
library(nlme) # for gls()

# Load any data stored from previous session
load('~/Desktop/cbtnuggetsbusinessanalyticsproject/nuggets2.RData')

# Outliers replaced with NAs
data2 <- read.csv(
  '~/Desktop/cbtnuggetsbusinessanalyticsproject/dataCleanNoOutlier.csv')
  
# Seed random number generator
set.seed(11)


# Convert binary data to categorical 
data2$highseason   <- as.factor(data2$highseason)
data2$holiday      <- as.factor(data2$holiday)
data2$autoenroll   <- as.factor(data2$autoenroll)
data2$socialperson <- as.factor(data2$socialperson)
data2$promoteposts <- as.factor(data2$promoteposts)
data2$learner      <- as.factor(data2$learner)

# Convert 'Week' to Date type
data2$Week <- as.Date(data2$Week, format = '%m/%d/%y')


# Scale numerical data; DO NOT SCALE RESPONSE
# Rescale so all values are on the interval [0, 1]
rescale <- function(x) {
	x <- x - min(x, na.rm = T)
	x <- x / max(x, na.rm = T)  
	return (x)
}

for (i in 3:ncol(data2)) {
  if (class(data2[, i]) != 'factor') {
    data2[, i] <- rescale(data2[, i])
  }
}

# Split data into train and test sets
# Put 29 rows (~25%) aside in test set
test2.inds <- sort(sample(1:nrow(data2), size = 29))

# In the test set, in order to be able to make predictions if there are NAs,
# replace NA's with values; Since these are time series with clear temporal 
# autocorrelation, simplify by filling with the data from the previous date
for (j in 2:ncol(data2)) {
  for (i in 1:nrow(data2)) {
  	if (i %in% test2.inds & is.na(data2[i, j])) {
  	  if (i == 1) {
  	  	# first row has no prior date; use following
  	  	data2[i, j] <- data2[i + 1, j]
  	  } else {
  	    data2[i, j] <- data2[i - 1, j]
  	  }
  	} 	
  }
}

test2 <- data2[test2.inds, ]
train2 <- data2[-test2.inds, ]

# Fill remaining NAs
test2$rev2weekAgo[1:2] <- test2$rev2weekAgo[3]
test2$rev3weekAgo[1:2] <- test2$rev3weekAgo[3]
test2$rev4weekAgo[1:2] <- test2$rev4weekAgo[3]

# Just as a baseline, do a simple linear model and check its predictions on the 
# test2 set as a "score-to-beat"
# (Ignore data2/test22/train22 for now)
mod.lm1 <- lm(revenue ~ ., data = train2) 
summary(mod.lm1)

# Remove linearly dependent variables, in order to be able to use predict
mod.lm1 <- update(mod.lm1, 
                  . ~ . - weekno - Fbpageimp - Twpostimp - logSocialpostlag - 
                  logOrganicnew - blognewusers2 - blognewusersRecip - Fbpostimplag2 
                  - weekOfYear2 - sinWeekOfYear - Twpostimp2 - logTwpostimp)
summary(mod.lm1)
mod.lm1.p <- predict(mod.lm1, newdata = test2)

# Compare predictions to actual values
cbind(mod.lm1.p, test2$revenue)

# Metric for quantifying fit (sum of squared error)
sse <- function(preds, actual) {
  return (sum((preds - actual)^2))
}

(mod.lm1.sse <- sse(mod.lm1.p, test2$revenue)) # 2.026e+12

# To think about this number in terms of our response variable, we can calculate the
# average error:
sqrt(mod.lm1.sse / nrow(test2))
mean(data2$revenue)

# i.e., the average error in the predictions is $264,320; more than the mean 
# value we are trying to predict.  Obviously a poor fit.

# Track current best model, and sse of current best
currentBest <- list(mod = 'mod.lm1', sse = mod.lm1.sse)

# Reduce model to prevent overfitting
# NOTE: here the step function will not complete due to NAs changing the 
# available amount of data.  Model was updated at each iteration then update
# was run again.  Strictly speaking, AICs are not directly comparable in this 
# way because the number of underlying data going into the computation changes,
# but at this point the goal is simply to thin out the model predictor space
mod.lm2 <- step(mod.lm1, direction = 'both')
mod.lm2 <- update(mod.lm1, . ~ . - brandpaid)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - socialpostimp - socialperson)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - emailclick - nonbrandclicks)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - learner - socialimp)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, 
                  . ~ . - rev2weekAgo - rev3weekAgo - brandclicks)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - displayimp)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - Fblikes - rev4weekAgo)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - blognewusers)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - emailsent)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - twengage - fbposts)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - nonbrandpaid)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - blogvisits)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - weekOfYear - twposts - organicnew - emailopen)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - displaylag)
mod.lm2 <- step(mod.lm2, direction = 'both')
summary(mod.lm2)
mod.lm2.p <- predict(mod.lm2, newdata = test2)
(mod.lm2.sse <- sse(mod.lm2.p, test2$revenue)) # 9.4850e+9
sqrt(mod.lm2.sse / nrow(test2))

# Check against current best
currentBest$sse

# Much better already... Replace
currentBest <- list(mod = 'mod.lm2', sse = mod.lm2.sse)

# Add in the variables that could not be explored in the initial model
mod.lm3 <- update(mod.lm2, 
                  . ~ . + weekno + Fbpageimp + Twpostimp + logSocialpostlag + 
                  logOrganicnew + blognewusers2 + blognewusersRecip + Fbpostimplag2 
                  + weekOfYear2 + sinWeekOfYear + Twpostimp2 + logTwpostimp)
# And reduce again
mod.lm3 <- step(mod.lm3, direction = 'both')
mod.lm3 <- update(mod.lm3, 
                  . ~ . - Fbpostimplag2 - blognewusers2 - logTwpostimp - 
                  logOrganicnew - blogrefernew)
mod.lm3 <- step(mod.lm3, direction = 'both')
summary(mod.lm3)

mod.lm3.p <- predict(mod.lm3, newdata = test2)
(mod.lm3.sse <- sse(mod.lm3.p, test2$revenue)) # 8.8241e+9
sqrt(mod.lm3.sse / nrow(test2))
currentBest$sse  # Improved again
currentBest <- list(mod = 'mod.lm3', sse = mod.lm3.sse)

# Explore potential interaction terms in a similar manner.  It is generally - though 
# not always - the case that when interaction terms are significant, so are the main
# effects.  In creating random models we initially require that if the x:y 
# interaction term is present, so are x and y
predictors <- names(train2)[-1]

# Do not include binary variables at this point
binVars <- c(
  'highseason', 'holiday', 'autoenroll', 'socialperson', 'promoteposts', 'learner')
predictors <- predictors[-which(predictors %in% binVars)]

# Also exclude Week, weekno from predictors here, as these will always be unique
# values (never repeated), so we don't want extrapolate 
predictors <- predictors[-c(1, 2)]

#bestAIC <- c(Inf, Inf, Inf)
#bestInteractionMods <-  c('', '', '')
iterations <- 1000

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

  mod <- lm(as.formula(form), data = train2)
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


bestInteractionMod3 <- lm(as.formula(bestInteractionMods[3]), data = train2)
bestInteractionMod2 <- lm(as.formula(bestInteractionMods[2]), data = train2)
bestInteractionMod1 <- lm(as.formula(bestInteractionMods[1]), data = train2)

# Reduce
bestInteractionMod3 <- step(bestInteractionMod3)
bestInteractionMod2 <- step(bestInteractionMod2)
bestInteractionMod1 <- step(bestInteractionMod1)
bestInteractionMod1 <- update(
  bestInteractionMod1, 
  . ~ . - socialpostlag:emailclick - nonbrandclicks:fbposts -
  displayimp:logSocialpostlag - fbposts)
bestInteractionMod1 <- step(bestInteractionMod1)
bestInteractionMod1 <- update(
  bestInteractionMod1, 
  . ~ . - rev2weekAgo:socialimp - Fbpostimp:youtubelag - Fbpostimp:nonbrandclicks - 
  nonbrandclicks)
bestInteractionMod1 <- step(bestInteractionMod1)

summary(bestInteractionMod3)
bestInteractionMod3 <- update(bestInteractionMod3, . ~ . - Fbpostimp)
summary(bestInteractionMod3)
summary(bestInteractionMod2)
summary(bestInteractionMod1)

bestInteractionMod3.p <- predict(bestInteractionMod3, test2)
bestInteractionMod2.p <- predict(bestInteractionMod2, test2)
bestInteractionMod1.p <- predict(bestInteractionMod1, test2)
(bestInteractionMod3.sse <- sse(bestInteractionMod3.p, test2$revenue)) # 1.2583e+11
(bestInteractionMod2.sse <- sse(bestInteractionMod2.p, test2$revenue)) # 9.0118e+10
(bestInteractionMod1.sse <- sse(bestInteractionMod1.p, test2$revenue)) # 6.4023e+11
currentBest$sse  # NO improvement

# The current best model, mod.lm3, has an average error of 
(sqrt(mod.lm3.sse / nrow(test2)))
# $17,444 or about 12% of the mean revenue, less than the standard deviation of 
sd(data2$revenue)
# $23,944 but we can do better.  At this point though, it is reasonably clear that 
# removing the outliers is improving the fit, so work will continue using this data
# set rather than the one with the outliers left in.

# Try ridge and lasso
# Cannot handle categorical data; change binaries to numeric
train2num <- train2
test2num <- test2
for (b in binVars) {
  train2num[, b] <- as.numeric(as.character(train2num[, b]))
  test2num[, b] <- as.numeric(as.character(test2num[, b]))
}

train2num[, 'Week'] <- as.numeric(train2num[, 'Week'])
test2num[, 'Week'] <- as.numeric(test2num[, 'Week'])


# Model as ridge regression: All predictors including all 2-way interactions
lambdas <- c(0, 10^(0:5))
mod.ridge1 <- lm.ridge(revenue ~ .^2, lambda = lambdas, data = train2num)
plot(mod.ridge1$GCV ~ lambdas, type = 'l')

# update lambdas to finer resolution around pest GCV
lambdas <- seq(0, 1000, length = 10)
mod.ridge2 <- lm.ridge(revenue ~ .^2, lambda = lambdas, data = train2num)
plot(mod.ridge2$GCV ~ lambdas, type = 'l')

lambdas <- seq(400, 600, length = 10)
mod.ridge3 <- lm.ridge(revenue ~ .^2, lambda = lambdas, data = train2num)
plot(mod.ridge3$GCV ~ lambdas, type = 'l')


# construct df equivalent to model matrix used in mod.ridge3
test2numInter <- test2num

for (i in 2:(ncol(test2num) - 1)) {
  for (j in (i + 1):ncol(test2num)) {
  	test2numInter <- cbind(test2numInter, test2num[, i] * test2num[, j])
  	names(test2numInter)[length(names(test2numInter))] <- paste(
  	  names(test2num)[i], names(test2num)[j], sep = ':')
  }
}

testScaled <- scale(
  test2numInter[, -1], center = mod.ridge3$xm, scale = mod.ridge3$scales)

mod.ridge3.p <- testScaled %*% mod.ridge3$coef[, which.min(mod.ridge3$GCV)] + 
  mod.ridge3$ym
(mod.ridge3.sse <- sse(mod.ridge3.p, test2$revenue))  # 1.0024e+10
currentBest$sse  # Previous best still better



# Examine current best
currentBest
par(mfrow = c(2, 2))
plot(mod.lm3)

# Reasonably good diagnostics, though pt 116 clearly has disproportionate influence.
# Examine:
train2['116', ]
which(rownames(train2) == '116')

par(mfrow = c(3, 3))
for (i in 1:ncol(train2)) {
  plot(train2[, i], main = names(train2)[i])
  points(88, train2[88, i], pch = 16, col = 2)
  if (i %% 9 == 0) { pause() }
}

# This record has an unusually high value for both Fblikes and Fbengage
# We can try transforming these predictor, or potentially removing these outliers
hist(data2$Fblikes)
sort(scale(data2$Fblikes))
hist(data2$Fbengage)
sort(scale(data2$Fbengage))
# ....continued in nuggetAnalysis3.R



save.image('~/Desktop/cbtnuggetsbusinessanalyticsproject/nuggets2.RData')