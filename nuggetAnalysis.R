#=============================================================#
#                                                             #
# Analyze the CBT Nuggets Test Data Set                       #
# @author: Damian Satterthwaite-Phillips <damiansp@gmail.com> #
# @version: 21 Sep 2016                                       #
#                                                             #
#=============================================================#

# Clear workspace
rm(list = ls())

# Load libraries (assumes they have been installed)
library(mgcv) # for gam() 


data <- read.csv('~/Desktop/cbtnuggetsbusinessanalyticsproject/dataClean.csv')
# Outliers replaced with NAs
data2 <- read.csv(
  '~/Desktop/cbtnuggetsbusinessanalyticsproject/dataCleanNoOutlier.csv')
  
# Seed random number generator
set.seed(11)


# Convert binary data to categorical 
data$highseason   <- as.factor(data$highseason)
data$holiday      <- as.factor(data$holiday)
data$autoenroll   <- as.factor(data$autoenroll)
data$socialperson <- as.factor(data$socialperson)
data$promoteposts <- as.factor(data$promoteposts)
data$learner      <- as.factor(data$learner)

# Convert binary data to categorical 
data2$highseason   <- as.factor(data2$highseason)
data2$holiday      <- as.factor(data2$holiday)
data2$autoenroll   <- as.factor(data2$autoenroll)
data2$socialperson <- as.factor(data2$socialperson)
data2$promoteposts <- as.factor(data2$promoteposts)
data2$learner      <- as.factor(data2$learner)

# Convert 'Week' to Date type
data$Week <- as.Date(data$Week, format = '%m/%d/%y')
data2$Week <- as.Date(data2$Week, format = '%m/%d/%y')


# Scale numerical data; DO NOT SCALE RESPONSE
for (i in 2:ncol(data)) {
  if (class(data[, i]) != 'factor') {
    data[, i] <- scale(data[, i])
    data2[, i] <- scale(data[, i])
  }
}

# Split data into train and test sets
# Put 29 rows (~25%) aside in test set
test.inds <- sort(sample(1:nrow(data), size = 29))

test <- data[test.inds, ]
train <- data[-test.inds, ]
test2 <- data2[test.inds, ]
train2 <- data2[-test.inds, ]

# In the test sets, in order to be able to make predictions if there are NAs,
# replace NA's with mean values (0 since scaled)
test[is.na(test)] <- 0
test2[is.na(test2)] <- 0

# Just as a baseline, do a simple linear model and check its predictions on the 
# test set as a "score-to-beat"
# (Ignore data2/test2/train2 for now)
mod.lm1 <- lm(revenue ~ ., data = train) 
summary(mod.lm1)

# Remove linearly dependent variables, in order to be able to use predict
mod.lm1 <- update(mod.lm1, . ~ . - weekno - Fbpageimp - Twpostimp)
mod.lm1.p <- predict(mod.lm1, newdata = test)
cbind(mod.lm1.p, test$revenue)
# Metric for quantifying fit (sum of squared error)
sse <- function(preds, actual) {
  return (sum((preds - actual)^2))
}

(mod.lm1.sse <- sse(mod.lm1.p, test$revenue))

# Track current best model, and sse of current best
currentBest <- list(mod = 'mod.lm1', sse = mod.lm1.sse)

# Reduce model to prevent overfitting
# NOTE: here the step function will not complete due to NAs changing the 
# available amount of data.  Model was updated at each iteration then update
# was run again.  Strictly speaking, AICs are not directly comparable in this 
# way because the number of underlying data going into the computation changes,
# but at this point the goal is simply to thin out the model predictor space
mod.lm2 <- step(mod.lm1, direction = 'both')
mod.lm2 <- update(mod.lm1, . ~ . - socialpostimp - highseason - displayimp)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - emailopen)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - organicnew)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - blogrefer)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - fbposts)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - Fbpostimp - socialpostlag - Fbpostimplag)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - rev2weekAgo - youtubelag)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - blogvisits)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - rev1weekAgo - weekOfYear - youtube)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - displaylag - socialimplag)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - brandclicks)
summary(mod.lm2)
mod.lm2.p <- predict(mod.lm2, newdata = test)
(mod.lm2.sse <- sse(mod.lm2.p, test$revenue))

# Check against current best
currentBest$sse

# Much better already... Replace
currentBest <- list(mod = 'mod.lm2', sse = mod.lm2.sse)


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
iterations <- 10
# Do not include binary variables at this point

predictors <- names(train)[-1]


binVars <- c(highseason, holiday, autoenroll, socialperson, promoteposts, learner)

# Exclude categorical variables for now
predictors <- predictors[-which(predictors %in% catVars)]
# Also exclude Week, weekno from predictors here, as these will always be unique
# values (never repeated), so we don't want extrapolate 
predictors <- predictors[-c(1, 2)]

np <- 7
iterations <- 100

# Keep track of the best 3 models
bestGCV <- c(Inf, Inf, Inf)
bestMods <-  c('', '', '')

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

bestMod1 <- gam(as.formula(bestMods[1]), data = train)
bestMod2 <- gam(as.formula(bestMods[2]), data = train)
bestMod3 <- gam(as.formula(bestMods[3]), data = train)
summary(bestMod1)
summary(bestMod2)
summary(bestMod3)