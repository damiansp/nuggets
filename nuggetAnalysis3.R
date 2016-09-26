#=============================================================#
#                                                             #
# Analyze the CBT Nuggets Test Data Set -                     #
#     Even more outliers removed                              #
# @author: Damian Satterthwaite-Phillips <damiansp@gmail.com> #
# @version: 24 Sep 2016                                       #
#                                                             #
#=============================================================#

# Clear workspace
rm(list = ls())

# Load libraries (assumes they have been installed)
library(mgcv) # for gam() 

# Load any data stored from previous session
load('~/Desktop/cbtnuggetsbusinessanalyticsproject/nuggets3.RData')


data3 <- read.csv(
  '~/Desktop/cbtnuggetsbusinessanalyticsproject/dataCleanNoOutlier.csv')
  
# Seed random number generator
set.seed(11)

# Convert binary data to categorical 
data3$highseason   <- as.factor(data3$highseason)
data3$holiday      <- as.factor(data3$holiday)
data3$autoenroll   <- as.factor(data3$autoenroll)
data3$socialperson <- as.factor(data3$socialperson)
data3$promoteposts <- as.factor(data3$promoteposts)
data3$learner      <- as.factor(data3$learner)

# Convert 'Week' to Date type
data3$Week <- as.Date(data3$Week, format = '%m/%d/%y')

# Removal of additional outliers
sort(data3$Fblikes)
data3$Fblikes[data3$Fblikes > 1000] <- NA
sort(data3$Fbengage)
data3$Fbengage[data3$Fbengage > 17000] <- NA

# Scale numerical data; DO NOT SCALE RESPONSE
# Rescale so all values are on the interval [0, 1]
rescale <- function(x) {
	x <- x - min(x, na.rm = T)
	x <- x / max(x, na.rm = T)  
	return (x)
}

for (i in 3:ncol(data3)) {
  if (class(data3[, i]) != 'factor') {
    data3[, i] <- rescale(data3[, i])
  }
}

# Split data into train and test sets
# Put 29 rows (~25%) aside in test set
test3.inds <- sort(sample(1:nrow(data3), size = 29))

# In the test set, in order to be able to make predictions if there are NAs,
# replace NA's with values; Since these are time series with clear temporal 
# autocorrelation, simplify by filling with the data from the previous date
for (j in 2:ncol(data3)) {
  for (i in 1:nrow(data3)) {
  	if (i %in% test3.inds & is.na(data3[i, j])) {
  	  if (i == 1) {
  	  	# first row has no prior date; use following
  	  	data3[i, j] <- data3[i + 1, j]
  	  } else {
  	    data3[i, j] <- data3[i - 1, j]
  	  }
  	} 	
  }
}

test3 <- data3[test3.inds, ]
train3 <- data3[-test3.inds, ]

# Fill remaining NAs
test3$rev2weekAgo[1:2] <- test3$rev2weekAgo[3]
test3$rev3weekAgo[1:2] <- test3$rev3weekAgo[3]
test3$rev4weekAgo[1:2] <- test3$rev4weekAgo[3]

# Just as a baseline, do a simple linear model and check its predictions on the 
# test3 set as a "score-to-beat"
# (Ignore data3/test32/train32 for now)
mod.lm1 <- lm(revenue ~ ., data = train3) 
summary(mod.lm1)

# Remove linearly dependent variables, in order to be able to use predict
mod.lm1 <- update(mod.lm1, 
                  . ~ . - Week - weekno - Fbpageimp - Twpostimp - logSocialpostlag - 
                  logOrganicnew - blognewusers2 - blognewusersRecip - Fbpostimplag2 
                  - weekOfYear2 - sinWeekOfYear - Twpostimp2 - logTwpostimp)
summary(mod.lm1)
mod.lm1.p <- predict(mod.lm1, newdata = test3)

# Compare predictions to actual values
cbind(mod.lm1.p, test3$revenue)

# Metric for quantifying fit (sum of squared error)
sse <- function(preds, actual) {
  return (sum((preds - actual)^2))
}

(mod.lm1.sse <- sse(mod.lm1.p, test3$revenue)) # 5.3778e+11

# To think about this number in terms of our response variable, we can calculate the
# average error:
sqrt(mod.lm1.sse / nrow(test3))
mean(data3$revenue)

# i.e., the average error in the predictions is $136,176.30; just less than the mean 
# revenue value.  Obviously not a great fit.

# Track current best model, and sse of current best
currentBest <- list(mod = 'mod.lm1', sse = mod.lm1.sse)

# Reduce model to prevent overfitting
# NOTE: here the step function will not complete due to NAs changing the 
# available amount of data.  Model was updated at each iteration then update
# was run again.  Strictly speaking, AICs are not directly comparable in this 
# way because the number of underlying data going into the computation changes,
# but at this point the goal is simply to thin out the model predictor space
mod.lm2 <- step(mod.lm1, direction = 'both')
mod.lm2 <- update(
  mod.lm1, . ~ . - weekOfYear - holiday - organicnew2 - socialperson)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - nonbrandclicks)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - rev4weekAgo)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - emailclick - promoteposts)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - displayimp)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - rev3weekAgo - rev1weekAgo - blognewusers)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - Fbpostimp - fbposts)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - blogvisits)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - emailopen)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - learner - brandpaid)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - socialimplag)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - blogsignup - blogrefer)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - blogrefernew)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - Fbengage - Fblikes)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - nonbrandpaid - displaylag)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - rev2weekAgo - autoenroll - socialimp)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - youtubelag)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - youtube - socialpostimp)
mod.lm2 <- step(mod.lm2, direction = 'both')
mod.lm2 <- update(mod.lm2, . ~ . - twposts - emailsent)
mod.lm2 <- step(mod.lm2, direction = 'both')
summary(mod.lm2)
mod.lm2.p <- predict(mod.lm2, newdata = test3)
(mod.lm2.sse <- sse(mod.lm2.p, test3$revenue)) # 9.5910e+9
sqrt(mod.lm2.sse / nrow(test3))

# Check against current best
currentBest$sse

# Much better already... Replace
currentBest <- list(mod = 'mod.lm2', sse = mod.lm2.sse)

# Add in the variables that could not be explored in the initial model, and lagged
# revenue vars, since there is known to be autocorrelation
mod.lm3 <- update(mod.lm2, 
                  . ~ . + Week + weekno + Fbpageimp + Twpostimp + logSocialpostlag + 
                  logOrganicnew + blognewusers2 + blognewusersRecip + Fbpostimplag2 
                  + weekOfYear2 + sinWeekOfYear + Twpostimp2 + logTwpostimp + 
                  rev1weekAgo + rev2weekAgo + rev3weekAgo + rev4weekAgo)
# And reduce again
mod.lm3 <- step(mod.lm3, direction = 'both')
mod.lm3 <- update(mod.lm3, . ~ . - rev2weekAgo - weekOfYear2 - rev4weekAgo)
mod.lm3 <- step(mod.lm3, direction = 'both')
mod.lm3 <- update(mod.lm3, 
                  . ~ . - organicnew - rev1weekAgo - logTwpostimp - Fbpostimplag2 - 
                  rev3weekAgo)
mod.lm3 <- step(mod.lm3, direction = 'both')
summary(mod.lm3)

mod.lm3.p <- predict(mod.lm3, newdata = test3)
(mod.lm3.sse <- sse(mod.lm3.p, test3$revenue)) # 1.2028e+10
sqrt(mod.lm3.sse / nrow(test3))
currentBest$sse  # Previous is better. Do not update

# Explore potential interaction terms in a similar manner.  It is generally - though 
# not always - the case that when interaction terms are significant, so are the main
# effects.  In creating random models we initially require that if the x:y 
# interaction term is present, so are x and y
predictors <- names(train3)[-1]

# Do not include binary variables at this point
binVars <- c(
  'highseason', 'holiday', 'autoenroll', 'socialperson', 'promoteposts', 'learner')
predictors <- predictors[-which(predictors %in% binVars)]

# Also exclude Week, weekno from predictors here, as these will always be unique
# values (never repeated), so we don't want extrapolate 
predictors <- predictors[-c(1, 2)]

# Uncomment for first run
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

  mod <- lm(as.formula(form), data = train3)
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
#bestInteractionMods


bestInteractionMod3 <- lm(as.formula(bestInteractionMods[3]), data = train3)
bestInteractionMod2 <- lm(as.formula(bestInteractionMods[2]), data = train3)
bestInteractionMod1 <- lm(as.formula(bestInteractionMods[1]), data = train3)

# Reduce
bestInteractionMod3 <- step(bestInteractionMod3)
bestInteractionMod2 <- step(bestInteractionMod2)
bestInteractionMod1 <- step(bestInteractionMod1)
summary(bestInteractionMod3)
summary(bestInteractionMod2)
bestInteractionMod2 <- update(bestInteractionMod2, .~. - socialpostimp)
summary(bestInteractionMod1)

bestInteractionMod3.p <- predict(bestInteractionMod3, test3)
bestInteractionMod2.p <- predict(bestInteractionMod2, test3)
bestInteractionMod1.p <- predict(bestInteractionMod1, test3)
(bestInteractionMod3.sse <- sse(bestInteractionMod3.p, test3$revenue)) # 2.4363e+11
(bestInteractionMod2.sse <- sse(bestInteractionMod2.p, test3$revenue)) # 7.5661e+11
(bestInteractionMod1.sse <- sse(bestInteractionMod1.p, test3$revenue)) # 3.3163e+11
currentBest$sse  # NO improvement

# The current best model, mod.lm3, has an average error of 
(sqrt(mod.lm3.sse / nrow(test3)))
# $20,365.61 or about 14% of the mean revenue, less than the standard deviation of 
sd(data3$revenue)
# $23,944 but we can do better.  At this point though, it is reasonably clear that 
# removing the outliers is improving the fit, so work will continue using this data
# set rather than the one with the outliers left in.

# Try ridge and lasso
# Cannot handle categorical data; change binaries to numeric
train3num <- train3
test3num <- test3
for (b in binVars) {
  train3num[, b] <- as.numeric(as.character(train3num[, b]))
  test3num[, b] <- as.numeric(as.character(test3num[, b]))
}

train3num[, 'Week'] <- as.numeric(train3num[, 'Week'])
test3num[, 'Week'] <- as.numeric(test3num[, 'Week'])


# Model as ridge regression: All predictors including all 2-way interactions
lambdas <- c(0, 10^(0:5))
mod.ridge1 <- lm.ridge(revenue ~ .^2, lambda = lambdas, data = train3num)
plot(mod.ridge1$GCV ~ lambdas, type = 'l')

# update lambdas to finer resolution around pest GCV
lambdas <- seq(0, 1000, length = 10)
mod.ridge2 <- lm.ridge(revenue ~ .^2, lambda = lambdas, data = train3num)
plot(mod.ridge2$GCV ~ lambdas, type = 'l')

lambdas <- seq(400, 600, length = 10)
mod.ridge3 <- lm.ridge(revenue ~ .^2, lambda = lambdas, data = train3num)
plot(mod.ridge3$GCV ~ lambdas, type = 'l')


# construct df equivalent to model matrix used in mod.ridge3
test3numInter <- test3num

for (i in 2:(ncol(test3num) - 1)) {
  for (j in (i + 1):ncol(test3num)) {
  	test3numInter <- cbind(test3numInter, test3num[, i] * test3num[, j])
  	names(test3numInter)[length(names(test3numInter))] <- paste(
  	  names(test3num)[i], names(test3num)[j], sep = ':')
  }
}

testScaled <- scale(
  test3numInter[, -1], center = mod.ridge3$xm, scale = mod.ridge3$scales)

mod.ridge3.p <- testScaled %*% mod.ridge3$coef[, which.min(mod.ridge3$GCV)] + 
  mod.ridge3$ym
(mod.ridge3.sse <- sse(mod.ridge3.p, test3$revenue))  # 9.9817e+9
currentBest$sse  # Previous best still a little better

# Try without interaction terms
lambdas <- c(0, 10^(0:5))
mod.ridge1b <- lm.ridge(revenue ~ ., lambda = lambdas, data = train3num)
plot(mod.ridge1b$GCV ~ lambdas, type = 'l')

lambdas <- seq(0, 1000, length = 10)
mod.ridge2b <- lm.ridge(revenue ~ ., lambda = lambdas, data = train3num)
plot(mod.ridge2b$GCV ~ lambdas, type = 'l')

lambdas <- seq(25, 75, length = 10)
mod.ridge3b <- lm.ridge(revenue ~ ., lambda = lambdas, data = train3num)
plot(mod.ridge3b$GCV ~ lambdas, type = 'l')

testScaled <- scale(
  test3num[, -1], center = mod.ridge3b$xm, scale = mod.ridge3b$scales)

mod.ridge3b.p <- testScaled %*% mod.ridge3b$coef[, which.min(mod.ridge3b$GCV)] + 
  mod.ridge3b$ym
(mod.ridge3b.sse <- sse(mod.ridge3b.p, test3$revenue))  # 1.0144
currentBest$sse  # Previous best still a little better

# Examine current best
currentBest
par(mfrow = c(2, 2))
plot(mod.lm2)

# Reasonably good diagnostics, though pt 116 clearly has disproportionate influence.
# Although the best model here is not quite as good as with data2, the diagnostics 
# are better, so we might expect the model to generalize to new data better.


# Continue random searching of the model space, combining smooths, and interactions
predictors <- names(train3num)
predictors <- predictors[-c(1:3)]


iterations <- 10000 # reduced here to knit file, actually ran several thousand

# Keep track of the best 3 models
# Uncomment the following 2 lines for first run only
#bestGCV <- c(Inf, Inf, Inf)
#bestMods <-  c('', '', '')


for (i in 1:iterations) {
  np <- sample(5:20, 1)
  sampP <- sample(predictors, np)
  
  # Smooth the first pred if not binary
  if (sum(!is.na(unique(train3num[, sampP[1]]))) > 2) {
  	sampP[1] <- paste('s(', sampP[1], ')', sep = '')
  }
  
  sampP <- c(sampP, paste(sampP[2], sampP[3], sep = ':'))
  
  form <- 'revenue ~'

  for (p in 1:length(sampP)) { 
    form <- paste(form, sampP[p], sep = ' + ')
  }

  mod <- gam(as.formula(form), data = train3num)

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

bestgamMod3 <- gam(as.formula(bestMods[3]), data = train3num)
bestgamMod2 <- gam(as.formula(bestMods[2]), data = train3num)
bestgamMod1 <- gam(as.formula(bestMods[1]), data = train3num)

# Reduce
# Mod3
summary(bestgamMod3)  # GCV = 1.47e+8
bestgamMod3b <- update(bestgamMod3, .~. -brandpaid)
summary(bestgamMod3b) # GCV = 1.4269e+8
bestgamMod3c <- update(bestgamMod3b, .~. -twengage)
summary(bestgamMod3c) # GCV = 1.3779e+8
bestgamMod3d <- update(bestgamMod3c, .~. -logTwpostimp)
summary(bestgamMod3d) # GCV = 1.3592e+8
bestgamMod3e <- update(bestgamMod3d, .~. -organicnew2)
summary(bestgamMod3e) # GCV = 1.3211e+8
bestgamMod3f <- update(bestgamMod3e, .~. -blognewusers2)
summary(bestgamMod3f) # GCV = 1.2739e+8
bestgamMod3g <- update(bestgamMod3f, .~. -Fbengage)
summary(bestgamMod3g) # GCV = 1.2383e+8
plot(bestgamMod3g)

# Mod2
summary(bestgamMod2)  # GCV = 1.4447e+8
bestgamMod2b <- update(bestgamMod2, .~. -logOrganicnew)
summary(bestgamMod2b) # 1.3996e+8
bestgamMod2c <- update(bestgamMod2b, .~. - blognewusers2)
summary(bestgamMod2c) # 1.3902e+8
bestgamMod2d <- update(bestgamMod2c, .~. - Fbpostimplag2)
summary(bestgamMod2d) # 1.3855e+8
bestgamMod2e <- update(bestgamMod2d, .~. - socialimp)
summary(bestgamMod2e) # 1.3822e+8
plot(bestgamMod2e)

# Mod1
summary(bestgamMod1)  # GCV = 1.4445e+8
bestgamMod1b <- update(bestgamMod1, .~. -rev4weekAgo)
summary(bestgamMod1b)  # GCV = 1.3588e+8 rev4weekAgo
bestgamMod1c <- update(bestgamMod1b, .~. - rev2weekAgo)
summary(bestgamMod1c)  # GCV = 1.3066+8 rev2weekAgo
bestgamMod1d <- update(bestgamMod1c, .~. - Fbpostimp)
summary(bestgamMod1d)  # GCV = 1.2703+8 Fbpostimp
bestgamMod1e <- update(bestgamMod1d, .~. - Twpostimp)
summary(bestgamMod1e)  # GCV = 1.2228+8 Twpostimp
bestgamMod1f <- update(bestgamMod1e, .~. - brandpaid)
summary(bestgamMod1f)  # GCV = 1.2089 brandpaid
bestgamMod1g <- update(bestgamMod1f, .~. - emailclick:brandpaid)
summary(bestgamMod1g)  # GCV = 1.1957 emailclick:brandpaid
bestgamMod1h <- update(bestgamMod1g, .~. - emailclick)
summary(bestgamMod1h)  # GCV = 1.1564
plot(bestgamMod1h)

bestgamMod3g.p <- predict(bestgamMod3g, newdata = test3num)
(bestgamMod3g.sse <- sse(bestgamMod3g.p, test3num$revenue)) # 1.3685e+10

bestgamMod2e.p <- predict(bestgamMod2e, newdata = test3num)
(bestgamMod2e.sse <- sse(bestgamMod2e.p, test3num$revenue)) # 2.1583e+10

bestgamMod1h.p <- predict(bestgamMod1h, newdata = test3num)
(bestgamMod1h.sse <- sse(bestgamMod1h.p, test3num$revenue)) # 1.3620e+10

currentBest


# NOTE: there are a number of variables that have sharp changes in their trends at around week 40 or 50; and most of the lag variables peak sharply at this point
par(mfrow = c(2, 3))
plot(data3$revenue); abline(v = c(2, 54, 106), col = 2)
plot(data3$twposts); abline(v = c(2, 54, 106), col = 2)
plot(data3$twengage); abline(v = c(2, 54, 106), col = 2)
plot(data3$emailsent); abline(v = c(2, 54, 106), col = 2)
plot(data3$emailopen); abline(v = c(2, 54, 106), col = 2)
plot(data3$emailclick); abline(v = c(2, 54, 106), col = 2)

plot(data3$socialpostlag); abline(v = c(2, 54, 106), col = 2)
plot(data3$Fbpostimplag); abline(v = c(2, 54, 106), col = 2)
plot(data3$fbengagelag); abline(v = c(2, 54, 106), col = 2)
plot(data3$TWimpLag); abline(v = c(2, 54, 106), col = 2)
plot(data3$twengagelag); abline(v = c(2, 54, 106), col = 2)

# The vertical red lines indicate the appx location of the shift, and the same week in the year before and after

# Suggests trying a regression tree model, and perhaps an ensemble
plot(data3$revenue, col = data3$highseason, pch = 16)
plot(data3$revenue, col = data3$holiday, pch = 16) # !!! <- clearly decreases rev.
plot(data3$revenue, col = data3$autoenroll, pch = 16)
plot(data3$revenue, col = data3$socialperson, pch = 16)
plot(data3$revenue, col = data3$promoteposts, pch = 16)
plot(data3$revenue, col = data3$learner, pch = 16)

par(mfrow = c(3, 3))
for (i in 1:dim(data3)[2]) {
  plot(data3$revenue ~ data3[, i], main = names(data3)[i])
  lines(lowess(data3$revenue[!is.na(data3[, i])] ~ data3[, i][!is.na(data3[, i])]), 
        col = 2)
  if (i %% 9 == 0 ) { pause() }
}


simpleMod1 <- lm(revenue ~ holiday*organicnew, data = train3num)
summary(simpleMod1)
simpMod1.p <- predict(simpleMod1, newdata = test3num)
(simpMod1.sse <- sse(simpMod1.p, test3num$revenue)) # 1.2734e+10

simpleMod2 <- lm(revenue ~ holiday + organicnew, data = train3num)
summary(simpleMod2)
simpMod2.p <- predict(simpleMod2, newdata = test3num)
(simpMod2.sse <- sse(simpMod2.p, test3num$revenue)) # 1.2591e+10


mod.lm4 <- update(mod.lm2, . ~ . + holiday, data = train3num)
summary(mod.lm4)
mod.lm4.p <- predict(mod.lm4, newdata = test3num)
(mod.lm4.sse <- sse(mod.lm4.p, test3num$revenue)) # 9.0418e+9

currentBest
currentBest <- list(mod = 'mod.lm4', sse = mod.lm4.sse)
sqrt(mod.lm4.sse / nrow(test3))
sd(data3$revenue)


# In the plots above, with the vertical red lines marking years, we can further
# see that many of the predictors have long-tailed distributions, which we may be
# able to better normalize in order to improve the fit.
# For example
par(mfrow = c(1, 1))
hist(data3$socialpostimp)
hist(data3$Fbpostimp)
hist(data3$Fbpostimplag)
hist(data3$Fblikes)
hist(data3$Fbengage)
hist(data3$Twpostimp)


# NEXT:  We can also see that several predictors have data that fall into 
# distinct categories.  E.g.
plot(data3$displaylag)
hist(data3$displaylag)

# We see that most of the data fall into 2 bins. Similarly, the following all 
# have large amounts of data that fall into distinct bands, and/or have sharp 
# discontinuities, that might lend themselves to a tree model:
hist(data3$displayimp)
hist(data3$brandpaid)
hist(data3$socialpostlag)
hist(data3$TWimpLag)
plot(data3$emailsent)
plot(data3$emailclick)
plot(data3$emailopen)

# (Later tree models will be examined, and then an ensemble of regression and 
# tree methods will be explored with the goal of combining the strengths of each 
# approach)

# For now, transform some of the above predictors that are amenable to 
# normalizing transformations.  For each of the following, find the exponential 
# transformation that best normalizes the distribution (e.g., has the smallest p-
# value for the Shapiro-Wilk test):
# E.g.:
tr <- seq(1, 0.29, length = 9)
par(mfrow = c(3, 3))

for (exponent in tr) {
  hist(data3$socialpostimp^exponent, 
       main = '', 
       xlab = paste('socialpostimp^', round(exponent, 2), sep = ''))
  legend('topright', 
         legend = paste(
           'p: ', shapiro.test(data3$socialpostimp^exponent)$p, sep = ''),
         cex = 0.8)
}

# Here we can see that as we decrease the exponent from 1, the distribution 
# becomes closer and closer to normal, reaching its optimum at with an exponent 
# of appx 0.29.  Automate this process so other optimal transformations may be 
# found quickly
par(mfrow = c(1, 1))

findBestExpTransform <- function(
  x, start = 1, end = 0.01, stepSize = -0.01, plt = T) {
  lambdas <- seq(start, end, stepSize) # exponents to try
  ps <- numeric(length(lambdas))
  
  for (i in 1:length(lambdas)) {
  	ps[i] <- shapiro.test(x^lambdas[i])$p
  }
  
  if (plt) {
    plot(ps ~ lambdas, type = 'l')
    abline(v = lambdas[which.max(ps)], col = 2)
  }
  
  return (lambdas[which.max(ps)])
}

par(mfrow = c(2, 3))
(socialpostimpExp <- findBestExpTransform(data3$socialpostimp))
# opt at exp = ~0.29 as before
FbpostimpExp <- findBestExpTransform(data3$Fbpostimp)
FbpostimplagExp <- findBestExpTransform(data3$Fbpostimplag)
FblikesExp <- findBestExpTransform(data3$Fblikes)
FbengageExp <- findBestExpTransform(data3$Fbengage)
TwpostimpExp <- findBestExpTransform(data3$Twpostimp)

# Note that in none of the cases above is a truly normal distribution acheived 
# (best p values still significantly non-normal), but the transformations may 
# pull the tails in enough to provide better models.  One way to find out....
data3Tr <- data3  
# NOTE: these are data that have already been normalized to be on [0, 1]
data3Tr$Fbpostimp <- data3Tr$Fbpostimp^FbpostimpExp
data3Tr$Fbpostimplag <- data3Tr$Fbpostimplag^FbpostimplagExp
data3Tr$Fblikes <- data3Tr$Fblikes^FblikesExp
data3Tr$Fbengage <- data3Tr$Fbengage^FbengageExp
data3Tr$Twpostimp <- data3Tr$Twpostimp^TwpostimpExp

# And rename to avoid accidental mix-ups
rename <- which(names(data3Tr) %in% 
  c('Fbpostimp', 'Fbpostimplag', 'Fblikes', 'Fbengage', 'Twpostimp'))
names(data3Tr)[rename] <- paste(names(data3Tr)[rename], 'Tr', sep = '')

# Now all transformed columns end in Tr; reset seed and repeat above efforts
set.seed(11)

# Convert binary data to categorical 
data3Tr$highseason   <- as.factor(data3Tr$highseason)
data3Tr$holiday      <- as.factor(data3Tr$holiday)
data3Tr$autoenroll   <- as.factor(data3Tr$autoenroll)
data3Tr$socialperson <- as.factor(data3Tr$socialperson)
data3Tr$promoteposts <- as.factor(data3Tr$promoteposts)
data3Tr$learner      <- as.factor(data3Tr$learner)

# Convert 'Week' to Date type
data3Tr$Week <- as.Date(data3Tr$Week, format = '%m/%d/%y')

# Scale numerical data; DO NOT SCALE RESPONSE
for (i in 3:ncol(data3Tr)) {
  if (class(data3Tr[, i]) != 'factor') {
    data3Tr[, i] <- rescale(data3Tr[, i])
  }
}

# Split data into train and test sets
# Put 29 rows (~25%) aside in test set
test3Tr.inds <- sort(sample(1:nrow(data3Tr), size = 29))
test3Tr <- data3Tr[test3Tr.inds, ]
train3Tr <- data3Tr[-test3Tr.inds, ]

# Fill remaining NAs
test3Tr$rev2weekAgo[1:2] <- test3Tr$rev2weekAgo[3]
test3Tr$rev3weekAgo[1:2] <- test3Tr$rev3weekAgo[3]
test3Tr$rev4weekAgo[1:2] <- test3Tr$rev4weekAgo[3]

# Just as a baseline, do a simple linear model and check its predictions on the 
# test3Tr set as a "score-to-beat"
# (Ignore data3Tr/test3Tr2/train3Tr2 for now)
modTr.lm1 <- lm(revenue ~ ., data = train3Tr) 
summary(modTr.lm1)

# Remove linearly dependent variables, in order to be able to use predict
modTr.lm1 <- update(modTr.lm1, 
                  . ~ . - Week - weekno - weekOfYear - fbEngage3 - 
                  logSocialpostlag - organicnew2 - logOrganicnew - blognewusers2 
                  - blognewusersRecip - Fbpostimplag2 - weekOfYear2 - 
                  sinWeekOfYear - Twpostimp2 - logTwpostimp)
summary(modTr.lm1)
modTr.lm1.p <- predict(modTr.lm1, newdata = test3Tr)

# Compare predictions to actual values
cbind(modTr.lm1.p, test3Tr$revenue)
(modTr.lm1.sse <- sse(modTr.lm1.p, test3Tr$revenue)) # 4.3531e+11
sqrt(modTr.lm1.sse / nrow(test3Tr))
mean(data3Tr$revenue); sd(data3Tr$revenue)

# i.e., the average error in the predictions is $136,176.30; just less than the
# mean revenue value.  Obviously not a great fit.
# Track current best modTrel, and sse of current best
currentBest  # No improvement

# Reduce modTr.lm1 to prevent overfitting
# NOTE: here the step function will not complete due to NAs changing the 
# available amount of data.  ModTrel was updated at each iteration then update
# was run again.  Strictly speaking, AICs are not directly comparable in this 
# way because the number of underlying data going into the computation changes,
# but at this point the goal is simply to thin out the modTrel predictor space
modTr.lm2 <- step(modTr.lm1, direction = 'both')
modTr.lm2 <- update(modTr.lm1, . ~ . - socialimplag)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - emailsent)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - emailclick - promoteposts)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - FblikesTr)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - rev1weekAgo - rev4weekAgo)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - blogrefernew)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - nonbrandclicks)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - FbpostimplagTr)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - rev2weekAgo - fbposts)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - socialperson)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - autoenroll - learner - TwpostimpTr)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - twengagelag)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - displaylag)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - twengage - twposts - blogrefer)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - FbpostimpTr - socialpostimp)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - rev3weekAgo)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - Fbpageimp - socialimp)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - nonbrandpaid)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
modTr.lm2 <- update(modTr.lm2, . ~ . - displayimp)
modTr.lm2 <- step(modTr.lm2, direction = 'both')
summary(modTr.lm2)
modTr.lm2.p <- predict(modTr.lm2, newdata = test3Tr)
(modTr.lm2.sse <- sse(modTr.lm2.p, test3Tr$revenue)) # 7.6168e+9
sqrt(modTr.lm2.sse / nrow(test3Tr))

# Check against current best
currentBest$sse

# Wow, quite an improvement... Replace
currentBest <- list(modTr = 'modTr.lm2', sse = modTr.lm2.sse)

# Add in the variables that could not be explored in the initial modTrel, and 
# lagged revenue vars, since there is known to be autocorrelation
modTr.lm3 <- update(modTr.lm2, 
                    . ~ . + Week + weekno + weekOfYear + fbEngage3 + 
                  logSocialpostlag + organicnew2 + logOrganicnew + blognewusers2 
                  + blognewusersRecip + Fbpostimplag2 + weekOfYear2 + 
                  sinWeekOfYear + Twpostimp2 + logTwpostimp)
# And reduce again
modTr.lm3 <- step(modTr.lm3, direction = 'both')
modTr.lm3 <- update(modTr.lm3, 
                    . ~ . - organicnew2 - Twpostimp2 - organicnew - Week - 
                    sinWeekOfYear - logTwpostimp)
modTr.lm3 <- step(modTr.lm3, direction = 'both')
modTr.lm3 <- update(modTr.lm3, 
                  . ~ . - weekno - blognewusers - blognewusersRecip - brandpaid)
modTr.lm3 <- step(modTr.lm3, direction = 'both')
modTr.lm3 <- update(modTr.lm3, . ~ . - emailopen)
modTr.lm3 <- step(modTr.lm3, direction = 'both')
modTr.lm3 <- update(modTr.lm3, . ~ . - brandclicks)
modTr.lm3 <- step(modTr.lm3, direction = 'both')
summary(modTr.lm3)

modTr.lm3.p <- predict(modTr.lm3, newdata = test3Tr)
(modTr.lm3.sse <- sse(modTr.lm3.p, test3Tr$revenue)) # 7.8254e+9
sqrt(modTr.lm3.sse / nrow(test3Tr))
currentBest$sse  # Previous is better. Do not update

# Explore potential interaction terms in a similar manner.  It is generally - 
# though not always - the case that when interaction terms are significant, so 
# are the main effects.  In creating random modTrels we initially require that if 
# the x:y interaction term is present, so are x and y
predictors <- names(train3Tr)[-1]

# Do not include binary variables at this point
predictors <- predictors[-which(predictors %in% binVars)]

# Also exclude Week, weekno from predictors here, as these will always be unique
# values (never repeated), so we don't want extrapolate 
predictors <- predictors[-c(1, 2)]

# Uncomment for first run
# bestTrAIC <- c(Inf, Inf, Inf)
# bestInteractionModTrs <-  c('', '', '')
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

  modTr <- lm(as.formula(form), data = train3Tr)
  summary(modTr)
  aic <- extractAIC(modTr)[2]
  
  if (aic == -Inf) { 
  	# bad modTrel
  	continue 
  } else {
    if (aic < bestTrAIC[3]) {
  	  if (aic < bestTrAIC[2]) {
        if (aic < bestTrAIC[1]) {
  	      # insert in first place, bump others
  	      bestTrAIC[3] <- bestTrAIC[2]
  	      bestTrAIC[2] <- bestTrAIC[1]
  	      bestTrAIC[1] <- aic
  	      bestInteractionModTrs[3] <- bestInteractionModTrs[2]
  	      bestInteractionModTrs[2] <- bestInteractionModTrs[1]
  	      bestInteractionModTrs[1] <- form
  	    } else { 
  	      # insert in second place, bump other
  	      bestTrAIC[3] <- bestTrAIC[2]
  	      bestTrAIC[2] <- aic
  	      bestInteractionModTrs[3] <- bestInteractionModTrs[2]
  	      bestInteractionModTrs[2] <- form
  	    } 
  	  } else {
  	    # insert in third place; no bumping
  	    bestTrAIC[3] <- aic
  	    bestInteractionModTrs[3] <- form
  	  }
    }
  }
}

bestTrAIC
#bestInteractionModTrs


bestInteractionModTr3 <- lm(as.formula(bestInteractionModTrs[3]), 
                            data = train3Tr)
bestInteractionModTr2 <- lm(as.formula(bestInteractionModTrs[2]), 
                            data = train3Tr)
bestInteractionModTr1 <- lm(as.formula(bestInteractionModTrs[1]), 
                            data = train3Tr)

# Reduce
bestInteractionModTr3 <- step(bestInteractionModTr3)
bestInteractionModTr2 <- step(bestInteractionModTr2)
bestInteractionModTr1 <- step(bestInteractionModTr1)
summary(bestInteractionModTr3)
summary(bestInteractionModTr2)
summary(bestInteractionModTr1)

bestInteractionModTr3.p <- predict(bestInteractionModTr3, test3Tr)
bestInteractionModTr2.p <- predict(bestInteractionModTr2, test3Tr)
bestInteractionModTr1.p <- predict(bestInteractionModTr1, test3Tr)
(bestInteractionModTr3.sse <- sse(bestInteractionModTr3.p, test3Tr$revenue)) 
# 9.1946e+11
(bestInteractionModTr2.sse <- sse(bestInteractionModTr2.p, test3Tr$revenue)) 
# 4.6487e+11
(bestInteractionModTr1.sse <- sse(bestInteractionModTr1.p, test3Tr$revenue)) 
# 1.4465e+12
currentBest$sse  # NO improvement

# The current best model, modTr.lm3, has an average error of 
(sqrt(modTr.lm3.sse / nrow(test3Tr)))
# $16,424.89 or about 11% of the mean revenue, and less than the standard 
# deviation of 
sd(data3Tr$revenue)
# $23,944

# Try ridge
# Cannot handle categorical data; change binaries to numeric
train3Trnum <- train3Tr
test3Trnum <- test3Tr
for (b in binVars) {
  train3Trnum[, b] <- as.numeric(as.character(train3Trnum[, b]))
  test3Trnum[, b] <- as.numeric(as.character(test3Trnum[, b]))
}

train3Trnum[, 'Week'] <- as.numeric(train3Trnum[, 'Week'])
test3Trnum[, 'Week'] <- as.numeric(test3Trnum[, 'Week'])


# ModTrel as ridge regression: All predictors including all 2-way interactions
par(mfrow = c(1, 1))
lambdas <- c(0, 10^(0:5))
modTr.ridge1 <- lm.ridge(revenue ~ .^2, lambda = lambdas, data = train3Trnum)
plot(modTr.ridge1$GCV ~ lambdas, type = 'l')

# update lambdas to finer resolution around pest GCV
lambdas <- seq(0, 1000, length = 10)
modTr.ridge2 <- lm.ridge(revenue ~ .^2, lambda = lambdas, data = train3Trnum)
plot(modTr.ridge2$GCV ~ lambdas, type = 'l')

lambdas <- seq(500, 600, length = 10)
modTr.ridge3 <- lm.ridge(revenue ~ .^2, lambda = lambdas, data = train3Trnum)
plot(modTr.ridge3$GCV ~ lambdas, type = 'l')


# construct df equivalent to modTrel matrix used in modTr.ridge3
test3TrnumInter <- test3Trnum

for (i in 2:(ncol(test3Trnum) - 1)) {
  for (j in (i + 1):ncol(test3Trnum)) {
  	test3TrnumInter <- cbind(test3TrnumInter, test3Trnum[, i] * test3Trnum[, j])
  	names(test3TrnumInter)[length(names(test3TrnumInter))] <- paste(
  	  names(test3Trnum)[i], names(test3Trnum)[j], sep = ':')
  }
}

testScaled <- scale(
  test3TrnumInter[, -1], center = modTr.ridge3$xm, scale = modTr.ridge3$scales)

modTr.ridge3.p <- (testScaled %*% 
                   modTr.ridge3$coef[, which.min(modTr.ridge3$GCV)] +
                   modTr.ridge3$ym)
(modTr.ridge3.sse <- sse(modTr.ridge3.p, test3Tr$revenue))  # 9.9797e+9
currentBest$sse  # Previous best still a little better

# Try without interaction terms
lambdas <- c(0, 10^(0:5))
modTr.ridge1b <- lm.ridge(revenue ~ ., lambda = lambdas, data = train3Trnum)
plot(modTr.ridge1b$GCV ~ lambdas, type = 'l')

lambdas <- seq(0, 1000, length = 10)
modTr.ridge2b <- lm.ridge(revenue ~ ., lambda = lambdas, data = train3Trnum)
plot(modTr.ridge2b$GCV ~ lambdas, type = 'l')

lambdas <- seq(35, 40, length = 10)
modTr.ridge3b <- lm.ridge(revenue ~ ., lambda = lambdas, data = train3Trnum)
plot(modTr.ridge3b$GCV ~ lambdas, type = 'l')

testScaled <- scale(
  test3Trnum[, -1], center = modTr.ridge3b$xm, scale = modTr.ridge3b$scales)

modTr.ridge3b.p <- (testScaled %*% 
                    modTr.ridge3b$coef[, which.min(modTr.ridge3b$GCV)] + 
                    modTr.ridge3b$ym)
(modTr.ridge3b.sse <- sse(modTr.ridge3b.p, test3Tr$revenue))  # 9.7001e+9
currentBest$sse  # Previous best still a little better

# Examine current best
currentBest
par(mfrow = c(2, 2))
plot(modTr.lm2)

# Reasonably good diagnostics

# Continue random searching of the modTrel space, combining smooths, and 
# interactions
predictors <- names(train3Trnum)
predictors <- predictors[-c(1:3)]


iterations <- 10000 # reduced here to knit file, actually ran several thousand

# Keep track of the best 3 modTrels
# Uncomment the following 2 lines for first run only
# bestTrGCV <- c(Inf, Inf, Inf)
# bestModTrs <-  c('', '', '')

for (i in 1:iterations) {
  np <- sample(5:20, 1)
  sampP <- sample(predictors, np)
  
  # Smooth the first pred if not binary
  if (sum(!is.na(unique(train3Trnum[, sampP[1]]))) > 2) {
  	sampP[1] <- paste('s(', sampP[1], ')', sep = '')
  }
  
  sampP <- c(sampP, paste(sampP[2], sampP[3], sep = ':'))
  
  form <- 'revenue ~'

  for (p in 1:length(sampP)) { 
    form <- paste(form, sampP[p], sep = ' + ')
  }

  modTr <- gam(as.formula(form), data = train3Trnum)

  if (modTr$gcv.ubre < bestTrGCV[3]) {
  	if (modTr$gcv.ubre < bestTrGCV[2]) {
      if (modTr$gcv.ubre < bestTrGCV[1]) {
  	    # insert in first place, bump others
  	    bestTrGCV[3] <- bestTrGCV[2]
  	    bestTrGCV[2] <- bestTrGCV[1]
  	    bestTrGCV[1] <- modTr$gcv.ubre
  	    bestModTrs[3] <- bestModTrs[2]
  	    bestModTrs[2] <- bestModTrs[1]
  	    bestModTrs[1] <- form
  	  } else { 
  	    # insert in second place, bump other
  	    bestTrGCV[3] <- bestTrGCV[2]
  	    bestTrGCV[2] <- modTr$gcv.ubre
  	    bestModTrs[3] <- bestModTrs[2]
  	    bestModTrs[2] <- form
  	  } 
  	} else {
  	  # insert in third place; no bumping
  	  bestTrGCV[3] <- modTr$gcv.ubre
  	  bestModTrs[3] <- form
  	}
  }
}

bestTrGCV

bestgamModTr3 <- gam(as.formula(bestModTrs[3]), data = train3Trnum)
bestgamModTr2 <- gam(as.formula(bestModTrs[2]), data = train3Trnum)
bestgamModTr1 <- gam(as.formula(bestModTrs[1]), data = train3Trnum)

# Reduce
# ModTr3
summary(bestgamModTr3)  # GCV = 1.4743e+8
bestgamModTr3b <- update(bestgamModTr3, .~. - blogsignup)
summary(bestgamModTr3b) # GCV = 1.4223e+8 blogsignup
bestgamModTr3c <- update(bestgamModTr3b, .~. - rev1weekAgo)
summary(bestgamModTr3c) # GCV = 1.37361e+8 rev1weekAgo
bestgamModTr3d <- update(bestgamModTr3c, .~. - highseason)
summary(bestgamModTr3d) # GCV = 1.3411 highseason
bestgamModTr3e <- update(bestgamModTr3d, .~. - weekOfYear)
summary(bestgamModTr3e) # GCV = 1.3408e+8
bestgamModTr3f <- update(bestgamModTr3e, .~. - weekOfYear2)
summary(bestgamModTr3f) # GCV = 1.3022+8 weekOfYear2
par(mfrow = c(1, 1))
plot(bestgamModTr3f)
bestgamModTr3g <- update(bestgamModTr3f, .~. - s(blogvisits) + blogvisits)
summary(bestgamModTr3g) # GCV = 1.3022+8 weekOfYear2


# ModTr2
summary(bestgamModTr2)  # GCV = 1.4378e+8
bestgamModTr2b <- update(bestgamModTr2, .~. - brandpaid)
summary(bestgamModTr2b) # 1.3830e+8
bestgamModTr2c <- update(bestgamModTr2b, .~. - fbposts)
summary(bestgamModTr2c) # 1.3308+8 FbpostimplagTr
plot(bestgamModTr2c)
summary(bestgamModTr2c) # 1.3308+8 FbpostimplagTr

# ModTr1
summary(bestgamModTr1)  # GCV = 1.4122e+8
bestgamModTr1b <- update(bestgamModTr1, .~. - rev4weekAgo)
summary(bestgamModTr1b)  # GCV = 1.3476+8 rev4weekAgo
bestgamModTr1c <- update(bestgamModTr1b, .~. - rev2weekAgo)
summary(bestgamModTr1c)  # GCV = 1.2975+8 rev2weekAgo
bestgamModTr1d <- update(bestgamModTr1c, .~. - logTwpostimp)
summary(bestgamModTr1d)  # GCV = 1.2593+8 logTwpostimp
bestgamModTr1e <- update(bestgamModTr1d, .~. - brandpaid)
summary(bestgamModTr1e)  # GCV = 1.2320+8 brandpaid
bestgamModTr1f <- update(bestgamModTr1e, .~. - emailclick:brandpaid)
summary(bestgamModTr1f)  # GCV = 1.1793 emailclick:brandpaid
bestgamModTr1g <- update(bestgamModTr1f, .~. - emailclick)
summary(bestgamModTr1g)  # GCV = 1.1379+8 emailclick
bestgamModTr1h <- update(bestgamModTr1g, .~. - FbpostimpTr)
summary(bestgamModTr1h)  # GCV = 1.1331 FbpostimpTr
plot(bestgamModTr1h)

bestgamModTr3g.p <- predict(bestgamModTr3g, newdata = test3Trnum)
(bestgamModTr3g.sse <- sse(bestgamModTr3g.p, test3Trnum$revenue)) # 1.1059e+10

bestgamModTr2c.p <- predict(bestgamModTr2c, newdata = test3Trnum)
(bestgamModTr2c.sse <- sse(bestgamModTr2c.p, test3Trnum$revenue)) # 2.0010e+10

bestgamModTr1h.p <- predict(bestgamModTr1h, newdata = test3Trnum)
(bestgamModTr1h.sse <- sse(bestgamModTr1h.p, test3Trnum$revenue)) # 1.4540e+10

currentBest  # No improvement
summary(modTr.lm2)



# NEXT:
# TREE
# REG/TREE ENSEMBLE

save.image('~/Desktop/cbtnuggetsbusinessanalyticsproject/nuggets3.RData')