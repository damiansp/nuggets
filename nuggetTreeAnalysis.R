#=============================================================#
#                                                             #
# Analyze the CBT Nuggets Test Data Set - Regression Tree     #
# @author: Damian Satterthwaite-Phillips <damiansp@gmail.com> #
# @version: 25 Sep 2016                                       #
#                                                             #
#=============================================================#

# Clear workspace
rm(list = ls())

# Load libraries (assumes they have been installed)
library(rpart) # for rpart() tree modeling 

# Load any data stored from previous session
load('~/Desktop/cbtnuggetsbusinessanalyticsproject/nuggets4.RData')

# Start by removing the additional outliers, and transforming long-tailed 
# distributions as before
dataTree <- read.csv(
  '~/Desktop/cbtnuggetsbusinessanalyticsproject/dataCleanNoOutlier.csv')

# Store current best from previous analyses
currentBest <- list(mod = 'modTr.lm2', sse = 7616808500) # Found in nuggetAnalysis3.R

# Seed random number generator
set.seed(11)

dataTree$Week <- as.numeric(as.Date(dataTree$Week, format = '%m/%d/%y'))

# Removal of additional outliers
sort(dataTree$Fblikes)
dataTree$Fblikes[dataTree$Fblikes > 1000] <- NA
sort(dataTree$Fbengage)
dataTree$Fbengage[dataTree$Fbengage > 17000] <- NA

# Scale numerical data; DO NOT SCALE RESPONSE
# Rescale so all values are on the interval [0, 1]
rescale <- function(x) {
	x <- x - min(x, na.rm = T)
	x <- x / max(x, na.rm = T)  
	return (x)
}

for (i in 3:ncol(dataTree)) {
  if (class(dataTree[, i]) != 'factor') {
    dataTree[, i] <- rescale(dataTree[, i])
  }
}

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

socialpostimpExp <- findBestExpTransform(dataTree$socialpostimp, plt = F)
FbpostimpExp <- findBestExpTransform(dataTree$Fbpostimp, plt = F)
FbpostimplagExp <- findBestExpTransform(dataTree$Fbpostimplag, plt = F)
FblikesExp <- findBestExpTransform(dataTree$Fblikes, plt = F)
FbengageExp <- findBestExpTransform(dataTree$Fbengage, plt = F)
TwpostimpExp <- findBestExpTransform(dataTree$Twpostimp, plt = F)

dataTreeTr <- dataTree
dataTreeTr$Fbpostimp <- dataTreeTr$Fbpostimp^FbpostimpExp
dataTreeTr$Fbpostimplag <- dataTreeTr$Fbpostimplag^FbpostimplagExp
dataTreeTr$Fblikes <- dataTreeTr$Fblikes^FblikesExp
dataTreeTr$Fbengage <- dataTreeTr$Fbengage^FbengageExp
dataTreeTr$Twpostimp <- dataTreeTr$Twpostimp^TwpostimpExp

# And rename to avoid accidental mix-ups
rename <- which(names(dataTreeTr) %in% 
  c('Fbpostimp', 'Fbpostimplag', 'Fblikes', 'Fbengage', 'Twpostimp'))
names(dataTreeTr)[rename] <- paste(names(dataTreeTr)[rename], 'Tr', sep = '')

# And since the only data set for tree models so far uses the transformed data,
# just call it tree:
dataTree <- dataTreeTr
rm(dataTreeTr)


# Preprocessing done.... begin modeling
set.seed(11)

# Split data into train and test sets
# Put 29 rows (~25%) aside in test set
testTree.inds <- sort(sample(1:nrow(dataTree), size = 29))

# In the test set, in order to be able to make predictions if there are NAs,
# replace NA's with values; Since these are time series with clear temporal 
# autocorrelation, simplify by filling with the data from the previous date
for (j in 2:ncol(dataTree)) {
  for (i in 1:nrow(dataTree)) {
  	if (i %in% testTree.inds & is.na(dataTree[i, j])) {
  	  if (i == 1) {
  	  	# first row has no prior date; use following
  	  	dataTree[i, j] <- dataTree[i + 1, j]
  	  } else {
  	    dataTree[i, j] <- dataTree[i - 1, j]
  	  }
  	} 	
  }
}

testTree <- dataTree[testTree.inds, ]
trainTree <- dataTree[-testTree.inds, ]

# Fill remaining NAs
testTree$rev2weekAgo[1:2] <- testTree$rev2weekAgo[3]
testTree$rev3weekAgo[1:2] <- testTree$rev3weekAgo[3]
testTree$rev4weekAgo[1:2] <- testTree$rev4weekAgo[3]


# Initial tree
mod.tree <- rpart(revenue ~ ., data = trainTree)
mod.tree
plot(mod.tree)
text(mod.tree, cex = 0.6)

# Examine the cross-validation error at different levels of complexity in the 
# tree
mod.tree2 <- rpart(revenue ~ ., data = trainTree, cp = 0.001)
mod.tree2
plotcp(mod.tree2)

# The CV error reaches a minimum at a complexity cutoff of 0.094
printcp(mod.tree2)

mod.tree2.prune <- prune(mod.tree2, 0.055)
plot(mod.tree2.prune)
text(mod.tree2.prune, cex = 0.7)

# As this is still ultimately a regression model, we can perform the same 
# diagnostics, and use the sum of squared errors (SSE) as a metric of goodness of 
# fit as before.
plot(predict(mod.tree2.prune), 
     resid(mod.tree2.prune), 
     xlab = 'Fitted', 
     ylab = 'Residuals')
qqnorm(resid(mod.tree2.prune))
qqline(resid(mod.tree2.prune))

mod.tree2.prune.p <- predict(mod.tree2.prune, newdata = trainTree)

sse <- function(preds, actual) {
  return (sum((preds - actual)^2))
}

(mod.tree2.sse <- sse(mod.tree2.prune.p, trainTree$revenue)) # 1.7116e+10
currentBest

# This model is nowhere near as good as our best regression model, but given that 
# it is essentially using just 3 variables as binary values, it is actually doing 
# pretty well.  We might now try combining the models.  There are a couple of 
# approaches that we could take.  We can use an ensemble model, or alternately, 
# we can try looking at each of the 4 partitions that the tree model splits the 
# data into, and fitting a different regression model on each.  The problem with
# the latter approach is that there are already few rows of data relative to the 
# number of predictors, so the ensemble method may be the better approach in this 
# case.

# The ensemble method will be carried out in a Python notebook... Save the 
# preprocessed data to make them accessible.
write.csv(dataTree, 
          '~/Desktop/cbtnuggetsbusinessanalyticsproject/dataFinal.csv')

save.image('~/Desktop/cbtnuggetsbusinessanalyticsproject/nuggets4.RData')
