# Generate classification data.
# Given a model, this function generates points within
# the range of the data, classifies them, and attempts to locate boundaries 
# by looking at advantage.
# 
# If posterior probabilities of classification are available, then the
# \code{\link{advantage}} will be calculated directly.  If not, \code{\link{knn}}
# is used calculate the advantage based on the number of neighbouring points
# that share the same classification.  Because knn is $O(n^2)$ this method is rather 
# slow for large (>20,000 say) data sets. 
# 
# By default, the boundary points are identified 
# as those below the 5th-percentile for advantage.  
# 
# @arguments classification model
# @arguments data set used in model
# @arguments number of points to generate
# @arguments method to use, currently either grid (an evenly spaced grid), random (uniform random distribution across cube), or nonaligned (grid + some random peturbationb)
# @returns data.frame of classified data
# @keyword datagen 
generate_classification_data <- function(model, data, n, method, advantage) {
	v <- variables(model)
	
	df <- generate_data(data[, v$predictors, drop=FALSE], n=n, method=method)
	post <- posterior(model, df)
	
	df[[".ADVANTAGE"]] <- NA
	if (is.null(post)) {
		df[[v$response]] <- classify(model, df)
		if (advantage) {
			v <- variables(model)
			pred <- rescaler(df[, v$predictors], type="range")
			a <- knn(pred, pred, df[,v$response], prob=T, k=5)

			df[[".ADVANTAGE"]] <- attr(a, "prob")
			
		}
	} else {
		df[[v$response]] <- factor(max.col(post), levels=1:ncol(post), labels=colnames(post))
		if (advantage) df[[".ADVANTAGE"]] <- advantage(post)
		df <- cbind(df, post)
	}
	df[[".TYPE"]] <- factor("simulated")
	
	df	
}

# Classify
# Common interface to extract predict classification from a variety of classification objects
# 
# If the classification method can produce a matrix of posterior
# probabilities (see \code{\link{posterior}}), then that will be used to 
# calculate the \code{\link{advantage}}.  Otherwise, the classify method
# will be used and the advantage calculated using a k-nearest neighbours
# approach.
# 
# @arguments model object
# @arguments data set used in model
# @alias classify.rpart
# @keyword internal
classify <- function(model, data) UseMethod("classify", model)
classify.rpart <- function(model, data, ...)  predict(model, data, type="class")

# Posterior
# Common interface to extract posterior group probabilities
# 
# Every classification method seems to provide a slighly different 
# way of retrieving the posterior probability of group membership.  This 
# function provides a common interface to all of them
# 
# @arguments model object
# @arguments data set used in model
# @alias posterior.lda
# @alias posterior.qda
# @alias posterior.randomForest
# @alias posterior.nnet
# @alias posterior.svm
# @alias posterior.glm
# @alias posterior.default
# @keyword internal
posterior <- function(model, data) UseMethod("posterior", model)
posterior.default <- function(model, data) NULL
posterior.qda <- posterior.lda <- function(model, data) predict(model, data)$posterior
posterior.randomForest <- function(model, data) predict(model, data, type="prob")
posterior.svm <- function(model, data) attr(predict(model, data, probability = TRUE), "probabilities")
posterior.nnet <- function(model, data) {
	probs <- predict(model, data)
	cbind(probs, 1 - rowSums(probs))
}
posterior.glm <- function(model, data) {
	probs <- predict(model, data, type="response")
	probs <- cbind(probs, 1 - probs)
	colnames(probs) <- levels(model$model[[variables(model)$response]])
	probs
}

# Advantage
# Calculate the advantage the most likely class has over the next most likely.
# 
# This is used to identify the boundaries between classification regions.
# Points with low (close to 0) advantage are likely to be near boundaries.
# 
# @arguments matrix of posterior probabilities
# @keyword classif
advantage <- function(post) {
	apply(post, 1, function(x) -diff(x[order(x, decreasing=TRUE)[1:2]]))
}

# Variables
# Extract predictor and response variables for a model object.
# 
# Due to the way that most model objects are stored, you
# also need to supply the data set you used with the original
# data set.  It currently doesn't support model fitted without
# using a data argument. 
# 
# @alias variables.default
# @arguments model object
# @returns response variable
# @returns predictor variables
# @keyword attribute 
variables <- function(model) UseMethod("variables", model)
variables.default <- function(model) {
	list(
		response = all.vars(model$terms[[2]]),
		predictors = all.vars(model$terms[[3]])
	)	
}
