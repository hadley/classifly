# Classify and explore a data set
# Classifly provides a convenient method to fit a classification function
# and then explore the results in the original high dimensional space.
# 
# This is a convenient function to fit a classification function and
# then explore the results using GGobi.  You can also do this in two
# separate steps using the classification function and then \code{\link{explore}}.
# 
# By default in GGobi, points that are not on the boundary (ie. that have an
# advantage greater than the 5% percentile) are hidden.  To show them, switch
# to brush mode and choose include shadowed points from the brush menu on
# the plot window.  You can then brush them yourself to explore how the 
# certainty of classification varies throughout the space
# 
# Special notes:
# 
# \itemize{
# 	\item You should make sure the response variable is a factor
# 	\item For SVM, make sure to include \code{probability = TRUE} in the arguments to \code{classifly}
# 
# }
# 
# @arguments Data set use for classification
# @arguments Classification formula, usually of the form response ~ predictors
# @arguments Function to use for the classification, eg \code{\link[MASS]{lda}}
# @arguments Other arguments passed to classification function.  For example. if you use \code{\link[e1071]{svm}} you need to use \code{probabiltiy = TRUE} so that posterior probabilities can be retrieved.
# @arguments Number of points to simulate.  To maintain the illusion of a filled solid this needs to increase with dimension.  10,000 points seems adequate for up to four of five dimensions, but if you have more predictors than that, you will need to increase this number.
# @arguments method to simulate points: grid, random or nonaligned (default).  See \code{\link{simvar}} for more details on the methods used.
# @arguments type of scaling to apply to data.  Defaults to commmon range.  See \code{\link[reshape]{rescaler}} for more details.
# @alias package-classifly
# @seealso \code{\link{explore}}, \url{http://had.co.nz/classifly}
# @keyword dynamic 
#X classifly(kyphosis, Kyphosis ~ . , lda)
#X classifly(kyphosis, Kyphosis ~ poly(Age,2) + poly(Number,2) + poly(Start,2) , lda)
#X classifly(kyphosis, Kyphosis ~ . , qda)
#X classifly(kyphosis, Kyphosis ~ . , rpart)
#X classifly(kyphosis, Kyphosis ~ . , knnf, k=3)
#X classifly(kyphosis, Kyphosis ~ . , glm, family="binomial")
#X 
#X classifly(kyphosis, Kyphosis ~ . , svm, probability=TRUE)
#X classifly(kyphosis, Kyphosis ~ . , svm, probability=TRUE, kernel="linear")
#X classifly(kyphosis, Kyphosis ~ . , best.svm, probability=TRUE, kernel="linear")
#X
#X #Also can use explore directorly
#X bsvm <- best.svm(Species~., data = iris, gamma = 2^(-1:1), cost = 2^(2:+ 4), probability=TRUE)
#X explore(bsvm, iris)
classifly <- function(data, model, classifier, ..., n=10000, method="nonaligned", type="range") {
  data <- rescaler(data, type=type)
	classifly <- classifier(model, data=data, ...)
	explore(classifly, data, n=n, method=method, advantage=TRUE)
}

# Explore default
# Default method for exploring objects
# 
# The default method currently works for classification
# functions.
# 
# It generates a data set filling the design space, finds 
# class boundaries (if desired) and then displays in a new 
# ggobi instance.
# 
# @arguments classification object
# @arguments data set used with classifier
# @arguments number of points to generate when searching for boundaries
# @arguments method to generate points, see \code{\link{generate_data}}
# @arguments only display boundaries
# @keyword dynamic 
# @seealso \code{\link{generate_classification_data}}, \url{http://had.co.nz/classifly}
# @returns A \code{\link{invisible}} data frame of class \code{classifly} that contains all the simulated and true data.  This can be saved and then printed later to open with rggobi.
#X bsvm <- best.svm(Species~., data = iris, gamma = 2^(-1:1), cost = 2^(2:+ 4), probability=TRUE)
#X explore(bsvm, iris)
explore <- function(model, data, n=10000, method="nonaligned", advantage=TRUE, ...) {
	v <- variables(model)
	grid <- generate_classification_data(model, data, n=n, method=method, advantage=TRUE)
	actual <- data[,c(v$predictor, v$response)]
	actual[[".TYPE"]] <- factor("actual")
	
	data <- rbind.fill(grid, actual)
	class(data) <- c("classifly", class(data))
	attr(data, "variables") <- v
	data
}


# Print classifly object
# Opens with ggobi
# 
# @keyword internal 
print.classifly <- function(x, ...) {
	v <- attr(x, "variables")
	g <- ggobi(x)
	
	d <- g[1]
	glyph_colour(d) <- as.numeric(x[[v$response]]) + 1
	glyph_type(d) <- ifelse(x[[".TYPE"]] == "simulated", 1, 6)
	excluded(d) <- !is.na(x[[".ADVANTAGE"]]) & x[[".ADVANTAGE"]] > quantile(x[[".ADVANTAGE"]], 0.1, na.rm=TRUE)
	invisible(d)	
}