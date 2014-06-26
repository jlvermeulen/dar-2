doKNN <- function(test.dtm, train.dtm, samples, labels, k) {
	library(class)
	#test.dtm <- weightTfIdf(test.dtm)
	#train.dtm <- weightTfIdf(train.dtm)
	train.dtm <- as.data.frame(as.matrix(train.dtm) > 0)
	test.dtm <- as.data.frame(as.matrix(test.dtm) > 0)

	prediction <- knn(train.dtm, test.dtm, labels[samples], k)
	table(prediction, labels[-samples])
}