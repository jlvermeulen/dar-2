doKNN <- function(test.dtm, train.dtm, samples, labels, k) {
	library(class)
	test.dtm <- weightTfIdf(test.dtm)
	train.dtm <- weightTfIdf(train.dtm)

	prediction <- knn(train.dtm, test.dtm, labels[samples], k)
	table(prediction, labels[-samples])
}