doSVM <- function(test.dtm, train.dtm, samples, labels) {
	library(MASS)
	test.dtm <- weightTfIdf(test.dtm)
	train.dtm <- weightTfIdf(train.dtm)

	model <- svm(x = train.dtm, y = labels[samples])
	prediction <- predict(model, test.dtm)
	table(prediction, labels[-samples])
}