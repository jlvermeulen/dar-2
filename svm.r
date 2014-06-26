doSVM <- function(test.dtm, train.dtm, samples, labels) {
	library(e1071)
	#test.dtm <- weightTfIdf(test.dtm)
	#train.dtm <- weightTfIdf(train.dtm)
	train.dtm <- as.data.frame(as.matrix(train.dtm) > 0)
	test.dtm <- as.data.frame(as.matrix(test.dtm) > 0)

	model <- svm(x = train.dtm, y = labels[samples], kernel = "sigmoid", gamma = 0.001, cost = 10)
	prediction <- predict(model, test.dtm)
	table(prediction, labels[-samples])
}