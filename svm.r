doSVM <- function(test.dtm, train.dtm, samples, labels) {
	library(e1071)
	#test.dtm <- weightTfIdf(test.dtm)
	#train.dtm <- weightTfIdf(train.dtm)

	model <- svm(x = train.dtm, y = labels[samples], kernel = "linear", cost = 10, gamma = 0.1)
	prediction <- predict(model, test.dtm)
	table(prediction, labels[-samples])
}