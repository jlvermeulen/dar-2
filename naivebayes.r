doNaiveBayes <- function(test.dtm, train.dtm, samples, labels) {
	library(e1071)
	train.dtm <- toBin(train.dtm)
	test.dtm <- toBin(test.dtm)

	nb <- naiveBayes(train.dtm, labels[samples], laplace = 1)
	prediction <- predict(nb, test.dtm)
	table(prediction, labels[-samples])
}

toBin <- function(dtm) {
	bin <- as.data.frame(as.matrix(dtm) > 0)
	for (i in 1:dim(bin)[2]) {
		bin[,i] = as.factor(bin[,i])
	}
	bin
}