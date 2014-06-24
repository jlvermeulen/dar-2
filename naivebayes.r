doNaiveBayes <- function(test.dtm, train.dtm, samples, labels) {
	library(e1071)
	train.bin <- toBin(train.dtm)
	test.bin <- toBin(test.dtm)

	nb <- naiveBayes(train.bin, labels[samples], laplace = 1)
	prediction <- predict(nb, test.bin)
	table(prediction, labels[-samples])
}

toBin <- function(dtm) {
	bin <- as.data.frame(as.matrix(dtm) > 0)
	for (i in 1:dim(bin)[2]) {
		bin[,i] = as.factor(bin[,i])
	}
	bin
}