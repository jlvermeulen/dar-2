library(tm)

genreSize <- 50
trainSize <- 25
genres <- c("Blues", "Country", "Folk", "Gospel", "Metal", "R&B", "Rap", "Soul")

buildCorpora <- function() {
	lyrics.blues	<- Corpus(DirSource("./Training Data/Blues"))
	lyrics.country	<- Corpus(DirSource("./Training Data/Country"))
	lyrics.folk		<- Corpus(DirSource("./Training Data/Folk"))
	lyrics.gospel	<- Corpus(DirSource("./Training Data/Gospel"))
	lyrics.metal	<- Corpus(DirSource("./Training Data/Metal"))
	lyrics.rnb		<- Corpus(DirSource("./Training Data/R&B"))
	lyrics.rap		<- Corpus(DirSource("./Training Data/Rap"))
	lyrics.soul		<- Corpus(DirSource("./Training Data/Soul"))
	lyrics 			<- c(lyrics.blues, lyrics.country, lyrics.folk, lyrics.gospel, lyrics.metal, lyrics.rnb, lyrics.rap, lyrics.soul)
	lyrics
}

getLabels <- function(gens) {
	as.factor(rep(gens, each = genreSize))
}

getSamples <- function(gens) {
	offset <- 1
	samples <- c()
	for (i in gens) {
		samples <- append(samples, sample(offset:(offset + genreSize - 1), trainSize))
		offset <- offset + genreSize
	}
	samples
}

preprocess <- function(lyrics) {
	lyrics <- tm_map(lyrics, stripWhitespace)
	for(i in 1:length(lyrics)) {
		lyrics[[i]]$content <- tolower(lyrics[[i]]$content)
	}
	lyrics <- tm_map(lyrics, removeWords, stopwords("english"))
	lyrics <- tm_map(lyrics, removePunctuation)
	lyrics <- tm_map(lyrics, removeNumbers)
	#lyrics <- tm_map(lyrics, stemDocument)
	lyrics
}

trainDTM <- function(lyrics, samples) {
	dtm <- DocumentTermMatrix(lyrics[samples])
	dtm <- removeSparseTerms(dtm, 0.9)
	dtm
}

testDTM <- function(train.dtm, lyrics, samples) {
	dict <- dimnames(train.dtm)[[2]]
	DocumentTermMatrix(lyrics[-samples], list(dictionary = dict))
}

reload <- function() {
	rm(list = ls())
	source("main.r")
}

buildData <- function(builder, gens) {
	lyrics <- preprocess(builder())
	samples <- getSamples(gens)
	labels <- getLabels(gens);
	train.dtm <- trainDTM(lyrics, samples)
	test.dtm <- testDTM(train.dtm, lyrics, samples)
	list(lyrics = lyrics, samples = samples, labels = labels, train.dtm = train.dtm, test.dtm = test.dtm)
}

testNaiveBayes <- function(gens, builder) {
	source("naivebayes.r")
	options(warn = -1)
	data <- buildData(builder, gens)
	result <- doNaiveBayes(data$test.dtm, data$train.dtm, data$samples, data$labels)
	options(warn = 0)
	result
}

testKNN <- function(gens, builder, k = 9) {
	source("knn.r")
	options(warn = -1)
	data <- buildData(builder, gens)
	result <- doKNN(data$test.dtm, data$train.dtm, data$samples, data$labels, k)
	options(warn = 0)
	result
}

testSVM <- function(gens, builder) {
	source("svm.r")
	options(warn = -1)
	data <- buildData(builder, gens)
	result <- doSVM(data$test.dtm, data$train.dtm, data$samples, data$labels)
	options(warn = 0)
	result
}