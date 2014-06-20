library(tm)

genreSize <- 20
trainSize <- 10
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

getLabels <- function() {
	as.factor(rep(genres, each = genreSize))
}

preprocess <- function(lyrics) {
	lyrics <- tm_map(lyrics, stripWhitespace)
	for(i in 1:length(lyrics)) {
      lyrics[[i]]$content <- tolower(lyrics[[i]]$content)
    }
	lyrics <- tm_map(lyrics, removeWords, stopwords("english"))
	lyrics <- tm_map(lyrics, removePunctuation)
	lyrics
}

trainNaiveBayes <- function(lyrics, samples) {
	dtm <- DocumentTermMatrix(lyrics[samples])
	dtm <- removeSparseTerms(dtm, 0.9)
	dtm
}

testNaiveBayes <- function(lyrics, train.dtm, samples) {
	labels <- getLabels()
	dict <- dimnames(train.dtm)[[2]]

	test.dtm <- DocumentTermMatrix(lyrics[-samples], list(dictionary = dict))

	train.bin <- toBin(train.dtm, dict)
	test.bin <- toBin(test.dtm, dict)

	library(e1071)
	nb <- naiveBayes(train.bin, labels[samples], laplace = 1)
	prediction <- predict(nb, test.bin)
	table(prediction, labels[-samples])
}

toBin <- function(dtm, dict) {
	bin <- as.data.frame(as.matrix(dtm) > 0)
	for (i in dict) {
		bin[,i] = as.factor(bin[,i])
	}
	bin
}

getSamples <- function() {
	offset <- 1
	samples <- c()
	for (i in genres) {
		samples <- append(samples, sample(offset:(offset + genreSize - 1), trainSize))
		offset <- offset + genreSize
	}
	samples
}

reload <- function() {
	rm(list=ls())
	source("naivebayes.r")
}

doStuff <- function() {
	lyrics <- preprocess(buildCorpora())
	samples <- getSamples()
	nb <- trainNaiveBayes(lyrics, samples)
	test <- testNaiveBayes(lyrics, nb, samples)
	test
}

naivebayes.result <- doStuff()