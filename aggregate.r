source("main.r")

aggregate <- function(func, gens = genres, builder = buildCorpora, iterations = 10) {
	result <- matrix(rep(0, each = length(gens) ^ 2), length(gens), length(gens))
	for (i in 1:iterations) {
		result <- result + func(gens, builder)
	}
	result <- (result * 100 / iterations) / (genreSize - trainSize)
	avg <- 0
	for (i in 1:length(gens)) {
		avg <- avg + result[i, i]
	}
	avg <- avg / length(genres)
	list(average = avg, table = result)
}