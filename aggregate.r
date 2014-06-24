aggregate <- function(func, iterations = 10) {
	result <- matrix(rep(0, each = length(genres) ^ 2), length(genres), length(genres))
	for (i in 1:iterations) {
		result <- result + func()
	}
	(result * 100 / iterations) / (genreSize - trainSize)
}