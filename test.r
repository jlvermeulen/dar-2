source("aggregate.r")

testPairs <- function(func) {
	result <- matrix(rep(0, each = length(genres) ^ 2), length(genres), length(genres))
	g <- genres
	x <- 1
	y <- 1
	print("before")
	for (i in 1:(length(g) - 1)) {
		for (j in (i + 1):length(g)) {
			Sys.sleep(0.1)
			print(paste(i, j, sep=","))
			build <- function() {
				a <- Corpus(DirSource(paste("./Training Data/", g[i], sep = "")))
				b <- Corpus(DirSource(paste("./Training Data/", g[j], sep = "")))
				c(a, b)
			}
			gen <- c(g[i], g[j])
			r <- aggregate(func, gen, build)
			result[i, j] <- r$table[1, 1]
			result[j, i] <- r$table[2, 2]
			y <- y + 1
		}
		y <- 1
		x <- x + 1
	}
	print("after")
	result
}