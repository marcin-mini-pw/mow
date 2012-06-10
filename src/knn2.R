#
# Plik zawiera implementacje algorytmu knn
#
#

# FUNC metric.euclid(v1,v2) 
# 
# Funkcja liczy odleglosc pomiedzy wektorami zgodnie z metryka euklidesowa
#
# v1 - pierwszy wektor
# v2 - drugi wektor
#
# RETURN odleglosc pomiedzy wektorami 
#
metric.euclid <- function(v1,v2) {
  return (sqrt (sum ((v2 - v1)^2)))
}

# FUNC metric.cos(v1,v2) 
# 
# Funkcja liczy odleglosc pomiedzy wektorami zgodnie z metryka kosinusowa
#
# v1 - pierwszy wektor
# v2 - drugi wektor
#
# Wektory musza byc znormalizowane
#
# RETURN odleglosc pomiedzy wektorami wyrazona katem 
#
metric.cos <- function(v1,v2) {
  return ((sum (v1*v2)) / (sqrt (sum (v1^2)) * sqrt (sum (v2^2))))
}

vec.norm <- function(v) {
	return( v / sqrt(sum(v*v)) );
}

# FUNC knn2(data, fact, test.data, k)
#
# data - ramka danych 
# fact - klasyfikacja wierszy z data
# test.data - dane ktore nalezy zaklasyfikowac
# k - liczba sasiadow
# metric - uzyta metryka

knn2 <- function(data, fact, test.data, metric, k=5) {
	data <- as.matrix(data);
	test.data <- as.matrix(test.data);

	N  <- nrow(test.data);
	NR <- nrow(data);

	lvl    <- levels(fact);
	CN	  <- length(lvl);
	result <- character(N);

	for(r in 1:N) {
		cat(sprintf('Wiersz %d/%d\t\t\r', r, N));		
		test.row <- test.data[r, ];
		
		dists <- rep(0, nrow(data));
		for(i in 1:NR) {
			dists[i] <- metric(test.row, data[i, ]);
		}
		
		ord   <- order(dists,decreasing=FALSE);
		nbh   <- ord[1:k]; # numery sasiadow
		nbh.cls <- fact[nbh];

		class.votes <- sapply(1:CN, function(r) sum(nbh.cls == lvl[r]) );
		result[r] <- lvl[ which(class.votes == max(class.votes))[1] ];
	}

	tmp <- factor(result, lvl);
	cat('\n');
	return(tmp);
}