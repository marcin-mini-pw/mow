#
# Plik zawiera implementacje algorytmu
# Naiwnego Klasyfikatora Bayesowskiego
#
# !mc
#


# FUNC train.naive.bayes(data, fact)
#
# data - ramka danych zawierajaca liczby
# fact - faktor zawierajacy poprawna klasyfikacje
# tekstow nalezacych do data
# p    - stala m-estymacji (-1 oznacza wykorzystanie
# wartosci domyslnej zaleznej od rozmiaru danych
# uczacych)
#
# RETURN obiekt klasy mcTmnb do uzycia w funkcji predict
#
train.naive.bayes <- function(data, fact, p=-1) {
	data = sign(data); # Zamiana na wartosci binarne

	lvl <- levels(fact);
	CN  <- length(lvl);
	A   <- ncol(data);

	prior <- rep(0, CN);
	N     <- nrow(data);
	condprob <- matrix(ncol=ncol(data), nrow=2*CN);

	for(c in 1:CN) {
		Nc       <- sum(fact == lvl[c]);
		prior[c] <- Nc / N;
		pc <- ifelse(p == -1, 1/Nc, p);

		class.rows <- which(fact == lvl[c]);
		for(a in 1:A) {
			condprob[2*c-1, a] = 
				(sum(data[class.rows, a] == 0) + pc*Nc) / (2*Nc);
			condprob[2*c, a] = 
				(sum(data[class.rows, a] == 1) + pc*Nc) / (2*Nc);
		}	
	}

	model <- list(prior=prior, condprob=condprob);
	class(model) <- 'mcNb';
	return(model);
}

# FUNC predict.mcNb(model, data)
#
# Rozszerzenie funkcji generycznej predict.
# Obiekt model musi byc lista zawierajaca
# dwa pola $prior $condprob bedace odpowiednio
# wektorem oraz macierza.
# data musi zawierac dane niezbene do klasyfikacji
# (data MUSI zawierac miec IDENTYCZNA strukture co
# dane uzyte przy nauce klasyfikatora)
#
# RETURN macierz ktora dla kazdego wiersza danych zwraca 
# wiersz okreslajacy p-stwo przynaleznosci do danej ktegorii
# Kolumny sa uporzadkowoane zgodnie z porzadkiem leveli  faktora
# zwracanym przez levels
#
predict.mcNb <- function(model, data) {
	data <- sign(as.matrix(data));

	CN <- length(model$prior);
	A  <- ncol(data);
	result <- matrix(nrow=nrow(data), ncol=CN);

	N <- nrow(data);
	log.cp <- log(model$condprob);
	log.prior <- log(model$prior);

	for(r in 1:N) {
		cat(sprintf("Wiersz %d/%d\t\t\r", r, N));
	
		for(c in 1:CN) {
			tmp <- log.cp[c];
			tmp <- tmp + 
				sum( ifelse(data[r,] == 0, log.cp[2*c-1,], log.cp[2*c,]) );
			result[r, c] <- tmp;
		}
	}

	cat('\n');
	return(result);
}




