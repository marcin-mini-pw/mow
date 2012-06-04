#
# Plik zawiera metody train.multinominal.nb oraz train.bernoulli.nb
# ktore buduja odpowiednio modele korzystajac ze wzoru bayesa oraz
# ze wzoru bernoulli'ego. Wiecej informacji na temat algorytmow
# znajduje sie w ksizce: Introduction to Information Retrieval
# strony: 253 - 265 (rozdzia 13)
#
# Format danych wejsciowych:
# I) Ramka danych zawierajaca teksty w fomacie bag-of-word
# II) Faktor zawierajcy kategrie dla kazdego wiersza zbioru 
#  uczacego (1 kategoria na wiersz)
#
# !mc
#

# FUNC predict.mcTmnb(model, data)
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
predict.mcTmnb <- function(model, data) {
	result <- matrix(nrow=nrow(data), ncol=ncol(model$condprob));

	for(c in 1:ncol(model$condprob)) {
		cat(sprintf("\nKlasa nr %d:\n", c));
		for(i in 1:nrow(data)) {
			cat(sprintf("\tWiersz: %d\t\t\r", i));
			flush(stdout());

			tmp <- log( model$prior[c] );
			tmp <- tmp + sum(
				# ifelse(data[i,] > 0, log(model$condprob[,c]), 0)
				data[i,] * log(model$condprob[,c])
			);
			
			#for(t in 1:ncol(data)) {
			#	if(data[i, t] > 0)
			#		tmp <- tmp + log( model$condprob[t,c] );
			#}

			result[i, c] <- tmp;
		}
	}

	return(result);
}

predict.mcTmnb.v2 <- function(model, data) {
	result <- matrix(nrow=nrow(data), ncol=ncol(model$condprob));
	log.prior <- log(model$prior);
	log.condprob <- log(model$condprob);

	for(i in 1:nrow(data)) {
		cat(sprintf("Wiersz: %d\t\t\r", i));
		flush(stdout());
		
		d <- data[i, ];
		for(c in 1:ncol(model$condprob)) {	
			tmp <- log.prior[c];
			tmp <- tmp + sum(
				d * log.condprob[,c]
			);

			result[i, c] <- tmp;
		}
	}

	cat('\n');
	return(result);
}

# FUNC train.multinominal.nb(data, fact)
#
# data - ramka danych zawierajaca liczby
# fact - faktor zawierajacy poprawna klasyfikacje
# tekstow nalezacych do data
#
# RETURN obiekt klasy mcTmnb do uzycia w funkcji predict
#
train.multinomial.nb <- function(data, fact) {
	lvl         <- levels(fact);
	class.count <- length(lvl);
	words.count <- ncol(data);

	prior <- rep(NA, class.count);
	condprob <- matrix(nrow=words.count, ncol=class.count);

	# Budowa modelu
	N <- nrow(data);
	for(c in 1:class.count) {
		cat(sprintf("\nKlasa %s:\n", lvl[c]));
		Nc <- sum(fact == lvl[c]); # Ilosc wystapien klasy c
		prior[c] <- Nc / N;

		tmp.df <- data[which(fact == lvl[c]),];
		S <- words.count + sum(tmp.df);
		for(t in 1:words.count) {
			cat(sprintf("\tKolumna: %d\t\t\r", t));
			flush(stdout());

			tmp <- data[, t];
			condprob[t, c] <- 
				(1 + sum(tmp[which(fact == lvl[c])])) / S;
		}
	}

	cat('\n');
	l <- list(prior=prior,condprob=condprob);
	class(l) <- 'mcTmnb';
	return(l);
}

# FUNC predict.mcTbnb(model, data)
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
predict.mcTbnb <- function(model, data) {
	result <- matrix(nrow=nrow(data), ncol=ncol(model$condprob));

	for(c in 1:ncol(model$condprob)) {
		cat(sprintf("\nKlasa nr %d:\n", c));
		for(i in 1:nrow(data)) {
			cat(sprintf("\tWiersz: %d\t\t\r", i));
			flush(stdout());

			tmp <- log( model$prior[c] );
			tmp <- tmp + sum(
				log( ifelse(data[i,] > 0, model$condprob[,c], 1 - model$condprob[,c]) )
			);
			

			result[i, c] <- tmp;
		}
	}

	return(result);
}

# FUNC train.bernoulli.nb(data, fact)
#
# data - ramka danych zawierajaca liczby
# fact - faktor zawierajacy poprawna klasyfikacje
# tekstow nalezacych do data
#
# RETURN obiekt klasy mcTbnb do uzycia w funkcji predict
#
train.bernoulli.nb <- function(data, fact) {
	lvl         <- levels(fact);
	class.count <- length(lvl);
	words.count <- ncol(data);

	prior <- rep(NA, class.count);
	condprob <- matrix(nrow=words.count, ncol=class.count);

	# Budowa modelu
	N <- nrow(data);
	for(c in 1:class.count) {
		cat(sprintf("\nKlasa %s:\n", lvl[c]));
		Nc <- sum(fact == lvl[c]); # Ilosc wystapien klasy c
		prior[c] <- Nc / N;

		for(t in 1:words.count) {
			cat(sprintf("\tKolumna: %d\t\t\r", t));
			flush(stdout());

			Nct <- sum(data[,t] > 0 & fact == lvl[c]);
			condprob[t, c] <- (Nct + 1) / (Nc + 2);
		}
	}

	cat('\n');
	l <- list(prior=prior,condprob=condprob);
	class(l) <- 'mcTbnb';
	return(l);
}
