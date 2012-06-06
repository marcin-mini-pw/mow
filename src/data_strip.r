#
# Plik zawiera funkcje odpowiedzialne za 
# odrzucenie czesci kategorii, oraz wektoryzacje
# kategorii. 
# Przyklad wektoryzacji, zalozmy ze mamy wiersz danych
# a1, a2, a3, ..., an, c1, c2, ..., ck
# a wiec nalezacy do k klas. Wiersz ten zostanie
# zamieniony na k wierszy z ktorych kazdy nalezy
# tylko i wylacznie do 1 kategorii.
# Zamiana wymuszona przez postac funkcji naiveBayes
# Dodatkowo zostana odrzucone kategoria 'OtherApplicationsNEC'
# oraz kategorie reprezentowane przez dane w niedostateczny sposob.
# 
# !mc
#

# FUNC is.column.name(s)
#
is.column.name <- function(s) {
	return(length( grep('^c\\.', s) ) > 0);
}

# FUNC is.invalid(column.name, invalid.cats.hash)
#
is.invalid <- function(column.name, invalid.categories.hash) {
	return(exists(column.name, envir=invalid.categories.hash, inherits=FALSE));
}


# FUNC vectorize.categories(text.data.frame, min.n, invalid.cats.hash = emptyenv())
#
# min.n - minimalna liczba wystapien danej kategorii
# invalid.cats.hash - nazwy kategori ktore powinny zostac
#  	odrzucone
#
# RETURN wektor logiczny l taki ze l[nr.kolumny] == T jezeli
# dana kategoria powinna wystepowac w ostatecznych danych
#
vectorize.categories <- function(text.data.frame, min.n, invalid.cats.hash) {
	column.names <- colnames(text.data.frame);
	result       <- logical( ncol(text.data.frame) );

	for(i in 1:ncol(text.data.frame)) {
		column.name <- column.names[i];
		if(is.column.name(column.name) && !is.invalid(column.name, invalid.cats.hash)) {
			number.of.arts <- text.data.frame[1, i];
			if(number.of.arts >= min.n) {
				result[i] = TRUE;
			}
		}
	}

	return(result);
}

# FUNC create.hash(str.vector)
#
create.hash <- function(str.vector) {
	e <- new.env(hash=TRUE, parent=emptyenv());
	
	for(s in str.vector) {
		assign(s, TRUE, envir=e);
	}

	return(e);
}

# FUNC get.df.column.indexes(df, cs)
#
# funkcja zwraca wektor indeksow kolumn nalezacych
# do klasy cs
# cs = 'c' dla kategori, cs = 'w' dla slow oraz
# cs = 'meta' dla informacji dodatkowych
#
get.df.column.indexes <- function(df, cs) {
	column.names  <- colnames(df);
	class.columns <- grep(paste("^",cs,"\\.",sep=''), column.names);

	return( (1:ncol(df)) [class.columns] );
}

# FUNC vectorize.data(df, min.n, invalid.cats.hash, min.w)
#
# funkcja przeksztalca ramke danych, na postac
# ktora moze zostac podana dla funkcji naiveBayes
#
# min.n - minimalna liczba wystapien danej kategorii
# invalid.cats.hash - nazwy kategori ktore powinny zostac
#  	odrzucone
# min.w - minimalna liczba wystapien danego slowa
# 
# RETURN lista zawierajaca $data - dane oraz $fact - factor
# bedacy lista kategori odpowiadajaca poszczegolnym wierszom danych
#
# format ramki danych (nazwy kolumn)
# w. w. w. ... w. c. ... c. meta. ... meta.
#
vectorize.data <- function(df, min.n=500, invalid.cats.hash=emptyenv(), min.w=2) {
	cats.names    <- colnames(df);
	cats.indexes  <- get.df.column.indexes(df, 'c');
	allow.in.data <- vectorize.categories(df, min.n, invalid.cats.hash);

	max.words.index <- min(cats.indexes) - 1;
	
	rows.vector <- integer(0);
	cats.vector <- character(0);

	# Pierszy wiersz ma specjalne znaczenie
	N = nrow(df);

	for(i in 2:N){
		cat(sprintf('I Faza: Wiersz %d / %d\r', i, N));
		flush(stdout());
		for(ci in cats.indexes) {
			if(allow.in.data[ci] && df[i, ci] > 0) {
				rows.vector <- c(rows.vector, i);
				cats.vector <- c(cats.vector, cats.names[ci]);
			}
		}
	}

	# Usuwamy te slow ktore wystepuja w zbyt malej liczbie
	#
	words.count  <- integer(max.words.index);
	for(wi in 1:max.words.index) {
		cat(sprintf('II Faza: Kolumna %d / %d\t\t\t\r', wi, max.words.index));
		flush(stdout());

		words.count[wi] = sum(df[rows.vector,wi]);
	}
	words <- (1:max.words.index)[words.count >= min.w];
	cat('\n');

	used.columns <- which(allow.in.data == TRUE);

	return(list(data=df[rows.vector, words], 
			  fact=factor(cats.vector),
			  rows.indexes=rows.vector,
			  org.classes=df[rows.vector,used.columns]));
}

# FUNC make.df.binary(df)
#
# funkcja zamienia ramke danych zawierajaca liczby
# na ramke zawierajaco 0 i 1 (dla liczb >= 0)
#
make.df.binary <- function(df) {
	for(i in 1:ncol(df)) {
		df[,i] <- sign(df[,i]);
	}
	return(df);
}





