#
# Funkcje zawarte w pliku odpowiadaja za przeksztalcenie 
# zbioru strzeszczen postaci (FieldOfApp, Abstract)
# na format *bag of words*. 
# W wyniku tego przeksztalcenia zbior plikow txt
# zostanie zamieniony na ramke danych o nastepujacym formacie
# * Wiersz o idenksie 1 zawiera LACZNA liczbe wystapien slow
#   oraz klas we wszystkich przetwarzanych dokumentach
# * Nastepne wiersze odpowiadaja kolejnym przetwarzanym plikom
#   streszczen
# Pierwsze N kolumn ramki danych o nazwach zaczynajacych sie od
# 'w.' odpowiada czestosciom wystepowania poszczegolnych slow w danym
# dokumencie. Kolejne kolumny rozpoczynajace sie do 'c.' zawieraja
# w komorkach 0 lub 1 okreslajace czy dany dokument nalezy do
# klasy reprezentowanej przez kolumne. Ostatnie kolumny o nazwach
# rozpoczynajacych sie od 'meta.' zawieraja dodatkowe informacje
# np. nazwe pliku (umozliwiajaca wyswietlenie jego zawartosci uzyt-
# kownikowi)
#
# Przyklad (dla slow word1, word2 i word3 oraz dla klas class1 i class2)
#
# | w.word1 | w.word2 | w.word3 | c.class1 | c.class2 | meta.filename |
# | 2       | 3       | 7       | 2        | 0        | NA            |
# | 1       | 2       | 3       | 1        | 0        | file1.txt     |
# | 1       | 1       | 4       | 1        | 0        | file2.txt     |
#
# !mc
#

# zeby uchronic sie od wplywu jezyka na sposob sortowania napisow
# ustawiamy locale na C
Sys.setlocale(category="LC_ALL", locale="C");

library('Rstem');

# FUNC read.all.lines(file.name)
# 
# funkcja wczytuje i zwraca wszystkie linie
# z pliku tekstowego o podanej nazwie
#
read.all.lines <- function(file.name) {
	handle <- file(file.name, "rt");
	lines  <- readLines(handle);
	close(handle);

	return(lines);
}

# FUNC get.article.categories(category.line)
# 
# funkcja zwraca wektor kategorii
# na podstawie lancucha znakow postaci
# "class1, class2, class3"
#
get.article.categories <- function(category.line) {
	category.vector <- strsplit(category.line, "[ \t]*,[ \t]*");
	return( category.vector[[1]] );
}

# FUNC read.preprocessed.article(file.name)
# 
# funkcja odpowiada za wczytanie wstepnie przetworzonego
# pliku artykulu (opis przetworzonego pliku patrz: preprocess.r)
# artykul zwracany jest w postaci listy zawierajacej pola
# $abstract - streszczenie artykulu, oraz $classes - wektor
# klas do ktorych dany artykol nalezy.
# w przypadku bledy zwracana jest wartosc NA
#
read.preprocessed.article <- function(file.name) {
	lines <- read.all.lines(file.name);
	if( length(lines) != 3 ) {
		stop(paste('invalid article file:', file.name));
	}

	categories <- get.article.categories(lines[1]);
	abstract   <- lines[3];

	return(list(abstract=abstract, categories=categories));
}


# FUNC stem.abstract.text(abstract.text)
#
# funkcja odpowiada za wykonanie opisanego w
# dokumentacji wstepenej przetwarzania tekstu
# na ktore sklada sie:
# * zamiana duzych liter na male 
# * usniecie znakow nie bedacych literami
# * dokonanie stemmingu algorytmem Portera
# funkcja zwraca wektor slow
# 
stem.abstract.text <- function(abstract.text) {
	abstract.text <- gsub("[^A-Za-z]+", " ", abstract.text);
	abstract.text <- tolower(abstract.text);

	# dzielimy tekst na poszczegolne slowa
	# wynik jest zwracany w postaci listy wektorow
	abstract.text <- strsplit(abstract.text, "[ \t]+");
	abstract.text <- abstract.text[[1]];
	
	# stemujemy poszczegolne slowa
	# wordStem obsluguje slowa do dlugosci 255
	# dluzsze usuwamy, dodatkowo podczas splitu
	# moga pojawic sie napis o 0 dlugosci
	MAX.WORD.LENGTH <- 255;
	abstract.text   <- abstract.text[
		nchar(abstract.text) < MAX.WORD.LENGTH &  # NIE MOZE BYC &&
		nchar(abstract.text) > 0
	];

	return(wordStem(abstract.text, language="english"));
}

# --------------------------------------------------
# ladowanie pliku do obslugi stopwords
# --------------------------------------------------
source('stopwords.R');

init.stop.words.dictionary();
# --------------------------------------------------

# FUNC remove.stop.words(text)
#
# funkcja usuwa z angielskiego tekstu znaki bedace
# tzw. stop words a wiec np. 'the', 'you'
#
# tekst jest reprezentowany jako wektor slow
# np. remove.stop.words(c("one", "two", "three"))
#
remove.stop.words <- function(text) {
	return( text[!is.stop.word(text)] );
}


# FUNC load.article(file.name)
#
# funkcja wczytuje artykul i zwraca 
# liste zawierajaca $abstract - wektor
# slow ze streszczenia poddanych stemmingowi oraz
# usuwaniu stopwords, $categories - wektor napisow
# okreslajacych poszczegolne kategorie arytkulu
#
load.article <- function(file.name) {
	art <- read.preprocessed.article(file.name);

	abstract <- stem.abstract.text(art$abstract);
	abstract <- remove.stop.words(abstract);

	categories <- art$categories;

	return( list(abstract=abstract, categories=categories) );
}

# FUNC insert.words(env, words)
#
# funkcja dodaje slowa z wektora words do slownika
# (environment) env
# przy okazji env[word] zawiera liczbe wystapien danego
# slowa w przetwarzanych teskstach
#
insert.words <- function(env, words) {
	for(w in words) {
		if(!exists(w, envir=env)) {
			assign(w, 1, envir=env);
		}
		else {
			assign(w, 1+get(w, envir=env), envir=env);
		}
	}
}




# FUNC compute.global.stats(articles.directory)
#
# funkcja przebiega po wszystkich plikach wstepnie 
# przetworzonych artykulow i buduje liste uzywanych 
# w artach slow oraz kategorii. podane listy zawieraja
# slowa i kategorie ze wszystkich przejrzanych plikow.
# funkcja zwraca liste z atrybutami $words - slownik uzywanych
# slow, $categories - slownik uzywanych kategorii
# $files.count - liczba przetworzonych plikow
# 
compute.global.stats <- function(articles.directory) {
	words 	 <- new.env(hash=TRUE, parent=emptyenv());
	categories <- new.env(hash=TRUE, parent=emptyenv());
	count 	 <- 0;

	articles <- list.files(
		path=articles.directory, 
		pattern="*.txt",
		full.names=TRUE,
		ignore.case=TRUE
		);

	for(a in articles) {
		cat(sprintf("[%s]\n", a));
		tmp <- load.article(a);

		insert.words(words, tmp$abstract);
		insert.words(categories, tmp$categories);
		count <- count + 1;
	}

	return( list(count=count, words=words, categories=categories) );
}

# META COLUMNS - dodatkowe kolumny ktore dodajemy do
# data frame'u
META.COLUMNS <- c('filename', 'wordcount');


# FUNC add.df.header(df.file, art.stats)
#
# funkcja zapisuje do pliku df.file nazwy kolumn
# dataframe'u na podstawie art.stats 
#
add.df.header <- function(df.file, art.stats) {
	word.col.names <- paste('w.', sort(ls(envir=art.stats$words)), sep='');
	cats.col.names <- paste('c.', sort(ls(envir=art.stats$categories)), sep='');

	meta.col.names <- paste('meta.', META.COLUMNS, sep='');

	cat( c(word.col.names, cats.col.names, meta.col.names), 
		file=df.file, sep='\t' );
	cat( '\n', file=df.file, sep='' );
}


# FUNC write.env.cols(env, file)
#
write.env.cols <- function(envir, file) {
	col.names <- sort(ls(envir=envir));

	for(c in col.names) {
		tmp <- get(c, envir=envir);
		cat(tmp, file=file, sep='');
		cat('\t', file=file, sep='');
	}
}

# FUNC add.df.stats.row(df.file, art.stats)
#
# funkcja dodaje pierwszy wiersz do data frame'u
# pierwszy wiersz zawiera statystyki dotyczace 
# calego zbioru artow np. ilosc wystapien danego slowa
# we wszystkich artach razem wzietych
#
add.df.stats.row <- function(df.file, art.stats) {
	write.env.cols(art.stats$words, df.file);
	write.env.cols(art.stats$categories, df.file);

	for(  i in 1:(length(META.COLUMNS)-1)  ) {
		cat('na\t', file=df.file, sep='');
	}
	cat('na\n', file=df.file, sep='');
}

# FUNC write.art.row.cols(aenv, genv, file)
#
write.art.row.cols <- function(aenv, genv, file) {
	col.names <- sort(ls(envir=genv));

	for(c in col.names) {
		if( exists(c, envir=aenv, inherits=FALSE) ) {
			tmp <- get(c, envir=aenv);
			cat(tmp, file=file, sep='');
		}
		else {
			cat('0', file=file, sep='');
		}

		cat('\t', file=file, sep='');
	}
}

# FUNC add.article.row(art, stats, file)
#
# funkcja odpowiada za dodanie wiersza danych odpowiadajacych
# pojedynczemu plikowi artykulu
#
add.article.row <- function(art, stats, file) {
	art.env <- new.env(hash=TRUE, parent=emptyenv());
	insert.words(art.env, art$abstract);

	category.env <- new.env(hash=TRUE, parent=emptyenv());
	insert.words(category.env, art$categories);

	write.art.row.cols(art.env, stats$words, file);
	write.art.row.cols(category.env, stats$categories, file);

	# specjalne kolumny
	cat(sprintf('"%s"\t', art$file.name), file=file, sep='');
	cat(sprintf('%d\n', length(art$abstract)), file=file, sep='');
}

# FUNC add.article.rows(file, articles.directory)
#
# funkcja odpowiada za dodanie wierszy reprezentujacych
# poszczegolne artykuly do dataframe'u
#
add.article.rows <- function(art.stats, file, articles.directory) {
	articles <- list.files(
		path=articles.directory, 
		pattern="*.txt",
		full.names=TRUE,
		ignore.case=TRUE
		);

	for(a in articles) {
		tmp <- load.article(a);
		tmp$file.name <- a; # dodaj info o nazwie pliku
		cat(sprintf('Adding row: %s\n', a));

		add.article.row(tmp, art.stats, file);
	}
}


# FUNC create.aritcles.data.frame(articles.directory, output.df.filename)
#
# funkcja przeksztalca katalog zawierajcy przetworzone wstepnie
# artykuly na pojedynczy plik zgody z formatem *data frame* jezyka R
# drugi parametr okresla lokalizacje pliku ramki
# 
create.articles.data.frame <- function(articles.directory, output.df.filename) {
	art.stats <- compute.global.stats(articles.directory);
	df.file   <- file(output.df.filename, "wt");
	
	add.df.header(df.file, art.stats);
	add.df.stats.row(df.file, art.stats);

	add.article.rows(art.stats, df.file, articles.directory);	

	close(df.file);
}



