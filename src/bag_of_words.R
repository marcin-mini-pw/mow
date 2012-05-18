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
source('stopwords.r');

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




