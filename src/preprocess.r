# preproces.r
#
# Plik odpowiada za wstepne przetworzenie artykulow 
# naukowych do postaci:
#
# Pierwsza linijka zawiera kategorie artykulu naukowego
# Pusta linia
# Trzecia linijka zawiera tekst strzeszczeni bez znakow przestankowych
# i liczb
#
# !mc
#

# FUNC append.slash(dir.path)
#
# funkcja dodaje na koncu sciezki dir.path
# \ jezeli nie zostal juz dodany
#
append.slash <- function(dir.path) {
	if(length(grep("[\\/]$", dir.path)) > 0)
		return(dir.path);
	return(paste(dir.path, '/', sep=''));
}


# FUNC trim.lines
#
# funkcja usuwa biale znaki z poczatku i konca
# oraz ze srodka linii. usuwane znaki ze
# srodka sa zastepowane pojedyncza spacja
# funkca operuje na zbiorze linii
#
trim.lines <- function(lines) {
	lines <- gsub("[ \t\n\r]+", " ", lines);
	lines <- gsub("(^[ ]+)|([ ]+$)", "", lines);
	return(lines);
}

# FUNC cat.lines
#
# funkcja laczy stringi w wektorze
# lines w jedna dluga linje
#
cat.lines <- function(lines, sep=' ') {
	result <- '';

	for(line in lines) {
		result <- paste(result, line, sep=sep);
	}
	return(result);
}

# FUNC extract.field.of.application.lines
# 
# wyodrebnia z pliku linie zwiazane z wlasciwoscia
# Fld Applictn
#
extract.field.of.application.lines <- function(lines) {
	tmp <- grep("^Fld Applictn[ \t]*:", lines);
	start <- tmp[1];

	stop  <- NA;
	for(i in (start+1):length(lines)) {
		if(length(grep(":", lines[i])) > 0) {
			stop <- i - 1;
			break;
		}
	}

	return( lines[start:stop] );
}

# FUNC extract.field.of.application
#
# funkcja wydobywa pole zastosowan ze streszczenia.
# przykladowa postac pola:
# |Fld Applictn: 0302000   Biological Pest Control                 
# |              45        Ecology 
#
# streszczenie jest dostarczane funkcji w postaci
# wektora liniji
#
extract.field.of.application <- function(lines) {
	lines <- extract.field.of.application.lines(lines);
	
	lines <- gsub("^Fld Applictn[ \t]*:[ \t]*", "", lines);
	lines <- gsub("^[ \t]*[0-9]+[ \t]*", "", lines);
	lines <- gsub("[^A-Za-z0-9\n\t ]+", ".", lines);

	lines <- trim.lines(lines);
	
	tmp   <- '';
	if(length(lines) > 1)
		tmp <- cat.lines(lines[2:length(lines)], sep=', ');

	return(paste(lines[1], tmp, sep=''));
}

# FUNC extract.abstract(lines)
#
# funkcja wydobywa abstract z pliku streszczenia
#
# streszczenie jest dostarczane funkcji w postaci
# wektora liniji
#
extract.abstract <- function(lines) {
	results <- grep("^Abstract[ \t]*:[ \t]*$", lines);
	
	abstract.header.line <- results[1];
	abstract.lines 	  <- lines[(abstract.header.line+1):length(lines)];
	

	abstract.lines <- cat.lines(abstract.lines);
	return(trim.lines(abstract.lines));
}

# FUNC extract.information(lines)
# 
# funkcja dokonuje opisanego w naglowku pliku
# przeksztalcenia streszczenia artykulu nukowego 
#
# lines - to wektor napisow reprezentujacych
# plik ze streszczeniem
#
extract.information <- function(lines) {
	field.of.app <- extract.field.of.application(lines);
	abstract <- extract.abstract(lines);

	cat(field.of.app);
	return(c(field.of.app, "", abstract));
}


# FUNC preprocess.article(full.input.path, full.output.path)
#
preprocess.article <- function(full.input.path, full.output.path)  {
	cat(sprintf("Open: %s, Save: %s\n", full.input.path, full.output.path));
	
	handle <- file(full.input.path, "rt");
	lines  <- readLines(handle);
	close(handle);

	lines <- extract.information(lines);

	handle <- file(full.output.path, "wt");
	writeLines(lines, handle);
	close(handle);
}

# FUNC preprocess(input.path, output.path)
#
# input.path - Sciezka do katalogu zawierajacego
# pliki txt z surowymi tekstami artkulow
#
# output.path - Sciezka do katalogu w ktorym zostana
# zapisane przetworzone pliki
#
preprocess <- function(input.path, output.path) {

	input.path  <- append.slash(input.path);
	output.path <- append.slash(output.path);

	articles <- list.files(
		path=input.path, 
		pattern="*.txt",
		full.names=FALSE,
		ignore.case=TRUE
		);

	for(a in articles) {
		full.input.path  <- paste(input.path, a, sep='');
		full.output.path <- paste(output.path, a, sep='');
		preprocess.article(full.input.path, full.output.path);
	}

}
