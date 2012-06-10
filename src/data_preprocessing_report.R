# 
# Przetwarzanie danych Etap I
#
# !mc: 16.05.2012
#

# NAZWY KATALOGOW POWINNY KONCZYC SIE /

# Sciezka do katalogu ktory zawiera rozpakowane
# archiwum Part1.zip
org.data.directory <- 'D:/tmp/words/Part1/'


# Sciezka do katalogu ktory bedzie zawieral
# przetworzone dane oraz dane tymczsowe
#
data.directory <- 'd:/mtmp/';

# -----------------------------------------------

# FUNC str.join
#
# funkcja jest aliasem dla wywolania
# paste(a, b, sep='')
#
str.join <- function(a, b) {
	return(paste(a, b, sep=''));
}


data.in  <- str.join(data.directory, 'data_original/');
data.out <- str.join(data.directory, 'data_preprocessed/');

# Tworzenie katalogow
dir.create(data.in, recursive=TRUE);
dir.create(data.out, recursive=TRUE);

# Kopiowanie plikow txt do katalogu data.in
cat('Kopiowanie plikow...\n');

flist  <- list.files(org.data.directory, recursive=TRUE, pattern='*.txt', full.names=TRUE);
for(f in flist) {
	cat(sprintf("Copy file %s\n", f));

	new.name <- str.join(data.in, basename(f));
	file.copy(f, new.name, overwrite=TRUE);
}


source('preprocess.R');
preprocess(data.in, data.out);
