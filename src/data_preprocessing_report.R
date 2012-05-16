# 
# Raport ze wstepnego przetwarzania danych
#
# !mc: 16.05.2012
#

# Sciezka do katalogu zawierajacego dane
data.directory <- 'd:/mow/';

# FUNC str.join
#
# funkcja jest aliasem dla wywolania
# paste(a, b, sep='')
#
str.join <- function(a, b) {
	return(paste(a, b, sep=''));
}

source('preprocess.r');
preprocess(
	str.join(data.directory, 'data_original'), 
	str.join(data.directory, 'data_preprocessed')
	);