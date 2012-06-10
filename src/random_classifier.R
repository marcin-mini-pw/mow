# 
# Losowy klasyfikator
#


# FUNC random.predict(factor.levels, n)
#
# factor - factor zwierajacy poprawna klasyfikacje
#          elementow
# n - ilosc klasyfikowanych elementow
#
# RETURN factor zawierajacy n elementow, do uzycia
# w funkcji table()
#
random.predict <- function(factor, n) {
	lbl <- levels(factor);
	cn  <- length(lbl);

	tmp <- runif(n, min=0, max=cn);
	tmp <- as.integer(floor(tmp));
	tmp <- ifelse(tmp == cn, cn-1, tmp);

	return( factor(lbl[tmp+1], levels=lbl) );
 }