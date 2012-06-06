#
# Plik zawiera funkcje odpowiedzialne
# za obliczanie bledu klasyfikacji
#


# FUNC mclass.error(p.table, test.rows, lst)
#
# funkcja oblicza blad klasyfikacji w przypadku
# gdy klasyfikator zwraca tablice p-stwa przynaleznosci
# przykladow do poszczegolnych klas
#
# p.table - tabela [logarytmow] prawdopodobienstw
# przynaleznosci wiersza do poszczegolnych klas
# test.rows - indeksy wierszy dla ktorych
# dokonano klasyfikacji
# lst - dane uczace oraz testowe, w formie
# zwroconej przez wectorize data
#
# RETURN lista zawierajaca $good - liczbe
# poprawnie zaklasyfikowanych przykladow
# oraz $bad - liczbe zle zaklasyfikowanych 
# przykladow
#
mclass.error <- function(p.table, test.rows, lst) {
	pfi <- apply(p.table, 1, function(r) which(r == max(r)));
	lvl <- levels(lst$fact);
	class.vec <- lvl[pfi];

	return( class.error(class.vec, test.rows, lst) );
}

# FUNC class.error(class.vec, test.rows, lst) 
#
# funkcja oblicza blad klasyfikacji w przypadku
# gdy klasyfikator zwraca dla danego przykladu etykiete
# klasy
#
# class.vec - faktor zawierajacy poprawna klasyfikacje
# test.rows - zbior wierszy na ktorych dokonano 
# klasyfikacji
# lst - zbior danych testowych, uczacych oraz
# inforamcji pomocniczych zwrocony przez
# vectorize.data
#
# RETURN lista zawierajaca $good - liczbe
# poprawnie zaklasyfikowanych przykladow
# oraz $bad - liczbe zle zaklasyfikowanych 
# przykladow $skipped - liczbe przykladow pominietych
# poniewaz byly one powtorzeniami wczesniej
# przetwrzonego wiersza
#
class.error <- function(class.vec, test.rows, lst) {
	good <- 0;
	bad <- 0;
	skipped <- 0;

	ri 		  <- lst$rows.indexes;
	org.classes <- lst$org.classes;

	total.rows <- nrow(lst$data);
	last.row   <- -1;
	i <- 0;

	tmp       <- order(test.rows);
	test.rows <- test.rows[tmp];
	class.vec <- class.vec[tmp];
	for(r in test.rows) {
		i <- i + 1;
		cat(sprintf("Wiersz: %d/%d\t\t\r", r, total.rows));

		if(last.row == ri[r]) {
			skipped <- skipped + 1;
			next; # continue for(r ...
		}
		last.row = ri[r];

		row.class <- class.vec[i];
		if(org.classes[r, row.class] == 1) {
			good <- good + 1;
		} else {
			bad <- bad + 1;
		}
	}

	cat('\n');
	return(list(good=good, bad=bad, skipped=skipped ));
}


