#
# Klasyfikator K-NN
#

# FUNC train.knn(data, fact)
#
# data - ramka danych zawierajaca liczby
# fact - faktor zawierajacy poprawna klasyfikacje
# tekstow nalezacych do data
#
# RETURN obiekt klasy mcTbnb do uzycia w funkcji predict
#
train.knn <- function(data, fact, k=3, metric=metric.cos) {
  m <- list(data=data, fact=fact, k=k, metric=metric)
  class(m) <- 'ptmKnn'
  return (m)
 }

# FUNC predict.ptmKnn(model, data)
#
# Rozszerzenie funkcji generycznej predict.
# Obiekt model musi byc lista zawierajaca
# następujące pola $data $fact $k $metric bedace odpowiednio
# tablica danych, wektorem kalsyfikacji, wartoscia k i metryka.
# data musi zawierac dane niezbene do klasyfikacji
# (data MUSI zawierac miec IDENTYCZNA strukture co
# dane uzyte przy nauce klasyfikatora)
#
# RETURN wektor która dla kazdego wiersza danych zwraca 
# wiersz etykietę kategorii.
#
predict.ptmKnn <- function(model, data) {

	return (rep (model$fact[1], nrow(data)));
}

