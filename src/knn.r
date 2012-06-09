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
metric.euc <- function(v1,v2) {
  v3 <- v2 - v1
  v3 <- v3^2
  dist <- sum(v3)
  return (sqrt(dist))
}

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
  
  result <- c()

  # przeglądamy wszystkie testowane przypadki
  for (i in 1:nrow(data)){
    distances <- sapply (model$data, model$metric, data[i,])
    order <- rev(order(distances))
    voters <- table(model$fact[order[1:model$k]])
    voteing <- as.data.frame(voters)
    voteing_order <- rev(order(voteing$Freq))
    voteing_res <- voteing$voters[voteing_order[1]]
    voteing_res <- 1 
    
    result <- append(result, voteing_res)
  }
	return (result)
}

