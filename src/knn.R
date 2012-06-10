#
# Klasyfikator K-NN
#

# FUNC metric.euclid(v1,v2) 
# 
# Funkcja liczy odległość pomiędzy wektorami zgodnie z metryką euklidesową.
#
# v1 - pierwszy wektor
# v2 - drugi wektor
#
# RETURN odległość pomiędzy wektorami 
#
metric.euclid <- function(v1,v2) {
  return (sqrt (sum ((v2 - v1)^2)))
}

# FUNC metric.cos(v1,v2) 
# 
# Funkcja liczy odległość pomiędzy wektorami zgodnie z metryką kosinusową.
#
# v1 - pierwszy wektor
# v2 - drugi wektor
#
# RETURN odległość pomiędzy wektorami wyrażona kontem  
#
metric.cos <- function(v1,v2) {
  return ((sum (v1*v2)) / (sqrt (sum (v1^2)) * sqrt (sum (v2^2))))
}

# FUNC data.compact(data) 
# 
# Funkcja kompresuje dane (usuwa kolumny z wartością zero) na potrzeby
# algorytmu knn.
#
# data - tablica danych 
#
# RETURN lista ramek reprezentująca indeksy słów i ich liczność.
#
data.compact <- function (data){
  vectors <- list()

  for (i in 1:nrow(data)){
    zeroes <- data[i,] == 0
    id <- (1:ncol(data))[!zeroes]
    count <- data[i,][!zeroes]
    vec <- data.frame (id=id, count=count)
    vectors <- append (vectors, list(vec))
  }

  return (vectors)
}

# FUNC train.knn(data, fact)
#
# data - ramka danych zawierajaca liczby
# fact - faktor zawierajacy poprawna klasyfikacje
# tekstow nalezacych do data
#
# RETURN obiekt klasy ptmKnn do uzycia w funkcji predict
#
train.knn <- function(data, fact, k=3, metric=metric.cos) {

  vectors <- data.compact(data)
  model <- list(vectors=vectors, fact=fact, k=k, metric=metric)

  class(model) <- 'ptmKnn'
  return (model)
}

# FUNC predict.ptmKnn(model, data)
#
# Rozszerzenie funkcji generycznej predict.  Obiekt model musi byc lista
# zawierajaca następujące pola $vectors $fact $k $metric bedace odpowiednio
# niezerową ramką danych zliczającą słowa, wektorem kalsyfikacji, wartoscia k i
# metryka.  data musi zawierac dane niezbene do klasyfikacji (data MUSI
# zawierac miec IDENTYCZNA strukturę co dane uzyte przy nauce klasyfikatora)
#
# RETURN wektor która dla kazdego wiersza danych zwraca 
# wiersz etykietę kategorii.
#
predict.ptmKnn <- function(model, data) {
  
  result <- c()
  cdata <- data.compact(data)

  apply.metric <- function (v1,v2,metric){
    mock.v2 <- data.frame (id=v2$id, count=v2$count*0)
    m.v1 <- match(v1$id, v2$id)
    c.v1 <- rbind (v1[is.na(m.v1),], v1[!is.na(m.v1),], mock.v2[-m.v1[!is.na(m.v1)],])
    mock.v1 <- data.frame (id=v1$id, count=v1$count*0)
    m.v2 <- match (v2$id, v1$id)
    c.v2 <- rbind (mock.v1[-m.v2[!is.na(m.v2)],], v2[is.na(m.v2),], v2[!is.na(m.v2),])

    return (metric (c.v1, c.v2))
  }

  # przeglądamy wszystkie testowane przypadki
  for (i in 1:nrow(data)){
    distances <- sapply (model$vectors, apply.metric, cdata[[i]], model$metric)
    order <- order(distances)
    voters <- table(model$fact[order[1:model$k]])
    voteing <- as.data.frame(voters)
    voteing_order <- rev(order(voteing$Freq))
    voteing_res <- voteing[voteing_order[1],1]
    result <- append(result, levels(model$fact)[voteing_res])
  }
	return (result)
}

