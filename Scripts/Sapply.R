
#SAPPLY
#--------------------------------------

cities <- c("New York", "Paris", "London", "Tokyo", "Rio de Janeiro", "Cape Town")

sapply(cities, nchar) # Obtenemos un vector

first_and_last <- function(name) {
  name <- gsub(" ", "", name) # Elimina los espacios en blanco
  letters <- strsplit(name, split = "")[[1]] #Separa ciudades en vector.
  c(first = min(letters), last = max(letters)) 
} #Devuelve la letra “menor” y la “mayor” de la ciudad.

sapply(cities, first_and_last) # Obtenemos una matriz 



#Ejercicios

'Muestras de temperatura de cada día de la semana'
temp <- list(monday = c(3, 7, 9, 6, -1), tuesday = c(6, 9, 12, 13, 5), wednesday = c(4, 8, 3, -1, -3), thursday = c(1, 4, 7, 2, -2), friday = c(5, 7, 9, 4, 2), saturday = c(-3, 5, 8, 9, 4), sunday = c(3, 6, 9, 4, 1))
temp

'Utiliza lapply para encontrar la temperatura mínima de cada día.'
#Lapply me devuelve una lista, con una lista no puedo operar
minima_l <- lapply(temp, min)
minima_l


'Utiliza sapply para encontrar la temperatura mínima de cada día.'
#Me devuelve un vector con caracter numerico
minima_s <- sapply(temp, min)
minima_s

'Crea una función que calcule la media entre el día mínimo y el máximo.'
#Aclaración lo que quiere es que de cada dia, me calcule el minimo y el máximo. Y luego la media

extremes_avg <- function(x){
  minimo_vect <- sapply(x,min)
  maximo_vect <- sapply(x,min)
  minima <- min(minimo_vect)
  maxima <- max(maximo_vect)
  media <- (maxima + minima)/2
  return(media)
}

extremes_avg(temp)

#Calcula la media entre el dia mínimo y el máximo.
extremes_avg1 <- function(x){
  mean(min(x), max(x))
}


'Aplica la nueva función utilizando sapply.'
temperatura_media <- sapply(temp, extremes_avg)
temperatura_media

sapply(temp, extremes_avg)

'Crea una función extremes que devuelva una matriz con:
1. La temperatura mínima.
2. La temperatura máxima.'

extremes <- function(x){
  c(min = min(x), max = max(x), media= mean(x))
}
  
'Aplica la nueva función utilizando sapply.'
sapply(temp, extremes)

'Crea una función que devuelva las muestras menores que cero.'
below_zero <- function(x){
  return(x[x<0])
}
  
'Aplica la nueva función utilizando sapply y guardarla en freezing_s'
freezing_s <- sapply(temp, below_zero)
freezing_s
