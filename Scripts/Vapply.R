
#VAPPLY
#Controla el tipo de dato de retorno (el resto es igual que Sapply). 
#Es decir tienes que especificar al final el tipo de dato que quieres y longitud

cities <- c("New York", "Paris", "London", "Tokyo", "Rio de Janeiro", "Cape Town")

vapply(cities, nchar, numeric(1)) # El dato de retorno es numérico.

first_and_last <- function(name) {
  name <- gsub(" ", "", name)  #reemplaza espacio o "" por el nombre que le pasas por la función
  letters <- strsplit(name, split = "")[[1]]
  c(first = min(letters), last = max(letters))
}

vapply(cities, first_and_last, character(2)) # # El 2 es xq devuelve una matriz de 2 filas.

vapply(cities, first_and_last, character(1)) # Esto daría un error, ya que estamos devolviendo un vector de longitud 2.



#EJERCICIOS
#------------------------------------

'Muestras de temperatura de cada día de la semana.'
temp <- list(monday = c(3, 7, 9, 6, -1), tuesday = c(6, 9, 12, 13, 5), wednesday = c(4, 8, 3, -1, -3), thursday = c(1, 4, 7, 2, -2), friday = c(5, 7, 9, 4, 2), saturday = c(-3, 5, 8, 9, 4), sunday = c(3, 6, 9, 4, 1))

'Crea la función basics que devuelve un vector con nombres con:
1. min: temperatura mínima
2. mean: temperatura media
3. max: temperatura máxima'

basics <- function(x) {
  c(min = min(x), mean = mean(x), max = max(x))
}

'Usando vapply aplica la función basics.'
vapply(temp, basics, numeric(3))

'Con esta nueva definición de la función basics.'
basics <- function(x) {
  c(min = min(x), mean = mean(x), median = median(x), max = max(x))
}

'Arregla el error:'
vapply(temp, basics, numeric(4)) #El numero tiene que coincidir con los calculos de la funcion

'Convierte este sapply en un vapply.'
sapply(temp, max)
vapply(temp, max, numeric(1))

'Convierte este sapply en un vapply.'
sapply(temp, function(x, y) { mean(x) > y }, y = 5)
vapply(temp, function(x, y) { mean(x) > y }, y = 5, logical(1))

'Con esta función get_info, convierte el sapply en un vapply.'
get_info <- function(x, y) { 
  if (mean(x) > y) {
    return("Not too cold!")
  } else {
    return("Pretty cold!")
  }
}

sapply(temp, get_info, y = 5)
vapply(temp, get_info, y = 5, character(1))