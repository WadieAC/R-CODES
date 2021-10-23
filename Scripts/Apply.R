
#LAPPLY - ME DEVUELVE UNA LISTA
#--------------------------------------


nyc <- list(pop = 8405837, boroughs = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"), capital = FALSE)
for (info in nyc) {
  print(class(info))
}

lapply(nyc,class)



cities <- c("New York", "Paris", "London", "Tokyo", "Rio de Janeiro", "Cape Town")
num_chars <- c()
for (i in 1:length(cities)) {
  num_chars[i] <- nchar(cities[i])
}

num_chars

unlist(lapply(cities, nchar))

'Con funciones propias'
oil_prices <- list(2.37, 2.49, 2.18, 2.22, 2.47, 2.32)
triple <- function(x) {
  return(x * 3)
}

unlist(lapply(oil_prices, triple))


'Con funciones propias pasando argumentos'
oil_prices <- list(2.37, 2.49, 2.18, 2.22, 2.47, 2.32)
multiply <- function(x, factor) {
  return(x * factor)
}
unlist(lapply(oil_prices, multiply, factor = 3))

#EJERCICIOS - LAPPLY
#-------------------------------

'Vector con listado de nombres de matemáticos y sus años de nacimiento.'

pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")

'Separa los nombres y años utilizando la función strsplit.'
split_math <- unlist(strsplit(pioneers, ":"))
split_math

#split_math <- unlist(strsplit(pioneers, ":"))


'Aplica la función tolower a todos los elementos de split_math.'
split_low <-  unlist(tolower(split_math))
split_low

#split_low <- lapply(splith_math, tolower)


'Escribe una función que devuelva el primer elemento de un vector.'
select_first <- function(vector){
  vector[1]
}

select_first(split_low)
  
'Aplica la función select_first a split_low'
names <-  select_first(split_low)

#names <- lapply(split_low, select_first)
  
  'Escribe una función que devuelva el segundo elemento de un vector.'
select_second <- function(vector){
    vector[2]
  }
  
  'Aplica la función select_second a split_low'
years <-  select_second(split_low)
  
#years <- lapply(split_low, select_second)


