getwd()
directorio <- getwd()
setwd(directorio)


'Lee el fichero y asígnalo a una variable'

setwd("/Users/WadieAlbakri/Desktop/Máster BME/Módulos/Módulo 1/R")
bikes <- read.csv("bikes.csv",
                  header = FALSE, 
                  sep = ";", 	# sep = "\t", #El separador es el tabulador
                  stringsAsFactors = FALSE, # No deseamos convertir el resultado en un factor (poner siempre esto).
                  dec = ",", #el separador decimal es la coma
                  quote = "") #Especificamos las cadenas de caracteres

class(bikes)

#La primera columna es con una numeración de fila, no me interesa, la cepillo
library("dplyr")
bikes <- select(bikes, -V1)
names(bikes)


#Reasigno nombres a las columnas y filas, porque se me ha metido morralla en la primera columna
colnames(bikes) <- bikes[1,]
colnames(bikes)
bikes[1,]
bikes <- bikes[-1,]
dim(bikes)
vector_filas <- c(1:1739)
rownames(bikes) <- names(vector_filas)

bikes_reserva <- bikes
bikes_reserva

'¿De qué clase es el objeto?'
class(bikes) #data.frame


'¿Cómo se puede ver el tipo de cada columna y una muestra de ejemplos?'
summary(bikes)
  
'Muestra los primeros 15 registros del dataset.'
head(bikes,15)


'Muestra los últimos 20 registros del dataset'
tail(bikes,20)


'¿Cuáles son las dimensiones del dataset?'
dim(bikes)
  
'¿Cuáles son los nombres de las variables del dataset?'
colnames(bikes)
  
'Las variables season y weekday deberían ser categóricas. Crea un factor con etiquetas para dichas columnas (no hace falta que sea ordenable) y asígnalo a las columnas de nuevo.'

bikes$season <- factor(bikes$season, ordered = TRUE)
bikes$weekday <- factor(bikes$weekday, ordered = TRUE)

levels(bikes$season)
levels(bikes$weekday)

season_factor <- levels(bikes$season)
weekday_factor <- levels(bikes$weekday)


'Las variables is.holiday y is.workingday son variables booleanas. Conviértelas a variable booleana y asígnalas a las columnas de nuevo.'

for(i in 1:length(bikes$is.holiday)){
  if(bikes$is.holiday[i] == "TRUE" ){
    bikes$is.holiday[i] <-  "FALSE"
  }else{
    bikes$is.holiday[i] <-  "TRUE"
  }
}

bikes$is.holiday


for(i in 1:length(bikes$is.workingday)){
  if(bikes$is.workingday[i] == "0" ){
    bikes$is.workingday[i] <-  "FALSE"
  }else{
    bikes$is.workingday[i] <-  "TRUE"
  }
}

bikes$is.workingday

'Calcula la suma de la columna casual.'

suma_casual <- sum(as.numeric(bikes$casual))
suma_casual

'Guarda en un vector la suma de las columnas casual y registered.'

suma_registred <- sum(as.numeric(bikes$registered))
suma_registred

'¿Qué variables son numéricas? PISTA: utiliza sapply junto con la funcion is.numeric.'

summary(bikes)

'Utilizando el resultado anterior, selecciona aquellas columnas numéricas y calcula la media de todos los registros.'

c

'Selecciona aquellas observaciones del dataset para las que el día de la semana es Monday, están entre las 15 y 20 horas y la temperatura es superior a 25 grados.'


'Selecciona las 100 primeras filas y todas las columnas menos las dos últimas (con índices positivos).'


'Selecciona las 100 primeras filas y todas las columnas menos las dos últimas (con índices negativos).'


'Obtén los cuantiles de la variable hum.'


'Obtén los deciles de la variable hum.'


'Obtén los estadísticos básicos de todas las variables en un solo comando.'


'¿Cuantos valores únicos existen para la variable atemp?'
  
  
'¿Cuantos registros tienen un valor superior a 100 para la variable casual?'
  
  
'Ordena de menor a mayor los 100 primeros elementos de la variable registered.'


'Ordena el dataset por la variable registered de manera ascendente. Inspecciona los primeros resultados.'


'Obtén los índices de los registros para los que el valor de la variable temp es superior a la media.'


'Obtén los registros para los que el valor de la variable windspeed es máximo.'


'Comprueba si alguna de las variables contiene NAs.'


'Comprueba utilizando el boxplot si la variable windspeed tiene outliers.'


'Pinta un histograma de la variable atemp.'


'Crea una función (cold.heat) que reciba dos parámetros: temperatura y límite; y devuelva cold o heat.'


'Aplica la función a la columna temp y almacena el resultado en una nueva columna cold.heat del data.frame.'

