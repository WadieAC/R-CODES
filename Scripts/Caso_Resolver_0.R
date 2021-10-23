
library(dplyr)
setwd("/Users/WadieAlbakri/Desktop/Máster BME/Módulos/Módulo 1/R")
getwd()

#directorio_Melendez <- getwd()
#setwd(directorio_Melendez)


bikes <-  read.table("bikes.csv", 
                                 header = TRUE, 
                                 sep = ",", 	# sep = "\t", #El separador es el tabulador
                                 stringsAsFactors = FALSE, # No deseamos convertir el resultado en un factor (poner siempre esto).
                                 dec = ".") #el separador decimal es el punto o el valor que le pases
                                

'¿De qué clase es el objeto?'
class(bikes) #data.frame


'¿Cómo se puede ver el tipo de cada columna y una muestra de ejemplos?'
str(bikes) #A parte del tipo de columna da la muestra de cada objeto

summary(bikes) #Summary también me devuelve el tipo de columna, pero me saca estadísticos.


'Muestra los primeros 15 registros del dataset.'
head(bikes,15)


'Muestra los últimos 20 registros del dataset'
tail(bikes,20)


'¿Cuáles son las dimensiones del dataset?'
dim(bikes)

'¿Cuáles son los nombres de las variables del dataset?'
colnames(bikes)
names(bikes)

'Las variables season y weekday deberían ser categóricas. 
Crea un factor con etiquetas para dichas columnas (no hace falta que sea ordenable) y 
asígnalo a las columnas de nuevo.'

?factor

#código mio
season_p <- factor(bikes$season) #creo el factor
levels(season_p) <- c("winter","spring","summer","fall") 
# OJO!! En ese caso no hace falta slicing porque tengo los mismos nombres en el vector que niveles

#Ahora tengo que cambiar el vector del data frame bikes por mi vector season
bikes$season <- season_p
bikes[,2] #se puede usar esta indexación también


#Código Guillermo
season <- factor(bikes$season, levels = c(1,2,3,4))
levels(season)[1:4] <-c("winter","spring","summer","fall") 

weekday <- factor(bikes$weekday, levels = c(0,1,2,3,4,5,6)) #creo la variable que contiene el factor
levels(weekday)[1:7] <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
bikes$weekday <- weekday

levels(bikes$weekday)
'Las variables is.holiday y is.workingday son variables booleanas. Conviértelas a variable booleana y asígnalas a las columnas de nuevo.'

bikes$is.holiday <- as.logical(bikes$is.holiday)
bikes$is.workingday <- as.logical(bikes$is.workingday)


#Este sería el for que hay detras del as.logical
for(i in 1:length(bikes$is.workingday)){
  if(bikes$is.workingday[i] == "0" ){
    bikes$is.workingday[i] <-  "FALSE"
  }else{
    bikes$is.workingday[i] <-  "TRUE"
  }
}


'Calcula la suma de la columna casual.'
suma_casual <- sum(bikes$casual)


'Guarda en un vector la suma de las columnas casual y registered.'
suma_registred <- sum(bikes$registered)

vector_casual_registred <- c(suma_casual, suma_registred)
vector_casual_registred
sum(vector_casual_registred)

total_por_fila<-as.vector(bikes$casual+bikes$registered)  #suma pares en las filas.
total_por_columna <-c(sum(bikes$casual), sum(bikes$registered))


'¿Qué variables son numéricas? PISTA: utiliza sapply junto con la funcion is.numeric.'

sapply(bikes, is.numeric) #Me devuelve True o False en función de la variable que sea.


'Utilizando el resultado anterior, selecciona aquellas columnas numéricas y calcula la media de todos los registros.'

prueba <- sapply(bikes, is.numeric) #El vector TRUE o FALSE me lo inserta en una variable
prueba <- prueba[prueba == TRUE] #Filtro y me quedo solo con los TRUE
prueba1 <- bikes[names(prueba)] #Ahora selecciono de bikes los nombres de TRUE del vector anterior

media_numeric <- c(mean_hour = mean(prueba1$hour),mean_weathersit = mean(prueba1$weathersit))
media_numeric

#Todo lo anterior lo puedo ahorrar indexando por los que son numericos
seleccion <- bikes[sapply(bikes,is.numeric)]
sapply(seleccion, mean) 
#Vuelvo a ejecutar la función sapply y me ahorraria el for que tendria que tirar para calcular todas
#Las medias a introducirlas en un vector.

'Selecciona aquellas observaciones del dataset para las que el día de la semana es Monday, 
están entre las 15 y 20 horas y la temperatura es superior a 25 grados.'

glimpse(bikes)

#Selecciono todas las columnas y fijo el valor de mis filas
bikes[(bikes$weekday == "Monday" & bikes$hour >= 15 & bikes$hour <= 20 & bikes$temp > 25),]

'Selecciona las 100 primeras filas y todas las columnas menos las dos últimas (con índices positivos).'

col <- dim(bikes)[2]-2
bikes[1:100, 1:col]

'Selecciona las 100 primeras filas y todas las columnas menos las dos últimas (con índices negativos).'

ultima_col <- dim(bikes)[2]
penul_col <- dim(bikes)[2]-1

bikes[1:100,-(penul_col:ultima_col)]

'Obtén los cuantiles de la variable hum.'

#Si no le pasas nada a la función te devuelve todos los cuartiles, divide en 0%-25%-50%-75%-100%

quantile(bikes$hum) #Me devuelve un vector

'Obtén los deciles de la variable hum.'

#Utilizo la función quantile pero le paso en prob la segmentación que quiero

quantile(bikes$hum, probs = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))

#En vez de picar a mano el vecto podemos crearlo o introducirlo en el código

probs_vector <- seq(0,1,0.1)
quantile(bikes$hum, probs = probs_vector)
quantile(bikes$hum, probs = seq(0,1,0.1))

'Obtén los estadísticos básicos de todas las variables en un solo comando.'
summary(bikes)

'¿Cuantos valores únicos existen para la variable atemp?'
unique(bikes$atemp) #esto me devuelve todos los valores únicos ahora quiero contarlos
class(unique(bikes$atemp)) #Me devuelve un vector
length(unique(bikes$atemp)) #cuento los valores unicos con length

'¿Cuantos registros tienen un valor superior a 100 para la variable casual?'
#hago slicing por nombre
length(bikes[(bikes$casual>100),"casual"]) #length me sirve para una dimensión unitaria nx1 o 1xn


#Otra manera es utilizar nrow y asi no tengo que fijar una columna puedo asignarselo a todo el DF
nrow(bikes[bikes$casual>100,])


'Ordena de menor a mayor los 100 primeros elementos de la variable registered.'

sort(bikes$registered) #ordena todos los valores
sort(bikes$registered[1:100]) #con esto obtengo solo 100 primeros.

#Recordamos el slicing por posiciones de fila-columna
bikes[1:100,1:4]

glimpse(bikes)

#slicing por nombres de columnas donde además fijo el número de filas
bikes[1:100, names(bikes) %in% c("date","season")] 
bikes[, names(bikes) %in% c("date","season")]  #aqui seleccionaria todas las filas

#-------------------------NOTA----------------------------------------------------------
#OBTENIENDO EL ÍNDICE DE UNA COLUMNA EN UN DATA FRAME!!!!!!!!!!
which(colnames(bikes) == "date") #Me devuelve la posición de la columna bike.
which(names(bikes) %in% c("date","season")) #Me devuelve las posiciones de las columnas
#-------------------------NOTA----------------------------------------------------------


'Ordena el dataset por la variable registered de manera ascendente. Inspecciona los primeros resultados.'
head(arrange(bikes, registered))

order(bikes$registered, decreasing = F)#esto devuelve posiciones

bikes[order(bikes$registered, decreasing = F),] 
#otra manera de hacerlo, slicing con orden en las filas indicadas.


'Obtén los índices de los registros para los que el valor de la variable temp es superior a la media.'

mean(bikes$temp) #con esto saco la media de temp

#con esto obtengo los registros de temp superiores a la media de la columna temp
bikes[bikes$temp > mean(bikes$temp),"temp"]

which(bikes$temp > mean(bikes$temp)) 
#me devuelve posiciones superiores a la media a partir de la 1004

bikes[1000:1004, "temp"] > mean(bikes$temp)
#con esto compruebo que los valores a partir del 1004 son superiores la media


'Obtén los registros para los que el valor de la variable windspeed es máximo.'

which(bikes$windspeed == max(bikes$windspeed)) 
#Me devuelve 2 resultado porque el valor máximo se repite.

#Como me devuelve 2 posiciones o incluso puede devolver más, pillo la primera posición (row)
min(which(bikes$windspeed == max(bikes$windspeed)))

#Con esto obtengo solo la row de los registros donde windspeed es máximo
bikes[min(which(bikes$windspeed == max(bikes$windspeed))),]

#Con esto obtengo a partir del máximo de windspeed
bikes[(min(which(bikes$windspeed == max(bikes$windspeed))):dim(bikes)[2]),]

'Comprueba si alguna de las variables contiene NAs.'

which(is.na(bikes) == TRUE) 
any(is.na(bikes)) #función correcta para comprobar los NA en el data frame
#Me esta devolviendo un cero como una poia porque puede que no haya ningún NA

'Comprueba utilizando el boxplot si la variable windspeed tiene outliers.'
boxplot(bikes$windspeed) #si tiene outliers

'Pinta un histograma de la variable atemp.'
hist(bikes$atemp)

'Crea una función (cold.heat) que reciba dos parámetros: temperatura y límite; y devuelva cold o heat.'
cold.heat <- function(x,y=20){
  if(x > y){
    return("Heat")
  }else if(x < y){
    return("Cold")
  }else{
    return("Ideal de 20")
   }
  }


'Aplica la función a la columna temp y almacena el resultado en una nueva columna cold.heat del data.frame.'
bikes$cold.heat <- sapply(bikes$temp, cold.heat)


'Calcula el número medio de usuarios registrados por día de la semana'
glimpse(bikes)

bikes %>%
  group_by(bikes$weekday) %>%
  #select(registered) %>%
  summarise(nº_de_dias = n()) #esto se me va a contar el numero de dias
#El comando hace el count de el group by

bikes %>%
  group_by(weekday, registered) %>%
  select(weekday, registered) %>%
  summarise(nº_de_dias = n()) %>%
  summarise(rank(registered))



sum(bikes$registered)
dim(bikes)

#al hacer el group_by y meterle el n() el ya sabe que tiene que sumar los iguales
#Esto me da el total de usuarios registrados por dia de la semana.


  

'Ordena de menor a mayor las temperaturas máximas de cada estación'


'Calcula el número total de usuarios (registered + casual) en días festivos y no festivos.'



'Calcula el día que se produjeron el mayor número de alquileres casuales.'




'Calcula el ratio de alquileres registrados/casuales por hora en verano.'



'Para todos los lunes festivos calcular el número total de alquileres registrados, temperatura media, humedad máxima y velocidad del viento mínima.'



