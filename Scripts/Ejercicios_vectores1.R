#APUNTES Y EJERCICIOS DE VECTORES.
#---------------------------------------------

'Creación de vectores'
X <- 1:30 #genera un vector de nº de 1 a 30. El operador “:” tiene prioridad sobre otros operadores (sumar, dividir…)
1:10-1 #genera un vector de 0 a 9	
1:(10-1) #genera un vector de 1 a 9 	

Y <- seq(1, 5, 0.5) # genera un vector de 1.0 a 5.0 con incrementos de 0.5
Z <- c(1, 2, 3) # permite asignar valores a un vector directamente.		
M <- Rep(1, 30) # crea un vector de 30 valores iguales de valor 1.
v5 <- seq(from = 0, to = 1, by = 0.1) # Asignamos el vector a una variable.
# Ponemos nombres a los valores del vector
v11 <- c(a = 1, b = 2, c = 3) 


'Indexación de vectores (recuperar valores)'
'=========================================================================='
v=100:200
v[1] # Seleccionamos / recuperamos el primer valor del vector.
v[c(1, 1, 4, 5)] # Seleccionamos el primero dos veces, el cuarto y el quinto
v[-1] # Seleccionamos todos menos el primer elemento. 		
v[-length(v)] # Seleccionamos todos menos el último elemento.
v[-c(1, 3, 4, 5)] # Seleccionamos todos menos el primero, el tercero, el cuarto y el quinto

v <- v > 30 # Construyendo un índice booleano
#Devuelve un vector TRUE/FALSE, FALSE en las pos < que 30

# Aplicando un índice booleano
v[v > 30] 
'Me devuelve los VALORES mayores que 30 - CONTENIDO!'

#Localizando posiciones a través de un índice booleano
which(v > 150) 
'El which me devuelve la POSICIÓN! de los elementos cuyo contenido cumplen esa condicion'

# Seleccionamos los elementos por posiciones
v[which(v > 30)] 
'Me devuelve el contenido cuyas posiciones'

# Seleccionamos todos los elementos cuyo valor sea  > 30 y <= 50
v[v > 30 & v <= 50] 

# Seleccionamos todos los elementos cuyo valor sea 0
v[v == 0] 

# Seleccionamos los elementos cuyo valor sea 101, 150 y 187
v[v %in% c(101, 150, 187)]

# Asignamos el valor cero a los elementos entre los índices 70 y 100
v[70:100] <- 0 
v0 = v[1:5]

# ponemos nombres a los valores del vector v0. NAMES para poner nombre a las posiciones
names(v0) <- c("SAN", "TEL", "ACC", "IAG", "MAP") 
v0

# Podemos seleccionar los elementos por su posición, por su valor o por su nombre…
v0[c("SAN", "IAG")] 

#devuelve el nombre y valor de los elementos de v0 que no son TEL o MAP
v0[!(names(v0) %in% c("TEL", "MAP"))]

#Me devuelve los valores en TEL y MAP.
v0[(names(v0) %in% c("TEL", "MAP"))]


#Introduzco el valor de 150 en TEL, slicing por nombre.
v0["TEL"] <- 150
v0

'Vectores auxiliares para el ejercicio'
poker_vector <- c(140,-50,20,-120,240)
ruleta_vector <- c(-24,-50,100,-350,10)

total_resultados <- c(poker_vector, ruleta_vector) #append de 2 vectores.

'Crea dos vectores con las ganancias y pérdidas de la semana'
ganancias_semana <- c()
perdidas_semana <- c()
a <- 1
b <- 1

for(i in 1:length(total_resultados)){
  if(total_resultados[i] >= 0){
    ganancias_semana[a] <- total_resultados[i]
    a <- a +1
  }else{
    perdidas_semana[b] <- total_resultados[i]
    b <- b+1
  }
}
ganancias_semana
perdidas_semana


'Crea un vector con las ganancias en el póker de lunes a viernes'
a <- 1
b <- 1
ganancias_poker <- c()
perdidas_poker <- c()


for(i in 1:length(poker_vector)){
  if(poker_vector[i] >= 0){
    ganancias_poker[a] <- poker_vector[i]
    a <- a +1
  }else{
    perdidas_poker[b] <- poker_vector[i]
    b <- b+1
  }
}


'Crea un vector con las ganancias en la ruleta de lunes a viernes'

a <- 1
b <- 1
ganancias_ruleta <- c()
perdidas_ruleta <- c()


for(i in 1:length(ruleta_vector)){
  if(ruleta_vector[i] >= 0){
    ganancias_ruleta[a] <- ruleta_vector[i]
    a <- a +1
  }else{
    perdidas_ruleta[b] <- ruleta_vector[i]
    b <- b+1
  }
}

ganancias_ruleta
perdidas_ruleta


'Crea un vector con los días de la semana y asígnalo como nombre a los elementos de los vectores anteriores'
dias_semana <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes")

names(poker_vector) <- dias_semana
names(ruleta_vector) <- dias_semana

'Calcula el total de ganancias diarias'
ganancias_diarias <- poker_vector + ruleta_vector

'Calcula el total de ganancias en el póker'
sum(poker_vector)

'Calcula el total de ganancias en la ruleta'
sum(ruleta_vector)

'Calcula el total de la semana'
sum(ganancias_diarias)
ganancias_totales <- sum(poker_vector) + sum(ruleta_vector)
ganancias_totales

'Selecciona las ganancias del martes, miércoles y jueves en el póker'
poker_vector[c("Martes","Miercoles","Jueves")]

'Selecciona las ganancias desde el martes al viernes en la ruleta'
ruleta_vector[2:5]
ruleta_vector[-1]

'Calcula la media de ganancias en el póker el lunes, martes y miércoles'
mean(poker_vector[1:3])

'¿Qué días de la semana hubo ganancias al póker?' 
poker_vector[which(poker_vector>0)]

'Haz la selección anterior sobre el vector con los datos del póker'
#Es una indexación por valor de vector
poker_vector[poker_vector %in% c(140,20,240)]

'Haz lo mismo sobre el vector con los datos de la ruleta'
ruleta_vector[which(ruleta_vector>0)]


