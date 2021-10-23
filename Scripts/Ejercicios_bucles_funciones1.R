
#SENTENCIAS Y BUCLES.
#-----------------------------------------------

#Sentencia IF
#--------------------------------

x <- 5
if(x<0){
  print("x es un número negativo")
}else if (x ==0 ){
  print("x es 0")
}else {
  print("X es un número negativo")
}

#Bucle FOR
#-----------------------------
for( año in 1:10){
  print(paste0("Feliz año ", año, "!")) #toma valores de 1 a 10 ambos incluidos
}

'Podemos iterar sobre un vector'

cities <- c("New York", "Paris", "London", "Tokyo", "Rio de Janeiro", "Cape Town")
for(city in cities){
  if(nchar(city) == 6){
    print(paste0("La cidudad ", city, " tiene 6 caracteres"))
  }
}

for( elemento in 1:length(cities)){
  print(paste("Hola bienvenido a ",cities[elemento]))
}


años <- c(2015,2016,2017,2018,2019,2020)
for (i in años){
  print(paste0("Estamos en ", i, "!"))
}



#Bucle WHILE. Se cumple siempre que la condición sea TRUE
#-----------------------------------------------------------
variable <- 1
while(variable <= 7){ 
  if (variable == 5){
    print("A 2 iter de finalizar")
    #break
  }
  print(paste("Valor de variable", variable))
  variable <- variable +1
}

#Nota: me va alterando el valor de variable, en las funciones no es asi, los valores nacen y mueren
v<-130
while(v>120){
  print(paste(v, "Reduce la velocidad, safe your life"))
  v <- v -1
}
print(paste0("Tu velocidad es ", v, " Enhorabuena has bajado la marcha"))




#EJERCICIOS DE BUCLES.
#---------------------------------------

'Recorre los elementos del vector y extrae los que empiecen por C'
numeros<-c("Uno","dos","tres","cuatro","cinco","treinta")

#substr - (x, pos_ini, pos_fin)

for(numero in numeros){
  tolower(numero) #añado esta función que pasa a minúsculas para reforzar
  if(substr(numero,1,1) == "C" | substr(numero,1,1) == "c"){
    print(numero)
  }
}


'Escribe un programa que encuentre todos los enteros entre 2000 y 3200 
(ambos incluidos) que sean divisibles por 7 pero no múltiplos de 5'

#Creamos un vector entre 2000 y 3200
vec1 <- seq(2000,3200, by=1)
vec1[1] #obtengo la primera posición
vec1[-(1:1200)] #obtengo la última posición dame todo excepto las 1200 posiciones iniciales

for(vec in vec1){
  if(vec %% 7 == 0 && vec && 5 != 0){
    print(vec)
  }
}

'Escribe un programa que calcule el número de vocales en la frase 
"En un lugar de la Mancha, de cuyo nombre no quiero acordarme"'

#substr(quijote,3,3)
#for(letra in)

quijote <- "En un lugar de la Mancha, de cuyo nombre no quiero acordarme"
quijote_min <- tolower(quijote)
quijote_min
vector_vocales <- c("a","e","i","o","u")

contador <- 0
for(letra in 1:nchar(quijote)){
  if(substr(quijote_min,letra,letra) == "a" || substr(quijote_min,letra,letra) == "e" || substr(quijote_min,letra,letra) == "i" || substr(quijote_min,letra,letra) == "o" || substr(quijote_min,letra,letra) == "u"){
    contador = contador +1
    print(letra)
  }
}
contador



#UNUSEFUL
#---------------------
quijote_min[1] #después de pasarla a minusculas la rompo y monto una lista
strsplit(quijote_min," ")
quijote_vec <- strsplit(quijote_min," ") #Me devuelve una lista de un objeto y x posiciones
quijote_vec[[1]][12]



# 2. CREAR A INVOCAR FUNCIONES.
# --------------------------------------------------

'Escribe una función a la que se le pase un vector de números y calcule su media'

vec1
media_vector <- function(c){
  return (mean(c))
}
media_vector(numeros)

numeros <- c(5,6,7,8)
media_for <- function(vec){
  acumulado <- 0
  frecuencia <- 0
  for(i in 1:length(vec)){
    acumulado = acumulado + vec[i]
    frecuencia = frecuencia +1
    if (i == length(vec)){
      media = acumulado/frecuencia
      return(media)
      break
    }
  }
}

media_for(numeros)

'Escribe una función que reconozca palíndromos. Un palíndromo es una palabra que 
se lee igual al derecho que al revés'


palabra = "soleado"
palabra1 = "sugus"

palindromo <- function(y){
  longitud_cadena <- nchar(y)
  contador_palindromo <- 0
  for (posicion in 1:nchar(y)){
    letra_i <- substr(y, posicion, posicion)
    letra_f <- substr(y, longitud_cadena, longitud_cadena)
    if( letra_i == letra_f){
      longitud_cadena <- longitud_cadena -1
    }else{
      return("La palabra no es un palíndromo")
      break
    }
  }
  return(paste("La palabra ", y, " es un palíndromo my friend"))
}

palindromo(palabra)
palindromo(palabra1)


'Escribe una función que calcule el IVA (21%) de un producto dado su precio de venta 
sin IVA y devuelva el precio total'


calculo_IVA <- function(precio){
  precio_iva <- precio * 1.21
  iva <- precio_iva - precio
  return(paste("El precio con IVA es: ", precio_iva, "el IVA es ", iva))
}

calculo_IVA(100)
