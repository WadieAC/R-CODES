#APUNTES DE LISTAS
#====================================

#1. Creación de listas
#===================================================================================
my_vector <- c(1,2,3,4,5)
my_matriz <- matrix(1:9, nrow=3, byrow = TRUE)
my_df <- iris

#Puedo crear una lista con elementos de todos los tipos, vector, data_frame, matrices
lista <- list(my_vector,my_matriz,my_df)
class(lista)
lista

#Puedo crear una lista y asignar Nombres.
lista2 <- list(vec = my_vector, mat = my_matriz, df = my_df)
lista2

#Puedo crear una lista - uniendo otras listas.
lista3 <- list(lista,lista2)
lista3

#2. Manipulación de listas
#==================================================================================

lista <- c(lista, year = 1980) #añade el elemento year a la lista, al final.
lista
lista2[length(lista2)+1] <- 1980 #añado elemento pero sin nombre al elemento, al final
lista2
lista2 <- append(lista2, "Hola mundo") #añado al final "hola mundo"
names(lista2)
lista2

names(lista2$vec) <- c("pos1","pos2","pos3","pos4","pos5") #renommbro las columnas de un objeto dentro de una lista
names(lista2)

names(lista2)[4] <- "Año" #cambiar el nombre de una sub-lista(obejto) en particular
names(lista2)[1] <- "vect" #cambio el nombre del 1º objeto de la lista

lista3[[2]] <- NULL #cepilla la celda y desplaza lo demás, cuidado porque reestructura la dim
lista3
lista3[[1]][[3]]

#Cambiar todos los nombres de los objetos de la lista del tirón
#-----------------------------------------------------------------------------------
length(lista)
vect_names <- c("vect1","mat1","df1","year1") #creo un vector nombres con los nombres nuevos
vect_names[1]

for (i in 1:length(lista)) {
  names(lista)[i] <- vect_names[i]
}

#Listas recursivas
#-----------------------------------------------------------------------
lista_recursiva <- list(list1 = list(vec0 = my_vector, mat0 = my_matriz, df0 = my_df), mat1 = my_matriz, df1 = my_df)
lista_recursiva #me genera una lista, con una lista de 3 objetos en la 1º pos

#acceso a listas recursivas
lista_recursiva$list1$vec0 #accedo al objeto vec0 del objeto list1 de la lista_recursiva
lista_recursiva[[1]][[1]] #este comando da lo mismo que el anterior

#Si quiero modificar el elemento de una lista_recursiva, accedo a el y asigno nuevo <- 



#INDEXACIÓN DE LISTAS - 2 llaves a diferencia de data.frame, matrices, etc
#=============================================================================
lista[1] #un corchete genera una sub-lista de 1 objeto
lista[[2]] #el doble corchete me devuelve el contenido - me devuelve un DataFrame

lista3[[1]][[3]][1,1] #Selecciona el 1º objeto, luego el 3º elemento y de ese el punto 1,1

lista3[[1]][[3]][1:5,1:5]
head(lista2)

lista2[[1]][3] #en este caso el primer objeto es un vector de 1 dim

lista2[[2]][1,] #El 2º objeto de la lista es una matriz, selcciono 1º fila y all cols

str(lista2) #resumen de la lista en cuestion


#EJERCICIOS LISTAS.
#=================================================================

actors <- c("Jack Nicholson", "Shelley Duvall", "Danny Lloyd", "Scatman Crothers", "Barry Nelson")
scores <- c(4.5, 4.0, 5.0)
comments <- c("Best Horror Film I Have Ever Seen", "A truly brilliant and scary film from Stanley Kubrick", "A masterpiece of psychological horror")
reviews <- data.frame(scores, comments)


'Crea una lista que contenga los siguientes componentes:
1. moviename: "The Shining"
2. actors: el vector de actores
3. reviews: el data frame de reviews'

pelicula <- list(nombre_pelicula = "The Shining", actores = actors, critica = reviews)

'Selecciona el último actor del vector de actores de la lista:'
pelicula$actores[length(pelicula$actores)]

'Selecciona la segunda de las críticas del data frame de reviews de la lista:'
pelicula$critica[[2]][2]


'Añade un nuevo elemento a lista: year: 1980'
pelicula <- c(pelicula, year = 1980)
pelicula

'Comprueba el contenido de la lista empleando la función str:'
str(pelicula)

vector <- 1:10 
matriz <- matrix(1:9, ncol = 3)
frase <- "En un lugar de la Mancha, de cuyo nombre no quiero acordarme"
lista <- list(vector, matriz, frase) 

'Añade el nº 11 al vector.'
append(lista[[1]], 11, after = length(lista[[1]]))  #añadir un elemento a un vector
lista

'Cambia el valor 9 de la matriz por un 22.'
lista[[2]][3,3] <- 22
lista

'Eliminar la frase de la lista'
lista[[3]] <- NULL
lista

#-------------------------------------------------------------------------
max(lista[[1]])
min(lista[[1]])
lista


min(lista[[2]])


getwd()
