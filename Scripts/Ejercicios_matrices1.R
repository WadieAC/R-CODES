#Apuntes y Ejercicios de MATRICES.
#--------------------------------------------------

'Matrices: Creación'

m1 <- matrix(1:9, byrow = TRUE, nrow = 3)

m2 <- matrix(c(0, -1, 4)) # Crea una matriz con una columna (xq si no indicas nada, se rellena por filas).

d1 <- diag(3) # Crea una matriz diagonal 3x3, MATRIZ IDENTIDAD 3x3

d2 <- diag(c(1, 2, 3)) # Crea matriz diagonal, asigna vector a la diagonal.

t_m1 <- t(m1) # Traspuesta de m1
d <- det(m1) # Determinante de la matriz

'Matrices: Operaciones' 
a_matrix <- matrix(1:9, byrow = TRUE, nrow = 3)
b_matrix <- matrix(11:19, byrow = TRUE, nrow = 3)

total_matrix <- a_matrix + b_matrix
total_matrix #suma por posiciones, es decir cada posición con la coincidente.

total_matrix <- a_matrix + 2 #suma un escalar a todas las posiciones
total_matrix

rowSums(total_matrix) #suma de todos los elementos de una fila
colMeans(total_matrix) #media de todos los elementos de una columna
max(total_matrix) #Devuelve el valor máximo que encuentra en la matriz

'Unión de matrices por columnas' #Para unión por columnas deben tener las mismas filas
a_matrix
b_matrix
big_matrix_2 <- cbind(a_matrix, b_matrix)
big_matrix_2

'Unión de matriz y vector por columnas' 
#El vector con las mismas posiciones que filas de la matriz lo lee en vertical,append derecha
big_matrix_3 <- cbind(big_matrix_2, c(1, 5, 6))
big_matrix_3

'Unión de matrices por filas' #Deben de tener las mismas columnas
big_matrix_4 <- rbind(a_matrix, b_matrix)
big_matrix_4

'Unión de matriz y vector por filas'
#En este caso el vector tiene las mismas posiciones que columnas, append debajo
big_matrix_5 <- rbind(big_matrix_4, c(1, 5, 6))
big_matrix_5

'Indexando con números positivos'
m <- matrix(1:9, byrow = TRUE, nrow = 3)
m
m[1, ] # Seleccionamos la primera fila y TODAS las columnas
m[1:2, ] # Seleccionamos las dos primeras filas y TODAS las columnas
m[, 1:2] #seleccionamos las 2 primeras columnas
m[, 3] # Seleccionamos la última columna y TODAS las filas
m[, c(1, 3)] # Seleccionamos la primera y la última columna
m[c(1,2),] #seleccionamos las 2 primeras filas
m[1, ] <- 0 # Asigna un vector de ceros a la primera fila
m
m[1,] <- 0
m[,3] <- 0

'Indexando con números negativos'
m[-1, ] # Seleccionamos todas las filas menos la primera
m[-nrow(m), -ncol(m)] # Quitamos la última fila y la última columna

'Indexando con vectores lógicos o expresiones booleanas'
m_selection <- matrix(c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE), byrow = TRUE, nrow = 3)
m_selection
m

#INDEXANDO CON UNA MATRIZ BOOLEANA. ME DEVUELVE LAS POSICIONES DONDE TENGO LOS TRUE
m[m_selection] # Indexamos usando la selección anterior.

m[m > 7] # Todos los > 7
m[m == 0] # Todos los 0

'Indexando por nombre'
colnames(m) <- c("c1", "c2", "c3")
rownames(m) <- c("r1", "r2", "r3")
m
m[, c("c1", "c3")] # Selección de columnas por nombre.
m[c("r2", "r3"), c("c1", "c2")] # Selección de F y C por nombre.


#EJERCICIOS DE MATRICES.
#--------------------------------------------------------

'Los siguientes vectores contienen la recaudación de las tres primeras películas de Star Wars en US y fuera de US (non-US)'
nueva_esperanza <- c(461, 314.4)
imperio_contrataca<- c(290.5, 247.9)
retorno_jedi <- c(309.3, 165.8)

'Crea una matriz que contenga toda la información (con tres filas)'
recaudacion_total_saga1<- matrix(c(nueva_esperanza, imperio_contrataca, retorno_jedi), byrow = TRUE, nrow = 3)
recaudacion_total_saga1

'Ponles nombres a las columnas: "US" y "non-US"'
colnames(recaudacion_total_saga1) <- c("US", "non-US")
recaudacion_total_saga1

'Ponles nombres a las filas: "Una nueva esperanza", "El imperio contraataca" y "El retorno del jedi"'
rownames(recaudacion_total_saga1) <- c( "Una nueva esperanza", "El imperio contraataca" , "El retorno del jedi")
recaudacion_total_saga1

'Si el precio de la entrada es de 5$, estima el número de espectadores de cada película.'
recaudacion_total_saga1
numero_estimado_espectadores <- (recaudacion_total_saga1*1000000)/5
numero_estimado_espectadores
'Como el precio de las entradas no es el mismo todos los años, creamos una matriz de precios'
ticket_prices_matrix <- matrix(c(5, 5, 6, 6, 7, 7), nrow = 3, byrow = TRUE, dimnames = list(rownames(recaudacion_total_saga1), colnames(recaudacion_total_saga1)))

'Repite el cálculo del número de espectadores con la matriz anterior.'
numero_exacto_espec <- (recaudacion_total_saga1*1000000)/ticket_prices_matrix
numero_exacto_espec

'Calcula el número de espectadores medio en US'
numero_exacto_espec[,"US"]
media_espec_us <- sum(numero_exacto_espec[,"US"])/length(numero_exacto_espec[,"US"])
media_espec_us

'Calcula el número de espectadores medio fuera de US'
media_espec_non_us <- sum(numero_exacto_espec[,"non-US"])/length(numero_exacto_espec[,"non-US"])
media_espec_non_us

'Calcula los totales de recaudación por película'
recaudacion_nueva_esperanza <- sum(recaudacion_total_saga1["Una nueva esperanza",])
recaudacion_imperio_contrataca <- sum(recaudacion_total_saga1["El imperio contraataca",])
recaudacion_retorno_jedi <- sum(recaudacion_total_saga1["El retorno del jedi",])

'Añade el vector anterior como una nueva columna de la matriz recaudacion_total_saga1.'
recaudacion_total_saga1 <- cbind(recaudacion_total_saga1, c(recaudacion_nueva_esperanza, recaudacion_imperio_contrataca, recaudacion_retorno_jedi))
recaudacion_total_saga1

colnames(recaudacion_total_saga1) <- c("US", "non-US", "Total")
recaudacion_total_saga1

'Crea una nueva matriz con las recaudaciones de las siguientes tres películas.'
amenaza_fantasma <- c(474.5, 552.5)
ataque_clon<- c(310.7, 338.7)
venganza_sith <- c(380.3, 468.5)

recaudacion_total_saga2<- matrix(c(amenaza_fantasma, ataque_clon, retorno_jedi), byrow = TRUE, nrow = 3)
'Ponles nombres a las columnas: "US" y "non-US"'
colnames(recaudacion_total_saga2) <- c("US", "non-US")
recaudacion_total_saga2

'Ponles nombres a las filas: "Amenaza fantasma", "Ataque de los clones" y "Venganza Sith"'
rownames(recaudacion_total_saga2) <- c( "Amenaza fantasma", "Ataque de los clones", "Venganza Sith")
recaudacion_total_saga2
'Une en una nueva matriz la recaudación de todas las películas, las tres primeras filas corresponderán a las tres primeras películas y las tres siguientes a las últimas películas.'
recaudacion_amenaza_fantasma <- sum(recaudacion_total_saga2["Amenaza fantasma",])
recaudacion_ataque_clones <- sum(recaudacion_total_saga2["Ataque de los clones",])
recaudacion_venganza_sith <- sum(recaudacion_total_saga2["Venganza Sith",])

recaudacion_total_saga2 <- cbind(recaudacion_total_saga2, c(recaudacion_amenaza_fantasma, recaudacion_ataque_clones, recaudacion_venganza_sith))
recaudacion_total_saga2
colnames(recaudacion_total_saga2) <- c("US", "non-US", "Total")

recaudacion_total_saga_entera <- rbind(recaudacion_total_saga1, recaudacion_total_saga2)
recaudacion_total_saga_entera

'Calcula los totales de recaudación de todas las películas en US y fuera de US'
recaudacion_total_US <- sum(recaudacion_total_saga_entera[,1])
recaudacion_total_non_US <- sum(recaudacion_total_saga_entera[,2])

'Calcula la media recaudada de las tres primeras películas fuera de US'
media_saga1_non_US <- sum(recaudacion_total_saga1[,2])/length(recaudacion_total_saga1[,2])
media_saga1_non_US

'Calcula la media recaudada de las 2 primeras películas fuera de US'
media_2_prim_non_US <- sum(recaudacion_total_saga1[c(1,2),2])/length(recaudacion_total_saga1[c(1,2),2])




