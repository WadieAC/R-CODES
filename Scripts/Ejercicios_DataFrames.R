#DATA.FRAMES
#===========================================


getwd()
setwd("/Users/WadieAlbakri/Desktop/Máster BME/Módulos/Módulo 1/R")
getwd()

'creación de un data.frame vacio'
df_vacio <- data.frame()

c1 <- 1:10
c2 <- letters[1:10] #crea un vector con las letras de la 'a' a la 'j'
df <- data.frame(col1 = c1, col2 = c2) #crea un data.frame y pone nombre a las columnas

'lectura de csv'
df_USArrests <- read.csv("USArrests.csv", header = T)

#Análisis exploratorio de un DataFrame
head(mtcars)
head(mtcars, 10) #me devuelve las 10 primeras filas del head
head(mtcars, -10) #me devuelve todas menos las 10 primeras filas
tail(mtcars, 10) #me devuelve los 10 últimos datos
str(mtcar) #me devuelve un resumen de variables, tipo y ejemplo de datos
dim(mtcars) #me devuelve la dimension
summary(mtcars) #me devuelve un estadístico de las principales variables del Data.Frame
resumen_data(mtcars)

resumen_data <- function(datos){
  print(head(datos,10))
  print(tail(datos,10))
  print(summary(datos))
  print(str(datos))
  print(dim(datos))
  
}


#Manipulación del Data.Frame
#------------------------------------------------------------------------------

'añadir una fila al final del Data.Frame y además la inserto con el nombre de fila'
'le paso los valores de la columna'
df <- rbind(mtcars, data.frame(mpg = 22, cyl = 5, disp = 202, hp = 100, drat = 2.56, wt = 3.1, qsec = 15, vs = 1, am = 0, gear =5, carb = 4, row.names=c("seat"))) 

#Creamos una nueva columna llammado 'newcolum' con un vector de 1
df$newcolum <- rep(1,nrow(df))
df

#Añade una nueva columna con los mismos valore que la columna hp y la llama 'copyofhp'
df[,'copyofhp'] <- df$hp
df

#Genera una nueva columna llamada hp.gear cuyo valor es el resultado de dividir hp entre gear
df$hp.gear <- df$hp/df$gear 
df

#Vector que va desde 1 hasta el número de filas de df step by 1 desde 1 hasta longitud
v <- 1:nrow(df)
v
v[2]

#Si quiero añadir ese vector de números a mi df puedo utilizar un cbind
df <- cbind(df,v)
df



#Indexación - slicing del Data.Frame
#------------------------------------------------------------------------------

df <- data.frame(mtcars)
df

'Indexando celdas'
df[5,2] #dato fila 5 columna 2
df[1:5, 1:2] #subset filas 1 to 5 columnas 1 to 2
df[1:2,c("gear", "am")] #de las filas 1 to 2 las columnas gear y am
df[1:2, c("gear","am")] <- 0 #va a tomar esas filas y col y le va a enchufar un 0

'Indexando por filas - devuelve Data.Frames'
df[1,]
df[-nrow(df),] #devuelve todas menos la última
df[nrow(df),] #devuelve la última
df[1:5,] #filas 1 to 5 y todas las columnas
df[(df$hp >150 & df$hp < 200), ] #devuelve un df que cumpla esas condiciones
subset(df, hp > 150 & hp < 200) #otra manera de tirar el comando anterior

#Podemos convertir el resultado anterior en vector
vrow <- as.numeric(as.vector(df[1,])) 'genero el vector del df'
nom_col_df <- as.vector(colnames(df)) 'genero un vector con el nombre de las columnas'
names(vrow) <- nom_col_df #names solo lo puedo utilizar con una dimensión- añado el vector nombres


'Indexando columnas (distintas manera de hacer lo mismo)'
df$hp # Devuelve una columna como un vector
df[, "hp"] # Devuelve un vector - mismo resultado que el anterior código
df[, 4] # Devuelve un vector
df[["hp"]] # Devuelve un vector

df["hp"] # Devuelve un data frame con una columna
df[4] # Devuelve un data frame con una columna
df[ , c(4, 6)] # Devuelve un data frame
df[ , c("hp", "wt")] # Devuelve un data frame

'Unión de Data frames mediante una key: merge'
c1 <- 1:10; c2 <- letters[1:10]; c3 <- 5:20; c4 <- letters[5:20]
df.x <- data.frame(col1 = c1, col2 = c2)
df.y <- data.frame(col1 = c3, col2 = c4)

df.x
df.y

'Matcheo por valores iguales de columna - join'
df.match <- merge(df.x, df.y, by = c("col1")) #join
df.match

left.join <- merge(df.x, df.y, by = c("col1"), all.x = T) #left join
left.join
right.join <- merge(df.x, df.y, by = c("col1"), all.y = T) #right join
right.join
full.join <- merge(df.x, df.y, by = c("col1"), all = T) #full join
full.join #right join + left join - unido


'recorrer un data.frame con un 2 for'
for (j in (1:dim(titanic)[2])){
  for (i in (1:dim(titanic)[1])){
    if ( titanic[1:i,1:j] == "" | titanic [1:i,1:j] == " "){
      variable_espacio[p] <- colnames(titanic[j])
      contador_espacio <- contador_espacio + 1
    }
  }
}


#EJERCICIOS DE DATA-FRAMES.
#======================================

'Crea a partir de los siguientes vectores un Data Frame'
planets <- c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune")
type <- c("Terrestrial planet", "Terrestrial planet", "Terrestrial planet", "Terrestrial planet", "Gas giant", "Gas giant", "Gas giant", "Gas giant")

diameter <- c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883)

rotation <- c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67)

rings <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)

planets_df  <-data.frame(planetas = planets, tipo=type, diametro=diameter, rotacion=rotation, anillos=rings)
planets_df
'Comprueba el contenido del Data Frame (nº variables y tipo)'
resumen_data(planets_df)

'Selecciona la información de los tres primeros planetas.'
closest_planets_df <- as.vector(planets_df[1:3,])
closest_planets_df

'Selecciona la información de los últimos tres planetas.'
planets_df[6:8,]
#Puedo utilizar la función dim
dim(planets_df)[1] #Esto me devuelve la primera dimensión - mis filas
dim(planets_df)[2] #Esto me devuelve mis columnas

furthest_planets_df <- planets_df[ (length(planets_df$planetas) -2) : length(planets_df$planetas), ]
furthest_planets_df

'Selecciona la columna diameter de los últimos seis planetas. '
furthest_planets_diameter <- planets_df[ (length(planets_df$diametro) -5) : length(planets_df$diametro), c("diametro") ]
furthest_planets_diameter
planets_df[3:8,3]

'Selecciona sólo los planetas que tienen anillos.'
planets_with_rings_df <- planets_df[(planets_df$anillos == TRUE), ]  
planets_with_rings_df

'Selecciona los planetas que tienen un diámetro inferior al de la tierra (aquellos que tienen diametro < 1, la variable es relativa a la tierra)'
planets_lower_tierra_df <- planets_df[(planets_df$diametro < 1), ]
planets_lower_tierra_df

indice<-planets_df$diametro<1
small_planets_df <- planets_df[indice,]  
small_planets_df

'Ordena el Data Frame según el diámetro de los planetas, ascendentemente, usando la función order.'
largest_first_df  <- planets_df[order(planets_df$diametro), ]
largest_first_df

