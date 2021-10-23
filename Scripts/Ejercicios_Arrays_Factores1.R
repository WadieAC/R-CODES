#EJERCICIOS DE ARRAYS Y FACTORES.
#----------------------------------------

#este vector con 3 dimensiones 2 filas 2 columnas y 2 capas de profundidad
a <- array(1:8, dim=c(2, 2, 2)) 

'FACTORES'
'------------------------------------------------------'

#Creación de factor sin orden
gender_vector <- c('M', 'F', 'F', 'M', 'M', 'F')
gender_factor <- factor(gender_vector)
gender_factor

#Creación de factor con orden (sin especificar qué orden) - alfabéticamente
size_vector <- c('S', 'L', 'M', 'L', 'S', 'M')
size_factor <- factor(size_vector, ordered = TRUE) # L < M < S ordena alfabeticamente
size_factor

#Creación de factor con orden (especificando el orden)
size_vector_2 <- c('S', 'L', 'M', 'L', 'S', 'M')
size_factor_2 <- factor(size_vector_2, ordered = TRUE, levels = c("S", "M", "L")) # S < M < L
size_factor_2

#Factores: Operaciones 
'Comprobaciones en factores sin orden solo se puede usar =='
gender_factor[1] == gender_factor[2] #Devuelve FALSE porque no están en el mismo orden
gender_factor[1] == size_factor[2] # Da ERROR: solo se pueden comparar factores si son del mismo tipo.

#En factores con orden se puede usar >, < …
size_factor[1] > size_factor[2] # Da True.

#Obtener los niveles
levels(size_factor)
levels(size_factor)[1] #podemos aplicar slicing con los niveles.

#Comprobar la existencia de niveles
any(levels(size_factor) %in% c('L', 'S')) #comprobar si existe un factor

#Añadir nuevos niveles
levels(size_factor)[length(levels(size_factor)) + 1] <- 'XL' 
levels(size_factor) <- c(levels(size_factor), 'XS')
# En ambos casos, añadimos un nivel nuevo al final del todo.

#Reordenar niveles- pero lo hará por orden alfabético.
size_factor <- factor(size_factor, ordered = TRUE, levels(size_factor[c(5, 3:1, 4)]))
size_factor


#EJERCICIOS DE FACTORES
#---------------------------------

'Crea dos factores con los siguientes vectores
1. Animales sin orden
2. Temperatura con orden'

animals_vector <- c("Elephant", "Giraffe", "Donkey", "Horse")
temperature_vector <- c("High", "Low", "High","Low", "Medium")

animals_factor <- factor(animals_vector)
temperature_factor <- factor(temperature_vector, ordered = TRUE, levels = c("Low", "Medium", "High"))


'Modifica los niveles del factor para que sean: "Female" y "Male"'
survey_vector <- c("M", "F", "F", "M", "M")

#Modo 1, modifico vector inicial y genero factores.
for(nivel in 1:length(survey_vector)){
  if(survey_vector[nivel] == "M"){
    survey_vector[nivel] <- "Male"
  }else{
    survey_vector[nivel] <- "Female"
  }
}

survey_vector
survey_factor <- factor(survey_vector)
survey_factor

#Modo 2. Genero los factores y hago el mismo for pero con el slicing al factor
survey_vector <- c("M", "F", "F", "M", "M")

length(levels(survey_factor))

survey_factor <- factor(survey_vector)
for(nivel in 1:length(levels(survey_factor))){
  if(levels(survey_factor)[nivel] == "M"){
    levels(survey_factor)[nivel] <- "Male"
  }else{
    levels(survey_factor)[nivel] <- "Female"
  }
}

'Crea un factor ordenado para el siguiente vector'
speed_vector <- c("Fast", "Slow", "Slow", "Fast", "Ultra-fast")
speed_factor <- factor(speed_vector, ordered = TRUE, levels = c("Slow","Fast","Ultra-Fast"))
speed_factor
