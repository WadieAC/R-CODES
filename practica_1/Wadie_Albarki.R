
library(dplyr)
library(ggplot2)


#setwd("/Users/WadieAlbakri/Desktop/Máster BME/Módulos/Módulo 1/R/practica_1")

directorio_Melendez <- getwd()
setwd(directorio_Melendez)

'Ejercicio 1' 
#•	Carga el set de datos en una variable llamada "titanic"

titanic <-  read.table("train.csv", 
                     header = TRUE, 
                     sep = ",", 	
                     stringsAsFactors = FALSE, 
                     dec = ".") 

#•	Obtén el número de filas y columnas incluidas en el set de datos. 
dim(titanic)


#•	Obtén las primeras 10 observaciones para ver qué pinta tienen.
head(titanic,10)

#•	Obtén el listado de variables incluidas en el set de datos y sus tipos.
str(titanic)

#•	Obtén la distribución de las variables categóricas 
#(sex, embarked, survived, pclass) del set de datos. 
#Es decir, valores únicos / valores totales. 

length(unique(titanic$Sex))/length(titanic$Sex)
length(unique(titanic$Embarked))/length(titanic$Embarked)
length(unique(titanic$Survived))/length(titanic$Survived)
length(unique(titanic$Pclass))/length(titanic$Pclass)


#•	Obtén los estadísticos básicos de las variables del set de datos.
summary(titanic)

'Ejercicio 2'
#•	Indica en qué variables hay valores con NA, NULL o vacíos "" 

  #Bucle que identifica los Null o vacios
p <- 1
variables_NULL <- c()
for( i in 7:12){
  columna <- sum(titanic[,i] == "") 
   if(columna != 0){
     variables_NULL[p] <- colnames(titanic[i])
     p <- p +1
   }
}

rm(columna,i,p)

  #Este bucle me identifica donde tengo NA
p <- 1
variables_NA <- c()
for( i in 1:dim(titanic)[2]){
  if (any(is.na(titanic[i])) == TRUE){
      variables_NA[p] <- colnames(titanic[i])
      p <- p +1
  }
}

rm(i,p)

variables_NA_NULL <- c(variables_NA, variables_NULL)
print(paste0("Variable con NA, NULL o vacíos: ", variables_NA_NULL))
rm(variables_NA, variables_NULL)

#Y cuál es su proporción dentro de la variable.
Propor_NA_Age <- c(Porcent_NA_Age = sum(is.na(titanic$Age))/nrow(titanic) * 100)
Propor_NULL_Embarked <- c(Porcent_NULL_Embarked = (sum(titanic$Embarked == "")/nrow(titanic) * 100))
Propor_NULL_Cabin <- c(Porcent_NULL_Cabin = (sum(titanic$Cabin == "")/nrow(titanic) * 100))

Porcentajes <- c(Propor_NA_Age,Propor_NULL_Embarked,Propor_NULL_Cabin)
Porcentajes

rm(Propor_NA_Age, Propor_NULL_Cabin, Propor_NULL_Embarked)

#apply(is.na(titanic), 2, mean) #Con esto me sale el % de NA's


#•	Imputa la media del valor de la variable en el caso 
#de las numéricas y "No disponible" en el caso de string. 


    #Primero quito los NA y los sustituyo en mi DF
copia_Age <- titanic$Age
copia_Age_sin_NA <- na.omit(copia_Age)
media_AGE <- mean(copia_Age_sin_NA)

for (i in 1:length(copia_Age)) {
  if (any(is.na(copia_Age[i])) == TRUE){
      copia_Age[i] <- media_AGE
  }
}

titanic$Age <- copia_Age #sustituyo el vector del DF con NA por mi media.
rm(i, copia_Age, copia_Age_sin_NA, media_AGE)

     #Sustituyo los espacios por "No disponible"

copia_Cabin <- titanic$Cabin
copia_Embarked <- titanic$Embarked
no_disponible <- "No disponible"

for( i in 1:length(copia_Cabin)){
  columna <- "" 
  if(copia_Cabin[i] == columna){
     copia_Cabin[i] <- no_disponible
  }
}

titanic$Cabin <- copia_Cabin

for( i in 1:length(copia_Embarked)){
  columna <- "" 
  if(copia_Embarked[i] == columna){
    copia_Embarked[i] <- no_disponible
  }
}

titanic$Embarked <- copia_Embarked

rm(i,copia_Cabin,copia_Embarked, no_disponible, columna)


#•	Vuelve a comprobar la existencia de NAs y NULL 
#para verificar que los has eliminado.

Na_Age_vector <- any(is.na(titanic$Age))
Null_Cabin_vector <- sum(titanic$Cabin == "")
Null_Embarked_vector <- sum(titanic$Embarked == "")

verificacion <- c(Na_Age = Na_Age_vector, Null_Cabin = Null_Cabin_vector, Null_Embarked = Null_Embarked_vector)
verificacion

rm(Na_Age_vector, Null_Embarked_vector, Null_Cabin_vector, verificacion)


'Ejercicio 3' 
#•	¿Fallecieron más mujeres u hombres? 

Survived_titanic <- as.numeric(titanic$Survived)
Sex_titanic <- as.character(titanic$Sex)
sexo_superviviente <- data.frame(Supervivientes = Survived_titanic, Sexo= Sex_titanic)

hombre_moridos <- sexo_superviviente %>%
  filter(Supervivientes == 0 & Sexo == 'male')%>%
  summarise(Hombres_caidos = n())

mujeres_moridas <- sexo_superviviente %>%
  filter(Supervivientes == 0 & Sexo == 'female')%>%
  summarise(Mujeres_caidas = n())


#Con este código lo tiro en una sola pipe
sexo_superviviente %>%
  arrange(Sexo) %>%
  group_by(Sexo) %>%
  filter(Supervivientes == 0) %>%
  summarise(Defunciones = n())

if( hombre_moridos[1,1] > mujeres_moridas[1,1]){
  print(paste0("Fallecieron más hombres que mujeres total: ", hombre_moridos[1,1]))
}else{
  print(paste0("Fallecieron más mujeres que hombres total", mujeres_moridas[1,1]))
}


#(Cuánto en porcentaje sobre el total de su género).
total_hombres <- sexo_superviviente %>%
  filter(Sexo == 'male') %>%
  summarise(Nº_total_hombres = n())

total_mujeres <- sexo_superviviente %>%
  filter(Sexo == 'female') %>%
  summarise(Nº_total_mujeres = n())

Porcentaje_fallecidos <- data.frame(Porcent_dead_men = (hombre_moridos[1,1]/total_hombres[1,1])*100, Porcent_dead_Women = (mujeres_moridas[1,1]/total_mujeres[1,1])*100)
Porcentaje_fallecidos

rm(hombre_moridos, mujeres_moridas, sexo_superviviente, Survived_titanic, Sex_titanic)
rm(total_hombres, total_mujeres)

#•	¿Qué clase de pasajeros (primera, segunda o tercera) sobrevivió más?
#Quiero que el resultado de, únicamente, el porcentaje y la clase que más 
#sobrevivió (no el porcentaje de las 3 clases).


#Selecciono solo las columnas que me interesan
filtro_titanic <- data.frame(titanic$Pclass, titanic$Survived)

#Monto un DF filtrado
filtro1 <- data.frame(filtro_titanic %>%
  arrange(titanic.Pclass) %>%
  group_by(titanic.Pclass, titanic.Survived) %>%
  summarise(Conteo=n())) 

#Con esto cuento el numero total de pasajeros por clase
pasajeros_total_clase <- data.frame(filtro1 %>%
  group_by(Clase = titanic.Pclass) %>%
  summarise(Total_por_clase = sum(Conteo)))

#Con esto saco el numero de supervivientes por clase
supervivientes_por_clase <- data.frame(filtro1 %>% 
  filter(titanic.Survived == 1) %>%
  select(Clase = titanic.Pclass, nº_Supervivientes = Conteo))

#Añado la columna para calcular estadisticos despues
pasajeros_total_clase$nº_Supervivientes <- cbind(supervivientes_por_clase$nº_Supervivientes)

pasajeros_total_clase %>%
  mutate(Proporcion_supervivientes = (nº_Supervivientes/Total_por_clase)*100) %>%
  select(Clase, Proporcion_supervivientes) %>%
  mutate(rank = rank(desc(Proporcion_supervivientes))) %>%
  filter(rank == 1)


rm(filtro_titanic, filtro1, pasajeros_total_clase, supervivientes_por_clase)


#•	¿Cuál fue la edad media y máxima de los supervivientes 
#en cada una de las clases?

titanic %>%
  arrange(Pclass) %>%
  group_by(Pclass) %>%
  filter(Survived == 1) %>%
  summarise(Edad_media=mean(Age),Edad_maxima= max(Age))


'Ejercicio 4' 
#•	¿Cuál de los puertos de embarque es el que tiene la media de precio de billete más barata? 
#Indica solo ese puerto, no una lista con todos.


titanic %>%
  group_by(Puerto = Embarked) %>%
  summarise(Media_billete = mean(Fare)) %>%
  mutate(Orden = rank(Media_billete)) %>%
  filter(Orden == 1) %>%
  select(Puerto, Media_billete)
  

#•	¿Qué correlación hay entre la longitud del nombre de un pasajero y el importe de su billete?
#No modifiques el nombre del pasajero para hacer el cálculo (úsalo tal y como venga).

titanic %>% 
  select(Fare, Name) %>%
  mutate(Longitud_nombre = nchar(Name)) %>%
  select(Fare,Longitud_nombre) %>%
  mutate(cor(Longitud_nombre, Fare)) %>%
  arrange(desc(Longitud_nombre))


'Ejercicio 5' 
#•	Obtén los nombres de los pasajeros que no sobrevivieron y el precio de su billete está 
#en el decil superior. 

titanic %>%
  filter(Survived == 0 & Fare >= (quantile(Fare, probs = 0.9))) %>%
  select(Name)


#•	¿En qué cabina deberías alojarte para tener una mayor probabilidad de sobrevivir
#siendo hombre de entre 30 y 40 años?


#Quitando el dato de cabina no disponible - E25 resultado.

titanic %>% #cuantas personas sobreviven por cabina sin 'no disponible'
  select(Sex, Age, Cabin, Survived) %>%
  filter(Sex == 'male' & Cabin != 'No disponible' & Age >= 30 & Age <= 40 & Survived == 1) %>%
  group_by(Cabin) %>%
  summarise(Cabina_reps = n()) %>%
  arrange(desc(Cabina_reps)) %>%
  mutate(rank = rank(desc(Cabina_reps))) %>%
  filter(rank == 1)
  

'Ejercicio 6 '
'------------------------------------------'


ibex_data <-  read.table("ibex_data.csv", 
                         header = TRUE, 
                         sep = ",", 	
                         stringsAsFactors = FALSE, 
                         dec = ".") 

names(ibex_data)[2] <- "Fecha"
capital_inicial <- 30000
prima_broker <- 0.0003



'Algoritmo de inversión - lógica'

ibex_data$Acciones_apertura <- capital_inicial/ibex_data$open
ibex_data$low_high_close <- 0


for(i in 1:length(ibex_data$X)){
  if(ibex_data$low[i] <= (ibex_data$open[i]-0.10)){
    ibex_data$low_high_close[i] <- (ibex_data$Acciones_apertura[i]*(ibex_data$open[i]-0.10))
  }else if(ibex_data$high[i] >= (ibex_data$open[i]+0.03)){
    ibex_data$low_high_close[i] <- (ibex_data$Acciones_apertura[i]*(ibex_data$open[i]+0.03))
  }else{
    ibex_data$low_high_close[i] <- ibex_data$Acciones_apertura[i]*ibex_data$close[i]
  }
}


rm(i)

ibex_data$comision_compra <- prima_broker*capital_inicial
ibex_data$comision_venta <- prima_broker*ibex_data$low_high_close
ibex_data$comision_total <- ibex_data$comision_compra + ibex_data$comision_venta
ibex_data$beneficio_dia_ticker <- ibex_data$low_high_close - capital_inicial - ibex_data$comision_total


#filtrar la tabla(elimina) para aquellos tickers con menos de 30 datos
tickers_out <- data.frame(ibex_data %>%
                            select(ticker) %>%
                            group_by(ticker) %>%
                            summarise(numero_datos = n()) %>%
                            filter(numero_datos< 30))

vector_tickers <- tickers_out[,1] 
ibex_data <- ibex_data[ibex_data$X != vector_tickers,]

rm(tickers_out, vector_tickers)


'código para obtener el beneficio medio por ticker' 
df_resultado_6 <- data.frame(ibex_data %>%
                               select(ticker, beneficio_dia_ticker) %>%
                               group_by(ticker) %>%
                               summarise('Bº medio por operacion' = mean(beneficio_dia_ticker)))



'código para obtener el beneficio acumulado por ticker' 
df_resultado_6$Beneficio_acumulado <- data.frame(ibex_data %>%
                                                   select(ticker, beneficio_dia_ticker) %>%
                                                   group_by(ticker) %>%
                                                   summarise(Beneficio_acumulado= sum(beneficio_dia_ticker)) %>%
                                                   select(Beneficio_acumulado))


'código para obtener porcentaje dias positivos y negativos'
#Dias positivos sin 0 incluido.
df_dias_positivos <- data.frame(ibex_data %>%
                                  select(ticker, beneficio_dia_ticker) %>%
                                  filter(beneficio_dia_ticker>0) %>%
                                  group_by(ticker) %>%
                                  summarise(dias_positivos = n()))


#Dias negativos
df_dias_negativos <- data.frame(ibex_data %>%
                                  select(ticker, beneficio_dia_ticker) %>%
                                  filter(beneficio_dia_ticker<0) %>%
                                  group_by(ticker) %>%
                                  summarise(dias_negativos = n()))

#matching total para homogeneizar datos. Los dias de beneficio 0 pone NA
matcheo_dias <- merge(df_dias_positivos, df_dias_negativos, by = c("ticker"), all = T) 


#cambio los NA por 0 en dias positivos
for (i in 1:length(matcheo_dias$dias_positivos)) {
  if (any(is.na(matcheo_dias$dias_positivos[i])) == TRUE){
    matcheo_dias$dias_positivos[i] <- 0
  }
}

rm(i)

#cambio los NA por 0 en dias negativos
for (i in 1:length(matcheo_dias$dias_negativos)) {
  if (any(is.na(matcheo_dias$dias_negativos[i])) == TRUE){
    matcheo_dias$dias_negativos[i] <- 0
  }
}

rm(i)

df_porcen_dias <- data.frame(matcheo_dias %>%
                               mutate(dias_totales = dias_positivos+dias_negativos, '%_dias_positivos' = dias_positivos/dias_totales , '%_dias_negativos'= dias_negativos/dias_totales) %>%
                               select('%_dias_positivos', '%_dias_negativos'))


df_resultado_6$porcen_dias_positivos <- df_porcen_dias[,1]
df_resultado_6$porcen_dias_negativos <- df_porcen_dias[,2]

rm(df_dias_positivos,df_dias_negativos, matcheo_dias, df_porcen_dias)


'código para obtener las horquillas' 
df_horquillas <- data.frame(ibex_data %>%
                              select(ticker, high, open, low) %>%
                              mutate(horquilla_sup = high-open, horquilla_inf = open - low) %>%
                              group_by(ticker) %>%
                              summarise(horquilla_sup_media = mean(horquilla_sup), horquilla_inf_media = mean(horquilla_inf)))

df_resultado_6$Horquilla_superior_media <- df_horquillas[,2]
df_resultado_6$Horqilla_inferior_media <- df_horquillas[,3]

rm(df_horquillas)

'codigo para obtener el numero de operaciones' 
df_resultado_6$Numero_de_operaciones <- data.frame(ibex_data %>%
                                                     select(ticker) %>%
                                                     group_by(ticker) %>%
                                                     summarise(numero_datos = n()) %>%
                                                     select(numero_datos))


#Estructuramos los resultados tal y como pide el enunciado
df_resultado_6T <- data.frame(t(df_resultado_6[-1]))
colnames(df_resultado_6T) <- df_resultado_6[, 1]

vector_nombres <- c('Bº medio por operación','Beneficio acumulado', '% días positivos','% días negativos','Horquilla superior media','Horquilla inferior media','Número de operaciones')
row.names(df_resultado_6T) <- vector_nombres

df_resultado_6T <- round(df_resultado_6T,3)

rm(df_resultado_6)


'ploteo del beneficio medio acumulado vs numero de operaciones'
ibex_data$beneficio_acum_for <- 0
ibex_data$numero_operacion <- 0

#inicializar la primera celda para que tome un valor, obtengo primero el beneficio acumulado por dia y un conteo de las ops
ibex_data$beneficio_acum_for[1] <- ibex_data$beneficio_dia_ticker[1]
ibex_data$numero_operacion[1] <- 1

for(i in 2:(length(ibex_data$X))){
  if (ibex_data$X[i-1] == ibex_data$X[i]){
    ibex_data$beneficio_acum_for[i] <-  ibex_data$beneficio_acum_for[i-1] + ibex_data$beneficio_dia_ticker[i]
    ibex_data$numero_operacion[i] <-  ibex_data$numero_operacion[i-1] + 1
  }else{
    ibex_data$beneficio_acum_for[i] =  ibex_data$beneficio_dia_ticker[i]
    ibex_data$numero_operacion[i] =  1
  }
}


rm(i)


'código para plotear todos los tickers a la vez'
for(activo in unique(ibex_data$ticker)){
  
  plot_activo <- data.frame(ibex_data %>%
                              select(ticker, numero_operacion, beneficio_acum_for) %>%
                              filter(ticker == activo ) %>%
                              select(numero_operacion, beneficio_acum_for))
  
  plot(plot_activo$numero_operacion, plot_activo$beneficio_acum_for,
       xlab = 'Nº operacion',
       ylab = 'Beneficio acumulado',
       main = activo,
       type="l")
  
}

rm(activo, ibex_data)


'Ejercicio 7' 
#--------------------------------------------------------------
price_departures <-  read.table("price_departures.csv", 
                                header = TRUE, 
                                sep = ",", 	
                                stringsAsFactors = FALSE, 
                                dec = ".") 


#Primero voy a transformar mi matriz de Price departures para dejarla como ibex_data y reutilizar código

vector_fechas <- as.character(price_departures$X) 
vector_tickers_price <- colnames(price_departures) 
vector_tickers_price <- vector_tickers_price[2:80]


vector_valores_price <- c()

for (i in 2:(length(colnames(price_departures)))){
  vector_valores_price <- append(vector_valores_price, price_departures[,i] )
}

rm(i)

vector_fechas <- rep(vector_fechas, 79)

vector_tickers_price <- rep(vector_tickers_price, 3953)
vector_tickers_price <- sort(vector_tickers_price)

df_price_departures <- data.frame(X = vector_tickers_price,Fecha = vector_fechas,price_departures = vector_valores_price)

rm(vector_tickers_price,vector_valores_price,vector_fechas)

colnames(df_price_departures) <- c("X","Fecha","price_departure")



# Matcheo por fechas, elimino las filas con NA y tickers con menos de 30 datos
df_price_departures <- na.omit(df_price_departures)


ibex_data_7 <-  read.table("ibex_data.csv", 
                           header = TRUE, 
                           sep = ",", 	
                           stringsAsFactors = FALSE, 
                           dec = ".") 

names(ibex_data_7)[2] <- "Fecha"

ibex_data_7<- merge(ibex_data_7, df_price_departures, by = c("X","Fecha"), all.ibex_data_7 = T) 


'Código para quitar los valores con menos de 30 datos'
tickers_out_price <- (data.frame(ibex_data_7) %>%
                        select(X) %>%
                        group_by(X) %>%
                        summarise(numero_datos = n()) %>%
                        filter(numero_datos< 30))


vector_tickers_out_price <- as.character(tickers_out_price[,1]) 
ibex_data_7 <- ibex_data_7[ibex_data_7$X != vector_tickers_out_price,]

ibex_data_7 <- data.frame(ibex_data_7 %>%
                            select(Fecha, X, price_departure, open, low, high, close, vol))

rm(tickers_out_price, vector_tickers_out_price)


'Algoritmo de inversión - lógica'
#compra a precio de apertura cuando price_departure >= 0.75
salida_price_departure <- 0.75
ibex_data_7$Acciones_apertura <- 0
ibex_data_7$low_high_close <- 0
ibex_data_7$comision_compra <- 0

#asigno el capital a aquellos que cumplen el price departure, los que no los dejo en 0
for(i in 1:length(ibex_data_7$Fecha)){
  if(ibex_data_7$price_departure[i] >= salida_price_departure){
    ibex_data_7$Acciones_apertura[i] <- capital_inicial/ibex_data_7$open[i]
    ibex_data_7$comision_compra[i] <- prima_broker*capital_inicial
  }
} 

rm(i)

#algoritmo de inversión
for(i in 1:length(ibex_data_7$Fecha)){
  if(ibex_data_7$low[i] <= (ibex_data_7$open[i]-0.10)){
    ibex_data_7$low_high_close[i] <- (ibex_data_7$Acciones_apertura[i]*(ibex_data_7$open[i]-0.10))
  }else if(ibex_data_7$high[i] >= (ibex_data_7$open[i]+0.03)){
    ibex_data_7$low_high_close[i] <- (ibex_data_7$Acciones_apertura[i]*(ibex_data_7$open[i]+0.03))
  }else{
    ibex_data_7$low_high_close[i] <- ibex_data_7$Acciones_apertura[i]*ibex_data_7$close[i]
  }
}

rm(i)

ibex_data_7$comision_venta <- prima_broker*ibex_data_7$low_high_close
ibex_data_7$comision_total <- ibex_data_7$comision_compra + ibex_data_7$comision_venta


'Cálculo del beneficio/dia ticker'
for(i in 1:length(ibex_data_7$Fecha)){
  if(ibex_data_7$Acciones_apertura[i] == 0){
    ibex_data_7$beneficio_dia_ticker[i] <- 0
  }else{
    ibex_data_7$beneficio_dia_ticker[i] <- ibex_data_7$low_high_close[i] - capital_inicial - ibex_data_7$comision_total[i]
  }
} 

rm(i)



'código para obtener el beneficio medio por ticker' 
df_resultado_7 <- data.frame(ibex_data_7 %>%
                               select(X, beneficio_dia_ticker, price_departure) %>%
                               filter(price_departure >= salida_price_departure) %>% 
                               group_by(X) %>%
                               summarise('Bº medio por operacion' = mean(beneficio_dia_ticker)))




'código para obtener el beneficio acumulado por ticker' 
df_resultado_7$Beneficio_acumulado <- data.frame(ibex_data_7 %>%
                                                   select(X, beneficio_dia_ticker, price_departure) %>%
                                                   filter(price_departure >= salida_price_departure) %>% 
                                                   group_by(X) %>%
                                                   summarise(Beneficio_acumulado= sum(beneficio_dia_ticker)) %>%
                                                   select(Beneficio_acumulado))


'código para obtener porcentaje dias positivos y negativos'
#Dias positivos sin 0 incluido.
df_dias_positivos_7 <- data.frame(ibex_data_7 %>%
                                    select(X, beneficio_dia_ticker) %>%
                                    filter(beneficio_dia_ticker>0) %>%
                                    group_by(X) %>%
                                    summarise(dias_positivos = n()))


#Dias negativos
df_dias_negativos_7 <- data.frame(ibex_data_7 %>%
                                    select(X, beneficio_dia_ticker) %>%
                                    filter(beneficio_dia_ticker<0) %>%
                                    group_by(X) %>%
                                    summarise(dias_negativos = n()))

#matching total para homogeneizar datos. Los dias de beneficio 0 pone NA
matcheo_dias_7 <- merge(df_dias_positivos_7, df_dias_negativos_7, by = c("X"), all = T) 


#cambio los NA por 0 en dias positivos
for (i in 1:length(matcheo_dias_7$dias_positivos)) {
  if (any(is.na(matcheo_dias_7$dias_positivos[i])) == TRUE){
    matcheo_dias_7$dias_positivos[i] <- 0
  }
}

rm(i)

#cambio los NA por 0 en dias negativos
for (i in 1:length(matcheo_dias_7$dias_negativos)) {
  if (any(is.na(matcheo_dias_7$dias_negativos[i])) == TRUE){
    matcheo_dias_7$dias_negativos[i] <- 0
  }
}

rm(i)

df_porcen_dias_7 <- data.frame(matcheo_dias_7 %>%
                                 mutate(dias_totales = dias_positivos+dias_negativos, '%_dias_positivos' = dias_positivos/dias_totales , '%_dias_negativos'= dias_negativos/dias_totales) %>%
                                 select('%_dias_positivos', '%_dias_negativos'))


df_resultado_7$porcen_dias_positivos <- df_porcen_dias_7[,1]
df_resultado_7$porcen_dias_negativos <- df_porcen_dias_7[,2]

rm(df_dias_positivos_7,df_dias_negativos_7, matcheo_dias_7, df_porcen_dias_7)


'código para obtener las horquillas' 
df_horquillas_7 <- data.frame(ibex_data_7 %>%
                                select(X, high, open, low, price_departure) %>%
                                filter(price_departure >= salida_price_departure) %>% 
                                mutate(horquilla_sup = high - open, horquilla_inf = open - low) %>%
                                group_by(X) %>%
                                summarise(horquilla_sup_media = mean(horquilla_sup), horquilla_inf_media = mean(horquilla_inf)))

df_resultado_7$Horquilla_superior_media <- df_horquillas_7[,2]
df_resultado_7$Horqilla_inferior_media <- df_horquillas_7[,3]


rm(df_horquillas_7)

'codigo para obtener el numero de operaciones' 
df_resultado_7$Numero_de_operaciones <- data.frame(ibex_data_7 %>%
                                                     select(X, price_departure) %>%
                                                     group_by(X) %>%
                                                     filter(price_departure >= salida_price_departure) %>% 
                                                     summarise(numero_datos = n()) %>%
                                                     select(numero_datos))


#Estructuramos los resultados tal y como pide el enunciado
df_resultado_7T <- data.frame(t(df_resultado_7[-1]))
colnames(df_resultado_7T) <- df_resultado_7[, 1]
vector_nombres <- c('Bº medio por operación','Beneficio acumulado', '% días positivos','% días negativos','Horquilla superior media','Horquilla inferior media','Número de operaciones')
row.names(df_resultado_7T) <- vector_nombres

df_resultado_7T <- round(df_resultado_7T,3)

rm(df_resultado_7)


'ploteo del beneficio medio acumulado vs numero de operaciones'

ibex_data_7$beneficio_acum_for <- 0
ibex_data_7$numero_operacion <- 0
ibex_data_7 <- ibex_data_7[ibex_data_7$price_departure >= salida_price_departure,]

#inicializar la primera celda para que tome un valor, obtengo primero el beneficio acumulado por dia y un conteo de las ops
ibex_data_7$beneficio_acum_for[1] <- ibex_data_7$beneficio_dia_ticker[1]
ibex_data_7$numero_operacion[1] <- 1

for(i in 2:(length(ibex_data_7$X))){
  if (ibex_data_7$X[i-1] == ibex_data_7$X[i]){
    ibex_data_7$beneficio_acum_for[i] <-  ibex_data_7$beneficio_acum_for[i-1] + ibex_data_7$beneficio_dia_ticker[i]
    ibex_data_7$numero_operacion[i] <-  ibex_data_7$numero_operacion[i-1] + 1
  }else{
    ibex_data_7$beneficio_acum_for[i] <-   ibex_data_7$beneficio_dia_ticker[i]
    ibex_data_7$numero_operacion[i] <-   1
  }
}

rm(i)


#código para plotear todos los tickers a la vez
for(activo in unique(ibex_data_7$X)){
  
  plot_activo_7 <- data.frame(ibex_data_7 %>%
                                select(X, numero_operacion, beneficio_acum_for) %>%
                                filter(X == activo) %>%
                                select(numero_operacion, beneficio_acum_for))
  
  plot(plot_activo_7$numero_operacion, plot_activo_7$beneficio_acum_for,
       xlab = 'Nº operacion',
       ylab = 'Beneficio acumulado',
       main = activo,
       type="l")
  
}



rm(activo, price_departures)


'Ejercicio 8'
'-------------------------------------------------------------'


ibex_data_8 <- ibex_data_7[,1:9]

'vamos a calcular la media de los datos de cierre del dia y asignarla'

ibex_data_8$media_datos_cierre <- (ibex_data_8$open + ibex_data_8$low + ibex_data_8$high + ibex_data_8$close)/4


ibex_data_8 <- data.frame(ibex_data_8 %>%
                            select(Fecha, X,price_departure, open, low, high, close,vol, media_datos_cierre))

#introduzco el capital asignado a cada activo cada dia.
ibex_data_8$capital_asignado <- ibex_data_8$media_datos_cierre * ibex_data_8$vol * 0.005


'calculo del stop_loss & stop_profit objetivo para cada acitvo'

vector_tickers_cuantil <- unique(ibex_data_8$X)

stop_loss <- 1:length(vector_tickers_cuantil)
stop_profit <- 1:length(vector_tickers_cuantil)

matriz_cuantil <- rbind(stop_loss, stop_profit)

#df auxiliar donde introducire mis valores de stop loss y profit para cada activo
df_cuantil_stop_loss_profit <- data.frame(matriz_cuantil)
rm(matriz_cuantil, stop_loss, stop_profit)
colnames(df_cuantil_stop_loss_profit) <- vector_tickers_cuantil

'Para calcular el stop_loss y el stop_profit de cada activo monto un vector dinámico que metere en un for y enchufare cada valor en un df'
#inicializar el vector con los primeros datos del primer ticker
vector_tickers_dinamico_loss <- c(ibex_data_8$open[1] - ibex_data_8$low[1])
vector_tickers_dinamico_profit <- c(ibex_data_8$high[1] - ibex_data_8$open[1])
j <- 1


#For para calcular para cada activo el cuantil correspondiente del stop loss y el stop profit y enchufarlo en un df auxiliar
for(i in 2:(length(ibex_data_8$X))){
  if (ibex_data_8$X[i] == ibex_data_8$X[i-1]){
    vector_tickers_dinamico_loss <- c(vector_tickers_dinamico_loss,(ibex_data_8$open[i]-ibex_data_8$low[i]))
    vector_tickers_dinamico_profit <- c(vector_tickers_dinamico_profit,(ibex_data_8$high[i]-ibex_data_8$open[i]))
  }else{
    df_cuantil_stop_loss_profit[1,j] <- quantile(vector_tickers_dinamico_loss,0.8)
    df_cuantil_stop_loss_profit[2,j] <- quantile(vector_tickers_dinamico_profit,0.3)
    j = j +1
    vector_tickers_dinamico_loss <- c(ibex_data_8$low[i],ibex_data_8$open[i])
    vector_tickers_dinamico_profit <- c(ibex_data_8$open[i],ibex_data_8$high[i])
  }
}

#como el último else no lo ejecuta cierro el data frame 'a mano' fuera del for.
df_cuantil_stop_loss_profit[1,j] <- quantile(vector_tickers_dinamico_loss,0.8)
df_cuantil_stop_loss_profit[2,j] <- quantile(vector_tickers_dinamico_profit,0.3)

rm(i,j,vector_tickers_dinamico_loss,vector_tickers_dinamico_profit)


'logica del algoritmo'
ibex_data_8$Acciones_apertura <- ibex_data_8$capital_asignado/ibex_data_8$open
ibex_data_8$low_high_close <- 0
ibex_data_8$comision_compra <- prima_broker*ibex_data_8$capital_asignado


for(i in 1:length(ibex_data_8$Fecha)){
  if(ibex_data_8$low[i] <= (ibex_data_8$open[i]-(df_cuantil_stop_loss_profit[1,(ibex_data_8$X[i])]))){
    ibex_data_8$low_high_close[i] <- (ibex_data_8$Acciones_apertura[i]*(ibex_data_8$open[i]-(df_cuantil_stop_loss_profit[1,(ibex_data_8$X[i])])))
  }else if(ibex_data_8$high[i] >= (ibex_data_8$open[i]+(df_cuantil_stop_loss_profit[2,(ibex_data_8$X[i])]))){
    ibex_data_8$low_high_close[i] <- (ibex_data_8$Acciones_apertura[i]*(ibex_data_8$open[i]+(df_cuantil_stop_loss_profit[2,(ibex_data_8$X[i])])))
  }else{
    ibex_data_8$low_high_close[i] <- ibex_data_8$Acciones_apertura[i]*ibex_data_8$close[i]
  }
}

rm(i)

ibex_data_8$comision_venta <- prima_broker*ibex_data_8$low_high_close
ibex_data_8$comision_total <- ibex_data_8$comision_compra + ibex_data_8$comision_venta
ibex_data_8$beneficio_dia_ticker <- ibex_data_8$low_high_close - ibex_data_8$capital_asignado - ibex_data_8$comision_total


'código para obtener el beneficio medio por ticker' 
df_resultado_8 <- data.frame(ibex_data_8 %>%
                               select(X, beneficio_dia_ticker) %>%
                               group_by(X) %>%
                               summarise('Bº medio por operacion' = mean(beneficio_dia_ticker)))




'código para obtener el beneficio acumulado por ticker' 
df_resultado_8$Beneficio_acumulado <- data.frame(ibex_data_8 %>%
                                                   select(X, beneficio_dia_ticker) %>%
                                                   group_by(X) %>%
                                                   summarise(Beneficio_acumulado= sum(beneficio_dia_ticker)) %>%
                                                   select(Beneficio_acumulado))


'código para obtener porcentaje dias positivos y negativos'
#Dias positivos sin 0 incluido.
df_dias_positivos_8 <- data.frame(ibex_data_8 %>%
                                    select(X, beneficio_dia_ticker) %>%
                                    filter(beneficio_dia_ticker>0) %>%
                                    group_by(X) %>%
                                    summarise(dias_positivos = n()))


#Dias negativos
df_dias_negativos_8 <- data.frame(ibex_data_8 %>%
                                    select(X, beneficio_dia_ticker) %>%
                                    filter(beneficio_dia_ticker<0) %>%
                                    group_by(X) %>%
                                    summarise(dias_negativos = n()))

#matching total para homogeneizar datos. Los dias de beneficio 0 pone NA
matcheo_dias_8 <- merge(df_dias_positivos_8, df_dias_negativos_8, by = c("X"), all = T) 


#cambio los NA por 0 en dias positivos
for (i in 1:length(matcheo_dias_8$dias_positivos)) {
  if (any(is.na(matcheo_dias_8$dias_positivos[i])) == TRUE){
    matcheo_dias_8$dias_positivos[i] <- 0
  }
}

rm(i)

#cambio los NA por 0 en dias negativos
for (i in 1:length(matcheo_dias_8$dias_negativos)) {
  if (any(is.na(matcheo_dias_8$dias_negativos[i])) == TRUE){
    matcheo_dias_8$dias_negativos[i] <- 0
  }
}

rm(i)

df_porcen_dias_8 <- data.frame(matcheo_dias_8 %>%
                                 mutate(dias_totales = dias_positivos+dias_negativos, '%_dias_positivos' = dias_positivos/dias_totales , '%_dias_negativos'= dias_negativos/dias_totales) %>%
                                 select('%_dias_positivos', '%_dias_negativos'))


df_resultado_8$porcen_dias_positivos <- df_porcen_dias_8[,1]
df_resultado_8$porcen_dias_negativos <- df_porcen_dias_8[,2]

rm(df_dias_positivos_8,df_dias_negativos_8, matcheo_dias_8, df_porcen_dias_8)


'código para obtener las horquillas' 
df_horquillas_8 <- data.frame(ibex_data_8 %>%
                                select(X, high, open, low, price_departure) %>%
                                mutate(horquilla_sup = high - open, horquilla_inf = open - low) %>%
                                group_by(X) %>%
                                summarise(horquilla_sup_media = mean(horquilla_sup), horquilla_inf_media = mean(horquilla_inf)))

df_resultado_8$Horquilla_superior_media <- df_horquillas_8[,2]
df_resultado_8$Horquilla_inferior_media <- df_horquillas_8[,3]


rm(df_horquillas_8)


'codigo para obtener el numero de operaciones' 
df_resultado_8$Numero_de_operaciones <- data.frame(ibex_data_8 %>%
                                                     select(X, price_departure) %>%
                                                     group_by(X) %>%
                                                     summarise(numero_datos = n()) %>%
                                                     select(numero_datos))

'codigo para obtener el beneficio medio por euro invertido'
df_resultado_8$total_invertido <- data.frame(ibex_data_8 %>%
                                               select(X, capital_asignado) %>%
                                               group_by(X) %>%
                                               summarise(total_invertido = sum(capital_asignado)) %>%
                                               select(total_invertido))

df_resultado_8$Beneficio_medio_euro <- (Beneficio_medio_euro = df_resultado_8$Beneficio_acumulado/df_resultado_8$total_invertido)
df_resultado_8$importe_medio_operacion <- df_resultado_8$total_invertido/df_resultado_8$Numero_de_operaciones


#Estructuramos los resultados tal y como pide el enunciado
df_resultado_8 <- data.frame(df_resultado_8 %>%
                               select(X,
                                      importe_medio_operacion,
                                      Bº.medio.por.operacion,
                                      Beneficio_medio_euro,
                                      Beneficio_acumulado,
                                      porcen_dias_positivos,
                                      porcen_dias_negativos,
                                      Horquilla_superior_media,
                                      Horquilla_inferior_media,
                                      Numero_de_operaciones))



df_resultado_8T <- data.frame(t(df_resultado_8[-1]))
colnames(df_resultado_8T) <- df_resultado_8[,1]
df_resultado_8T_1 <- df_resultado_8T[9,] 
df_resultado_8T <- df_resultado_8T[1:8,]  
df_resultado_8T <- rbind(df_resultado_8T,df_cuantil_stop_loss_profit, df_resultado_8T_1)
df_resultado_8T <- round(df_resultado_8T,3)


vector_nombres <- c('Importe medio por operación','Bº medio por operación','Bº medio por euro invertido','Beneficio acumulado', '% días positivos','% días negativos','Horquilla superior media','Horquilla inferior media','Stop profit objetivo','Stop loss','Número de operaciones')
row.names(df_resultado_8T) <- vector_nombres

rm(df_resultado_8,df_resultado_8T_1, df_cuantil_stop_loss_profit, df_price_departures,ibex_data_7, ibex_data_8, Beneficio_medio_euro)
rm(capital_inicial, prima_broker, salida_price_departure, vector_nombres, vector_tickers_cuantil)
rm(titanic, plot_activo, plot_activo_7)

  
