

titanic %>% #cuantas personas hay por cabina
  select(Sex, Age, Cabin, Survived) %>%
  filter(Sex == 'male' & Cabin != 'No disponible' & Age >= 30 & Age <= 40) %>%
  group_by(Cabin) %>%
  summarise(Cabina_reps = n()) %>%
  arrange(desc(Cabina_reps))


#En cabina no disponible 
titanic %>%
  select(Sex, Age, Cabin, Survived) %>%
  filter(Sex == 'male' & Age >= 30 & Age <= 40 & Survived == 1) %>%
  group_by(Cabin) %>%
  summarise(Cabina_reps = n()) %>%
  arrange(desc(Cabina_reps)) %>%
  mutate(rank = rank(desc(Cabina_reps))) %>%
  filter(rank == 1)

'Ejercicio 6 '

library(dplyr)
library(ggplot2)

getwd()

setwd("/Users/WadieAlbakri/Desktop/Máster BME/Módulos/Módulo 1/R/practica_1")

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

for(i in 1:length(ibex_data_7$Fecha)){
  if(ibex_data_7$price_departure[i] >= salida_price_departure){
    ibex_data_7$Acciones_apertura[i] <- capital_inicial/ibex_data_7$open[i]
    ibex_data_7$comision_compra[i] <- prima_broker*capital_inicial
  }
} 

rm(i)

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

'--------PRUEBA--------------------'


ibex_data_8$media_datos_cierre <- (ibex_data_8$open + ibex_data_8$low + ibex_data_8$high + ibex_data_8$close)/4


ibex_data_8 <- data.frame(ibex_data_8 %>%
                            select(Fecha, X,price_departure, open, low, high, close,vol, media_datos_cierre))

#introduzco el capital asignado a cada activo cada dia.
ibex_data_8$capital_asignado <- ibex_data_8$media_datos_cierre * ibex_data_8$vol * 0.005

'---------FIN PRUEBA-------------------'

'---------código anterior-------------------'
#calculo media de los datos de cierre por dia
mean_close_key <- data.frame(ibex_data_7 %>%
  group_by(Fecha) %>%
  summarise(media_close = mean(close)))

#matcheo por fecha y asigno la media de cada dia a cada ticker
ibex_data_8<- data.frame(merge(ibex_data_7, mean_close_key, by = c("Fecha"), all.ibex_data_7 = T))

ibex_data_8 <- arrange(ibex_data_8,X,Fecha)
ibex_data_8 <- data.frame(ibex_data_8 %>%
  select(Fecha, X,price_departure, open, low, high, close,vol, media_close))

rm(mean_close_key)

#introduzco el capital asignado a cada activo cada dia.
ibex_data_8$capital_asignado <- ibex_data_8$media_close * ibex_data_8$vol * 0.005

'---------código anterior-------------------'


'calculo del stop_loss & stop_profit objetivo para cada acitvo'
                               
vector_tickers_cuantil <- unique(ibex_data_8$X)

stop_loss <- 1:length(vector_tickers_cuantil)
stop_profit <- 1:length(vector_tickers_cuantil)

matriz_cuantil <- rbind(stop_loss, stop_profit)

#df auxiliar donde introducire mis valores de stop loss y profit para cada activo
df_cuantil_stop_loss_profit <- data.frame(matriz_cuantil)
rm(matriz_cuantil, stop_loss, stop_profit)
colnames(df_cuantil_stop_loss_profit) <- vector_tickers_cuantil



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

glimpse(df_resultado_8)

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


#Estructuramos los resultados tal y como pide el enunciado
df_resultado_8T <- data.frame(t(df_resultado_8[-1]))
colnames(df_resultado_8T) <- df_resultado_8[,1]
df_resultado_8T_1 <- df_resultado_8T[9,] 
df_resultado_8T <- df_resultado_8T[1:8,]  
df_resultado_8T <- rbind(df_resultado_8T,df_cuantil_stop_loss_profit, df_resultado_8T_1)
df_resultado_8T <- round(df_resultado_8T,3)


vector_nombres <- c('Importe medio por operación','Bº medio por operación','Bº medio por euro invertido','Beneficio acumulado', '% días positivos','% días negativos','Horquilla superior media','Horquilla inferior media','Stop profit objetivo','Stop loss','Número de operaciones')
row.names(df_resultado_8T) <- vector_nombres

rm(df_resultado_8,df_resultado_8T_1, df_cuantil_stop_loss_profit, df_price_departures,ibex_data_7, ibex_data_8)
rm(capital_inicial, prima_broker, salida_price_departure, vector_nombres, vector_tickers_cuantil, ben)





