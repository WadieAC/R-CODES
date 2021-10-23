# DPLYR & PIPES.
#---------------------------------------


install.packages("dplyr")
library(dplyr)
install.packages("hflights")
library(hflights)

hflights.tbl <- tbl_df(hflights) #convierte los datos a una tabla
glimpse(hflights) #equivalente a str, me devuelve información, resumen visión un poco general.

'Selection - select' #select(tabla, col1:coln...)
'--------------------------------------'

#Crear una subtabla o una vista, tiro una select y lo almaceno en una variable
hflights.select <- select(hflights.tbl, "Year", "Month","DayofMonth")
hflights.select

glimpse(hflights)

hflights.select <- select(hflights.tbl, "Year":"DepDelay") #seleccciona desde la columna Year hasta ArrTime
select(hflights.tbl, -"Month",-"ArrTime") #selecciono todas las columnas excepto las que quedan
#Entre Month y ArrTime

select(hflights.tbl, starts_with("Air")) #Selecciona las columnas cuyo nombre empiece por "Air"

#Selecciona las columnas cuyo nombre termine en Time o Delay
select(hflights.tbl, ends_with("Time"), ends_with("Delay"))


'Creación de columnas - Mutate'
'--------------------------------------'
glimpse(hflights.tbl)
glimpse(hflights.select)

#Creo otra subtabla de la vista anterior con una nueva columna llamada loss
seleccion <- mutate(hflights.select, loss = ArrDelay - DepDelay)
seleccion #'tengo las mismas col que hflights.select + mi columna loss'

#Puedo crear todas las columnas que quiera, incluso algunas con los datos creados.
m1 <- mutate(hflights.tbl, loss = ArrDelay - DepDelay, loss_percent = loss/DepDelay *100)
m1

'Recodificación de columnas'
'-------------------------------------'
glimpse(hflights.tbl)
recode.carrier <- rep("CC",length(hflights.tbl$UniqueCarrier))
recode.carrier

#Basicamente es acceder a la columna y sustituirla por los datos del vector recode.carrier.
hflights.tbl$UniqueCarrier <- recode.carrier[hflights.tbl$UniqueCarrier]
hflights.tbl$UniqueCarrier <- recode.carrier #Este comando también funciona


'Filtrado de columnas - Filter'
'--------------------------------------'
glimpse(hflights.select)

#Voy a coger de mi hflights.select todos aquellos datos cuyo mes sea 1.
hflights.filter <- filter(hflights.select, Month == 1)

filter(hflights.tbl, Distance >= 1000) #Vuelos cuya distancia sera mayor que 1000 km

filter(hflights.tbl, UniqueCarrier %in% c("JetBlue", "Southwest", "Delta"))

#Filtra todos los vuelos que salieron a las 5.00 am o los que llegaron despues de las 22.00
filter(hflights.tbl, DepTime < 500 | ArrTime > 2200)

#Filtra todos los vuelos cancelados en sábado o domingo - fin de semana.
filter(hflights.tbl, Cancelled == 1 & DayOfWeek %in% c(6,7))


'Ordenar - Arrange'
'---------------------------------------'
hflights.arrange <- arrange(hflights.select, DepDelay)

#Ordena primero por DepDelay, después por ArrDelay
hflights.arrange <- arrange(hflights.select, DepDelay, ArrDelay)

#Puedo combinar el arrange con filter por ejemplo, filtrar y ordenar
dtc <- filter(hflights.tbl, Cancelled == 1, !is.na(DepDelay)) ; arrange(dtc, DepDelay)
#'me permite incluso tirar un arrange con una variable que estoy creando al momento :O '


'Summarise - Estadísticos'
'---------------------------------------'

#Me devolveria estos estadísticos pero al encontrar NA me peina como una oveja.
summarise(hflights.select_sinNA, min = min(DepDelay), max = max(DepDelay), mean=mean(DepDelay), median = median(DepDelay))

#Puedo trabajar con los NA's
sum(is.na(hflights.tbl$DepDelay)) #quiero saber los NAs que tiene
is.na(hflights.tbl$DepDelay)
hflights.select_sinNA <- na.omit(hflights.select$DepDelay) #Esto me devuelve un vector
dim(hflights.select_sinNA)
mean(hflights.select_sinNA) #para trabajar con el vector

estadisticos_DepDelay <- c(max = max(hflights.select_sinNA), min = min(hflights.select_sinNA), mean = mean(hflights.select_sinNA), median = median(hflights.select_sinNA), sd = sd(hflights.select_sinNA))
estadisticos_DepDelay

'Agrupar filas - Group by'
'------------------------------------------'

hflights.group <- group_by(hflights.tbl, UniqueCarrier)
hflights.group

summarise(hflights.group, n())


'PIPES'
#---------------------------
glimpse(hflights.tbl)

#Ojo solo puedes filtrar por aquello que has seleccionado, es decir primero seleccionas 
#tus variables Después de haber seleccionado tus variables en select trabajas con ellas.

hflights.tbl %>% 
  select(Year, ArrTime, DepTime, FlightNum, TailNum) %>%
  filter(ArrTime > 1000 & Year >= 2011) %>%
  mutate(Tiempo_vuelo = ArrTime - DepTime)

hflights.tbl %>% 
  select(FlightNum, TailNum, ArrTime)


hflights.tbl %>% # Selecciono la tabla.
  filter(!is.na(DepDelay)) %>% # Elimino los datos con NA (valor no disponible).
  summarise(min = min(DepDelay), max = max(DepDelay), mean = mean(DepDelay), median = median(DepDelay))

glimpse(hflights.tbl)

hflights.tbl %>% # Selecciono la tabla.
  group_by(UniqueCarrier) %>% # Agrupo la información por compañías.
  summarise(n_flights = n(),  # Obtendo el nº de vuelos. - n() es el COUNT!!!!
            n_canc = sum(Cancelled),  # Calculo el nº de vuelos cancelados.
            p_canc = mean(Cancelled) * 100, # Calculo el % de vuelos cancelados.
            avg_delay = mean(ArrDelay, na.rm = TRUE)) %>% # Calculo el retraso medio de llegada quitando los NA.
  arrange(avg_delay, p_canc) # Ordena los resultados por el retraso medio y el porcentaje de vuelos cancelados

hflights.tbl %>% 
  filter(!(is.na(ArrDelay))) %>% # Elimino los NA
  group_by(UniqueCarrier) %>% # Agrupo por compañía
  summarise(p_delay = mean(ArrDelay > 0)) %>% #Calculo el retraso medio (pero solo de los vuelos con retraso).
  mutate(rank = rank(p_delay)) %>% # creo una variable nueva que es un ranking de los vuelos con retraso.
  arrange(rank) # ordeno el ranking.



'EJEMPLOS'
'------------------------------'

#Select (selección de datos)
hflights.select <- select(hflights.tbl, ActualElapsedTime, AirTime, ArrDelay, DepDelay) # El primer parámetro es la tabla y el resto las columnas seleccionadas.

#Mutate (creación de variables)
hflights.mutate <- mutate(hflights.select, loss = ArrDelay - DepDelay) # Usando el resultado del select, creamos una nueva variable “loss”, restando dos columnas (ArrDelay y DepDelay)

#Filter (selección de datos)
hflights.select <- select(hflights.tbl, starts_with("Cancel"), DepDelay) # Seleccionamos las columas cuyo nombre empiece por “Cancel” y la columna DepDelay.

hflights.filter <- filter(hflights.select, Cancelled == 1) # Seleccionamos las filas que sean = 1 de la columna Cancelled.

#Arrange (ordenación de datos)
hflights.select <- select(hflights.tbl, TailNum, contains("Delay")) # Seleccionamos las filas que contengan “Delay” de la columna TailNum.

hflights.arrange <- arrange(hflights.select, DepDelay) # Ordena la selección por la columna DepDelay.

hflights.arrange <- arrange(hflights.select, DepDelay, ArrDelay) # Ordena por dos columnas (izq 1º).

#Summarise (estadísticos)
hflights.summarise <- summarise(hflights.select, min = min(DepDelay), max = max(DepDelay), mean = mean(DepDelay), median = median(DepDelay)) # Obtengo los estadísticos de la selección.

#Group by (agrupar filas)
hflights.group <- group_by(hflights.tbl, UniqueCarrier) # Agrupa las filas por la columna UniqueCarrier.

hflights.summarise.group <- summarise(hflights.group, avgDep = mean(DepDelay, na.rm = T), avgArr = mean(ArrDelay, na.rm = T)) # Sacamos los estadísticos de la agrupación, quitando los “NA”.

#Pipes (encadena llamadas a funciones)
#Pasa el resultado a la siguiente fila / función.

hflights.tbl %>%
  filter(!is.na(DepDelay)) %>%
  summarise(min = min(DepDelay), max = max(DepDelay), mean = mean(DepDelay), median = median(DepDelay))




#--------------------------------------------------------------------


'EJERCICIOS'
'--------------------------------------------------------------------'


'Carga la librería de vuelos del aeropuerto de Houston, conviérte la información en una tabla y guárdala en una variable.'
library(hflights)
library(dplyr)
hflights.tbl <- tbl_df(hflights)

'Recodifica la variable UniqueCarrier a partir del vector recode.carrier'
recode.carrier <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")
length(hflights.tbl$UniqueCarrier)
hflights.tbl$UniqueCarrier <- recode.carrier[hflights.tbl$UniqueCarrier]


glimpse(hflights.tbl)
unique(hflights.tbl$UniqueCarrier) #Con unique me saco los valore únicos de ese columna
hflights.tbl$UniqueCarrier

'Recodifica la variable CancellationCode a partir del vector recode.cancellation'
hflights.tbl[hflights.tbl$CancellationCode == "", "CancellationCode"] <- "E"
recode.cancellation <-  c("A" = "carrier", "B" = "weather", "C" = "FFA", "D" = "security", "E" = "not cancelled")

hflights.tbl$CancellationCode <- recode.cancellation[hflights.tbl$CancellationCode]
hflights.tbl$CancellationCode

#SELECT.
#------------------------------------------------------------------------------
'Selecciona las variables: ActualElapsedTime, AirTime, ArrDelay, DepDelay'

select(hflights.tbl, "ActualElapsedTime", "AirTime", "ArrDelay", "DepDelay")

'Selecciona las variables desde Origin a Cancelled'
select(hflights.tbl, "Origin":"Cancelled")

'Selecciona todas las variables menos aquellas entre DepTime y AirTime'
select(hflights.tbl, -"DepTime", -"AirTime")

'Selecciona todas las variables que terminan en "Delay"'
select(hflights.tbl, ends_with("Delay"))

'Selecciona las variables que terminan en "Num" o empiezan por "Cancell"'
select(hflights.tbl, ends_with("Num") | starts_with("Cancell"))

'Selecciona las que terminan en "Num" y la variable UniqueCarrier'
select(hflights.tbl, ends_with("Num"), "UniqueCarrier")

#MUTATE.
#----------------------------------------------------------------------------

'Crea la variable ActualGroundTime como la resta de ActualElapsedTime y AirTime. Guarda el resultado en g1.'
g0 <-  mutate(hflights.tbl, ActualGroundTime =  ActualElapsedTime - AirTime)
g1 <- select(g0, "ActualGroundTime") 


'Crea una nueva variable GroundTime como la suma TaxiIn y TaxiOut y guarda el resultado en g2'
g0 <- mutate(hflights.tbl, GroundTime =  TaxiIn - TaxiOut) 
g2 <-  select(g0, "GroundTime") 
g2

'Crea dos variables a la vez y guardalas en m1:
loss como la resta de ArrDelay y DepDelay
loss_percent como el ratio de ArrDelay - DepDelay entre DepDelay'

#utilizo una variable auxiliar para luego ejecutar un select y guarar solo las variables que me piden
g0 <- mutate(hflights.tbl, loss = ArrDelay - DepDelay, loss_percent = loss/ DepDelay * 100)
m1 <- select(g0, "loss", "loss_percent") 
m1


#FILTER
#---------------------------------------------------------------------------------
'Filtra los vuelos con distancia recorrida mayor o igual que 3000'

filter(hflights.tbl, Distance >= 3000)

'Filtra los vuelos de JetBlue, Southwest, o Delta'

filter(hflights.tbl, UniqueCarrier == "JetBlue" | UniqueCarrier ==  "Southwest" | UniqueCarrier == "Delta")

'Filtra los vuelos cuyo tiempo en Taxi fue mayor que el tiempo de vuelo.'

hflights.tbl$TaxiIn
hflights.tbl$AirTime

filter(hflights.tbl, !(is.na(TaxiIn + TaxiOut)) > AirTime)

filter(hflights.tbl, (!(is.na(TaxiIn)) + !(is.na(TaxiOut))) > AirTime)

'Filtra los vuelos que salieron con retraso y llegaron a tiempo'

glimpse(hflights.tbl)

vuelos_prueba <- filter(hflights.tbl, DepDelay > 0 & ArrDelay <= 0)
sum(is.na(vuelos_prueba)) #Con esto compruebo que no me ha pillado ningún NA

'Filtra los vueltos cancelados después de haber sido retrasados'
#Con glimpse vemos que 0 = Not Cancelled

filter(hflights.tbl, DepDelay > 0 & Cancelled  == 1)


#ARRANGE.
#---------------------------------------------------------------------------------

'Ordena la tabla según el retraso de salida'
dtc <- arrange(hflights.tbl, DepDelay)
dtc
  
'Ordena los vuelos según compañía y retraso de salida descendentemente'
arrange(hflights.tbl, UniqueCarrier, desc(DepDelay))


'Ordena los vuelos según el retraso total'

#1º Lo que debo saber es que un vuelo no este cancelado

glimpse(hflights.tbl)
select(hflights.tbl, na.omit(hflights.tbl$DepDelay))
filter(hflights.tbl, Cancelled == 0)
g0 <- mutate(g0, TotalDelay = na.omit(ArrDelay) + na.omit(DepDelay)) ; arrange(g0, TotalDelay)


#Código propuesto - por mispe
g0 <- filter(hflights.tbl, Cancelled == 0)

sum(is.na(g0$ArrDelay)) #Me da 649 NA's 
sum(is.na(g0$DepDelay)) #Me devuelve 0 NA's

prueba <- na.omit(g0$ArrDelay)

class(prueba)
arrange(g0, (DepDelay + ArrDelay))


'Filtra los vuelos con destino DFW y hora de salida anterior a las 8am, ordenalos según el tiempo en vuelo descendentemente.'

glimpse(hflights.tbl)
filtro <- filter(hflights.tbl, Dest == "DFW" & DepTime < 0800)
arrange(filtro, desc(AirTime)) #Con mi variable intermedia filtrada la ordeno después


#SUMMARISE
#-----------------------------------------------

'Calcula la distancia máxima de los vuelos desviados'

summarise(filter(hflights.tbl, Diverted == 1), max(Distance))

'Crea en una variable con los vuelos que no tienen NA en TaxiIn y TaxiOut.
Calcula un estadístico (max_taxi_diff) que contenga la mayor diferencia en valor absoluto 
entre TaxiIn y TaxiOut'

#temp2 <- select(hflights.tbl, na.omit(TaxiIn)) na.omit(TaxiOut))

glimpse(hflights.tbl)

hflights.tbl[complete.cases(hflights.tbl$TaxiIn)]

'Usando pipes a partir de este punto, realiza:
1. Crea una variable diff como la diferencia de TaxiIn y TaxiOut
2. Filtra aquellas filas para las que diff no es NA
3. Calcula la media'

#Para crear la variable tienes que volver a asignarla a la tabla, no basta solo con mutate
hflights.tbl <- mutate(hflights.tbl, dif = TaxiIn - TaxiOut)

#Con el pipe ya no vuelvo a nombrar la tabla, a diferencia del filter y summarise aislados.
hflights.tbl %>%
  filter(!(is.na(dif))) %>% #Quito los NA's del DataFrame
  summarise(media = mean(dif)) 



'Usando los pipes calcula cuantos vuelos nocturnos hay. Los vuelos nocturnos 
son aquellos para los que la hora de llegada es menor que la de salida.'

glimpse(hflights.tbl)

hflights.tbl %>%
  filter(ArrTime < DepTime) %>%
  summarise(vuelos_nocturnos = n()) # n() - cuenta en una variable el número de observaciones


'Usando los pipes calcula, para cada compañía los siguientes estadísticos:
n_flights: número de vuelos
n_canc: número de vuelos cancelados
p_canc: porcentaje de vuelos cancelados
avg_delay: retraso medio de llegada (cuidado con los NAs)
Ordena los resultados por el retraso medio y % de vuelos cancelados'


hflights.tbl %>%
  group_by(UniqueCarrier) %>%
  #summarise(nº_vuelos_total = n())
  filter(!(is.na(Cancelled)) & Cancelled == 1) %>%
  summarise(nº_vuelos_cancelados = n()) 

#Esta pipe me da el número de vuelos cancelados independientemente de su código de cancelación
#Es el numero de vuelos totales cancelados

hflights.tbl %>%
  group_by(UniqueCarrier) %>%
  summarise(nº_vuelos_totales = n())


'Usando una sola pipe utilizo este código porque aprovecho el 1 de código de cancelación
para sumar un cancelado'

hflights.tbl %>% # Selecciono la tabla.
  group_by(UniqueCarrier) %>% # Agrupo la información por compañías.
  summarise(n_flights = n(),  # Obtendo el nº de vuelos. - n() es el COUNT!!!!
            n_canc = sum(Cancelled),  # Calculo el nº de vuelos cancelados.
            p_canc = mean(Cancelled) * 100, # Calculo el % de vuelos cancelados.
            avg_delay = mean(ArrDelay, na.rm = TRUE)) %>% # Calculo el retraso medio de llegada quitando los NA.
  arrange(avg_delay, p_canc) # Ordena los resultados por el retraso medio y el porcentaje de vuelos cancelados



'Calcula qué avión (TailNum) voló más veces. ¿Cuántas?'

tail_vuelos <- hflights.tbl %>%
  group_by(TailNum) %>%
  filter(TailNum != "") %>%
  summarise(n_vuelos_tail = n()) %>%
  arrange(desc(n_vuelos_tail))

tail_vuelos[1,1] #fila 1 columna 1
tail_vuelos[1,2] #Fila 1 columna 2

max_avion <- c(tail_vuelos[1,1], tail_vuelos[1,2])
max_avion


'¿Cuántos aviones volaron a un único destino?' #probar con un join
  
glimpse(hflights.tbl)

filtro_1 <- hflights.tbl %>%
   group_by(TailNum) %>%
   filter(TailNum != "") %>%
   select(TailNum, Dest) %>%
   arrange(TailNum)
 
filtro_2 <- unique(filtro_1) #quito repetidos
filtro_2.5 <- data.frame(Numero_cola= filtro_2$TailNum) 

#Creo un data.frame porque la función aggregate es la que me va a contar las veces 
#que se repite una variable

filtro_3 <- aggregate(filtro_2.5$Numero_cola,filtro_2.5, length)
colnames(filtro_3)[2] <- "Incidencias"

vuelos_unicos <- filtro_3 %>%
  filter(Incidencias == 1) %>%
  summarise(nº_aviones_unico_destino = n())

vuelos_unicos

#Código sencillo de Guillermo
hflights.tbl %>%
  group_by(TailNum) %>%
  summarise(ndest = n_distinct(Dest)) %>%
  filter(ndest == 1) %>%
  summarise(nplanes = n())

# n_distinct es la equivalente de length(unique(x))

'¿Cuál es el destino más visitado de cada compañía?'
count(hflights.tbl, UniqueCarrier, Dest)

#Con esto saco los vuelos de cada compañia a cada destino
conteo_company_dest <- hflights.tbl %>%
  group_by(UniqueCarrier) %>%
  select(UniqueCarrier, Dest) %>%
  summarise(count(hflights.tbl, UniqueCarrier, Dest))

conteo_company_dest <- hflights.tbl %>%
  group_by(UniqueCarrier, Dest) %>%
  select(UniqueCarrier, Dest) %>%
  summarise(count(hflights.tbl, UniqueCarrier, Dest)) %>%
  mutate(ranking = rank(n))

  
conteo_company_dest #falta eliminar el 2º, 3º, 4º...destino de cada compañia


#Resuelto con el código de Guillermo
hflights.tbl %>% 
  group_by(UniqueCarrier, Dest) %>%
  summarise(n = n()) %>%
  mutate(rank = rank(desc(n))) %>%
  filter(rank == 1)



'¿Qué compañías viaja más a cada destino?'
hflights.tbl %>% 
  group_by(Dest, UniqueCarrier) %>%
  summarise(n = n()) %>%
  mutate(rank = rank(desc(n))) %>%
  filter(rank == 1)









