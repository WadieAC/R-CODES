
'Funciones muy útiles'

sort(x) # ordena el vector.
order(x) # devuelve un índice, pero no devuelve el vector ordenado. 
str_rev(x) # Necesita library("miscset") le doy la vuelta a la cadena

'Me va a quitar cosas incomodas del texto //, ¨, <> '
gsub("in", "adios", vector) # busca los in en el vector y los reemplaza por adiós.

paste("a", "b", "c", sep = ";") # concatena
strsplit(vector, ' ') # separa
grepl('in', vector) # Devuelve si la subcadena existe o no dentro de la cadena

any(x %in% c(1, 3, 5)) # ¿Alguno de estos valores están en x?
all(x %in% c(1, 3, 5)) # ¿Estan todos estos valores en el vector?
which(c(48, 50, 1, 20, -9) %in% x) # busca valores en X y devuelve sus índices.
match(26, x) # Si existe el 26 en el vector te devuelve la posición, sino, NA

'Trabajar con fechas'
today <- Sys.Date()
now <- Sys.time()
d <- as.Date("2016-03-17")
d <- as.Date("17-03-2016", format = "%d-%m-%Y")
d + 1 # Añade un día

d2 <- as.Date("2015-03-17")
d - d2 # Diferencia en días


'Funciones muy útiles'
rep(NA, 10) #repite NA 10 veces en un vector de 10 posiciones
append(1:20, c(1, 2, 3))
seq(from = 5, to = 100, by = 5)
x <- c(15, 26, 5, 9, 1, -9)
sort(x)
order(x) #order me de las posiciones
str_rev(x) # Necesita library("miscset")

any(x %in% c(1, 3, 5)) #busca dentro de un vector.
all(x %in% c(1, 3, 5))
all(c(15, 9, 1) %in% x) #busca los elementos de un vector en una variables
which(c(48, 50, 1, 20, -9) %in% x) #cual de estos numeros esta en x me devuelve posicones
match(26, x) #me devuelve la posicion donde se encuentra


'Funciones matemáticas'
x <- seq(from = 5, to = 100, by = 5)

is.na(x) # vector de T o F - busca NA en mi vector
is.finite(x) # Vector de T
is.infinite(x) # Vector de F son numeros finitos me devuelve false
abs(x) 
sqrt(x)
log(x)
log10(x)
exp(x)
ceiling(log(x)) # Redondeo al más cercano.
floor(log(x)) # Redondeo hacia abajo.
round(log(x), digits = 2) # Redondeo con dos dígitos.
sin(x)
cos(x)
tan(x)
sum(x)
prod(x)
cumsum(x) #suma acumulada
cumprod(x) #producto acumulado



'Funciones sobre cadenas'
#---------------------------------------------------
s <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur."
nchar(s) # Número de caracteres.
toupper(s) # Todo mayúsculas.
tolower(s) # Todo minúsculas.
gsub("in", "adios", s) # Busca los in y los cambia por adios.
substr(s, 7, 11) # Devuelve una subcadena, inicio – fin.
substr(s, 7, 11) <- "IPSUM" # Modifica una subcadena con el elemento que le pasas
paste("a", "b", "c", sep = ";") #monta una estructura separada por ; de los valores introducidos
strsplit(s, ' ') # Separa la cadena por espacios en blanco.

'Devuelve un TRUE si encuentra dolor en mi cadena'
grepl('dolor', s) # Devuelve si la subcadena existe o no dentro de la cadena 

library(stringr) 
str_locate_all(pattern ='dolor', s) # Devuelve posiciones de la subcadena



'Funciones sobre fechas'
#---------------------------------------------------
today <- Sys.Date()
today
class(today) #me devuelve una DATE

now <- Sys.time() #me devuelve fecha y hora justo en el momento que ejecuto el código
now
class(now)

d <- as.Date("2016-03-17") #año - mes - dia
d <- as.Date("17-03-2016") # No da error pero lo hace mal, formate MAL la fecha  NO USAR
d
d <- as.Date("17-03-2016", format = "%d-%m-%Y") #Formatea de manera correcta la fecha
d
t <- as.POSIXct("2016-03-17 22:32:00")
t
t <- as.POSIXct("17-03-2016 22:32:00", format = "%d-%m-%Y %H:%M:%S")
t


'Cálculos con fechas'
#---------------------------------------------------
d + 1 # Añade un día

d2 <- as.Date("2015-03-17")
d - d2 #Diferencia en días.

t + 1 #Añade un segundo

t2 <- as.POSIXct("2015-03-17 22:32:00")
t - t2 #Diferencia en días.

unclass(d) # Da un entero: número de días desde el 1 Enero 1970.
unclass(t) # Da un entero: número de segundos desde el 1 Enero 1970.

getwd()


library(stringr)


#Importar ficheros con sus rutas
#-------------------------------------------

mun_csv_1 <- read.table("/Users/WadieAlbakri/Desktop/Máster BME/Módulos/Módulo 1/municipios1.csv", 
                        header = TRUE, 
                        sep = ",", 	# sep = "\t", #El separador es el tabulador
                        stringsAsFactors = FALSE, # No deseamos convertir el resultado en un factor (poner siempre esto).
                        dec = ",", #el separador decimal es la coma
                        quote = "") #Especificamos las cadenas de caracteres

mun_csv_2 <- read.csv("/Users/WadieAlbakri/Desktop/Máster BME/Módulos/Módulo 1/municipios1.csv", stringsAsFactors = FALSE)

'Fichero excel'
library(xlsx)
read.xlsx(paste(getwd(),"/warrants.xls", sep = "")
                        , sheetName="Warrants"
                        , startRow=6
                        , endRow=NULL
                        , as.data.frame=TRUE
                        , header=TRUE
                        , keepFormulas=FALSE
                        , encoding="UTF-8")

#Lectura de ficheros
#----------------------------------------

'Lectura de archivos'
vinos <- read.csv("wine.csv", header = TRUE)

'Análisis exploratorio'
class(vinos)
dim(vinos)
str(vinos)
summary(vinos)
names(vinos)
head(vinos)
tail(vinos)
plot(vinos$alcohol, vinos$proline) #plot sencillo
hist(vinos$alcohol) #histograma sencillo
print(vinos) #veo todas

'Preparacion'
install.packages("lubridate")
library(lubridate) # Trabajando con formatos de fecha.
ymd("2015-08-25")
ymd("2015 August 25") 
mdy("August 25, 2015")
hms("14:17:07")
ymd_hms("2015/08/25 13.33.09")

library(stringr) # Trabajando con string
str_trim("    this is a test    ") #quita espacios
str_pad("244493", width = 7, side = "left", pad = "0")#completa con 7 cifras y 0 a la izquierda

names <- c("Sarah", "Tom", "Alice") #creo un vector con nombres
str_detect(names, "Alice") #identifica los nombres Alice me devuelve un vector booleano
str_replace(names, "Alice", "David") #Pon Alice en lugar de David
names


#NAs
df <- data.frame(A = c(1, NA, 8), B = c(3, NA, 88), C = c(2, 45, 3))

'Detección de NAs'

any(is.na(df)) #Existe algún Na
is.na(df) #Da una matriz de T o F en los lugares del NA
sum(is.na(df)) #Devuelve el número exacto de NAs
summary(df)

'Eliminación de NAs'
na.omit(df) # elimina las filas con na. Es el comando más utilizado
df[complete.cases(df), ] # elimina las filas con na.

#Eliminar los NA en una pipe
hflights.tbl %>% 
  filter(!(is.na(ArrDelay))) %>% # Elimino los NA



'Outliers'
set.seed(10) # Inicializamos la semilla para generar valores aleatorios

x <- c(rnorm(30, mean = 15, sd = 5), -5, 28, 35) 
# Generamos 30 aleatorios con una media y una desviación y añade -5, 28, 35
x #He creado un vector con 33 posiciones las 30 primeras aleatorios.

boxplot(x, horizontal = T) # Lo graficamos para ver qué pinta tiene.
# Muestra una “caja” con el 1er y 3er cuartil, y la mediana
# Las mechas son 1,5 veces Q3 – Q1
# Los atípicos están fuera de las mechas - outliers

boxplot(x, horizontal = F) #así se grafican las velas de bolsa; ves 
#apertura, cierre, máximo y mínimo


df2 <- data.frame(A = rnorm(100, 50, 100), B = c(rnorm(99, 50, 10), 500), C = c(rnorm(99, 50, 10), -1))

hist(df2$B, 20) 
boxplot(df2)

