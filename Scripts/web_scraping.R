install.packages("rvest")
install.packages("xml2")
library(xml2)
library(rvest)

datos_pelicula <- read_html ("https://es.wikipedia.org/wiki/El_lobo_de_Wall_Street")

# Obtenemos la ficha técnica (la columna de la derecha con los datos de dirección, producción, etc)
ficha_tecnica <- datos_pelicula %>%
  html_nodes("table") %>% 
  .[1] %>% # Sabemos lo que queremos extraer gracias a selectorgadget
  html_table()
ficha_tecnica<-as.data.frame(ficha_tecnica)
ficha_tecnica<-ficha_tecnica[2:dim(ficha_tecnica)[1],1:2]


# Obtenemos las referencias
referencias<- datos_pelicula %>%
  html_nodes("span.reference-text") %>% # Sabemos lo que queremos extraer gracias a selectorgadget
  html_text() 
referencias<-as.data.frame(referencias)


# Obtenemos el contenido de la página con el argumento
argumento<- datos_pelicula %>%
  html_nodes("p") %>% # Sabemos lo que queremos extraer gracias a selectorgadget
  html_text() 
argumento<-as.data.frame(argumento)
argumento<-as.data.frame(argumento[3:(dim(argumento)[1]-1),1])

#1. EJERCICIO.----------------------------------------------------------

CNMV <- read_html("http://www.cnmv.es/portal/HR/HRAldia.aspx")
datos_relevantes<- CNMV %>%
  html_nodes("li#elementoPrimerNivel") %>% # Sabemos lo que queremos extraer gracias a selectorgadget
  html_text() 
referencias<-as.data.frame(datos_relevantes)


#2. EJERCICIO.----------------------------------------------------------

expansion <- read_html("https://www.expansion.com/economia.html?intcmp=MENUHOM24101&s_kw=economia")
noticias_economia_exp<- expansion %>%
  html_nodes("h2") %>% # Sabemos lo que queremos extraer gracias a selectorgadget
  html_text() 
noticias_economia<-as.data.frame(noticias_economia_exp)

#3. EJERCICIO.----------------------------------------------------------

#Con este nodo tendria que limpiar mi data frame quitando tabulaciones y montando la tapa
investing <- read_html("https://es.investing.com/economic-calendar/")
agenda_macro<- investing %>%
  html_nodes("tr") %>% # Sabemos lo que queremos extraer gracias a selectorgadget
  html_text()
agenda_macro_invest<-as.data.frame(agenda_macro)

dim(agenda_macro_invest)

prueba <- as.character(agenda_macro_invest[6:76,])
prueba


#4. EJERCICIO.----------------------------------------------------------

investing_comm <- read_html("https://es.investing.com/commodities/")
materias_primas<- investing_comm %>%
  html_nodes("table") %>% 
  .[1] %>% # Sabemos lo que queremos extraer gracias a selectorgadget
  html_table()
materias<-as.data.frame(materias_primas)

materias <- materias[,2:dim(materias)[2]]


#5. EJERCICIO.----------------------------------------------------------

bolsa <- read_html("http://www.bolsamadrid.es/esp/aspx/Mercados/Precios.aspx?indice=ESI100000000")
bolsa_bme <- bolsa %>%
  html_nodes("table") %>% 
  .[5] %>% # Sabemos lo que queremos extraer gracias a selectorgadget
  html_table()
bolsa_bme<-as.data.frame(bolsa_bme)


#6. EJERCICIO.----------------------------------------------------------

expansion <- read_html("https://www.expansion.com/mercados/cotizaciones/valores/telefonica_M.TEF.html")
ultimas_noticias<- expansion %>%
  html_nodes("h3") %>% # Sabemos lo que queremos extraer gracias a selectorgadget
  html_text() 
ultimas_noticias<-as.data.frame(ultimas_noticias)

ultimas_noticias <- as.data.frame(ultimas_noticias[10:17,])
colnames(ultimas_noticias)[1] <- "Ultimas Noticias"


