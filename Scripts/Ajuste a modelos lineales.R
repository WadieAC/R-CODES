#MODELOS LINEALES. method = lm
#-----------------------------------------------
data(cars)
data(mpg)
resumen_data(mpg)

myGG <- ggplot(mpg, aes(x= displ, y=hwy))
myGG
myGG + geom_point(aes(color = manufacturer)) + stat_smooth( method = lm, se = FALSE)

myGG + geom_point(aes(color = manufacturer)) + stat_smooth(method = lm)

# Gráfico 2.
#-----------------------------------------
ggplot(cars, aes(x=speed, y=dist)) + 
  geom_point(shape = 1) +  #usa circulos huecos
  geom_smooth(method=lm)   #añade recta de regresión con intervalo confianza 95%


# Gráfico 3.
#-----------------------------------------
ggplot(cars, aes(x = speed, y=dist)) + 
  geom_point(shape = 1) + 
  geom_smooth(method=lm, se=FALSE) #añade recta peros sin intervalo de confianza 95%


# Gráfico 4.
#------------------------------------------
ggplot(cars, aes(x=speed, y=dist)) + 
  geom_point(shape=1) +
  geom_smooth() #añade una curva suavizada sobre los datos y su región de confianza



resumen_data(cars)

sp <- ggplot(cars, aes(x=speed, y=dist))
sp + geom_point()

#Ahora a este gráfico le añadimos una tendencia lineal
sp + geom_point() + stat_smooth(method = lm)

'modificación del intervalo de confianza'
sp + geom_point() + stat_smooth(method = lm, level= 0.99)

'curva suavizada buscando una tendencia'
sp + geom_point() + stat_smooth(method = loess, se = FALSE)


#se pueden hacer gráficos aún más sofisticados:
bin + stat_binhex() + scale_fill_gradient(low = "lightblue", high = "red", breaks = c(0,500, 1000, 2000, 4000, 6000, 8000), limits = c(0, 8000)) + stat_smooth(method = lm) + 
  ylim(0, 20000)



#Funciones qplot
#-----------------------------------------------------------------------

#representa en una nube de puntos los valores, dspl y hwy
#la variable marca o manufactures es la que ira en el color
qplot(data = mpg, x= displ, y=hwy, color= manufacturer)

#mismo gráfico pero ahora para ver el tipo o clase de coche
qplot(data = mpg, x= displ, y=hwy, color= class)

#FACETING. 
#===========================================================================

'consiste en agrupar el DataFrame según factores y pintar scatterplots
de variables en función de esos agrupamientos'

qplot(data = mpg, x=displ, y=hwy, color=manufacturer, facets = ~class)

#regresión múltiple más faceting

#linealización de un modelo 4 variables, en un solo gráfico
'facet_grid es la class me va a generar varios gráficos, uno por clase de coche
color manufacturer es que de cada manufactura me genera un color'
myGG + geom_point(aes(color = manufacturer)) + stat_smooth(method = lm, se = FALSE) +
      facet_grid(class ~ .) 

'analizamos ahora las cuentas con el % que dejan de propina'
sp <- ggplot(tips, aes(x=total_bill, y=tip/total_bill)) + geom_point(shape=1)
sp

'con mi representación gráfica fijadas mis 2 variables añado parámetros'
#Con faceting parto la gráfica en función del sexo.
sp + facet_grid(sex ~ .) #me genera 2 gráficos 1 por sexo en horizontal

sp + facet_grid(. ~ sex) #si lo pongo a la izquierda el ~ de la var lo hace en vertical


'ahora puedo dividir en horizontal por sex y por day en horizontal'
sp + facet_grid(sex ~ day) 


'divido por dia en horizontal pero con solo 2 columnas, es decir 2 dias y siguiente linea'
sp + facet_wrap(~ day, ncol=2)


#FACETING + HISTOGRAMAS
#--------------------------------------------------------------------

#A histogram of bill sizes
'Voy a obtener un conteo dividido por sexo y fumador del total de la cuenta pagada'
hp <- ggplot(tips, aes(x= total_bill)) + geom_histogram(binwidth = 2, colour="white")

hp + facet_grid(sex ~ smoker) 

'puedo reescalar el tramo de arriba y hacer una escala individualizada'
hp + facet_grid(sex ~ smoker, scales = "free_y")

'adapto a la reescalación también el rango'
hp + facet_grid(sex ~ smoker, scales="free", space="free")
