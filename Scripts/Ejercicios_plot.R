
#4 gráficos en 2 filas y dos columnas.
attach(mtcars) # Cargamos la base de datos.
par(mfrow=c(2,2)) # Dividimos en 2 filas y 2 columnas.
plot(wt,mpg, main="Scatterplot of wt vs. mpg")
plot(wt,disp, main="Scatterplot of wt vs disp")
hist(wt, main="Histogram of wt")
boxplot(wt, main="Boxplot of wt")



#3 filas 1 columna
attach(mtcars)
par(mfrow=c(3,1))
hist(wt)
hist(mpg)
hist(disp)

attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
hist(wt)
hist(mpg)
hist(disp)

# layout(matrix(c(1,1,2,3) le estamos indicando que 
#el primer gráfico ocupa las dos primeras posiciones de la matriz 
#(la fila de arriba), el segundo gráfico está en la 3ª posición de la matriz 
#(2ª fila 1ª columna) y el tercer gráfico en el hueco restante (2ª fila 2ª columna).



#Añadir diagramas de caja a un gráfico de dispersión

# Creamos el gráfico de dispersión.
par(fig=c(0,0.8,0,0.8), new=TRUE)
plot(mtcars$wt, mtcars$mpg, xlab="Car Weight",
     ylab="Miles Per Gallon")

# Añadimos el boxplot de arriba.
par(fig=c(0,0.8,0.55,1), new=TRUE)
boxplot(mtcars$wt, horizontal=TRUE, axes=FALSE)

# Añadimos el boxplot de la derecha.
par(fig=c(0.65,1,0,0.8),new=TRUE)
boxplot(mtcars$mpg, axes=FALSE)

# Añadimos el título.
mtext("Enhanced Scatterplot", side=3, outer=TRUE, line=-3)




#EJERCICIOS.
#------------------------------------------
# Graficamos Acciona con una línea azul, delimitando el eje Y entre 0 y 12

# Definimos un vector con 5 valores.
Acciona <- c(1,3,6,4,9)
posiciones <- c(1,2,3,4,5)
datos <- data.frame(posiciones,Acciona)

# Graficamos el vector con todas las opciones en default.
ggplot(datos, aes(x=posiciones, y=Acciona )) + 
  geom_line(colour="red")  + 
  geom_point( size=2, shape=21, fill="white", colour="red") + 
  theme_minimal()



# Graficamos Telefonica con una línea de puntos roja y la añadimos al gráfico anterior.

Acciona <- c(1, 3, 6, 4, 9)
Telefonica <- c(2, 5, 4, 5, 12)
posiciones <- c(1,2,3,4,5)


# Create a first line
plot(posiciones, Acciona, type = "o",ylim = c(0,12), frame = FALSE, pch = 19, 
     col = "blue", xlab = "x", ylab = "y")
# Add a second line
lines(posiciones, Telefonica, pch = 18, col = "red", type = "b", lty = 2)
# Add a legend to the plot
legend("topleft", legend=c("Acciona", "Telefonica"),
       col=c("blue", "red"), lty = 1:2, cex=0.8)



# Ponemos el título en rojo y tamaño 4.



