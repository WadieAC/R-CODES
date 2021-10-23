profvis({
  
  tiempo <- proc.time() # Inicia el cronómetro
  
  # Establecemos una semilla de generación aleatoria determinada para que los rdos sean siempre iguales.
  set.seed(1000)
  
  # Hacemos una función que saque una bola y compruebe que no ha salido ya en la combinación actual.
  sacar_bola <- function(combi,nbola){
    bola <-sort(sample(seq(1,50,1), 1, replace = TRUE))
    
    # Comprobamos que este número no ha salido ya en la combinación actual
    for (comprobar_bola in 1:nbola){
      if (bola == combi[comprobar_bola]){
        bola <-sort(sample(seq(1,50,1), 1, replace = TRUE)) # Si la bola ya ha salido sacamos otra.
      }
    }
    return(bola)
  }
  
  # Sacamos la combinación ganadora
  combi_ganadora<- matrix(0,nrow=1,ncol=5,byrow=T) # Creamos una matriz para la combi ganadora.
  for (nbola in 1:5){
    combi_ganadora[nbola]<- sacar_bola(combi_ganadora,nbola)
  }
  
  # Sacamos las combinaciones apostadas y comprobamos cuantos aciertos tenemos en cada una de ellas.
  combinaciones <- 50000
  apuestas<- matrix(0,nrow=combinaciones,ncol=5,byrow=T) # Creamos una matriz para las apuestas.
  aciertos<-matrix(0,nrow=combinaciones,ncol=1,byrow=T) # Creamos un vector para los aciertos.
  barra_progreso <- winProgressBar(title= "Barra de progreso", min = 0, max = combinaciones, width=300) 
  # width es el nº de pixeles de la barra.
  
  for (combinacion in 1:combinaciones){
    
    # Obtenemos las apuestas realizadas (sacamos una combinación)
    combi<- matrix(0,nrow=1,ncol=5,byrow=T) # Creamos una matriz donde guardaremos la combi apostada 
    for (nbola in 1:5){
      combi[nbola]<- sacar_bola(combi,nbola)
    }
    apuestas[combinacion,]<-combi 
    
    # Comprobamos los aciertos que tenemos entre nuestras apuestas y la combinación ganadora.
    for (bola_apostada in 1:5){
      for (bola_premiada in 1:5){ # comparamos cada bola_apostada con cada bola_premiada
        if(apuestas[combinacion,bola_apostada]==combi_ganadora[bola_premiada]){
          aciertos[combinacion]<- aciertos[combinacion]+1
        }
      }
    }
    setWinProgressBar(barra_progreso, combinacion, title=paste(round(combinacion/combinaciones*100,0), "% realizado"))
  }
  close(barra_progreso)
  
  # Calculamos la frecuencia de los aciertos (cuantas veces hemos acertado 1 nº, cuantas veces 2 etc)
  library(plyr) 
  aciertos<-count(aciertos)
  aciertos
  print(proc.time()-tiempo) # Detiene el cronómetro (elapsed es el tiempo que ha tardado el programa)
})
