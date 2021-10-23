dadoTrucoNum=sample(c(1:6), 1000, replace = TRUE, prob = c(2,3,1,9,8,5))

table(dadoTrucoNum) 


library(utils)

n_escenarios <- 10000
barra_progreso <- winProgressBar(title= "Barra de progreso", min = 0, max = n_escenarios, width=300)
# width es el nº de pixeles de la barra.

for (escenario in 1:n_escenarios){
  setWinProgressBar(barra_progreso, escenario, title=paste(round(escenario/n_escenarios*100,0), "% realizado"))
}
close(barra_progreso)



library(foreach)
# Gives us the progress bar object.
library(utils)
# Some number of iterations to process.
n <- 10000
# Create the progress bar.
pb <- txtProgressBar(min = 1, max = n, style=3)
# The foreach loop we are monitoring. This foreach loop will log2 all 
# the values from 1 to n and then sum the result. 
k <- foreach(i = icount(n), .final=sum, .combine=c) %do% {
  setTxtProgressBar(pb, i)
  log2(i)
}
# Close the progress bar.
close(pb)