####Enrique Franco Garcia - 259366####

library(deSolve) #Cargo la librería

####SIR####
SIR <- function(t, state, parameters) { #Creo una nueva funcion
  with(as.list(c(state, parameters)), { #Defino las ecuaciones
    dS <- -beta*S*I/(S+I+R)+epsilon*S -lambda*S
    dI <- beta*S*I/(S+I+R) -gama*I -lambda*I
    dR <- gama*I -lambda*R
    list(c(dS, dI, dR)) #Hago una lista con todas las ecuaciones
  })
}
#lambda representa la mortalidad y epsilon la natalidad 

pars <- c(beta = 4, gama = 2, epsilon = 5, lambda = 1) #Agrego los parametros 
condiciones_iniciales <- c(S = 950, I = 50, R = 0) #Agrego las condiciones iniciales
tiempo <- seq(0, 20, by = 0.001) #El tiempo que correra la funcion
out <- ode(condiciones_iniciales, tiempo, SIR, pars) 

## Grafica ##
matplot(out[ , 1], out[ , 2:4], type = "l", xlab = "tiempo", ylab = "Población",
        main = "SIR", lwd = 2) #Creo la grafica
legend("topright", c("Susceptible", "Infectado","Recuperado"), col = 1:3,lty=1:3,cex=0.5)
#Agrego un recuadro en el cual se especifica a que pertenece cada linea

####SEIR####
SEIR <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), { 
    # Acá van definidas las ecuaciones
    # Vamos a suponer dependencia de la frecuencia
    dS <- -beta*S*I/(S+E+I+R)+epsilon*S -lambda*S
    dE <- beta*S*I/(S+E+I+R) -delta*E -lambda*E
    dI <- delta*E/(S+E+I+R) -gama*I -lambda*I 
    dR <- gama*I -lambda*R 
    list(c(dS, dE, dI, dR))
  })
}

pars1 <- c(beta = 15, delta = 5 ,gama = 10, epsilon = 30, lambda = 10)  
condiciones_iniciales1 <- c(S = 950, E = 5, I = 45, R = 0)
tiempo1 <- seq(0, 20, by = 0.001)
out1 <- ode(condiciones_iniciales1, tiempo, SEIR, pars1) 

## Grafica ##
matplot(out1[ , 1], out1[ , 2:4], type = "l", xlab = "tiempo", ylab = "Población",
        main = "SEIR", lwd = 2)
legend("topright", c("Susceptible", "Expuesto" ,"Infectado", "Recuperado"), col = 1:3,lty=1:3,cex=0.5)

####SEIRS####
SEIRS <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), { 
    # Acá van definidas las ecuaciones
    # Vamos a suponer dependencia de la frecuencia
    dS <- -beta*S*I/(S+E+I+R)+omega*R +epsilon*S -lambda*S
    dE <- beta*S*I/(S+E+I+R) -delta*E -lambda*E
    dI <- delta*E/(S+E+I+R) -gama*I -lambda*I
    dR <- gama*I -lambda*R -omega*R
    list(c(dS, dE, dI, dR))
  })
}

pars2 <- c(beta = 10, delta = 2 ,gama = 5, omega = 7 ,epsilon = 5, lambda = 1.5)  
condiciones_iniciales2 <- c(S = 950, E = 10, I = 90, R = 0)
tiempo2 <- seq(0, 20, by = 0.001)
out2 <- ode(condiciones_iniciales2, tiempo2, SEIRS, pars2) 

## Grafica ##
matplot(out2[ , 1], out2[ , 2:5], type = "l", xlab = "tiempo", ylab = "Población",
        main = "SEIRS", lwd = 2)
legend("topright", c("Susceptible", "Expuesto" ,"Infectado", "Recuperado"), col = 1:4,lty=1:4,cex=0.5)
