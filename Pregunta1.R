#Solucion Pregunta 1

x <- c(0,1)
fx <- c(0.68, 0.32)

cbind(x, fx)

plot(x, fx, ylim=c(0,1), pch=16,)
lines(x, fx, type=h)

mu <- sum(x*fx) ## valor esperat (media)
mu

sigmassq <- sum((x-mu)^2*fx) ## varianza
sigmassq

n <- 43
sum(sample(x, n, prob=fx, replace = TRUE))
Y <- function(i)sum(sample(x, n, prob=fx, replace = TRUE))

m <- 400000
encuestas <- sapply(1:m, Y)

fi<- table(encuestas)/m
Fi <- cumsum(fi)
cbind(2:29, fi, Fi)

dbinom(13, 43, 0.32)  ### calcula la probabilitat de que una enquesta feta a 43 llars, hi haguin 13 que tinguin dos televisors, sen la probabilitat de tindre dos televisors de 0.32

resultados <- 0:43
fy <- dbinom(resultados, 43, 0.32)


pbinom(16, 44, 0.32) ### calcula la probabilitat enquesta 44 de que menys de 17 tinguin com a minim dos teles.

reusltados <- 0:24
fy<- dbinom(resultados, 24, 0.68)
mu <- sum(resultados*fy)
mu

sum((resultados-mu)^2*fy) ### variancia

qbinom(0.25, 24, 0.68) ## primer quartil
