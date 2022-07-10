rm(list=ls())
library(numDeriv)
library(pracma)
library(ggplot2)
library(MASS)
library(calculus)
library(matrixcalc)
library(nlme)

help('ginv')
x <- seq(0.1, 1000, by=0.1)
n <- length(x)
n
beta_1 <- 80
beta_2 <- 100
beta_3 <- 0.005
war <- 0.5


#zadanie 1
eps <- rnorm(n, 0, sqrt(war))
y <- as.numeric(c(rep(0, n))) #inicjalizacja zerami
for (i in 1:n) {
  y[i] <- beta_1+beta_2*exp(-beta_3*x[i])+eps[i]
}


#Zadanie 2
g_funkcja <- function(beta, x) {
  beta <- as.vector(beta)
  if (length(beta) > 3) {
    print('Zły wymiar bety!')
  }
  g <- rep(0,length(x))
  for (i in 1:length(x)) {
    g[i] = beta[1]+beta[2]*exp(-beta[3]*x[i])
  }
  return(g)
}

g_gradient <- function(i, beta, x) {
  beta <- as.vector(beta)
  if (length(beta) > 3) {
    print('Zły wymiar bety!')
  }
  g <- rep(0, length(x))
  g <- c(1, exp(-beta[3]*x[i]), -x[i]*beta[2]*exp(-beta[3]*x[i]))
  return(g)
}

as.vector(matrix(c(1,2,3),nrow=3,ncol=1))

#Zadanie 3

gauss_newton <- function(x, beta_0) {
  max_krok <- 100
  n <- length(x)
  beta_0 <- matrix(beta_0, nrow=3, ncol=1)
  G_macierz <- function(beta,x) {
    G_macierz_0 <- matrix(c(0, 3*n), nrow=n, ncol=3)
  for (k in 1:n) {
    G_macierz_0[k,] <- g_gradient(k, beta, x)
  }
    return(G_macierz_0)
  }
  res_0 <- y-g_funkcja(beta_0, x)
  k <- 1
  wyniki <- matrix(rep(0, 3*max_krok), nrow=max_krok, ncol=3)
  wyniki[k,] <- beta_0
  repeat {
    if (isTRUE(det(t(G_macierz(beta_0, x)) %*% G_macierz(beta_0, x)) > 0) == FALSE ) {
      print('Algorytm rozbieżny!')
      break
    }
    beta_next <- beta_0 + inv(t(G_macierz(beta_0, x)) %*% G_macierz(beta_0, x)) %*%
      t(G_macierz(beta_0, x)) %*% res_0
    k <- k+1
    if (k >= max_krok || Norm(beta_next-beta_0) <= 0.0001) break;
    beta_0 <- beta_next
    res_0 <- y-g_funkcja(beta_0, x)
    wyniki[k,] <- beta_next
    
  }
  wyniki <- as.data.frame(cbind(seq(0,99, by=1), wyniki))
  colnames(wyniki)[1] <- 'Iteracja'
  colnames(wyniki)[2] <- 'beta_1'
  colnames(wyniki)[3] <- 'beta_2'
  colnames(wyniki)[4] <- 'beta_3'
  return(wyniki)
}

GN <- gauss_newton(x, c(79,101,0.004))
c(as.matrix(GN[4,2:4]))
gauss_newton(x, c(79,101,0.004))
#implementujemy estymator wariancji, p=3
wariancja_hat <- function(beta, x) {
  beta <- as.vector(beta)
  return(sum((y-g_funkcja(beta, x))^2)/(n-3))
}


#estymator wariancji
wariancja_hat(c(as.matrix(GN[4,2:4])), x)
GN[3,2:4]

#Zadanie 4

loglikelihood <- function(beta,wariancja){
  v <- -n*log(2*pi)/2 -n/2 *log(wariancja) -1/(2*wariancja)*sum((y-g_funkcja(beta))^2)
  return(v)
}
war_hat_0 <- 34946.17/(n-3)
loglikelihood(c(79,101,0.004),war_hat_0)

war_hat_1 <- 5512.913/(n-3)
loglikelihood(c(80.024, 98.678, 0.0048), war_hat_1)

war_hat_2 <-  4901.962/(n-3)
loglikelihood(c(79.999, 99.881,0.005), war_hat_2)

loglikelihood(c(79.998, 99.936, 0.005), war_hat)
war_hat

#Zadanie 5 - w TeXu

#Zadanie 6
beta_hat <- c(as.matrix(GN[4,2:4]))
beta <- c(beta_1, beta_2, beta_3)
x1 <- x
x2 <- x
df_1 <- data.frame(x=x1,y=y, type='y obserwowane')
df_2 <- data.frame(x=x2, y=g_funkcja(beta_hat, x), type='y szacowane')
df <- rbind(df_1, df_2)

ggplot(df) +
  geom_point(aes(x,y,colour=type)) +
  ggtitle('Wykres rozproszenia')


#Zadanie 7
summary(model)
macierz_kowariancji <- summary(model)$cov
macierz_kowariancji

#Zadanie 8

gauss_newton(x, c(73,110, 0.01))[1:5,]
gauss_newton(x, c(50,120,0.4))[1:5,]

help('gradient')
f <- function(beta) {
  return(norm(y-g_funkcja(beta,x), type='2')^2)
}

#gradient powyższej formy kwadratowej w punkcie zwróconym w (b) jako
#ostatnia wartosc beta
f_gradient <- calculus::gradient(f, var=c(99, -1.5, -0.7))
f_gradient

beta_w <- c(99.08, -1.55,-0.651)
