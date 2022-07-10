rm(list=ls())
library(HoRM)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(caret)
library(KernSmooth)
library(PLRModels)

#SEKCJA 1.1
#zadanie 1.
#zadajemy początkowego parametry, generujemy wektor x
n <- 500
sigma <- 0.5
x <- seq(1/n, 1, 1/n)

#Generujemy wektor Y i r(x)
r <- rep(0, n)
for (i in 1:n) {
  r[i] <- 10*sin(2*pi*x[i])
}
y <- r+rnorm(n, mean=0, sd=sigma)

#zadanie 2
regres <- function(x,y,k){
  # k = liczba przedzialów
  B = seq(0,1,length=k+1)
  WhichBin = findInterval(x,B)
  N = tabulate(WhichBin)
  m.hat = rep(0,k)
  for(j in 1:k){
    if(N[j]>0) m.hat[j] = mean(y[WhichBin == j])
  }
  return(list(bins=B,m.hat=m.hat))
}

#Zadanie nr 3
p1 <- regressogram(x,y,nbins=20)
regres_20 <- regres(x,y,20)
df_20 <- data.frame(x, y, r, rep(0,n))
colnames(df_20)[4] <- 'reg_punkty'
for (j in 1:n) {
  for (i in 1:20) {
    if (p1$data$z[j] == i) {
    df_20$reg_punkty[j] <- regres_20$m.hat[i]
    }
  }
}
ggplot(df_20, aes(x=x, y=reg_punkty)) +
  geom_point(color='blue') +
  geom_line(y=r, color='red', lwd=1) +
  geom_point(y=y, color='green', size=1) +
  labs(x='x', y='y') +
  ggtitle('Przybliżanie regressogramem')

#zadanie 4
#m=10
p2 <- regressogram(x,y,nbins=10)
regres_10 <- regres(x,y,10)
df_10 <- data.frame(x, y, r, rep(0,n))
colnames(df_10)[4] <- 'reg_punkty'
for (j in 1:n) {
  for (i in 1:k) {
    if (p2$data$z[j] == i) {
      df_10$reg_punkty[j] <- regres_10$m.hat[i]
    }
  }
}

ggplot(df_10, aes(x=x, y=reg_punkty)) +
  geom_point(color='blue') +
  geom_line(y=r, color='red') +
  geom_point(y=y, color='green') +
  labs(x='x', y='y') +
  ggtitle('Przybliżanie regressogramem')

#m=50
p3 <- regressogram(x,y,nbins=50)
regres_50 <- regres(x,y,50)
df_50 <- data.frame(x, y, r, rep(0,n))
colnames(df_50)[4] <- 'reg_punkty'
for (j in 1:n) {
  for (i in 1:k) {
    if (p3$data$z[j] == i) {
      df_50$reg_punkty[j] <- regres_50$m.hat[i]
    }
  }
}

ggplot(df_50, aes(x=x, y=reg_punkty)) +
  geom_point(color='blue') +
  geom_line(y=r, color='red') +
  geom_point(y=y, color='green') +
  labs(x='x', y='y') +
  ggtitle('Przybliżanie regressogramem')

#zadanie 5
local_avg <- function(x,y,h) {
  Y <- c()
  for (x_star in seq(0.01, 1, by=0.01)) {
    wt <- rep(0, n)
    window <- c(max(x_star - h,0), min(x_star + h,1))
    for (x_point in x) {
      if (x_point <= window[2] & x_point > window[1]) {
        idx <- which(x_point == x)
        wt[idx] <- 1
        #k <- k+1
        }
      }
    if (sum(wt) > 0) {
      y_star <- sum(y*wt)/sum(wt)
      Y <- append(Y, rep(y_star, sum(wt)))
    } else {
      y_star <- 0
    }
  }
  z <- seq(1/length(Y),1,by=1/length(Y))
  results <- data.frame(z, Y)
  return(results)
}
loc_avg_0.05 <- local_avg(x,y,1/20)
plot(local_avg(x,y,1/20))
plot(loc_avg_0.05, main='local averages, h=0.05', col='blue', xlab='x', ylab='y')
lines(x, 10*sin(2*pi*x), lwd=2, col='red')
points(x,y,pch=1, col='purple')
grid(col='black')
legend('bottomleft',
       legend=c("local averages estimator", "true curve", "data points"),
       col=c('blue', 'red', 'purple'),
       pch = 15
       )


#h = 1/10
loc_avg_0.1 <- local_avg(x,y,1/10)
plot(loc_avg_0.1, main='local averages, h=0.1', col='blue', xlab='x', ylab='y')
lines(x, 10*sin(2*pi*x), lwd=2, col='red')
points(x,y,pch=1, col='purple')
grid(col='black')
legend('bottomleft',
       legend=c("local averages estimator", "true curve", "data points"),
       col=c('blue', 'red', 'purple'),
       pch = 15
)



#h = 1/50
loc_avg_0.02 <- local_avg(x,y,1/50)
plot(loc_avg_0.02, main='local averages, h=0.02', col='blue', xlab='x', ylab='y')
lines(x, 10*sin(2*pi*x), lwd=2, col='red')
points(x,y,pch=1, col='purple')
grid(col='black')
legend('bottomleft',
       legend=c("local averages estimator", "true curve", "data points"),
       col=c('blue', 'red', 'purple'),
       pch = 15
)



#SEKCJA 1.2
#Zadanie 1
n_1 <- 1000
x_1 <- seq(1/n_1, 1, 1/n_1)
sigma_1 <- 1
r_1 <- c(0, n_1)
for (k in 1:n_1) {
  r_1[k] <- sqrt(x_1[k]*(1-x_1[k]))*sin(2.*pi/(x_1[k]+0.05))
}
y_1 <- r_1+rnorm(n_1, 0, sigma_1)

#Zadanie 2
#najpierw wybierzmy h przy pomocy metody leave-one-out cross-validation
gaussian_kernel <- function(x) {
  return(dnorm(x, 0, 1))
}


nw_estimator <- function(x, y, h) {
  Kij <- outer(x, x, function(x, x_i) gaussian_kernel((x-x_i)/h))
  S <- Kij / rowSums(Kij)
  return(S%*%y)
}

LOOCV <- function(h) {
  Kij <- outer(x_1, x_1, function(x, x_i) gaussian_kernel((x-x_i)/h))
  S <- Kij / rowSums(Kij)
  mean(((y_1-S%*%y_1) / (1-diag(S)))^2)
}

h <- seq(0.01, 1, by=0.01)
LOOCV_values <- sapply(h, LOOCV)
h_opt <- h[which.min(LOOCV_values)]
h_opt
np.cv(data=as.matrix.data.frame(y_1,x_1),
      h.seq=h,
      w=c(0,1),
      estimator='NW',
      kernel='gaussian'
      )

ggplot(data=data.frame(h, LOOCV_values), aes(x=h, y=LOOCV_values)) +
  geom_point(colour='blue') +
  geom_point(data=data.frame(h, LOOCV_values)[h == h_opt,],
             aes(x=h, y=LOOCV_values),
             colour = 'red'
             ) +
  geom_text(data=data.frame(h, LOOCV_values)[h == h_opt,],
            label="h = 0.02",
            aes(x=h*5, y=LOOCV_values),
            colour='red'
            ) +
  ggtitle('LOOCV - wybór parametru h')

#zadanie 3
#za wagi przyjmujemy jądro gaussowskie
loc_pol <- function(x,y,h) {
  return(locpoly(x,y, kernel='normal', bandwidth = h, degree=1,
                 range.x = c(min(x), max(x))))
}



LOOCV_fit <- np.cv(data=matrix(c(y_1, x_1), ncol=2, nrow=n_1),
                   h.seq=h,
                   w=c(0,1),
                   estimator='LLP',
                   kernel='gaussian'
                  )
h_opt_1 <- LOOCV_fit$h.opt[2,1]
df_1 <- data.frame(h, LOOCV_fit$CV)
colnames(df_1)[2] <- 'LOOCV_values'
ggplot(data=df_1, aes(x=h, y=LOOCV_values)) +
  geom_point(colour='blue') +
  geom_point(data=df_1[h == h_opt_1,],
             aes(x=h, y=LOOCV_values),
             colour = 'red'
  ) +
  geom_text(data=df_1[h == h_opt_1,],
            label="h = 0.02",
            aes(x=h*5, y=LOOCV_values),
            colour='red'
  ) +
  ggtitle('LOOCV - wybór parametru h')

#zadanie 4
df_2 <- data.frame(x_1, r_1, nw_estimator(x_1, y_1, h_opt))
df_3 <- data.frame(loc_pol(x_1, y_1, h_opt_1)$x,
                   loc_pol(x_1, y_1, h_opt_1)$y
                   )
colnames(df_2)[3] <- c('NW_estimator')
colnames(df_3)[1] <- 'x'
colnames(df_3)[2] <- 'local_polynomials_estimator'
head(df_3)
ggplot(data=df_2, aes(x=x_1, y=r_1)) +
  geom_line() +
  geom_line(aes(x= x_1, y=NW_estimator), colour='red', lwd=1) +
  geom_line(data=df_3, aes(x=x, y=local_polynomials_estimator), colour='blue', lwd=1) +
  ggtitle('Funkcja Dopplera oraz jej estymatory N-W i local polynomials') +
  labs(x='x', y='y')

#zadanie 5
#sigma = 0.5
sigma_2 <- 0.5
y_2 <- r_1+rnorm(n_1, 0, sigma_2)
LOOCV_fit_nw_0.5 <- np.cv(data=matrix(c(y_2, x_1), ncol=2, nrow=n_1),
      h.seq=h,
      w=c(0,1),
      estimator='NW',
      kernel='gaussian'
)
h_opt_2 <- LOOCV_fit_nw_0.5$h.opt[2,1]
df_3 <- data.frame(h, LOOCV_fit_nw_0.5$CV)
colnames(df_3)[2] <- 'LOOCV_values'
ggplot(data=df_3, aes(x=h, y=LOOCV_values)) +
  geom_point(colour='blue') +
  geom_point(data=df_3[h == h_opt_2,],
             aes(x=h, y=LOOCV_values),
             colour = 'red'
  ) +
  geom_text(data=df_3[h == h_opt_2,],
            label="h = 0.01",
            aes(x=h+0.07, y=LOOCV_values),
            colour='red'
  ) +
  ggtitle('LOOCV - wybór parametru h')

LOOCV_fit_llp_0.5 <- np.cv(data=matrix(c(y_2, x_1), ncol=2, nrow=n_1),
                   h.seq=h,
                   w=c(0,1),
                   estimator='LLP',
                   kernel='gaussian'
)
h_opt_3 <- LOOCV_fit_llp_0.5$h.opt[2,1]
df_4 <- data.frame(h, LOOCV_fit_llp_0.5$CV)
colnames(df_4)[2] <- 'LOOCV_values'
ggplot(data=df_4, aes(x=h, y=LOOCV_values)) +
  geom_point(colour='blue') +
  geom_point(data=df_4[h == h_opt_3,],
             aes(x=h, y=LOOCV_values),
             colour = 'red'
  ) +
  geom_text(data=df_4[h == h_opt_3,],
            label="h = 0.01",
            aes(x=h+0.1, y=LOOCV_values),
            colour='red'
  ) +
  ggtitle('LOOCV - wybór parametru h')

df_5 <- data.frame(x_1, r_1, nw_estimator(x_1, y_2, h_opt_2))
df_6 <- data.frame(loc_pol(x_1, y_2, h_opt_3)$x,
                   loc_pol(x_1, y_2, h_opt_3)$y
)
colnames(df_5)[3] <- c('NW_estimator')
colnames(df_6)[1] <- 'x'
colnames(df_6)[2] <- 'local_polynomials_estimator'
ggplot(data=df_5, aes(x=x_1, y=r_1)) +
  geom_line() +
  geom_line(aes(x= x_1, y=NW_estimator), colour='red', lwd=1) +
  geom_line(data=df_6, aes(x=x, y=local_polynomials_estimator), colour='blue', lwd=1) +
  ggtitle('Funkcja Dopplera oraz jej estymatory N-W i local polynomials') +
  labs(x='x', y='y')

#sigma = 0.1
sigma_3 <- 0.1
y_3 <- r_1+rnorm(n_1, 0, sigma_3)
LOOCV_fit_nw_0.1 <- np.cv(data=matrix(c(y_3,x_1), ncol=2, nrow=n_1),
      h.seq=h,
      w=c(0,1),
      estimator='NW',
      kernel='gaussian'
)

h_opt_4 <- LOOCV_fit_nw_0.1$h.opt[2,1]
df_6 <- data.frame(h, LOOCV_fit_nw_0.1$CV)
colnames(df_6)[2] <- 'LOOCV_values'
ggplot(data=df_6, aes(x=h, y=LOOCV_values)) +
  geom_point(colour='blue') +
  geom_point(data=df_6[h == h_opt_4,],
             aes(x=h, y=LOOCV_values),
             colour = 'red'
  ) +
  geom_text(data=df_6[h == h_opt_4,],
            label="h = 0.01",
            aes(x=h+0.07, y=LOOCV_values),
            colour='red'
  ) +
  ggtitle('LOOCV - wybór parametru h')

LOOCV_fit_llp_0.1 <- np.cv(data=matrix(c(y_3, x_1), ncol=2, nrow=n_1),
                           h.seq=h,
                           w=c(0,1),
                           estimator='LLP',
                           kernel='gaussian'
)

h_opt_5 <- LOOCV_fit_llp_0.1$h.opt[2,1]
df_7 <- data.frame(h, LOOCV_fit_llp_0.1$CV)
colnames(df_7)[2] <- 'LOOCV_values'
ggplot(data=df_7, aes(x=h, y=LOOCV_values)) +
  geom_point(colour='blue') +
  geom_point(data=df_7[h == h_opt_5,],
             aes(x=h, y=LOOCV_values),
             colour = 'red'
  ) +
  geom_text(data=df_7[h == h_opt_5,],
            label="h = 0.01",
            aes(x=h+0.1, y=LOOCV_values),
            colour='red'
  ) +
  ggtitle('LOOCV - wybór parametru h')
df_8 <- data.frame(x_1, r_1, nw_estimator(x_1, y_3, h_opt_4))
df_9 <- data.frame(loc_pol(x_1, y_3, h_opt_5)$x,
                   loc_pol(x_1, y_3, h_opt_5)$y
)
colnames(df_8)[3] <- c('NW_estimator')
colnames(df_9)[1] <- 'x'
colnames(df_9)[2] <- 'local_polynomials_estimator'
ggplot(data=df_8, aes(x=x_1, y=r_1)) +
  geom_line() +
  geom_line(aes(x= x_1, y=NW_estimator), colour='red', lwd=1) +
  geom_line(data=df_9, aes(x=x, y=local_polynomials_estimator), colour='blue', lwd=1) +
  ggtitle('Funkcja Dopplera oraz jej estymatory N-W i local polynomials') +
  labs(x='x', y='y')

