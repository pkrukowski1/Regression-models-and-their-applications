rm(list=ls())
library(car)
library(ggplot2)
library(dplyr)

help('read.table')
#Wczytujemy i przygotowujemy dane do analizy
setwd('C:/Users/Lenovo/Desktop/Modele regresji i ich zastosowania')
getwd()

dane <- read.table('lab1.txt', skip = 1,col.names = c('x','y'))
dane <- data.frame(dane)
head(dane)

dane[,1] <- as.numeric(gsub(",", ".", gsub("\\.","", dane[,1])))
dane[,2] <- as.numeric(gsub(",", ".", gsub("\\.","", dane[,2])))
typeof(dane)
dim(dane)
#Zadanie 1
summary(dane)
hist(dane[,1], main='Histogram dla x', xlab='x', ylab='Częstość')
hist(dane[,2], main='Histogram dla y', xlab='y', ylab='Częstość')

boxplot(dane, main='Wykres pudelkowy')
#Zadanie 2


dane %>%
  ggplot(aes(x = dane[,1], y = dane[,2])) +
  geom_point(colour = "red") +
  ggtitle('Wykres rozproszenia') +
  labs(x='x', y='y')

wsp <- cor(dane$x, dane$y)

#Zadanie 3

model <- lm(y~x, data=dane) #wspolczynniki są liczone przy pomocy OLS
model$coefficients

#Zadanie 4
anova(model)
sigma_hat_square <- anova(model)[2,3]
sigma_hat_square
#alternatywnie
summary(model)$sigma^2

#Zadanie 5

summary(model) #odrzucamy hipoteze zerową na poziomie istotności 0.05
#Odpowiedź: Tak, gdyby wyszlo, że wspolczynnik b_1 = 0, to regresja liniowa bylaby stalą, co byloby
#bez sensu

#Zadanie 6

help('confint')

confint(model,parm='x', level=0.99)

#Zadanie 7

predict(model, data.frame(x = 1), interval = 'confidence', level=0.99)
#Narysujmy ten przedzial ufności

#Zadanie 8

hist(model$residuals, main='Histogram dla rezyduów', xlab='Rezyduum', ylab='Częstość')

df <- as.data.frame(cbind(dane$x, dane$y, model$residuals, model$fitted.values))

qplot(sample=V3, data=df, col='red') + #Wykres kwantylowy
  labs(x='x',y='Rezyduum') +
  ggtitle('Wykres kwantylowy')

df %>%
  ggplot(aes(x = df[,4], y = df[,3])) +
  geom_point(colour = "red") +
  ggtitle('Wykres rozproszenia') +
  labs(x='y_hat', y='e_n')

df %>%
  ggplot(aes(x = df[,1], y = df[,3])) +
  geom_point(colour = "red") +
  ggtitle('Wykres rozproszenia') +
  labs(x='x', y='e_n')

#Zadanie 9
dane_modyfikacja <- dane
dane_modyfikacja[100,2] <- 1480.640

dane_new <- dane_modyfikacja[-100,]

model_new <- lm(y~x, data=dane_new)

model_modyfikacja <- lm(y~x, data=dane_modyfikacja)
model_modyfikacja$coefficients

beta_0_hat_mod <- model_modyfikacja$coefficients[1]
beta_1_hat_mod <- model_modyfikacja$coefficients[2]


abs(beta_0_hat-beta_0_hat_mod) #Duża różnica
abs(beta_1_hat-beta_1_hat_mod) #Duża różnica

#jest wplywowa, ale jest odstająca, zweryfikujmy to, używając hat matrix

H <- hatvalues(model_modyfikacja)
H[100]


dane_modyfikacja %>%
  ggplot(aes(y = y, x = x)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Prosta regresji dla dane_modyfikacja") +
  geom_text(label=rownames(dane_modyfikacja), 
            nudge_x = 0.5, 
            nudge_y = 0.25, 
            check_overlap = T)


dane_new %>%
  ggplot(aes(y = y, x = x)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Prosta regresji dla dane_new") +
  geom_text(label=rownames(dane_new), 
            nudge_x = 0.5, 
            nudge_y = 0.25, 
            check_overlap = T)

model_modyfikacja$residuals[100]
model$residuals[100]


dane %>%
  ggplot(aes(y = y, x = x)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Prosta regresji dla pierwotnych danych") +
  geom_text(label=rownames(dane), 
            nudge_x = 0.5, 
            nudge_y = 0.25, 
            check_overlap = T)


#Zadanie 10

#a)

x <- runif(100,0,1)
epsilon <- rnorm(100,0,0.1)
beta_0 <- 1
beta_1 <- 2

y <- c()

for (k in c(1:100)) {
  y[k] <- beta_0 +beta_1*x[k] + epsilon[k]  
}

#b)
dane_1 <- as.data.frame(cbind(x,y))

dane_1 %>%
  ggplot(aes(x = dane_1[,1], y = dane_1[,2])) +
  geom_point(colour = "red") +
  ggtitle('Wykres rozproszenia dla nowych danych dla sigma=0.1') +
  labs(x='x', y='y')

#c)
model_1 <- lm(y~x, data=dane_1)

beta_0_hat <- model_1$coefficients[1]
beta_1_hat <- model_1$coefficients[2]

abs(beta_0-beta_0_hat)
abs(beta_1-beta_1_hat)

R_kwadrat <- summary(model)$r.squared

#d)
###############
#sigma = 0.5
epsilon_2 <- rnorm(100,0,0.5)
y_2 <- c()

for (k in c(1:100)) {
  y_2[k] <- beta_0 +beta_1*x[k] + epsilon_2[k]  
}

dane_2 <- as.data.frame(cbind(x,y_2))

dane_2 %>%
  ggplot(aes(x = dane_2[,1], y = dane_2[,2])) +
  geom_point(colour = "red") +
  ggtitle('Wykres rozproszenia dla sigma=0.5') +
  labs(x='x', y='y')

model_2 <- lm(y_2~x, data=dane_2)

beta_0_hat_2 <- model_2$coefficients[1]
beta_1_hat_2 <- model_2$coefficients[2]

abs(beta_0-beta_0_hat_2)
abs(beta_1-beta_1_hat_2)

R_kwadrat_2 <- summary(model_2)$r.squared

###################
#sigma=1


epsilon_3 <- rnorm(100,0,1)
y_3 <- c()

for (k in c(1:100)) {
  y_3[k] <- beta_0 +beta_1*x[k] + epsilon_3[k]  
}

dane_3 <- as.data.frame(cbind(x,y_3))

dane_3 %>%
  ggplot(aes(x = dane_3[,1], y = dane_3[,2])) +
  geom_point(colour = "red") +
  ggtitle('Wykres rozproszenia dla sigma=1') +
  labs(x='x', y='y')

model_3 <- lm(y_3~x, data=dane_3)

beta_0_hat_3 <- model_3$coefficients[1]
beta_1_hat_3 <- model_3$coefficients[2]

abs(beta_0-beta_0_hat_3)
abs(beta_1-beta_1_hat_3)

R_kwadrat_3 <- summary(model_3)$r.squared
R_kwadrat_3
