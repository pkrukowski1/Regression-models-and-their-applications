rm(list=ls())
library(xlsx)
library(ggplot2)
library(car)
library(GGally)
library(reshape2)
library(olsrr)
library(MASS)
library(ggpubr)

#Wczytujemy dane
dane <- read.xlsx2('C:/Users/Lenovo/Desktop/Modele regresji i ich zastosowania/regresja wielokrotna.xlsx',
                   sheetName = 1)

#Konwertujemy list do dobule
dane <- apply(dane, MARGIN=2, function(x) return(as.numeric(x)))
apply(dane, 2, function(x) any(is.na(x)))

#Zadanie 1
#Tworzymy wykresy rozrzutu
ggpairs(as.data.frame(dane), title = 'Macierz wykresów rozrzutu', axisLabels = "none")
help('ggpairs')

#Zadanie 2
ggcorr(dane, method = c("everything", "pearson"), label =T) +
  ggtitle('Heatmap')

#Zadanie 3
model <- lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data=as.data.frame(dane))
#a)
beta_hat <- model$coefficients
#b)
summary(model)
#Model ma sens, wiec istnieje zmienna objaśniająca, która ma liniowy wplyw na zmienną objaśnianą
#c)
R_kwadrat <- summary(model)$r.squared
adj_R_kwadrat <- summary(model)$adj.r.squared
summary(model)

#Zadanie 4
#a)
dane <- as.data.frame(dane)
help('vif')
vif(model) #X2 ma najwiekszą wartość wspólczynnika VIF, wiec usuwamy z modelu
model_1 <- lm(Y~X1+X3+X4+X5+X6+X7+X8+X9+X10, data=as.data.frame(dane))

#b)
vif(model_1) #usuwamy X9
model_2 <- lm(Y~X1+X3+X4+X5+X6+X7+X8+X10, data=as.data.frame(dane))
vif(model_2) #Usuwamy X10
model_3 <- lm(Y~X1+X3+X4+X5+X6+X7+X8, data=as.data.frame(dane))
vif(model_3) #Rozwiązaliśmy problem wspóliniowości

dane_1 <- dane
dane_1$X2 <- NULL
dane_1$X9 <- NULL
dane_1$X10 <- NULL
ggcorr(dane_1, method = c("everything", "pearson"), label =T) +
  ggtitle('Heatmap')

#Zadanie 5
#a
p <- sum(hatvalues(model_3)) #ślad macierzy H

#Szukamy obserwacji wplywowych ze wzgledu na x
outliers <- hatvalues(model_3) > 3 * mean(hatvalues(model_3))
dane_1[outliers, ] #obserwacja nr 175 jest wplywowa ze wzgledu na x

#b
odl_cook <- cooks.distance(model_3)
D_n <- data.frame(cbind(1:length(odl_cook), odl_cook))
cutoff <- data.frame(x = c(0, nrow(D_n)), y=4/(nrow(D_n)-p),cutoff = factor(4/(nrow(D_n)-p)))

ggplot(data=D_n, mapping=aes(x=V1, y=odl_cook)) +
  geom_point(aes(colour='(n,D_n)'), lwd=1.5, color='black') +
  ggtitle('Wykres rozproszenia dla par (n, D_n)') +
  geom_hline(yintercept =4/(nrow(D_n)-p), color='red', linetype='dashed', lwd=1.5) + 
  annotate("text",x=10, y=4/(nrow(D_n)-p),vjust=-1, label = "y=4/(n-p)", color='red')

#zatem obserwacje nr 100 i 200 mogą być wplywowe i odstające ze wzgledu na x lub y, sprawdźmy to
student_rezydua <- rstudent(model_3)
student_rezydua[100] #odstające ze wzgledu na y
student_rezydua[200] #odstające ze wzgledu na y

dane_bez_100 <- dane_1[-100,]
dane_bez_200 <- dane_1[-200,]
model_bez_100 <- lm(Y~X1+X3+X4+X5+X6+X7+X8, data=as.data.frame(dane_bez_100))
model_bez_200 <- lm(Y~X1+X3+X4+X5+X6+X7+X8, data=as.data.frame(dane_bez_200))

beta_bez_100 <- model_bez_100$coefficients
beta_bez_200 <- model_bez_200$coefficients
beta <- model_3$coefficients
matrix(c(beta_bez_100, beta), nrow=length(beta), ncol=2) #cieżko ocenić
matrix(c(beta_bez_200, beta), nrow=length(beta), ncol=2) #cieżko ocenić


#c
istotne <-as.numeric(c(rep(0,nrow(dane_1)))) #inicjalizacja wektora na
                                              #indeksy obserwacji istotnie statystycznych
for (k in 1:length(student_rezydua)) {
  if (abs(student_rezydua[k]) >= 2) {
    istotne[k] <- k #gdy obserwacja statystycznie istotna, to przypisujemy 
  }
}
istotne[which(istotne > 0)] #obserwacje 100 i 200 są podejrzane, odstajace ze wzgledu na y

#d
dane_1[which(dffits(model_3) > 2*sqrt(p/nrow(dane_1))),] #obserwacje 100 i 200 są wplywowe

#usuwamy obserwacje wpływowe
dane_2 <- dane_1[-c(100,175,200),]

#Zadanie 6
model_4 <- lm(Y~X1+X3+X4+X5+X6+X7+X8, data=dane_2)
#a
beta_model_4 <- model_4$coefficients #estymator najmniejszych kwadratów

#b
summary(model_4) #istnieje zmienna objaśniając mająca liniowy wpływ na zmienną objaśnianą

#c
R_kwadrat_model_4 <- summary(model_4)$r.squared
adj_R_kwadrat_model_4 <- summary(model_4)$adj.r.squared

#Zadanie 7
help('step')

step(model_4, direction = 'both')
step(model_4, direction = 'forward')
step(model_4, direction = 'backward')

summary(lm(Y~X1+X3+X4+X6+X8, data=dane_2))$adj.r.squared
adj_R_kwadrat_model_4
M <- lm(Y~X1+X3+X4+X6+X8, data=dane_2)

#a
beta_model_M <- M$coefficients

#b
summary(M)

#c
M <- lm(Y~X3+X4+X6+X8, data=dane_2)
#d
PU <- confint(M,level=0.95)
data.frame(PU, M$coefficients)

#e
summary(M)$r.squared
summary(M)$adj.r.squared

#Zadanie 8
#a, użyjmy studentyzowanych rezyduuów
studentyzowane_rezydua <- data.frame(studres(M))
dane_3 <- dane_2[,-c(1,4,6)]
d <- data.frame(group=rep(1:2, each=nrow(dane_3)), sample=c(rnorm(nrow(dane_3),0,1),studres(M)))
d$group <- replace(d$group, c(1:197), 'Dystr. stand. rozkładu normalnego')
d$group <- replace(d$group, c(198:394), 'Studentyzowane rezydua')

qplot(sample=sample, data=d, color=as.factor(group)) +
  labs(colour='Legenda') +
  ggtitle("Wykresy kwantylowe")

#b
dane_rezyduum <- cbind(dane_3,studres(M))
names(dane_rezyduum)[6] <- 'stud_rezydua'
head(dane_rezyduum)
dane_rezyduum %>%
  ggplot(aes(x = X3, y = stud_rezydua)) +
  geom_point(colour = "red") +
  ggtitle('Wykres rozproszenia') +
  labs(x='x', y='y') +
  geom_hline(yintercept =0, color='blue', linetype='dashed', lwd=1.5) + 
  annotate("text",x=-1, y=0,vjust=-1, label = "y=0", color='blue')

dane_rezyduum %>%
  ggplot(aes(x = X4, y = stud_rezydua)) +
  geom_point(colour = "red") +
  ggtitle('Wykres rozproszenia') +
  labs(x='x', y='y') +
  geom_hline(yintercept =0, color='blue', linetype='dashed', lwd=1.5) + 
  annotate("text",x=-1, y=0,vjust=-1, label = "y=0", color='blue')

dane_rezyduum %>%
  ggplot(aes(x = X6, y = stud_rezydua)) +
  geom_point(colour = "red") +
  ggtitle('Wykres rozproszenia') +
  labs(x='x', y='y') +
  geom_hline(yintercept =0, color='blue', linetype='dashed', lwd=1.5) + 
  annotate("text",x=5, y=0,vjust=-1, label = "y=0", color='blue')

dane_rezyduum %>%
  ggplot(aes(x = X8, y = stud_rezydua)) +
  geom_point(colour = "red") +
  ggtitle('Wykres rozproszenia') +
  labs(x='x', y='y') +
  geom_hline(yintercept =0, color='blue', linetype='dashed', lwd=1.5) + 
  annotate("text",x=-15, y=0,vjust=-1, label = "y=0", color='blue')

#c
dane_rezyduum %>%
  ggplot(aes(x = Y, y = stud_rezydua)) +
  geom_point(colour = "red") +
  ggtitle('Wykres rozproszenia') +
  labs(x='x', y='y') +
  geom_hline(yintercept =0, color='blue', linetype='dashed', lwd=1.5) + 
  annotate("text",x=80, y=0,vjust=-1, label = "y=0", color='blue')

#Zadanie 9
predict(M, data.frame(X3=3, X4=4,X6=6,X8=8))


