rm(list=ls())
library(plyr)
library(lmtest)
library(ggplot2)
library(ggpubr)
library(GGally)
library(car)
library(caret)
library(pROC)
library(ROCit)
library(MASS)
library(xlsx)
library(xtable)

getwd()
setwd('C:/Users/Lenovo/Desktop/Modele regresji i ich zastosowania')
dane <- read.table('logistyczna.txt', skip=1, header=T)
head(dane)
dane <- as.data.frame(dane)
dane$STAN <- as.factor(dane$STAN)
dim(dane)
#Zadanie 1
#(a)
set.seed(12)
help('seq_len')
indeks <- sample(seq_len(nrow(dane)), size=0.7*nrow(dane)) #w rozmiarze jest wzieta podloga
crea
treningowy <- dane[indeks,]
testowy <- dane[-indeks,]
freq_train <- count(treningowy, vars='STAN')
freq_test <- count(testowy, vars='STAN')

#frakcje są w porządku
freq_test
freq_train
help('table')
72/81
160/186
#(b)
#Tworzymy model regresji logistycznej
model <- glm(STAN~RMS10+RMS20+RMS30+PWD+A+DD+YA+YDD+AR, data=treningowy, method='glm.fit',
             family = 'binomial')
model
#(c)
model_0 <- glm(STAN~1, data=treningowy, method='glm.fit',
               family = 'binomial')


#odrzucamy H_0
lrtest(model, model_0)

#(d)
summary(model)

#bez stalej
beta_hat <- model$coefficients[2:10]
se_hat <- summary(model)$coefficients[,2][2:10]

CI <- function(beta, se, alpha) {
  L <- rep(0, length(beta))
  R <- rep(0, length(beta))
  for (k in 1:length(beta)) {
    L[k] <- beta[k]+qnorm(alpha/2)*se[k]
    R[k] <- beta[k]-qnorm(alpha/2)*se[k]
  }
  return(data.frame(L,R, beta))
}

CI(beta_hat, se_hat, 0.05)

model_1 <- glm(STAN~PWD, data=treningowy, method='glm.fit',
               family = 'binomial')
ggcorr(treningowy[-1], method = c("everything", "pearson"), label =T) +
  ggtitle('Heatmap')

vif(model)

#(e)
help('predict')
prob_0.5 <- predict(model_1, testowy, type='response')
pred_0.5 <- rep(0, dim(testowy)[1])
pred_0.5[prob_0.5 > 0.5] <- 1
tab_0.5 <- table(pred_0.5, testowy$STAN)
tab_0.5
TP_0.5 <- tab_0.5[2,2]
TN_0.5 <- tab_0.5[1,1] 
FP_0.5 <- tab_0.5[2,1]
FN_0.5 <- tab_0.5[1,2]
sens_0.5 <- TP_0.5/(TP_0.5+FN_0.5)
spec_0.5 <- TN_0.5/(TN_0.5+FP_0.5)
FPR_0.5 <- FP_0.5/(FP_0.5+TN_0.5)
FNR_0.5 <- FN_0.5/(FN_0.5+TP_0.5)
prop_correct_0.5 <- mean(pred_0.5 == testowy$STAN)
prop_correct_0.5
prob_0.7 <- predict(model_1, testowy, type='response')
pred_0.7 <- rep(0, dim(testowy)[1])
pred_0.7[prob_0.7 > 0.7] <- 1
tab_0.7 <- table(pred_0.7, testowy$STAN)
tab_0.7
TP_0.7 <- tab_0.7[2,2]
TN_0.7 <- tab_0.7[1,1] 
FP_0.7 <- tab_0.7[2,1]
FN_0.7 <- tab_0.7[1,2]
sens_0.7 <- TP_0.7/(TP_0.7+FN_0.7)
spec_0.7 <- TN_0.7/(TN_0.7+FP_0.7)
FPR_0.7 <- FP_0.7/(FP_0.7+TN_0.7)
FNR_0.7 <- FN_0.7/(FN_0.7+TP_0.7)
prop_correct_0.7 <- mean(pred_0.7 == testowy$STAN)
prop_correct_0.7
help('predict')

#(f)
ROC <- rocit(predict(model_1, testowy,type='response'), class=testowy$STAN,
             method='non')

plot(ROC, YIndex=FALSE)
title(main='Krzywa ROC')

#Zadanie 2
dane_2 <- read.xlsx2('C:/Users/Lenovo/Desktop/Modele regresji i ich zastosowania/cancer remission.xlsx',
                     sheetName = 1)
head(dane_2)
for (k in 1:ncol(dane_2)) {
    dane_2[,k] <- as.numeric(dane_2[,k])
}

ggcorr(dane_2[-1], method = c("everything", "pearson"), label =T) +
  ggtitle('Heatmap')
vif(glm(remission~cell+temp+smear.+li+blast+infil, data=dane_2, method='glm.fit',
        family = 'binomial'))

model_2 <- glm(remission~1, data=dane_2, method='glm.fit',
                   family = 'binomial')
model_2_1 <- glm(remission~cell, data=dane_2, method='glm.fit',
                 family = 'binomial')
model_2_2 <- glm(remission~smear., data=dane_2, method='glm.fit',
                 family = 'binomial')
model_2_3 <- glm(remission~li, data=dane_2, method='glm.fit',
                 family = 'binomial')
model_2_4 <- glm(remission~blast, data=dane_2, method='glm.fit',
                 family = 'binomial')
model_2_5 <- glm(remission~temp, data=dane_2, method='glm.fit',
                 family = 'binomial')
help('waldtest')
waldtest(model_2, model_2_1)
waldtest(model_2, model_2_2)
waldtest(model_2, model_2_3) #bierzemy te zmienną do modelu
waldtest(model_2, model_2_4)
waldtest(model_2, model_2_5) 

model_2 <- glm(remission~li, data=dane_2, method='glm.fit',
               family = 'binomial')
model_2_1 <- glm(remission~li+cell, data=dane_2, method='glm.fit',
                 family = 'binomial')
model_2_2 <- glm(remission~li+smear., data=dane_2, method='glm.fit',
                 family = 'binomial')
model_2_3 <- glm(remission~li+temp, data=dane_2, method='glm.fit',
                 family = 'binomial')
model_2_4 <- glm(remission~li+blast, data=dane_2, method='glm.fit',
                 family = 'binomial')
waldtest(model_2, model_2_1)
waldtest(model_2, model_2_2)
waldtest(model_2, model_2_3)#bierzemy te zmienną do modelu
waldtest(model_2, model_2_4) 

model_2 <- glm(remission~li+temp, data=dane_2, method='glm.fit',
               family = 'binomial')
#czy usunąć którąś ze zmiennych?
model_2_1 <- glm(remission~li, data=dane_2, method='glm.fit',
                 family = 'binomial')
model_2_2 <- glm(remission~temp, data=dane_2, method='glm.fit',
                 family = 'binomial')
#nie wyrzucamy
waldtest(model_2, model_2_1)
waldtest(model_2, model_2_2)
names(dane_2)
model_2_1 <- glm(remission~li+temp+cell, data=dane_2, method='glm.fit',
                 family='binomial')
model_2_2 <- glm(remission~li+temp+smear., data=dane_2, method='glm.fit',
                 family='binomial')
model_2_3 <- glm(remission~li+temp+blast, data=dane_2, method='glm.fit',
                 family='binomial')
waldtest(model_2, model_2_1) #bierzemy do modelu
waldtest(model_2, model_2_2)
waldtest(model_2, model_2_3)

model_2 <- glm(remission~li+temp+cell, data=dane_2, method='glm.fit',
               family = 'binomial')

#czy usuwamy jakąś zmienną?
model_2_1 <- glm(remission~li+temp, data=dane_2, method='glm.fit',
                 family='binomial')
model_2_2 <- glm(remission~li+cell, data=dane_2, method='glm.fit',
                 family='binomial')
model_2_3 <- glm(remission~temp+cell, data=dane_2, method='glm.fit',
                 family='binomial')

#nie usuwamy
waldtest(model_2, model_2_1)
waldtest(model_2, model_2_2)
waldtest(model_2, model_2_3)

model_2_1 <- glm(remission~li+temp+cell+smear., data=dane_2, method='glm.fit',
                 family='binomial')
model_2_2 <- glm(remission~li+temp+cell+blast, data=dane_2, method='glm.fit',
                 family='binomial')

#nie dolączamy kolejnych zmiennych
waldtest(model_2, model_2_1)
waldtest(model_2, model_2_2)

beta_hat_2 <- model_2$coefficients[2:4]
se_hat_2 <- summary(model_2)$coefficients[,2][2:4]
p_value_2 <- summary(model_2)$coefficients[,4][2:4]
aic_2 <- model_2$aic
aic_2

pi_prob <- predict(model_2, dane_2, type='response')
#teraz konstruujemy przedzialy ufności Walda dla estymowanego p-ństwa
#(1) liczymy CI Walda dla iloczynu x^T*beta
dim(model_2$model)[2]


inverse_logit <- function(x){
  return(exp(x)/(1+exp(x)))
}
#liczy logit
logit <- predict(model_2, dane_2, type='link', se.fit=TRUE)

CI_R <- inverse_logit(logit$fit + (logit$se.fit*qnorm(1-0.05/2)))
CI_L <- inverse_logit(logit$fit - (logit$se.fit*qnorm(1-0.05/2)))
model_2$model$remission <- as.factor(model_2$model$remission)
df <- data.frame(model_2$model,
                 pi_prob,
                 CI_L,
                 CI_R)
names(df)[5] <- 'estimated_pi(x)'
names(df)[6] <- 'CI_Wald_left'
names(df)[7] <- 'CI_Wald_right'
head(df)
logit$fit


help('xtable')
xtable(df)
