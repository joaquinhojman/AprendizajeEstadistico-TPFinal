library(glmnet)
library(caret)
setwd('Escritorio/aprendizaje_estadistico/tps/Aprendizaje_Estadistico_tp3/')
datos <- read.csv('dummy_heart.csv')

{
  set.seed(23)
  muestra <- sample(1:303, 303 * 0.7)
}


train <- datos[muestra,]

test <- datos[-muestra, -9]
y_test <- datos[-muestra, 9]
logit <-glm(formula = 'target ~ .', family = 'binomial', data = train)

preds <- ifelse(predict(logit,newdata = test ,type = 'response') > 0.5, 1, 0)

mean(y_test == preds)

summary(logit)


#sin ca_4 
#menor AIC misma precision -> vale la pena tirarlo
logit.2 <- glm(formula = 'target ~ . -ca_4', family = 'binomial', data = train)
summary(logit.2)
preds.2 <- ifelse(predict(logit.2, newdata = test ,type = 'response') > 0.5, 1, 0)
mean(y_test == preds.2)

# test chi^2 para ver si agregando ca_4 se llega a un modelo con una mejora significativa
anova(logit.2, logit, test='Chisq')
# el p-valor dio 0.274, asi que no hay evidencia suficiente de que ca_4 sea una variable que agregue mas informacion
# ademas, el aic define como mas parsimonioso al modelo mas sencillo (sin ca_4)
# descartamos ca_4 del analisis


# agregando alguna variable? 

logit.3 <- glm(formula = 'target ~ . -ca_4 + slope_2:oldpeak', family = 'binomial', data = train)
preds.3 <- ifelse(predict(logit.3, newdata = test ,type = 'response') > 0.5, 1, 0)
mean(y_test == preds.3)
summary(logit.3)
anova(logit.2, logit.3, test='Chisq')

# explicacion 

# el segmento st del electrocardiograma es la parte de la forma de onda donde la tension leida es 0mV
# (por lo menos en casos normales)
# este segmento se "deprime", cae por debajo del eje en los casos en que una persona sufre una cardiopatia
# por ej puede ser un infarto 
# la pendiente medida en el segmento st es un indicador de una cardiopatia (si es positiva o negativa)
# si se da que es la pendiente 2 (que no sabemos cual es) y que ademas la persona tuvo un pico abnormal
# las chances son mayores (e^1.193760 = 3.3 aprox, esto es 3 : 1 odds sobre el target)

logit.4 <- glm(formula = 'target ~ . -ca_4 + thalach:ca_3', family = 'binomial', data = train)
preds.4 <- ifelse(predict(logit.4, newdata = test ,type = 'response') > 0.5, 1, 0)
mean(y_test == preds.4)
summary(logit.4)
anova( logit.3,logit.4, test='Chisq')
train


logit.5 <- glm(formula = 'target ~ (. - ca_4)^2', family = 'binomial', data = train, control = list(maxit = 1000))
preds.5 <- ifelse(predict(logit.5 , newdata = test ,type = 'response') > 0.5, 1, 0)
mean(y_test == preds.5)
summary(logit.5)
logit.5$iter


## kNN


