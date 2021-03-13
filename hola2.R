library(caret)
library(woe)
library(xtable)
library(tidyverse)
library(ggplot2)
library(verification)
library(GGally)

setwd(dir = 'Escritorio/aprendizaje_estadistico/tps/Aprendizaje_Estadistico_tp3/')
heart <- read.csv(file ='heart.csv')
heart_dummy <- read.csv('dummy_heart.csv')
heart[sample(1:303, 10),]

heart$sex <- as.factor(heart$sex)
heart$cp <- as.factor(heart$cp)
heart$fbs <- as.factor(heart$fbs)
heart$restecg <- as.factor(heart$restecg)
heart$exang <- as.factor(heart$exang)
heart$slope <- as.factor(heart$slope)
heart$ca <- as.factor(heart$ca)
heart$thal <- as.factor(heart$thal)
heart$target <- as.factor(heart$target)
levels(heart)
ggparcoord(heart, scale = 'std',
           columns = c(1, 4, 5, 8, 10), groupColumn = 14
)
{
  set.seed(23)
  muestra <- sample(1:303, 303 * 0.7)
}

train <- heart_dummy[muestra,]
test <- heart_dummy[-muestra, ]


## modelo base 
modelo.1 <- glm('target ~ .', family = 'binomial', data = train)
# acc = 86.81319 % 
preds <- predict(modelo.1, newdata=  test, type='response')
preds.train <- predict(modelo.1, newdata = train, type='response')
mean(test$target ==ifelse(preds > 0.6,1, 0))
mean(train$target == ifelse(preds.train > 0.5, 1, 0))
summary(modelo.1)
## curvas roc
confint(modelo.1)

roc.plot(
  as.numeric(levels(test$target))[test$target],
  preds,
  threshold = seq(0, max(preds), 0.001),
  plot.thres = c(0.5, 0.6, 0.7)
)


ggplot(heart_dummy, aes(x = as.factor(ca_4), fill = as.factor(target))) + geom_bar() + xlab('Ca = 4 ?') + ylab('Cantidad de aciertos') + labs(fill="Enfermedad")
train$ca <- as.ordered(train$ca)
test$ca <- as.ordered(test$ca)

modelo.2 <- glm('target ~ . - ca_4', family = 'binomial', data = train)
xtable(summary(modelo.2)$coef)
preds <- predict(modelo.2, newdata =test , type = 'response')
mean(test$target == ifelse(predict(modelo.2, newdata =test , type = 'response') > 0.6, 1, 0))
mean(train$target == ifelse(predict(modelo.2, newdata = train, type = 'response') > 0.6, 1, 0))
roc.plot(
  test$target,
  preds,
  threshold = seq(0, max(preds), 0.001),
  plot.thres = c(0.5, 0.6, 0.7)
)
summary(modelo.2)

confint(modelo.2)

anova(modelo.2, modelo.1)
1-pchisq(1.196585,1)
qchisq(0.95,1)
mi_chi <- function(x){
  return(1-pchisq(x, df=1))
}

ggplot(data.frame(x=seq(1, 4.5, 0.001)), aes(x)) +
  stat_function(fun = mi_chi) +  stat_function(fun = mi_chi, geom="area", xlim= c(qchisq(p=.95, df=1), 4.5),fill="red", alpha=0.3) +
  geom_vline(xintercept = anova(modelo.2, modelo.1)$Deviance[2], linetype="dotted", color="red", size=0.8) + xlab("Chi") + ylab("Densidad") +
  labs(title="Zona de rechazo y Deviance") + theme(plot.title = element_text(hjust = 0.5))

modelo.3 <- glm('target ~ . - ca_4 + slope_1:oldpeak', family = 'binomial', data = train)
summary(modelo.3)
preds.3 <- predict(modelo.3, newdata = test, type = 'response')
mean(test$target == ifelse(predict(modelo.3, newdata = test, type = 'response') > 0.5, 1, 0))
mean(train$target == ifelse(predict(modelo.3, newdata = train, type = 'response') > 0.5, 1, 0))

roc.plot(
  test$target,
  preds.3,
  threshold = seq(0, max(preds.3), 0.0001),
  plot.thres = c(0.5, 0.6, 0.7)
)

anova(modelo.2, modelo.3, test= 'Chisq')


# knn

ctrl <- trainControl(method="cv") #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(target ~ ., data = train, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
knnFit

preds.knn <- predict(knnFit, newdata = test)
mean(preds.knn == test$target)
confusionMatrix(knnFit)
preds.knn
test$target


# woe 
?woe
woe(Data = train, Independent = 'sex', Continuous = FALSE, Dependent  = 'target', C_Bin = 10,Bad = 0, Good = 1 )
woe(Data = train, Independent = 'cp', Continuous = FALSE, Dependent  = 'target', C_Bin = 10,Bad = 0, Good = 1 )
woe(Data = train, Independent = 'fbs', Continuous = FALSE, Dependent  = 'target', C_Bin = 10,Bad = 0, Good = 1 )
woe(Data = train, Independent = 'restecg', Continuous = FALSE, Dependent  = 'target', C_Bin = 10,Bad = 0, Good = 1 )
woe(Data = train, Independent = 'exang', Continuous = FALSE, Dependent  = 'target', C_Bin = 10,Bad = 0, Good = 1 )
woe(Data = train, Independent = 'slope', Continuous = FALSE, Dependent  = 'target', C_Bin = 10,Bad = 0, Good = 1 )
woe(Data = train, Independent = 'ca', Continuous = FALSE, Dependent  = 'target', C_Bin = 10,Bad = 0, Good = 1 )
woe(Data = train, Independent = 'thal', Continuous = FALSE, Dependent  = 'target', C_Bin = 10,Bad = 0, Good = 1 )

sex.map <- c("0"=98.5, "1"=-41.4)
cp.map <- c("0"=-116.7, "1"=155.4, "2"=102.4, "3"=68.3)
fbs.map <- c("0"=-1.7, "1"=11)
restecg.map <- c("0"=-41.1, "1"=42.2, "2"=79.9)
exang.map <- c("0"=58.9, "1"=-140.8)
slope.map <- c("0"=-24.5, "1"=-80.3, "2"=88.8)
ca.map <- c("0"=79.7, "1"=-69, "2"=-156, "3"=-148.3, "4"=0)
thal.map <- c("0"=-10.5, "1"=-101.2, "2"=112.7, "3"=-136.4)


match(heart$sex, sex.map)
match(heart$cp, cp.map)
match(heart$fbs, fbs.map)
match(heart$restecg, restecg.map)
match(heart$exang, exang.map)
match(heart$slope, slope.map)
match(heart$ca, ca.map)
match(heart$thal, thal.map)

levels(heart$sex) <- sex.map
levels(heart$cp) <- cp.map
levels(heart$fbs) <- fbs.map
levels(heart$restecg) <- restecg.map
levels(heart$exang) <- exang.map
levels(heart$slope) <- slope.map
levels(heart$ca) <- ca.map
levels(heart$thal) <- thal.map


heart$sex <- as.numeric(levels(heart$sex))[heart$sex]
heart$cp <- as.numeric(levels(heart$cp))[heart$cp]
heart$fbs <- as.numeric(levels(heart$fbs))[heart$fbs]
heart$restecg <- as.numeric(levels(heart$restecg))[heart$restecg]
heart$exang <- as.numeric(levels(heart$exang))[heart$exang]
heart$slope <- as.numeric(levels(heart$slope))[heart$slope]
heart$ca <- as.numeric(levels(heart$ca))[heart$ca]
heart$thal <- as.numeric(levels(heart$thal))[heart$thal]

modelo.woe <- glm('target ~ .', family = 'binomial', data = train)
summary(modelo.woe)
mean(test$target == ifelse(predict(modelo.woe, newdata = test, type = 'response') > 0.5, 1, 0))
#meh
