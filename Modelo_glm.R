#MODELO REGRESION LOGISTICA#

#LIBRERIAS UTILIZADAS

library(readr)
library(dplyr)
library(tidyr)
library(tseries)
library(car)
library(DescTools)
library(ggplot2)
library(gplots)
library(ggpubr)
library(stats)
library(rstatix)
library(cowplot)
library(ggstatsplot)
library(rsample)
library(tidyverse)
library(ISLR)
library(vcd)


EES_2018 <- read_delim("CSV/EES_2018.csv", #Ruta del archivo
                       delim = "\t", escape_double = FALSE, trim_ws = TRUE)

dg = EES_2018 %>% select(ORDENTRA, SALBASE, JSP1, SEXO, ANOS2, TIPOPAIS, ESTU, ANOANTI, TIPOJOR, TIPOCON, CNO1, CNACE, NUTS1, FACTOTAL)
names (dg)


dg$ANOS2 <- as.numeric(factor(dg$ANOS2, 
                              levels=c("01", "02", "03",
                                       "04", "05", "06")))

dg<-mutate(dg,
           ORDENTRA = as.factor(ORDENTRA),
           SEXO = as.factor(SEXO),
           ANOS2 = as.factor(ANOS2),
           TIPOPAIS = as.factor(TIPOPAIS),
           ESTU = as.factor(ESTU),
           TIPOJOR = as.factor(TIPOJOR),
           TIPOCON = as.factor(TIPOCON),
           CNO1 = as.factor(CNO1),
           CNACE = as.factor(CNACE),
           NUTS1 = as.factor(NUTS1),
           
)
dg

dg$CNO2 <- dg$CNO1





dg$TIPOCON <- factor(dg$TIPOCON, labels = c("Indefinido", "Temporal"))
table(dg$TIPOCON)
dg$TIPOJOR <- factor(dg$TIPOJOR, labels = c("Completo", "Parcial"))
table(dg$TIPOJOR)
dg$SEXO <- factor(dg$SEXO, labels = c("Hombre", "Mujer"))
table(dg$SEXO)


set.seed(123)
split_start <- initial_split(dg, prop = 0.7) 
train <- training(split_start)
test <- testing(split_start)

table(train$TIPOCON) %>% prop.table()
table(test$TIPOCON) %>% prop.table()

table(train$TIPOJOR) %>% prop.table()
table(test$TIPOJOR) %>% prop.table()




#MODELO LOG PARA TIPO DE CONTRATO#
#veo la distibución para mi variable Tipo de Contrato
dg %>%
  ggplot(aes(x = TIPOCON)) +
  geom_bar() +
  labs(x = "Tipo de Contrato",
       y = "Número de observaciones")


tabla <- table(dg$TIPOCON, dg$SEXO,
               dnn = c("Tipo de contrato","sexo"))
addmargins(tabla)

plot(tabla, col = c("#FF7256", "#A2CD5A"), main = " ")
#Con la matriz de contingencia no queda del todo claro si exite relación. 
#Parece que las mujeres sí sufren más parcialidad.

#Prueba Chi-cuadrado para examinar relación
#H0: No existe relación entre variables
chisq.test(tabla)
#Rechazamos H0, exite relación entre sexo y tipo de contrato

contrasts(dg$TIPOCON)#Toma de referencia indefinido. Mi éxito es temporal



# Ajuste de un modelo logístico simple.
logit_sexo <- glm(TIPOCON ~ SEXO, data = train, family = "binomial")

summary(logit_sexo)


confint(object = logit_sexo, level = 0.95 )
anova(logit_sexo, test = "Chisq")


#Validación train
predicted_value <-0
predicted_value <- predict(logit_sexo, type = "response", newdata = train)

predicted_class <- ifelse(predicted_value > 0.4, "Sí", "No")

performace_data <- data.frame(observed= train$TIPOCON, predicted= predicted_class)

#Construcción Matriz de Confusión
positive <- sum(performace_data$observed=="Temporal")
negative <- sum(performace_data$observed=="Indefinido")
predicted_positive <- sum(performace_data$predicted=="Sí")
predicted_negative <- sum(performace_data$predicted=="No")
total <- nrow(performace_data)
data.frame(positive, negative, predicted_positive, predicted_negative)

clasificado1 <- 1*(predicted_value>=0.4)
tabla1 <- table(train$TIPOCON, clasificado1, dnn = c("observaciones", "predicciones"))
tabla1

mosaic(tabla1, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

tp= sum(performace_data$observed=="Temporal" & performace_data$predicted=="Sí")
tn= sum(performace_data$observed=="Indefinido" & performace_data$predicted=="No")
fp= sum(performace_data$observed=="Indefinido" & performace_data$predicted=="Sí")
fn= sum(performace_data$observed=="Temporal" & performace_data$predicted=="No")
data.frame(tp, tn, fp, fn)

exactitud = (tp+tn)/total
tasa_error= (fp+fn)/total
sensibilidad= tp/positive
especifidad= tn/negative
precision= tp/predicted_positive
vpn= tn/predicted_negative

data.frame(exactitud, tasa_error, sensibilidad, especifidad, precision, vpn)






#Validación test
predicted_value <-0
predicted_value <- predict(logit_sexo, type = "response", newdata = test)

predicted_class <- ifelse(predicted_value > 0.4, "Sí", "No")

performace_data <- data.frame(observed= test$TIPOCON, predicted= predicted_class)

#Construcción Matriz de Confusión
positive <- sum(performace_data$observed=="Temporal")
negative <- sum(performace_data$observed=="Indefinido")
predicted_positive <- sum(performace_data$predicted=="Sí")
predicted_negative <- sum(performace_data$predicted=="No")
total <- nrow(performace_data)
data.frame(positive, negative, predicted_positive, predicted_negative)

clasificado1 <- 1*(predicted_value>=0.4)
tabla1 <- table(test$TIPOCON, clasificado1, dnn = c("observaciones", "predicciones"))
tabla1

mosaic(tabla1, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

tp= sum(performace_data$observed=="Temporal" & performace_data$predicted=="Sí")
tn= sum(performace_data$observed=="Indefinido" & performace_data$predicted=="No")
fp= sum(performace_data$observed=="Indefinido" & performace_data$predicted=="Sí")
fn= sum(performace_data$observed=="Temporal" & performace_data$predicted=="No")
data.frame(tp, tn, fp, fn)

exactitud = (tp+tn)/total
tasa_error= (fp+fn)/total
sensibilidad= tp/positive
especifidad= tn/negative
precision= tp/predicted_positive
vpn= tn/predicted_negative

data.frame(exactitud, tasa_error, sensibilidad, especifidad, precision, vpn)






# Modelo lineal generalizado estimado por MLE (máxima verosimilitud)

tabla2 <- table(dg$TIPOCON, dg$ANOS2,
                dnn = c("Tipo de jornada","edad"))
tabla3 <- table(dg$TIPOCON, dg$ESTU,
                dnn = c("Tipo de jornada","estudios"))
tabla4 <- table(dg$TIPOCON, dg$NUTS1,
                dnn = c("Tipo de jornada","localización"))
tabla5 <- table(dg$TIPOCON, dg$CNO2,
                dnn = c("Tipo de jornada","CNO"))
tabla6 <- table(dg$TIPOCON, dg$CNACE,
                dnn = c("Tipo de jornada","CNAE"))

#Prueba Chi-cuadrado para examinar relación
#H0: No existe relación entre variables
chisq.test(tabla2)
chisq.test(tabla3)
chisq.test(tabla4)
chisq.test(tabla5)
chisq.test(tabla6)

dg %>%
  group_by(TIPOCON) %>%
  summarize(
    obs = n(),
    media_AN = mean(ANOANTI),
    
  )

hist(dg$ANOANTI, main = "Histograma de frecuencias", # Frecuencia
     ylab = "Frecuencia", xlab = "Año antigüedad")


logit <- glm(TIPOCON ~ SEXO + ANOS2 + ESTU + NUTS1+ ANOANTI + CNO1 + CNACE, data = train,
             family = binomial())

summary(logit)

vif(logit)#Elimino CNAE por multicolinealidad con CNO



#MODELO SIN CNAE
logit <- glm(TIPOCON ~ SEXO + ANOS2 + ESTU + NUTS1 + ANOANTI + CNO1, data = train,
             family = binomial())

summary(logit)

vif(logit)


#Recodifico 
train$NUTS12 <- recode(train$NUTS1, "1=1 ; 2=2 ; 3=3 ; 4=4 ; 5=5 ; 6=6 ; 7=6")
test$NUTS12 <- recode(test$NUTS1, "1=1 ; 2=2 ; 3=3 ; 4=4 ; 5=5 ; 6=6 ; 7=6")
table(train$NUTS12)
table(train$NUTS1)

logit <- glm(TIPOCON ~ SEXO + ANOS2 + ESTU + NUTS12 + ANOANTI + CNO1, data = train,
             family = binomial())

summary(logit)#Q0 sigue sin ser significativa, pero las demás categorías sí
vif(logit)


#Elimino la categoría Q0 de CNO, que hace referencia a los militares
table(train$CNO1)

train <- subset(x=train, subset = train$CNO1 != "Q0" )
test <- subset(x=test, subset = test$CNO1 != "Q0" )
table(train$CNO1)

logit <- glm(TIPOCON ~ SEXO + ANOS2 + ESTU + NUTS12 + ANOANTI + CNO1, data = train,
             family = binomial())

summary(logit)
vif(logit)


#Recodifico la variable de edad
train$ANOS22 <- recode(train$ANOS2, "1=1 ; 2=2 ; 3=3 ; 4=4 ; 5=5 ; 6=5")
test$ANOS22 <- recode(test$ANOS2, "1=1 ; 2=2 ; 3=3 ; 4=4 ; 5=5 ; 6=5")
table(train$ANOS2)
table(train$ANOS22)


logit <- glm(TIPOCON ~ SEXO + ANOS22 + ESTU + NUTS12 + ANOANTI + CNO1, data = train,
             family = binomial())

summary(logit)
vif(logit)

#Elimino NUTS1, aun que unifique variables, no es significativo

train$ESTU2 <- recode(train$ESTU, "1=1 ; 2=1 ; 3=3 ; 4=4 ; 5=5 ; 6=6 ; 7=7")
test$ESTU2 <- recode(test$ESTU, "1=1 ; 2=1 ; 3=3 ; 4=4 ; 5=5 ; 6=6 ; 7=7")


logit <- glm(TIPOCON ~ SEXO + ANOS22 + ESTU2 + ANOANTI + CNO1, data = train,
             family = binomial())

summary(logit)

confint(object = logit, level = 0.95 )

# Diferencia de residuos
dif_residuos <- logit$null.deviance - logit$deviance

# Grados libertad
df <- logit$df.null - logit$df.residual
# p-value
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)

paste("Diferencia de residuos:", round(dif_residuos, 4))
paste("Grados de libertad:", df)
paste("p-value:", round(p_value, 4))

#El modelo es significativo


#Multicolinealidad
vif(logit) #No existe multicolinealidad


#Linealidad
logodds <- logit$linear.predictor
loganti <- data.frame(logodds, A.Anti = train$ANOANTI)
ggplot(data = loganti, aes(x = A.Anti, y = logodds)) + 
  geom_point() +
  ggtitle(" ") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
#La valiable ANOANTI, la única variable numética


#Independencia de las observaciones
Indice <- seq(1,151669,1)
Residuales <- logit$residuals
residuales <- data.frame(Indice, Residuales)
ggplot(data = residuales, aes(x = Indice, y = Residuales)) +
  geom_point() +
  ggtitle(" ") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Se observa que la mayoría de los resíduos son positivos y que existen algunos outliers
#Sin embargo, no se observa ningún patrón, por lo que se puede concluir que se cumple el supuesto de independencia entre observaciones 




#Validación train
predicted_value <-0
predicted_value <- predict(logit, type = "response", newdata = train)

predicted_class <- ifelse(predicted_value > 0.4, "Sí", "No")

performace_data <- data.frame(observed= train$TIPOCON, predicted= predicted_class)

#Construcción Matriz de Confusión
positive <- sum(performace_data$observed=="Temporal")
negative <- sum(performace_data$observed=="Indefinido")
predicted_positive <- sum(performace_data$predicted=="Sí")
predicted_negative <- sum(performace_data$predicted=="No")
total <- nrow(performace_data)
data.frame(positive, negative, predicted_positive, predicted_negative)

clasificado1 <- 1*(predicted_value>=0.4)
tabla1 <- table(train$TIPOCON, clasificado1, dnn = c("observaciones", "predicciones"))
tabla1

mosaic(tabla1, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

tp= sum(performace_data$observed=="Temporal" & performace_data$predicted=="Sí")
tn= sum(performace_data$observed=="Indefinido" & performace_data$predicted=="No")
fp= sum(performace_data$observed=="Indefinido" & performace_data$predicted=="Sí")
fn= sum(performace_data$observed=="Temporal" & performace_data$predicted=="No")
data.frame(tp, tn, fp, fn)

exactitud = (tp+tn)/total
tasa_error= (fp+fn)/total
sensibilidad= tp/positive
especifidad= tn/negative
precision= tp/predicted_positive
vpn= tn/predicted_negative

data.frame(exactitud, tasa_error, sensibilidad, especifidad, precision, vpn)






#Validación test
predicted_value <-0
predicted_value <- predict(logit, type = "response", newdata = test)

predicted_class <- ifelse(predicted_value > 0.4, "Sí", "No")

performace_data <- data.frame(observed= test$TIPOCON, predicted= predicted_class)

#Construcción Matriz de Confusión
positive <- sum(performace_data$observed=="Temporal")
negative <- sum(performace_data$observed=="Indefinido")
predicted_positive <- sum(performace_data$predicted=="Sí")
predicted_negative <- sum(performace_data$predicted=="No")
total <- nrow(performace_data)
data.frame(positive, negative, predicted_positive, predicted_negative)

clasificado1 <- 1*(predicted_value>=0.4)
tabla1 <- table(test$TIPOCON, clasificado1, dnn = c("observaciones", "predicciones"))
tabla1

mosaic(tabla1, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

tp= sum(performace_data$observed=="Temporal" & performace_data$predicted=="Sí")
tn= sum(performace_data$observed=="Indefinido" & performace_data$predicted=="No")
fp= sum(performace_data$observed=="Indefinido" & performace_data$predicted=="Sí")
fn= sum(performace_data$observed=="Temporal" & performace_data$predicted=="No")
data.frame(tp, tn, fp, fn)

exactitud = (tp+tn)/total
tasa_error= (fp+fn)/total
sensibilidad= tp/positive
especifidad= tn/negative
precision= tp/predicted_positive
vpn= tn/predicted_negative

data.frame(exactitud, tasa_error, sensibilidad, especifidad, precision, vpn)












#Meto tipo Jornada

logit2 <- glm(TIPOCON ~ SEXO + ANOS2 + ESTU  + ANOANTI + TIPOJOR + CNO1, data = train,
              family = binomial())

summary(logit2)



logit2 <- glm(TIPOCON ~ SEXO + ANOS2 + ESTU2  + ANOANTI + TIPOJOR + CNO1, data = train,
              family = binomial())

summary(logit2)



#Sexo es no significativa, la elimino

logit2 <- glm(TIPOCON ~ ANOS22 + ESTU2 + ANOANTI + TIPOJOR + CNO1, data = train,
              family = binomial())

summary(logit2)


confint(object = logit, level = 0.95 )

# Diferencia de residuos
dif_residuos <- logit2$null.deviance - logit2$deviance

# Grados libertad
df <- logit2$df.null - logit2$df.residual
# p-value
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)

paste("Diferencia de residuos:", round(dif_residuos, 4))
paste("Grados de libertad:", df)
paste("p-value:", round(p_value, 4))



#Evaluación de supuestos en regresioón logística#

#Multicolinealidad
vif(logit2)


#Linealidad
logodds <- logit2$linear.predictor
loganti <- data.frame(logodds, A.Anti = train$ANOANTI)
ggplot(data = loganti, aes(x = A.Anti, y = logodds)) + 
  geom_point() +
  ggtitle(" ") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



#Independencia de las observaciones
Indice <- seq(1,151669,1)
Residuales <- logit2$residuals
residuales <- data.frame(Indice, Residuales)
ggplot(data = residuales, aes(x = Indice, y = Residuales)) +
  geom_point() +
  ggtitle(" ") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))




#Validación train
predicted_value <-0
predicted_value <- predict(logit2, type = "response", newdata = train)

predicted_class <- ifelse(predicted_value > 0.4, "Sí", "No")

performace_data <- data.frame(observed= train$TIPOCON, predicted= predicted_class)

#Construcción Matriz de Confusión
positive <- sum(performace_data$observed=="Temporal")
negative <- sum(performace_data$observed=="Indefinido")
predicted_positive <- sum(performace_data$predicted=="Sí")
predicted_negative <- sum(performace_data$predicted=="No")
total <- nrow(performace_data)
data.frame(positive, negative, predicted_positive, predicted_negative)

clasificado1 <- 1*(predicted_value>=0.4)
tabla1 <- table(train$TIPOCON, clasificado1, dnn = c("observaciones", "predicciones"))
tabla1

mosaic(tabla1, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

tp= sum(performace_data$observed=="Temporal" & performace_data$predicted=="Sí")
tn= sum(performace_data$observed=="Indefinido" & performace_data$predicted=="No")
fp= sum(performace_data$observed=="Indefinido" & performace_data$predicted=="Sí")
fn= sum(performace_data$observed=="Temporal" & performace_data$predicted=="No")
data.frame(tp, tn, fp, fn)

exactitud = (tp+tn)/total
tasa_error= (fp+fn)/total
sensibilidad= tp/positive
especifidad= tn/negative
precision= tp/predicted_positive
vpn= tn/predicted_negative

data.frame(exactitud, tasa_error, sensibilidad, especifidad, precision, vpn)






#Validación test
predicted_value <-0
predicted_value <- predict(logit2, type = "response", newdata = test)

predicted_class <- ifelse(predicted_value > 0.4, "Sí", "No")

performace_data <- data.frame(observed= test$TIPOCON, predicted= predicted_class)

#Construcción Matriz de Confusión
positive <- sum(performace_data$observed=="Temporal")
negative <- sum(performace_data$observed=="Indefinido")
predicted_positive <- sum(performace_data$predicted=="Sí")
predicted_negative <- sum(performace_data$predicted=="No")
total <- nrow(performace_data)
data.frame(positive, negative, predicted_positive, predicted_negative)

clasificado1 <- 1*(predicted_value>=0.4)
tabla1 <- table(test$TIPOCON, clasificado1, dnn = c("observaciones", "predicciones"))
tabla1

mosaic(tabla1, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

tp= sum(performace_data$observed=="Temporal" & performace_data$predicted=="Sí")
tn= sum(performace_data$observed=="Indefinido" & performace_data$predicted=="No")
fp= sum(performace_data$observed=="Indefinido" & performace_data$predicted=="Sí")
fn= sum(performace_data$observed=="Temporal" & performace_data$predicted=="No")
data.frame(tp, tn, fp, fn)

exactitud = (tp+tn)/total
tasa_error= (fp+fn)/total
sensibilidad= tp/positive
especifidad= tn/negative
precision= tp/predicted_positive
vpn= tn/predicted_negative

data.frame(exactitud, tasa_error, sensibilidad, especifidad, precision, vpn)












#TIPO JORNADA#

#veo la distibución para mi variable Tipo de Contrato
dg %>%
  ggplot(aes(x = TIPOJOR)) +
  geom_bar() +
  labs(x = "Tipo de Jornada",
       y = "Número de observaciones")


tabla <- table(dg$TIPOJOR, dg$SEXO,
               dnn = c("Tipo de jornada","sexo"))
addmargins(tabla)

plot(tabla, col = c("#FF7256", "#A2CD5A"), main = " ")
#Las mujeres sufren, claramente, más parcialidad.

#Prueba Chi-cuadrado para examinar relación
#H0: No existe relación entre variables
chisq.test(tabla)
#Rechazamos H0, exite relación entre sexo y tipo de contrato

contrasts(dg$TIPOJOR)#Toma de referencia Completo. Mi éxito es Parcial


logit_sexo2 <- glm(TIPOJOR ~ SEXO, data = train, family = "binomial")

summary(logit_sexo2)


confint(object = logit_sexo2, level = 0.95 )

# Diferencia de residuos
dif_residuos <- logit_sexo2$null.deviance - logit_sexo2$deviance

# Grados libertad
df <- logit_sexo2$df.null - logit_sexo2$df.residual
# p-value
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)

paste("Diferencia de residuos:", round(dif_residuos, 4))
paste("Grados de libertad:", df)
paste("p-value:", round(p_value, 4))




#Validación train
predicted_value <-0
predicted_value <- predict(logit_sexo2, type = "response", newdata = train)

predicted_class <- ifelse(predicted_value > 0.4, "Sí", "No")

performace_data <- data.frame(observed= train$TIPOJOR, predicted= predicted_class)

#Construcción Matriz de Confusión
positive <- sum(performace_data$observed=="Parcial")
negative <- sum(performace_data$observed=="Completo")
predicted_positive <- sum(performace_data$predicted=="Sí")
predicted_negative <- sum(performace_data$predicted=="No")
total <- nrow(performace_data)
data.frame(positive, negative, predicted_positive, predicted_negative)

clasificado1 <- 1*(predicted_value>=0.4)
tabla1 <- table(train$TIPOJOR, clasificado1, dnn = c("observaciones", "predicciones"))
tabla1

mosaic(tabla1, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

tp= sum(performace_data$observed=="Parcial" & performace_data$predicted=="Sí")
tn= sum(performace_data$observed=="Completo" & performace_data$predicted=="No")
fp= sum(performace_data$observed=="Completo" & performace_data$predicted=="Sí")
fn= sum(performace_data$observed=="Parcial" & performace_data$predicted=="No")
data.frame(tp, tn, fp, fn)

exactitud = (tp+tn)/total
tasa_error= (fp+fn)/total
sensibilidad= tp/positive
especifidad= tn/negative
precision= tp/predicted_positive
vpn= tn/predicted_negative

data.frame(exactitud, tasa_error, sensibilidad, especifidad, precision, vpn)






#Validación test
predicted_value <-0
predicted_value <- predict(logit_sexo2, type = "response", newdata = test)

predicted_class <- ifelse(predicted_value > 0.4, "Sí", "No")

performace_data <- data.frame(observed= test$TIPOJOR, predicted= predicted_class)

#Construcción Matriz de Confusión
positive <- sum(performace_data$observed=="Parcial")
negative <- sum(performace_data$observed=="Completo")
predicted_positive <- sum(performace_data$predicted=="Sí")
predicted_negative <- sum(performace_data$predicted=="No")
total <- nrow(performace_data)
data.frame(positive, negative, predicted_positive, predicted_negative)

clasificado1 <- 1*(predicted_value>=0.4)
tabla1 <- table(test$TIPOJOR, clasificado1, dnn = c("observaciones", "predicciones"))
tabla1

mosaic(tabla1, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

tp= sum(performace_data$observed=="Parcial" & performace_data$predicted=="Sí")
tn= sum(performace_data$observed=="Completo" & performace_data$predicted=="No")
fp= sum(performace_data$observed=="Completo" & performace_data$predicted=="Sí")
fn= sum(performace_data$observed=="Parcial" & performace_data$predicted=="No")
data.frame(tp, tn, fp, fn)

exactitud = (tp+tn)/total
tasa_error= (fp+fn)/total
sensibilidad= tp/positive
especifidad= tn/negative
precision= tp/predicted_positive
vpn= tn/predicted_negative

data.frame(exactitud, tasa_error, sensibilidad, especifidad, precision, vpn)







# Modelo lineal generalizado estimado por MLE (máxima verosimilitud)

tabla2 <- table(dg$TIPOJOR, dg$ANOS2,
                dnn = c("Tipo de jornada","edad"))
tabla3 <- table(dg$TIPOJOR, dg$ESTU,
                dnn = c("Tipo de jornada","estudios"))
tabla4 <- table(dg$TIPOJOR, dg$NUTS1,
                dnn = c("Tipo de jornada","localización"))
tabla5 <- table(dg$TIPOJOR, dg$CNO2,
                dnn = c("Tipo de jornada","CNO"))
tabla6 <- table(dg$TIPOJOR, dg$CNACE,
                dnn = c("Tipo de jornada","CNAE"))

#Prueba Chi-cuadrado para examinar relación
#H0: No existe relación entre variables
chisq.test(tabla2)
chisq.test(tabla3)
chisq.test(tabla4)
chisq.test(tabla5)
chisq.test(tabla6)


logit3 <- glm(TIPOJOR ~ SEXO + ANOS2 + ESTU + NUTS1+ ANOANTI + CNO2 + CNACE, data = dg,
              family = binomial())

summary(logit3)

vif(logit3)#Elimino CNAE por multicolinealidad con CNO


#MODELO SIN CNAE
logit3 <- glm(TIPOJOR ~ SEXO + ANOS2 + ESTU + NUTS1+ ANOANTI + CNO2 , data = train,
              family = binomial())

summary(logit3)

#Recodifico 
train$NUTS13 <- recode(train$NUTS1, "1=1 ; 2=2 ; 3=3 ; 4=3 ; 5=4 ; 6=6 ; 7=7")
test$NUTS13 <- recode(test$NUTS1, "1=1 ; 2=2 ; 3=3 ; 4=3 ; 5=4 ; 6=6 ; 7=7")
table(train$NUTS13)
table(train$NUTS1)

logit3 <- glm(TIPOJOR ~ SEXO + ANOS2 + ESTU + NUTS13+ ANOANTI + CNO2 , data = train,
              family = binomial())

summary(logit3)

#Elimino la variable geográfica
logit3 <- glm(TIPOJOR ~ SEXO + ANOS2 + ESTU + ANOANTI + CNO2 , data = train,
              family = binomial())

summary(logit3)

train$CNO12 <- as.numeric(factor(train$CNO2, 
                                 levels=c("A0", "B0", "C0",
                                          "D0", "E0", "F0",
                                          "G0", "H0", "I0",
                                          "J0", "K0", "L0",
                                          "M0", "N0", "O0",
                                          "P0", "Q0")))
test$CNO12 <- as.numeric(factor(test$CNO2, 
                                levels=c("A0", "B0", "C0",
                                         "D0", "E0", "F0",
                                         "G0", "H0", "I0",
                                         "J0", "K0", "L0",
                                         "M0", "N0", "O0",
                                         "P0", "Q0")))
table(train$CNO2)
train$CNO12 <- recode(train$CNO12, "1=1 ; 2=2 ; 3=3 ; 4=4 ; 5=5 ; 6=6 ; 
                      7=7 ; 8=8 ; 9=9 ; 10=10 ; 11=13 ; 12=12 ; 13=13 ; 14=13 ; 15=15 ; 16=16 ")
test$CNO12 <- recode(test$CNO12, "1=1 ; 2=2 ; 3=3 ; 4=4 ; 5=5 ; 6=6 ; 
                      7=7 ; 8=8 ; 9=9 ; 10=10 ; 11=13 ; 12=12 ; 13=13 ; 14=13 ; 15=15 ; 16=16 ")

table(train$CNO12)
train$CNO12 <- factor(train$CNO12, labels = c("A0", "B0", "C0",
                                              "D0", "E0", "F0",
                                              "G0", "H0", "I0",
                                              "J0", "L0",
                                              "K0M0N0", "O0","P0"
))
test$CNO12 <- factor(test$CNO12, labels = c("A0", "B0", "C0",
                                            "D0", "E0", "F0",
                                            "G0", "H0", "I0",
                                            "J0", "L0",
                                            "K0M0N0", "O0","P0"
))
table(train$CNO12)



logit3 <- glm(TIPOJOR ~ SEXO  + ANOS2 + ESTU + ANOANTI + CNO12 , data = train,
              family = binomial())

summary(logit3)

confint(object = logit3, level = 0.95 )

# Diferencia de residuos
dif_residuos <- logit3$null.deviance - logit3$deviance

# Grados libertad
df <- logit3$df.null - logit3$df.residual
# p-value
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)

paste("Diferencia de residuos:", round(dif_residuos, 4))
paste("Grados de libertad:", df)
paste("p-value:", round(p_value, 4))




#Evaluación de supuestos en regresioón logística#

#Multicolinealidad
vif(logit3)

#Linealidad
logodds <- logit3$linear.predictor
loganti <- data.frame(logodds, A.Anti = train$ANOANTI)
ggplot(data = loganti, aes(x = A.Anti, y = logodds)) + 
  geom_point() +
  ggtitle(" ") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#La valiable ANOANTI, la única variable numética


#Independencia de las observaciones
Indice <- seq(1,151669,1)
Residuales <- logit3$residuals
residuales <- data.frame(Indice, Residuales)
ggplot(data = residuales, aes(x = Indice, y = Residuales)) +
  geom_point() +
  ggtitle(" ") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))





#Validación train
predicted_value <-0
predicted_value <- predict(logit3, type = "response", newdata = train)

predicted_class <- ifelse(predicted_value > 0.4, "Sí", "No")

performace_data <- data.frame(observed= train$TIPOJOR, predicted= predicted_class)

#Construcción Matriz de Confusión
positive <- sum(performace_data$observed=="Parcial")
negative <- sum(performace_data$observed=="Completo")
predicted_positive <- sum(performace_data$predicted=="Sí")
predicted_negative <- sum(performace_data$predicted=="No")
total <- nrow(performace_data)
data.frame(positive, negative, predicted_positive, predicted_negative)

clasificado1 <- 1*(predicted_value>=0.4)
tabla1 <- table(train$TIPOJOR, clasificado1, dnn = c("observaciones", "predicciones"))
tabla1

mosaic(tabla1, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

tp= sum(performace_data$observed=="Parcial" & performace_data$predicted=="Sí")
tn= sum(performace_data$observed=="Completo" & performace_data$predicted=="No")
fp= sum(performace_data$observed=="Completo" & performace_data$predicted=="Sí")
fn= sum(performace_data$observed=="Parcial" & performace_data$predicted=="No")
data.frame(tp, tn, fp, fn)

exactitud = (tp+tn)/total
tasa_error= (fp+fn)/total
sensibilidad= tp/positive
especifidad= tn/negative
precision= tp/predicted_positive
vpn= tn/predicted_negative

data.frame(exactitud, tasa_error, sensibilidad, especifidad, precision, vpn)






#Validación test
predicted_value <-0
predicted_value <- predict(logit3, type = "response", newdata = test)

predicted_class <- ifelse(predicted_value > 0.4, "Sí", "No")

performace_data <- data.frame(observed= test$TIPOJOR, predicted= predicted_class)

#Construcción Matriz de Confusión
positive <- sum(performace_data$observed=="Parcial")
negative <- sum(performace_data$observed=="Completo")
predicted_positive <- sum(performace_data$predicted=="Sí")
predicted_negative <- sum(performace_data$predicted=="No")
total <- nrow(performace_data)
data.frame(positive, negative, predicted_positive, predicted_negative)

clasificado1 <- 1*(predicted_value>=0.4)
tabla1 <- table(test$TIPOJOR, clasificado1, dnn = c("observaciones", "predicciones"))
tabla1

mosaic(tabla1, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

tp= sum(performace_data$observed=="Parcial" & performace_data$predicted=="Sí")
tn= sum(performace_data$observed=="Completo" & performace_data$predicted=="No")
fp= sum(performace_data$observed=="Completo" & performace_data$predicted=="Sí")
fn= sum(performace_data$observed=="Parcial" & performace_data$predicted=="No")
data.frame(tp, tn, fp, fn)

exactitud = (tp+tn)/total
tasa_error= (fp+fn)/total
sensibilidad= tp/positive
especifidad= tn/negative
precision= tp/predicted_positive
vpn= tn/predicted_negative

data.frame(exactitud, tasa_error, sensibilidad, especifidad, precision, vpn)







#Meto Tipo contrato

logit4 <- glm(TIPOJOR ~  SEXO  + ANOS2 + ESTU + ANOANTI + TIPOCON + CNO2 , data = train,
              family = binomial())

summary(logit4)



logit4 <- glm(TIPOJOR ~  SEXO  + ANOS2 + ESTU + ANOANTI + TIPOCON + CNO12 , data = train,
              family = binomial())

summary(logit4)


# Diferencia de residuos
dif_residuos <- logit4$null.deviance - logit4$deviance

# Grados libertad
df <- logit4$df.null - logit4$df.residual
# p-value
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)

paste("Diferencia de residuos:", round(dif_residuos, 4))
paste("Grados de libertad:", df)
paste("p-value:", round(p_value, 4))



#Evaluación de supuestos en regresioón logística#

#Multicolinealidad
vif(logit4)

#Linealidad
logodds <- logit4$linear.predictor
loganti <- data.frame(logodds, A.Anti = train$ANOANTI)
ggplot(data = loganti, aes(x = A.Anti, y = logodds)) + 
  geom_point() +
  ggtitle(" ") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#La valiable ANOANTI, la única variable numética


#Independencia de las observaciones
Indice <- seq(1,151669,1)
Residuales <- logit4$residuals
residuales <- data.frame(Indice, Residuales)
ggplot(data = residuales, aes(x = Indice, y = Residuales)) +
  geom_point() +
  ggtitle(" ") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



#Validación train
predicted_value <-0
predicted_value <- predict(logit4, type = "response", newdata = train)

predicted_class <- ifelse(predicted_value > 0.4, "Sí", "No")

performace_data <- data.frame(observed= train$TIPOJOR, predicted= predicted_class)

#Construcción Matriz de Confusión
positive <- sum(performace_data$observed=="Parcial")
negative <- sum(performace_data$observed=="Completo")
predicted_positive <- sum(performace_data$predicted=="Sí")
predicted_negative <- sum(performace_data$predicted=="No")
total <- nrow(performace_data)
data.frame(positive, negative, predicted_positive, predicted_negative)

clasificado1 <- 1*(predicted_value>=0.4)
tabla1 <- table(train$TIPOJOR, clasificado1, dnn = c("observaciones", "predicciones"))
tabla1

mosaic(tabla1, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

tp= sum(performace_data$observed=="Parcial" & performace_data$predicted=="Sí")
tn= sum(performace_data$observed=="Completo" & performace_data$predicted=="No")
fp= sum(performace_data$observed=="Completo" & performace_data$predicted=="Sí")
fn= sum(performace_data$observed=="Parcial" & performace_data$predicted=="No")
data.frame(tp, tn, fp, fn)

exactitud = (tp+tn)/total
tasa_error= (fp+fn)/total
sensibilidad= tp/positive
especifidad= tn/negative
precision= tp/predicted_positive
vpn= tn/predicted_negative

data.frame(exactitud, tasa_error, sensibilidad, especifidad, precision, vpn)






#Validación test
predicted_value <-0
predicted_value <- predict(logit4, type = "response", newdata = test)

predicted_class <- ifelse(predicted_value > 0.4, "Sí", "No")

performace_data <- data.frame(observed= test$TIPOJOR, predicted= predicted_class)

#Construcción Matriz de Confusión
positive <- sum(performace_data$observed=="Parcial")
negative <- sum(performace_data$observed=="Completo")
predicted_positive <- sum(performace_data$predicted=="Sí")
predicted_negative <- sum(performace_data$predicted=="No")
total <- nrow(performace_data)
data.frame(positive, negative, predicted_positive, predicted_negative)

clasificado1 <- 1*(predicted_value>=0.4)
tabla1 <- table(test$TIPOJOR, clasificado1, dnn = c("observaciones", "predicciones"))
tabla1

mosaic(tabla1, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

tp= sum(performace_data$observed=="Parcial" & performace_data$predicted=="Sí")
tn= sum(performace_data$observed=="Completo" & performace_data$predicted=="No")
fp= sum(performace_data$observed=="Completo" & performace_data$predicted=="Sí")
fn= sum(performace_data$observed=="Parcial" & performace_data$predicted=="No")
data.frame(tp, tn, fp, fn)

exactitud = (tp+tn)/total
tasa_error= (fp+fn)/total
sensibilidad= tp/positive
especifidad= tn/negative
precision= tp/predicted_positive
vpn= tn/predicted_negative

data.frame(exactitud, tasa_error, sensibilidad, especifidad, precision, vpn)
