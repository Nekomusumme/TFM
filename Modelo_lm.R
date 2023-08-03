#MODELO REGRESION LINEAL#

#Librerias utilizadas

library(readr)
library(dplyr)
library(tidyr)
library(tseries) #Carga los comandos para las pruebas de normalidad
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


EES_2018 <- read_delim("/CSV/EES_2018.csv", #Ruta del archivo
                       delim = "\t", escape_double = FALSE, trim_ws = TRUE)

df = EES_2018 %>% select(ORDENTRA, SALBASE, JSP1, SEXO, ANOS2, TIPOPAIS, ESTU, ANOANTI, TIPOJOR, TIPOCON, CNO1, CNACE, NUTS1, FACTOTAL)
names (df)

df$ANOS2 <- as.numeric(factor(df$ANOS2, 
                              levels=c("01", "02", "03",
                                       "04", "05", "06")))

df<-mutate(df,
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
df
attach(df)

df$SEXO <- factor(df$SEXO, labels = c("Hombre", "Mujer"))
table(df$SEXO)

hist(df$SALBASE, main = "Histograma de frecuencias", # Frecuencia
     ylab = "Frecuencia", xlab = "Salario base")

ggboxplot(data = df, x= "SEXO", y= "SALBASE" ,
          fill = "SEXO", palette = c("#1C86EE", "#EE2C2C"))




#MODELOS SIN TRANSFORMAR

mod_lm <- lm(SALBASE ~ SEXO + ANOS2 + ESTU + NUTS1, data=df, weights = (df$FACTOTAL))
summary(mod_lm)

mod_lm2 <- lm(SALBASE~ SEXO + ANOS2 + ESTU + NUTS1 + TIPOJOR + TIPOCON + CNO1 + CNACE, data=df, weights = (df$FACTOTAL))
summary(mod_lm2)


#Multicolinealidad
VIF(mod_lm2) #Existe multicolinealidad entre CNO y CNAE


mod_lm2 <- lm(SALBASE~ SEXO + ANOS2 + ESTU + NUTS1 + TIPOJOR + TIPOCON + CNO1, data=df, weights = (df$FACTOTAL))
summary(mod_lm2)


#Recodifico nuts
df$NUTS12 <- recode(df$NUTS1, "1=1 ; 2=2 ; 3=3 ; 4=4 ; 5=4 ; 6=6 ; 7=6")

mod_lm2 <- lm(SALBASE~ SEXO + ANOS2 + ESTU + NUTS12 + TIPOJOR + TIPOCON + CNO1, data=df, weights = (df$FACTOTAL))
summary(mod_lm2)


#Elimino Nuts
mod_lm2 <- lm(SALBASE~ SEXO + ANOS2 + ESTU  + TIPOJOR + TIPOCON + CNO1, data=df, weights = (df$FACTOTAL))
summary(mod_lm2)





#Validación de supuestos#

#Linealidad
plot(mod_lm, 1)
plot(mod_lm2, 1)
#Los modelos son lineales


#Normalidad de los residuos
plot(mod_lm,2)
jarque.bera.test(mod_lm$residual)
plot(mod_lm2,2)
jarque.bera.test(mod_lm2$residual)
#no hay normalidad en los residuos, hay que transformar variables


#Homocedasticidad en residuos
ncvTest(mod_lm) 
ncvTest(mod_lm2)
#Hay heteroceasticidad, hay que transformar variables


#Multicolinealidad
VIF(mod_lm) 
VIF(mod_lm2)


#Observaciones influyentes
plot(mod_lm,4)
plot(mod_lm2,4)



#Se eliminan valores extremos
df <- df[df$SALBASE<5000, ]
max(df$SALBASE)
df <- df[df$SALBASE>500, ]
min(df$SALBASE)

#Transformamos en logaritmo

df$log_SALBASE <-log(df$SALBASE)


hist(df$log_SALBASE, main = "Histograma de frecuencias", # Frecuencia
     ylab = "Frecuencia", xlab = "Salario base")
max(df$log_SALBASE)


ggboxplot(data = df, x= "SEXO", y= "log_SALBASE" ,
          fill = "SEXO", palette = c("#1C86EE", "#EE2C2C"))



#MODELOS TRANSFORMADOS

mod_lm <- lm(log_SALBASE ~ SEXO + ANOS2 + ESTU + NUTS1, data=df, weights = (df$FACTOTAL))
summary(mod_lm)

mod_lm2 <- lm(log_SALBASE~ SEXO + ANOS2 + ESTU + NUTS1 + TIPOJOR + TIPOCON + CNO1 + CNACE, data=df, weights = (df$FACTOTAL))
summary(mod_lm2)


#Multicolinealidad
VIF(mod_lm2) #Existe multicolinealidad entre CNO y CNAE


mod_lm2 <- lm(log_SALBASE~ SEXO + ANOS2 + ESTU + NUTS1 + TIPOJOR + TIPOCON + CNO1, data=df, weights = (df$FACTOTAL))
summary(mod_lm2)


#Recodifico nuts
df$NUTS12 <- recode(df$NUTS1, "1=1 ; 2=2 ; 3=3 ; 4=4 ; 5=4 ; 6=6 ; 7=6")

mod_lm2 <- lm(log_SALBASE~ SEXO + ANOS2 + ESTU + NUTS12 + TIPOJOR + TIPOCON + CNO1, data=df, weights = (df$FACTOTAL))
summary(mod_lm2)


#Elimino Nuts
mod_lm2 <- lm(log_SALBASE~ SEXO + ANOS2 + ESTU  + TIPOJOR + TIPOCON + CNO1, data=df, weights = (df$FACTOTAL))
summary(mod_lm2)




#Validación de supuestos#

#Linealidad
plot(mod_lm, 1)
plot(mod_lm2, 1)
#Los modelos son lineales


#Normalidad de los residuos
plot(mod_lm,2)
jarque.bera.test(mod_lm$residual)
plot(mod_lm2,2)
jarque.bera.test(mod_lm2$residual)
#no hay normalidad en los residuos


#Homocedasticidad en residuos
ncvTest(mod_lm) 
ncvTest(mod_lm2)
#Hay heteroceasticidad


#Multicolinealidad
VIF(mod_lm) 
VIF(mod_lm2)


#Observaciones influyentes
plot(mod_lm,4)
plot(mod_lm2,4)
