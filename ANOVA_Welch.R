#ANOVA DE WELCH#

#Librerias utilizadas

library(readr)
library(dplyr)
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

EES_2018 <- read_delim("E/CSV/EES_2018.csv", #Ruta del archivo
                       delim = "\t", escape_double = FALSE, trim_ws = TRUE)

df = EES_2018 %>% select(ORDENTRA, SALBASE, JSP1, SEXO, ANOS2, TIPOPAIS, ESTU, ANOANTI, TIPOJOR, TIPOCON, CNO1, CNACE, NUTS1, FACTOTAL)
names (df)

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

df <- df[df$SALBASE<5000, ]
max(df$SALBASE)
df <- df[df$SALBASE>500, ]
min(df$SALBASE)

df$SEXO <- factor(df$SEXO, labels = c("Hombre", "Mujer"))
table(df$SEXO)

df %>% 
  group_by(SEXO) %>% 
  get_summary_stats(SALBASE, type = "mean_sd")
#Observo un mayor número de hombres 115.524, fente a las 84.838 mujeres de la muestra
#Las mujeres obtienen un menor salrio en media 1.308, frente a los 1.473 de los hombres
#Los hombres son los que presentan mayor desviación 724, frente a los 670 de las mujeres



#SUPUESTOS PARA PRUEBAS PARAMÉTRICAS#


#-NORMALIDAD-#

hist(df$SALBASE, main = "Histograma de frecuencias", # Frecuencia
     ylab = "Frecuencia", xlab = "Salario base")

#Como la distribución del salario no es normal, aplico logaritmo para suavizar

df$log_SALBASE <-log(df$SALBASE)
df$log_ANOANTI<-log(df$ANOANTI)#Por si acaso lo uso

#Se observa una distribución aproximádamente normal. 
hist(df$log_SALBASE, main = "Histograma de frecuencias", # Frecuencia
     ylab = "Frecuencia", xlab = "Logaritmo del salario base")

#Test de normalidad
jarque.bera.test(df$log_SALBASE)

normalidad <- lm(log_SALBASE ~ SEXO, data = df)
ggqqplot(residuals(normalidad))
jarque.bera.test(normalidad$residual)
#Sigo rechazando normalidad estadísticamente hablando
#Pero mi muestra es superior a los 200.000 registros, por lo que me acojo al Teorema Central del Límite
#Aún así, debo tenerlo en cuenta para las conclusiones del análisis.

#De la misma forma, la distribución del salario intra-grupo, es aproximádamente normal
A <- df %>% ggplot(aes(as.numeric(log_SALBASE))) + 
  geom_histogram(data = subset(df,SEXO=="Mujer"),fill="#EE2C2C", colour= "black")+
  labs(x="Salario",y="Frecuencia") 

A

B <- df %>% ggplot(aes(as.numeric(log_SALBASE))) + 
  geom_histogram(data = subset(df,SEXO=="Hombre"),fill="#1C86EE", colour= "black") +
  labs(x="Salario",y="Frecuencia")

B

#-NO OUTIERS-#

df %>% 
  group_by(SEXO) %>% 
  identify_outliers(log_SALBASE)
#Como mi distribución no es 100% simétrica, detecta agunos. Sin embargo, no son outliars extremos, por lo que puedo trabajar con ellos


#-HOMOGENEIDAD DE VARIANZAS-#
leveneTest(SALBASE ~ SEXO, data = df, center = "median")
df %>% 
  levene_test(log_SALBASE  ~ SEXO)
#Existe heterogeneidad de varianzas entre, por lo que no podemos utilizar un ANOVA paramétrico


boxplot_pre <- ggboxplot(data = df, x= "SEXO", y= "SALBASE" ,
                         fill = "SEXO", palette = c("#1C86EE", "#EE2C2C"))
boxplot_pre

boxplot <- ggboxplot(data = df, x= "SEXO", y= "log_SALBASE" ,
                     fill = "SEXO", palette = c("#1C86EE", "#EE2C2C"))
boxplot

#con la transformación log se ha suvizado también la dispersión de los datos
#No contamos con tantos outliers, ni son tan extremos

#-MUESTRAS INDEPENDIENTES-#
#La muestra procede de la Encuesta de Estructura Salarial del INE
#Se acepta la hipótesis de que dicha muestra es aleatoria, por la naturaleza de los propios datos
#Por lo que las observaciones deben ser independientes


#ANOVA de Welch. OPCIÓN NO PARAMÉTRICA#

modelo1 <- df %>% welch_anova_test(log_SALBASE  ~ SEXO)#H0: NO HAY DIFERENCIAS EN SALARIO / H1: SÍ HAY DIFERENCIAS
modelo1

#Existen diferencias significativas en el salario entre sexos

#Comprobación post hoc con la prueba Games - Howell
comp_mult <- df %>% games_howell_test(log_SALBASE ~ SEXO)
comp_mult
#La diferencia es significativa. 
#Pero la interpretación se complica. Lo único que puedo asegurar es que la categoría mujer afecta negativamente al salario

#Visualización 
comp_mult <- comp_mult %>% add_xy_position(x = "SEXO")

boxplot +
  stat_pvalue_manual(comp_mult, hide.ns = TRUE)+
  labs(subtitle = get_test_label(modelo1, detailed = TRUE),
       caption = get_pwc_label(comp_mult))
