*MODELO REGRESION CUANTILICA

*La base de datos procede de la carpeta de STATA, procedente del EES
* La ruta es la siguiente: ESS INE\STATA\EES_2018.dta
help qplot
help grqreg

*Elimino las variables que son 0 para aplicar transformación BOX-COX (en este caso log natural)
drop if salbase<10
gen log_salbase = log(salbase)

 
*Reestructuro las variables para convertirlas en tipo numético
*En stata se indica que una variable es de tipo categórico con un "i." delante del nombre de la variable. Por lo que debe ser transformada en numérica en el caso de que se encuentre codificada como str2
destring sexo, replace
destring estu , replace
destring anos2 , replace
destring tipojor,  replace
destring tipocon,  replace
encode cnace, generate(cnace2)
encode cno1, generate(cno2)

*Añado etiquetas a mis categorías
label define sexlvl 1 "Hombre" 6 "Mujer"
label define tipojorlvl 1 "T.Completo" 2 "T.Parcial"
label define tipoconlvl 1 "Indefinido" 2 "Temporal"
label define estulvl 1 "Sin estudios" 2 "Priamria" 3 "1ª Etapa Secundaria" 4 "2ª Etapa Secundaria" 5 "Formación Profesional" 6 "Universitarios" 7 "Licenciados y Doctores"
label define anos2lvl 1 "Menos de 19" 2 "De 20 a 29" 3 "De 30 a 39" 4 "De 40 a 49" 5 "De 50 a 59" 6 "Más de 59"

label values sexo sexlvl
label values tipojor tipojorlvl
label values tipocon tipoconlvl
label values estu estulvl
label values anos2 anos2lvl


*He escogido el método de boostrap para la regresión de cuantiles. Es el más general y es el más indicado cuando hay presencia de heterocedastididad, como en este caso.
bsqreg log_salbase ib(1).sexo ib(1).tipojor ib(1).tipocon ib(1).estu ib(1).anos2 i.cnace2 i.cno2, quantile(25) reps(20)
*En este priemr modelo he utilizado todas las variables que he considerado relevantes en mi estudio descriptivo. No obstante, STATA lanza el siguiente mensaje:
*convergence not achieved
*Lo que significa que no consigue modelar con las variables escogidas. Como he utilizado un análisis de tanteamiento en R, y la máxima agrupación psible para el CNAE (de forma coherene), sigue no convergiendo, puedo suponer que la variable CNAE no es significativa en el modelo y por lo tanto, voy a intentar modelizar sin ella

bsqreg log_salbase ib(1).sexo ib(1).anos2 ib(1).estu ib(1).tipojor ib(1).tipocon i.cno, quantile(25) reps(20)
*convergence not achieved
*recodifico el cno para favorecer la convergencia del modelo
recode cno2 (1 = 1) (2 3 = 2) (4 = 3) (5 6 = 4) (7 8 9 = 5) (10 11 12 = 6) (13 14 = 7) (15 16 = 8) (17 =9), generate(new_cno2)

bsqreg log_salbase ib(1).sexo ib(1).anos2 ib(1).estu ib(1).nuts1 ib(1).tipojor ib(1).tipocon i.new_cno2, quantile(20) reps(20)
*convergence not achieved.
bsqreg log_salbase ib(1).sexo ib(1).anos2 ib(1).estu ib(1).tipojor ib(1).tipocon i.new_cno2, quantile(20) reps(20)


*El único supuesto que debe cumplirse obligatoriamente en este tipo de regresión es la no  multicolinealidad. Para medirlo, utilizaré el VIF no centrado, que es el adecuado para este tipo de regresión. Además, toma en cuenta la exitencia de la constante como un regresor más, para evitar que sufra multicolinealidad con alguna de las variables independientes. Si el valor del VIF supera el 10, significará que existe multicolinealidad entre variables.
vif, uncentered

*La bariable de edad y la de nivel de estudios son codependientes. Elijo eliminar la variable edad, la variable nivel de estudios es más significativa para explicar el salario.
bsqreg log_salbase ib(1).sexo ib(1).estu ib(1).tipojor ib(1).tipocon i.new_cno2, quantile(20) reps(20)
bsqreg log_salbase ib(1).sexo ib(1).estu ib(1).tipojor ib(1).tipocon i.new_cno2, quantile(40) reps(20)
bsqreg log_salbase ib(1).sexo ib(1).estu ib(1).tipojor ib(1).tipocon i.new_cno2, quantile(60) reps(20)
bsqreg log_salbase ib(1).sexo ib(1).estu ib(1).tipojor ib(1).tipocon i.new_cno2, quantile(80) reps(20)

*En la regresión exploratoria me doy cuenta de que el nivel 9 de cno, repercute muy negativamente. Me extraña, porque son los militares. Haciendo una tabla de frecuencia compruebo que apenas son 51, por lo que no lo considero un nivel significativamnte representativo de su población, para este tipo de regresión. Y no tendría sentido agrupara en otra categoría, por lo especial de sus características. Elimino el nivel.
tab new_cno2
drop if new_cno2==9

bsqreg log_salbase ib(1).sexo ib(1).estu ib(1).tipojor ib(1).tipocon i.new_cno2, quantile(20) reps(20)
bsqreg log_salbase ib(1).sexo ib(1).estu ib(1).tipojor ib(1).tipocon i.new_cno2, quantile(40) reps(20)
bsqreg log_salbase ib(1).sexo ib(1).estu ib(1).tipojor ib(1).tipocon i.new_cno2, quantile(60) reps(20)
bsqreg log_salbase ib(1).sexo ib(1).estu ib(1).tipojor ib(1).tipocon i.new_cno2, quantile(80) reps(20)


destring nuts1, replace
label define nuts1lvl 1 "Noroeste" 2 "Noreste" 3 "C. Madrid" 4 "Centro" 5 "Este" 6 "Sur" 7 "Canarias"
label values nuts1 nuts1lvl

bsqreg log_salbase ib(1).sexo ib(1).estu ib(1).nuts1 ib(1).tipojor ib(1).tipocon i.new_cno2, quantile(20) reps(20)
tab nuts1
tab nuts1, nolab

*Mdrid sale no significativa, voy a recodificar y alinearla con Centro
recode nuts1 (1 = 1) (2 = 2) (3 4 = 3) (5 = 4) (6 = 5) (7 = 6), generate(new_nuts1)
label define nuts1lvl2 1 "Noroeste" 2 "Noreste" 3 "Centro" 4 "Este" 5 "Sur" 6 "Canarias"
label values new_nuts1 nuts1lvl2
tab new_nuts1
tab new_nuts1, nolab

bsqreg log_salbase ib(1).sexo ib(1).estu ib(1).new_nuts1 ib(1).tipojor ib(1).tipocon i.new_cno2, quantile(20) reps(20)

*Centro + Madrid ya es significativo. Pero Sur sigue sin serlo. Juntaremos canarias y sur.
recode new_nuts1 (5 6 = 6), generate(new2_nuts1)
label define nuts1lvl22 1 "Noroeste" 2 "Noreste" 3 "Centro" 4 "Este" 6 "Sur"
label values new2_nuts1 nuts1lvl22
bsqreg log_salbase ib(1).sexo ib(1).estu ib(1).new2_nuts1 ib(1).tipojor ib(1).tipocon i.new_cno2, quantile(20) reps(20)

vif, uncentered
*El modelo es significativo para todas las variables y categorías. Tampoco existe multicolinealidad, por lo que se procede a repetir el proceso para el resto de quintiles.

bsqreg log_salbase ib(1).sexo ib(1).estu ib(1).new2_nuts1 ib(1).tipojor ib(1).tipocon i.new_cno2, quantile(40) reps(20)
bsqreg log_salbase ib(1).sexo ib(1).estu ib(1).new2_nuts1 ib(1).tipojor ib(1).tipocon i.new_cno2, quantile(60) reps(20)
bsqreg log_salbase ib(1).sexo ib(1).estu ib(1).new2_nuts1 ib(1).tipojor ib(1).tipocon i.new_cno2, quantile(80) reps(20)
