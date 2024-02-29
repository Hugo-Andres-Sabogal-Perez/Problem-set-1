#### Script problem set 1 ######

#Realizamos inicialmente una limpieza del entorno
rm(list = ls())

#Llamamos las librerías necesarias para la realización del trabajo 
require(pacman)
require(tidyverse)
require(rvest)
require(stargazer)
require(rio)
require(caret)
require(gridExtra)
require(skimr)
require(boot)
require(tidytable)

### Función para importar datos
importar_datos<-function(){
  
  ##Llamamos el link para el web scraping
  link_incompleto<-'https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_' 
  
  ##Invocamos la tabla de la primera pagina como dataframe (datos) concatenando strings 
  datos<-read_html(paste0(link_incompleto, 'page_',as.character(1),'.html'))  %>% 
    html_table() %>% as.data.frame()
  
  #Relizamos un ciclo para añadir las filas de la página 2 a la 10 de a nuestro data set
  for (i in 2:10){
    html<-read_html(paste0(link_incompleto, 'page_',as.character(i),'.html'))  %>% 
          html_table() %>% as.data.frame()
    datos<-rbind(datos,html)
    
  }
  # Selección de la muestra de interés: edad >= 18 y empleado (ocu).
  datos <- datos[datos$age >= 18 & datos$ocu == 1,] #Se utiliza la variable de ocupados dado que según su definición corresponde a las personas que tienen un trabajo formal, realizan actividades independientes y han trabajado durante la semana de referencia.
  
  #retornamos el data set final
  return(datos)
}

DF<-importar_datos() 

#Exportamos los datos deaspués del web scraping en una base de datos llamada DF.cvs para que el código sea más eficiente
write.csv(x = DF, file = "Stores/DF.csv", row.names = FALSE) 

#DESDE AQUÍ SE PUEDE COMENZAR EL SCIRPT UNA VEZ REALIZADO EL WEB SCRAPING Y YA TENIEDO EL ARCHIVO DE LA BASE GUARDADO
#Llamamos nuestra base de datos 

DF<-import("Stores/DF.csv")

# Se crea una base para guardar las estadisticas descriptivas más relevantes para el trabajo:
vars = length(colnames(DF))
ED = data.frame('Variable' = colnames(DF), 'Missings' = rep(NA, vars), 'Media' =  rep(NA, vars), 'Desviacion Estandard' = rep(NA, vars))

# Se cuentan los missings y se calcula la media y la desviación estándar de la muestra:
for(col in colnames(DF)){
  df = DF[,colnames(DF) == col]
  NAs = sum(is.na(df))
  mean = mean(df, na.rm = T)
  sd = sqrt(var(df, na.rm = T))
  
  ED[ED$Variable == col, 2] = NAs
  ED[ED$Variable == col, 3] = mean
  ED[ED$Variable == col, 4] = sd
}

# 1. Limpieza de datos:
# Se eliminan las constantes (u observaciones que tienen desviación estándar igual a cero) y las variables sin observaciones (missings):
C = ED %>% filter(Desviacion.Estandard == 0 | is.na(Desviacion.Estandard)) %>% select(Variable) %>% as.vector()
ED = ED %>% filter(Desviacion.Estandard != 0 | !is.na(Desviacion.Estandard))
DF = DF[!is.na(DF$y_ingLab_m_ha),]
DF = DF %>% select(-C$Variable)
DF = DF[DF$age!=78,]
DF=DF[!(rownames(DF)%in%c("5733", "579")),]

# 2. Estadisticas descriptivas:
base= DF %>% select(age,oficio, formal, maxEducLevel, orden, p7040, sex, sizeFirm, y_ingLab_m_ha, hoursWorkUsual)
base$ln_sal = log(base$y_ingLab_m_ha) #Se crea el logaritmo del salario por horas para normalizar los valores de la variable.
stargazer(base, type= "text", summary=T, title = "Estadisticas Descriptivas",out = "Views/esta_des.txt")


#Gráficas relevantes para las estadísticas descriptivas
#1. Histograma de la variable Y: salarios por horas
histograma_salario <- ggplot(base, aes(x=y_ingLab_m_ha)) + 
  geom_histogram(color="white",fill="darkblue") + 
  xlab('Salario por hora') + ylab('Frecuencia') + 
  theme_bw() 
histograma_salario

ggsave("Views/histograma_sal.png", width = 6, height = 4,plot=histograma_salario)

#2. Histograma de la variable Y: log del salario por hora (transformación)
histograma <- ggplot(base, aes(x=ln_sal)) + 
              geom_histogram(color="white",fill="darkblue") + 
              xlab('Logaritmo del salario por hora') + ylab('Frecuencia') + 
              theme_bw() 
histograma

ggsave("Views/histograma.png", width = 6, height = 4,plot=histograma)

#3. Gráfica de Dispersión: Edad vs. Logaritmo del Salario por hora
# El ln(w) es relativamente homocedastico sobre la edad.
dispersion = ggplot(base, aes(x=age, y=ln_sal)) + geom_point(color="navy") + 
             theme_bw() +
             geom_smooth(method = 'lm',color="firebrick") +xlab('Edad')+ 
             ylab('Logaritmo del salario por hora')
dispersion

ggsave("Views/dispersion.png", width = 6, height = 4,plot=dispersion)


#4. Gráfico de Barras: Sexo Vs. Salario Promedio
base$sex_factor <- factor(base$sex, levels = c(1,0),
                          labels = c('Masculino', 'Femenino'))
Salario_sex <- base %>% group_by(sex_factor)  %>% 
  summarize(mean_sal_sex=mean(y_ingLab_m_ha))
barras1 <- ggplot(Salario_sex, aes(x = sex_factor, y = mean_sal_sex)) +
  geom_bar(width = 0.5, colour = "skyblue", fill = "skyblue", stat = "identity") +
  labs(x = "Sexo", y = "Log del Salario por hora") +
  theme_bw() +
  scale_y_continuous(labels = scales::dollar_format()) 
barras1
ggsave("Views/barras1.png", width = 6, height = 4,plot=barras1)

#6. Gráfico de Barras: Edad vs. Salario Promedio

Edad <-base %>% group_by(age) %>% 
              summarize(mean_sal=mean(y_ingLab_m_ha))

barras2 <- ggplot(Edad, aes(x = age, y = mean_sal)) +
  geom_bar(width = 0.5, colour ="skyblue", fill = "skyblue", stat = "identity") +
  labs(x = "Edad", y = "Salario promedio") +
  theme_bw() +
  scale_y_continuous(labels = scales::dollar_format()) 
barras2
ggsave("Views/barras2.png", width = 6, height = 4,plot=barras2)

#3  LOG DEL SALARIO VS EDAD Y EDAD AL CUADRADO 

#A Regresión_ Age
base$age_2 <- base$age^2
modelo1 <- lm(ln_sal~age + age_2, data=base)
stargazer(modelo1, type="latex", title = "Resultados Modelo 1", out = "Views/mod1.txt")

# Intervalo de confianza con boostrap:
boostage <-function(data,index){
  
  f = lm(ln_sal~age + age_2, data, subset = index)
  
  coefs = f$coefficients
  
  b2 = coefs[2] 
  b3 = coefs[3] 
  
  page = -b2/(2*b3)
  
  
  return(page)
}

# Se hace la estimacion por bootstrap:
peakage = boot(data=base, boostage, R=nrow(base))
peakage
# Calculo intervalo de confianza:
boot.ci(boot.out = peakage, conf = c(0.95, 0.99), type = 'all')
#3.PLOT 

dispersion2 = ggplot(base, aes(x = age, y = ln_sal)) +
  geom_point(color = "salmon") +
  theme_bw() +
  geom_smooth(color = "black", method = "lm", formula = y ~ poly(x, 2)) +
  xlab("Edad") +
  ylab("Logaritmo del salario por hora") 

dispersion2
ggsave("Views/dispersion2.png", width = 6, height = 4,plot=dispersion2)

#PUNTO 4
# A. Regresión simple: Female 
base$Female <- ifelse(base$sex == 0, 1, 0) #cambiamos la variable sexo dado que ésta inicialmente toma el valor de 1 si la persona es hombre y 0 d.l.c para que tome el valor de 1 si la persona es mujer y 0 d.l.c y así correr el modelo con la que realmente se requiere en las instrucciones
modelo2 <- lm(ln_sal~ Female , data=base)
stargazer(modelo2, keep="Female", type="latex", title = "Resultados Modelo 2", out = "Views/mod2.txt")

# Regresión multiple (controles): Female
#Inicialmente vamos a volver P7040 de Binaria a Dummy
base$p7040= base$p7040-1
#creamos un ID
base$id<-rownames(base)

# FWL simple:
ypmod = lm(ln_sal ~ age + factor(maxEducLevel) + formal + factor(oficio) + hoursWorkUsual + p7040 + sizeFirm, data=base)
xpmod = lm(Female ~ age + factor(maxEducLevel) + formal + factor(oficio) + hoursWorkUsual + p7040 + sizeFirm, data=base)

FWL = data.frame('yprima' = ypmod["residuals"], 'xprima' = xpmod["residuals"])

fwlmod = lm(residuals ~ residuals.1, data = FWL)
stargazer(fwlmod, type="latex", title = "Resultados FWL Simple", out = "Views/modfwl.txt")

#FWL con Bootstrap:
FWL_boots <-function(data,index){
  ypmod = lm(ln_sal ~ age + factor(maxEducLevel) + formal + factor(oficio) + hoursWorkUsual + p7040 + sizeFirm, data, subset=index)
  xpmod = lm(Female ~ age + factor(maxEducLevel) + formal + factor(oficio) + hoursWorkUsual + p7040 + sizeFirm,data, subset=index)
  
  yprima = ypmod["residuals"]
  xprima = xpmod["residuals"]
  
  FWL = data.frame('yprima' = yprima, 'xprima' = xprima)
  colnames(FWL)= c("yprima", "xprima")
  fwlmod = lm(yprima ~ xprima, data = FWL)
  
  coefs = fwlmod$coefficients[2]
  
  return(coefs)
}
# Se hace la estimacion por bootstrap:
wage_gap = boot(data=base, FWL_boots, R=100)
wage_gap
#HUGO pon stargazer a mano 
# Calculo intervalo de confianza:
boot.ci(boot.out = wage_gap, conf = c(0.95, 0.99), type = 'all')


#Plot of predicting income

base_female<- base %>% filter(Female==1)
base_male<-base %>% filter(Female==0)

predict_plot<-ggplot() + 
              geom_smooth(data=base_male, aes(x=age, y=ln_sal), color='steelblue', method = "lm", formula = y ~ poly(x, 2)) + 
              geom_smooth(data=base_female, aes(x=age, y=ln_sal), color='coral2', method = "lm", formula = y ~ poly(x, 2)) + 
              theme_bw() +
              labs(x = "Edad en años", y = "Logaritmo del salario") +
predict_plot

#5.Predicting Earnings 
set.seed(10101)  

inTrain <- createDataPartition(
  y = base$ln_sal,  ## the outcome data are needed
  p = .70, ## The percentage of data in the
  list = FALSE
)

training <- base[ inTrain,]
testing  <- base[-inTrain,]

# Training 7 models 
# 1. 
form_1 <- ln_sal ~ age + age_2 
modelo1a <- lm(form_1,
               data = training)
predictions <- predict(modelo1a, testing)
score1a<- RMSE(predictions, testing$ln_sal )
score1a

# 2. 
form_2<- ln_sal ~ Female + age + factor(maxEducLevel) + formal + factor(oficio) + hoursWorkUsual + p7040 + sizeFirm

modelo2a <- lm(form_2,
               data = training )
predictions <- predict(modelo2a, testing)
score2a<- RMSE(predictions, testing$ln_sal )

score2a

# 3. 
form_3 <- ln_sal~ age + age^2 + age^3 + age^4 + age^5 + age^6 + age^7 + age^8
modelo3a <- lm(form_3, data = training)
predictions <- predict(modelo3a, testing)
score3a <- RMSE(predictions, testing$ln_sal)
score3a

#4. 
form_4 <- ln_sal ~ age + age_2 +
  poly(age,3,raw=TRUE):Female  + 
  poly(age,3,raw=TRUE):factor(maxEducLevel)  +
  poly(age,3,raw=TRUE):formal +
  poly(age,3,raw=TRUE):factor(oficio) +
  poly(age,3,raw=TRUE):hoursWorkUsual + 
  poly(age,3,raw=TRUE):p7040 +
  poly(age,3,raw=TRUE):sizeFirm 
modelo4a <- lm(form_4,
               data = training )
predictions <- predict(modelo4a, testing)
score4a<- RMSE(predictions, testing$ln_sal)
score4a

#5.
# Se admitirán 20% de datos faltantes como máximo:
porcentaje_obs = nrow(DF)*0.2
DF= DF %>% select(ED[ED$Missings<porcentaje_obs, 1]) 
ED = ED[ED$Missings<porcentaje_obs,]

# Clasificación por tipo de variable:
ED$Tipo = rep(NA, nrow(ED))
ordinales = c('estrato1','age', 'p6100','p6210', 'p6210s1',  "oficio", "relab", "p6870", "fex_dpto","hoursWorkUsual", 
              "fweight", "maxEducLevel", "regSalud",   "totalHoursWorked", "sizeFirm")
categoricas = c('mes', 'p6050', "p6240", "p6920")
binarias = c('sex',"college", "formal", "informal", "microEmpresa")
binar12 = c('p6090',"p7040","p7090","p7495","p7505", "cotPension")
continuas = c('p6426', "p6500", "p6510s1", "p6545s1", "p6580s1", "p6585s1a1", "p6585s2a1", "p6585s3a1", "p6585s4a1", 
              "p6590s1", "p6600s1", "p6610s1", "p6620s1", "p6630s1a1", "p6630s2a1", "p6630s3a1", "p6630s4a1","p6630s6a1",
              "p7070", "p7500s1a1", "p7500s2a1", "p7500s3a1", "p7510s1a1", "p7510s2a1", "p7510s3a1", "p7510s5a1","p7510s6a1",
              "p7510s7a1", "impa", "isa", "ie", "iof1", "iof2", "iof3h", "iof3i","iof6", "ingtotob", "ingtot","fex_c",
              "y_salary_m", "y_salary_m_hu", "y_ingLab_m", "y_ingLab_m_ha", "y_total_m", "y_total_m_ha")
rem = c("p6510","p6545","p6580","p6585s1","p6585s2", "p6585s3", "p6585s4","p6590", "p6600", "p6610", "p6620", "p6630s1",
        "p6630s2", "p6630s3", "p6630s4", "p6630s6", 'directorio', 'secuencia_p', 'orden')                                                  

ED = ED[!(ED$Variable %in% rem),]                                           
ED = ED %>% mutate(Tipo = case_when(Variable %in% continuas ~ 'continua',
                                    Variable %in% ordinales ~ 'ordi',
                                    Variable %in% categoricas ~ 'cat',
                                    Variable %in% c(binarias, binar12) ~ 'dummy'))                                 
                                       
                   
# Se estandarizan los datos ordinales y contunuos:
DFstandard = DF %>% mutate_at(c(continuas, ordinales), ~(scale(.) %>% as.vector))

# Se crean dummys para las categoricas:
DFstandard = get_dummies(
  DFstandard,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "_",
  drop_first = FALSE,
  dummify_na = TRUE)

# Se limpian las binarias:
DFstandard = DFstandard %>% mutate_at(binar12, ~  )



