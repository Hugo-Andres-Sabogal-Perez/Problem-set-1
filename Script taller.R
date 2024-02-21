#### Script problem set 1 ######

#limpiar entorno
rm(list = ls())

#llamar librerias
require(pacman)
require(tidyverse)
require(rvest)
require(stargazer)
require(rio)
require(caret)
require(gridExtra)
require(skimr)

importar_datos<-function(){
  
  link_incompleto<-'https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_'
  
  datos<-read_html(paste0(link_incompleto, 'page_',as.character(1),'.html'))  %>% 
    html_table() %>% as.data.frame()
  
  for (i in 2:10){
    html<-read_html(paste0(link_incompleto, 'page_',as.character(i),'.html'))  %>% 
          html_table() %>% as.data.frame()
    datos<-rbind(datos,html)
    
  }
  # Seleccion de la muetra de interes: edad >= 18 y empleado (ocu).
  datos <- datos[datos$age >= 18 & datos$ocu == 1,]
  datos <- datos[,-1]
  
  return(datos)
  
}

DF<-importar_datos()

vars = length(colnames(DF))
ED = data.frame('Variable' = colnames(DF), 'Tipo' = rep(NA, vars) , 'Missings' = rep(NA, vars), 'Media' =  rep(NA, vars), 
                'Desviacion Estandard' = rep(NA, vars))

for(col in colnames(DF)){
  df = DF[,colnames(DF) == col]
  NAs = sum(is.na(df))
  mean = mean(df, na.rm = T)
  sd = sqrt(var(df, na.rm = T))
  
  ED[ED$Variable == col, 3] = NAs
  ED[ED$Variable == col, 4] = mean
  ED[ED$Variable == col, 5] = sd
}

C = ED %>% filter(Desviacion.Estandard == 0) %>% select(Variable) %>% as.vector()

# 1. Limpieza de datos:
DF = DF[!is.na(DF$y_ingLab_m_ha),]
DF = DF1 %>% select(-C$Variable)

write.csv(x = DF, file = "DF.csv", row.names = FALSE) 
DF=import("Stores/DF.csv")
# 2. Estadisticas descriptivas:
#salario real o nominal?
base= DF %>% select(age,oficio, formal, maxEducLevel, orden, p7040, sex, sizeFirm, y_ingLab_m_ha, hoursWorkUsual)
stargazer(base, type= "text", summary=T, title = "Estadisticas Descriptivas",out = "Views/esta_des.txt")

# Missings de la sub muestra:1
vars = length(colnames(base))
missingb = data.frame('Variable' = colnames(base), 'Missings' = rep(NA, vars))
for(col in colnames(base)){
  df = base[,colnames(base) == col]
  NAs = sum(is.na(df))

  missingb[missingb$Variable == col, 2] = NAs
}

# Eliminar la observacion:
base = base[!(is.na(base$maxEducLevel)),]

#Gráficas 
#1. Histograma

base$ln_sal = log(base$y_ingLab_m_ha)

histograma <- ggplot(base, aes(x=ln_sal)) + geom_histogram(color="white",fill="darkblue") + xlab('Logaritmo del salario por hora') + ylab('Frecuencia') + theme_bw() 
histograma

ggsave("Views/histograma.png", width = 6, height = 4,plot=histograma)

#2. Dispersión
# El ln(w) es relativamente homocedasttico sobre la edad.
dispersion = ggplot(base, aes(x=age, y=ln_sal)) + geom_point(color="navy") + theme_bw() +
            geom_smooth(method = 'lm',color="firebrick") +xlab('Edad') + ylab('Logaritmo del salario por hora')
dispersion

ggsave("Views/dispersion.png", width = 6, height = 4,plot=dispersion)

#3. Regresión_ Age
base$age_2 <- base$age^2
modelo1 <- lm(ln_sal~age + age_2, data=base)
stargazer(modelo1, type="text", title = "Resultados Modelo 1", out = "Views/mod1.txt")

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

#4. Regresión: Female
base$Female <- ifelse(base$sex == 0, 1, 0)
modelo2 <- lm(ln_sal~ Female + age + maxEducLevel + formal + oficio + hoursWorkUsual + p7040 + sizeFirm, data=base)
modelo2
stargazer(modelo2, type="text", title = "Resultados Modelo 1", out = "Views/mod2.txt")
stargazer(modelo2, keep="Female", type="text", title = "Resultados Modelo 1", out = "Views/mod2.txt")

# FWL simple:
ypmod = lm(ln_sal ~ age + maxEducLevel + formal + oficio + hoursWorkUsual + p7040 + sizeFirm, data=base)
xpmod = lm(Female ~ age + maxEducLevel + formal + oficio + hoursWorkUsual + p7040 + sizeFirm, data=base)

yprima = predict(ypmod)
xprima = predict(xpmod)

FWL = data.frame('yprima' = yprima, 'xprima' = xprima)

fwlmod = lm(yprima ~ xprima, data = FWL)
stargazer(fwlmod, type="text", title = "Resultados FWL Simple", out = "Views/mod2.txt")

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
form_2<- ln_sal ~ Female + age + maxEducLevel + formal + oficio + hoursWorkUsual + p7040 + sizeFirm

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
  poly(age,3,raw=TRUE):maxEducLevel  +
  poly(age,3,raw=TRUE):formal +
  poly(age,3,raw=TRUE):oficio +
  poly(age,3,raw=TRUE):hoursWorkUsual + 
  poly(age,3,raw=TRUE):p7040 +
  poly(age,3,raw=TRUE):sizeFirm 
modelo4a <- lm(form_4,
               data = training )
predictions <- predict(modelo4a, testing)
score4a<- RMSE(predictions, testing$ln_sal)
score4a