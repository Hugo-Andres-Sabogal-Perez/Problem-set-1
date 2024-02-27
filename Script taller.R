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
require(boot)
require(tidytable)

### Funcion para importar datos
importar_datos<-function(){
  
  ##Llamamos el link para el web scraping
  link_incompleto<-'https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_' 
  
  ##Invocamos la tabla de la primera pagina como dataframe concatenando strings 
  datos<-read_html(paste0(link_incompleto, 'page_',as.character(1),'.html'))  %>% 
    html_table() %>% as.data.frame()
  
  #Relizamos un ciclo para añadir la filas de la pagina 2 a la 9 de a nuestro data set
  for (i in 2:10){
    html<-read_html(paste0(link_incompleto, 'page_',as.character(i),'.html'))  %>% 
          html_table() %>% as.data.frame()
    datos<-rbind(datos,html)
    
  }
  # Selección de la muestra de interes: edad >= 18 y empleado (ocu).
  datos <- datos[datos$age >= 18 & datos$ocu == 1,]
  
  #retornamos el data set final
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

## importamos y exportamos para no volver hacer web scraping
write.csv(x = DF, file = "Stores/DF.csv", row.names = FALSE) 
DF<-import("Stores/DF.csv")

# 2. Estadisticas descriptivas:
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

#Gráficas y 
#1. Histograma

base$ln_sal = log(base$y_ingLab_m_ha)

histograma <- ggplot(base, aes(x=ln_sal)) + 
              geom_histogram(color="white",fill="darkblue") + 
              xlab('Logaritmo del salario por hora') + ylab('Frecuencia') + 
              theme_bw() 
histograma

ggsave("Views/histograma.png", width = 6, height = 4,plot=histograma)

#2. Dispersión
# El ln(w) es relativamente homocedasttico sobre la edad.
dispersion = ggplot(base, aes(x=age, y=ln_sal)) + geom_point(color="navy") + 
             theme_bw() +
             geom_smooth(method = 'lm',color="firebrick") +xlab('Edad')+ 
             ylab('Logaritmo del salario por hora')
dispersion

ggsave("Views/dispersion.png", width = 6, height = 4,plot=dispersion)


#3. Dispersión 2
base$sex_factor <- factor(base$sex, levels = c(1,0),
                            labels = c('Masculino', 'Femenino'))

base$sex_factor<-factor(base$sex_factor)

dispersion2 = ggplot(base, aes(x = age, y = ln_sal)) +
  geom_point(aes(color = sex_factor)) +
  theme_bw() +
  geom_smooth(color = "black", method = "lm", formula = y ~ poly(x, 2)) +
  xlab("Edad") +
  ylab("Logaritmo del salario por hora") +
  scale_color_manual(values = c("Masculino" = "skyblue", "Femenino" ="maroon" )) +
  guides(color = guide_legend(title = "Sexo", title.hjust = 0.5))

dispersion2


#4. barras
#Creamos una variable categorica para la edad
base$edad_cat <- cut(base$age, breaks = c(17, 29, 45, 59, Inf), labels = c("18-29", "30-45", "46-59", "60 o más"))

edad_salario<-base %>% group_by(edad_cat) %>% 
              summarize(mean_sal=mean(y_ingLab_m_ha))


barras1 <- ggplot(edad_salario, aes(x = edad_cat, y = mean_sal)) +
  geom_bar(width = 0.5, colour = "black", fill = "skyblue", stat = "identity") +
  labs(x = "Edad en años", y = "Salario promedio") +
  theme_bw() +
  scale_y_continuous(labels = scales::dollar_format()) 
barras1


#3. Regresión_ Age
base$age_2 <- base$age^2
modelo1 <- lm(ln_sal~age + age_2, data=base)
stargazer(modelo1, type="text", title = "Resultados Modelo 1", out = "Views/mod1.txt")

#3.b  

dispersion3 = ggplot(base, aes(x = age, y = ln_sal)) +
  geom_point(color='salmon') +
  theme_bw() +
  geom_smooth(color = "black", method = "lm", formula = y ~ poly(x, 2)) +
  xlab("Edad") +
  ylab("Logaritmo del salario por hora") 

dispersion3


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

#4. Regresión simple: Female
base$Female <- ifelse(base$sex == 0, 1, 0)
modelo2 <- lm(ln_sal~ Female , data=base)
modelo2
stargazer(modelo2, type="text", title = "Resultados Modelo 1", out = "Views/mod2.txt")
stargazer(modelo2, keep="Female", type="text", title = "Resultados Modelo 1", out = "Views/mod2.txt")

#4. Regresión multiple (controles): Female

#creamos un ID
base$id<-rownames(base)



#Modelo 3.
base$Female <- ifelse(base$sex == 0, 1, 0)
modelo3 <- lm(ln_sal~ Female + age + maxEducLevel + formal + oficio + hoursWorkUsual + p7040 + sizeFirm, data=base)
modelo3
stargazer(modelo3, type="text", title = "Resultados Modelo 3", out = "Views/mod3.txt")
stargazer(modelo3, keep="Female", type="text", title = "Resultados Modelo 3", out = "Views/mod3.txt")


# FWL simple:
ypmod = lm(ln_sal ~ age + maxEducLevel + formal + oficio + hoursWorkUsual + p7040 + sizeFirm, data=base)
xpmod = lm(Female ~ age + maxEducLevel + formal + oficio + hoursWorkUsual + p7040 + sizeFirm, data=base)

FWL = data.frame('yprima' = ypmod["residuals"], 'xprima' = xpmod["residuals"])

fwlmod = lm(residuals ~ residuals.1, data = FWL)
stargazer(fwlmod, type="text", title = "Resultados FWL Simple", out = "Views/mod2.txt")

#FWL con Bootstrap:
FWL_boots <-function(data,index){
  ypmod = lm(ln_sal ~ age + maxEducLevel + formal + oficio + hoursWorkUsual + p7040 + sizeFirm, data, subset=index)
  xpmod = lm(Female ~ age + maxEducLevel + formal + oficio + hoursWorkUsual + p7040 + sizeFirm,data, subset=index)
  
  yprima = ypmod["residuals"]
  xprima = xpmod["residuals"]
  
  FWL = data.frame('yprima' = yprima, 'xprima' = xprima)
  colnames(FWL)= c("yprima", "xprima")
  fwlmod = lm(yprima ~ xprima, data = FWL)
  
  coefs = fwlmod$coefficients[1]

  return(coefs)
}

# Se hace la estimacion por bootstrap:
wage_gap = boot(data=base, FWL_boots, R=nrow(base))
wage_gap

# Calculo intervalo de confianza:
boot.ci(boot.out = wage_gap, conf = c(0.95, 0.99), type = 'all')


#Datos Condicionados 

#Plot
dispersion4 = ggplot(base, aes(x = age, y = ln_sal)) +
  geom_point(color='salmon') +
  theme_bw() +
  geom_smooth(color = "black", method = "lm", formula = y ~ poly(x, 2) + Female + maxEducLevel + formal + oficio + hoursWorkUsual + p7040 + sizeFirm) +
  xlab("Edad") +
  ylab("Logaritmo del salario por hora") 

dispersion4

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

#5.
porcentaje_obs = nrow(DF)*0.1
ED=ED %>% filter(Missings<porcentaje_obs & Desviacion.Estandard !=0) 

