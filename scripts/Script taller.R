#### Script problem set 1 ######
setwd('/Users/juansilva/Desktop/Universidad/Semestre 8/Big Data')
#limpiar entorno
rm(list = ls())

#llamar librerias
require(pacman)
require(tidyverse)
require(rvest)
require(stargazer)


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

# 2. Estadisticas descriptivas:
#salario real o nominal?
base= DF %>% select(age, depto, oficio, formal, maxEducLevel, orden, p7040, sex, sizeFirm, y_ingLab_m_ha, hoursWorkUsual)
stargazer(base, type= "text", summary=T, title = "Estadisticas Descriptivas",out = "Views/esta_des.txt")
#Gráficas 
#1. Histograma

base$ln_sal<-log(base$y_ingLab_m_ha)

hist(base$ln_sal,main=" ", xlab='Logaritmo del salario por hora', ylab='Frecuencia' )


#2. Dispersión
dispersion<-ggplot(base, aes(x=age, y=ln_sal)) + geom_point() + theme_bw() +
            geom_smooth(method = 'lm') +xlab('Edad') + ylab('Logaritmo del salario por hora')
            
dispersion

#3. Regresión
base$age_2 <- base$age^2
modelo1 <- lm(ln_sal~age + age_2, data=base)
stargazer(modelo1, type="text", title = "Resultados Modelo 1", out = "Views/mod1.txt")

#4. 
