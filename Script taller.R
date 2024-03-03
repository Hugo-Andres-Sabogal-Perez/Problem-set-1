#### Script problem set 1 ######

# Realizamos inicialmente una limpieza del entorno
rm(list = ls())

setwd(substr(getwd(), 1, nchar(getwd()) - 8))

# Llamamos las librerías necesarias para la realización del trabajo
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
require(VIM)
require(leaps)
require(margins)


### Función para importar datos
importar_datos <- function() {
  ## Llamamos el link para el web scraping
  link_incompleto <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_"

  ## Invocamos la tabla de la primera pagina como dataframe (datos) concatenando strings
  datos <- read_html(paste0(link_incompleto, "page_", as.character(1), ".html")) %>%
    html_table() %>%
    as.data.frame()

  # Relizamos un ciclo para añadir las filas de la página 2 a la 10 de a nuestro data set
  for (i in 2:10) {
    html <- read_html(paste0(link_incompleto, "page_", as.character(i), ".html")) %>%
      html_table() %>%
      as.data.frame()
    datos <- rbind(datos, html)
  }
  # Selección de la muestra de interés: edad >= 18 y empleado (ocu).
  datos <- datos[datos$age >= 18 & datos$ocu == 1, ] # Se utiliza la variable de ocupados dado que según su definición corresponde a las personas que tienen un trabajo formal, realizan actividades independientes y han trabajado durante la semana de referencia.

  # retornamos el data set final
  return(datos)
}

DF <- importar_datos()

# Exportamos los datos deaspués del web scraping en una base de datos llamada DF.cvs para que el código sea más eficiente
write.csv(x = DF, file = "Stores/DF.csv", row.names = FALSE)

# DESDE AQUÍ SE PUEDE COMENZAR EL SCIRPT UNA VEZ REALIZADO EL WEB SCRAPING Y YA TENIEDO EL ARCHIVO DE LA BASE GUARDADO
# Llamamos nuestra base de datos

DF <- import("Stores/DF.csv")

# Se crea una base para guardar las estadisticas descriptivas más relevantes para el trabajo:
vars <- length(colnames(DF))
ED <- data.frame("Variable" = colnames(DF), "Missings" = rep(NA, vars), "Media" = rep(NA, vars), "Desviacion Estandard" = rep(NA, vars))

# Se cuentan los missings y se calcula la media y la desviación estándar de la muestra:
for (col in colnames(DF)) {
  df <- DF[, colnames(DF) == col]
  NAs <- sum(is.na(df))
  mean <- mean(df, na.rm = T)
  sd <- sqrt(var(df, na.rm = T))

  ED[ED$Variable == col, 2] <- NAs
  ED[ED$Variable == col, 3] <- mean
  ED[ED$Variable == col, 4] <- sd
}

# 1. Limpieza de datos:
# Se eliminan las constantes (u observaciones que tienen desviación estándar igual a cero) y las variables sin observaciones (missings):
C <- ED %>%
  filter(Desviacion.Estandard == 0 | is.na(Desviacion.Estandard)) %>%
  select(Variable) %>%
  as.vector()
ED <- ED %>% filter(Desviacion.Estandard != 0 | !is.na(Desviacion.Estandard))
ED <- ED %>% filter(Variable != "cuentaPropia")
DF <- DF[!is.na(DF$y_ingLab_m_ha), ]
DF <- DF %>% select(-C$Variable)
DF <- DF[DF$age != 78, ]
DF <- DF[!(rownames(DF) %in% c("5733", "579")), ]

# 2. Estadisticas descriptivas:
base <- DF %>% select(age, oficio, formal, maxEducLevel, orden, p7040, sex, sizeFirm, y_ingLab_m_ha, hoursWorkUsual)
base$p7040[base$p7040 == 2] <- 0
base$ln_sal <- log(base$y_ingLab_m_ha) # Se crea el logaritmo del salario por horas para normalizar los valores de la variable.
stargazer(base, type = "text", summary = T, title = "Estadisticas Descriptivas", out = "Views/esta_des.txt")


# Gráficas relevantes para las estadísticas descriptivas
# 1. Histograma de la variable Y: salarios por horas
histograma_salario <- ggplot(base, aes(x = y_ingLab_m_ha)) +
  geom_histogram(color = "white", fill = "darkblue") +
  xlab("Salario por hora") +
  ylab("Frecuencia") +
  theme_bw()
histograma_salario

ggsave("Views/histograma_sal.pdf", width = 6, height = 4, plot = histograma_salario)

# 2. Histograma de la variable Y: log del salario por hora (transformación)
histograma <- ggplot(base, aes(x = ln_sal)) +
  geom_histogram(color = "white", fill = "darkblue") +
  xlab("Logaritmo del salario por hora") +
  ylab("Frecuencia") +
  theme_bw()
histograma

ggsave("Views/histograma.pdf", width = 6, height = 4, plot = histograma)

# 3. Gráfica de Dispersión: Edad vs. Logaritmo del Salario por hora
# El ln(w) es relativamente homocedastico sobre la edad.
dispersion <- ggplot(base, aes(x = age, y = ln_sal)) +
  geom_point(color = "navy") +
  theme_bw() +
  geom_smooth(method = "lm", color = "firebrick") +
  xlab("Edad") +
  ylab("Logaritmo del salario por hora")
dispersion

ggsave("Views/dispersion.pdf", width = 6, height = 4, plot = dispersion)


# 4. Gráfico de Barras: Sexo Vs. Salario Promedio
base$sex_factor <- factor(base$sex,
  levels = c(1, 0),
  labels = c("Masculino", "Femenino")
)

Salario_sex <- base %>%
  group_by(sex_factor) %>%
  summarize(mean_sal_sex = mean(y_ingLab_m_ha))


barras1 <- ggplot(Salario_sex, aes(x = sex_factor, y = mean_sal_sex)) +
  geom_bar(width = 0.5, colour = "skyblue", fill = "skyblue", stat = "identity") +
  labs(x = "Sexo", y = "Log del Salario por hora") +
  theme_bw() +
  scale_y_continuous(labels = scales::dollar_format())
barras1
ggsave("Views/barras1.pdf", width = 6, height = 4, plot = barras1)

# 6. Gráfico de Barras: Edad vs. Salario Promedio

Edad <- base %>%
  group_by(age) %>%
  summarize(mean_sal = mean(y_ingLab_m_ha))

barras2 <- ggplot(Edad, aes(x = age, y = mean_sal)) +
  geom_bar(width = 0.5, colour = "skyblue", fill = "skyblue", stat = "identity") +
  labs(x = "Edad", y = "Salario promedio") +
  theme_bw() +
  scale_y_continuous(labels = scales::dollar_format())
barras2
ggsave("Views/barras2.pdf", width = 6, height = 4, plot = barras2)

# 3  LOG DEL SALARIO VS EDAD Y EDAD AL CUADRADO

# A Regresión_ Age
base$age_2 <- base$age^2
modelo1 <- lm(ln_sal ~ age + I(age^2), data = base)
mar <- summary(margins(modelo1))

stargazer(modelo1, type = "latex", title = "Resultados Modelo 1", out = "Views/mod1.txt", digits = 5)

set.seed(10101)

# Intervalo de confianza con boostrap:
boostage <- function(data, index) {
  f <- lm(ln_sal ~ age + age_2, data, subset = index)

  coefs <- f$coefficients

  b2 <- coefs[2]
  b3 <- coefs[3]

  page <- -b2 / (2 * b3)


  return(page)
}

# Se hace la estimacion por bootstrap:
peakage <- boot(data = base, boostage, R = nrow(base))
peakage
# Calculo intervalo de confianza:
boot.ci(boot.out = peakage, conf = c(0.95, 0.99), type = "all")
# 3.PLOT

dispersion2 <- ggplot(base, aes(x = age, y = ln_sal)) +
  geom_point(color = "salmon") +
  theme_bw() +
  geom_smooth(color = "black", method = "lm", formula = y ~ poly(x, 2)) +
  xlab("Edad") +
  ylab("Logaritmo del salario por hora")

dispersion2
ggsave("Views/dispersion2.pdf", width = 6, height = 4, plot = dispersion2)

# PUNTO 4
# A. Regresión simple: Female
base$Female <- ifelse(base$sex == 0, 1, 0) # cambiamos la variable sexo dado que ésta inicialmente toma el valor de 1 si la persona es hombre y 0 d.l.c para que tome el valor de 1 si la persona es mujer y 0 d.l.c y así correr el modelo con la que realmente se requiere en las instrucciones
modelo2 <- lm(ln_sal ~ Female, data = base)

# Regresión multiple (controles): Female

# FWL simple:
ypmod <- lm(ln_sal ~ age + factor(maxEducLevel) + formal + factor(oficio) + hoursWorkUsual + p7040 + sizeFirm, data = base)
xpmod <- lm(Female ~ age + factor(maxEducLevel) + formal + factor(oficio) + hoursWorkUsual + p7040 + sizeFirm, data = base)

FWL <- data.frame("yprima" = ypmod["residuals"], "xprima" = xpmod["residuals"])

fwlmod <- lm(residuals ~ residuals.1, data = FWL)
stargazer(modelo2, fwlmod, fwlmod,
  type = "latex", title = "Resultados FWL Simple",
  out = "Views/modsfemale(latex).tex", digits = 5, add.lines = c(list(
    c("Errores estandar", "Convencionales", "Convencionales", "Bootstrap"),
    c("Controles", "No", "Si", "Si")
  ))
)

# FWL con Bootstrap:
FWL_boots <- function(data, index) {
  ypmod <- lm(ln_sal ~ age + factor(maxEducLevel) + formal + factor(oficio) + hoursWorkUsual + p7040 + sizeFirm, data, subset = index)
  xpmod <- lm(Female ~ age + factor(maxEducLevel) + formal + factor(oficio) + hoursWorkUsual + p7040 + sizeFirm, data, subset = index)

  yprima <- ypmod["residuals"]
  xprima <- xpmod["residuals"]

  FWL <- data.frame("yprima" = yprima, "xprima" = xprima)
  colnames(FWL) <- c("yprima", "xprima")
  fwlmod <- lm(yprima ~ xprima, data = FWL)

  coefs <- fwlmod$coefficients[2]

  return(coefs)
}
# Se hace la estimacion por bootstrap:
wage_gap <- boot(data = base, FWL_boots, R = 5000)
wage_gap

# Stargazer de 3 regresiones: El erro estandar de bootstrap se coloco manualmente
stargazer(modelo2, fwlmod,
  fwlmod,
  type = "latex",
  title = "Resultados FWL Simple",
  out = "Views/modsfemale(latex).tex", digits = 5,
  add.lines = c(list(
    c("Errores estandar", "Convencionales", "Convencionales", "Bootstrap"),
    c("Controles", "No", "Si", "Si")
  ))
)


# Hallando los peakages por género
base_female <- base %>% filter(Female == 1)
base_male <- base %>% filter(Female == 0)

boostage <- function(data, index) {
  f <- lm(ln_sal ~ age + age_2 + factor(maxEducLevel) + formal + factor(oficio) + hoursWorkUsual + p7040 + sizeFirm, data, subset = index)

  coefs <- f$coefficients

  b2 <- coefs[2]
  b3 <- coefs[3]

  page <- -b2 / (2 * b3)


  return(page)
}

# Se hace la estimacion por bootstrap:
peakage_female <- boot(data = base_female, boostage, R = 5000)
peakage_female
peakage_male <- boot(data = base_male, boostage, R = 5000)
peakage_male
# Calculo intervalo de confianza:
boot.ci(boot.out = peakage, conf = c(0.95, 0.99), type = "all")

# Plot of predicting income


predict_plot <- ggplot() +
  geom_smooth(data = base_male, aes(x = age, y = ln_sal, color = "Masculino"), method = "lm", formula = y ~ poly(x, 2)) +
  geom_smooth(data = base_female, aes(x = age, y = ln_sal, color = "Femenino"), method = "lm", formula = y ~ poly(x, 2)) +
  theme_bw() +
  labs(x = "Edad en años", y = "Logaritmo del salario") +
  scale_colour_manual(name = "Sexo", values = c("Masculino" = "steelblue", "Femenino" = "coral2"))

predict_plot

ggsave("Views/predict_by_zex.pdf", width = 6, height = 4, plot = predict_plot)


# 5.Predicting Earnings
set.seed(10101)

inTrain <- createDataPartition(
  y = base$ln_sal, ## the outcome data are needed
  p = .70, ## The percentage of data in the
  list = FALSE
)

training <- base[inTrain, ]
testing <- base[-inTrain, ]

# Training 7 models
# 1.
form_1 <- ln_sal ~ age + age_2
modelo1a <- lm(form_1,
  data = training
)
predictions1 <- predict(modelo1a, testing)
score1a <- RMSE(predictions1, testing$ln_sal)
score1a

# 2.
testing_of <- testing

testing_of$oficio <- factor(testing_of$oficio, exclude = c(73, 63))

form_2 <- ln_sal ~ Female + age + factor(maxEducLevel) + formal + factor(oficio) + hoursWorkUsual + p7040 + factor(sizeFirm)

modelo2a <- lm(form_2,
  data = training
)

predictions2 <- predict(modelo2a, testing_of)

data_mod2a <- data.frame("ln_sal" = testing_of$ln_sal, "pred2" = predictions2)
data_mod2a <- na.omit(data_mod2a)

score2a <- RMSE(pred = data_mod2a$pred2, obs = data_mod2a$ln_sal)

score2a

# 3.
form_3 <- ln_sal~ age + age^2 + age^3 + age^4 + age^5 + age^6 + age^7 + age^8
modelo3a <- lm(form_3, data = training)
predictions <- predict(modelo3a, testing)
score3a <- RMSE(predictions, testing$ln_sal)
score3a

# 4.
form_4 <- ln_sal ~ age + age_2 +
  poly(age, 3, raw = TRUE):Female +
  poly(age, 3, raw = TRUE):factor(maxEducLevel) +
  poly(age, 3, raw = TRUE):formal +
  poly(age, 3, raw = TRUE):factor(oficio) +
  poly(age, 3, raw = TRUE):hoursWorkUsual +
  poly(age, 3, raw = TRUE):p7040 +
  poly(age, 3, raw = TRUE):sizeFirm
modelo4a <- lm(form_4,
  data = training
)

predictions4 <- predict(modelo4a, testing_of)

data_mod4a <- data.frame("ln_sal" = testing_of$ln_sal, "pred4" = predictions4)
data_mod4a <- na.omit(data_mod4a)


score4a <- RMSE(pred = data_mod4a$pred4, obs = data_mod4a$ln_sal)
score4a

# 5.
# Se admitirán 20% de datos faltantes como máximo:
porcentaje_obs <- nrow(DF) * 0.2
Miss <- ED[ED$Missings < porcentaje_obs, 1]
DF <- DF %>% select(Miss$Variable)
ED <- ED[ED$Missings < porcentaje_obs, ]

# Se crea logaritmo del salario:
DF$lnw <- log(DF$y_ingLab_m_ha)

# Clasificación por tipo de variable:
ED$Tipo <- rep(NA, nrow(ED))
ordinales <- c(
  "estrato1", "age", "p6100", "p6210", "p6210s1", "oficio", "relab", "p6870", "fex_dpto", "hoursWorkUsual",
  "fweight", "maxEducLevel", "regSalud", "totalHoursWorked", "sizeFirm"
)
categoricas <- c("mes", "p6050", "p6240", "p6920", "cotPension")
binarias <- c("sex", "college", "formal", "informal", "microEmpresa")
binar12 <- c("p7040", "p7090", "p7495", "p7505", "p6090")
continuas <- c(
  "p6426", "p6500", "p6510s1", "p6545s1", "p6580s1", "p6585s1a1", "p6585s2a1", "p6585s3a1", "p6585s4a1",
  "p6590s1", "p6600s1", "p6610s1", "p6620s1", "p6630s1a1", "p6630s2a1", "p6630s3a1", "p6630s4a1", "p6630s6a1",
  "p7070", "p7500s1a1", "p7500s2a1", "p7500s3a1", "p7510s1a1", "p7510s2a1", "p7510s3a1", "p7510s5a1", "p7510s6a1",
  "p7510s7a1", "impa", "isa", "ie", "iof1", "iof2", "iof3h", "iof3i", "iof6", "ingtotob", "ingtot", "fex_c",
  "y_salary_m", "y_salary_m_hu", "y_ingLab_m", "y_total_m", "y_total_m_ha"
)
rem <- c(
  "p6510", "p6545", "p6580", "p6585s1", "p6585s2", "p6585s3", "p6585s4", "p6590", "p6600", "p6610", "p6620", "p6630s1",
  "p6630s2", "p6630s3", "p6630s4", "p6630s6", "directorio", "secuencia_p", "orden", "y_ingLab_m_ha"
)

DF <- DF %>% select(-rem)
ED <- ED[!(ED$Variable %in% rem), ]
ED <- ED %>% mutate(Tipo = case_when(
  Variable %in% continuas ~ "continua",
  Variable %in% ordinales ~ "ordi",
  Variable %in% categoricas ~ "cat",
  Variable %in% c(binarias, binar12) ~ "dummy"
))
# Eliminacion de outliers de categoricas:
DF <- DF[DF$p6090 != 9, ]
DF <- DF[DF$p6100 != 9, ]
DF <- DF[DF$p6210 != 9, ]
DF <- DF[DF$relab != 8, ]

# Se estandarizan los datos ordinales y contunuos:
DF <- DF %>% mutate_at(c(continuas, ordinales), ~ (scale(.) %>% as.vector()))

# Se crean dummys para las categoricas:
DF <- get_dummies(
  DF,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "_",
  drop_first = FALSE,
  dummify_na = TRUE
)

DF <- DF %>% select(-categoricas)
ED <- ED[!(ED$Variable %in% categoricas), ]

# Se limpian las binarias:
resta1 <- function(x) {
  y <- x - 1
  returnValue(y)
}
DF <- DF %>% mutate_at(binar12, ~ (resta1(.)))

# Se escogen variables que no tengan covarianza 1 con otras:
Corr <- as.data.frame(cor(DF, use = "complete.obs"))

ED$Corr <- as.vector(rep(NA, nrow(ED)))

for (var in rownames(Corr)) {
  COR <- Corr %>% select(var)
  names <- colnames(Corr)[abs(COR) > 0.999]
  names <- names[!is.na(names)]
  ED$Corr[ED$Variable == var] <- toString.default(names)
}

# Variables a descartar:
r <- c(
  "y_total_m", "y_total_m_ha", "y_ingLab_m", "p6500", "informal", "p6100", "fex_c", "fex_dpto",
  "fweight", "iof6", "iof2", "iof1", "p7070"
)

DF <- DF %>% select(-r)
ED <- ED[!(ED$Variable %in% r), ]

# Imputación de variables por knn:
# k = 5
DFimp <- kNN(DF)
DFimp <- DFimp[, 1:89]

# Test y train:
set.seed(10101)

inTrain <- createDataPartition(
  y = DFimp$lnw, ## the outcome data are needed
  p = .70, ## The percentage of data in the
  list = FALSE
)

training <- DFimp[inTrain, ]
testing <- DFimp[-inTrain, ]

# Formula general para la selección de variables:
varspoly <- ED$Variable[ED$Tipo == "continua"]
varsum <- ED$Variable[ED$Tipo != "continua"]
varcat <- colnames(DFimp)[57:89]
maxmodel <- as.formula(paste("lnw ~", paste(varsum, collapse = "+"), "+", paste(varcat, collapse = "+"), "+", paste("poly(", varspoly, ", 2, raw=TRUE)", collapse = " + ")))

# Modelo Forward Selection:
set.seed(10101)
folds <- sample(rep(1:10, length = nrow(DFimp)))
crossval <- matrix(NA, 10, 50, dimnames = list(NULL, paste(1:50)))
for (j in 1:10) {
  fit <- regsubsets(maxmodel, data = DFimp[folds != j, ], nvmax = 50, method = "forward")
  test <- model.matrix(maxmodel, data = DFimp[folds == j, ])
  for (i in 1:50) {
    coefi <- coef(fit, id = i)
    pred <- test[, names(coefi)] %*% coefi
    crossval[j, i] <- mean((DFimp$lnw[folds == j] - pred)^2)
  }
}

# Se calcula la raiz del error cuadratico medio:
errforward <- apply(crossval, 2, mean)

nvars <- which.min(errforward)[[1]]

# Se estima el mejor modelo con 24 variables elegidas mediante el algoritmo forward.
forward_model <- regsubsets(maxmodel,
  data = DFimp,
  nvmax = nvars,
  method = "forward"
)

forward_model_names <- names(coef(forward_model, id = nvars))

# Se extrae el RMSE del modelo:
score5 <- errforward[nvars]

# Se plentea la forma funcional de forward selection:
formfor <- as.formula(lnw ~ sex + p6210 +
  oficio + college + totalHoursWorked +
  formal + sizeFirm + p6240_3 +
  p6240_4 + p6585s1a1 + p6585s2a1 +
  p6585s4a1 + p6610s1 + p6630s6a1 +
  p6630s6a1^2 + p7500s3a1^2 + iof3i^2 +
  ingtotob^2 + ingtot + p6920_3 +
  iof3h + ingtotob)

# Se calcula el RMSE fuera de muestra:
modelo5 <- lm(formfor, data = training)
predictions <- predict(modelo5, testing)
score5a <- RMSE(predictions, testing$lnw)


# Modelo Backward Selection:
for (j in 1:10) {
  fit <- regsubsets(maxmodel, data = DFimp[folds != j, ], nvmax = 50, method = "backward")
  test <- model.matrix(maxmodel, data = DFimp[folds == j, ])
  for (i in 1:50) {
    coefi <- coef(fit, id = i)
    pred <- test[, names(coefi)] %*% coefi
    crossval[j, i] <- mean((DFimp$lnw[folds == j] - pred)^2)
  }
}

# Se calcula la raiz del error cuadratico medio:
errbackward <- apply(crossval, 2, mean)

nvars <- which.min(errbackward)[[1]]

# Se estiam el mejor modelo de 35 variables:
backward_model <- regsubsets(maxmodel,
  data = DFimp,
  nvmax = nvars,
  method = "backward"
)

backward_model_names <- names(coef(backward_model, id = nvars))

# Se extrae el RMSE del modelo de 35 variables:
score6 <- errbackward[nvars]

# Se plantea la forma funcional del modelo:
formback <- as.formula(lnw ~ sex + p6210 +
  p6210s1 + oficio + p7505 +
  college + totalHoursWorked + sizeFirm +
  p6240_4 + p6920_2 + p6426^2 +
  p6585s1a1 + p6585s1a1^2 + p6585s2a1 +
  p6585s3a1 + p6585s4a1 + p6585s4a1^2 +
  p6600s1^2 + p6610s1 + p6620s1 +
  p6630s6a1 + p6630s6a1^2 + p7500s1a1 +
  p7500s1a1^2 + p7500s2a1^2 + p7500s3a1^2 +
  p7510s3a1 + ingtotob^2 + ingtot +
  ingtot^2 + mes_12 + p6920_3 + iof3h +
  ingtotob)

# Se calcula el RMSE fuera de muestra:
modelo6 <- lm(formback, data = training)
predictions <- predict(modelo6, testing)
score6a <- RMSE(predictions, testing$lnw)

# Interseccion entre modelos:
# Creo la matriz de predictores:
formint <- as.formula(lnw ~ sex + p6210 + oficio +
  college + totalHoursWorked + sizeFirm +
  p6240_4 + p6585s1a1 + p6585s2a1 +
  p6585s4a1 + p6610s1 + p6630s6a1 + p6630s6a1^2 +
  p7500s3a1^2 + ingtotob^2 + ingtot +
  p6920_3 + iof3h + ingtotob)

# Se calcula el RMSE fuera de muestra:
modelo7 <- lm(formint, data = training)
predictions <- predict(modelo7, testing)
score7 <- RMSE(predictions, testing$lnw)


# Se crea un data frame con los errores de cada modelo:
ERR <- data.frame("MODELO" = rep(NA, 7), "MSE(Muestral)" = rep(NA, 7))
ERR[1, ] <- c("Modelo 1", score1a)
ERR[2, ] <- c("Modelo 2", score2a)
ERR[3, ] <- c("Modelo 3", score3a)
ERR[4, ] <- c("Modelo 4", score4a)
ERR[5, ] <- c("Modelo 5", score5a)
ERR[6, ] <- c("Modelo 6", score6a)
ERR[7, ] <- c("Modelo 7", score7)

stargazer(ERR, summary = F, type = "latex", out = "Views/tabla_mse.tex")

# Errores de prediccion en el test (modelo 6):
predictions = predict(modelo6, testing)
MSEdist = data.frame('ID' = rownames(testing), 'salario_real' = testing$lnw, 'salario_predicho' = predictions) 

MSEdist$MSE = (MSEdist$salario_real - MSEdist$salario_predicho)^2

##Histograma del MSE


histograma_mse <- ggplot(MSEdist, aes(x = MSE)) +
  geom_histogram(color = "darkblue",fill = "darkblue", bins=40) +
  xlab("MSE del modelo 6") +
  ylab("Frecuencia") +
  theme_bw()

histograma_mse

ggsave("Views/histograma_mse.pdf", width = 6, height = 4, plot = histograma_mse)


# Calculo de valores extremos (> 3 veces la desviacion estandard):
sd = sqrt(var(MSEdist$MSE))
crit = 3*sd



MSEdist$missObs = ifelse(MSEdist$MSE > crit, 1, 0)
sum(MSEdist$missObs)

MSEdist$error = MSEdist$salario_real - MSEdist$salario_predicho

# Extraccion de la informacion de estas observaciones n = 23:
ids = MSEdist$ID[MSEdist$missObs == 1]

MISSOBS = testing[rownames(testing) %in% ids,]

# LOOCV:
# Eliminamos cotPension y p6090_1 debido a que mediante el metodo lm sus coeficientes estimados son nulos.

loocv <- trainControl(method = "LOOCV")

# LOOCV para el modelo de forward:
modelfw <- train(formfor,
  data = DFimp,
  method = "lm",
  trControl = loocv
)
scorefw <- RMSE(modelfw$pred$pred, DFimp$lnw)

# LOOCV para el back:
modelbw <- train(formback,
  data = DFimp,
  method = "lm",
  trControl = loocv
)
scorebw <- RMSE(modelbw$pred$pred, DFimp$lnw)
