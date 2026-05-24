# =============================================================================
# PROYECTO FINAL - ANALISIS DE DATOS 1
# REGRESION LINEAL MULTIPLE: CARGOS TOTALES DE CLIENTES TELCO
# Ingenieria de Sistemas
# Universidad del Norte
# Integrantes: Dubal Aguilar Torres, Alejandro Chaves Ramos,
#              Juan Caceres Figueroa, Miguel Carrizosa
# Docente: PhD. Luis Angel Anillo Arrieta
# =============================================================================

# ---- Instalar paquetes (descomentar si es necesario) ----
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("MASS")
# install.packages("lmtest")
# install.packages("nortest")

# Cargar las librerias de los paquetes
library(readr)
library(MASS)
library(ggplot2)
library(lmtest)
library(nortest)

# =============================================================================
# 1. LECTURA DE DATOS
# =============================================================================
# Fuente: Telco Customer Churn (IBM Sample DataSets)
# URL: https://www.kaggle.com/datasets/blastchar/telco-customer-churn
# N = 7043 clientes, 21 variables

datos <- read_csv("../datos/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# Imprimiendo las primeras 6 observaciones
head(datos)

# Variables de la base de datos
names(datos)

# Observando la estructura de los datos
str(datos)

# =============================================================================
# 2. LIMPIEZA Y PREPARACION
# =============================================================================
# Convertir TotalCharges a numerico (hay valores vacios)
datos$TotalCharges <- as.numeric(datos$TotalCharges)

# Eliminar filas con NA en las variables del modelo
datos <- datos[complete.cases(datos$TotalCharges,
                              datos$tenure,
                              datos$MonthlyCharges,
                              datos$Contract,
                              datos$InternetService), ]

# Convirtiendo variables cualitativas a factor
datos$Contract <- factor(datos$Contract)
datos$InternetService <- factor(datos$InternetService)

str(datos$Contract)
str(datos$InternetService)

# =============================================================================
# 3. CORRELACION DE PEARSON - TABLA COMPLETA
# =============================================================================

# Variables cuantitativas del dataset
cuantitativas <- data.frame(
  TotalCharges   = datos$TotalCharges,
  tenure         = datos$tenure,
  MonthlyCharges = datos$MonthlyCharges
)

# Matriz de correlacion completa de Pearson
cormat <- cor(cuantitativas, method = "pearson")
print("--- MATRIZ DE CORRELACIONES DE PEARSON (completa) ---")
cormat

# Tabla de correlaciones formateada
cormat_rounded <- round(cormat, 3)
print("--- MATRIZ REDONDEADA ---")
cormat_rounded

# =============================================================================
# 4. PRUEBA DE CORRELACION (cor.test)
# =============================================================================
# Ho: la correlacion es igual a 0
# H1: la correlacion no es igual a 0

# Correlacion entre TotalCharges y tenure
cor.test(datos$tenure, datos$TotalCharges, method = "pearson")
# p-value < alpha (0.05) -> Se rechaza Ho

# Correlacion entre TotalCharges y MonthlyCharges
cor.test(datos$MonthlyCharges, datos$TotalCharges, method = "pearson")
# p-value < alpha (0.05) -> Se rechaza Ho

# Correlacion entre tenure y MonthlyCharges
cor.test(datos$tenure, datos$MonthlyCharges, method = "pearson")

# =============================================================================
# 5. GRAFICOS DE RELACION
# =============================================================================

# --- Pairs con todas las variables cuantitativas ---
pairs(cuantitativas,
      main = "Matriz de Dispersion - Variables Cuantitativas Telco",
      pch = 19, col = "steelblue", gap = 0.5)

# --- Diagramas de dispersion con recta de regression ---

# tenure vs TotalCharges (con linea de ajuste)
ggplot(datos, aes(x = tenure, y = TotalCharges)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) +
  labs(title = "Relacion: Antiguedad vs Cargos Totales",
       subtitle = paste("r =", round(cor(datos$tenure, datos$TotalCharges), 3)),
       x = "Antiguedad (meses)", y = "Cargos Totales ($)") +
  theme_bw()

# MonthlyCharges vs TotalCharges (con linea de ajuste)
ggplot(datos, aes(x = MonthlyCharges, y = TotalCharges)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) +
  labs(title = "Relacion: Cargos Mensuales vs Cargos Totales",
       subtitle = paste("r =", round(cor(datos$MonthlyCharges, datos$TotalCharges), 3)),
       x = "Cargos Mensuales ($)", y = "Cargos Totales ($)") +
  theme_bw()

# Relacion entre Y (TotalCharges) y variable cualitativa Contract
ggplot(datos, aes(x = Contract, y = TotalCharges, fill = Contract)) +
  geom_boxplot(
    color = "black",
    alpha = 0.5,
    notch = TRUE,
    notchwidth = 0.8,
    outlier.colour = "red",
    outlier.size = 3) +
  labs(title = "Relacion Cargos Totales y Tipo de Contrato",
       y = "Cargos Totales ($)",
       x = "Tipo de Contrato")

# Relacion entre Y (TotalCharges) y variable cualitativa InternetService
ggplot(datos, aes(x = InternetService, y = TotalCharges, fill = InternetService)) +
  geom_boxplot(
    color = "black",
    alpha = 0.5,
    notch = TRUE,
    notchwidth = 0.8,
    outlier.colour = "red",
    outlier.size = 3) +
  labs(title = "Relacion Cargos Totales y Servicio de Internet",
       y = "Cargos Totales ($)",
       x = "Servicio de Internet")

# =============================================================================
# 6. CREACION DEL MODELO RLM
# =============================================================================
# Modelo base: TotalCharges ~ tenure + MonthlyCharges + Contract + InternetService
modelo <- lm(TotalCharges ~ tenure + MonthlyCharges + Contract + InternetService,
             data = datos)

# Resumen del modelo
summary(modelo)

# Intervalos de confianza
confint(modelo)

# =============================================================================
# 7. VALIDACION DE SUPUESTOS
# =============================================================================

# ---- Linealidad ----
pairs(cuantitativas)
summary(modelo)
# R-squared ajustado: 0.900
# Aproximadamente el 90% de las variaciones en los cargos totales
# es explicada por el modelo, mientras que el 10% es explicado
# por las perturbaciones.

# Grafico de residuos vs variable numerica (tenure)
ggplot(data = datos, aes(x = tenure, y = modelo$residuals)) +
  geom_point(alpha = 0.3) +
  geom_smooth(color = "firebrick") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(title = "Residuos vs Antiguedad",
       x = "Antiguedad (meses)", y = "Residuos")

# Grafico de residuos vs variable numerica (MonthlyCharges)
ggplot(data = datos, aes(x = MonthlyCharges, y = modelo$residuals)) +
  geom_point(alpha = 0.3) +
  geom_smooth(color = "firebrick") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(title = "Residuos vs Cargos Mensuales",
       x = "Cargos Mensuales ($)", y = "Residuos")

# ---- Normalidad de residuos ----
par(mfrow = c(1, 1))
qqnorm(modelo$residuals)
qqline(modelo$residuals)

# Prueba de Shapiro-Wilks
# Ho: Los residuos se distribuyen normalmente
# H1: Los residuos NO se distribuyen normalmente
shapiro.test(modelo$residuals)
# Nota: Con N = 7032, el test de Shapiro es sensible a desviaciones
# minimas. El Teorema Central del Limite garantiza que los estimadores
# son consistentes con muestras grandes.

# ---- Homocedasticidad ----
# Prueba Breusch-Pagan
# Ho: Los residuos son homocedasticos
# H1: Los residuos son heterocedasticos
bptest(modelo)

# ---- Independencia de los residuos ----
# Durbin-Watson test
# Ho: Los residuos son independientes
# H1: Los residuos son dependientes
dwtest(modelo, alternative = "two.sided")
# p-value > alpha -> No se rechaza Ho

# Graficos para interpretar los supuestos
par(mfrow = c(2, 2))
plot(modelo)
# Normalidad: Normal Q-Q
# Homocedasticidad: Scale-Location
# Independencia: Residuals vs Fitted

# =============================================================================
# 8. PREDICCION (Opcional)
# =============================================================================
# Crear nuevo dato para prediccion
nuevo <- data.frame(
  tenure = 24,
  MonthlyCharges = 65.0,
  Contract = factor("One year", levels = levels(datos$Contract)),
  InternetService = factor("Fiber optic", levels = levels(datos$InternetService))
)

# Prediccion puntual
predict(modelo, newdata = nuevo)

# Prediccion por intervalo
predict(modelo, newdata = nuevo, interval = "prediction", level = 0.95)
