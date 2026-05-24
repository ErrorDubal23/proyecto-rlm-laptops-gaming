# =============================================================================
# PROYECTO FINAL - ANALISIS DE DATOS 1
# REGRESION LINEAL MULTIPLE: PREDICCION DE PRECIOS DE LAPTOPS GAMING
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
# Fuente: Laptop Price Prediction (Muhammet Varlı, Kaggle)
# URL original: https://www.kaggle.com/datasets/muhammetvarl/laptop-price
# Descargado via mirror: https://github.com/MainakRepositor/Datasets
# N = 1303 laptops, 13 variables
# Nota: Guardar el archivo "laptop_price.csv" en la carpeta de trabajo

datos <- read_csv("laptop_price.csv",
                  locale = locale(encoding = "latin1"))

# Imprimiendo las primeras 6 observaciones
head(datos)

# Variables de la base de datos
names(datos)

# Observando la estructura de los datos
str(datos)

# =============================================================================
# 2. LIMPIEZA Y PREPARACION
# =============================================================================
# Extraer valores numericos de las columnas de texto
datos$Ram_GB <- as.numeric(gsub("GB", "", datos$Ram))
datos$Weight_kg <- as.numeric(gsub("kg", "", datos$Weight))
datos$Inches <- as.numeric(datos$Inches)
datos$Price_euros <- as.numeric(datos$Price_euros)

# Crear variables cualitativas limpias
datos$CPU_Brand <- ifelse(grepl("Intel", datos$Cpu), "Intel",
                   ifelse(grepl("AMD", datos$Cpu), "AMD", "Other"))

datos$GPU_Brand <- ifelse(grepl("Nvidia|NVIDIA", datos$Gpu), "Nvidia",
                   ifelse(grepl("Intel", datos$Gpu), "Intel",
                   ifelse(grepl("AMD", datos$Gpu), "AMD", "Other")))

# Convirtiendo variables cualitativas a factor
datos$CPU_Brand <- factor(datos$CPU_Brand)
datos$GPU_Brand <- factor(datos$GPU_Brand)
datos$TypeName <- factor(datos$TypeName)

str(datos$CPU_Brand)
str(datos$GPU_Brand)

# Filtrar solo laptops Gaming
datos_gaming <- datos[datos$TypeName == "Gaming", ]
datos_gaming <- datos_gaming[complete.cases(datos_gaming$Price_euros,
                                            datos_gaming$Ram_GB,
                                            datos_gaming$Weight_kg), ]

# Transformacion logaritmica del precio para mejorar normalidad
datos_gaming$Log_Price <- log(datos_gaming$Price_euros)

# =============================================================================
# 3. CORRELACION DE PEARSON - TABLA COMPLETA
# =============================================================================

# Variables cuantitativas del dataset de laptops
cuantitativas <- data.frame(
  Log_Price   = datos_gaming$Log_Price,
  Price_euros = datos_gaming$Price_euros,
  Ram_GB      = datos_gaming$Ram_GB,
  Weight_kg   = datos_gaming$Weight_kg,
  Inches      = datos_gaming$Inches
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

# Correlacion entre Log_Price y Ram_GB
cor.test(datos_gaming$Ram_GB, datos_gaming$Log_Price, method = "pearson")
# p-value < alpha (0.05) -> Se rechaza Ho

# Correlacion entre Log_Price y Weight_kg
cor.test(datos_gaming$Weight_kg, datos_gaming$Log_Price, method = "pearson")
# p-value < alpha (0.05) -> Se rechaza Ho

# Correlacion entre Log_Price y Inches
cor.test(datos_gaming$Inches, datos_gaming$Log_Price, method = "pearson")
# Informacion adicional

# Correlacion entre Ram_GB y Weight_kg
cor.test(datos_gaming$Ram_GB, datos_gaming$Weight_kg, method = "pearson")

# =============================================================================
# 5. GRAFICOS DE RELACION
# =============================================================================

# --- Pairs con TODAS las variables cuantitativas ---
pairs(cuantitativas,
      main = "Matriz de Dispersion - Todas las Variables Cuantitativas",
      pch = 19, col = "steelblue", gap = 0.5)

# --- Pairs de las variables del modelo (Y vs X numericas) ---
pairs(Log_Price ~ Ram_GB + Weight_kg,
      data = datos_gaming,
      main = "Relaciones: Log(Precio) vs Variables Numericas",
      labels = c("Log(Precio)", "RAM (GB)", "Peso (kg)"),
      pch = 19, col = "steelblue")

# --- Diagramas de dispersion con recta de regression ---

# RAM vs Log_Price (con linea de ajuste)
ggplot(datos_gaming, aes(x = Ram_GB, y = Log_Price)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) +
  labs(title = "Relacion: RAM vs Log(Precio)",
       subtitle = paste("r =", round(cor(datos_gaming$Ram_GB, datos_gaming$Log_Price), 3)),
       x = "RAM (GB)", y = "Log(Precio)") +
  theme_bw()

# Peso vs Log_Price (con linea de ajuste)
ggplot(datos_gaming, aes(x = Weight_kg, y = Log_Price)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) +
  labs(title = "Relacion: Peso vs Log(Precio)",
       subtitle = paste("r =", round(cor(datos_gaming$Weight_kg, datos_gaming$Log_Price), 3)),
       x = "Peso (kg)", y = "Log(Precio)") +
  theme_bw()

# Pulgadas vs Log_Price (con linea de ajuste) - informativo, no en modelo
ggplot(datos_gaming, aes(x = Inches, y = Log_Price)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) +
  labs(title = "Relacion: Pulgadas vs Log(Precio)",
       subtitle = paste("r =", round(cor(datos_gaming$Inches, datos_gaming$Log_Price, use = "complete.obs"), 3)),
       x = "Pulgadas", y = "Log(Precio)") +
  theme_bw()

# RAM vs Peso (correlacion entre predictoras)
ggplot(datos_gaming, aes(x = Ram_GB, y = Weight_kg)) +
  geom_point(color = "darkgreen", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) +
  labs(title = "Relacion: RAM vs Peso",
       subtitle = paste("r =", round(cor(datos_gaming$Ram_GB, datos_gaming$Weight_kg), 3)),
       x = "RAM (GB)", y = "Peso (kg)") +
  theme_bw()

# Relacion entre Y (Log_Price) y variable cualitativa CPU_Brand
ggplot(datos_gaming, aes(x = CPU_Brand, y = Log_Price, fill = CPU_Brand)) +
  geom_boxplot(
    color = "black",
    alpha = 0.5,
    notch = TRUE,
    notchwidth = 0.8,
    outlier.colour = "red",
    outlier.size = 3) +
  labs(title = "Relacion Precio y Marca de CPU",
       y = "Log(Precio)",
       x = "Marca de CPU")

# Relacion entre Y (Log_Price) y variable cualitativa GPU_Brand
ggplot(datos_gaming, aes(x = GPU_Brand, y = Log_Price, fill = GPU_Brand)) +
  geom_boxplot(
    color = "black",
    alpha = 0.5,
    notch = TRUE,
    notchwidth = 0.8,
    outlier.colour = "red",
    outlier.size = 3) +
  labs(title = "Relacion Precio y Marca de GPU",
       y = "Log(Precio)",
       x = "Marca de GPU")

# =============================================================================
# 6. CREACION DEL MODELO RLM
# =============================================================================
# Modelo con transformacion logaritmica en Y
modelo <- lm(Log_Price ~ Ram_GB + Weight_kg + CPU_Brand + GPU_Brand,
             data = datos_gaming)

# Resumen del modelo
summary(modelo)

# Intervalos de confianza
confint(modelo)

# =============================================================================
# 7. VALIDACION DE SUPUESTOS
# =============================================================================

# ---- Linealidad ----
pairs(datos_gaming$Log_Price ~ datos_gaming$Ram_GB + datos_gaming$Weight_kg)
summary(modelo)
# R-squared ajustado: 0.534
# Aproximadamente el 53.4% de las variaciones en el log(precio)
# es explicada por el modelo,
# mientras que el 46.6% es explicado por las perturbaciones.

# Grafico de residuos vs variable numerica (Ram_GB)
ggplot(data = datos_gaming, aes(x = Ram_GB, y = modelo$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick") +
  geom_hline(yintercept = 0) +
  theme_bw()

# Grafico de residuos vs variable numerica (Weight_kg)
ggplot(data = datos_gaming, aes(x = Weight_kg, y = modelo$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick") +
  geom_hline(yintercept = 0) +
  theme_bw()

# ---- Normalidad de residuos ----
par(mfrow = c(1, 1))
qqnorm(modelo$residuals)
qqline(modelo$residuals)

# Prueba de Shapiro-Wilks
# Ho: Los residuos se distribuyen normalmente
# H1: Los residuos NO se distribuyen normalmente
shapiro.test(modelo$residuals)
# p-value = 0.552 > alpha (0.05) -> No se rechaza Ho

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
  Ram_GB = 32,
  Weight_kg = 2.5,
  CPU_Brand = factor("Intel", levels = levels(datos_gaming$CPU_Brand)),
  GPU_Brand = factor("Nvidia", levels = levels(datos_gaming$GPU_Brand))
)

# Prediccion puntual
predict(modelo, newdata = nuevo)

# Prediccion por intervalo
predict(modelo, newdata = nuevo, interval = "prediction", level = 0.95)
