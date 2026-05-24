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
# Nota: Descargar el dataset "laptop_price.csv" desde Kaggle y guardarlo
# en la carpeta de trabajo. Luego ejecutar:

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
# 3. CORRELACION DE PEARSON
# =============================================================================
# Matriz de correlacion de Pearson (variables cuantitativas)
cormat <- cor(cbind(datos_gaming$Log_Price,
                    datos_gaming$Ram_GB,
                    datos_gaming$Weight_kg),
              method = "pearson")

# Asignar nombres a filas y columnas
colnames(cormat) <- c("Log_Price", "Ram_GB", "Weight_kg")
row.names(cormat) <- c("Log_Price", "Ram_GB", "Weight_kg")
cormat

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

# =============================================================================
# 5. GRAFICOS DE RELACION
# =============================================================================

# Relacion variable Y (Log_Price) y variables numericas (X)
pairs(datos_gaming$Log_Price ~ datos_gaming$Ram_GB + datos_gaming$Weight_kg,
      main = "Relaciones entre Variables Numericas",
      labels = c("Log(Precio)", "RAM (GB)", "Peso (kg)"))

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
