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
# install.packages("ggfortify")
# install.packages("gridExtra")

# Cargar las librerias de los paquetes
library(readr)
library(MASS)
library(ggplot2)
library(lmtest)
library(nortest)
library(ggfortify)
library(gridExtra)

# =============================================================================
# PALETA DE COLORES PROFESIONAL
# =============================================================================
# Paleta inspirada en el ejemplo del profe Anillo
color_principal   <- "#0047AB"    # Azul oscuro profundo
color_secundario  <- "#4682B4"    # Azul acero
color_acento      <- "#B22222"    # Rojo ladrillo para outliers
ncolor_fondo      <- "#F5F5F5"    # Gris muy claro
color_texto       <- "#2F4F4F"    # Gris oscuro

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
# 3. ESTADISTICAS DESCRIPTIVAS
# =============================================================================
print("--- Estadisticas descriptivas ---")
summary(datos[, c("TotalCharges", "tenure", "MonthlyCharges")])

# =============================================================================
# 4. CORRELACION DE PEARSON - TABLA COMPLETA
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
cormat_rounded <- round(cormat, 4)
print("--- MATRIZ REDONDEADA ---")
cormat_rounded

# =============================================================================
# 5. PRUEBA DE CORRELACION (cor.test)
# =============================================================================
# Ho: la correlacion es igual a 0
# H1: la correlacion no es igual a 0

# Correlacion entre TotalCharges y tenure
cor.test(datos$tenure, datos$TotalCharges, method = "pearson")
# r = 0.8259 (fuerte), p < 0.001 -> Se rechaza Ho

# Correlacion entre TotalCharges y MonthlyCharges
cor.test(datos$MonthlyCharges, datos$TotalCharges, method = "pearson")
# r = 0.6511 (moderada), p < 0.001 -> Se rechaza Ho

# Correlacion entre tenure y MonthlyCharges
cor.test(datos$tenure, datos$MonthlyCharges, method = "pearson")
# r = 0.2469 (debil)

# =============================================================================
# 6. GRAFICOS DE RELACION CON PALETA DE COLORES
# =============================================================================

# --- Pairs con todas las variables cuantitativas (colores profesionales) ---
pairs(cuantitativas,
      main = "Matriz de Dispersion - Variables Cuantitativas Telco",
      pch = 19, col = color_principal, gap = 0.5,
      cex.main = 1.2, col.main = color_principal)

# --- Diagramas de dispersion con recta de regression (ggplot) ---

# Figura 1: tenure vs TotalCharges
ggplot(datos, aes(x = tenure, y = TotalCharges)) +
  geom_point(color = color_principal, size = 2, alpha = 0.3) +
  geom_smooth(method = "lm", color = color_acento, se = TRUE, 
              fill = "#FFCCCC", size = 1.2) +
  labs(title = "Relacion: Antiguedad vs Cargos Totales",
       subtitle = paste("r =", round(cor(datos$tenure, datos$TotalCharges), 4)),
       x = "Antiguedad (meses)", y = "Cargos Totales ($)") +
  theme_bw() +
  theme(
    plot.title = element_text(color = color_principal, size = 14, face = "bold"),
    plot.subtitle = element_text(color = color_secundario, size = 11),
    axis.title = element_text(color = color_texto, size = 12),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

# Figura 2: MonthlyCharges vs TotalCharges
ggplot(datos, aes(x = MonthlyCharges, y = TotalCharges)) +
  geom_point(color = color_principal, size = 2, alpha = 0.3) +
  geom_smooth(method = "lm", color = color_acento, se = TRUE,
              fill = "#FFCCCC", size = 1.2) +
  labs(title = "Relacion: Cargos Mensuales vs Cargos Totales",
       subtitle = paste("r =", round(cor(datos$MonthlyCharges, datos$TotalCharges), 4)),
       x = "Cargos Mensuales ($)", y = "Cargos Totales ($)") +
  theme_bw() +
  theme(
    plot.title = element_text(color = color_principal, size = 14, face = "bold"),
    plot.subtitle = element_text(color = color_secundario, size = 11),
    axis.title = element_text(color = color_texto, size = 12),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

# --- Relacion entre variables cualitativas y TotalCharges (boxplots) ---

# Figura 3: Contract vs TotalCharges
ggplot(datos, aes(x = Contract, y = TotalCharges, fill = Contract)) +
  geom_boxplot(
    color = "black",
    alpha = 0.7,
    notch = TRUE,
    notchwidth = 0.8,
    outlier.colour = color_acento,
    outlier.size = 2) +
  scale_fill_manual(values = c("#0047AB", "#4682B4", "#87CEEB")) +
  labs(title = "Relacion Cargos Totales y Tipo de Contrato",
       y = "Cargos Totales ($)",
       x = "Tipo de Contrato") +
  theme_bw() +
  theme(
    plot.title = element_text(color = color_principal, size = 14, face = "bold"),
    axis.title = element_text(color = color_texto, size = 12),
    legend.position = "none"
  )

# Figura 4: InternetService vs TotalCharges
ggplot(datos, aes(x = InternetService, y = TotalCharges, fill = InternetService)) +
  geom_boxplot(
    color = "black",
    alpha = 0.7,
    notch = TRUE,
    notchwidth = 0.8,
    outlier.colour = color_acento,
    outlier.size = 2) +
  scale_fill_manual(values = c("#0047AB", "#4682B4", "#87CEEB")) +
  labs(title = "Relacion Cargos Totales y Servicio de Internet",
       y = "Cargos Totales ($)",
       x = "Servicio de Internet") +
  theme_bw() +
  theme(
    plot.title = element_text(color = color_principal, size = 14, face = "bold"),
    axis.title = element_text(color = color_texto, size = 12),
    legend.position = "none"
  )

# =============================================================================
# 7. CREACION DEL MODELO RLM
# =============================================================================
# Modelo: TotalCharges ~ tenure + MonthlyCharges + Contract + InternetService
modelo <- lm(TotalCharges ~ tenure + MonthlyCharges + Contract + InternetService,
             data = datos)

# Resumen del modelo con R2, R2 ajustado, Fisher, p-valor
summary(modelo)
# R-squared: 0.9001
# Adj R-squared: 0.9000
# Residual standard error: 716.7 on 7025 df
# F-statistic: 10551 on 6 and 7025 DF, p-value < 2.2e-16

# Intervalos de confianza al 95%
confint(modelo)

# =============================================================================
# 8. VALIDACION DE SUPUESTOS - GRAFICAS PROFESIONALES
# =============================================================================

# ---- Linealidad ----
pairs(cuantitativas)
summary(modelo)

# Grafico de residuos vs tenure (colores personalizados)
ggplot(data = datos, aes(x = tenure, y = modelo$residuals)) +
  geom_point(alpha = 0.3, color = color_principal) +
  geom_smooth(color = color_acento, se = FALSE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bw() +
  labs(title = "Residuos vs Antiguedad",
       x = "Antiguedad (meses)", y = "Residuos") +
  theme(
    plot.title = element_text(color = color_principal, size = 14, face = "bold"),
    axis.title = element_text(color = color_texto, size = 12)
  )

# Grafico de residuos vs MonthlyCharges (colores personalizados)
ggplot(data = datos, aes(x = MonthlyCharges, y = modelo$residuals)) +
  geom_point(alpha = 0.3, color = color_principal) +
  geom_smooth(color = color_acento, se = FALSE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bw() +
  labs(title = "Residuos vs Cargos Mensuales",
       x = "Cargos Mensuales ($)", y = "Residuos") +
  theme(
    plot.title = element_text(color = color_principal, size = 14, face = "bold"),
    axis.title = element_text(color = color_texto, size = 12)
  )

# ---- Figura 5: Q-Q Plot de Residuos (con colores profesionales) ----
qq_data <- data.frame(
  teorico = qqnorm(modelo$residuals, plot.it = FALSE)$x,
  muestra = qqnorm(modelo$residuals, plot.it = FALSE)$y
)

ggplot(qq_data, aes(x = teorico, y = muestra)) +
  geom_point(color = color_principal, alpha = 0.5, size = 2) +
  geom_abline(intercept = 0, slope = 1, color = color_acento, 
              linetype = "dashed", size = 1.2) +
  labs(title = "Q-Q Plot de Residuos",
       subtitle = "Prueba de Normalidad",
       x = "Cuantiles Teoricos", y = "Cuantiles Muestra") +
  theme_bw() +
  theme(
    plot.title = element_text(color = color_principal, size = 14, face = "bold"),
    plot.subtitle = element_text(color = color_secundario, size = 11),
    axis.title = element_text(color = color_texto, size = 12)
  )

# Prueba de Shapiro-Wilks
# Ho: Los residuos se distribuyen normalmente
# H1: Los residuos NO se distribuyen normalmente
# NOTA: shapiro.test() solo acepta maximo N = 5000
# Tomamos una muestra aleatoria de 5000 residuos
set.seed(123)
residuos_muestra <- sample(modelo$residuals, 5000)
shapiro.test(residuos_muestra)
# W = 0.9879, p < 0.001
# Nota: Con N = 7032, el test de Shapiro es sensible a desviaciones
# minimas. El Teorema Central del Limite (TLC) garantiza que los
# estimadores son consistentes con muestras grandes.

# ---- Figura 6: Residuos vs Valores Ajustados (diagnostico) ----
residuos_df <- data.frame(
  fitted = modelo$fitted.values,
  residuos = modelo$residuals
)

ggplot(residuos_df, aes(x = fitted, y = residuos)) +
  geom_point(alpha = 0.3, color = color_principal) +
  geom_smooth(color = color_acento, se = FALSE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bw() +
  labs(title = "Residuos vs Valores Ajustados",
       subtitle = "Diagnostico de Independencia y Linealidad",
       x = "Valores Ajustados", y = "Residuos") +
  theme(
    plot.title = element_text(color = color_principal, size = 14, face = "bold"),
    plot.subtitle = element_text(color = color_secundario, size = 11),
    axis.title = element_text(color = color_texto, size = 12)
  )

# ---- Figura 7: Scale-Location (Raiz de residuos estandarizados) ----
residuos_df$sqrt_std_resid <- sqrt(abs(rstandard(modelo)))

ggplot(residuos_df, aes(x = fitted, y = sqrt_std_resid)) +
  geom_point(alpha = 0.3, color = color_principal) +
  geom_smooth(color = color_acento, se = FALSE, size = 1.2) +
  theme_bw() +
  labs(title = "Scale-Location",
       subtitle = "Raiz de Residuos Estandarizados vs Valores Ajustados",
       x = "Valores Ajustados", y = "sqrt(|Residuos Estandarizados|)") +
  theme(
    plot.title = element_text(color = color_principal, size = 14, face = "bold"),
    plot.subtitle = element_text(color = color_secundario, size = 11),
    axis.title = element_text(color = color_texto, size = 12)
  )

# ---- Homocedasticidad ----
# Prueba Breusch-Pagan
# Ho: Los residuos son homocedasticos
# H1: Los residuos son heterocedasticos
bptest(modelo)
# Se detecta heterocedasticidad con N grande.
# Los estimadores siguen siendo consistentes.

# ---- Multicolinealidad (VIF) ----
library(car)
vif(modelo)
# tenure         MonthlyCharges 
#   2.615            2.615
# VIF < 5 -> Sin multicolinealidad
# Durbin-Watson test
# Ho: Los residuos son independientes
# H1: Los residuos son dependientes
dwtest(modelo, alternative = "two.sided")
# DW = 2.018, p > 0.05 -> No se rechaza Ho
# Los residuos son independientes

# ---- Figura 8: Graficos de Diagnostico Completos (2x2) ----
# Los 4 graficos clasicos de diagnostico con par(mfrow=c(2,2))
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# 1. Residuals vs Fitted
plot(modelo, which = 1,
     main = "Residuos vs Valores Ajustados",
     col = color_principal, pch = 20,
     cex.main = 1.1, col.main = color_principal)

# 2. Normal Q-Q
plot(modelo, which = 2,
     main = "Q-Q Plot de Residuos",
     col = color_principal, pch = 20,
     cex.main = 1.1, col.main = color_principal)

# 3. Scale-Location
plot(modelo, which = 3,
     main = "Scale-Location",
     col = color_principal, pch = 20,
     cex.main = 1.1, col.main = color_principal)

# 4. Residuals vs Leverage
plot(modelo, which = 5,
     main = "Residuos vs Leverage",
     col = color_principal, pch = 20,
     cex.main = 1.1, col.main = color_principal)

# Restaurar par(mfrow)
par(mfrow = c(1, 1))

# =============================================================================
# 9. PREDICCION (Opcional)
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
