# =============================================================================
# PROYECTO FINAL - ANALISIS DE DATOS 1
# REGRESION LINEAL MULTIPLE: PREDICCION DE PRECIOS DE LAPTOPS
# Ingenieria de Sistemas
# Universidad del Norte
# Integrantes: Dubal Aguilar Torres, Alejandro Chaves Ramos,
#              Juan Caceres Figueroa, Miguel Carrizosa
# Docente: PhD. Luis Angel Anillo Arrieta
# Fecha: 25 de mayo de 2026
# =============================================================================

# ---- Instalar paquetes (descomentar si es necesario) ----
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("MASS")
# install.packages("lmtest")
# install.packages("nortest")
# install.packages("ggfortify")
# install.packages("gridExtra")
# install.packages("car")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("knitr")
# install.packages("rmarkdown")

# Cargar las librerias de los paquetes
library(readr)
library(dplyr)
library(stringr)
library(MASS)
library(ggplot2)
library(lmtest)
library(nortest)
library(ggfortify)
library(gridExtra)
library(car)

# =============================================================================
# CONFIGURACION DE PALETA DE COLORES
# =============================================================================

color_principal   <- "#0047AB"    # Azul oscuro profundo
color_secundario  <- "#4682B4"    # Azul acero
color_acento      <- "#B22222"    # Rojo ladrillo para outliers
color_fondo       <- "#F5F5F5"    # Gris muy claro
color_texto       <- "#2F4F4F"    # Gris oscuro

# =============================================================================
# 1. LECTURA DE DATOS
# =============================================================================
# Fuente: Laptop Price Prediction Dataset (Kaggle)
# URL: https://www.kaggle.com/datasets/arnabchaki/laptop-price-prediction
# N = 1303 laptops, 13 variables
# Nota: Guardar el archivo "laptop_price.csv" en la carpeta de trabajo

datos <- read_csv("laptop_price.csv",
                  locale = locale(encoding = "UTF-8"))

# Imprimiendo las primeras 6 observaciones
cat("--- Primeras 6 filas del dataset ---\n")
head(datos)

# Variables de la base de datos
cat("\n--- Nombres de las variables ---\n")
names(datos)

# Observando la estructura de los datos
cat("\n--- Estructura de los datos ---\n")
str(datos)

# =============================================================================
# 2. LIMPIEZA Y PREPARACION DE DATOS
# =============================================================================

cat("\n--- LIMPIEZA DE DATOS ---\n")

# Extraer RAM en GB (eliminar "GB" y convertir a numerico)
datos$Ram_GB <- as.numeric(gsub("GB", "", datos$Ram))

# Extraer peso en kg (eliminar "kg" y convertir a numerico)
datos$Weight_kg <- as.numeric(gsub("kg", "", datos$Weight))

# Extraer frecuencia del CPU en GHz
datos$CPU_GHz <- as.numeric(str_extract(datos$Cpu, "\\d+\\.?\\d*(?=GHz)"))

# Tamano de pantalla (ya es numerico)
datos$Inches <- as.numeric(datos$Inches)

# Precio en euros (ya es numerico)
datos$Price_euros <- as.numeric(datos$Price_euros)

# Extraer resolucion de pantalla (ancho x alto)
datos$Res_Width <- as.numeric(str_extract(datos$ScreenResolution, "\\d+(?=x)"))
datos$Res_Height <- as.numeric(str_extract(datos$ScreenResolution, "(?<=x)\\d+"))

# Crear variable cualitativa: Marca del CPU
datos$CPU_Brand <- ifelse(grepl("Intel", datos$Cpu), "Intel",
                   ifelse(grepl("AMD", datos$Cpu), "AMD", "Other"))

# Crear variable cualitativa: Marca de la GPU
datos$GPU_Brand <- ifelse(grepl("Nvidia|NVIDIA", datos$Gpu), "Nvidia",
                   ifelse(grepl("Intel", datos$Gpu), "Intel",
                   ifelse(grepl("AMD", datos$Gpu), "AMD", "Other")))

# CORRECCION IMPORTANTE: Consolidar categorias de OpSys con pocas observaciones
# Categorias originales: Windows 10(1072), No OS(66), Linux(62), Windows 7(45), 
#                        Chrome OS(27), macOS(13), Mac OS X(8), Windows 10 S(8), Android(2)
# Consolidamos las que tienen < 15 observaciones para evitar inestabilidad

datos$OpSys_Consolidado <- datos$OpSys

# Consolidar categorias con menos de 15 observaciones
levels_to_group <- names(table(datos$OpSys)[table(datos$OpSys) < 15])
cat("Categorias de OpSys consolidadas (menos de 15 obs):", 
    paste(levels_to_group, collapse = ", "), "\n")

datos$OpSys_Consolidado[datos$OpSys %in% levels_to_group] <- "Otros"
datos$OpSys_Consolidado <- factor(datos$OpSys_Consolidado)

# Convirtiendo variables cualitativas a factor
datos$CPU_Brand <- factor(datos$CPU_Brand)
datos$GPU_Brand <- factor(datos$GPU_Brand)
datos$TypeName <- factor(datos$TypeName)
datos$Company <- factor(datos$Company)
datos$OpSys <- factor(datos$OpSys)

# Filtrar solo filas completas para las variables del modelo
# Variables del modelo: Price_euros, Ram_GB, CPU_GHz, Inches, TypeName, OpSys_Consolidado
datos <- datos[complete.cases(
  datos$Price_euros,
  datos$Ram_GB,
  datos$CPU_GHz,
  datos$Inches,
  datos$TypeName,
  datos$OpSys_Consolidado
), ]

cat(paste("Registros despues de limpieza:", nrow(datos), "\n"))
cat("\n--- Frecuencia de OpSys consolidado ---\n")
print(table(datos$OpSys_Consolidado))

# =============================================================================
# 3. ESTADISTICA DESCRIPTIVA
# =============================================================================

cat("\n--- ESTADISTICAS DESCRIPTIVAS ---\n")

# Variables cuantitativas del dataset
cuantitativas <- data.frame(
  Price_euros = datos$Price_euros,
  Ram_GB      = datos$Ram_GB,
  CPU_GHz     = datos$CPU_GHz,
  Inches      = datos$Inches,
  Weight_kg   = datos$Weight_kg,
  Res_Width   = datos$Res_Width
)

summary(cuantitativas)

# Detectar outliers en precio (metodo IQR)
Q1 <- quantile(datos$Price_euros, 0.25, na.rm = TRUE)
Q3 <- quantile(datos$Price_euros, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR
outliers_precio <- datos$Price_euros < limite_inferior | datos$Price_euros > limite_superior

cat("\n--- DETECCION DE OUTLIERS EN PRECIO ---\n")
cat(paste("Limite inferior:", round(limite_inferior, 2), "EUR\n"))
cat(paste("Limite superior:", round(limite_superior, 2), "EUR\n"))
cat(paste("Numero de outliers:", sum(outliers_precio), "(", 
          round(sum(outliers_precio)/nrow(datos)*100, 1), "%)\n"))

# =============================================================================
# 4. CORRELACION DE PEARSON Y TABLAS
# =============================================================================

cat("\n--- MATRIZ DE CORRELACIONES DE PEARSON ---\n")

# Matriz de correlacion completa de Pearson (solo variables numericas del modelo)
vars_num_modelo <- data.frame(
  Price_euros = datos$Price_euros,
  Ram_GB      = datos$Ram_GB,
  CPU_GHz     = datos$CPU_GHz,
  Inches      = datos$Inches
)

cormat <- cor(vars_num_modelo, method = "pearson", use = "complete.obs")
cat("--- MATRIZ DE CORRELACIONES DE PEARSON ---\n")
print(round(cormat, 4))

# =============================================================================
# 5. PRUEBA DE CORRELACION
# =============================================================================

cat("\n--- PRUEBAS DE CORRELACION ---\n")
# Ho: la correlacion es igual a 0
# H1: la correlacion no es igual a 0

# Correlacion entre Price_euros y Ram_GB
cat("\n=== Correlacion Price vs Ram_GB ===\n")
cor.test(datos$Ram_GB, datos$Price_euros, method = "pearson")

# Correlacion entre Price_euros y CPU_GHz
cat("\n=== Correlacion Price vs CPU_GHz ===\n")
cor.test(datos$CPU_GHz, datos$Price_euros, method = "pearson")

# Correlacion entre Price_euros e Inches
cat("\n=== Correlacion Price vs Inches ===\n")
cor.test(datos$Inches, datos$Price_euros, method = "pearson")

# =============================================================================
# 6. GRAFICOS DE RELACION
# =============================================================================

cat("\n--- GENERANDO GRAFICOS DE CORRELACION ---\n")

# --- Pairs con variables cuantitativas seleccionadas ---
pairs(vars_num_modelo,
      main = "Matriz de Dispersion - Variables del Modelo",
      pch = 19, col = color_principal, gap = 0.5,
      cex.main = 1.2, col.main = color_principal)

# --- Diagramas de dispersion con recta de regression (ggplot) ---

# Figura 1: Ram_GB vs Price_euros
ggplot(datos, aes(x = Ram_GB, y = Price_euros)) +
  geom_point(color = color_principal, size = 2, alpha = 0.4) +
  geom_smooth(method = "lm", color = color_acento, se = TRUE, 
              fill = "#FFCCCC", size = 1.2) +
  labs(title = "Relacion: RAM vs Precio del Laptop",
       subtitle = paste("r =", round(cor(datos$Ram_GB, datos$Price_euros), 4)),
       x = "RAM (GB)", y = "Precio (EUR)") +
  theme_bw() +
  theme(
    plot.title = element_text(color = color_principal, size = 14, face = "bold"),
    plot.subtitle = element_text(color = color_secundario, size = 11),
    axis.title = element_text(color = color_texto, size = 12),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

# Figura 2: CPU_GHz vs Price_euros
ggplot(datos, aes(x = CPU_GHz, y = Price_euros)) +
  geom_point(color = color_principal, size = 2, alpha = 0.4) +
  geom_smooth(method = "lm", color = color_acento, se = TRUE,
              fill = "#FFCCCC", size = 1.2) +
  labs(title = "Relacion: Frecuencia CPU vs Precio del Laptop",
       subtitle = paste("r =", round(cor(datos$CPU_GHz, datos$Price_euros, use = "complete.obs"), 4)),
       x = "Frecuencia CPU (GHz)", y = "Precio (EUR)") +
  theme_bw() +
  theme(
    plot.title = element_text(color = color_principal, size = 14, face = "bold"),
    plot.subtitle = element_text(color = color_secundario, size = 11),
    axis.title = element_text(color = color_texto, size = 12),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

# Figura 3: Inches vs Price_euros
ggplot(datos, aes(x = Inches, y = Price_euros)) +
  geom_point(color = color_principal, size = 2, alpha = 0.4) +
  geom_smooth(method = "lm", color = color_acento, se = TRUE,
              fill = "#FFCCCC", size = 1.2) +
  labs(title = "Relacion: Tamano de Pantalla vs Precio del Laptop",
       subtitle = paste("r =", round(cor(datos$Inches, datos$Price_euros, use = "complete.obs"), 4)),
       x = "Tamano de Pantalla (pulgadas)", y = "Precio (EUR)") +
  theme_bw() +
  theme(
    plot.title = element_text(color = color_principal, size = 14, face = "bold"),
    plot.subtitle = element_text(color = color_secundario, size = 11),
    axis.title = element_text(color = color_texto, size = 12),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

# --- Relacion entre variables cualitativas y Price (boxplots) ---

# Figura 4: TypeName vs Price_euros
ggplot(datos, aes(x = TypeName, y = Price_euros, fill = TypeName)) +
  geom_boxplot(
    color = "black",
    alpha = 0.7,
    notch = TRUE,
    notchwidth = 0.8,
    outlier.colour = color_acento,
    outlier.size = 2) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Relacion Precio y Tipo de Laptop",
       y = "Precio (EUR)",
       x = "Tipo de Laptop") +
  theme_bw() +
  theme(
    plot.title = element_text(color = color_principal, size = 14, face = "bold"),
    axis.title = element_text(color = color_texto, size = 12),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Figura 5: OpSys_Consolidado vs Price_euros
ggplot(datos, aes(x = OpSys_Consolidado, y = Price_euros, fill = OpSys_Consolidado)) +
  geom_boxplot(
    color = "black",
    alpha = 0.7,
    notch = TRUE,
    notchwidth = 0.8,
    outlier.colour = color_acento,
    outlier.size = 2) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Relacion Precio y Sistema Operativo",
       y = "Precio (EUR)",
       x = "Sistema Operativo") +
  theme_bw() +
  theme(
    plot.title = element_text(color = color_principal, size = 14, face = "bold"),
    axis.title = element_text(color = color_texto, size = 12),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# =============================================================================
# 7. CREACION DEL MODELO RLM
# =============================================================================

cat("\n--- CREACION DEL MODELO RLM ---\n")

# Modelo: Price_euros ~ Ram_GB + CPU_GHz + Inches + TypeName + OpSys_Consolidado
# NOTA: Se usa Price_euros directamente (SIN transformacion logaritmica)
# para cumplir con los requisitos del enunciado

modelo <- lm(Price_euros ~ Ram_GB + CPU_GHz + Inches + TypeName + OpSys_Consolidado, data = datos)

# Resumen del modelo con R2, R2 ajustado, Fisher, p-valor
cat("\n--- RESUMEN DEL MODELO ---\n")
summary(modelo)

# Intervalos de confianza al 95%
cat("\n--- INTERVALOS DE CONFIANZA (95%) ---\n")
confint(modelo)

# =============================================================================
# 8. VALIDACION DE SUPUESTOS
# =============================================================================

cat("\n--- VALIDACION DE SUPUESTOS ---\n")

# ---- Linealidad ----
cat("\n--- LINEALIDAD ---\n")

# Grafico de residuos vs Ram_GB
ggplot(data = datos, aes(x = Ram_GB, y = modelo$residuals)) +
  geom_point(alpha = 0.3, color = color_principal) +
  geom_smooth(color = color_acento, se = FALSE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bw() +
  labs(title = "Residuos vs RAM",
       x = "RAM (GB)", y = "Residuos") +
  theme(
    plot.title = element_text(color = color_principal, size = 14, face = "bold"),
    axis.title = element_text(color = color_texto, size = 12)
  )

# Grafico de residuos vs CPU_GHz
ggplot(data = datos, aes(x = CPU_GHz, y = modelo$residuals)) +
  geom_point(alpha = 0.3, color = color_principal) +
  geom_smooth(color = color_acento, se = FALSE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bw() +
  labs(title = "Residuos vs Frecuencia CPU",
       x = "CPU (GHz)", y = "Residuos") +
  theme(
    plot.title = element_text(color = color_principal, size = 14, face = "bold"),
    axis.title = element_text(color = color_texto, size = 12)
  )

# ---- Normalidad ----
cat("\n--- NORMALIDAD DE RESIDUOS ---\n")

# Figura 6: Q-Q Plot de Residuos (con colores profesionales)
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
# Tomamos una muestra aleatoria de 5000 residuos si N > 5000
if(length(modelo$residuals) > 5000) {
  set.seed(123)
  residuos_muestra <- sample(modelo$residuals, 5000)
} else {
  residuos_muestra <- modelo$residuals
}
cat("\n--- Prueba Shapiro-Wilk ---\n")
shapiro.test(residuos_muestra)

# Prueba de Jarque-Bera
cat("\n--- Prueba Jarque-Bera ---\n")
jarque.bera.test(modelo$residuals)

# ---- Independencia ----
cat("\n--- INDEPENDENCIA ---\n")

# Figura 7: Residuos vs Valores Ajustados
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

# Durbin-Watson test
# Ho: Los residuos son independientes
# H1: Los residuos son dependientes
cat("\n--- Prueba Durbin-Watson ---\n")
dwtest(modelo, alternative = "two.sided")

# ---- Homocedasticidad ----
cat("\n--- HOMOCEDASTICIDAD ---\n")

# Figura 8: Scale-Location
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

# Prueba Breusch-Pagan
# Ho: Los residuos son homocedasticos
# H1: Los residuos son heterocedasticos
cat("\n--- Prueba Breusch-Pagan ---\n")
bptest(modelo)

# ---- Multicolinealidad (VIF) ----
cat("\n--- MULTICOLINEALIDAD (VIF) ---\n")

# IMPORTANTE: vif() en el paquete car usa vif(lm) para modelos
# Cuando hay factores, devuelve GVIF (Generalized VIF)
# GVIF^(1/(2*df)) es el equivalente al VIF estandar
vif_result <- vif(modelo)
print(vif_result)

cat("\n--- INTERPRETACION VIF ---\n")
cat("VIF < 5: Sin multicolinealidad problematica\n")
cat("VIF 5-10: Multicolinealidad moderada\n")
cat("VIF > 10: Multicolinealidad severa\n")

# =============================================================================
# 9. GRAFICOS DE DIAGNOSTICO COMPLETOS
# =============================================================================

cat("\n--- GRAFICOS DE DIAGNOSTICO (2x2) ---\n")

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
# 10. PREDICCION (Opcional)
# =============================================================================

cat("\n--- PREDICCION ---\n")

# Crear nuevo dato para prediccion
# IMPORTANTE: Usar los levels del factor OpSys_Consolidado
nuevo <- data.frame(
  Ram_GB = 16,
  CPU_GHz = 2.8,
  Inches = 15.6,
  TypeName = factor("Gaming", levels = levels(datos$TypeName)),
  OpSys_Consolidado = factor("Windows 10", levels = levels(datos$OpSys_Consolidado))
)

# Prediccion puntual
cat("Prediccion puntual:\n")
predict(modelo, newdata = nuevo)

# Prediccion por intervalo
cat("\nPrediccion por intervalo (95%):\n")
predict(modelo, newdata = nuevo, interval = "prediction", level = 0.95)

# =============================================================================
# 11. RESUMEN DE RESULTADOS PARA EL DOCUMENTO
# =============================================================================

cat("\n")
cat("=" ,rep("=", 70), "\n", sep="")
cat("RESUMEN DE RESULTADOS DEL MODELO\n")
cat("=" ,rep("=", 70), "\n", sep="")

cat("\n--- Variables Independientes ---\n")
cat("Numericas: Ram_GB, CPU_GHz, Inches\n")
cat("Cualitativas: TypeName, OpSys_Consolidado\n")
cat("Variable Dependiente: Price_euros\n")

summary_table <- summary(modelo)
coef_sig <- summary_table$coefficients[summary_table$coefficients[,4] < 0.05, ]

cat("\n--- Coeficientes Significativos (p < 0.05) ---\n")
print(coef_sig)

cat("\n--- Metricas del Modelo ---\n")
cat("R-squared:", summary_table$r.squared, "\n")
cat("Adj R-squared:", summary_table$adj.r.squared, "\n")
cat("F-statistic:", summary_table$fstatistic[1], "\n")

cat("\n--- Interpretacion de Coeficientes Principales ---\n")

# Interpretacion de RAM
if("Ram_GB" %in% rownames(summary_table$coefficients)) {
  coef_ram <- summary_table$coefficients["Ram_GB", 1]
  cat(paste("Por cada GB adicional de RAM, el precio aumenta en promedio", 
            round(coef_ram, 2), "EUR.\n"))
}

# Interpretacion de CPU
if("CPU_GHz" %in% rownames(summary_table$coefficients)) {
  coef_cpu <- summary_table$coefficients["CPU_GHz", 1]
  cat(paste("Por cada GHz adicional de CPU, el precio aumenta en promedio", 
            round(coef_cpu, 2), "EUR.\n"))
}

# Interpretacion de Inches
if("Inches" %in% rownames(summary_table$coefficients)) {
  coef_inches <- summary_table$coefficients["Inches", 1]
  cat(paste("Por cada pulgada adicional, el precio aumenta en promedio", 
            round(coef_inches, 2), "EUR.\n"))
}

cat("=" ,rep("=", 70), "\n", sep="")
cat("FIN DEL ANALISIS\n")
cat("=" ,rep("=", 70), "\n", sep="")
