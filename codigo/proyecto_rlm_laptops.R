# ============================================================================
# PROYECTO FINAL - ANALISIS DE DATOS 1
# REGRESION LINEAL MULTIPLE: PREDICCION DE PRECIOS DE LAPTOPS
# Ingenieria de Sistemas
# Universidad del Norte
# Integrantes: Dubal Aguilar Torres, Alejandro Chaves Ramos,
#              Juan Caceres Figueroa, Miguel Carrizosa
# Docente: PhD. Luis Angel Anillo Arrieta
# Fecha: 25 de mayo de 2026
# ============================================================================

# Instalamos los paquetes solo si no los tienes ya instalados
paquetes <- c("readr", "ggplot2", "MASS", "lmtest", "nortest",
              "ggfortify", "gridExtra", "car", "dplyr", "stringr",
              "knitr", "rmarkdown", "tseries")
for (p in paquetes) {
  if (!require(p, character.only = TRUE, quietly = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}


color_principal   <- "#0047AB"    # Azul oscuro profundo
color_secundario  <- "#4682B4"    # Azul acero
color_acento      <- "#B22222"    # Rojo ladrillo para outliers
color_fondo       <- "#F5F5F5"    # Gris muy claro
color_texto       <- "#2F4F4F"    # Gris oscuro


# -------------------------------------------------
# 1. LECTURA DE DATOS
# -------------------------------------------------
# Fuente: Laptop Price Prediction Dataset (Kaggle)
# N = 1303 laptops, 13 variables
# Asegurate de tener el CSV en Desktop/Final/

datos <- read_csv("Desktop/Final/laptop_price.csv",
                  locale = locale(encoding = "UTF-8"))

# Primer vistazo
cat("--- Primeras 6 filas ---\n")
head(datos)

cat("\n--- Variables del dataset ---\n")
names(datos)

cat("\n--- Estructura de los datos ---\n")
str(datos)


# -------------------------------------------------
# 2. LIMPIEZA Y PREPARACION DE DATOS
# -------------------------------------------------

cat("\n--- Limpiando datos ---\n")

# Extraemos los numeros del texto (RAM, peso, CPU, etc.)
# Y creamos factores limpios para CPU, GPU
datos$Ram_GB <- as.numeric(gsub("GB", "", datos$Ram))
datos$Weight_kg <- as.numeric(gsub("kg", "", datos$Weight))
datos$CPU_GHz <- as.numeric(str_extract(datos$Cpu, "\\d+\\.?\\d*(?=GHz)"))
datos$Inches <- as.numeric(datos$Inches)
datos$Price_euros <- as.numeric(datos$Price_euros)

# Resolucion de pantalla
datos$Res_Width <- as.numeric(str_extract(datos$ScreenResolution, "\\d+(?=x)"))
datos$Res_Height <- as.numeric(str_extract(datos$ScreenResolution, "(?<=x)\\d+"))

# Marca del CPU
datos$CPU_Brand <- ifelse(grepl("Intel", datos$Cpu), "Intel",
                          ifelse(grepl("AMD", datos$Cpu), "AMD", "Other"))

# Marca de la GPU
datos$GPU_Brand <- ifelse(grepl("Nvidia|NVIDIA", datos$Gpu), "Nvidia",
                          ifelse(grepl("Intel", datos$Gpu), "Intel",
                                 ifelse(grepl("AMD", datos$Gpu), "AMD", "Other")))

# Convertimos a factor
datos$CPU_Brand <- factor(datos$CPU_Brand)
datos$GPU_Brand <- factor(datos$GPU_Brand)
datos$TypeName <- factor(datos$TypeName)
datos$Company <- factor(datos$Company)
datos$OpSys <- factor(datos$OpSys)

# Filtramos filas completas para las variables del modelo
# Usamos el dataset COMPLETO (no solo gaming) para cumplir con el enunciado
datos <- datos[complete.cases(
  datos$Price_euros,
  datos$Ram_GB,
  datos$CPU_GHz,
  datos$Inches,
  datos$TypeName,
  datos$OpSys
), ]

cat(paste("Registros despues de limpieza:", nrow(datos), "\n"))

str(datos$CPU_Brand)
str(datos$GPU_Brand)
str(datos$TypeName)


# -------------------------------------------------
# 3. ESTADISTICA DESCRIPTIVA
# -------------------------------------------------

cat("\n--- Estadisticas descriptivas ---\n")

# Variables cuantitativas
cuantitativas <- data.frame(
  Price_euros = datos$Price_euros,
  Ram_GB      = datos$Ram_GB,
  CPU_GHz     = datos$CPU_GHz,
  Inches      = datos$Inches,
  Weight_kg   = datos$Weight_kg,
  Res_Width   = datos$Res_Width
)

summary(cuantitativas)


# -------------------------------------------------
# 4. CORRELACION DE PEARSON
# -------------------------------------------------

cat("\n--- Matriz de correlaciones de Pearson ---\n")

# Matriz de correlacion completa de Pearson
cormat <- cor(cuantitativas, method = "pearson", use = "complete.obs")
cat("--- Matriz de correlaciones de Pearson (completa) ---\n")
cormat

# Tabla de correlaciones formateada
cormat_rounded <- round(cormat, 4)
cat("\n--- Matriz redondeada ---\n")
cormat_rounded


# -------------------------------------------------
# 5. PRUEBA DE CORRELACION
# -------------------------------------------------

cat("\n--- Pruebas de correlacion ---\n")
# Ho: la correlacion es igual a 0
# H1: la correlacion no es igual a 0

# Price vs RAM
cat("\nCorrelacion Price vs Ram_GB:\n")
cor.test(datos$Ram_GB, datos$Price_euros, method = "pearson")

# Price vs CPU
cat("\nCorrelacion Price vs CPU_GHz:\n")
cor.test(datos$CPU_GHz, datos$Price_euros, method = "pearson")

# Price vs Inches
cat("\nCorrelacion Price vs Inches:\n")
cor.test(datos$Inches, datos$Price_euros, method = "pearson")


# -------------------------------------------------
# 6. GRAFICOS DE RELACION
# -------------------------------------------------

cat("\n--- Generando graficos de correlacion ---\n")

# --- Matriz de dispersion ---
pairs(cuantitativas[, c("Price_euros", "Ram_GB", "CPU_GHz", "Inches")],
      main = "Matriz de Dispersion - Variables Cuantitativas Laptops",
      pch = 19, col = color_principal, gap = 0.5,
      cex.main = 1.2, col.main = color_principal)

# --- Scatter plots individuales con recta de regresion roja ---

# Figura 1: Ram_GB vs Price_euros
ggplot(datos, aes(x = Ram_GB, y = Price_euros)) +
  geom_point(color = color_principal, size = 2, alpha = 0.4) +
  geom_smooth(method = "lm", color = "red", se = TRUE,
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
  geom_smooth(method = "lm", color = "red", se = TRUE,
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
  geom_smooth(method = "lm", color = "red", se = TRUE,
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

# --- Boxplots: variables cualitativas vs Precio ---

# Figura 4: TypeName vs Price_euros
# Usamos scale_fill_brewer para evitar problemas con numero de categorias
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

# Figura 5: OpSys vs Price_euros
# Usamos scale_fill_brewer para evitar problemas con numero de categorias
ggplot(datos, aes(x = OpSys, y = Price_euros, fill = OpSys)) +
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


# -------------------------------------------------
# 7. CREACION DEL MODELO RLM
# -------------------------------------------------

cat("\n--- Creando modelo RLM ---\n")

# Modelo: Price_euros ~ Ram_GB + CPU_GHz + Inches + TypeName + OpSys
# Usamos Price_euros directamente (sin transformacion logaritmica)
# para cumplir con el enunciado

modelo <- lm(Price_euros ~ Ram_GB + CPU_GHz + Inches + TypeName + OpSys, data = datos)

# Resumen del modelo (R2, R2 ajustado, F, p-valor)
cat("\n--- Resumen del modelo ---\n")
summary(modelo)

# Intervalos de confianza al 95%
cat("\n--- Intervalos de confianza (95%) ---\n")
confint(modelo)


# -------------------------------------------------
# 8. VALIDACION DE SUPUESTOS
# -------------------------------------------------

cat("\n--- Validacion de supuestos ---\n")

# --- Linealidad ---
cat("\n--- Linealidad ---\n")

# Graficos de residuos vs variables
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

# --- Normalidad ---
cat("\n--- Normalidad de residuos ---\n")

# Q-Q Plot de residuos con linea de referencia correcta
qq <- qqnorm(modelo$residuals, plot.it = FALSE)

qq_data <- data.frame(
  teorico = qq$x,
  muestra = qq$y
)

# Cuantiles teoricos normales (25% y 75%)
xq <- qnorm(c(0.25, 0.75))

# Cuantiles muestrales de los residuos
yq <- quantile(modelo$residuals, c(0.25, 0.75), names = FALSE)

# Pendiente e intercepto correctos
slope <- diff(yq) / diff(xq)
intercept <- yq[1] - slope * xq[1]

ggplot(qq_data, aes(x = teorico, y = muestra)) +
  geom_point(color = color_principal, alpha = 0.5, size = 2) +
  geom_abline(
    intercept = intercept,
    slope = slope,
    color = "red",
    linetype = "solid",
    size = 1.2
  ) +
  labs(
    title = "Q-Q Plot de Residuos",
    subtitle = "Prueba de Normalidad",
    x = "Cuantiles Teoricos",
    y = "Cuantiles Muestra"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(
      color = color_principal,
      size = 14,
      face = "bold"
    ),
    plot.subtitle = element_text(
      color = color_secundario,
      size = 11
    ),
    axis.title = element_text(
      color = color_texto,
      size = 12
    )
  )

# Prueba de Shapiro-Wilk
# Nota: shapiro.test() solo acepta maximo N = 5000
# Tomamos muestra aleatoria si N > 5000
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

# --- Independencia ---
cat("\n--- Independencia ---\n")

# Residuos vs Valores Ajustados
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

# Prueba Durbin-Watson
cat("\n--- Prueba Durbin-Watson ---\n")
dwtest(modelo, alternative = "two.sided")

# --- Homocedasticidad ---
cat("\n--- Homocedasticidad ---\n")

# Scale-Location
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
cat("\n--- Prueba Breusch-Pagan ---\n")
bptest(modelo)

# --- Multicolinealidad (VIF) ---
cat("\n--- Multicolinealidad (VIF) ---\n")
vif(modelo)

cat("\n--- Interpretacion VIF ---\n")
cat("VIF < 5: Sin multicolinealidad problematica\n")
cat("VIF 5-10: Multicolinealidad moderada\n")
cat("VIF > 10: Multicolinealidad severa\n")


# -------------------------------------------------
# 9. GRAFICOS DE DIAGNOSTICO COMPLETOS
# -------------------------------------------------

cat("\n--- Graficos de diagnostico (2x2) ---\n")

# Los 4 graficos clasicos de diagnostico
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

# Restauramos par(mfrow)
par(mfrow = c(1, 1))


# -------------------------------------------------
# 10. PREDICCION (ejemplo)
# -------------------------------------------------

cat("\n--- Prediccion ---\n")

# Nuevo dato para prediccion
nuevo <- data.frame(
  Ram_GB = 16,
  CPU_GHz = 2.8,
  Inches = 15.6,
  TypeName = factor("Gaming", levels = levels(datos$TypeName)),
  OpSys = factor("Windows 10", levels = levels(datos$OpSys))
)

# Prediccion puntual
cat("Prediccion puntual:\n")
predict(modelo, newdata = nuevo)

# Prediccion por intervalo
cat("\nPrediccion por intervalo (95%):\n")
predict(modelo, newdata = nuevo, interval = "prediction", level = 0.95)


# -------------------------------------------------
# 11. RESUMEN DE RESULTADOS
# -------------------------------------------------

cat("\n")
cat("=" ,rep("=", 70), "\n", sep="")
cat("RESUMEN DE RESULTADOS DEL MODELO\n")
cat("=" ,rep("=", 70), "\n", sep="")

cat("\n--- Variables Independientes ---\n")
cat("Numericas: Ram_GB, CPU_GHz, Inches\n")
cat("Cualitativas: TypeName, OpSys\n")
cat("Variable Dependiente: Price_euros\n")

cat("\n--- Coeficientes Significativos (p < 0.05) ---\n")
summary_table <- summary(modelo)
coef_sig <- summary_table$coefficients[summary_table$coefficients[,4] < 0.05, ]
print(coef_sig)

cat("\n--- Metricas del Modelo ---\n")
cat("R-squared:", summary_table$r.squared, "\n")
cat("Adj R-squared:", summary_table$adj.r.squared, "\n")
cat("F-statistic:", summary_table$fstatistic[1], "\n")

cat("\n--- Interpretacion ---\n")
cat("Por cada GB adicional de RAM, el precio aumenta en promedio\n")
cat(round(summary_table$coefficients["Ram_GB", 1], 2), "EUR, manteniendo constantes las demas variables.\n\n")

cat("Por cada GHz adicional de CPU, el precio aumenta en promedio\n")
cat(round(summary_table$coefficients["CPU_GHz", 1], 2), "EUR, manteniendo constantes las demas variables.\n\n")

cat("=" ,rep("=", 70), "\n", sep="")
cat("FIN DEL ANALISIS\n")
cat("=" ,rep("=", 70), "\n", sep="")
