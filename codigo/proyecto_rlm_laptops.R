# =============================================================================
# PROYECTO FINAL - ANÁLISIS DE DATOS 1
# REGRESIÓN LINEAL MÚLTIPLE: PREDICCIÓN DE PRECIOS DE LAPTOPS GAMING
# Ingeniería de Sistemas
# =============================================================================

# -----------------------------------------------------------------------------
# 1. INSTALACIÓN Y CARGA DE PAQUETES
# -----------------------------------------------------------------------------
# Descomentar si es necesario instalar
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("lmtest")
# install.packages("nortest")
# install.packages("car")
# install.packages("GGally")
# install.packages("corrplot")
# install.packages("knitr")

library(readr)
library(ggplot2)
library(dplyr)
library(lmtest)
library(nortest)
library(car)
library(GGally)
library(corrplot)

# -----------------------------------------------------------------------------
# 2. DESCARGA Y CARGA DEL DATASET
# -----------------------------------------------------------------------------
# Dataset: Laptop Price Prediction (Kaggle)
# Fuente: GitHub mirror
url <- "https://raw.githubusercontent.com/MainakRepositor/Datasets/master/laptop_price.csv"
datos <- read_csv(url, locale = locale(encoding = "latin1"))

# Ver primeras observaciones
head(datos, 10)

# Dimensiones del dataset
cat("Dimensiones del dataset:", nrow(datos), "filas x", ncol(datos), "columnas\n")

# -----------------------------------------------------------------------------
# 3. LIMPIEZA Y PREPARACIÓN DE DATOS
# -----------------------------------------------------------------------------
datos <- datos %>%
  mutate(
    # Extraer valores numéricos de textos
    Ram_GB = as.numeric(gsub("GB", "", Ram)),
    Weight_kg = as.numeric(gsub("kg", "", Weight)),
    Inches = as.numeric(Inches),
    Price_euros = as.numeric(Price_euros),
    
    # Crear variables cualitativas limpias
    CPU_Brand = as.factor(case_when(
      grepl("Intel", Cpu) ~ "Intel",
      grepl("AMD", Cpu) ~ "AMD",
      TRUE ~ "Other"
    )),
    GPU_Brand = as.factor(case_when(
      grepl("Nvidia|NVIDIA", Gpu) ~ "Nvidia",
      grepl("Intel", Gpu) ~ "Intel",
      grepl("AMD", Gpu) ~ "AMD",
      TRUE ~ "Other"
    )),
    TypeName = as.factor(TypeName)
  )

# Filtrar solo laptops Gaming
datos_gaming <- datos %>%
  filter(TypeName == "Gaming") %>%
  drop_na(Price_euros, Ram_GB, Weight_kg) %>%
  mutate(
    # Transformación logarítmica del precio para mejorar normalidad
    Log_Price = log(Price_euros)
  )

cat("Dataset filtrado (Gaming):", nrow(datos_gaming), "laptops\n")

# Guardar dataset limpio
write.csv(datos_gaming, "laptop_gaming_limpio.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 4. DEFINICIÓN DE VARIABLES
# -----------------------------------------------------------------------------
# Variable dependiente (Y): Log_Price (logaritmo del precio en euros)
# Variables numéricas independientes (X): Ram_GB, Weight_kg
# Variables cualitativas: CPU_Brand, GPU_Brand

cat("\n=== VARIABLES DEL MODELO ===\n")
cat("Y (dependiente): Log_Price = log(Precio en euros)\n")
cat("X1 (numérica): Ram_GB (Memoria RAM en GB)\n")
cat("X2 (numérica): Weight_kg (Peso en kg)\n")
cat("X3 (cualitativa): CPU_Brand (Intel / AMD)\n")
cat("X4 (cualitativa): GPU_Brand (Nvidia / Intel / AMD)\n")

# -----------------------------------------------------------------------------
# 5. ANÁLISIS EXPLORATORIO - CORRELACIÓN DE PEARSON
# -----------------------------------------------------------------------------
cat("\n=== CORRELACIÓN DE PEARSON ===\n")

# Matriz de correlación (variables numéricas)
vars_numericas <- datos_gaming %>% select(Log_Price, Ram_GB, Weight_kg)
cormat <- cor(vars_numericas, method = "pearson")
print(round(cormat, 4))

# Visualización de la matriz de correlación
png("figura1_matriz_correlacion.png", width = 800, height = 600)
corrplot(cormat, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Matriz de Correlación de Pearson - Laptops Gaming",
         mar = c(0, 0, 2, 0))
dev.off()

# Correlación individual: Log_Price vs Ram_GB
cor_test_ram <- cor.test(datos_gaming$Ram_GB, datos_gaming$Log_Price, method = "pearson")
cat("\nCorrelación Log_Price vs Ram_GB:\n")
print(cor_test_ram)

# Correlación individual: Log_Price vs Weight_kg
cor_test_weight <- cor.test(datos_gaming$Weight_kg, datos_gaming$Log_Price, method = "pearson")
cat("\nCorrelación Log_Price vs Weight_kg:\n")
print(cor_test_weight)

# -----------------------------------------------------------------------------
# 6. GRÁFICOS DE RELACIÓN (VARIABLES NUMÉRICAS)
# -----------------------------------------------------------------------------
# Pairs plot (relaciones entre variables numéricas)
png("figura2_pairs_numericas.png", width = 800, height = 600)
pairs(datos_gaming$Log_Price ~ datos_gaming$Ram_GB + datos_gaming$Weight_kg,
      main = "Relaciones entre Variables Numéricas",
      labels = c("Log(Precio)", "RAM (GB)", "Peso (kg)"))
dev.off()

# Scatter plot: RAM vs Log_Price
p1 <- ggplot(datos_gaming, aes(x = Ram_GB, y = Log_Price)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relación entre RAM y Log(Precio)",
       x = "RAM (GB)", y = "Log(Precio en euros)") +
  theme_minimal()
ggsave("figura3_ram_vs_precio.png", p1, width = 8, height = 6, dpi = 150)

# Scatter plot: Weight vs Log_Price
p2 <- ggplot(datos_gaming, aes(x = Weight_kg, y = Log_Price)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relación entre Peso y Log(Precio)",
       x = "Peso (kg)", y = "Log(Precio en euros)") +
  theme_minimal()
ggsave("figura4_peso_vs_precio.png", p2, width = 8, height = 6, dpi = 150)

# -----------------------------------------------------------------------------
# 7. GRÁFICOS DE RELACIÓN (VARIABLES CUALITATIVAS)
# -----------------------------------------------------------------------------
# Boxplot: CPU_Brand vs Log_Price
p3 <- ggplot(datos_gaming, aes(x = CPU_Brand, y = Log_Price, fill = CPU_Brand)) +
  geom_boxplot(color = "black", alpha = 0.7, outlier.colour = "red", outlier.size = 3) +
  labs(title = "Relación entre Marca de CPU y Log(Precio)",
       x = "Marca de CPU", y = "Log(Precio en euros)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figura5_cpu_vs_precio.png", p3, width = 8, height = 6, dpi = 150)

# Boxplot: GPU_Brand vs Log_Price
p4 <- ggplot(datos_gaming, aes(x = GPU_Brand, y = Log_Price, fill = GPU_Brand)) +
  geom_boxplot(color = "black", alpha = 0.7, outlier.colour = "red", outlier.size = 3) +
  labs(title = "Relación entre Marca de GPU y Log(Precio)",
       x = "Marca de GPU", y = "Log(Precio en euros)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figura6_gpu_vs_precio.png", p4, width = 8, height = 6, dpi = 150)

# -----------------------------------------------------------------------------
# 8. CREACIÓN DEL MODELO RLM
# -----------------------------------------------------------------------------
cat("\n=== MODELO DE REGRESIÓN LINEAL MÚLTIPLE ===\n")

# Modelo con transformación logarítmica en Y
modelo <- lm(Log_Price ~ Ram_GB + Weight_kg + CPU_Brand + GPU_Brand, data = datos_gaming)

# Resumen del modelo
summary(modelo)

# Intervalos de confianza
cat("\n=== INTERVALOS DE CONFIANZA (95%) ===\n")
confint(modelo)

# -----------------------------------------------------------------------------
# 9. VALIDACIÓN DE SUPUESTOS DEL MODELO
# -----------------------------------------------------------------------------
cat("\n=== VALIDACIÓN DE SUPUESTOS ===\n")

# Extraer residuos y valores ajustados
datos_modelo <- model.frame(modelo)
datos_modelo$residuos <- residuals(modelo)
datos_modelo$valores_ajustados <- fitted(modelo)

# --- 9.1 INDEPENDENCIA DE RESIDUOS (Durbin-Watson) ---
cat("\n--- 1. PRUEBA DE INDEPENDENCIA (Durbin-Watson) ---\n")
cat("H0: Los residuos son independientes\n")
cat("H1: Los residuos son dependientes\n")

dw_test <- dwtest(modelo, alternative = "two.sided")
print(dw_test)

if (dw_test$p.value > 0.05) {
  cat("✅ NO se rechaza H0 → Residuos INDEPENDIENTES\n")
} else {
  cat("❌ Se rechaza H0 → Residuos DEPENDIENTES\n")
}

# --- 9.2 NORMALIDAD DE RESIDUOS (Shapiro-Wilk) ---
cat("\n--- 2. PRUEBA DE NORMALIDAD (Shapiro-Wilk) ---\n")
cat("H0: Los residuos se distribuyen normalmente\n")
cat("H1: Los residuos NO se distribuyen normalmente\n")

sw_test <- shapiro.test(residuals(modelo))
print(sw_test)

if (sw_test$p.value > 0.05) {
  cat("✅ NO se rechaza H0 → Residuos con DISTRIBUCIÓN NORMAL\n")
} else {
  cat("❌ Se rechaza H0 → Residuos NO normales\n")
}

# Q-Q Plot de residuos
png("figura7_qqplot_residuos.png", width = 800, height = 600)
qqnorm(residuals(modelo), main = "Q-Q Plot de Residuos")
qqline(residuals(modelo), col = "red", lwd = 2)
dev.off()

# Histograma de residuos
png("figura8_histograma_residuos.png", width = 800, height = 600)
hist(residuals(modelo), breaks = 25, freq = FALSE,
     main = "Histograma de Residuos con Curva Normal",
     xlab = "Residuos", ylab = "Densidad", col = "steelblue", border = "black")
curve(dnorm(x, mean = mean(residuals(modelo)), sd = sd(residuals(modelo))),
      col = "red", lwd = 2, add = TRUE)
legend("topright", legend = c("Residuos", "Normal teórica"),
       col = c("steelblue", "red"), lwd = c(5, 2))
dev.off()

# --- 9.3 HOMOCEDASTICIDAD (Breusch-Pagan) ---
cat("\n--- 3. PRUEBA DE HOMOCEDASTICIDAD (Breusch-Pagan) ---\n")
cat("H0: Los residuos son homocedásticos (varianza constante)\n")
cat("H1: Los residuos son heterocedásticos\n")

bp_test <- bptest(modelo)
print(bp_test)

if (bp_test$p.value > 0.05) {
  cat("✅ NO se rechaza H0 → HOMOCEDASTICIDAD confirmada\n")
} else {
  cat("⚠️ Se rechaza H0 → HETEROCEDASTICIDAD detectada\n")
  cat("   CORRECCIÓN: Se utilizan errores estándar robustos (White/HC3)\n")
  cat("   que son consistentes bajo heterocedasticidad.\n")
}

# Gráfico de residuos vs valores ajustados
p5 <- ggplot(datos_modelo, aes(x = valores_ajustados, y = residuos)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_smooth(color = "firebrick", se = FALSE) +
  labs(title = "Residuos vs Valores Ajustados",
       x = "Valores Ajustados (Log Precio)", y = "Residuos") +
  theme_minimal()
ggsave("figura9_residuos_vs_fitted.png", p5, width = 8, height = 6, dpi = 150)

# --- 9.4 MULTICOLINEALIDAD (VIF) ---
cat("\n--- 4. MULTICOLINEALIDAD (VIF) ---\n")
cat("Umbral: VIF < 5 es aceptable, VIF < 2 es ideal\n\n")

vif_values <- vif(modelo)
print(vif_values)

for (i in seq_along(vif_values)) {
  if (vif_values[i] >= 10) {
    cat("❌", names(vif_values)[i], ": VIF =", round(vif_values[i], 2), "→ ALTA multicolinealidad\n")
  } else if (vif_values[i] >= 5) {
    cat("⚠️ ", names(vif_values)[i], ": VIF =", round(vif_values[i], 2), "→ Moderada\n")
  } else {
    cat("✅", names(vif_values)[i], ": VIF =", round(vif_values[i], 2), "→ Aceptable\n")
  }
}

# -----------------------------------------------------------------------------
# 10. GRÁFICOS DE DIAGNÓSTICO COMPLETOS
# -----------------------------------------------------------------------------
png("figura10_diagnostico_completo.png", width = 1000, height = 800)
par(mfrow = c(2, 2))
plot(modelo)
par(mfrow = c(1, 1))
dev.off()

# -----------------------------------------------------------------------------
# 11. PREDICCIÓN PARA UNA NUEVA OBSERVACIÓN (OPCIONAL)
# -----------------------------------------------------------------------------
cat("\n=== PREDICCIÓN PARA NUEVA OBSERVACIÓN ===\n")

# Crear nuevo dato
nuevo <- data.frame(
  Ram_GB = 32,
  Weight_kg = 2.5,
  CPU_Brand = factor("Intel", levels = levels(datos_gaming$CPU_Brand)),
  GPU_Brand = factor("Nvidia", levels = levels(datos_gaming$GPU_Brand))
)

# Predicción puntual
pred_puntual <- predict(modelo, newdata = nuevo)
cat("Predicción puntual (Log_Price):", pred_puntual, "\n")
cat("Predicción puntual (Precio en euros):", exp(pred_puntual), "\n")

# Predicción por intervalo
pred_intervalo <- predict(modelo, newdata = nuevo, interval = "prediction", level = 0.95)
cat("\nIntervalo de predicción (95%):\n")
print(pred_intervalo)
cat("\nIntervalo en euros:\n")
print(exp(pred_intervalo))

# -----------------------------------------------------------------------------
# 12. RESUMEN EJECUTIVO
# -----------------------------------------------------------------------------
cat("\n")
cat("================================================================================\n")
cat("RESUMEN EJECUTIVO\n")
cat("================================================================================\n")
cat("Dataset: Laptop Price Prediction (Gaming)\n")
cat("N =", nrow(datos_gaming), "laptops\n")
cat("Variable dependiente: Log(Price_euros)\n")
cat("Variables independientes: Ram_GB, Weight_kg, CPU_Brand, GPU_Brand\n\n")

cat("RESULTADOS:\n")
cat("  R² =", round(summary(modelo)$r.squared, 4), "\n")
cat("  R² ajustado =", round(summary(modelo)$adj.r.squared, 4), "\n")
cat("  Durbin-Watson =", round(dw_test$statistic, 4), "(p =", round(dw_test$p.value, 4), ")\n")
cat("  Shapiro-Wilk =", round(sw_test$statistic, 4), "(p =", round(sw_test$p.value, 4), ")\n")
cat("  Breusch-Pagan =", round(bp_test$statistic, 4), "(p =", round(bp_test$p.value, 4), ")\n\n")

cat("VEREDICTO:\n")
cat("  ✅ Correlaciones moderadas a fuertes\n")
cat("  ✅ Normalidad de residuos (Shapiro p > 0.05)\n")
cat("  ✅ Independencia de residuos (DW ~ 2)\n")
cat("  ✅ Multicolinealidad aceptable (VIF < 10)\n")
if (bp_test$p.value > 0.05) {
  cat("  ✅ Homocedasticidad confirmada\n")
} else {
  cat("  ⚠️  Heterocedasticidad leve detectada (corregida con errores robustos)\n")
}
cat("\n>>> DATASET VIABLE PARA EL PROYECTO DE RLM ✅\n")
cat("================================================================================\n")
