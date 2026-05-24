# Proyecto Final - Análisis de Datos 1
## Regresión Lineal Múltiple: Predicción de Precios de Laptops Gaming

**Ingeniería de Sistemas** | Universidad del Norte | Mayo 2026

---

## 📋 Descripción

Este proyecto aplica técnicas de **Regresión Lineal Múltiple (RLM)** para predecir el precio de laptops gaming a partir de sus especificaciones técnicas. El análisis cumple con los requisitos del curso de Análisis de Datos en Ingeniería I.

**Dataset utilizado:** [Laptop Price Prediction](https://www.kaggle.com/datasets/muhammadzulfadhilah/laptop-price-prediction) (Kaggle)

---

## 📁 Estructura del Repositorio

```
proyecto-rlm-laptops-gaming/
├── README.md                          # Este archivo
├── codigo/
│   └── proyecto_rlm_laptops.R        # Código completo en R
├── documento/
│   └── proyecto_rlm_latex.tex        # Documento LaTeX
└── resultados/
    └── (imágenes PNG generadas por el código R)
```

---

## 🔬 Variables del Modelo

| Tipo | Variable | Descripción |
|---|---|---|
| **Y (Dependiente)** | `Log_Price` | Logaritmo natural del precio en euros |
| **X₁ (Numérica)** | `Ram_GB` | Memoria RAM en GB |
| **X₂ (Numérica)** | `Weight_kg` | Peso del equipo en kg |
| **X₃ (Cualitativa)** | `CPU_Brand` | Marca del procesador (Intel / AMD) |
| **X₄ (Cualitativa)** | `GPU_Brand` | Marca de la GPU (Nvidia / Intel / AMD) |

---

## 📊 Resultados Principales

| Métrica | Valor | Estado |
|---|---|---|
| **R²** | 0.544 | ✅ Aceptable |
| **R² Ajustado** | 0.534 | ✅ |
| **Normalidad (Shapiro-Wilk)** | p = 0.552 | ✅ **NORMAL** |
| **Independencia (Durbin-Watson)** | DW = 2.156 | ✅ **OK** |
| **Multicolinealidad (VIF)** | < 10 | ✅ **Aceptable** |
| **Homocedasticidad (Breusch-Pagan)** | p = 0.004 | ⚠️ Corregida con errores robustos |

---

## 📈 Interpretación de Coeficientes

| Variable | Coeficiente | Interpretación |
|---|---|---|
| **RAM (GB)** | +0.033 | Cada GB extra aumenta el precio ~3.3% |
| **Peso (kg)** | +0.137 | Cada kg extra aumenta el precio ~13.7% |
| **CPU Intel** | +0.177 | Intel cuesta ~17.7% más que AMD |
| **GPU Nvidia** | -0.058 | No significativa vs AMD (p=0.421) |

---

## 🚀 Cómo usar el código en R

1. Abre **RStudio**
2. Crea un nuevo script
3. Copia el contenido de `codigo/proyecto_rlm_laptops.R`
4. Ejecuta línea por línea con **Ctrl+Enter**
5. Las gráficas se guardan automáticamente como archivos PNG

### Librerías requeridas:
```r
install.packages(c("readr", "ggplot2", "dplyr", "lmtest", "nortest", "car", "GGally", "corrplot"))
```

---

## 📝 Cómo compilar el documento LaTeX

1. Ve a [Overleaf](https://www.overleaf.com)
2. Nuevo Proyecto → Subir Proyecto → Elige `documento/proyecto_rlm_latex.tex`
3. Compilador: **pdfLaTeX**
4. Reemplaza los placeholders (`[Nombre del Estudiante]`, etc.)
5. Sube las imágenes PNG generadas por R en los espacios marcados
6. Compila con **Recompile**

---

## ✅ Veredicto Final

El dataset de **Laptops Gaming** es **VIABLE** para el proyecto de RLM:
- ✅ Correlaciones moderadas a fuertes
- ✅ Normalidad de residuos confirmada
- ✅ Independencia de residuos (sin autocorrelación)
- ✅ Multicolinealidad aceptable
- ⚠️ Heterocedasticidad leve corregida con errores robustos (White/HC3)

---

## 📚 Referencias

- Dataset: [Kaggle - Laptop Price Prediction](https://www.kaggle.com/datasets/muhammadzulfadhilah/laptop-price-prediction)
- Método: Regresión Lineal Múltiple con transformación logarítmica
- Corrección: Errores estándar robustos (White, 1980)

---

**Integrantes:** [Nombre 1], [Nombre 2], [Nombre 3]  
**Docente:** PhD. [Nombre del Docente]  
**Fecha:** 25 de mayo de 2026
