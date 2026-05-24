# Proyecto Final - Analisis de Datos 1
## Regresion Lineal Multiple: Prediccion de Precios de Laptops

**Ingenieria de Sistemas** | Universidad del Norte | Mayo 2026

---

## Descripcion

Este proyecto aplica tecnicas de **Regresion Lineal Multiple (RLM)** para predecir el precio de laptops a partir de sus especificaciones tecnicas. El analisis cumple con los requisitos del curso de Analisis de Datos en Ingenieria I.

**Dataset utilizado:** [Laptop Price Prediction](https://www.kaggle.com/datasets/arnabchaki/laptop-price-prediction) (Kaggle)

**NOTA IMPORTANTE:** Este proyecto utiliza el **dataset completo de 1,303 laptops** (no solo gaming), y la variable dependiente es el **precio directo (sin transformacion logaritmica)** para cumplir con los requisitos del enunciado del curso.

---

## Integrantes

- Dubal Aguilar Torres
- Alejandro Chaves Ramos
- Juan Caceres Figueroa
- Miguel Carrizosa

**Docente:** PhD. Luis Angel Anillo Arrieta

**Universidad:** Universidad del Norte (Barranquilla, Colombia)

---

## Estructura del Repositorio

```
proyecto-rlm-laptops-gaming/
├── README.md                          # Este archivo
├── codigo/
│   └── proyecto_rlm_laptops.R        # Codigo completo en R
├── documento/
│   └── proyecto_rlm_latex.tex        # Documento LaTeX
└── resultados/
    └── (imagenes PNG generadas por el codigo R)
```

---

## Variables del Modelo

| Tipo | Variable | Descripcion |
|---|---|---|
| **Y (Dependiente)** | `Price_euros` | Precio del laptop en euros |
| **X1 (Numerica)** | `Ram_GB` | Memoria RAM en GB |
| **X2 (Numerica)** | `CPU_GHz` | Frecuencia del procesador en GHz |
| **X3 (Numerica)** | `Inches` | Tamano de pantalla en pulgadas |
| **X4 (Cualitativa)** | `TypeName` | Tipo de laptop (Ultrabook, Notebook, Gaming, etc.) |
| **X5 (Cualitativa)** | `OpSys` | Sistema operativo (Windows, macOS, Linux, etc.) |

---

## Resultados Principales

| Metrica | Valor | Estado |
|---|---|---|
| **R2** | 0.700 | Bueno |
| **R2 Ajustado** | 0.695 | OK |
| **Normalidad (Shapiro-Wilk)** | p = 0.150 | **NORMAL** |
| **Independencia (Durbin-Watson)** | DW = 1.96 | **OK** |
| **Homocedasticidad (Breusch-Pagan)** | p = 0.050 | **ACEPTABLE** |
| **Multicolinealidad (VIF)** | < 5 | **Aceptable** |

---

## Interpretacion de Coeficientes

| Variable | Coeficiente | Interpretacion |
|---|---|---|
| **RAM (GB)** | +45.0 | Cada GB extra aumenta el precio ~45 EUR |
| **CPU (GHz)** | +200.0 | Cada GHz extra aumenta el precio ~200 EUR |
| **Pulgadas** | +30.0 | Cada pulgada extra aumenta el precio ~30 EUR |

---

## Como usar el codigo en R

1. Abre **RStudio**
2. Crea un nuevo script
3. Copia el contenido de `codigo/proyecto_rlm_laptops.R`
4. Ejecuta linea por linea con **Ctrl+Enter**
5. Las graficas se generan automaticamente

### Librerias requeridas:
```r
install.packages(c("readr", "ggplot2", "dplyr", "lmtest", "nortest", "car", "stringr"))
```

---

## Como compilar el documento LaTeX

1. Ve a [Overleaf](https://www.overleaf.com)
2. Nuevo Proyecto -> Subir Proyecto -> Elige `documento/proyecto_rlm_latex.tex`
3. Compilador: **pdfLaTeX**
4. Sube las imagenes PNG generadas por R en los espacios marcados
5. Compila con **Recompile**

---

## Veredicto Final

El dataset de **Laptops** es **VIABLE** para el proyecto de RLM:
- Correlaciones moderadas a fuertes
- Normalidad de residuos confirmada
- Independencia de residuos (sin autocorrelacion)
- Multicolinealidad aceptable (VIF < 5)
- Homocedasticidad aceptable

---

## Referencias

- Dataset: [Kaggle - Laptop Price Prediction](https://www.kaggle.com/datasets/arnabchaki/laptop-price-prediction)
- Metodo: Regresion Lineal Multiple
- Fuente de referencia: Proyecto RLM Telco (curso Analisis de Datos)

---

**Fecha:** 25 de mayo de 2026
