# Proyecto Final - Analisis de Datos 1
## Regresion Lineal Multiple: Prediccion de Precios de Laptops Gaming

**Ingenieria de Sistemas** | Universidad del Norte | Mayo 2026

---

## Descripcion

Este proyecto aplica tecnicas de **Regresion Lineal Multiple (RLM)** para predecir el precio de laptops gaming a partir de sus especificaciones tecnicas. El analisis cumple con los requisitos del curso de Analisis de Datos en Ingenieria I.

**Dataset utilizado:** [Laptop Price Prediction](https://www.kaggle.com/datasets/muhammadzulfadhilah/laptop-price-prediction) (Kaggle)

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
| **Y (Dependiente)** | `Log_Price` | Logaritmo natural del precio en euros |
| **X1 (Numerica)** | `Ram_GB` | Memoria RAM en GB |
| **X2 (Numerica)** | `Weight_kg` | Peso del equipo en kg |
| **X3 (Cualitativa)** | `CPU_Brand` | Marca del procesador (Intel / AMD) |
| **X4 (Cualitativa)** | `GPU_Brand` | Marca de la GPU (Nvidia / Intel / AMD) |

---

## Resultados Principales

| Metrica | Valor | Estado |
|---|---|---|
| **R2** | 0.544 | Aceptable |
| **R2 Ajustado** | 0.534 | OK |
| **Normalidad (Shapiro-Wilk)** | p = 0.552 | **NORMAL** |
| **Independencia (Durbin-Watson)** | DW = 2.156 | **OK** |
| **Multicolinealidad (VIF)** | < 10 | **Aceptable** |
| **Homocedasticidad (Breusch-Pagan)** | p = 0.004 | Corregida con errores robustos |

---

## Interpretacion de Coeficientes

| Variable | Coeficiente | Interpretacion |
|---|---|---|
| **RAM (GB)** | +0.033 | Cada GB extra aumenta el precio ~3.3% |
| **Peso (kg)** | +0.137 | Cada kg extra aumenta el precio ~13.7% |
| **CPU Intel** | +0.177 | Intel cuesta ~17.7% mas que AMD |
| **GPU Nvidia** | -0.058 | No significativa vs AMD (p=0.421) |

---

## Como usar el codigo en R

1. Abre **RStudio**
2. Crea un nuevo script
3. Copia el contenido de `codigo/proyecto_rlm_laptops.R`
4. Ejecuta linea por linea con **Ctrl+Enter**
5. Las graficas se guardan automaticamente como archivos PNG

### Librerias requeridas:
```r
install.packages(c("readr", "ggplot2", "dplyr", "lmtest", "nortest", "car", "GGally", "corrplot"))
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

El dataset de **Laptops Gaming** es **VIABLE** para el proyecto de RLM:
- Correlaciones moderadas a fuertes
- Normalidad de residuos confirmada
- Independencia de residuos (sin autocorrelacion)
- Multicolinealidad aceptable
- Heterocedasticidad leve corregida con errores robustos (White/HC3)

---

## Referencias

- Dataset: [Kaggle - Laptop Price Prediction](https://www.kaggle.com/datasets/muhammadzulfadhilah/laptop-price-prediction)
- Metodo: Regresion Lineal Multiple con transformacion logaritmica
- Correccion: Errores estandar robustos (White, 1980)

---

**Fecha:** 25 de mayo de 2026
