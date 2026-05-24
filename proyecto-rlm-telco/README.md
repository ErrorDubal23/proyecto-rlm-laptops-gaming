# Proyecto RLM - Telco Customer Churn

## Predicción de Cargos Totales con Regresión Lineal Múltiple

### Equipo
- Dubal Aguilar Torres
- Alejandro Chaves Ramos
- Juan Caceres Figueroa
- Miguel Carrizosa

### Docente
PhD. Luis Angel Anillo Arrieta

### Asignatura
Análisis de Datos 1 - Ingeniería de Sistemas
Universidad del Norte

---

## Dataset

**Fuente:** IBM Sample DataSets (Kaggle)  
**URL:** https://www.kaggle.com/datasets/blastchar/telco-customer-churn  
**N:** 7,032 clientes  
**Descripción:** Dataset de churn de clientes de telecomunicaciones con información demográfica, servicios y cargos.

## Estructura del Proyecto

```
proyecto-rlm-telco/
├── datos/
│   └── WA_Fn-UseC_-Telco-Customer-Churn.csv
├── codigo/
│   └── proyecto_rlm_telco.R
└── documento/
    └── proyecto_rlm_telco.tex
```

## Variables del Modelo

### Variable Dependiente (Y)
- **TotalCharges**: Cargos totales acumulados por el cliente ($)

### Variables Independientes

**Cuantitativas:**
- **tenure**: Antigüedad del cliente en meses
- **MonthlyCharges**: Cargo mensual al cliente ($)

**Cualitativas:**
- **Contract**: Tipo de contrato (Month-to-month, One year, Two year)
- **InternetService**: Tipo de servicio de internet (DSL, Fiber optic, No)

## Modelo

```r
lm(TotalCharges ~ tenure + MonthlyCharges + Contract + InternetService, data = df)
```

## Resultados Principales

| Métrica | Valor |
|---------|-------|
| R² | 0.900 |
| R² ajustado | 0.900 |
| N | 7,032 |
| VIF (max) | 1.06 |
| DW | 2.018 |

## Nota sobre Normalidad

Con N = 7,032, el test de Shapiro-Wilk es sensible a desviaciones mínimas de la normalidad. El Teorema Central del Límite (TLC) garantiza que los estimadores son consistentes y siguen una distribución aproximadamente normal con muestras grandes (n > 30).

## Instrucciones

1. Descargar el dataset de Kaggle y guardarlo en `datos/`
2. Abrir `codigo/proyecto_rlm_telco.R` en RStudio
3. Ejecutar sección por sección
4. Para el documento LaTeX, compilar `documento/proyecto_rlm_telco.tex` en Overleaf
