# REVISION PROFUNDA DEL PROYECTO RLM
## Comparando output real vs codigo vs LaTeX

---

## 1. CORRELACIONES (Tabla en LaTeX)

### Output REAL:
| | Price | RAM | CPU | Inches |
|---|---|---|---|---|
| Price | 1.0000 | 0.7430 | 0.4303 | 0.0682 |
| RAM | 0.7430 | 1.0000 | 0.3680 | 0.2380 |
| CPU | 0.4303 | 0.3680 | 1.0000 | 0.3079 |
| Inches | 0.0682 | 0.2380 | 0.3079 | 1.0000 |

### En LaTeX:
| | Price | RAM | CPU | Pulgadas |
|---|---|---|---|---|
| Precio | 1.000 | 0.743 | 0.430 | 0.068 |
| RAM | 0.743 | 1.000 | 0.210 | 0.150 |
| CPU | 0.430 | 0.210 | 1.000 | 0.050 |
| Pulgadas | 0.068 | 0.150 | 0.050 | 1.000 |

### PROBLEMAS ENCONTRADOS:
1. **CPU vs RAM**: Real = 0.368, LaTeX = 0.210 (ERROR)
2. **CPU vs Pulgadas**: Real = 0.3079, LaTeX = 0.050 (ERROR)
3. **RAM vs Pulgadas**: Real = 0.2380, LaTeX = 0.150 (ERROR)
4. Faltan correlaciones con Weight_kg y Res_Width en el LaTeX
5. Res_Width vs Price = 0.5565 (significativa) - no incluida en LaTeX

---

## 2. COEFICIENTES DEL MODELO (Output REAL vs LaTeX)

### Output REAL:
| Variable | Coef | Error | t | p-valor |
|---|---|---|---|---|
| (Intercept) | 42.920 | 292.168 | 0.147 | 0.883 |
| Ram_GB | 81.075 | 2.581 | 31.414 | <2e-16 *** |
| CPU_GHz | 208.313 | 24.493 | 8.505 | <2e-16 *** |
| Inches | -23.088 | 11.150 | -2.071 | 0.03859 * |
| TypeNameGaming | -46.892 | 53.627 | -0.874 | 0.382 |
| TypeNameNetbook | -247.928 | 88.427 | -2.804 | 0.00513 ** |
| TypeNameNotebook | -293.233 | 42.865 | -6.841 | 1.21e-11 *** |
| TypeNameUltrabook | 120.528 | 45.988 | 2.621 | 0.00887 ** |
| TypeNameWorkstation | 650.140 | 85.032 | 7.646 | 4.04e-14 *** |
| OpSysChrome OS | 263.411 | 282.896 | 0.931 | 0.352 |
| OpSysLinux | 231.796 | 279.931 | 0.828 | 0.408 |
| OpSysMac OS X | 401.450 | 306.282 | 1.311 | 0.190 |
| OpSysmacOS | 617.621 | 295.898 | 2.087 | 0.03706 * |
| OpSysNo OS | 179.496 | 279.594 | 0.642 | 0.521 |
| OpSysWindows 10 | 420.911 | 274.979 | 1.531 | 0.126 |
| OpSysWindows 10 S | 502.645 | 306.600 | 1.639 | 0.101 |
| OpSysWindows 7 | 827.967 | 280.866 | 2.948 | 0.00326 ** |

### En LaTeX:
| Variable | Coef | Error | t | p-valor |
|---|---|---|---|---|
| (Intercept) | -1000.0 | 500.0 | -2.00 | 0.046 * |
| RAM | 45.0 | 3.0 | 15.0 | <0.001 *** |
| CPU | 200.0 | 25.0 | 8.0 | <2e-16 *** |
| Pulgadas | 30.0 | 15.0 | 2.0 | 0.045 * |

### PROBLEMAS CRITICOS:
1. **Intercepto**: Real = 42.92, LaTeX = -1000 (ERROR GRAVISIMO)
2. **RAM**: Real = 81.08, LaTeX = 45.0 (ERROR)
3. **Pulgadas**: Real = -23.09 (NEGATIVO!), LaTeX = +30.0 (ERROR, signo invertido!)
4. Faltan TODAS las variables cualitativas en la tabla del LaTeX

---

## 3. METRICAS DEL MODELO

### Output REAL:
- R-squared: 0.7063255
- Adj R-squared: 0.7026717
- F-statistic: 193.3123
- df: 16 y 1286
- p-value F: < 2.2e-16
- RSE: 381.2

### En LaTeX:
- R2: 0.700 (aprox OK)
- Adj R2: 0.695 (aprox OK)
- F: 280.0 (ERROR)
- df: 5 y 1295 (ERROR)

### PROBLEMAS:
1. F-statistic: Real = 193.3, LaTeX = 280.0 (ERROR)
2. Grados de libertad: Real = 16 y 1286, LaTeX = 5 y 1295 (ERROR)

---

## 4. PRUEBAS DE SUPUESTOS

### Output REAL:
| Prueba | Estadistico | p-valor | Resultado |
|---|---|---|---|
| Shapiro-Wilk | W = 0.92379 | < 2.2e-16 | NO NORMAL (p < 0.05) |
| Jarque-Bera | X2 = 2411.5 | < 2.2e-16 | NO NORMAL |
| Durbin-Watson | DW = 2.0272 | p = 0.6331 | INDEPENDIENTES (OK) |
| Breusch-Pagan | BP = 201.21 | < 2.2e-16 | HETEROCEDASTICO (p < 0.05) |
| VIF Ram_GB | 1.54 | - | OK |
| VIF CPU_GHz | 1.38 | - | OK |
| VIF Inches | 2.27 | - | OK |
| VIF TypeName | 3.79 | - | OK |
| VIF OpSys | 1.60 | - | OK |

### En LaTeX:
| Prueba | Estadistico | p-valor | Resultado |
|---|---|---|---|
| Shapiro-Wilk | W = 0.995 | p = 0.150 | NORMAL |
| Durbin-Watson | DW = 1.96 | p = 0.450 | INDEPENDIENTES |
| Breusch-Pagan | LM = 15.50 | p = 0.050 | HOMOCEDASTICO |
| VIF RAM | < 5 | - | OK |

### PROBLEMAS GRAVES:
1. **Shapiro-Wilk**: Real p < 2.2e-16 (NO NORMAL), LaTeX p = 0.150 (NORMAL) - **CONTRADICCION TOTAL**
2. **Breusch-Pagan**: Real p < 2.2e-16 (HETEROCEDASTICO), LaTeX p = 0.050 (HOMOCEDASTICO) - **CONTRADICCION TOTAL**
3. Faltan valores exactos en el LaTeX

---

## 5. INTERVALOS DE CONFIANZA (95%)

### Output REAL (solo significativos):
| Variable | 2.5% | 97.5% |
|---|---|---|
| Ram_GB | 76.012 | 86.138 |
| CPU_GHz | 160.262 | 256.364 |
| Inches | -44.963 | -1.214 |
| TypeNameWorkstation | 483.324 | 816.957 |
| OpSysWindows 7 | 276.962 | 1378.973 |

### PROBLEMA:
- **Inches**: Intervalo NEGATIVO (-44.96, -1.21), lo que confirma que es NEGATIVO
- LaTeX dice "aumenta 30 EUR" cuando en realidad DISMINUYE 23 EUR - **ERROR DE SIGNO**

---

## 6. PREDICCION

### Output REAL:
- Punto: 1937.242 EUR
- Intervalo 95%: [1187.382, 2687.103]

### En LaTeX:
- No se incluye la prediccion

---

## 7. RESUMEN DE ERRORES CRITICOS

### ERRORES GRAVES:
1. **Intercepto**: LaTeX usa -1000 en vez de 42.92
2. **RAM**: LaTeX usa 45 en vez de 81.08
3. **Inches**: LaTeX dice +30 y "aumenta", pero real es -23 y DISMINUYE
4. **Shapiro-Wilk**: LaTeX dice NORMAL pero es NO NORMAL
5. **Breusch-Pagan**: LaTeX dice HOMOCEDASTICO pero es HETEROCEDASTICO
6. **Correlaciones**: Varios valores incorrectos en la matriz
7. **Falta tabla completa** de coeficientes con variables cualitativas

### ADVERTENCIAS (warnings en R):
1. Notch went outside hinges (boxplots) - usar notch=FALSE
2. geom_smooth con pocos valores unicos - usar method="lm" explicito
3. RColorBrewer: Set2 solo tiene 8 colores pero OpSys tiene 9 categorias

---

## 8. RECOMENDACIONES

### Corregir URGENTEMENTE:
1. **Actualizar TODOS los valores numericos** en el LaTeX con los del output real
2. **Corregir el signo de Inches** de +30 a -23
3. **Añadir interpretacion correcta**: "cada pulgada DISMINUYE 23 EUR"
4. **Actualizar supuestos**:
   - Shapiro-Wilk: NO NORMAL, sugerir transformacion o robustez
   - Breusch-Pagan: HETEROCEDASTICO, usar errores estandar robustos
5. **Añadir tabla completa** con todos los coeficientes
6. **Corregir F-statistic** y grados de libertad
7. **Incluir seccion de prediccion** con el valor real
8. **Agregar nota sobre asimetria** del precio (skewness = 1.52)
