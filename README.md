# DATA-FLOW

Repositorio del proyecto **DATA-FLOW** — análisis estadístico del contenido temático de letras de reggaetón colombiano y puertorriqueño (2010–2025), desarrollado como proyecto final del curso de Estadística Industrial, Universidad del Magdalena.

## Descripción

El proyecto analiza un corpus de 159 canciones de reggaetón usando un diccionario maestro de 21 categorías temáticas para responder cuatro preguntas:

1. ¿Qué proporción de palabras obscenas contienen las canciones y cómo varía entre artistas?
2. ¿Ha aumentado el contenido obsceno en el reggaetón entre 2010 y 2025?
3. ¿Difiere el contenido temático entre intérpretes masculinos y femeninos?
4. ¿Ha cambiado la temática de las canciones de mujeres a lo largo del período?

## Estructura del repositorio

## Scripts

| Script | Qué hace | Prueba estadística |
|--------|----------|--------------------|
| `Limpiar_duplicados.R` | Elimina canciones con letras idénticas | — |
| `analisis_obscenidad.R` | Proporciones por canción, tendencia 2010–2025, gráficos y resumen escrito | Spearman, regresión lineal, GLM cuasibinomial |
| `Analisis hombres y mujeres.R` | Compara contenido temático entre géneros, ajuste Benjamini-Hochberg | Mann-Whitney (wilcox.test) |
| `Analisis mujeres.R` | Evolución temática de intérpretes femeninas, incluye partes femeninas en colaboraciones mixtas | Spearman, regresión lineal |

## Requisitos

R 4.x con los siguientes paquetes:

```r
install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", 
                   "writexl", "patchwork", "here"))
```

## Cómo reproducir el análisis

1. Clona el repositorio y ábrelo como proyecto en RStudio (`DATA-FLOW.Rproj` o abriendo la carpeta directamente).
2. Verifica que `here::here()` apunte a la raíz del repositorio. Si no, ejecuta `here::set_here()` una vez.
3. Corre los scripts en este orden:

```r
source("Limpiar_duplicados.R")       # genera canciones_sin_letras_repetidas.xlsx
source("analisis_obscenidad.R")      # genera resultados/analisis_obscenidad_por_cancion.xlsx
source("Analisis hombres y mujeres.R")  # genera resultados/analisis_genero_tematico.xlsx
source("Analisis mujeres.R")         # genera resultados/analisis_evolucion_mujeres.xlsx
```

4. Los gráficos y resúmenes escritos quedan en la carpeta `resultados/`.

## Autores
Albert Gastelbondo
Mariana Carrillo
Valentina Cabarcas
Proyecto desarrollado para el curso de Estadística Industrial — Universidad del Magdalena, Santa Marta, Colombia.
