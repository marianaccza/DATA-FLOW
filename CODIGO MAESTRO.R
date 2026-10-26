# 00_maestro.R

# 1. Limpiar el entorno
rm(list = ls())

# 2. Trabajar en la carpeta donde está este archivo (sin escribir ruta)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}
cat("Trabajando en:", getwd(), "\n")

# 3. Crear carpeta de resultados si no existe
if (!dir.exists("resultados")) dir.create("resultados")

# 4. Paquetes (ajusta según los que usen tus scripts)
paquetes <- c("dplyr", "ggplot2", "readxl")
for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

# 5. Scripts en orden
scripts <- c(
  "limpiar_duplicados.R",
  "analisis_obscenidad.R",
  "Analisis hombres y mujeres.R",
  "Analisis mujeres.R"
)

# 6. Ejecutarlos, avisando cuál falla si hay error
for (s in scripts) {
  cat("Ejecutando:", s, "\n")
  tryCatch(
    source(s, encoding = "UTF-8"),
    error = function(e) {
      stop("Error en ", s, ": ", conditionMessage(e), call. = FALSE)
    }
  )
}

cat("Proceso terminado\n")