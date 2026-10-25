# install.packages("here")  # solo la primera vez
library(here)
library(readxl)
library(dplyr)
library(writexl)

# Busca desde la raíz del repo: <repo>/data/...
ruta_entrada <- here("base de canciones", "excel con todas las canciones.xlsx")

df <- read_excel(ruta_entrada)

df_limpio <- df %>%
  filter(is.na(letra) | !duplicated(letra))

cat("Antes:", nrow(df), "| Después:", nrow(df_limpio),
    "| Eliminadas:", nrow(df) - nrow(df_limpio), "\n")

write_xlsx(df_limpio, here("base de canciones", "canciones_sin_letras_repetidas.xlsx"))