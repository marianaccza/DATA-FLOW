# ============================================================
# ANÁLISIS DE CONTENIDO TEMÁTICO POR GÉNERO DEL INTÉRPRETE
# ============================================================
# Complemento de analisis_obscenidad.R
# Responde: ¿difiere el contenido temático entre hombres y mujeres?
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)

# ── 1. CARGAR DATOS ──────────────────────────────────────────
# Busca el Excel limpio (igual que en el script principal)
buscar_archivo <- function(patron) {
  hits <- list.files(getwd(), pattern = patron, recursive = TRUE,
                     full.names = TRUE, ignore.case = TRUE)
  hits <- hits[!grepl("~\\$", hits)]   # descarta archivos temporales de Excel
  if (length(hits) == 0) {
    message("No encontré '", patron, "'. Elige el archivo manualmente.")
    return(file.choose())
  }
  hits[1]
}

ruta_canciones   <- buscar_archivo("canciones_sin_letras_repetidas\\.xlsx")
ruta_diccionario <- buscar_archivo("diccionario.*\\.xlsx$")

cat("Canciones :", ruta_canciones, "\n")
cat("Diccionario:", ruta_diccionario, "\n")

df  <- read_excel(ruta_canciones)
dic <- read_excel(ruta_diccionario)

# ── 2. FILTRAR SOLO HOMBRES Y MUJERES ────────────────────────
# La columna "Género del intérprete" puede tener valores como
# "Hombre", "Mujer", "Dúo mixto", "Grupo", etc.
# Solo trabajamos con las filas etiquetadas exactamente como
# "Hombre" o "Mujer".  Ajusta los valores si en tu Excel
# dicen algo diferente (ejecuta unique(df$`Género del intérprete`)
# para verlos todos).

df_gen <- df %>%
  filter(`Género del Intérprete` %in% c("Hombre", "Mujer")) %>%
  filter(!is.na(letra), nchar(trimws(letra)) > 0)

cat("\nCanciones con letra por género:\n")
print(table(df_gen$`Género del Intérprete`))

# ── 3. LIMPIAR LETRAS ────────────────────────────────────────
# Igual que en el script principal: minúsculas, sin tildes,
# sin signos, sin etiquetas de Genius, sin stopwords del español.
# Esto deja solo las palabras con contenido real.

limpiar_letra <- function(texto) {
  texto <- tolower(texto)
  # quitar etiquetas de Genius
  texto <- gsub("\\[.*?\\]", " ", texto)
  texto <- gsub("you might also like.*", " ", texto)
  texto <- gsub("\\d+embed.*", " ", texto)
  # normalizar tildes y eñe
  texto <- chartr("áéíóúüàèìòùâêîôûãõñ",
                  "aeiouuaeiouaeiouaon", texto)
  # quitar todo lo que no sea letra o espacio
  texto <- gsub("[^a-z ]", " ", texto)
  gsub("\\s+", " ", trimws(texto))
}

stopwords_es <- c(
  "de","la","el","en","y","a","los","las","un","una","con",
  "que","se","es","por","para","del","le","al","lo","su",
  "me","te","mi","tu","si","no","ya","yo","o","pero","como",
  "mas","más","muy","bien","tan","todo","toda","todos","todas",
  "hay","ser","fue","son","era","han","has","he","sus","este",
  "esta","eso","ese","esa","esos","esas","hay","puede","cuando",
  "donde","porque","que","aqui","ahi","alla","nos","voy","soy",
  "quiero","va","vamos","vienen","viene","da","dan","hace","hay"
)

df_gen <- df_gen %>%
  mutate(letra_limpia = limpiar_letra(letra))

# ── 4. CALCULAR PROPORCIONES POR CATEGORÍA DEL DICCIONARIO ───
# Para cada canción contamos cuántas palabras pertenecen
# a cada categoría temática del diccionario y calculamos
# la proporción sobre el total de palabras.
# Así el análisis es de contenido temático, no solo obscenidad.

# Normalizar diccionario igual que las letras
dic_norm <- dic %>%
  mutate(
    termino_norm = chartr("áéíóúüàèìòùâêîôûãõñ",
                          "aeiouuaeiouaeiouaon",
                          tolower(trimws(`Término/expresión`))),
    categoria = trimws(`Categoría principal`)
  ) %>%
  filter(!is.na(termino_norm), nchar(termino_norm) > 1) %>%
  distinct(termino_norm, .keep_all = TRUE)

categorias <- sort(unique(dic_norm$categoria))

# Función: dado el texto limpio de una canción, devuelve
# la proporción de palabras de cada categoría
proporciones_categoria <- function(texto_limpio) {
  palabras <- unlist(strsplit(texto_limpio, " "))
  palabras <- palabras[!palabras %in% stopwords_es & nchar(palabras) > 2]
  n_total  <- length(palabras)
  if (n_total == 0) return(setNames(rep(NA_real_, length(categorias)), categorias))

  sapply(categorias, function(cat) {
    terminos_cat <- dic_norm$termino_norm[dic_norm$categoria == cat]
    n_hits <- sum(palabras %in% terminos_cat)
    n_hits / n_total * 100
  })
}

# Aplicar a todas las canciones (puede tardar ~10-20 seg)
cat("\nCalculando proporciones por categoría...\n")
props_matrix <- t(sapply(df_gen$letra_limpia, proporciones_categoria))
colnames(props_matrix) <- paste0("pct_", gsub(" ", "_", categorias))

df_resultado <- bind_cols(
  df_gen %>% select(año, cancion, artista, `Género del Intérprete`),
  as.data.frame(props_matrix)
)

# ── 5. COMPARACIÓN ESTADÍSTICA HOMBRE vs MUJER ───────────────
# Para cada categoría temática:
#   - Promedio por género
#   - Prueba Mann-Whitney U (no asume normalidad, ideal para
#     proporciones con muchos ceros)
#   - Tamaño del efecto r = Z / sqrt(N)
# Una categoría "difiere significativamente" si p < 0.05

cols_cat <- grep("^pct_", names(df_resultado), value = TRUE)

comparacion <- lapply(cols_cat, function(col) {
  hombres <- df_resultado[[col]][df_resultado$`Género del Intérprete` == "Hombre"]
  mujeres  <- df_resultado[[col]][df_resultado$`Género del Intérprete` == "Mujer"]

  hombres <- hombres[!is.na(hombres)]
  mujeres  <- mujeres[!is.na(mujeres)]

  if (length(hombres) < 2 || length(mujeres) < 2) return(NULL)

  test   <- wilcox.test(hombres, mujeres, exact = FALSE)
  z_val  <- qnorm(test$p.value / 2)
  efecto <- abs(z_val) / sqrt(length(hombres) + length(mujeres))

  data.frame(
    categoria      = gsub("pct_", "", col),
    media_hombre   = round(mean(hombres), 4),
    media_mujer    = round(mean(mujeres), 4),
    diferencia     = round(mean(hombres) - mean(mujeres), 4),
    p_valor        = ifelse(test$p.value < 0.001, "< 0.001",
                            as.character(round(test$p.value, 3))),
    p_valor_num    = test$p.value,
    efecto_r       = round(efecto, 3),
    significativa  = ifelse(test$p.value < 0.05, "Sí", "No"),
    mayor_en       = ifelse(mean(hombres) > mean(mujeres), "Hombre", "Mujer")
  )
}) %>% bind_rows() %>%
  arrange(p_valor_num)

cat("\n--- CATEGORÍAS CON DIFERENCIA SIGNIFICATIVA ---\n")
print(comparacion %>% filter(significativa == "Sí") %>%
        select(categoria, media_hombre, media_mujer, p_valor, efecto_r, mayor_en))

# ── 6. GRÁFICOS ───────────────────────────────────────────────
dir.create("resultados", showWarnings = FALSE)

# 6a. Comparación de medias por categoría (todas)
# Muestra qué categorías usa más cada género
df_plot <- comparacion %>%
  select(categoria, media_hombre, media_mujer, significativa) %>%
  pivot_longer(cols = c(media_hombre, media_mujer),
               names_to  = "genero",
               values_to = "media") %>%
  mutate(
    genero    = ifelse(genero == "media_hombre", "Hombre", "Mujer"),
    categoria = reorder(categoria, media, sum)
  )

g1 <- ggplot(df_plot, aes(x = categoria, y = media, fill = genero)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("Hombre" = "#2196F3", "Mujer" = "#E91E63")) +
  labs(
    title    = "Proporción de palabras por categoría temática según género",
    subtitle = "% sobre el total de palabras de cada canción (promedio por género)",
    x = NULL, y = "Proporción media (%)", fill = "Género"
  ) +
  theme_minimal(base_size = 11)

ggsave("resultados/genero_categorias_comparacion.png", g1,
       width = 10, height = 7, dpi = 150)

# 6b. Solo las categorías con diferencia significativa
sig <- comparacion %>% filter(significativa == "Sí")

if (nrow(sig) > 0) {
  df_sig <- sig %>%
    select(categoria, media_hombre, media_mujer) %>%
    pivot_longer(cols = c(media_hombre, media_mujer),
                 names_to  = "genero",
                 values_to = "media") %>%
    mutate(genero = ifelse(genero == "media_hombre", "Hombre", "Mujer"),
           categoria = reorder(categoria, media, sum))

  g2 <- ggplot(df_sig, aes(x = categoria, y = media, fill = genero)) +
    geom_col(position = "dodge") +
    coord_flip() +
    scale_fill_manual(values = c("Hombre" = "#2196F3", "Mujer" = "#E91E63")) +
    labs(
      title    = "Categorías con diferencia significativa entre géneros (p < 0.05)",
      x = NULL, y = "Proporción media (%)", fill = "Género"
    ) +
    theme_minimal(base_size = 12)

  ggsave("resultados/genero_categorias_significativas.png", g2,
         width = 8, height = 5, dpi = 150)
} else {
  cat("No hubo categorías con diferencia significativa.\n")
}

# 6c. Boxplot de las 4 categorías más distintas
top4 <- comparacion %>%
  arrange(p_valor_num) %>%
  slice_head(n = 4) %>%
  pull(categoria)

df_box <- df_resultado %>%
  select(`Género del Intérprete`, all_of(paste0("pct_", gsub(" ", "_", top4)))) %>%
  pivot_longer(-`Género del Intérprete`,
               names_to  = "categoria",
               values_to = "proporcion") %>%
  mutate(categoria = gsub("pct_|_", " ", categoria) %>% trimws())

g3 <- ggplot(df_box,
             aes(x = `Género del Intérprete`, y = proporcion,
                 fill = `Género del Intérprete`)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.7) +
  facet_wrap(~categoria, scales = "free_y") +
  scale_fill_manual(values = c("Hombre" = "#2196F3", "Mujer" = "#E91E63")) +
  labs(
    title = "Distribución de las 4 categorías más diferenciadas por género",
    x = NULL, y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

ggsave("resultados/genero_top4_boxplot.png", g3,
       width = 9, height = 6, dpi = 150)

# ── 7. GUARDAR EXCEL ─────────────────────────────────────────
ruta_salida <- "resultados/analisis_genero_tematico.xlsx"
if (file.exists(ruta_salida)) {
  ruta_salida <- paste0("resultados/analisis_genero_tematico_",
                        format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
}

write_xlsx(
  list(
    comparacion_estadistica = comparacion %>% select(-p_valor_num),
    proporciones_por_cancion = df_resultado
  ),
  ruta_salida
)

# ── 8. RESUMEN ESCRITO ────────────────────────────────────────
n_h   <- sum(df_gen$`Género del Intérprete` == "Hombre")
n_m   <- sum(df_gen$`Género del Intérprete` == "Mujer")
n_sig <- sum(comparacion$p_valor_num < 0.05)
n_cat <- nrow(comparacion)

top_sig <- comparacion %>%
  filter(significativa == "Sí") %>%
  arrange(p_valor_num) %>%
  slice_head(n = 3)

resumen <- paste0(
  "ANÁLISIS DE CONTENIDO TEMÁTICO POR GÉNERO DEL INTÉRPRETE\n",
  "=========================================================\n\n",
  "Se compararon ", n_h, " canciones de intérpretes masculinos y ",
  n_m, " de intérpretes femeninas, usando las ",
  n_cat, " categorías temáticas del diccionario maestro.\n\n",

  if (n_sig == 0) {
    "No se encontraron diferencias estadísticamente significativas entre géneros en ninguna categoría (p < 0.05). El contenido temático del reggaetón es similar entre hombres y mujeres en esta muestra.\n"
  } else {
    paste0(
      "Se encontraron ", n_sig, " categoría(s) con diferencias estadísticamente significativas (p < 0.05):\n\n",
      paste(apply(top_sig, 1, function(r) {
        paste0("  - ", r["categoria"], ": mayor en ", r["mayor_en"],
               " (hombre: ", r["media_hombre"], "% vs mujer: ", r["media_mujer"],
               "%, p = ", r["p_valor"], ", efecto r = ", r["efecto_r"], ")")
      }), collapse = "\n"), "\n\n",
      "El tamaño del efecto (r) indica la magnitud de la diferencia:\n",
      "  r < 0.10 = efecto pequeño | 0.10-0.30 = moderado | > 0.30 = grande\n\n",
      "LIMITACIONES: la muestra tiene ", n_h + n_m,
      " canciones con letra disponible. Con pocos casos por género, ",
      "los resultados son indicativos y no generalizables a todo el reggaetón.\n"
    )
  }
)

cat("\n", resumen)
writeLines(resumen, "resultados/resumen_genero_tematico.txt")
cat("\nArchivos guardados en resultados/\n")