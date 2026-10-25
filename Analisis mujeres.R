# ============================================================
# ANÁLISIS DE EVOLUCIÓN TEMÁTICA — CANTANTES MUJERES (2010-2025)
# ============================================================
# Pregunta: ¿Ha cambiado la temática de las canciones de
# mujeres entre 2010 y 2025?
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)

# ── 1. CARGAR DATOS ──────────────────────────────────────────
buscar_archivo <- function(patron) {
  hits <- list.files(getwd(), pattern = patron, recursive = TRUE,
                     full.names = TRUE, ignore.case = TRUE)
  hits <- hits[!grepl("~\\$", hits)]
  if (length(hits) == 0) {
    message("No encontré '", patron, "'. Elige el archivo manualmente.")
    return(file.choose())
  }
  hits[1]
}

ruta_canciones   <- buscar_archivo("canciones_sin_letras_repetidas\\.xlsx")
ruta_diccionario <- buscar_archivo("diccionario")

cat("Canciones :", ruta_canciones, "\n")
cat("Diccionario:", ruta_diccionario, "\n")

df  <- read_excel(ruta_canciones)
dic <- read_excel(ruta_diccionario)

# ── 2. FILTRAR SOLO MUJERES CON LETRA ────────────────────────
# Ajusta el valor si en tu Excel dice algo distinto a "Mujer"
df_mujeres <- df %>%
  filter(`Género del Intérprete` == "Mujer") %>%
  filter(!is.na(letra), nchar(trimws(letra)) > 0)

cat("\nCanciones de mujeres con letra:", nrow(df_mujeres), "\n")
cat("Por año:\n")
print(sort(table(df_mujeres$año)))

# ── 3. LIMPIAR LETRAS ────────────────────────────────────────
limpiar_letra <- function(texto) {
  texto <- tolower(texto)
  texto <- gsub("\\[.*?\\]", " ", texto)
  texto <- gsub("you might also like.*", " ", texto)
  texto <- gsub("\\d+embed.*", " ", texto)
  texto <- chartr("áéíóúüàèìòùâêîôûãõñ",
                  "aeiouuaeiouaeiouaon", texto)
  texto <- gsub("[^a-z ]", " ", texto)
  gsub("\\s+", " ", trimws(texto))
}

stopwords_es <- c(
  "de","la","el","en","y","a","los","las","un","una","con",
  "que","se","es","por","para","del","le","al","lo","su",
  "me","te","mi","tu","si","no","ya","yo","o","pero","como",
  "mas","muy","bien","tan","todo","toda","todos","todas",
  "hay","ser","fue","son","era","han","has","he","sus","este",
  "esta","eso","ese","esa","esos","esas","nos","voy","soy",
  "quiero","va","vamos","vienen","viene","da","dan","hace"
)

df_mujeres <- df_mujeres %>%
  mutate(letra_limpia = limpiar_letra(letra))

# ── 4. PROPORCIONES POR CATEGORÍA ────────────────────────────
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

proporciones_categoria <- function(texto_limpio) {
  palabras <- unlist(strsplit(texto_limpio, " "))
  palabras <- palabras[!palabras %in% stopwords_es & nchar(palabras) > 2]
  n_total  <- length(palabras)
  if (n_total == 0) return(setNames(rep(NA_real_, length(categorias)), categorias))
  sapply(categorias, function(cat) {
    terminos_cat <- dic_norm$termino_norm[dic_norm$categoria == cat]
    sum(palabras %in% terminos_cat) / n_total * 100
  })
}

cat("\nCalculando proporciones por categoría...\n")
props_matrix <- t(sapply(df_mujeres$letra_limpia, proporciones_categoria))
colnames(props_matrix) <- paste0("pct_", gsub(" ", "_", categorias))

df_resultado <- bind_cols(
  df_mujeres %>% select(año, cancion, artista),
  as.data.frame(props_matrix)
)

# ── 5. TENDENCIA POR CATEGORÍA A LO LARGO DEL TIEMPO ─────────
# Para cada categoría: correlación de Spearman entre año y proporción
# y regresión lineal. Igual que en el script de tendencia general
# pero solo con mujeres.
cols_cat <- grep("^pct_", names(df_resultado), value = TRUE)

tendencias <- lapply(cols_cat, function(col) {
  datos <- df_resultado %>%
    filter(!is.na(.data[[col]])) %>%
    select(año, valor = all_of(col))
  
  if (nrow(datos) < 5) return(NULL)   # mínimo 5 obs para tener sentido
  
  sp   <- cor.test(datos$año, datos$valor, method = "spearman", exact = FALSE)
  lm_  <- lm(valor ~ año, data = datos)
  coef <- coef(summary(lm_))
  
  data.frame(
    categoria    = gsub("pct_", "", col),
    rho_spearman = round(sp$estimate, 3),
    p_spearman   = ifelse(sp$p.value < 0.001, "< 0.001",
                          as.character(round(sp$p.value, 3))),
    p_num        = sp$p.value,
    cambio_anual = round(coef["año", "Estimate"], 4),
    r2           = round(summary(lm_)$r.squared, 3),
    direccion    = ifelse(sp$estimate > 0, "Aumenta", "Disminuye"),
    significativa = ifelse(sp$p.value < 0.05, "Sí", "No")
  )
}) %>% bind_rows() %>%
  arrange(p_num)

cat("\n--- CATEGORÍAS CON TENDENCIA SIGNIFICATIVA (mujeres) ---\n")
print(tendencias %>% filter(significativa == "Sí") %>%
        select(categoria, rho_spearman, p_spearman, cambio_anual, direccion))

# ── 6. PROMEDIO POR AÑO (para graficar) ──────────────────────
promedio_anual <- df_resultado %>%
  group_by(año) %>%
  summarise(across(all_of(cols_cat), ~mean(.x, na.rm = TRUE)),
            n_canciones = n(), .groups = "drop")

# ── 7. GRÁFICOS ───────────────────────────────────────────────
dir.create("resultados", showWarnings = FALSE)

# 7a. Evolución de TODAS las categorías (heatmap por año)
# Muestra de un vistazo cómo cambia cada tema a lo largo del tiempo
df_heat <- promedio_anual %>%
  pivot_longer(all_of(cols_cat), names_to = "categoria", values_to = "media") %>%
  mutate(categoria = gsub("pct_|_", " ", categoria) %>% trimws())

g1 <- ggplot(df_heat, aes(x = año, y = categoria, fill = media)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#E91E63",
                      name = "Proporción\nmedia (%)") +
  scale_x_continuous(breaks = seq(2010, 2025, 2)) +
  labs(
    title    = "Evolución temática de canciones de mujeres (2010–2025)",
    subtitle = "Proporción media de palabras por categoría y año",
    x = "Año", y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("resultados/mujeres_heatmap_tematico.png", g1,
       width = 11, height = 7, dpi = 150)

# 7b. Líneas de evolución — solo categorías con tendencia significativa
sig_cats <- tendencias %>% filter(significativa == "Sí") %>% pull(categoria)

if (length(sig_cats) > 0) {
  cols_sig <- paste0("pct_", gsub(" ", "_", sig_cats))
  
  df_lineas <- df_resultado %>%
    select(año, all_of(cols_sig)) %>%
    pivot_longer(-año, names_to = "categoria", values_to = "proporcion") %>%
    mutate(categoria = gsub("pct_|_", " ", categoria) %>% trimws()) %>%
    group_by(año, categoria) %>%
    summarise(media = mean(proporcion, na.rm = TRUE), .groups = "drop")
  
  g2 <- ggplot(df_lineas, aes(x = año, y = media, color = categoria)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.15) +
    scale_x_continuous(breaks = seq(2010, 2025, 2)) +
    labs(
      title    = "Categorías con cambio significativo — cantantes mujeres",
      subtitle = "Línea de tendencia con banda de confianza 95%",
      x = "Año", y = "Proporción media (%)", color = "Categoría"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  
  ggsave("resultados/mujeres_tendencias_significativas.png", g2,
         width = 10, height = 6, dpi = 150)
} else {
  cat("No hubo categorías con tendencia significativa.\n")
}

# 7c. Boxplot por período (2010-2017 vs 2018-2025)
# Agrupa en dos mitades para ver si hay cambio general
# aunque no haya suficientes datos por año individual
df_resultado <- df_resultado %>%
  mutate(periodo = ifelse(año <= 2017, "2010–2017", "2018–2025"))

# Top 4 categorías con mayor proporción media
top4 <- colMeans(props_matrix, na.rm = TRUE) %>%
  sort(decreasing = TRUE) %>%
  head(4) %>%
  names()

df_box <- df_resultado %>%
  select(periodo, all_of(top4)) %>%
  pivot_longer(-periodo, names_to = "categoria", values_to = "proporcion") %>%
  mutate(categoria = gsub("pct_|_", " ", categoria) %>% trimws())

g3 <- ggplot(df_box, aes(x = periodo, y = proporcion, fill = periodo)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.7) +
  facet_wrap(~categoria, scales = "free_y") +
  scale_fill_manual(values = c("2010–2017" = "#9C27B0", "2018–2025" = "#E91E63")) +
  labs(
    title    = "Comparación por período — cantantes mujeres",
    subtitle = "Top 4 categorías temáticas más frecuentes",
    x = NULL, y = "Proporción (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

ggsave("resultados/mujeres_comparacion_periodos.png", g3,
       width = 9, height = 6, dpi = 150)

# ── 8. GUARDAR EXCEL ─────────────────────────────────────────
ruta_salida <- "resultados/analisis_evolucion_mujeres.xlsx"
if (file.exists(ruta_salida)) {
  ruta_salida <- paste0("resultados/analisis_evolucion_mujeres_",
                        format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
}

write_xlsx(
  list(
    tendencias_por_categoria = tendencias %>% select(-p_num),
    proporciones_por_cancion = df_resultado,
    promedio_anual           = promedio_anual
  ),
  ruta_salida
)

# ── 9. RESUMEN ESCRITO ────────────────────────────────────────
n_canciones <- nrow(df_mujeres)
n_años      <- n_distinct(df_mujeres$año)
n_sig       <- sum(tendencias$p_num < 0.05, na.rm = TRUE)

top3_sig <- tendencias %>%
  filter(significativa == "Sí") %>%
  slice_head(n = 3)

resumen <- paste0(
  "EVOLUCIÓN TEMÁTICA — CANTANTES MUJERES (2010–2025)\n",
  "===================================================\n\n",
  "Se analizaron ", n_canciones, " canciones de intérpretes femeninas ",
  "distribuidas en ", n_años, " años del período 2010–2025, ",
  "usando las ", length(categorias), " categorías temáticas del diccionario maestro.\n\n",
  
  if (n_sig == 0) {
    paste0(
      "No se encontró ninguna categoría con tendencia temporal significativa ",
      "(p < 0.05). El contenido temático de las canciones de mujeres no muestra ",
      "un cambio sistemático entre 2010 y 2025 en esta muestra. Esto puede deberse ",
      "al tamaño reducido de la muestra (", n_canciones, " canciones), que limita ",
      "el poder estadístico para detectar tendencias.\n"
    )
  } else {
    paste0(
      "Se encontraron ", n_sig, " categoría(s) con tendencia temporal ",
      "estadísticamente significativa (p < 0.05):\n\n",
      paste(apply(top3_sig, 1, function(r) {
        paste0("  - ", r["categoria"], ": ", tolower(r["direccion"]),
               " con el tiempo (ρ = ", r["rho_spearman"],
               ", p = ", r["p_spearman"],
               ", cambio estimado = ", r["cambio_anual"], "% por año)")
      }), collapse = "\n"), "\n\n",
      "NOTA: Con ", n_canciones, " canciones la tendencia es indicativa. ",
      "Al ampliar la muestra a 50+ canciones los resultados serán más robustos.\n"
    )
  }
)

cat("\n", resumen)
writeLines(resumen, "resultados/resumen_evolucion_mujeres.txt")
cat("\nArchivos guardados en resultados/\n")