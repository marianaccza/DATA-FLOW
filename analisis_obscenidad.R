# ============================================================
# Proporción de palabras "obscenas" por canción (reggaetón)
# usando el Diccionario Maestro como referencia
# ============================================================
# install.packages(c("readxl", "dplyr", "tidyr", "stringr",
#                    "stringi", "writexl", "ggplot2", "patchwork"))   # solo la primera vez
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(writexl)
library(ggplot2)

# ------------------------------------------------------------
# 0. CONFIGURACIÓN  (lo único que normalmente tienes que tocar)
# ------------------------------------------------------------
# Los archivos se buscan SOLOS por nombre dentro de la carpeta de trabajo
# y todas sus subcarpetas, así que no hay que escribir ninguna ruta.
# Si no encuentra alguno, abre una ventana para que lo selecciones.
buscar_archivo <- function(patron, descripcion) {
  hallados <- list.files(getwd(), pattern = patron, recursive = TRUE,
                         full.names = TRUE, ignore.case = TRUE)
  hallados <- hallados[!startsWith(basename(hallados), "~$")]  # temporales de Excel
  if (length(hallados) == 0) {
    message("No encontré ", descripcion, " dentro de ", getwd(),
            ". Selecciónalo en la ventana.")
    return(file.choose())
  }
  if (length(hallados) > 1) {
    message("Hay varios archivos para ", descripcion, "; uso el primero:\n  ", hallados[1])
  }
  hallados[1]
}

ruta_canciones   <- buscar_archivo("canciones_sin_letras_repetidas\\.xlsx$", "el Excel de canciones")
ruta_diccionario <- buscar_archivo("diccionario.*\\.xlsx$", "el diccionario")
ruta_salida      <- file.path(getwd(), "resultados", "analisis_obscenidad_por_cancion.xlsx")

cat("Canciones:  ", ruta_canciones, "\n")
cat("Diccionario:", ruta_diccionario, "\n")

# Qué categorías del diccionario cuentan como "obscenas".
# Opciones: SEXUALIDAD, DISCRIMINACION, VIOLENCIA, DROGAS, ALCOHOL, ...
# (el resto de categorías se calcula igual, aparte, en columnas pct_...)
categorias_obscenas <- c("SEXUALIDAD", "DISCRIMINACION")

# TRUE = no contar ad-libs tipo "ah", "eh", "yeah" en el total de palabras
excluir_onomatopeyas_del_total <- FALSE

# Palabras a cada lado para la columna "contexto" de la hoja de detalle
ventana_contexto <- 5

# ------------------------------------------------------------
# 1. FUNCIONES
# ------------------------------------------------------------

# Misma normalización para letras y diccionario: minúsculas, sin tildes
# (se conserva la ñ), sin apóstrofes ("vamo'" -> "vamo") y sin signos.
normalizar <- function(x) {
  x <- str_to_lower(x)
  x <- stri_trans_general(x, "NFD; [\\u0300-\\u0302\\u0304-\\u036F] Remove; NFC")
  x <- str_replace_all(x, "['\u2019\u2018`\u00b4]", "")
  x <- str_replace_all(x, "[^[:alnum:]]+", " ")
  str_squish(x)
}

# Quita restos de Genius: título "... Lyrics", [Coro], [Verso 1], etc.
limpiar_letra <- function(x) {
  x <- str_remove(x, "^[^\\n]*Lyrics")        # primera línea con "Lyrics"
  x <- str_remove_all(x, "\\[[^\\]]*\\]")     # encabezados entre corchetes
  x <- str_remove_all(x, "You might also like")
  x
}

# Busca primero las expresiones más largas y luego las palabras sueltas;
# una palabra ya usada en una expresión larga no se cuenta dos veces.
encontrar_coincidencias <- function(tokens, dic) {
  vacio <- tibble(posicion = integer(), forma = character(), n_palabras = integer())
  n_tok <- length(tokens)
  if (n_tok == 0) return(vacio)
  
  cubierto <- rep(FALSE, n_tok)
  res <- list()
  
  for (n in sort(unique(dic$n_palabras), decreasing = TRUE)) {
    if (n_tok < n) next
    formas  <- dic$forma[dic$n_palabras == n]
    inicios <- seq_len(n_tok - n + 1)
    ngramas <- vapply(inicios,
                      function(i) paste(tokens[i:(i + n - 1)], collapse = " "),
                      character(1))
    for (i in which(ngramas %in% formas)) {
      idx <- i:(i + n - 1)
      if (!any(cubierto[idx])) {
        cubierto[idx] <- TRUE
        res[[length(res) + 1]] <- tibble(posicion = i, forma = ngramas[i], n_palabras = n)
      }
    }
  }
  if (length(res) == 0) vacio else bind_rows(res)
}

# ------------------------------------------------------------
# 2. LEER DATOS
# ------------------------------------------------------------
canciones <- read_excel(ruta_canciones) %>%
  rename(anio = `año`) %>%
  mutate(id_cancion = row_number())

dic <- read_excel(ruta_diccionario, sheet = "Diccionario") %>%
  transmute(
    termino      = `Término/expresión`,
    forma        = normalizar(`Forma normalizada`),
    categoria    = `Categoría principal`,
    subcategoria = `Subcategoría`,
    explicitud   = `Explicitud propuesta`,
    polisemia    = Polisemia
  ) %>%
  filter(forma != "")

# Tras normalizar, dos entradas pueden quedar iguales (ej. "¿quieres?" y "quieres")
n_dup <- sum(duplicated(dic$forma))
if (n_dup > 0) {
  message("Aviso: ", n_dup, " entradas del diccionario quedaron duplicadas al ",
          "normalizar; se conserva la primera de cada una.")
  dic <- distinct(dic, forma, .keep_all = TRUE)
}
dic <- dic %>% mutate(n_palabras = str_count(forma, " ") + 1)

mal_escritas <- setdiff(categorias_obscenas, unique(dic$categoria))
if (length(mal_escritas) > 0) {
  stop("Categorías que no existen en el diccionario: ",
       paste(mal_escritas, collapse = ", "))
}
cat("Entradas del diccionario consideradas obscenas:",
    sum(dic$categoria %in% categorias_obscenas), "\n")

# ------------------------------------------------------------
# 3. ANALIZAR CADA CANCIÓN
# ------------------------------------------------------------
resultados <- lapply(seq_len(nrow(canciones)), function(k) {
  letra <- canciones$letra[k]
  tokens <- if (is.na(letra)) character(0) else
    str_split(normalizar(limpiar_letra(letra)), " ")[[1]]
  tokens <- tokens[tokens != ""]
  
  m <- encontrar_coincidencias(tokens, dic)
  if (nrow(m) > 0) {
    m <- m %>%
      mutate(contexto = vapply(posicion, function(i) {
        paste(tokens[max(1, i - ventana_contexto):
                       min(length(tokens), i + n_palabras - 1 + ventana_contexto)],
              collapse = " ")
      }, character(1))) %>%
      left_join(dic %>% select(-n_palabras), by = "forma")
  }
  m$id_cancion <- rep(canciones$id_cancion[k], nrow(m))
  list(total = length(tokens), matches = m)
})

totales <- tibble(
  id_cancion     = canciones$id_cancion,
  total_palabras = vapply(resultados, `[[`, numeric(1), "total")
)

detalle <- bind_rows(lapply(resultados, `[[`, "matches")) %>%
  left_join(canciones %>% select(id_cancion, cancion, artista, anio), by = "id_cancion") %>%
  select(id_cancion, cancion, artista, anio, posicion, forma, n_palabras,
         categoria, subcategoria, explicitud, polisemia, contexto) %>%
  arrange(id_cancion, posicion)

# ------------------------------------------------------------
# 4. PROPORCIONES POR CANCIÓN
# ------------------------------------------------------------
# Cada palabra del texto cubierta por una entrada cuenta 1
# (una expresión de 3 palabras suma 3).
detalle <- detalle %>%
  mutate(
    es_obscena    = categoria %in% categorias_obscenas,
    es_explicita  = normalizar(explicitud) == "explicito",
    es_polisemica = normalizar(polisemia) == "si"
  )

base <- canciones %>%
  select(id_cancion, cancion, artista, anio) %>%
  left_join(totales, by = "id_cancion")

if (excluir_onomatopeyas_del_total) {
  ono <- detalle %>%
    filter(categoria == "ONOMATOPEYAS") %>%
    group_by(id_cancion) %>% summarise(n_ono = sum(n_palabras), .groups = "drop")
  base <- base %>%
    left_join(ono, by = "id_cancion") %>%
    mutate(n_ono = coalesce(n_ono, 0),
           palabras_base = total_palabras - n_ono) %>%
    select(-n_ono)
} else {
  base <- base %>% mutate(palabras_base = total_palabras)
}

# 4a. Indicadores de obscenidad
indicadores <- detalle %>%
  group_by(id_cancion) %>%
  summarise(
    palabras_obscenas       = sum(n_palabras[es_obscena]),
    terminos_obscenos_dist  = n_distinct(forma[es_obscena]),
    palabras_explicitas     = sum(n_palabras[es_explicita]),
    palabras_obscenas_sin_polisemia = sum(n_palabras[es_obscena & !es_polisemica]),
    .groups = "drop"
  )

# 4b. Proporción por cada categoría del diccionario
por_categoria <- detalle %>%
  group_by(id_cancion, categoria) %>%
  summarise(n = sum(n_palabras), .groups = "drop") %>%
  pivot_wider(names_from = categoria, values_from = n,
              names_prefix = "n_", values_fill = 0)

resumen <- base %>%
  left_join(indicadores,   by = "id_cancion") %>%
  left_join(por_categoria, by = "id_cancion") %>%
  mutate(across(c(starts_with("palabras_"), terminos_obscenos_dist, starts_with("n_")),
                ~ coalesce(.x, 0))) %>%
  mutate(
    prop_obscenas_pct    = if_else(palabras_base > 0, 100 * palabras_obscenas / palabras_base, NA_real_),
    obscenas_x1000       = if_else(palabras_base > 0, 1000 * palabras_obscenas / palabras_base, NA_real_),
    prop_explicitas_pct  = if_else(palabras_base > 0, 100 * palabras_explicitas / palabras_base, NA_real_),
    prop_obscenas_sin_polisemia_pct =
      if_else(palabras_base > 0, 100 * palabras_obscenas_sin_polisemia / palabras_base, NA_real_)
  ) %>%
  mutate(across(starts_with("n_"),
                ~ if_else(palabras_base > 0, 100 * .x / palabras_base, NA_real_),
                .names = "pct_{sub('^n_', '', .col)}")) %>%
  select(-starts_with("n_"))

# Ordenar columnas: identificación -> obscenidad -> categorías
cols_pct <- sort(names(resumen)[startsWith(names(resumen), "pct_")])
resumen <- resumen %>%
  select(id_cancion, cancion, artista, anio, total_palabras, palabras_base,
         palabras_obscenas, prop_obscenas_pct, obscenas_x1000,
         terminos_obscenos_dist, palabras_explicitas, prop_explicitas_pct,
         palabras_obscenas_sin_polisemia, prop_obscenas_sin_polisemia_pct,
         all_of(cols_pct)) %>%
  mutate(across(where(is.numeric) & !c(id_cancion, anio, total_palabras, palabras_base,
                                       palabras_obscenas, terminos_obscenos_dist,
                                       palabras_explicitas, palabras_obscenas_sin_polisemia),
                ~ round(.x, 3)))

# ------------------------------------------------------------
# 5. RESUMEN GLOBAL Y RANKING
# ------------------------------------------------------------
global <- tibble(
  indicador = c("Canciones analizadas", "Canciones sin letra",
                "% obscenas: promedio", "% obscenas: mediana",
                "% obscenas: mínimo", "% obscenas: máximo",
                "% explícitas: promedio"),
  valor = c(nrow(resumen), sum(is.na(resumen$prop_obscenas_pct)),
            mean(resumen$prop_obscenas_pct, na.rm = TRUE),
            median(resumen$prop_obscenas_pct, na.rm = TRUE),
            min(resumen$prop_obscenas_pct, na.rm = TRUE),
            max(resumen$prop_obscenas_pct, na.rm = TRUE),
            mean(resumen$prop_explicitas_pct, na.rm = TRUE))
) %>% mutate(valor = round(valor, 3))

print(global)
cat("\nTop 10 canciones con mayor proporción de palabras obscenas:\n")
print(resumen %>% arrange(desc(prop_obscenas_pct)) %>%
        select(cancion, artista, anio, total_palabras, palabras_obscenas, prop_obscenas_pct) %>%
        head(10))

# ------------------------------------------------------------
# 5b. ¿AUMENTARON LAS PALABRAS OBSCENAS CON LOS AÑOS?
# ------------------------------------------------------------
datos_t <- resumen %>% filter(palabras_base > 0)

# Promedio y mediana por año
tabla_anual <- datos_t %>%
  group_by(anio) %>%
  summarise(canciones               = n(),
            promedio_obscenas_pct   = mean(prop_obscenas_pct),
            mediana_obscenas_pct    = median(prop_obscenas_pct),
            promedio_explicitas_pct = mean(prop_explicitas_pct),
            .groups = "drop") %>%
  mutate(across(c(promedio_obscenas_pct, mediana_obscenas_pct, promedio_explicitas_pct),
                ~ round(.x, 3)))

# Tres pruebas complementarias para un indicador (n_col = columna con el conteo):
#  - Spearman: ¿hay relación creciente/decreciente? (no supone forma de la distribución)
#  - Regresión lineal: cuántos puntos porcentuales cambia por año (con IC 95 %)
#  - GLM cuasibinomial: usa palabras obscenas vs. total de cada canción;
#    da el cambio relativo por año (% sobre la proporción)
evaluar_tendencia <- function(datos, n_col, indicador) {
  conteo <- datos[[n_col]]
  pct    <- 100 * conteo / datos$palabras_base
  
  sp  <- suppressWarnings(cor.test(datos$anio, pct, method = "spearman", exact = FALSE))
  ols <- lm(pct ~ anio, data = datos)
  co  <- summary(ols)$coefficients["anio", ]
  ci  <- confint(ols)["anio", ]
  glm_ <- glm(cbind(conteo, datos$palabras_base - conteo) ~ anio,
              family = quasibinomial, data = datos)
  cg  <- summary(glm_)$coefficients["anio", ]
  
  tibble(indicador = indicador,
         spearman_rho = unname(sp$estimate), spearman_p = sp$p.value,
         cambio_por_anio_pp = unname(co["Estimate"]),
         ic95_inf = unname(ci[1]), ic95_sup = unname(ci[2]),
         regresion_p = unname(co["Pr(>|t|)"]), r2 = summary(ols)$r.squared,
         cambio_relativo_por_anio_pct = 100 * (exp(unname(cg["Estimate"])) - 1),
         glm_p = unname(cg["Pr(>|t|)"]))
}

# Formatea valores p: 0.0003 -> "< 0.001"
fmt_p <- function(p) ifelse(p < 0.001, "< 0.001", sprintf("= %.3f", p))

indicadores_t <- tibble(
  col    = c("palabras_obscenas", "palabras_explicitas"),
  nombre = c("Obscenas (categorías elegidas)", "Solo términos explícitos")
)
tests_tendencia <- bind_rows(lapply(seq_len(nrow(indicadores_t)), function(i) {
  evaluar_tendencia(datos_t, indicadores_t$col[i], indicadores_t$nombre[i])
}))

cat("\n===== TENDENCIA POR AÑO (", min(datos_t$anio), "-", max(datos_t$anio), ") =====\n", sep = "")
for (i in seq_len(nrow(tests_tendencia))) {
  t <- tests_tendencia[i, ]
  veredicto <- if (t$spearman_p < 0.05) {
    if (t$spearman_rho > 0) "AUMENTA de forma estadísticamente significativa"
    else "DISMINUYE de forma estadísticamente significativa"
  } else {
    "no hay evidencia suficiente de un cambio sostenido"
  }
  cat(sprintf("\n%s: %s (Spearman rho = %.2f, p %s)\n", t$indicador, veredicto,
              t$spearman_rho, fmt_p(t$spearman_p)))
  cat(sprintf("  Cambio estimado: %+.3f puntos porcentuales por año (IC 95 %%: %+.3f a %+.3f; p %s)\n",
              t$cambio_por_anio_pp, t$ic95_inf, t$ic95_sup, fmt_p(t$regresion_p)))
  cat(sprintf("  Cambio relativo: %+.1f %% por año (GLM, p %s) | R2 = %.3f\n",
              t$cambio_relativo_por_anio_pct, fmt_p(t$glm_p), t$r2))
}
tests_tendencia <- tests_tendencia %>% mutate(across(where(is.numeric), ~ round(.x, 4)))

# ------------------------------------------------------------
# 6. GUARDAR
# ------------------------------------------------------------
dir.create(dirname(ruta_salida), showWarnings = FALSE, recursive = TRUE)

hojas <- list(
  resumen_por_cancion   = resumen,
  resumen_global        = global,
  tendencia_por_anio    = tabla_anual,
  tests_tendencia       = tests_tendencia,
  detalle_coincidencias = detalle %>% select(-es_obscena, -es_explicita, -es_polisemica)
)

# Si el Excel de resultados está abierto en Excel (o la carpeta no deja
# escribir), guarda una copia con fecha y hora en vez de detener el script.
guardado <- tryCatch({ write_xlsx(hojas, ruta_salida); TRUE },
                     error = function(e) FALSE)
if (!guardado) {
  ruta_alt <- sub("\\.xlsx$", paste0("_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
                  ruta_salida)
  message("No pude escribir ", basename(ruta_salida),
          " (¿está abierto en Excel?). Guardo una copia como ", basename(ruta_alt))
  write_xlsx(hojas, ruta_alt)
  ruta_salida <- ruta_alt
}
cat("\nGuardado en:", ruta_salida, "\n")

# ------------------------------------------------------------
# 7. GRÁFICOS  (se muestran en RStudio y se guardan como PNG en resultados/)
# ------------------------------------------------------------
carpeta_graficos <- dirname(ruta_salida)
color_principal  <- "#C0392B"
color_gris       <- "#95A5A6"
tema <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        panel.grid.minor = element_blank())
nota_def <- paste("Palabras obscenas = categorías:", paste(categorias_obscenas, collapse = " + "))

datos   <- resumen %>% filter(total_palabras > 0)      # sin canciones sin letra
media   <- mean(datos$prop_obscenas_pct)
mediana <- median(datos$prop_obscenas_pct)

# 7a. Histograma de la proporción por canción
g_hist <- ggplot(datos, aes(prop_obscenas_pct)) +
  geom_histogram(binwidth = 0.5, boundary = 0, fill = color_principal, color = "white") +
  geom_vline(xintercept = media,   linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = mediana, linetype = "dotted", linewidth = 0.8) +
  scale_x_continuous(breaks = seq(0, ceiling(max(datos$prop_obscenas_pct)), by = 1)) +
  labs(title = "Distribución de palabras obscenas por canción",
       subtitle = sprintf("Promedio %.2f%% (discontinua) · Mediana %.2f%% (punteada) · %d canciones",
                          media, mediana, nrow(datos)),
       x = "% de palabras obscenas en la canción", y = "Número de canciones",
       caption = nota_def) +
  tema

# 7b. Evolución por año (cada punto es una canción; la línea es el promedio del año)
por_anio <- datos %>%
  group_by(anio) %>%
  summarise(promedio = mean(prop_obscenas_pct), .groups = "drop")

g_anio <- ggplot(datos, aes(anio, prop_obscenas_pct)) +
  geom_point(alpha = 0.4, color = color_gris, size = 2) +
  geom_line(data = por_anio, aes(y = promedio), color = color_principal, linewidth = 1) +
  geom_point(data = por_anio, aes(y = promedio), color = color_principal, size = 2.8) +
  scale_x_continuous(breaks = seq(min(datos$anio), max(datos$anio), by = 2)) +
  labs(title = "Evolución por año",
       subtitle = "Puntos grises: canciones · Línea roja: promedio anual",
       x = "Año", y = "% de palabras obscenas") +
  tema

# 7c. Las 15 canciones con mayor proporción
top <- datos %>%
  arrange(desc(prop_obscenas_pct)) %>%
  slice_head(n = 15) %>%
  mutate(etiqueta = str_trunc(paste0(cancion, " (", artista, ", ", anio, ")"), 50),
         etiqueta = reorder(etiqueta, prop_obscenas_pct))

g_top <- ggplot(top, aes(prop_obscenas_pct, etiqueta)) +
  geom_col(fill = color_principal) +
  geom_text(aes(label = sprintf("%.1f%%", prop_obscenas_pct)), hjust = -0.15, size = 3.3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Top 15 canciones con mayor proporción",
       x = "% de palabras obscenas", y = NULL) +
  tema +
  theme(panel.grid.major.y = element_blank())

# 7d. Promedio por categoría del diccionario (todas las categorías)
cat_prom <- datos %>%
  summarise(across(starts_with("pct_"), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "categoria", values_to = "promedio") %>%
  mutate(categoria = sub("^pct_", "", categoria),
         cuenta = categoria %in% categorias_obscenas,
         categoria = reorder(categoria, promedio))

g_cat <- ggplot(cat_prom, aes(promedio, categoria, fill = cuenta)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = color_principal, "FALSE" = color_gris),
                    labels = c("TRUE" = "Sí", "FALSE" = "No"),
                    name = "Cuenta como\nobscena") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Peso de cada categoría del diccionario",
       subtitle = "Promedio del % de palabras por canción",
       x = "% promedio de palabras", y = NULL) +
  tema +
  theme(panel.grid.major.y = element_blank())

# Mostrar en RStudio
print(g_hist); print(g_anio); print(g_top); print(g_cat)

# Guardar cada gráfico
ggsave(file.path(carpeta_graficos, "grafico_histograma.png"),      g_hist, width = 9,  height = 5.5, dpi = 150, bg = "white")
ggsave(file.path(carpeta_graficos, "grafico_evolucion_anual.png"), g_anio, width = 9,  height = 5.5, dpi = 150, bg = "white")
ggsave(file.path(carpeta_graficos, "grafico_top15_canciones.png"), g_top,  width = 10, height = 6,   dpi = 150, bg = "white")
ggsave(file.path(carpeta_graficos, "grafico_categorias.png"),      g_cat,  width = 9,  height = 6.5, dpi = 150, bg = "white")

# Panel resumen con los 4 gráficos juntos (necesita el paquete patchwork)
if (requireNamespace("patchwork", quietly = TRUE)) {
  panel <- patchwork::wrap_plots(g_hist, g_anio, g_top, g_cat, ncol = 2) +
    patchwork::plot_annotation(
      title = "Resumen: palabras obscenas en canciones de reggaetón",
      theme = theme(plot.title = element_text(face = "bold", size = 16)))
  print(panel)
  ggsave(file.path(carpeta_graficos, "grafico_resumen_panel.png"), panel,
         width = 18, height = 12, dpi = 150, bg = "white")
} else {
  message("Para el panel resumen instala patchwork: install.packages('patchwork')")
}
cat("Gráficos guardados en:", carpeta_graficos, "\n")

# 7e. Tendencia: ¿suben o bajan con los años? (puntos = canciones, rojo = promedio anual,
#     línea negra = tendencia lineal con su banda de confianza)
long <- bind_rows(lapply(seq_len(nrow(indicadores_t)), function(i) {
  tibble(anio = datos_t$anio,
         pct = 100 * datos_t[[indicadores_t$col[i]]] / datos_t$palabras_base,
         indicador = indicadores_t$nombre[i])
})) %>%
  left_join(tests_tendencia %>%
              mutate(panel = sprintf("%s\n%+.3f pp por año · Spearman p %s",
                                     indicador, cambio_por_anio_pp, fmt_p(spearman_p))) %>%
              select(indicador, panel), by = "indicador")

medias_t <- long %>% group_by(panel, anio) %>% summarise(pct = mean(pct), .groups = "drop")

g_tend <- ggplot(long, aes(anio, pct)) +
  geom_point(alpha = 0.3, color = color_gris, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", fill = "grey70", linewidth = 0.8) +
  geom_line(data = medias_t, color = color_principal, linewidth = 1) +
  geom_point(data = medias_t, color = color_principal, size = 2.6) +
  facet_wrap(~ panel, scales = "free_y") +
  scale_x_continuous(breaks = seq(min(long$anio), max(long$anio), by = 3)) +
  labs(title = "¿Aumentaron las palabras obscenas con los años?",
       subtitle = "Rojo: promedio anual · Negro: tendencia lineal con banda de confianza al 95 %",
       x = "Año", y = "% de palabras",
       caption = nota_def) +
  tema
print(g_tend)
ggsave(file.path(carpeta_graficos, "grafico_tendencia.png"), g_tend,
       width = 12, height = 5.5, dpi = 150, bg = "white")

# ------------------------------------------------------------
# 8. RESUMEN ESCRITO DE HALLAZGOS
#    (el texto se arma solo con los números de TUS resultados;
#     se imprime en la consola y se guarda en resultados/resumen_hallazgos.txt)
# ------------------------------------------------------------
num   <- function(x, d = 2) format(round(x, d), nsmall = d, decimal.mark = ",")
numsg <- function(x, d = 3) paste0(ifelse(x >= 0, "+", "-"), num(abs(x), d))
pct   <- function(x, d = 2) paste0(num(x, d), " %")
p_es  <- function(p) sub("\\.", ",", fmt_p(p))
parrafo <- function(...) c(strwrap(paste0(...), width = 95), "")

n_can  <- nrow(datos)
n_sin  <- nrow(resumen) - n_can
n_cero <- sum(datos$palabras_obscenas == 0)
n_alto <- sum(datos$prop_obscenas_pct >= 2)
anio_i <- min(datos$anio); anio_f <- max(datos$anio)

top3 <- datos %>% arrange(desc(prop_obscenas_pct)) %>% slice_head(n = 3)
top_terminos <- detalle %>% filter(es_obscena) %>% count(forma, sort = TRUE) %>% slice_head(n = 5)
poli_ejemplos <- detalle %>% filter(es_obscena, es_polisemica) %>%
  count(forma, sort = TRUE) %>% slice_head(n = 3) %>% pull(forma)
anio_max <- tabla_anual %>% slice_max(promedio_obscenas_pct, n = 1, with_ties = FALSE)
anio_min <- tabla_anual %>% slice_min(promedio_obscenas_pct, n = 1, with_ties = FALSE)
cat_ord  <- cat_prom %>% mutate(categoria = as.character(categoria)) %>% arrange(desc(promedio))
cat_obs  <- cat_ord %>% filter(categoria %in% categorias_obscenas)

t1 <- tests_tendencia[1, ]   # obscenas (categorías elegidas)
t2 <- tests_tendencia[2, ]   # solo explícitas
ajuste <- lm(prop_obscenas_pct ~ anio, data = datos)
y_ini  <- unname(predict(ajuste, data.frame(anio = anio_i)))
y_fin  <- unname(predict(ajuste, data.frame(anio = anio_f)))

# --- Evolución por año
if (t1$spearman_p < 0.05) {
  dir1 <- if (t1$spearman_rho > 0) "aumentó" else "disminuyó"
  txt_tend <- c(
    parrafo("Con los datos de esta muestra, la proporción de palabras obscenas ", dir1,
            " de forma estadísticamente significativa entre ", anio_i, " y ", anio_f,
            " (Spearman rho = ", num(t1$spearman_rho), ", p ", p_es(t1$spearman_p), "). ",
            "La línea de tendencia pasa de aproximadamente ", pct(y_ini), " en ", anio_i,
            " a ", pct(y_fin), " en ", anio_f, ". El cambio estimado es de ",
            numsg(t1$cambio_por_anio_pp), " puntos porcentuales por año (IC 95 %: ",
            numsg(t1$ic95_inf), " a ", numsg(t1$ic95_sup), "), es decir, un cambio relativo de ",
            numsg(t1$cambio_relativo_por_anio_pct, 1), " % por año."),
    if (t1$r2 < 0.10)
      parrafo("Aun así, el año explica solo el ", pct(100 * t1$r2, 1), " de la variación entre canciones ",
              "(R2 = ", num(t1$r2, 3), "): la tendencia existe pero es débil, y el resto de la ",
              "variación se debe a factores que este análisis no mide.")
  )
} else {
  txt_tend <- parrafo("No se encontró evidencia suficiente de un cambio sostenido en la proporción de ",
                      "palabras obscenas entre ", anio_i, " y ", anio_f, " (Spearman rho = ",
                      num(t1$spearman_rho), ", p ", p_es(t1$spearman_p), ").")
}
txt_expl <- if (t2$spearman_p < 0.05) {
  parrafo("Al considerar solo los términos marcados como explícitos, el resultado es ",
          if (sign(t2$spearman_rho) == sign(t1$spearman_rho)) "en la misma dirección" else "en dirección contraria",
          " (rho = ", num(t2$spearman_rho), ", p ", p_es(t2$spearman_p), "; cambio relativo de ",
          numsg(t2$cambio_relativo_por_anio_pct, 1), " % por año).")
} else {
  parrafo("Al considerar solo los términos marcados como explícitos no se encontró un cambio ",
          "estadísticamente significativo (rho = ", num(t2$spearman_rho), ", p ", p_es(t2$spearman_p), ").")
}

# --- Texto completo
texto <- c(
  strrep("=", 70),
  "RESUMEN DE HALLAZGOS: PALABRAS OBSCENAS EN CANCIONES DE REGGAETÓN",
  strrep("=", 70), "",
  
  "1. QUÉ SE ANALIZÓ",
  parrafo("Se analizaron ", n_can, " canciones (", anio_i, "-", anio_f, "; ",
          format(sum(datos$total_palabras), big.mark = ".", decimal.mark = ","),
          " palabras en total)",
          if (n_sin == 1) "; 1 canción sin letra quedó fuera" else
            if (n_sin > 1) paste0("; ", n_sin, " canciones sin letra quedaron fuera") else "",
          ". Cada letra se comparó con el Diccionario Maestro (",
          format(nrow(dic), big.mark = ".", decimal.mark = ","), " términos). ",
          "Se consideran obscenas las categorías ", paste(categorias_obscenas, collapse = ", "),
          " (", sum(dic$categoria %in% categorias_obscenas), " entradas del diccionario). ",
          "La proporción de una canción es el porcentaje de sus palabras que coincide con esos términos."),
  
  "2. HALLAZGO GENERAL",
  parrafo("En promedio, el ", pct(mean(datos$prop_obscenas_pct)), " de las palabras de una canción es obscena ",
          "(mediana ", pct(median(datos$prop_obscenas_pct)), "; rango de ",
          pct(min(datos$prop_obscenas_pct)), " a ", pct(max(datos$prop_obscenas_pct)), "). ",
          n_cero, " canciones (", pct(100 * n_cero / n_can, 1), ") no contienen ninguna y ",
          n_alto, " (", pct(100 * n_alto / n_can, 1), ") superan el 2 %.",
          if (median(datos$prop_obscenas_pct) < mean(datos$prop_obscenas_pct))
            " La distribución es asimétrica: unas pocas canciones con valores altos elevan el promedio por encima de la mediana." else ""),
  
  "3. CANCIONES CON MAYOR PROPORCIÓN",
  parrafo("Las tres canciones con mayor proporción son «", top3$cancion[1], "» (", top3$artista[1], ", ",
          top3$anio[1], ", ", pct(top3$prop_obscenas_pct[1]), "), «", top3$cancion[2], "» (",
          top3$artista[2], ", ", top3$anio[2], ", ", pct(top3$prop_obscenas_pct[2]), ") y «",
          top3$cancion[3], "» (", top3$artista[3], ", ", top3$anio[3], ", ",
          pct(top3$prop_obscenas_pct[3]), ")."),
  
  "4. TÉRMINOS MÁS FRECUENTES",
  parrafo("Los términos obscenos más repetidos son: ",
          paste0(top_terminos$forma, " (", top_terminos$n, " veces)", collapse = ", "),
          ". Se cuentan todas las apariciones, de modo que los coros repetidos pesan más."),
  
  "5. CATEGORÍAS DEL DICCIONARIO",
  parrafo("De las ", nrow(cat_ord), " categorías del diccionario, las de mayor peso promedio por canción son ",
          paste0(cat_ord$categoria[1:3], " (", pct(cat_ord$promedio[1:3]), ")", collapse = ", "), ". ",
          "Entre las categorías que cuentan como obscenas: ",
          paste0(cat_obs$categoria, " promedia ", pct(cat_obs$promedio), collapse = "; "), "."),
  
  "6. EVOLUCIÓN A LO LARGO DE LOS AÑOS",
  parrafo("El promedio anual más alto fue el de ", anio_max$anio, " (", pct(anio_max$promedio_obscenas_pct),
          ") y el más bajo el de ", anio_min$anio, " (", pct(anio_min$promedio_obscenas_pct), ")."),
  txt_tend,
  txt_expl,
  
  "7. LIMITACIONES A TENER EN CUENTA",
  parrafo("a) El diccionario es una propuesta inicial sin validación manual completa y algunos términos tienen ",
          "más de un sentido",
          if (length(poli_ejemplos) > 0) paste0(" (por ejemplo: ", paste(poli_ejemplos, collapse = ", "), ")") else "",
          ", lo que puede inflar las cifras. Excluyendo los términos marcados como ambiguos, el promedio baja a ",
          pct(mean(datos$prop_obscenas_sin_polisemia_pct)), "."),
  parrafo("b) Hay alrededor de ", round(mean(tabla_anual$canciones)), " canciones por año, por lo que los ",
          "promedios anuales son inestables: pocas canciones muy explícitas los mueven bastante."),
  parrafo("c) Una tendencia estadística indica asociación, no causa, y vale para esta muestra de canciones, ",
          "no necesariamente para todo el reggaetón."),
  parrafo("d) Se recomienda revisar la hoja 'detalle_coincidencias' del Excel para validar en contexto las ",
          "palabras detectadas.")
)

writeLines(texto)
ruta_resumen <- file.path(carpeta_graficos, "resumen_hallazgos.txt")
con <- file(ruta_resumen, open = "w", encoding = "UTF-8")
writeLines(texto, con)
close(con)
cat("Resumen escrito guardado en:", ruta_resumen, "\n")