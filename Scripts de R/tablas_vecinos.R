# Tablas de municipios vecinos con estatus de tratamiento opuesto (d2_eucl, 4 puntos de corte)
# Vecinos: municipios de la muestra (37) que se tocan en el MGN 2021 (borde o vértice compartido).
# Tratamientos definidos en Master.do (sección 4.1); formato de tablas como en Master.do (sección 5.3.2).
library(sf)
library(haven)
library(officer)
library(flextable)

raiz    <- "/Users/samuelquevedo/Mi unidad (samuelalgo16@hotmail.com)/Andes/Investigación/CESED/Luces_Nocturnas_Coca_y_Cannabis"
fuentes <- file.path(raiz, "Análisis geográfico y mapas/Shapefiles y fuentes")
tablas  <- file.path(raiz, "Análisis geográfico y mapas/export/tables")

shp_mpios <- file.path(fuentes, "MGN_2021_COLOMBIA/ADMINISTRATIVO/MGN_MPIO_POLITICO.shp")
dta_eucl  <- file.path(raiz, "STATA/data/auxiliary/tratados_distancia_euclidiana.dta")

dir.create(tablas, showWarnings = FALSE)

# ---- 0. Insumos ----

tratados_eucl <- read_dta(dta_eucl)
tratados_eucl$MPIO_CDPMP <- sprintf("%05d", as.integer(tratados_eucl$mpio_cdpmp))
stopifnot(nrow(tratados_eucl) == 37)

mpios   <- st_read(shp_mpios, quiet = TRUE)
muestra <- st_transform(mpios[match(tratados_eucl$MPIO_CDPMP, mpios$MPIO_CDPMP), ], 9377)
stopifnot(!anyNA(muestra$MPIO_CDPMP))
muestra$nombre <- paste0(muestra$MPIO_CNMBR, " (", muestra$DPTO_CNMBR, ")")

dist <- tratados_eucl$dist_centroide_pnis_centroide

# Puntos de corte en el orden de la Tabla 6.2 de Master.do
cortes <- data.frame(
  var    = c("mediana",       "mean",  "p60",          "p75"),
  titulo = c("Mediana (p50)", "Media", "Percentil 60", "Percentil 75")
)
estatus <- sapply(paste0("d2_eucl_", cortes$var), function(v) as.integer(tratados_eucl[[v]]))

# ---- 1. Pares de vecinos dentro de la muestra ----

vecinos <- st_touches(muestra)
pares <- do.call(rbind, lapply(seq_along(vecinos), function(i) {
  j <- vecinos[[i]][vecinos[[i]] > i]
  if (length(j)) data.frame(i = i, j = j)
}))

# Cada par se ordena del más cercano al más lejano al PNIS alta prob.
pares$cerca <- ifelse(dist[pares$i] <= dist[pares$j], pares$i, pares$j)
pares$lejos <- ifelse(dist[pares$i] <= dist[pares$j], pares$j, pares$i)
opuesto <- estatus[pares$cerca, , drop = FALSE] != estatus[pares$lejos, , drop = FALSE]

# Si el estatus difiere, el más cercano es el tratado (los indicadores son "distancia <= corte")
stopifnot(estatus[pares$cerca, ][opuesto] == 1, estatus[pares$lejos, ][opuesto] == 0)
message("Pares de vecinos en la muestra: ", nrow(pares),
        " | con estatus opuesto en algún corte: ", sum(rowSums(opuesto) > 0))

# ---- 2. Formato común (putdocx de Master.do) ----

borde <- fp_border(color = "black", width = 0.5)

formato <- function(ft, centradas, anchos) {
  ft <- font(ft, fontname = "Times New Roman", part = "all")
  ft <- fontsize(ft, size = 10, part = "all")
  ft <- padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 4, padding.right = 4, part = "all")
  ft <- bold(ft, part = "header")
  ft <- bold(ft, part = "footer")
  ft <- bg(ft, bg = "#D3D3D3", part = "header")
  ft <- border_outer(ft, border = borde, part = "all")
  ft <- border_inner(ft, border = borde, part = "all")
  ft <- align(ft, j = centradas, align = "center", part = "all")
  ft <- valign(ft, valign = "center", part = "all")
  width(ft, width = anchos)
}

guardar <- function(ft, titulo, nota, archivo) {
  doc <- read_docx()
  doc <- body_set_default_section(doc, prop_section(
    page_size    = page_size(width = 8.5, height = 11, orient = "portrait"),
    page_margins = page_mar(top = 1, bottom = 1, left = 0.75, right = 0.75)))
  centro <- fp_par(text.align = "center")
  doc <- body_add_fpar(doc, fpar(ftext(titulo, fp_text(font.family = "Times New Roman",
                                                         font.size = 10, bold = TRUE)), fp_p = centro))
  doc <- body_add_fpar(doc, fpar(ftext(nota, fp_text(font.family = "Times New Roman",
                                                       font.size = 9, italic = TRUE)), fp_p = centro))
  doc <- body_add_flextable(doc, ft, align = "center")
  print(doc, target = file.path(tablas, archivo))
}

nota_dist <- paste("Distancia euclidiana (línea recta) del centroide municipal al centroide del municipio",
                   "'Alta prob. PNIS' más cercano; el estatus de cada municipio por punto de corte se reporta en la Tabla 6.2.")

# ---- 3. Tabla 6.4: número de vecinos con estatus opuesto por municipio ----

n_opuestos <- sapply(seq_len(ncol(estatus)), function(k)
  sapply(seq_along(vecinos), function(i) sum(estatus[vecinos[[i]], k] != estatus[i, k])))

conteo <- data.frame(
  codigo  = as.character(as.integer(tratados_eucl$mpio_cdpmp)),
  dpto    = muestra$DPTO_CNMBR,
  mpio    = muestra$MPIO_CNMBR,
  vecinos = as.character(lengths(vecinos))
)
conteo[cortes$var] <- lapply(seq_len(ncol(n_opuestos)), function(k) as.character(n_opuestos[, k]))

# Mismo orden que Master.do: gsort -d2_eucl_mediana mpio_cdpmp
conteo <- conteo[order(-estatus[, "d2_eucl_mediana"], tratados_eucl$MPIO_CDPMP), ]

# Cada par aparece dos veces en las filas (una por municipio)
stopifnot(colSums(n_opuestos) == 2 * colSums(opuesto))

ft_conteo <- flextable(conteo)
ft_conteo <- set_header_labels(ft_conteo, values = c(
  codigo = "Código", dpto = "Departamento", mpio = "Municipio", vecinos = "Vecinos en la muestra",
  setNames(cortes$titulo, cortes$var)))
ft_conteo <- add_footer_row(ft_conteo,
                            values = c("Total de pares de vecinos", nrow(pares), colSums(opuesto)),
                            colwidths = c(3, 1, rep(1, nrow(cortes))))
ft_conteo <- formato(ft_conteo, centradas = c(1, 4:8),
                     anchos = c(0.6, 1.3, 1.6, 0.7, rep(0.7, 4)))

guardar(ft_conteo,
        titulo  = "Tabla 6.4. Municipios vecinos con estatus de tratamiento opuesto según puntos de corte del indicador d2 (distancia euclidiana)",
        nota    = paste("Vecinos en la muestra: municipios de la muestra (37) que comparten frontera con el municipio (MGN 2021).",
                        "Columnas de puntos de corte: número de esos vecinos con el estatus de tratamiento opuesto (tratado frente a control).",
                        "La última fila reporta el número de pares de vecinos; cada par se cuenta dos veces en las filas.",
                        nota_dist),
        archivo = "tabla_vecinos_opuestos_d2_eucl.docx")

# ---- 4. Tabla 6.5: pares de vecinos con estatus opuesto en al menos un punto de corte ----

pares_op <- pares[rowSums(opuesto) > 0, ]
op       <- opuesto[rowSums(opuesto) > 0, , drop = FALSE]
orden    <- order(tratados_eucl$MPIO_CDPMP[pares_op$cerca], tratados_eucl$MPIO_CDPMP[pares_op$lejos])
pares_op <- pares_op[orden, ]
op       <- op[orden, , drop = FALSE]

detalle <- data.frame(
  cerca   = muestra$nombre[pares_op$cerca],
  d_cerca = sprintf("%.1f", dist[pares_op$cerca]),
  lejos   = muestra$nombre[pares_op$lejos],
  d_lejos = sprintf("%.1f", dist[pares_op$lejos])
)
detalle[cortes$var] <- lapply(seq_len(ncol(op)), function(k) ifelse(op[, k], "Sí", "No"))
for (r in seq_len(nrow(detalle))) {
  message("  ", detalle$cerca[r], " – ", detalle$lejos[r], ": ",
          paste(cortes$titulo[op[r, ]], collapse = ", "))
}

ft_pares <- flextable(detalle)
ft_pares <- set_header_labels(ft_pares, values = c(
  cerca = "Municipio más cercano", d_cerca = "Dist. (km)",
  lejos = "Municipio vecino más lejano", d_lejos = "Dist. (km)",
  setNames(cortes$titulo, cortes$var)))
ft_pares <- add_footer_row(ft_pares,
                           values = c("Total de pares con estatus opuesto", colSums(op)),
                           colwidths = c(4, rep(1, nrow(cortes))))
ft_pares <- formato(ft_pares, centradas = c(2, 4:8),
                    anchos = c(1.55, 0.55, 1.55, 0.55, rep(0.7, 4)))

guardar(ft_pares,
        titulo  = "Tabla 6.5. Pares de municipios vecinos con estatus de tratamiento opuesto según puntos de corte del indicador d2 (distancia euclidiana)",
        nota    = paste("Pares de municipios de la muestra (37) que comparten frontera (MGN 2021) y cuyo estatus de tratamiento difiere en al menos un punto de corte.",
                        "Sí: en ese punto de corte el municipio más cercano es tratado y su vecino más lejano es control; No: ambos tienen el mismo estatus.",
                        nota_dist),
        archivo = "tabla_pares_vecinos_opuestos_d2_eucl.docx")

# ---- 5. Lista de pares de vecinos para Master.do (sección 8) ----
# Solo la vecindad, que no cambia; el estatus de tratamiento se asigna en Stata con los indicadores de 4.1.

titulo <- function(x) sapply(strsplit(tolower(x), " "), function(p) {
  cap <- !(p %in% c("de", "del", "la", "las", "los", "y")) | seq_along(p) == 1
  p[cap] <- paste0(toupper(substr(p[cap], 1, 1)), substr(p[cap], 2, nchar(p[cap])))
  p[grepl(".", p, fixed = TRUE)] <- toupper(p[grepl(".", p, fixed = TRUE)])
  paste(p, collapse = " ")
})

lista <- data.frame(
  mpio_cdpmp_1 = as.integer(tratados_eucl$mpio_cdpmp[pares$i]),
  mpio_cdpmp_2 = as.integer(tratados_eucl$mpio_cdpmp[pares$j]),
  mpio_cnmbr_1 = titulo(muestra$MPIO_CNMBR[pares$i]),
  mpio_cnmbr_2 = titulo(muestra$MPIO_CNMBR[pares$j])
)
etiquetas <- c("Código DANE del municipio 1 del par de vecinos", "Código DANE del municipio 2 del par de vecinos",
               "Nombre del municipio 1 del par de vecinos", "Nombre del municipio 2 del par de vecinos")
for (k in seq_along(lista)) attr(lista[[k]], "label") <- etiquetas[k]

write_dta(lista, file.path(raiz, "STATA/data/auxiliary/vecinos_muestra_37.dta"))
message("vecinos_muestra_37.dta: ", nrow(lista), " pares de vecinos")
