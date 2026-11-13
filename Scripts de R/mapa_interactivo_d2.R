# Mapa interactivo (HTML) de tratados, controles y PNIS más cercano para d2_eucl, con selector de punto de corte
# y resaltado de vecinos de la muestra (todos los pares, o solo los de estatus opuesto: Tabla 6.5 de tablas_vecinos.R)
# Base de referencia: mapas_tratados.R (juego d2_eucl). Genera un solo archivo HTML autocontenido:
# la plantilla (plantilla_mapa_interactivo.html) recibe Leaflet (copiado del paquete leaflet de R) y los datos.
library(sf)
library(haven)
library(readxl)
library(jsonlite)
library(geojsonsf)

raiz      <- "/Users/samuelquevedo/Mi unidad (samuelalgo16@hotmail.com)/Andes/Investigación/CESED/Luces_Nocturnas_Coca_y_Cannabis"
fuentes   <- file.path(raiz, "Análisis geográfico y mapas/Shapefiles y fuentes")
scripts   <- file.path(raiz, "Análisis geográfico y mapas/Scripts de R")
salida    <- file.path(raiz, "Análisis geográfico y mapas/objetos html/mapa_interactivo_d2_eucl.html")

shp_mpios <- file.path(fuentes, "MGN_2021_COLOMBIA/ADMINISTRATIVO/MGN_MPIO_POLITICO.shp")
xlsx_eucl <- file.path(raiz, "STATA/data/raw/distancias_euclidianas_ciudades_intermedias.xlsx")
xls_pnis  <- file.path(raiz, "STATA/data/raw/d_pr_pnis.xls")
dta_eucl  <- file.path(raiz, "STATA/data/auxiliary/tratados_distancia_euclidiana.dta")
plantilla <- file.path(scripts, "plantilla_mapa_interactivo.html")
leaflet   <- system.file("htmlwidgets/lib/leaflet", package = "leaflet")

# Tolerancia de simplificación de fronteras (metros, EPSG:9377)
tolerancia <- 100

norm <- function(x) toupper(iconv(x, to = "ASCII//TRANSLIT"))

# ---- 0. Insumos (mismos que mapas_tratados.R) ----

mpios <- st_transform(st_read(shp_mpios, quiet = TRUE), 9377)
mpios$llave <- norm(paste0(mpios$MPIO_CNMBR, " (", mpios$DPTO_CNMBR, ")"))

# Centroides municipales en MAGNA-SIRGAS Origen Nacional (EPSG:9377), con las geometrías originales
xy <- st_coordinates(st_transform(st_centroid(st_geometry(mpios)), 4326))
rownames(xy) <- mpios$MPIO_CDPMP

pnis <- read_excel(xls_pnis)
pnis_alta <- sprintf("%05d", as.integer(pnis$cod_dane_mpio[which(pnis$d_pr_pnis == "high prob PNIS")]))

dist_eucl <- read_excel(xlsx_eucl)
dist_eucl$MPIO_CDPMP <- sprintf("%05d", as.integer(dist_eucl$codmpio))
dist_eucl$pnis_d2 <- mpios$MPIO_CDPMP[match(norm(dist_eucl$pnis_mas_cercano_centroide_desde_centroide), mpios$llave)]
stopifnot(!anyNA(dist_eucl$pnis_d2), dist_eucl$pnis_d2 %in% pnis_alta)

tratados_eucl <- read_dta(dta_eucl)
tratados_eucl$MPIO_CDPMP <- sprintf("%05d", as.integer(tratados_eucl$mpio_cdpmp))
tratados_eucl$pnis_d2 <- dist_eucl$pnis_d2[match(tratados_eucl$MPIO_CDPMP, dist_eucl$MPIO_CDPMP)]
stopifnot(nrow(tratados_eucl) == 37, !anyNA(tratados_eucl$pnis_d2))

dist <- tratados_eucl$dist_centroide_pnis_centroide

# Puntos de corte (orden de las Tablas 6.2 a 6.5). Valores con la definición de Stata
# (summarize, detail y _pctile = quantile tipo 2); deben reproducir los indicadores de Master.do.
cortes <- data.frame(
  var    = c("mediana",       "mean",       "p60",             "p75"),
  titulo = c("Mediana (p50)", "Media",      "Percentil 60",    "Percentil 75"),
  texto  = c("a la mediana",  "a la media", "al percentil 60", "al percentil 75")
)
cortes$km <- c(quantile(dist, 0.5, type = 2, names = FALSE), mean(dist),
               quantile(dist, c(0.6, 0.75), type = 2, names = FALSE))
for (i in seq_len(nrow(cortes))) {
  stopifnot(all(tratados_eucl[[paste0("d2_eucl_", cortes$var[i])]] == (dist <= cortes$km[i])))
}
cortes$n_trat <- sapply(cortes$var, function(v) sum(tratados_eucl[[paste0("d2_eucl_", v)]] == 1))
cortes$n_ctrl <- 37 - cortes$n_trat
cortes$km     <- round(cortes$km, 1)

# ---- 1. Simplificación topológica de fronteras ----
# Se simplifica una sola vez la red de fronteras (cada borde compartido es una sola línea) y los
# polígonos coloreados se reconstruyen a partir de esa misma red: rellenos y fronteras coinciden.

bordes <- st_line_merge(st_union(st_boundary(st_geometry(mpios))))
bordes <- st_simplify(bordes, preserveTopology = TRUE, dTolerance = tolerancia)

caras <- st_sf(geometry = st_cast(st_collection_extract(st_polygonize(bordes), "POLYGON"), "POLYGON"))
caras$MPIO_CDPMP <- mpios$MPIO_CDPMP[st_nearest_feature(st_point_on_surface(caras), mpios)]

poligonos <- function(cods) {
  geom <- do.call(c, lapply(cods, function(cod) st_union(st_geometry(caras)[caras$MPIO_CDPMP == cod])))
  p <- st_sf(MPIO_CDPMP = cods, geometry = geom)
  # El área simplificada debe ser casi igual a la original
  original <- as.numeric(st_area(mpios[match(cods, mpios$MPIO_CDPMP), ]))
  stopifnot(abs(as.numeric(st_area(p)) / original - 1) < 0.03)
  p
}

# ---- 2. Capas del mapa ----

atributos <- function(p) {
  i <- match(p$MPIO_CDPMP, mpios$MPIO_CDPMP)
  p$cod  <- p$MPIO_CDPMP
  p$mpio <- mpios$MPIO_CNMBR[i]
  p$dpto <- mpios$DPTO_CNMBR[i]
  p$cx   <- round(xy[p$MPIO_CDPMP, "X"], 5)
  p$cy   <- round(xy[p$MPIO_CDPMP, "Y"], 5)
  p
}

muestra <- atributos(poligonos(tratados_eucl$MPIO_CDPMP))
muestra$dist <- round(dist, 1)
muestra$pnis <- tratados_eucl$pnis_d2
for (v in cortes$var) muestra[[paste0("est_", v)]] <- as.integer(tratados_eucl[[paste0("d2_eucl_", v)]])

pnis_usados <- atributos(poligonos(sort(unique(tratados_eucl$pnis_d2))))

# Pares de vecinos dentro de la muestra (37), como en tablas_vecinos.R: geometrías originales,
# borde o vértice compartido. El estatus opuesto por punto de corte se calcula en el HTML.
vecinos <- st_touches(mpios[match(tratados_eucl$MPIO_CDPMP, mpios$MPIO_CDPMP), ])
pares <- do.call(rbind, lapply(seq_along(vecinos), function(i) {
  j <- vecinos[[i]][vecinos[[i]] > i]
  if (length(j)) cbind(tratados_eucl$MPIO_CDPMP[i], tratados_eucl$MPIO_CDPMP[j])
}))

message("Muestra: ", nrow(muestra), " | PNIS más cercanos: ", nrow(pnis_usados),
        " | Pares de vecinos: ", nrow(pares),
        " | Vértices de fronteras: ", nrow(st_coordinates(bordes)))

a_geojson <- function(x) sf_geojson(st_transform(x[, setdiff(names(x), "MPIO_CDPMP")], 4326), digits = 5)

datos <- paste0(
  '{"fondo":',    a_geojson(st_sf(geometry = bordes)),
  ',"muestra":',  a_geojson(muestra),
  ',"pnis":',     a_geojson(pnis_usados),
  ',"cortes":',   toJSON(cortes, dataframe = "rows", digits = NA),
  ',"vecinos":',  toJSON(unname(pares)),
  '}'
)

# ---- 3. HTML autocontenido ----

leer <- function(f) paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")

# Reemplazo literal (sin expresiones regulares: el código de Leaflet contiene barras invertidas)
insertar <- function(txt, marca, contenido) {
  i <- regexpr(marca, txt, fixed = TRUE)
  stopifnot(i > 0)
  paste0(substr(txt, 1, i - 1), contenido, substr(txt, i + nchar(marca), nchar(txt)))
}

html <- leer(plantilla)
html <- insertar(html, "/*__LEAFLET_CSS__*/", leer(file.path(leaflet, "leaflet.css")))
html <- insertar(html, "/*__LEAFLET_JS__*/",  leer(file.path(leaflet, "leaflet.js")))
html <- insertar(html, "/*__TOLERANCIA__*/",  tolerancia)
html <- insertar(html, "/*__DATOS__*/",       datos)

dir.create(dirname(salida), showWarnings = FALSE)
writeLines(enc2utf8(html), salida, useBytes = TRUE)
message(basename(salida), ": ", round(file.size(salida) / 1e6, 1), " MB")
