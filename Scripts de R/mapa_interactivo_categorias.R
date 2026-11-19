# Mapa interactivo (HTML) de las categorías municipales de Master.do (sección 12):
#   - Muestra original de 37 ciudades intermedias (sección 4)
#   - 2 intermedias excluidas por ser PNIS alta probabilidad (Tumaco y Uribia)
#   - 170 municipios PDET (ART, datos.gov.co idrk-ba8y)
#   - PNIS alta (109) y baja (983) probabilidad (d_pr_pnis.xls, sección 3)
# Las categorías se superponen: el HTML las muestra con interruptores y, cuando un municipio pertenece
# a varias categorías visibles, con franjas diagonales de los colores de cada una.
# Base: mapa_interactivo_d2.R (misma simplificación topológica y misma inserción de Leaflet y datos en
# una plantilla). Genera un solo archivo HTML autocontenido a partir de plantilla_mapa_categorias.html.
library(sf)
library(haven)
library(readxl)
library(jsonlite)
library(geojsonsf)

raiz      <- "/Users/samuelquevedo/Mi unidad (samuelalgo16@hotmail.com)/Andes/Investigación/CESED/Luces_Nocturnas_Coca_y_Cannabis"
fuentes   <- file.path(raiz, "Análisis geográfico y mapas/Shapefiles y fuentes")
scripts   <- file.path(raiz, "Análisis geográfico y mapas/Scripts de R")
salida    <- file.path(raiz, "Análisis geográfico y mapas/objetos html/mapa_interactivo_categorias.html")

shp_mpios <- file.path(fuentes, "MGN_2021_COLOMBIA/ADMINISTRATIVO/MGN_MPIO_POLITICO.shp")
shp_dptos <- file.path(fuentes, "MGN_2021_COLOMBIA/ADMINISTRATIVO/MGN_DPTO_POLITICO.shp")
xlsx_eucl <- file.path(raiz, "STATA/data/raw/distancias_euclidianas_ciudades_intermedias.xlsx")
xls_pnis  <- file.path(raiz, "STATA/data/raw/d_pr_pnis.xls")
csv_pdet  <- file.path(raiz, "STATA/data/raw/municipios_pdet.csv")
dta_eucl  <- file.path(raiz, "STATA/data/auxiliary/tratados_distancia_euclidiana.dta")
plantilla <- file.path(scripts, "plantilla_mapa_categorias.html")
leaflet   <- system.file("htmlwidgets/lib/leaflet", package = "leaflet")

# Tolerancia de simplificación de fronteras (metros, EPSG:9377): municipios y departamentos
tolerancia      <- 100
tolerancia_dpto <- 300

cod5 <- function(x) sprintf("%05d", as.integer(x))

# ---- 0. Insumos ----

mpios <- st_transform(st_read(shp_mpios, quiet = TRUE), 9377)
dptos <- st_transform(st_read(shp_dptos, quiet = TRUE), 9377)
stopifnot(nrow(mpios) == 1121, !anyDuplicated(mpios$MPIO_CDPMP))

# Muestra original (37) y universo de 39 intermedias: las 2 restantes son las excluidas en 4.1
muestra_37 <- cod5(read_dta(dta_eucl)$mpio_cdpmp)
universo   <- cod5(read_excel(xlsx_eucl)$codmpio)
excluidas  <- setdiff(universo, muestra_37)
stopifnot(length(muestra_37) == 37, length(universo) == 39, setequal(excluidas, c("44847", "52835")))

pnis <- read_excel(xls_pnis)
pnis$cod <- cod5(pnis$cod_dane_mpio)
pnis_alta <- pnis$cod[which(pnis$d_pr_pnis == "high prob PNIS")]
pnis_baja <- pnis$cod[which(pnis$d_pr_pnis == "low prob PNIS")]
stopifnot(length(pnis_alta) == 109, length(pnis_baja) == 983, all(excluidas %in% pnis_alta))

pdet <- read.csv(csv_pdet, encoding = "UTF-8")
pdet$cod <- cod5(pdet$CodigoMunicipio)
stopifnot(nrow(pdet) == 170, !anyDuplicated(pdet$cod), all(pdet$cod %in% mpios$MPIO_CDPMP))

# ---- 1. Simplificación topológica de fronteras (como en mapa_interactivo_d2.R) ----

bordes <- st_line_merge(st_union(st_boundary(st_geometry(mpios))))
bordes <- st_simplify(bordes, preserveTopology = TRUE, dTolerance = tolerancia)

caras <- st_sf(geometry = st_cast(st_collection_extract(st_polygonize(bordes), "POLYGON"), "POLYGON"))
caras$MPIO_CDPMP <- mpios$MPIO_CDPMP[st_nearest_feature(st_point_on_surface(caras), mpios)]

# Municipios con categoría: polígono reconstruido a partir de las caras simplificadas
cods <- sort(unique(c(muestra_37, excluidas, pdet$cod, pnis_alta, pnis_baja)))
stopifnot(all(cods %in% caras$MPIO_CDPMP))
geom <- do.call(c, lapply(split(st_geometry(caras), caras$MPIO_CDPMP)[cods], st_union))
p <- st_sf(MPIO_CDPMP = cods, geometry = geom)

# El área simplificada debe ser casi igual a la original (holgura absoluta para municipios pequeños)
i <- match(cods, mpios$MPIO_CDPMP)
original <- as.numeric(st_area(mpios[i, ]))
dif <- abs(as.numeric(st_area(p)) - original)
stopifnot(all(dif / original < 0.03 | dif < 2e6))

# Fronteras departamentales (solo trazo, sin interacción)
bordes_dpto <- st_simplify(st_line_merge(st_union(st_boundary(st_geometry(dptos)))),
                           preserveTopology = TRUE, dTolerance = tolerancia_dpto)

# ---- 2. Atributos de cada municipio ----

p$cod  <- p$MPIO_CDPMP
p$mpio <- mpios$MPIO_CNMBR[i]
p$dpto <- mpios$DPTO_CNMBR[i]
# Categorías como 0/1 (nombres cortos para aligerar el archivo)
p$m37  <- as.integer(cods %in% muestra_37)
p$exc  <- as.integer(cods %in% excluidas)
p$pdet <- as.integer(cods %in% pdet$cod)
p$alta <- as.integer(cods %in% pnis_alta)
p$baja <- as.integer(cods %in% pnis_baja)
p$sub  <- pdet$NombreSubregion[match(cods, pdet$cod)]

conteos <- c(m37 = sum(p$m37), exc = sum(p$exc), pdet = sum(p$pdet), alta = sum(p$alta), baja = sum(p$baja))
stopifnot(identical(unname(conteos), c(37L, 2L, 170L, 109L, 983L)))

message("Municipios con categoría: ", nrow(p), " | ",
        paste(names(conteos), conteos, sep = " = ", collapse = ", "),
        " | Vértices de fronteras: ", nrow(st_coordinates(bordes)))

a_geojson <- function(x) sf_geojson(st_transform(x[, setdiff(names(x), "MPIO_CDPMP")], 4326), digits = 4)

datos <- paste0(
  '{"fondo":',  a_geojson(st_sf(geometry = bordes)),
  ',"dptos":',  a_geojson(st_sf(geometry = bordes_dpto)),
  ',"mpios":',  a_geojson(p),
  ',"total":',  nrow(mpios),
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
