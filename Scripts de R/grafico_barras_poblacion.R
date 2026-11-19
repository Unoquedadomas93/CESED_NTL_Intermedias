# Gráfico de barras interactivo (HTML): "Comparativo de muestras por población". Población total municipal
# en 2005 o 2013 (panel CEDE) de las mismas muestras de mapa_interactivo_categorias.R:
#   - 37 ciudades intermedias (>150k en 2013), muestra original de Master.do (sección 4.1)
#   - 31 capitales departamentales continentales (código xx001, sin Agua de Dios; Bogotá incluida)
#   - 2 intermedias excluidas por ser PNIS alta probabilidad (Tumaco y Uribia)
#   - 109 municipios PNIS alta probabilidad (d_pr_pnis.xls, sección 3)
#   - 170 municipios PDET (ART, datos.gov.co idrk-ba8y)
# Colombia continental: sin el Archipiélago de San Andrés, Providencia y Santa Catalina (como el mapa).
# San José de Uré (23682, PDET) fue creado en 2007: sin población en 2005 (169 de 170 PDET ese año).
# Mismo estilo que grafico_barras_d2_d3.R: la plantilla (plantilla_grafico_poblacion.html) recibe los datos
# y el archivo resultante es autocontenido.
library(sf)
library(haven)
library(readxl)
library(jsonlite)

raiz      <- "/Users/samuelquevedo/Mi unidad (samuelalgo16@hotmail.com)/Andes/Investigación/CESED/Luces_Nocturnas_Coca_y_Cannabis"
fuentes   <- file.path(raiz, "Análisis geográfico y mapas/Shapefiles y fuentes")
scripts   <- file.path(raiz, "Análisis geográfico y mapas/Scripts de R")
salida    <- file.path(raiz, "Análisis geográfico y mapas/objetos html/grafico_barras_poblacion.html")

shp_mpios <- file.path(fuentes, "MGN_2021_COLOMBIA/ADMINISTRATIVO/MGN_MPIO_POLITICO.shp")
xlsx_eucl <- file.path(raiz, "STATA/data/raw/distancias_euclidianas_ciudades_intermedias.xlsx")
xls_pnis  <- file.path(raiz, "STATA/data/raw/d_pr_pnis.xls")
csv_pdet  <- file.path(raiz, "STATA/data/raw/municipios_pdet.csv")
dta_eucl  <- file.path(raiz, "STATA/data/auxiliary/tratados_distancia_euclidiana.dta")
dta_cede  <- file.path(raiz, "STATA/data/raw/panel_cede.dta")
plantilla <- file.path(scripts, "plantilla_grafico_poblacion.html")

cod5 <- function(x) sprintf("%05d", as.integer(x))

# ---- 0. Insumos (mismos que mapa_interactivo_categorias.R; del MGN solo se usan nombres) ----

mpios <- st_drop_geometry(read_sf(shp_mpios))
stopifnot(nrow(mpios) == 1121, !anyDuplicated(mpios$MPIO_CDPMP))

# Colombia continental: sin el Archipiélago de San Andrés, Providencia y Santa Catalina (dpto. 88)
stopifnot(setequal(mpios$MPIO_CDPMP[mpios$DPTO_CCDGO == "88"], c("88001", "88564")))
mpios <- mpios[mpios$DPTO_CCDGO != "88", ]
stopifnot(nrow(mpios) == 1119)

muestra_37 <- cod5(read_dta(dta_eucl)$mpio_cdpmp)
universo   <- cod5(read_excel(xlsx_eucl)$codmpio)
excluidas  <- setdiff(universo, muestra_37)
stopifnot(length(muestra_37) == 37, length(universo) == 39, setequal(excluidas, c("44847", "52835")))

pnis <- read_excel(xls_pnis)
pnis$cod <- cod5(pnis$cod_dane_mpio)
pnis_alta <- pnis$cod[which(pnis$d_pr_pnis == "high prob PNIS")]
pnis_baja <- pnis$cod[which(pnis$d_pr_pnis == "low prob PNIS")]
stopifnot(length(pnis_alta) == 109, all(excluidas %in% pnis_alta))

pdet <- read.csv(csv_pdet, encoding = "UTF-8")
pdet$cod <- cod5(pdet$CodigoMunicipio)
stopifnot(nrow(pdet) == 170, !anyDuplicated(pdet$cod))

# Capitales departamentales continentales: código xx001, excepto Agua de Dios (25001), pues la capital
# de Cundinamarca es Bogotá (11001); como en Master.do, sección 12.1
capitales <- mpios$MPIO_CDPMP[substr(mpios$MPIO_CDPMP, 3, 5) == "001" & mpios$MPIO_CDPMP != "25001"]
stopifnot(length(capitales) == 31)

cods <- sort(unique(c(muestra_37, excluidas, capitales, pnis_alta, pdet$cod)))
stopifnot(all(cods %in% mpios$MPIO_CDPMP))

# ---- 1. Población total 2005 y 2013 (panel CEDE) ----

cede <- read_dta(dta_cede, col_select = c("codmpio", "ano", "pobl_tot"))
cede$cod <- cod5(cede$codmpio)
poblacion <- function(anio) {
  x <- cede[cede$ano == anio, ]
  v <- as.integer(x$pobl_tot[match(cods, x$cod)])
  v[!is.na(v) & v <= 0] <- NA   # municipio aún no creado: población 0 en el panel
  v
}
p05 <- poblacion(2005)
p13 <- poblacion(2013)
stopifnot(!anyNA(p13), identical(cods[is.na(p05)], "23682"))

# ---- 2. Datos por municipio ----

i <- match(cods, mpios$MPIO_CDPMP)
datos_mpios <- data.frame(
  cod  = cods,
  mpio = mpios$MPIO_CNMBR[i],
  dpto = mpios$DPTO_CNMBR[i],
  p05  = p05,
  p13  = p13,
  m37  = as.integer(cods %in% muestra_37),
  cap  = as.integer(cods %in% capitales),
  exc  = as.integer(cods %in% excluidas),
  alta = as.integer(cods %in% pnis_alta),
  pdet = as.integer(cods %in% pdet$cod),
  pnis = ifelse(cods %in% pnis_alta, "alta", ifelse(cods %in% pnis_baja, "baja", NA)),
  sub  = pdet$NombreSubregion[match(cods, pdet$cod)]
)

conteos <- colSums(datos_mpios[, c("m37", "cap", "exc", "alta", "pdet")])
stopifnot(identical(unname(conteos), c(37, 31, 2, 109, 170)))
# Las 37 superan 150.000 habitantes en 2013 (etiqueta del HTML)
stopifnot(all(datos_mpios$p13[datos_mpios$m37 == 1] > 150000))

message("Municipios: ", nrow(datos_mpios), " | ", paste(names(conteos), conteos, sep = " = ", collapse = ", "),
        " | Sin población 2005: ", sum(is.na(p05)))

datos <- toJSON(list(mpios = datos_mpios, total_pdet = nrow(pdet)),
                dataframe = "rows", auto_unbox = TRUE, digits = NA, na = "null")

# ---- 3. HTML autocontenido ----

leer <- function(f) paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")

# Reemplazo literal (sin expresiones regulares)
insertar <- function(txt, marca, contenido) {
  i <- regexpr(marca, txt, fixed = TRUE)
  stopifnot(i > 0)
  paste0(substr(txt, 1, i - 1), contenido, substr(txt, i + nchar(marca), nchar(txt)))
}

html <- insertar(leer(plantilla), "/*__DATOS__*/", datos)

dir.create(dirname(salida), showWarnings = FALSE)
writeLines(enc2utf8(html), salida, useBytes = TRUE)
message(basename(salida), ": ", round(file.size(salida) / 1e3), " KB")
