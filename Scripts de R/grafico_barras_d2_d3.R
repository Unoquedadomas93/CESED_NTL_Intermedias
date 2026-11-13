# Gráfico de barras interactivo (HTML) de la distancia al PNIS más cercano para d2_eucl y d3_eucl,
# con selector de indicador y de punto de corte. Mismos insumos y colores que mapa_interactivo_d2.R.
# La plantilla (plantilla_grafico_barras.html) recibe los datos; el archivo resultante es autocontenido.
library(sf)
library(haven)
library(readxl)
library(jsonlite)

raiz      <- "/Users/samuelquevedo/Mi unidad (samuelalgo16@hotmail.com)/Andes/Investigación/CESED/Luces_Nocturnas_Coca_y_Cannabis"
fuentes   <- file.path(raiz, "Análisis geográfico y mapas/Shapefiles y fuentes")
scripts   <- file.path(raiz, "Análisis geográfico y mapas/Scripts de R")
salida    <- file.path(raiz, "Análisis geográfico y mapas/objetos html/grafico_barras_d2_d3_eucl.html")

shp_mpios <- file.path(fuentes, "MGN_2021_COLOMBIA/ADMINISTRATIVO/MGN_MPIO_POLITICO.shp")
xlsx_eucl <- file.path(raiz, "STATA/data/raw/distancias_euclidianas_ciudades_intermedias.xlsx")
xls_pnis  <- file.path(raiz, "STATA/data/raw/d_pr_pnis.xls")
dta_eucl  <- file.path(raiz, "STATA/data/auxiliary/tratados_distancia_euclidiana.dta")
plantilla <- file.path(scripts, "plantilla_grafico_barras.html")

norm <- function(x) toupper(iconv(x, to = "ASCII//TRANSLIT"))

# ---- 0. Insumos (mismos que mapas_tratados.R; del MGN solo se usan nombres) ----

mpios <- st_drop_geometry(read_sf(shp_mpios))
mpios$llave <- norm(paste0(mpios$MPIO_CNMBR, " (", mpios$DPTO_CNMBR, ")"))

pnis <- read_excel(xls_pnis)
pnis_alta <- sprintf("%05d", as.integer(pnis$cod_dane_mpio[which(pnis$d_pr_pnis == "high prob PNIS")]))

# d2: centroide municipal → centroide PNIS; d3: cabecera municipal → cabecera PNIS
dist_eucl <- read_excel(xlsx_eucl)
dist_eucl$MPIO_CDPMP <- sprintf("%05d", as.integer(dist_eucl$codmpio))
dist_eucl$pnis_d2 <- mpios$MPIO_CDPMP[match(norm(dist_eucl$pnis_mas_cercano_centroide_desde_centroide), mpios$llave)]
dist_eucl$pnis_d3 <- mpios$MPIO_CDPMP[match(norm(dist_eucl$pnis_mas_cercano_cabecera_desde_cabecera), mpios$llave)]
stopifnot(!anyNA(dist_eucl$pnis_d2), !anyNA(dist_eucl$pnis_d3),
          dist_eucl$pnis_d2 %in% pnis_alta, dist_eucl$pnis_d3 %in% pnis_alta)

tratados_eucl <- read_dta(dta_eucl)
tratados_eucl$MPIO_CDPMP <- sprintf("%05d", as.integer(tratados_eucl$mpio_cdpmp))
i_eucl <- match(tratados_eucl$MPIO_CDPMP, dist_eucl$MPIO_CDPMP)
stopifnot(nrow(tratados_eucl) == 37, !anyNA(i_eucl))

distancias <- list(d2 = tratados_eucl$dist_centroide_pnis_centroide,
                   d3 = tratados_eucl$dist_cabecera_pnis_cabecera)

# ---- 1. Puntos de corte por indicador (orden de las Tablas 6.2 a 6.5) ----
# Valores con la definición de Stata (summarize, detail y _pctile = quantile tipo 2);
# deben reproducir los indicadores de Master.do (sección 4.1).

cortes_base <- data.frame(
  var    = c("mediana",       "mean",       "p60",             "p75"),
  titulo = c("Mediana (p50)", "Media",      "Percentil 60",    "Percentil 75"),
  texto  = c("a la mediana",  "a la media", "al percentil 60", "al percentil 75")
)

cortes_de <- function(d) {
  dist <- distancias[[d]]
  cortes <- cortes_base
  cortes$km <- c(quantile(dist, 0.5, type = 2, names = FALSE), mean(dist),
                 quantile(dist, c(0.6, 0.75), type = 2, names = FALSE))
  for (i in seq_len(nrow(cortes))) {
    stopifnot(all(tratados_eucl[[paste0(d, "_eucl_", cortes$var[i])]] == (dist <= cortes$km[i])))
  }
  cortes$n_trat <- sapply(cortes$var, function(v) sum(tratados_eucl[[paste0(d, "_eucl_", v)]] == 1))
  cortes$n_ctrl <- 37 - cortes$n_trat
  cortes$km     <- round(cortes$km, 1)
  cortes
}

indicadores <- list(
  list(id = "d2", nombre = "d2_eucl", origen = "Centroide → centroide",
       subtitulo = paste("Distancia euclidiana (línea recta) del centroide municipal al centroide",
                         "del municipio PNIS de alta probabilidad más cercano, respecto "),
       cortes = cortes_de("d2")),
  list(id = "d3", nombre = "d3_eucl", origen = "Cabecera → cabecera",
       subtitulo = paste("Distancia euclidiana (línea recta) de la cabecera municipal a la cabecera",
                         "del municipio PNIS de alta probabilidad más cercano, respecto "),
       cortes = cortes_de("d3"))
)

# ---- 2. Datos de los 37 municipios y de sus PNIS más cercanos ----

nombres <- function(cods) {
  i <- match(cods, mpios$MPIO_CDPMP)
  stopifnot(!anyNA(i))
  data.frame(cod = cods, mpio = mpios$MPIO_CNMBR[i], dpto = mpios$DPTO_CNMBR[i])
}

muestra <- nombres(tratados_eucl$MPIO_CDPMP)
for (d in c("d2", "d3")) {
  muestra[[paste0("dist_", d)]] <- round(distancias[[d]], 1)
  muestra[[paste0("pnis_", d)]] <- dist_eucl[[paste0("pnis_", d)]][i_eucl]
  for (v in cortes_base$var) {
    muestra[[paste0("est_", d, "_", v)]] <- as.integer(tratados_eucl[[paste0(d, "_eucl_", v)]])
  }
}

pnis_usados <- nombres(sort(unique(c(muestra$pnis_d2, muestra$pnis_d3))))

message("Muestra: ", nrow(muestra), " | PNIS más cercanos (d2 o d3): ", nrow(pnis_usados))

datos <- toJSON(list(muestra = muestra, pnis = pnis_usados, indicadores = indicadores),
                dataframe = "rows", auto_unbox = TRUE, digits = NA)

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
