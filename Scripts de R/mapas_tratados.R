# Mapas de municipios tratados y de control (ciudades intermedias)
# Base de referencia: mapa_municipios_colombia.R (mapas 3 y 4)
# Tratamientos definidos en Master.do (sección 4); se leen las bases que guarda Master.do.
library(sf)
library(ggplot2)
library(haven)
library(readxl)
library(ggrepel)

raiz    <- "/Users/samuelquevedo/Mi unidad (samuelalgo16@hotmail.com)/Andes/Investigación/CESED/Luces_Nocturnas_Coca_y_Cannabis"
fuentes <- file.path(raiz, "Análisis geográfico y mapas/Shapefiles y fuentes")
figuras <- file.path(raiz, "Análisis geográfico y mapas/export/figures")

shp_mpios    <- file.path(fuentes, "MGN_2021_COLOMBIA/ADMINISTRATIVO/MGN_MPIO_POLITICO.shp")
shp_clase    <- file.path(fuentes, "MGN_2021_COLOMBIA/MGN/MGN_CLASE.shp")
shp_enclaves <- file.path(fuentes, "ENCLAVES_COCA_SIMCI_20260801/ENCLAVES_COCA_SIMCI_20260801.shp")
xlsx_eucl    <- file.path(raiz, "STATA/data/raw/distancias_euclidianas_ciudades_intermedias.xlsx")
xls_pnis     <- file.path(raiz, "STATA/data/raw/d_pr_pnis.xls")
dta_eucl     <- file.path(raiz, "STATA/data/auxiliary/tratados_distancia_euclidiana.dta")

norm <- function(x) toupper(iconv(x, to = "ASCII//TRANSLIT"))

# ---- 0. Insumos comunes ----

mpios <- st_read(shp_mpios, quiet = TRUE)
mpios$llave <- norm(paste0(mpios$MPIO_CNMBR, " (", mpios$DPTO_CNMBR, ")"))

# Centroides municipales calculados en MAGNA-SIRGAS Origen Nacional (EPSG:9377) y devueltos a lat/lon
centroides <- st_transform(st_centroid(st_transform(st_geometry(mpios), 9377)), st_crs(mpios))
xy <- st_coordinates(centroides)
rownames(xy) <- mpios$MPIO_CDPMP

# Cabeceras municipales: capa Clase del MGN (CLAS_CCDGO = 1), centroide calculado igual que arriba
cabeceras <- st_read(shp_clase, quiet = TRUE)
cabeceras <- cabeceras[cabeceras$CLAS_CCDGO == "1", ]
stopifnot(!anyDuplicated(cabeceras$MPIO_CDPMP))
xy_cab <- st_coordinates(st_transform(st_centroid(st_transform(st_geometry(cabeceras), 9377)),
                                      st_crs(mpios)))
rownames(xy_cab) <- cabeceras$MPIO_CDPMP

# Enclaves: misma construcción que la metodología de distancias (metodologia_distancias.docx, 2.2).
# Se disuelven las celdas SIMCI que se tocan (65 polígonos) y los 15 de más de 100 km² son los enclaves,
# numerados de mayor a menor área ("Enclave 1" = el más grande).
celdas   <- st_transform(st_read(shp_enclaves, quiet = TRUE), 9377)
poligonos <- st_cast(st_union(celdas), "POLYGON")
area_km2 <- as.numeric(st_area(poligonos)) / 1e6
orden    <- order(-area_km2)[area_km2[order(-area_km2)] > 100]
stopifnot(length(orden) == 15)
enclaves <- st_sf(enclave = seq_along(orden), geometry = poligonos[orden])

# Departamento dominante de cada enclave (para validar contra los nombres del Excel)
cel_enc <- st_join(st_centroid(celdas[, "DPTO_CNMBR"]), enclaves, join = st_within)
dpto_dom <- tapply(cel_enc$DPTO_CNMBR, cel_enc$enclave, function(x) names(which.max(table(x))))
enclaves$dpto <- dpto_dom[as.character(enclaves$enclave)]

xy_enc <- st_coordinates(st_transform(st_centroid(st_geometry(enclaves)), st_crs(mpios)))
rownames(xy_enc) <- enclaves$enclave
enclaves <- st_transform(enclaves, st_crs(mpios))
enclaves$id <- as.character(enclaves$enclave)

# Municipios PNIS de alta probabilidad (misma fuente que Master.do, sección 3)
pnis <- read_excel(xls_pnis)
pnis_alta <- sprintf("%05d", as.integer(pnis$cod_dane_mpio[which(pnis$d_pr_pnis == "high prob PNIS")]))

# Distancias euclidianas (Excel fuente): enclave y municipio PNIS más cercano a cada ciudad
dist_eucl <- read_excel(xlsx_eucl)
dist_eucl$MPIO_CDPMP <- sprintf("%05d", as.integer(dist_eucl$codmpio))
dist_eucl$enclave <- sub("^Enclave ([0-9]+) .*$", "\\1", dist_eucl$enclave_mas_cercano_desde_centroide)
dist_eucl$enclave_dpto <- sub("^Enclave [0-9]+ \\((.*)\\)$", "\\1",
                              dist_eucl$enclave_mas_cercano_desde_centroide)
# d2: centroide municipal → centroide PNIS; d3: cabecera municipal → cabecera PNIS
dist_eucl$pnis_d2 <- mpios$MPIO_CDPMP[match(norm(dist_eucl$pnis_mas_cercano_centroide_desde_centroide), mpios$llave)]
dist_eucl$pnis_d3 <- mpios$MPIO_CDPMP[match(norm(dist_eucl$pnis_mas_cercano_cabecera_desde_cabecera), mpios$llave)]

# Validaciones contra el Excel: enclave reconstruido (número, departamento y distancia),
# municipios PNIS emparejados y en la lista de alta probabilidad, y distancias d2 y d3
stopifnot(!anyNA(as.integer(dist_eucl$enclave)),
          norm(dist_eucl$enclave_dpto) == norm(enclaves$dpto[as.integer(dist_eucl$enclave)]),
          !anyNA(dist_eucl$pnis_d2), !anyNA(dist_eucl$pnis_d3),
          dist_eucl$pnis_d2 %in% pnis_alta, dist_eucl$pnis_d3 %in% pnis_alta)

dist_km <- function(xy_a, xy_b) {
  a <- st_transform(st_as_sf(as.data.frame(xy_a), coords = c("X", "Y"), crs = st_crs(mpios)), 9377)
  b <- st_transform(st_as_sf(as.data.frame(xy_b), coords = c("X", "Y"), crs = st_crs(mpios)), 9377)
  as.numeric(st_distance(a, b, by_element = TRUE)) / 1000
}
stopifnot(
  max(abs(dist_km(xy[dist_eucl$MPIO_CDPMP, ], xy_enc[dist_eucl$enclave, ]) -
          dist_eucl$dist_centroide_enclave)) < 0.01,
  max(abs(dist_km(xy[dist_eucl$MPIO_CDPMP, ], xy[dist_eucl$pnis_d2, ]) -
          dist_eucl$dist_centroide_pnis_centroide)) < 0.01,
  # Cabeceras: MGN 2021 vs. MGN 2020 usado en el cálculo original (diferencias < 0,25 km)
  max(abs(dist_km(xy_cab[dist_eucl$MPIO_CDPMP, ], xy_cab[dist_eucl$pnis_d3, ]) -
          dist_eucl$dist_cabecera_pnis_cabecera)) < 0.5
)

# Tratamientos euclidianos (37 municipios; Master.do excluye los PNIS de alta probabilidad)
tratados_eucl <- read_dta(dta_eucl)
tratados_eucl$MPIO_CDPMP <- sprintf("%05d", as.integer(tratados_eucl$mpio_cdpmp))

# Colores: relleno claro y banderas (número en color oscuro del grupo sobre rectángulo blanco)
colores_relleno  <- c("Tratados" = "#9ECAE1", "Controles" = "#FCAE91", "Destino" = "#A1D99B")
colores_banderas <- c("Tratados" = "#08306B", "Controles" = "#67000D", "Destino" = "#00441B")

numerar <- function(cods, grupo, xy_m) {
  d <- data.frame(id = cods, grupo = grupo, x = xy_m[cods, "X"], y = xy_m[cods, "Y"])
  d <- d[order(-d$y), ]
  d$num <- seq_len(nrow(d))
  d
}

# ---- 1. Función de mapa: tratados, controles y destino más cercano ----
# var:       variable de tratamiento (Master.do, sección 4.1)
# corte:     punto de corte con preposición, p. ej. "a la media" o "al percentil 60" (subtítulo)
# out:       archivo de salida
# destino:   columna de dist_eucl con el id del destino más cercano (enclave o código PNIS)
# xy_orig:   coordenadas de origen de cada ciudad (centroide municipal o de la cabecera)
# xy_dest:   coordenadas de los destinos
# poly_dest: polígonos de los destinos (columna id)
# num_dest:  "propio" (número del enclave) o "norte_sur" (numeración de norte a sur)
# txt:       textos del mapa (leyenda, título, subtítulo y nota)

mapa_tratados <- function(var, corte, out, destino, xy_orig, xy_dest, poly_dest, num_dest, txt) {

  pares <- merge(tratados_eucl[, c("MPIO_CDPMP", var)],
                 dist_eucl[, c("MPIO_CDPMP", destino)], by = "MPIO_CDPMP")
  names(pares)[names(pares) == destino] <- "destino"
  pares$grupo <- factor(pares[[var]], levels = c(1, 0), labels = c("Tratados", "Controles"))
  stopifnot(nrow(pares) == 37)
  message(basename(out), " | Tratados: ", sum(pares$grupo == "Tratados"),
          " | Controles: ", sum(pares$grupo == "Controles"))

  mpios_trt  <- merge(mpios, pares[, c("MPIO_CDPMP", "grupo")], by = "MPIO_CDPMP")
  dest_usados <- poly_dest[poly_dest$id %in% unique(pares$destino), ]
  message("  Destinos más cercanos: ", nrow(dest_usados))

  # Relleno conjunto: tratados, controles y destinos en una sola leyenda
  relleno <- rbind(
    mpios_trt[, "grupo"],
    st_sf(grupo = "Destino", geometry = st_geometry(dest_usados))
  )
  relleno$grupo <- factor(relleno$grupo, levels = c("Tratados", "Controles", "Destino"))

  # Líneas rectas origen → destino más cercano (la línea recta es la distancia usada en d*_eucl)
  lineas_ctrl <- with(pares[pares$grupo == "Controles", ], st_sf(
    geometry = st_sfc(lapply(seq_along(MPIO_CDPMP), function(i)
      st_linestring(rbind(xy_orig[MPIO_CDPMP[i], ], xy_dest[destino[i], ]))),
      crs = st_crs(mpios)),
    tipo = factor("Enlace control", levels = c("Enlace tratado", "Enlace control"))
  ))

  # Flechas de tratados (geom_sf no dibuja flechas, se usan segmentos con coordenadas lon/lat)
  pares_trt <- pares[pares$grupo == "Tratados", ]
  flechas <- data.frame(
    x    = xy_orig[pares_trt$MPIO_CDPMP, "X"], y    = xy_orig[pares_trt$MPIO_CDPMP, "Y"],
    xend = xy_dest[pares_trt$destino, "X"],    yend = xy_dest[pares_trt$destino, "Y"],
    tipo = factor("Enlace tratado", levels = c("Enlace tratado", "Enlace control"))
  )

  # Separación entre la punta de la flecha y el destino (en grados, ~0.1° ≈ 11 km)
  separacion <- 0.1
  largo <- sqrt((flechas$xend - flechas$x)^2 + (flechas$yend - flechas$y)^2)
  flechas$xend <- flechas$xend - separacion * (flechas$xend - flechas$x) / largo
  flechas$yend <- flechas$yend - separacion * (flechas$yend - flechas$y) / largo

  puntos <- rbind(
    data.frame(x = xy_orig[pares$MPIO_CDPMP, "X"], y = xy_orig[pares$MPIO_CDPMP, "Y"]),
    data.frame(x = xy_dest[dest_usados$id, "X"],   y = xy_dest[dest_usados$id, "Y"])
  )

  # Banderas: tratados y controles numerados de norte a sur; destinos según num_dest
  if (num_dest == "propio") {
    banderas_dest <- data.frame(id = dest_usados$id, grupo = "Destino",
                                x = xy_dest[dest_usados$id, "X"], y = xy_dest[dest_usados$id, "Y"],
                                num = as.integer(dest_usados$id))
  } else {
    banderas_dest <- numerar(dest_usados$id, "Destino", xy_dest)
  }
  banderas <- rbind(
    numerar(pares$MPIO_CDPMP[pares$grupo == "Tratados"],  "Tratados",  xy_orig),
    numerar(pares$MPIO_CDPMP[pares$grupo == "Controles"], "Controles", xy_orig),
    banderas_dest
  )
  banderas$grupo <- factor(banderas$grupo, levels = c("Tratados", "Controles", "Destino"))

  etiquetas_lineas <- c("Enlace tratado" = paste0("Línea recta tratado → ", txt$corto, " más cercano"),
                        "Enlace control" = paste0("Línea recta control → ", txt$corto, " más cercano"))

  p <- ggplot() +
    geom_sf(data = relleno, aes(fill = grupo), color = NA) +
    geom_sf(data = mpios, fill = NA, color = "black", linewidth = 0.06) +
    geom_sf(data = lineas_ctrl, aes(linetype = tipo, color = tipo), linewidth = 0.35) +
    geom_segment(data = flechas, aes(x = x, y = y, xend = xend, yend = yend,
                                     linetype = tipo, color = tipo),
                 linewidth = 0.12,
                 arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +
    geom_point(data = puntos, aes(x = x, y = y), color = "grey15", size = 0.25) +
    geom_label_repel(
      data = banderas,
      aes(x = x, y = y, label = num, color = grupo),
      inherit.aes = FALSE,
      fill = "white", size = 1.9, fontface = "bold",
      label.size = 0.15, label.r = unit(0, "pt"), label.padding = unit(0.08, "cm"),
      segment.size = 0.1, segment.color = "grey35", min.segment.length = 0,
      box.padding = 0.25, point.padding = 0.1, force = 3, max.overlaps = Inf,
      seed = 1, show.legend = FALSE
    ) +
    # Recuadro con N y conteo de tratados y controles (sobre Venezuela, arriba a la izquierda de Puerto Carreño)
    annotate("label", x = -70.3, y = 7.4, hjust = 0, vjust = 0,
             label = paste0("N = ", nrow(pares),
                            "\n# Tratados: ", sum(pares$grupo == "Tratados"),
                            "\n# Controles: ", sum(pares$grupo == "Controles")),
             size = 3, lineheight = 1.1, fill = "white", text.colour = "black",
             border.colour = "grey50", linewidth = 0.25,
             label.r = unit(0, "pt"), label.padding = unit(0.15, "cm")) +
    coord_sf(datum = NA) +
    # Leyenda: colores y etiquetas
    scale_fill_manual(
      name   = NULL,
      values = colores_relleno,
      labels = c("Tratados"  = paste0("Tratados (cerca: ", var, " = 1)"),
                 "Controles" = paste0("Controles (lejos: ", var, " = 0)"),
                 "Destino"   = txt$destino),
      guide  = guide_legend(order = 1)
    ) +
    # Leyenda de líneas (tipo y color comparten nombre/etiquetas para fusionarse en una sola);
    # la escala de color incluye también los colores de las banderas, sin mostrarlos en la leyenda
    scale_linetype_manual(
      name   = NULL,
      breaks = c("Enlace tratado", "Enlace control"),
      values = c("Enlace tratado" = "solid", "Enlace control" = "dotted"),
      labels = etiquetas_lineas,
      guide  = guide_legend(order = 2)
    ) +
    scale_color_manual(
      name   = NULL,
      breaks = c("Enlace tratado", "Enlace control"),
      values = c("Enlace tratado" = "grey15", "Enlace control" = "#8B0000", colores_banderas),
      labels = etiquetas_lineas,
      guide  = guide_legend(order = 2)
    ) +
    # Título, subtítulo y nota
    labs(
      title    = txt$titulo,
      subtitle = paste0(txt$subtitulo, ", respecto ", corte),
      caption  = txt$nota
    ) +
    theme_void() +
    theme(
      plot.background   = element_rect(fill = "white", color = NA),
      plot.title        = element_text(face = "bold", size = 15, hjust = 0.5),
      plot.subtitle     = element_text(size = 10, hjust = 0.5, color = "grey30"),
      plot.caption      = element_text(size = 7, color = "grey30", hjust = 0.5),
      plot.margin       = margin(12, 12, 12, 12),
      # Posición de la leyenda (esquina inferior izquierda, sobre el Pacífico)
      legend.position   = "inside",
      legend.position.inside = c(0.2, 0.2),
      legend.spacing.y  = unit(0.05, "cm"),
      legend.text       = element_text(size = 9),
      legend.key.size   = unit(0.45, "cm")
    )

  ggsave(out, p, width = 7, height = 8.4, dpi = 1200, bg = "white")
  invisible(p)
}

# ---- 2. Mapas 1 y 2: d1_eucl (centroide municipal → centroide del enclave más cercano) ----

txt_d1 <- list(
  destino   = "Enclave cocalero más cercano",
  corto     = "enclave",
  titulo    = "Municipios tratados, de control y enclave más cercano",
  subtitulo = "Distancia euclidiana (línea recta) del centroide municipal al centroide\ndel enclave más cercano",
  nota      = "Números: tratados (azul) y controles (rojo), de norte a sur; enclaves (verde) según su número."
)
args_d1 <- list(destino = "enclave", xy_orig = xy, xy_dest = xy_enc,
                poly_dest = enclaves, num_dest = "propio", txt = txt_d1)

do.call(mapa_tratados, c(list("d1_eucl_mean",    "a la media",   file.path(figuras, "mapa_1_d1_eucl_media.png")), args_d1))
do.call(mapa_tratados, c(list("d1_eucl_mediana", "a la mediana", file.path(figuras, "mapa_1_eucl_mediana.png")),  args_d1))

# Puntos de corte de d2 y d3 (Master.do, sección 4.1): sufijo de la variable, texto del subtítulo,
# nombre en el título y sufijo del archivo. Cada juego de 4 mapas se guarda en su propia subcarpeta.
cortes <- data.frame(
  var    = c("mean",       "mediana",       "p60",             "p75"),
  texto  = c("a la media", "a la mediana",  "al percentil 60", "al percentil 75"),
  titulo = c("Media",      "Mediana (p50)", "Percentil 60",    "Percentil 75"),
  arch   = c("media",      "mediana",       "p60",             "p75")
)

juego_mapas <- function(d, args) {
  carpeta <- file.path(figuras, paste0(d, "_eucl"))
  dir.create(carpeta, showWarnings = FALSE)
  titulo <- args$txt$titulo
  for (i in seq_len(nrow(cortes))) {
    # Título con el punto de corte en una segunda línea
    args$txt$titulo <- paste0(titulo, "\nPunto de corte: ", cortes$titulo[i])
    do.call(mapa_tratados, c(list(paste0(d, "_eucl_", cortes$var[i]), cortes$texto[i],
                                  file.path(carpeta, paste0("mapa_", d, "_eucl_", cortes$arch[i], ".png"))),
                             args))
  }
}

# ---- 3. Juego d2_eucl (centroide municipal → centroide del PNIS alta prob. más cercano) ----

poly_pnis <- mpios[mpios$MPIO_CDPMP %in% pnis_alta, ]
poly_pnis$id <- poly_pnis$MPIO_CDPMP

txt_d2 <- list(
  destino   = "PNIS alta probabilidad más cercano",
  corto     = "PNIS",
  titulo    = "Municipios tratados, de control y PNIS más cercano",
  subtitulo = "Distancia euclidiana (línea recta) del centroide municipal al centroide\ndel municipio PNIS de alta probabilidad más cercano",
  nota      = "Números: tratados (azul), controles (rojo) y PNIS (verde), de norte a sur."
)
args_d2 <- list(destino = "pnis_d2", xy_orig = xy, xy_dest = xy,
                poly_dest = poly_pnis, num_dest = "norte_sur", txt = txt_d2)

juego_mapas("d2", args_d2)

# ---- 4. Juego d3_eucl (cabecera municipal → cabecera del PNIS alta prob. más cercano) ----

txt_d3 <- list(
  destino   = "PNIS alta probabilidad más cercano",
  corto     = "PNIS",
  titulo    = "Municipios tratados, de control y PNIS más cercano",
  subtitulo = "Distancia euclidiana (línea recta) de la cabecera municipal a la cabecera\ndel municipio PNIS de alta probabilidad más cercano",
  nota      = "Números: tratados (azul), controles (rojo) y PNIS (verde), de norte a sur. Puntos: centroides de las cabeceras municipales."
)
args_d3 <- list(destino = "pnis_d3", xy_orig = xy_cab, xy_dest = xy_cab,
                poly_dest = poly_pnis, num_dest = "norte_sur", txt = txt_d3)

juego_mapas("d3", args_d3)
