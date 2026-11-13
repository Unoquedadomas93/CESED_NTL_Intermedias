# Mapa limpio de Colombia: solo fronteras municipales (sin relleno, ejes ni capas)
# Fuente: MGN 2021 (DANE) - el shapefile de Enclaves es una grilla de coca (46 municipios), no sirve para esto.
library(sf)
library(ggplot2)
library(haven)

raiz <- "/Users/samuelquevedo/Mi unidad (samuelalgo16@hotmail.com)/Andes/Investigación/CESED/Luces_Nocturnas_Coca_y_Cannabis"
shp  <- file.path(raiz, "Shapefile Colombia/MGN_2021_COLOMBIA/ADMINISTRATIVO/MGN_MPIO_POLITICO.shp")
out  <- file.path(raiz, "Análisis/mapa_municipios_colombia.png")

mpios <- st_read(shp, quiet = TRUE)
message("Municipios: ", nrow(mpios))

# ---- 1. Mapa base: solo fronteras municipales ----

p <- ggplot(mpios) +
  geom_sf(fill = NA, color = "black", linewidth = 0.06) +
  coord_sf(datum = NA) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA))

ggsave(out, p, width = 7, height = 8, dpi = 400, bg = "white")

# ---- 2. Mapa de tratados vs. controles (d2_total_mean) ----
# Tratamiento definido en Master.do, sección 4.2: d2_total_mean = 1 si la distancia total
# (vías + ríos) del centroide municipal al centroide PNIS de alta probabilidad es <= la media.
# Se lee la base que guarda Master.do (correr Master.do antes si cambian los insumos).

dta     <- file.path(raiz, "STATA/data/auxiliary/tratados_distancia_total.dta")
out_trt <- file.path(raiz, "Análisis/mapa_tratados_d2_total_mean.png")

tratados <- read_dta(dta)
tratados$MPIO_CDPMP <- sprintf("%05d", as.integer(tratados$mpio_cdpmp))
tratados$grupo <- factor(tratados$d2_total_mean, levels = c(1, 0),
                         labels = c("Tratados", "Controles"))

mpios_trt <- merge(mpios, tratados[, c("MPIO_CDPMP", "grupo")], by = "MPIO_CDPMP")
message("Tratados: ", sum(mpios_trt$grupo == "Tratados"),
        " | Controles: ", sum(mpios_trt$grupo == "Controles"))

p_trt <- ggplot() +
  geom_sf(data = mpios_trt, aes(fill = grupo), color = NA) +
  geom_sf(data = mpios, fill = NA, color = "black", linewidth = 0.06) +
  coord_sf(datum = NA) +
  # Leyenda: colores y etiquetas
  scale_fill_manual(
    name   = NULL,
    values = c("Tratados" = "#9ECAE1", "Controles" = "#FCAE91"),
    labels = c("Tratados" = "Tratados (cerca: d2_total_mean = 1)",
               "Controles" = "Controles (lejos: d2_total_mean = 0)")
  ) +
  # Título y subtítulo
  labs(
    title    = "Municipios tratados y de control",
    subtitle = "Distancia total (vías + ríos) del centroide municipal al centroide\nPNIS de alta probabilidad, respecto a la media"
  ) +
  theme_void() +
  theme(
    plot.background   = element_rect(fill = "white", color = NA),
    plot.title        = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle     = element_text(size = 10, hjust = 0.5, color = "grey30"),
    plot.margin       = margin(12, 12, 12, 12),
    # Posición de la leyenda (esquina inferior izquierda, sobre el Pacífico)
    legend.position   = "inside",
    legend.position.inside = c(0.2, 0.2),
    legend.text       = element_text(size = 9),
    legend.key.size   = unit(0.45, "cm")
  )

ggsave(out_trt, p_trt, width = 7, height = 8.4, dpi = 400, bg = "white")

# ---- 3. Tratados vs. controles + municipio PNIS alta prob. más cercano (columna S) ----
# Columna S del Excel (centroide_pnis_centroide_mas_cercano): municipio PNIS de alta probabilidad
# cuyo centroide es el más cercano (distancia total vías + ríos) al centroide de cada ciudad.
# Las líneas son rectas entre centroides (solo ilustrativas; la distancia usada en d2 es por red).

library(readxl)

xlsx    <- file.path(raiz, "STATA/data/raw/distancias_via_rio_ciudades_intermedias.xlsx")
out_pnis <- file.path(raiz, "Análisis/mapa_tratados_d2_total_mean_pnis.png")

dist <- read_excel(xlsx)
dist$MPIO_CDPMP <- sprintf("%05d", as.integer(dist$codmpio))

# Emparejar el texto "Municipio (Departamento)" de la columna S con el shapefile
norm <- function(x) toupper(iconv(x, to = "ASCII//TRANSLIT"))
mpios$llave <- norm(paste0(mpios$MPIO_CNMBR, " (", mpios$DPTO_CNMBR, ")"))

pares <- merge(tratados[, c("MPIO_CDPMP", "grupo")],
               dist[, c("MPIO_CDPMP", "centroide_pnis_centroide_mas_cercano")],
               by = "MPIO_CDPMP")
pares$PNIS_CDPMP <- mpios$MPIO_CDPMP[match(norm(pares$centroide_pnis_centroide_mas_cercano), mpios$llave)]
stopifnot(!anyNA(pares$PNIS_CDPMP))

mpios_pnis <- mpios[mpios$MPIO_CDPMP %in% unique(pares$PNIS_CDPMP), ]
message("Municipios PNIS alta prob. emparejados (col. S): ", nrow(mpios_pnis))

# Centroides calculados en MAGNA-SIRGAS Origen Nacional (EPSG:9377) y devueltos a lat/lon
centroides <- st_transform(st_centroid(st_transform(st_geometry(mpios), 9377)), st_crs(mpios))
xy <- st_coordinates(centroides)
rownames(xy) <- mpios$MPIO_CDPMP

# Líneas: sólida gris para tratados, punteada rojo oscuro para controles
lineas <- st_sf(
  geometry = st_sfc(lapply(seq_len(nrow(pares)), function(i)
    st_linestring(rbind(xy[pares$MPIO_CDPMP[i], ], xy[pares$PNIS_CDPMP[i], ]))),
    crs = st_crs(mpios)),
  tipo = factor(ifelse(pares$grupo == "Tratados", "Enlace tratado", "Enlace control"),
                levels = c("Enlace tratado", "Enlace control"))
)
lineas_ctrl <- lineas[lineas$tipo == "Enlace control", ]

# Flechas de tratados (geom_sf no dibuja flechas, se usan segmentos con coordenadas lon/lat)
pares_trt <- pares[pares$grupo == "Tratados", ]
flechas <- data.frame(
  x    = xy[pares_trt$MPIO_CDPMP, "X"], y    = xy[pares_trt$MPIO_CDPMP, "Y"],
  xend = xy[pares_trt$PNIS_CDPMP, "X"], yend = xy[pares_trt$PNIS_CDPMP, "Y"],
  tipo = factor("Enlace tratado", levels = levels(lineas$tipo))
)

# Separación entre la punta de la flecha y el centroide PNIS (en grados, ~0.1° ≈ 11 km)
separacion <- 0.1
largo <- sqrt((flechas$xend - flechas$x)^2 + (flechas$yend - flechas$y)^2)
flechas$xend <- flechas$xend - separacion * (flechas$xend - flechas$x) / largo
flechas$yend <- flechas$yend - separacion * (flechas$yend - flechas$y) / largo

puntos <- st_sf(geometry = centroides[mpios$MPIO_CDPMP %in% c(pares$MPIO_CDPMP, pares$PNIS_CDPMP)])

# Relleno conjunto: tratados, controles y PNIS en una sola leyenda
relleno <- rbind(
  mpios_trt[, "grupo"],
  st_sf(grupo = "PNIS", geometry = st_geometry(mpios_pnis))
)
relleno$grupo <- factor(relleno$grupo, levels = c("Tratados", "Controles", "PNIS"))

p_pnis <- ggplot() +
  geom_sf(data = relleno, aes(fill = grupo), color = NA) +
  geom_sf(data = mpios, fill = NA, color = "black", linewidth = 0.06) +
  geom_sf(data = lineas_ctrl, aes(linetype = tipo, color = tipo), linewidth = 0.35) +
  geom_segment(data = flechas, aes(x = x, y = y, xend = xend, yend = yend,
                                   linetype = tipo, color = tipo),
               linewidth = 0.12,
               arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +
  geom_sf(data = puntos, color = "grey15", size = 0.25) +
  coord_sf(datum = NA) +
  # Leyenda: colores y etiquetas
  scale_fill_manual(
    name   = NULL,
    values = c("Tratados" = "#9ECAE1", "Controles" = "#FCAE91", "PNIS" = "#A1D99B"),
    labels = c("Tratados"  = "Tratados (cerca: d2_total_mean = 1)",
               "Controles" = "Controles (lejos: d2_total_mean = 0)",
               "PNIS"      = "PNIS alta probabilidad más cercano"),
    guide  = guide_legend(order = 1)
  ) +
  # Leyenda de líneas (tipo y color comparten nombre/etiquetas para fusionarse en una sola)
  scale_linetype_manual(
    name   = NULL,
    breaks = c("Enlace tratado", "Enlace control"),
    values = c("Enlace tratado" = "solid", "Enlace control" = "dotted"),
    labels = c("Enlace tratado" = "Línea recta tratado → PNIS más cercano",
               "Enlace control" = "Línea recta control → PNIS más cercano"),
    guide  = guide_legend(order = 2)
  ) +
  scale_color_manual(
    name   = NULL,
    breaks = c("Enlace tratado", "Enlace control"),
    values = c("Enlace tratado" = "grey15", "Enlace control" = "#8B0000"),
    labels = c("Enlace tratado" = "Línea recta tratado → PNIS más cercano",
               "Enlace control" = "Línea recta control → PNIS más cercano"),
    guide  = guide_legend(order = 2)
  ) +
  # Título y subtítulo
  labs(
    title    = "Municipios tratados, de control y PNIS más cercano",
    subtitle = "Distancia total (vías + ríos) del centroide municipal al centroide\nPNIS de alta probabilidad más cercano, respecto a la media"
  ) +
  theme_void() +
  theme(
    plot.background   = element_rect(fill = "white", color = NA),
    plot.title        = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle     = element_text(size = 10, hjust = 0.5, color = "grey30"),
    plot.margin       = margin(12, 12, 12, 12),
    # Posición de la leyenda (esquina inferior izquierda, sobre el Pacífico)
    legend.position   = "inside",
    legend.position.inside = c(0.2, 0.2),
    legend.spacing.y  = unit(0.05, "cm"),
    legend.text       = element_text(size = 9),
    legend.key.size   = unit(0.45, "cm")
  )

ggsave(out_pnis, p_pnis, width = 7, height = 8.4, dpi = 1200, bg = "white")

# ---- 4. Mapa de la sección 3 + banderas numeradas por grupo (para tabla de convenciones) ----
# Tres listas independientes: tratados (azul oscuro), controles (rojo oscuro) y PNIS (verde oscuro).
# Numeración de norte a sur dentro de cada grupo. La tabla de claves se exporta a CSV.

library(ggrepel)

out_num   <- file.path(raiz, "Análisis/mapa_tratados_d2_total_mean_pnis_numerado.png")
out_claves <- file.path(raiz, "Análisis/claves_mapa_numerado.csv")

nombre <- function(cod) {
  i <- match(cod, mpios$MPIO_CDPMP)
  paste0(mpios$MPIO_CNMBR[i], " (", mpios$DPTO_CNMBR[i], ")")
}

numerar <- function(cods, grupo) {
  d <- data.frame(MPIO_CDPMP = cods, grupo = grupo,
                  x = xy[cods, "X"], y = xy[cods, "Y"])
  d <- d[order(-d$y), ]
  d$num <- seq_len(nrow(d))
  d
}

banderas <- rbind(
  numerar(pares$MPIO_CDPMP[pares$grupo == "Tratados"],  "Tratados"),
  numerar(pares$MPIO_CDPMP[pares$grupo == "Controles"], "Controles"),
  numerar(unique(pares$PNIS_CDPMP),                     "PNIS")
)
banderas$grupo <- factor(banderas$grupo, levels = c("Tratados", "Controles", "PNIS"))
banderas$municipio <- nombre(banderas$MPIO_CDPMP)

# Tabla de claves: número, municipio y, para tratados/controles, su PNIS emparejado
pnis_num <- banderas[banderas$grupo == "PNIS", c("MPIO_CDPMP", "num")]
claves <- merge(banderas[, c("grupo", "num", "MPIO_CDPMP", "municipio")],
                pares[, c("MPIO_CDPMP", "PNIS_CDPMP")], by = "MPIO_CDPMP", all.x = TRUE)
claves$pnis_num <- pnis_num$num[match(claves$PNIS_CDPMP, pnis_num$MPIO_CDPMP)]
claves$pnis_municipio <- ifelse(is.na(claves$PNIS_CDPMP), NA, nombre(claves$PNIS_CDPMP))
claves$dist_total_km <- round(dist$dist_centroide_pnis_centroide_total_km[
  match(claves$MPIO_CDPMP, dist$MPIO_CDPMP)], 1)
claves <- claves[order(claves$grupo, claves$num),
                 c("grupo", "num", "MPIO_CDPMP", "municipio", "pnis_num", "pnis_municipio", "dist_total_km")]
write.csv(claves, out_claves, row.names = FALSE, na = "", fileEncoding = "UTF-8")

# Banderas: número en color oscuro del grupo sobre rectángulo blanco, con línea guía al centroide
colores_banderas <- c("Tratados" = "#08306B", "Controles" = "#67000D", "PNIS" = "#00441B")

p_num <- p_pnis +
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
  # Escala de color de banderas separada de la de líneas (ggnewscale no es necesario:
  # se usan las mismas claves de color extendidas)
  scale_color_manual(
    name   = NULL,
    breaks = c("Enlace tratado", "Enlace control"),
    values = c("Enlace tratado" = "grey15", "Enlace control" = "#8B0000", colores_banderas),
    labels = c("Enlace tratado" = "Línea recta tratado → PNIS más cercano",
               "Enlace control" = "Línea recta control → PNIS más cercano"),
    guide  = guide_legend(order = 2)
  ) +
  labs(caption = "Números: tratados (azul), controles (rojo) y PNIS (verde), de norte a sur.") +
  theme(plot.caption = element_text(size = 7, color = "grey30", hjust = 0.5))

ggsave(out_num, p_num, width = 7, height = 8.4, dpi = 1200, bg = "white")
