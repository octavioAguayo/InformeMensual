# ==============================================================================
# CreaTablas.R
# Crea tablas flextable exportadas al informe
# Depende de: Funciones_ENE.R, Variables_ENE.R, Crea_Carga_BBDD.R
# ==============================================================================

# Librerías ####
library(dplyr)
library(tidyr)
library(lubridate)
library(flextable)
library(officer)
library(stringr)

# Parámetros globales ####
options(scipen = 999)

## Cortes para regresiones por tramos (compartido con CreaGraf.R) ####
cortes <- data.frame(
  inicio = c(as.Date("2010-02-01"), as.Date("2013-01-01"), Fechas["Fin_Covid"]),
  fin    = c(as.Date("2012-12-01"), Fechas["inicio_covid"] %m-% months(1), Fechas["actual"])
)

# ==============================================================================
# DATOS BASE COMPARTIDOS ####
# ==============================================================================

tmp_ENE_fechas <- dt_ENE_sexo %>% filter(fecha %in% Fechas)

res_completo <- tmp_ENE_fechas %>%
  mutate(valor = round(valor, 1)) %>%
  pivot_wider(
    id_cols     = c(sexo, categoria),
    names_from  = fecha,
    values_from = valor
  )

filtro_tabla <- tribble(
  ~sexo, ~categoria,              ~categoriaNombre,                    ~tipo_valor,
  "AS", "PET",                    "PET",                               "stock",
  "AS", "FT",                     "Fuerza de Trabajo",                 "stock",
  "AS", "Ocupados",               "Ocupados",                          "stock",
  "AS", "Desocupados",            "Desocupados",                       "stock",
  "AS", "Ocupados formales",      "Ocupados Formales",                 "stock",
  "AS", "Ocupado_Privado",        "Ocupados sin Asalariados Públicos", "stock",
  "AS", "Asalariados privados",   "Asalariados Privados",              "stock",
  "AS", "Asalariados público",    "Asalariados Públicos",              "stock",
  "M",  "Ocupados",               "Ocupadas",                          "stock",
  "M",  "Ocupados formales",      "Ocupadas Formales",                 "stock",
  "AS", "T_desocupación",         "Tasa de Desocupación",              "tasa",
  "M",  "T_desocupación",         "Tasa de Desocupación Femenina",     "tasa",
  "AS", "T_Informal",             "Tasa de Ocupación Informal",        "tasa",
  "AS", "T_ocupación",            "Tasa de Ocupación",                 "tasa",
  "AS", "T_participación",        "Tasa de Participación",             "tasa",
  "M",  "T_participación",        "Tasa de Participación Femenina",    "tasa"
) %>%
  mutate(indice = row_number())

res_filtrado <- res_completo %>%
  inner_join(filtro_tabla, by = c("sexo", "categoria")) %>%
  mutate(across(where(is.numeric),
                ~ if_else(tipo_valor == "stock", round(., 0), round(., 1)))) %>%
  arrange(indice) %>%
  select(-indice, -tipo_valor)

tabla_cambio <- function(res_filtrado, fecha_base, fecha_act) {
  col_base <- as.character(fecha_base)
  col_act  <- as.character(fecha_act)
  out <- res_filtrado %>%
    mutate(Cambio = as.numeric(.data[[col_act]]) - as.numeric(.data[[col_base]])) %>%
    select(categoriaNombre, all_of(col_base), all_of(col_act), Cambio)
  names(out) <- c("Categorias", fc_fecha_a_trimestre(col_base), fc_fecha_a_trimestre(col_act), "Diferencia")
  return(out)
}

# ==============================================================================
# TABLAS VARIACIÓN ENTRE PERÍODOS (fc_Form_Resumen) ####
# ==============================================================================

ft_MVar_y_DMes <- fc_Form_Resumen(
  tabla_cambio(res_filtrado, Fechas["mes_anterior"], Fechas["actual"]),
  vect_decimales = c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1),
  encabezado = "Tabla 3a. Variación entre Trimestre actual y mes anterior"
)

ft_MVar_y_DAnno <- fc_Form_Resumen(
  tabla_cambio(res_filtrado, Fechas["a_1_anno"], Fechas["actual"]),
  vect_decimales = c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1),
  encabezado = "Tabla 3b. Variación entre Trimestre actual y año anterior"
)

ft_MVar_y_DPostCOVID <- fc_Form_Resumen(
  tabla_cambio(res_filtrado, Fechas["Fin_Covid"], Fechas["actual"]),
  vect_decimales = c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1),
  encabezado = "Tabla 3c. Variación acumulada desde el quiebre estructural post-COVID"
)

res_PreCOVID <- tabla_cambio(res_filtrado, Fechas["inicio_covid"], Fechas["actual"])
res_gobierno <- tabla_cambio(res_filtrado, Fechas["inicio_gob"],   Fechas["actual"])

# ==============================================================================
# TABLA 1: Indicadores clave con variaciones trimestral e interanual ####
# ==============================================================================

indicadores_t1 <- list(
  list(nombre = "Desocupados",             cat = "Desocupados",       tipo = "entero"),
  list(nombre = "Ocupados",                cat = "Ocupados",          tipo = "entero"),
  list(nombre = "Ocupados Formales",       cat = "Ocupados formales", tipo = "entero"),
  list(nombre = "Tasa Desocupación",       cat = "T_desocupación",    tipo = "decimal"),
  list(nombre = "Tasa Ocupación Informal", cat = "T_Informal",        tipo = "decimal"),
  list(nombre = "Tasa Participación",      cat = "T_participación",   tipo = "decimal")
)

sexos_t1 <- list(
  list(label = "Total (ambos sexos)", cod = "AS"),
  list(label = "Hombres",             cod = "H"),
  list(label = "Mujeres",             cod = "M")
)

filas_t1 <- lapply(indicadores_t1, function(ind) {
  lapply(sexos_t1, function(sx) {
    val  <- fc_Datos_ENE(fecha_Actual,      sx$cod, ind$cat)
    prev <- fc_Datos_ENE(fecha_MesAnterior, sx$cod, ind$cat)
    anio <- fc_Datos_ENE(fecha1annos,       sx$cod, ind$cat)
    data.frame(
      Indicador        = ind$nombre,
      Desagregacion    = sx$label,
      Trimestre_actual = val,
      Dif_trim         = val - prev,
      Dif_interanual   = val - anio,
      tipo             = ind$tipo,
      stringsAsFactors = FALSE
    )
  }) %>% bind_rows()
}) %>% bind_rows()

fmt_num_t1 <- function(x, tipo, signo = FALSE) {
  digits <- ifelse(tipo == "decimal", 1, 0)
  s <- formatC(abs(x), format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
  if (signo && !is.na(x)) {
    if      (x > 0) paste0("+", s)
    else if (x < 0) paste0("-", s)
    else s
  } else s
}

base_t1 <- data.frame(
  Indicador     = filas_t1$Indicador,
  Desagregacion = filas_t1$Desagregacion,
  Actual        = mapply(fmt_num_t1, filas_t1$Trimestre_actual, filas_t1$tipo),
  Dif_trim      = filas_t1$Dif_trim,
  Dif_trim_fmt  = mapply(fmt_num_t1, filas_t1$Dif_trim, filas_t1$tipo),
  Dif_anio      = filas_t1$Dif_interanual,
  Dif_anio_fmt  = mapply(fmt_num_t1, filas_t1$Dif_interanual, filas_t1$tipo),
  tipo          = filas_t1$tipo,
  stringsAsFactors = FALSE
)

dt_display_t1 <- base_t1 %>%
  select(Indicador, Desagregacion, Actual, Dif_trim_fmt, Dif_anio_fmt)

names(dt_display_t1) <- c(
  "Indicador", " ",
  paste0("Trimestre actual\n(EFM ", año_act, ")"),
  "Diferencia trim.\nmóvil anterior",
  "Diferencia\ninteranual"
)

ft_Delta_indicadores <- flextable(dt_display_t1) %>%
  font(fontname = "Arial Narrow", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  fontsize(size = 8.5, part = "header") %>%
  bold(part = "header") %>%
  align(j = 1:2, align = "left",   part = "all") %>%
  align(j = 3:5, align = "right",  part = "all") %>%
  align(j = 3:5, align = "center", part = "header") %>%
  padding(padding.top = 2, padding.bottom = 2, part = "all") %>%
  width(j = 1, width = 1.3) %>% width(j = 2, width = 1.3) %>%
  width(j = 3, width = 1.1) %>% width(j = 4, width = 1.1) %>%
  width(j = 5, width = 1.1) %>%
  border_remove() %>%
  hline_top(border    = fp_border(width = 1.2), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.2), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.2), part = "body") %>%
  hline(i = c(3, 6, 9, 12, 15), border = fp_border(width = 0.5, color = "gray60"))

for (g in seq_along(indicadores_t1)) {
  filas_g <- ((g - 1) * 3 + 1):(g * 3)
  ft_Delta_indicadores <- merge_at(ft_Delta_indicadores, i = filas_g, j = 1)
  ft_Delta_indicadores <- valign(ft_Delta_indicadores, i = filas_g[1], j = 1, valign = "center")
}

for (i in seq_len(nrow(base_t1))) {
  for (col_idx in c(4, 5)) {
    val    <- if (col_idx == 4) base_t1$Dif_trim[i] else base_t1$Dif_anio[i]
    fmt    <- if (col_idx == 4) base_t1$Dif_trim_fmt[i] else base_t1$Dif_anio_fmt[i]
    unidad <- if (base_t1$tipo[i] == "decimal") " p.p" else ""
    flecha <- if (val > 0) "▲" else if (val < 0) "▼" else ""
    color  <- if (val > 0) "#27ae60" else if (val < 0) "#e74c3c" else "black"
    ft_Delta_indicadores <- compose(ft_Delta_indicadores, i = i, j = col_idx,
                                    value = as_paragraph(
                                      as_chunk(flecha, props = fp_text(color = color, font.size = 7)),
                                      as_chunk(paste0(" ", fmt, unidad))
                                    ))
  }
}

ft_Delta_indicadores <- add_header_lines(
  ft_Delta_indicadores,
  values = paste0("Tabla 1: Cambios trimestrales e interanuales en indicadores claves, ",
                  format(fecha_Actual, "%B %Y"))
) %>%
  bold(i = 1, part = "header") %>%
  fontsize(i = 1, size = 9, part = "header") %>%
  align(i = 1, align = "left", part = "header") %>%
  add_footer_lines("Fuente: Elaboración propia en base a datos ENE.") %>%
  fontsize(size = 7, part = "footer") %>%
  italic(part = "footer") %>%
  color(color = "gray40", part = "footer") %>%
  set_table_properties(opts_word = list(split = FALSE))

# ==============================================================================
# TABLA 1a: Empleos formales por sector económico ####
# ==============================================================================

cats_rama_form <- c(
  "Agricultura, ganadería, silvicultura y pesca formal",
  "Explotación de minas y canteras formal",
  "Industrias manufactureras formal",
  "Suministro de electricidad, gas, vapor y aire acondicionado formal",
  "Suministro de agua formal", "Construcción formal",
  "Comercio al por mayor y al por menor formal",
  "Transporte y almacenamiento formal",
  "Actividades de alojamiento y de servicio de comidas formal",
  "Información y comunicaciones formal",
  "Actividades financieras y de seguros formal",
  "Actividades inmobiliarias formal",
  "Actividades profesionales, científicas y técnicas formal",
  "Actividades de servicios administrativos y de apoyo formal",
  "Administración pública y defensa formal", "Enseñanza formal",
  "Actividades de atención de la salud humana y de asistencia social formal",
  "Actividades artísticas, de entretenimiento y recreativas formal",
  "Otras actividades de servicios formal",
  "Actividades de los hogares como empleadores formal",
  "Actividades de organizaciones y órganos extraterritoriales formal",
  "No sabe - No responde formal"
)

labels_rama_form <- c(
  "Agricultura, ganadería, silvicultura y pesca",
  "Explotación de minas y canteras", "Industrias manufactureras",
  "Suministro de electricidad, gas, vapor y aire acondicionado",
  "Suministro de agua", "Construcción",
  "Comercio al por mayor y al por menor", "Transporte y almacenamiento",
  "Actividades de alojamiento y de servicio de comidas",
  "Información y comunicaciones", "Actividades financieras y de seguros",
  "Actividades inmobiliarias",
  "Actividades profesionales, científicas y técnicas",
  "Actividades de servicios administrativos y de apoyo",
  "Administración pública y defensa", "Enseñanza",
  "Actividades de atención de la salud humana y de asistencia social",
  "Actividades artísticas, de entretenimiento y recreativas",
  "Otras actividades de servicios",
  "Actividades de los hogares como empleadores",
  "Actividades de organizaciones y órganos extraterritoriales",
  "No sabe/No responde"
)

base_rama_form <- dt_ENE_sexo %>%
  filter(sexo == "AS", categoria %in% cats_rama_form,
         fecha %in% c(fecha_Actual, fecha1annos)) %>%
  pivot_wider(names_from = fecha, values_from = valor) %>%
  rename(actual = as.character(fecha_Actual), anno = as.character(fecha1annos)) %>%
  mutate(dif_anno = actual - anno,
         label    = labels_rama_form[match(categoria, cats_rama_form)]) %>%
  arrange(match(categoria, cats_rama_form))

trim_label_t1a <- paste0("Trimestre actual\n(", toupper(format(fecha_Actual, "%b")),
                          " ", format(fecha_Actual, "%Y"), ")")

dt_t1a <- data.frame(
  Sector  = base_rama_form$label,
  Actual  = formatC(base_rama_form$actual, format = "f", digits = 0,
                    big.mark = ".", decimal.mark = ","),
  dif_val = base_rama_form$dif_anno,
  dif_fmt = formatC(abs(base_rama_form$dif_anno), format = "f", digits = 0,
                    big.mark = ".", decimal.mark = ","),
  stringsAsFactors = FALSE
)

ft_Empleos_Sector_y_d12m <- flextable(dt_t1a %>% select(Sector, Actual, dif_fmt)) %>%
  set_header_labels(Sector = "Sector económico", Actual = trim_label_t1a,
                    dif_fmt = "Diferencia\ninteranual") %>%
  font(fontname = "Arial Narrow", part = "all") %>%
  fontsize(size = 7.5, part = "all") %>%
  fontsize(size = 8,   part = "header") %>%
  bold(part = "header") %>%
  align(j = 1,   align = "left",   part = "all") %>%
  align(j = 2:3, align = "right",  part = "all") %>%
  align(j = 2:3, align = "center", part = "header") %>%
  padding(padding.top = 1, padding.bottom = 1, part = "all") %>%
  width(j = 1, width = 3.2) %>% width(j = 2, width = 1.2) %>%
  width(j = 3, width = 1.2) %>%
  border_remove() %>%
  hline_top(border    = fp_border(width = 1.2), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.2), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.2), part = "body")

for (i in seq_len(nrow(dt_t1a))) {
  val    <- dt_t1a$dif_val[i]
  fmt    <- dt_t1a$dif_fmt[i]
  flecha <- if (val > 0) "▲" else if (val < 0) "▼" else ""
  color  <- if (val > 0) "#27ae60" else if (val < 0) "#e74c3c" else "black"
  ft_Empleos_Sector_y_d12m <- compose(
    ft_Empleos_Sector_y_d12m, i = i, j = "dif_fmt",
    value = as_paragraph(
      as_chunk(flecha, props = fp_text(color = color, font.size = 7)),
      as_chunk(paste0(" ", fmt))
    ))
}

ft_Empleos_Sector_y_d12m <- ft_Empleos_Sector_y_d12m %>%
  add_header_lines("Tabla 1a: Cantidad de empleos formales creados o destruidos según sector económico") %>%
  bold(i = 1, part = "header") %>%
  fontsize(i = 1, size = 9, part = "header") %>%
  align(i = 1, align = "left", part = "header") %>%
  add_footer_lines("Fuente: Elaboración propia en base a datos ENE.") %>%
  fontsize(size = 7, part = "footer") %>%
  italic(part = "footer") %>%
  color(color = "gray40", part = "footer") %>%
  set_table_properties(opts_word = list(split = FALSE))

# ==============================================================================
# TABLA 2: Razones FFT ####
# ==============================================================================

fft_base <- dt_Coyuntural %>%
  filter(tipo_categoria == "fft ampliada",
         valor_eje      == "Total nacional",
         sexo_label     %in% c("Hombres", "Mujeres")) %>%
  mutate(Categoria = gsub("^FFT ", "", categoria)) %>%
  select(Categoria, sexo_label, valor)

fft_tabla <- fft_base %>%
  pivot_wider(names_from = sexo_label, values_from = valor) %>%
  mutate(Total        = Hombres + Mujeres,
         Femenización = round(100 * Mujeres / Total, 1)) %>%
  arrange(desc(Femenización))

ft_Razon_FFT <- fc_Form_Estandar(
  fft_tabla,
  formato_columnas = c("texto", "entero", "entero", "entero", "decimal"),
  encabezado = paste("Tabla 2. Razones para estar fuera de la fuerza de trabajo a",
                     fc_fecha_a_trimestre(fecha_Actual))
)

# ==============================================================================
# TABLAS REGIÓN ####
# ==============================================================================

fc_Tabla_Region <- function(dt, cols_def, titulo,
                            nota   = NULL,
                            fuente = "Fuente: Elaboración propia en base a datos ENE.") {

  fmt_val <- function(x, es_tasa) {
    if (es_tasa) paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "%")
    else formatC(x, format = "f", digits = 0, big.mark = ".", decimal.mark = ",")
  }
  fmt_dif <- function(x, es_tasa) {
    s <- formatC(abs(x), format = "f", digits = if (es_tasa) 1 else 0,
                 big.mark = ".", decimal.mark = ",")
    paste0(s, if (es_tasa) " p.p" else "")
  }
  make_wide <- function(fecha_f) {
    dt %>% filter(fecha == fecha_f) %>%
      select(region_label, categoria, valor) %>%
      pivot_wider(names_from = categoria, values_from = valor)
  }

  base      <- make_wide(fecha_Actual)
  base_mes  <- make_wide(fecha_MesAnterior)
  base_anno <- make_wide(fecha1annos)

  total_wide <- function(base_w) {
    base_w %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
      mutate(region_label = "Total (nacional)")
  }

  base      <- bind_rows(base,      total_wide(base))
  base_mes  <- bind_rows(base_mes,  total_wide(base_mes))
  base_anno <- bind_rows(base_anno, total_wide(base_anno))

  regiones  <- base$region_label
  resultado <- data.frame(region_label = regiones, stringsAsFactors = FALSE)

  for (cd in cols_def) {
    val   <- base[[cd$cat]]
    val_m <- base_mes[[cd$cat]]
    val_a <- base_anno[[cd$cat]]
    resultado[[cd$nombre]]                       <- mapply(fmt_val, val, cd$es_tasa)
    resultado[[paste0("dif_m_", cd$nombre)]]     <- val - val_m
    resultado[[paste0("dif_m_fmt_", cd$nombre)]] <- mapply(fmt_dif, val - val_m, cd$es_tasa)
    resultado[[paste0("dif_a_", cd$nombre)]]     <- val - val_a
    resultado[[paste0("dif_a_fmt_", cd$nombre)]] <- mapply(fmt_dif, val - val_a, cd$es_tasa)
    resultado[["sort_col"]]                      <- val
  }

  fila_total <- resultado %>% filter(region_label == "Total (nacional)")
  resultado  <- resultado %>%
    filter(region_label != "Total (nacional)") %>%
    arrange(desc(sort_col)) %>%
    bind_rows(fila_total)

  cd1 <- cols_def[[1]]
  if (length(cols_def) == 1) {
    dt_display <- resultado %>%
      select(region_label, !!cd1$nombre,
             !!paste0("dif_m_fmt_", cd1$nombre),
             !!paste0("dif_a_fmt_", cd1$nombre))
    trim_lbl <- paste0("Trimestre actual\n(", toupper(format(fecha_Actual, "%b")),
                       " ", format(fecha_Actual, "%Y"), ")")
    names(dt_display) <- c("Región", trim_lbl,
                           "Diferencia trimestre\nmóvil anterior tasa\ndesocupación",
                           "Diferencia interanual\ntasa desocupación")
    dif_cols <- list(
      list(j = 3, val_col = paste0("dif_m_", cd1$nombre), es_tasa = cd1$es_tasa),
      list(j = 4, val_col = paste0("dif_a_", cd1$nombre), es_tasa = cd1$es_tasa)
    )
  } else {
    cd2 <- cols_def[[2]]
    dt_display <- resultado %>% select(region_label, !!cd1$nombre, !!cd2$nombre)
    names(dt_display) <- c("Región", cd1$nombre, cd2$nombre)
    dif_cols <- list()
  }

  n_num     <- ncol(dt_display) - 1
  ft_region <- flextable(dt_display) %>%
    font(fontname = "Arial Narrow", part = "all") %>%
    fontsize(size = 7.5, part = "all") %>%
    fontsize(size = 8,   part = "header") %>%
    bold(part = "header") %>%
    align(j = 1,           align = "left",   part = "all") %>%
    align(j = 2:(1+n_num), align = "right",  part = "all") %>%
    align(j = 2:(1+n_num), align = "center", part = "header") %>%
    padding(padding.top = 1, padding.bottom = 1, part = "all") %>%
    width(j = 1, width = 1.8) %>%
    border_remove() %>%
    hline_top(border    = fp_border(width = 1.2), part = "header") %>%
    hline_bottom(border = fp_border(width = 1.2), part = "header") %>%
    hline_bottom(border = fp_border(width = 1.2), part = "body") %>%
    hline(i = nrow(dt_display) - 1, border = fp_border(width = 0.8, color = "gray40"))

  for (dc in dif_cols) {
    for (i in seq_len(nrow(dt_display))) {
      val <- resultado[[dc$val_col]][i]
      fmt <- dt_display[[dc$j]][i]
      if (is.na(val)) next
      flecha <- if (val > 0) "▲" else if (val < 0) "▼" else ""
      color  <- if (val > 0) "#27ae60" else if (val < 0) "#e74c3c" else "black"
      ft_region <- compose(ft_region, i = i, j = dc$j,
                           value = as_paragraph(
                             as_chunk(flecha, props = fp_text(color = color, font.size = 7)),
                             as_chunk(paste0(" ", fmt))
                           ))
    }
  }

  ft_region <- bold(ft_region, i = nrow(dt_display), part = "body")
  pie <- if (!is.null(nota)) paste0(fuente, "\n¹ ", nota) else fuente
  ft_region %>%
    add_header_lines(values = titulo) %>%
    bold(i = 1, part = "header") %>%
    fontsize(i = 1, size = 9, part = "header") %>%
    align(i = 1, align = "left", part = "header") %>%
    add_footer_lines(pie) %>%
    fontsize(size = 7, part = "footer") %>%
    italic(part = "footer") %>%
    color(color = "gray40", part = "footer") %>%
    set_table_properties(opts_word = list(split = FALSE))
}

## Tabla 2a: Tasa desocupación por región ####
ft_TDesocup_Region_m_y_d12m <- fc_Tabla_Region(
  dt       = dt_ENE_region %>% filter(categoria == "T_desocupación"),
  cols_def = list(list(nombre = "Tasa desocupación", cat = "T_desocupación", es_tasa = TRUE)),
  titulo   = "Tabla 2a: Tasa de desocupación y variaciones interanuales y trimestrales según región",
  nota     = "Las regiones se ordenan de mayor a menor según la tasa de desocupación del trimestre actual."
)

## Tabla 2b: Informalidad y asalariados dependientes informales por región ####
cats_A2_region <- c(
  "Ocupados informales", "Ocupados",
  "Asalariados privado formales", "Asalariados público formales", "Servicio doméstico formales",
  "Asalariados privado informales", "Asalariados público informales", "Servicio doméstico informales"
)

base_t2b_region <- dt_ENE_region %>%
  filter(categoria %in% cats_A2_region, fecha == fecha_Actual) %>%
  pivot_wider(names_from = categoria, values_from = valor) %>%
  mutate(
    T_Informal        = round(`Ocupados informales` / Ocupados * 100, 1),
    Asal_dep_Form     = `Asalariados privado formales` + `Asalariados público formales` + `Servicio doméstico formales`,
    Asal_dep_Inform   = `Asalariados privado informales` + `Asalariados público informales` + `Servicio doméstico informales`,
    Asal_dep          = Asal_dep_Form + Asal_dep_Inform,
    T_Asal_dep_Inform = round(Asal_dep_Inform / Asal_dep * 100, 1)
  )

total_t2b_region <- dt_ENE_region %>%
  filter(categoria %in% cats_A2_region, fecha == fecha_Actual) %>%
  group_by(categoria) %>%
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = categoria, values_from = valor) %>%
  mutate(
    region_label      = "Total (nacional)",
    T_Informal        = round(`Ocupados informales` / Ocupados * 100, 1),
    Asal_dep_Form     = `Asalariados privado formales` + `Asalariados público formales` + `Servicio doméstico formales`,
    Asal_dep_Inform   = `Asalariados privado informales` + `Asalariados público informales` + `Servicio doméstico informales`,
    Asal_dep          = Asal_dep_Form + Asal_dep_Inform,
    T_Asal_dep_Inform = round(Asal_dep_Inform / Asal_dep * 100, 1)
  )

fmt_pct_t2b <- function(x) paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "%")

dt_t2b <- bind_rows(
  base_t2b_region %>%
    arrange(desc(T_Informal)) %>%
    mutate(T_Inf_fmt = fmt_pct_t2b(T_Informal), T_Asal_fmt = fmt_pct_t2b(T_Asal_dep_Inform)) %>%
    select(region_label, T_Inf_fmt, T_Asal_fmt),
  total_t2b_region %>%
    mutate(T_Inf_fmt = fmt_pct_t2b(T_Informal), T_Asal_fmt = fmt_pct_t2b(T_Asal_dep_Inform)) %>%
    select(region_label, T_Inf_fmt, T_Asal_fmt)
)

ft_TInform_Region <- flextable(dt_t2b) %>%
  set_header_labels(region_label = "Región", T_Inf_fmt = "Tasa informalidad",
                    T_Asal_fmt   = "Tasa asalariados informales*") %>%
  font(fontname = "Arial Narrow", part = "all") %>%
  fontsize(size = 7.5, part = "all") %>%
  fontsize(size = 8,   part = "header") %>%
  bold(part = "header") %>%
  align(j = 1,   align = "left",   part = "all") %>%
  align(j = 2:3, align = "right",  part = "all") %>%
  align(j = 2:3, align = "center", part = "header") %>%
  padding(padding.top = 1, padding.bottom = 1, part = "all") %>%
  width(j = 1, width = 2.0) %>% width(j = 2, width = 1.3) %>%
  width(j = 3, width = 1.5) %>%
  border_remove() %>%
  hline_top(border    = fp_border(width = 1.2), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.2), part = "header") %>%
  hline_bottom(border = fp_border(width = 1.2), part = "body") %>%
  hline(i = nrow(dt_t2b) - 1, border = fp_border(width = 0.8, color = "gray40")) %>%
  bold(i = nrow(dt_t2b), part = "body") %>%
  add_header_lines("Tabla 2b: Tasa de ocupación informal y de asalariados informales según región") %>%
  bold(i = 1, part = "header") %>%
  fontsize(i = 1, size = 9, part = "header") %>%
  align(i = 1, align = "left", part = "header") %>%
  add_footer_lines(paste0(
    "Fuente: Elaboración propia en base a datos ENE.\n",
    "¹ *Tasa asalariados informales = (asalariados privados informales + asalariados públicos informales\n",
    "+ personal servicio doméstico informal) / (asalariados privados formales + asalariados privados\n",
    "+ informales + asalariados públicos formales + asalariados públicos informales + personal servicio\n",
    "+ doméstico formal + personal servicio doméstico informal)"
  )) %>%
  fontsize(size = 7, part = "footer") %>%
  italic(part = "footer") %>%
  color(color = "gray40", part = "footer") %>%
  set_table_properties(opts_word = list(split = FALSE))

