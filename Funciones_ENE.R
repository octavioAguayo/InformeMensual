library(lubridate)

## Función que convierte fecha en trimestre INE####

fc_fecha_a_trimestre <- function(fecha) {
  # Asegurarse que fecha sea Date
  fecha <- as.Date(fecha)
  mes <- as.integer(format(fecha, "%m"))
  anio <- as.integer(format(fecha, "%Y"))
  
  # Vector de nombres de trimestres (inverso)
  trimestre <- case_when(
    mes == 2 ~ "EFM",
    mes == 3 ~ "FMA",
    mes == 4 ~ "MAM",
    mes == 5 ~ "AMJ",
    mes == 6 ~ "MJJ",
    mes == 7 ~ "JJA",
    mes == 8 ~ "JAS",
    mes == 9 ~ "ASO",
    mes == 10 ~ "SON",
    mes == 11 ~ "OND",
    mes == 12 ~ "NDE",
    mes == 1 ~ "DEF",
    TRUE ~ NA_character_
  )
  fc_fecha_a_trimestre <- paste(trimestre,"-",anio)
  return(fc_fecha_a_trimestre)
}

##  Crea Vector de fechas con nombres filas, evita dependencias por posición ####

fecha_Actual <- as.Date(paste0(Trim_Actual, "01"), "%Y%m%d")
fecha1annos <- fecha_Actual %m-% years(1)
fecha2annos <- fecha_Actual %m-% years(2)
fecha3annos <- fecha_Actual %m-% years(3)
fecha4annos <- fecha_Actual %m-% years(4)
fecha5annos <- fecha_Actual %m-% years(5)
fecha6annos <- fecha_Actual %m-% years(6)
fecha7annos <- fecha_Actual %m-% years(7)
fecha8annos <- fecha_Actual %m-% years(8)
fecha9annos <- fecha_Actual %m-% years(9)
fecha10annos <- fecha_Actual %m-% years(10)
fecha11annos <- fecha_Actual %m-% years(11)
fecha12annos <- fecha_Actual %m-% years(12)
fecha13annos <- fecha_Actual %m-% years(13)
fecha_MesAnterior <- fecha_Actual %m-% months(1)
fecha_Inicio_Gob <- make_date(year = 2026 , month = 2 , day = 1)
fecha_Inicio_Covid <- as.Date("2020-02-01")
fecha_Covid_v <- as.Date("2020-06-01")
fecha_Fin_Covid <- make_date(year = 2022 , month = 2 , day = 1)
fecha_Formalidad <- as.Date("2017-08-01")

Fechas <- c(
  actual        = fecha_Actual,
  a_1_anno      = fecha1annos,
  a_2_annos     = fecha2annos,
  a_3_annos     = fecha3annos,
  a_4_annos     = fecha4annos,
  a_5_annos     = fecha5annos,
  a_6_annos     = fecha6annos,
  a_7_annos     = fecha7annos,
  a_8_annos     = fecha8annos,
  a_9_annos     = fecha9annos,
  a_10_annos    = fecha10annos,
  a_11_annos    = fecha11annos,
  a_12_annos    = fecha12annos,
  a_13_annos    = fecha13annos,
  mes_anterior  = fecha_MesAnterior,
  inicio_gob    = fecha_Inicio_Gob,
  inicio_covid  = fecha_Inicio_Covid,
  Fin_Covid     = fecha_Fin_Covid,
  fecha_Covid   = fecha_Covid_v
)


# Funciones y matrices

fc_salto_pagina <- function() {
  if (knitr::is_latex_output()) {
    knitr::asis_output("\\newpage")
  } else {
    knitr::asis_output('<w:p><w:r><w:br w:type="page"/></w:r></w:p>')
  }
}


mt_frases <- matrix(c(
  "aumentó en ",          "disminuyó en ",             "se mantuvo sin variación ",     "no presenta cambios significativos, con una variación de ",    #Usado
  "un aumento de ",       "una disminución de ",       "nula variación ",               "una variación poco significativa, de ",     #Usado
  "aumentaron en ",       "disminuyeron en ",          "se mantuvieron sin variación ", "no presenta cambios significativos, con una variación de ",  #Usado
  "se han creado ",       "se han destruido ",         "no hubo cambios ",              "el cambio fue menor, alcanzando ",
  "se incrementó en ",    "se redujo en ",             "se mantuvo estable ",           "presenta una variación acotada de ",        #Usado
  "registró un alza de ", "registró una caída de ",    "no presentó variaciones ",      "mostró cambios marginales de ",      #Usado
  "se crearon ",          "se perdieron ",             "no hubo cambios ",              "el cambio fue menor, alcanzando ",
  "el alza",              "la caida ",                 "no hubo alzas ",                "el alza fue menor, alcanzando ",
  "anotó un aumento de ", "anotó una disminución de ", "permaneció sin cambios ",       "evidenció una variación leve de "
), nrow = 9, byrow = TRUE)

fc_frase_cambio <- function(numero, decimal, unidad = "pp.", tol = 1e-2,estilo = 1) {
  numero2 <- fc_Form_Num(numero, decimal)
  dplyr::case_when(
    numero >  tol ~ paste0(mt_frases[estilo, 1], numero2, " ", unidad),
    numero < -tol ~ paste0(mt_frases[estilo, 2], str_remove(numero2, "-"), " ", unidad),
    numero == 0   ~ mt_frases[estilo, 3],
    TRUE          ~ paste0(mt_frases[estilo, 4], numero2, " ", unidad)
  )
}

fc_Form_Num <- function(numero, decimal) {
  format(round(numero, decimal), big.mark = ".", decimal.mark = ",")
}

fc_Datos_ENE <- function(fecha, sexo, categoria) {
  dt_ENE_sexo %>%
    dplyr::filter(fecha == !!fecha, sexo == !!sexo, categoria == !!categoria) %>%
    dplyr::mutate(valor = round(as.numeric(valor), 1)) %>%
    dplyr::pull(valor)
}

fc_Datos_ENE <- function(fecha, sexo, categoria) {
  dt_ENE_sexo %>%
    dplyr::filter(.data$fecha == !!fecha, .data$sexo == !!sexo, 
                  .data$categoria == !!categoria) %>%
    dplyr::pull(valor) %>%
    as.numeric() %>%
    round(1)
}

fc_Delta <- function(fecha_comp, sexo, categoria, fecha_base = fecha_Actual) {
  fc_Datos_ENE(fecha_base, sexo, categoria) - fc_Datos_ENE(fecha_comp, sexo, categoria)
}

fc_Brecha_ENE <- function(fecha_comp, categoria) {
  fc_Datos_ENE(fecha_comp, "H", categoria) - fc_Datos_ENE(fecha_comp, "M", categoria)
}

fc_Form_Estandar <- function(bbdd, formato_columnas,
                             fuente = "Fuente: Elaboración propia en base a datos ENE.",
                             encabezado = "tablas varias") {
  col_cat <- names(bbdd)[1]
  if (all(c("Mujeres","Hombres","Total") %in% names(bbdd))) {
    fila_total <- bbdd %>%
      summarise(!!col_cat := "Total razones",
                Mujeres = sum(.data$Mujeres, na.rm = TRUE),
                Hombres = sum(.data$Hombres, na.rm = TRUE),
                Total   = sum(.data$Total,   na.rm = TRUE)) %>%
      mutate(Femenización = ifelse(Total > 0, round(100 * Mujeres / Total, 1), NA_real_))
    for (col in names(bbdd)) if (!col %in% names(fila_total)) fila_total[[col]] <- NA
    bbdd <- bind_rows(bbdd, fila_total[, names(bbdd)])
  }
  bbdd <- bbdd %>% mutate(across(where(is.numeric), ~ round(.x, 1)))
  ft <- flextable(bbdd) %>%
    autofit() %>%
    fontsize(size = 7.5, part = "all") %>%
    padding(padding.top = 1, padding.bottom = 1)
  for (j in seq_along(formato_columnas)) {
    digits <- ifelse(formato_columnas[j] == "decimal", 1,
                     ifelse(formato_columnas[j] == "entero", 0, NA))
    if (!is.na(digits))
      ft <- colformat_num(ft, j = j, big.mark = ".", decimal.mark = ",", digits = digits)
  }
  if ("Femenización" %in% names(bbdd)) {
    pal <- scales::col_numeric(
      palette = scales::alpha(c("#1a9850","#fee08b","#d73027"), 0.4),
      domain  = bbdd$Femenización)
    ft <- bg(ft, j = "Femenización", bg = pal(bbdd$Femenización))
  }
  ft %>%
    add_footer_lines(values = fuente) %>%
    fontsize(size = 7, part = "footer") %>%
    italic(part = "footer") %>%
    color(color = "gray40", part = "footer")
  ft  %>%
    add_header_lines(values = encabezado)
}


fc_Form_Resumen <- function(bbdd, vect_decimales = NULL,
                            fuente = "Fuente: Elaboración propia en base a datos ENE.",
                            encabezado = "tablas varias") {
  num_cols <- which(sapply(bbdd, is.numeric))
  bbdd <- bbdd %>% mutate(across(where(is.numeric), ~ round(.x, 1)))
  col_dif <- names(bbdd)[which(names(bbdd) %in% c("Diferencia","Cambio","Variación"))[1]]
  if (is.na(col_dif)) col_dif <- names(bbdd)[4]
  valores_dif <- bbdd[[col_dif]]
  bbdd$flecha_aux <- ifelse(valores_dif > 0, "▲", ifelse(valores_dif < 0, "▼", ""))
  ft <- flextable(bbdd) %>%
    autofit() %>%
    fontsize(size = 7.5, part = "all") %>%
    padding(padding.top = 1, padding.bottom = 1)
  if (!is.null(vect_decimales)) {
    if (all(vect_decimales %in% c(0,1)) && length(vect_decimales) == nrow(bbdd)) {
      filas_dec <- which(vect_decimales == 1)
      filas_ent <- which(vect_decimales == 0)
    } else {
      filas_dec <- vect_decimales
      filas_ent <- setdiff(seq_len(nrow(bbdd)), vect_decimales)
    }
    if (length(filas_dec) > 0)
      ft <- colformat_num(ft, j = num_cols, i = filas_dec,
                          big.mark = ".", decimal.mark = ",", digits = 1)
    if (length(filas_ent) > 0)
      ft <- colformat_num(ft, j = num_cols, i = filas_ent,
                          big.mark = ".", decimal.mark = ",", digits = 0)
  }
  if (!is.null(vect_decimales) && length(vect_decimales) == nrow(bbdd)) {
    fmt_0 <- formatC(valores_dif, format = "f", digits = 0, decimal.mark = ",", big.mark = ".")
    fmt_1 <- formatC(valores_dif, format = "f", digits = 1, decimal.mark = ",", big.mark = ".")
    valores_fmt <- ifelse(vect_decimales == 0, fmt_0, fmt_1)
  } else {
    valores_fmt <- formatC(valores_dif, format = "f", digits = 1, decimal.mark = ",", big.mark = ".")
  }
  colores <- ifelse(valores_dif > 0, "green", ifelse(valores_dif < 0, "red", "black"))
  for (i in seq_len(nrow(bbdd))) {
    ft <- compose(ft, i = i, j = col_dif,
                  value = as_paragraph(
                    as_chunk(bbdd$flecha_aux[i], props = officer::fp_text(color = colores[i])),
                    as_chunk(" "),
                    as_chunk(valores_fmt[i])
                  ))
  }
  ft <- align(ft, j = num_cols, align = "right", part = "all")
  ft <- delete_columns(ft, j = "flecha_aux")
  ft %>%
    add_footer_lines(values = fuente) %>%
    fontsize(size = 7, part = "footer") %>%
    italic(part = "footer") %>%
    color(color = "gray40", part = "footer")
  ft  %>%
    add_header_lines(values = encabezado)
}

## Función genera predicciones ####

fc_generar_predicciones <- function(INEfin, columna_y, tramos, cortes) {
  predicciones <- list()
  for (i in seq_len(nrow(cortes))) {
    inicio <- cortes$inicio[i]
    fin <- cortes$fin[i]
    INE_corte <- INEfin %>%
      filter(fecha >= inicio & fecha <= fin) %>%
      mutate(fecha_num = as.numeric(fecha))
    
    pred_corte <- INE_corte %>%
      group_by(!!sym(tramos)) %>%
      reframe(
        {
          # Se usa cur_data() para obtener el set de datos del grupo actual,
          # asegurando que las columnas tengan la misma longitud para el modelo.
          model_data <- pick(everything()) #model_data <- cur_data()
          
          # 1. Ajustar el modelo lineal con los datos originales del grupo.
          modelo <- lm(as.formula(paste0("`", columna_y, "` ~ fecha_num")), data = model_data)
          
          # 2. Crear una secuencia completa de fechas para la predicción.
          new_fechas <- seq(min(model_data$fecha), max(model_data$fecha), by = "month")
          newdata_df <- data.frame(fecha_num = as.numeric(new_fechas))
          
          # 3. Predecir sobre el nuevo set de datos y devolver un tibble.
          tibble(
            fecha = new_fechas,
            prediccion = predict(modelo, newdata = newdata_df)
          )
        }
      )
    pred_corte$corte <- paste0("del ", format(as.Date(inicio), "%Y-%m"), " al ", format(as.Date(fin), "%Y-%m"), " ")
    predicciones[[i]] <- pred_corte
  }
  bind_rows(predicciones)
}

# Lee Coyuntural Ampliado del Mes ####

fc_val_coyuntural <- function(bbdd, sexo_label, categoria) {
  bbdd %>%
    filter(
      tipo_eje == "Region",
      valor_eje == "Total nacional",
      sexo_label == !!sexo_label,
      categoria == !!categoria
    ) %>%
    pull(valor) %>%
    as.numeric()
}

## Define función que da formato a tabla del gráfico ####

fc_crear_tema_tabla <- function(tabla) {
  ttheme_gtlight(
    base_size = 6,  # tamaño general de texto
    padding = unit(c(3, 3), "mm"),  # espacio en celdas
    core = list(
      bg_params = list(
        fill = c(rep("#ABEBC6", nrow(tabla)), "#e8f8f5"),
        alpha = 0.7
      ),
      fg_params = list(
        col = "#003366",
        fontface = c(rep(2, nrow(tabla)), 1),
        cex = 1.2
      )
    ),
    colhead = list(
      bg_params = list(fill = "#3498db", alpha = 0.8),
      fg_params = list(col = "white", fontface = 2, cex = 1.2)
    ),
    rowhead = list(
      bg_params = list(fill = "#ABEBC6", alpha = 0.7),
      fg_params = list(col = "white", fontface = 2, cex = 1.1)
    )
  )
}

fc_crear_tema_tabla <- function(tabla) {
  ttheme_gtlight(
    base_size = 6,
    padding   = unit(c(3, 3), "mm"),
    core = list(
      bg_params = list(
        fill  = c(rep("#D5F5E3", nrow(tabla)), "#A9DFBF"),
        alpha = 0.9
      ),
      fg_params = list(
        col      = "#1a5276",
        fontface = c(rep(1, nrow(tabla)), 2),
        cex      = 1.2
      )
    ),
    colhead = list(
      bg_params = list(fill = "#1a5276", alpha = 1),
      fg_params = list(col = "white", fontface = 2, cex = 1.2)
    ),
    rowhead = list(
      bg_params = list(fill = "#A9DFBF", alpha = 0.9),
      fg_params = list(col = "#1a5276", fontface = 2, cex = 1.1)
    )
  )
}
