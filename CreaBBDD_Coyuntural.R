source("CreaBBDD_Series.R")

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

if (!exists("Trim_Actual") || is.null(Trim_Actual)) {
  Trim_Actual <- "202603"
}

# 2. BBDD de Corte Transversal de doble Entrada ENE ####

## Función de lectura y limpieza ####

leer_Excel <- function(hoja) {
  tabla <- read_excel(archivo_Excel, sheet = hoja, skip = 5, .name_repair = "minimal")
  tabla <- tabla[filas_excel, columnas_excel]
  names(tabla) <- c("valor_eje", Vt_Campos)
  tabla %>%
    mutate(tipo_eje = Var_Pivot,
           sexo     = toupper(hoja))
}

## Función general de procesamiento coyuntural ####

procesar_coyuntural <- function(archivo, hojas, var_pivot, filas_excel, columnas_excel,
                                campos_df, escala = "miles") {
  archivo_Excel <<- archivo
  Var_Pivot     <<- var_pivot
  Vt_Campos     <<- campos_df$categoria
  filas_excel <<- filas_excel
  columnas_excel <<- columnas_excel
  
  hojas %>%
    lapply(leer_Excel) %>%
    bind_rows() %>%
    pivot_longer(
      cols      = all_of(campos_df$categoria),
      names_to  = "categoria",
      values_to = "valor"
    ) %>%
    mutate(valor = case_when(
      escala == "miles"      ~ round(as.numeric(valor) * 1000, 0),
      escala == "tasa_mixta" ~ if_else(
        campos_df$tipo_categoria[match(categoria, campos_df$categoria)] != "Tasa",
        round(as.numeric(valor) * 1000, 0),
        round(as.numeric(valor), 1)
      ),
      TRUE ~ as.numeric(valor)
    )) %>%
    inner_join(campos_df, by = "categoria") %>%
    select(tipo_eje, valor_eje, sexo, tipo_categoria,
           valor_categoria, categoria, valor, dplyr::everything())
}

hojas_coy <- c("as", "m", "h")

## 2.1 Sector Económico x Región ####

dt_Coyuntural_Rama <- procesar_coyuntural(
  archivo   = paste0("BBDD/", Trim_Actual, "/trimestre_movil/coyuntural_rama.xlsx"),
  hojas     = hojas_coy,
  var_pivot = "Region",
  filas_excel = 2:18,
  columnas_excel = c(1, 4:25),
  campos_df = tibble::tibble(
    categoria       = c(
      "Agricultura, ganadería, silvicultura y pesca",
      "Explotación de minas y canteras",
      "Industrias manufactureras",
      "Suministro de electricidad, gas, vapor y aire acondicionado",
      "Suministro de agua", "Construcción",
      "Comercio al por mayor y al por menor",
      "Transporte y almacenamiento",
      "Actividades de alojamiento y de servicio de comidas",
      "Información y comunicaciones",
      "Actividades financieras y de seguros",
      "Actividades inmobiliarias",
      "Actividades profesionales, científicas y técnicas",
      "Actividades de servicios administrativos y de apoyo",
      "Administración pública y defensa", "Enseñanza",
      "Actividades de atención de la salud humana y de asistencia social",
      "Actividades artísticas, de entretenimiento y recreativas",
      "Otras actividades de servicios",
      "Actividades de los hogares como empleadores",
      "Actividades de organizaciones y órganos extraterritoriales",
      "No sabe - No responde"
    ),
    tipo_categoria  = rep("ocupados", 22),
    valor_categoria = rep("rama", 22)
  )
)

## 2.2 Categoría Ocupacional x Región ####

dt_Coyuntural_Categoria <- procesar_coyuntural(
  archivo   = paste0("BBDD/", Trim_Actual, "/trimestre_movil/coyuntural_categoria.xlsx"),
  hojas     = hojas_coy,
  var_pivot = "Region",
  filas_excel = 2:18,
  columnas_excel = c(1, 4:9),
  campos_df = tibble::tibble(
    categoria       = c("Empleadores","Cuenta propia","Asalariados privados",
                        "Asalariados público","Servicio doméstico","Familiares no rem"),
    tipo_categoria  = rep("ocupados", 6),
    valor_categoria = rep("categoria", 6)
  )
)

## 2.3 Mercado Laboral x Edad ####

dt_Coyuntural_Edad <- procesar_coyuntural(
  archivo   = paste0("BBDD/", Trim_Actual, "/trimestre_movil/coyuntural_sft_edad.xlsx"),
  hojas     = hojas_coy,
  var_pivot = "Grupo_Edad",
  filas_excel = 2:14,
  columnas_excel = c(1, 3:18),
  campos_df = tibble::tibble(
    categoria       = c(
      "PET","Fuerza de Trabajo","Ocupados","Ocupados formales","Ocupados informales",
      "Desocupados","Cesantes","B_t_p_v","FFT","FFT Iniciadores",
      "FFT potencialmente activos","FFT habituales",
      "Tasa de desocupación","Tasa de ocupación","Tasa de participación",
      "Tasa de ocupación informal"
    ),
    tipo_categoria  = c(rep("MT", 8), rep("FFT", 4), rep("Tasa", 4)),
    valor_categoria = rep("Edad", 16)
  ),
  escala = "tasa_mixta"
)

## 2.4.1 Mercado Laboral y FFT x Región ####

dt_Coyuntural_sft <- procesar_coyuntural(
  archivo   = paste0("BBDD/", Trim_Actual, "/trimestre_movil/coyuntural_sft.xlsx"),
  hojas     = hojas_coy,
  var_pivot = "Region",
  filas_excel = 2:18,
  columnas_excel = c(1, 3:10, 12:13),
  campos_df = tibble::tibble(
    categoria       = c(
      "Población","PET","FT","Ocupados","Desocupados","Cesantes",
      "Buscan trabajo por primera vez","FFT",
      "FFT Inactivos potencialmente activos","FFT Inactivos habituales"
    ),
    tipo_categoria  = c(rep("MT ampliado", 7), rep("fft reducido", 3)),
    valor_categoria = rep("total", 10)
  ),
  escala = "raw"
)

## 2.4.2 Ocupados y FFT Ampliada x Región ####

dt_Coyuntural_sft_desag <- procesar_coyuntural(
  archivo   = paste0("BBDD/", Trim_Actual, "/trimestre_movil/coyuntural_sft_desagregado.xlsx"),
  hojas     = hojas_coy,
  var_pivot = "Region",
  filas_excel = 2:18,
  columnas_excel = c(1, 7:13, 18:28),
  campos_df = tibble::tibble(
    categoria       = c(
      "Ocupados presentes",
      "Ocupados presentes tradicionales",
      "Ocupados presentes no tradicionales",
      "Ocupados ausentes",
      "Ocupados ausentes con vínculo efectivo",
      "Ocupados ausentes con pronto retorno",
      "Ocupados ausentes con sueldo o ganancias",
      "FFT Iniciadores",
      "FFT Razones familiares permanentes",
      "FFT Razones de estudio",
      "FFT Razones de jubilación",
      "FFT Razones de pensión o montepiado",
      "FFT Razones de salud permanentes",
      "FFT Razones personales temporales",
      "FFT Sin deseos de trabajar",
      "FFT Razones estacionales",
      "FFT Razones de desaliento",
      "FFT Otras razones"
    ),
    tipo_categoria  = c(rep("ocupados ampliado", 7), rep("fft ampliada", 11)),
    valor_categoria = rep("total", 18)
  ),
  escala = "raw"
)

## 2.5 Integración final ####

# Combinar 2.4.1 y 2.4.2, escalar a miles y agregar fecha/sexo_label
dt_Coyuntural_sft_total <- bind_rows(dt_Coyuntural_sft, dt_Coyuntural_sft_desag) %>%
  mutate(
    valor = round(valor * 1000, 0),
    fecha = make_date(
      year  = as.numeric(str_sub(Trim_Actual, 1, 4)),
      month = as.numeric(str_sub(Trim_Actual, 5, 6)),
      day   = 1
    ),
    sexo_label = factor(
      sexo,
      levels = c("H", "M", "AS"),
      labels = c("Hombres", "Mujeres", "Ambos sexos")
    )
  ) %>%
  select(fecha, tipo_eje, valor_eje, sexo, sexo_label,
         tipo_categoria, valor_categoria, categoria, valor,
         dplyr::everything())

## 2.5 Integración final ####

tabla_regiones <- data.frame(
  larga = c(
    "Región de Arica y Parinacota","Región de Tarapacá","Región de Antofagasta",
    "Región de Atacama","Región de Coquimbo","Región de Valparaíso",
    "Región Metropolitana de Santiago",
    "Región del Libertador General Bernardo O'Higgins",
    "Región del Maule","Región del Ñuble","Región del Biobío",
    "Región de La Araucanía","Región de Los Ríos","Región de Los Lagos",
    "Región de Aysén del General Carlos Ibáñez del Campo",
    "Región de Magallanes y de la Antártica Chilena"
  ),
  corta = c(
    "Arica y Parinacota","Tarapacá","Antofagasta","Atacama","Coquimbo",
    "Valparaíso","Metropolitana","O'Higgins","Maule","Ñuble","Biobío",
    "La Araucanía","Los Ríos","Los Lagos","Aysén","Magallanes"
  ),
  stringsAsFactors = FALSE
)

dt_Coyuntural <- bind_rows(
  dt_Coyuntural_Rama,
  dt_Coyuntural_Categoria,
  dt_Coyuntural_Edad,
  dt_Coyuntural_sft_total
) %>%
  mutate(
    fecha = make_date(
      year  = as.numeric(str_sub(Trim_Actual, 1, 4)),
      month = as.numeric(str_sub(Trim_Actual, 5, 6)),
      day   = 1
    ),
    sexo_label = factor(sexo, levels = c("H","M","AS"),
                        labels = c("Hombres","Mujeres","Ambos sexos"))
  ) %>%
  left_join(tabla_regiones, by = c("valor_eje" = "larga")) %>%
  mutate(valor_eje = coalesce(corta, valor_eje)) %>%
  select(fecha, tipo_eje, valor_eje, sexo, sexo_label,
         tipo_categoria, valor_categoria, categoria, valor,
         dplyr::everything(), -corta)

# 3. Graba Dataset de salida en BBDD ####

save(dt_ENE_sexo, dt_ENE_region, dt_Coyuntural,
     file = paste0("BBDD/", Trim_Actual, "/BBDD_Estadisticas.RData"))

rm(list = ls()[!grepl("^dt_", ls()) & ls() != "Trim_Actual"])
