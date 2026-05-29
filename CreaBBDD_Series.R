library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# Funciones y parametros globales ####

options(scipen = 999)
if (!exists("Trim_Actual") || is.null(Trim_Actual)) {
  Trim_Actual <- "202603"
}

## Función auxiliar: convierte etiqueta de trimestre móvil a mes ####

trimestre_a_mes <- function(t) {
  case_when(
    t == "Ene - Mar" ~ 2,
    t == "Feb - Abr" ~ 3,
    t == "Mar - May" ~ 4,
    t == "Abr - Jun" ~ 5,
    t == "May - Jul" ~ 6,
    t == "Jun - Ago" ~ 7,
    t == "Jul - Sep" ~ 8,
    t == "Ago - Oct" ~ 9,
    t == "Sep - Nov" ~ 10,
    t == "Oct - Dic" ~ 11,
    t == "Nov - Ene" ~ 12,
    t == "Dic - Feb" ~ 1,
    TRUE ~ NA_real_
  )
}

# ------------------------------------------------------------------
# Función general: lee un Excel de series ENE (una hoja = un desagregado)
#   archivo      : ruta al .xlsx
#   hojas        : vector de nombres de hojas
#   vt_names_raw : nombres a asignar a TODAS las columnas del Excel
#                  (incluye los placeholders tipo "2","a","nota1", etc.)
#   vt_names_fin : columnas finales a conservar (orden de salida)
#   cols_num     : rango tidy para mutate(across(...)) — e.g. Col1:ColN
#                  se pasa como string y se evalúa con across(Col1:ColN)
# ------------------------------------------------------------------
leer_serie <- function(archivo, hojas, vt_names_raw, vt_names_fin) {
  
  lapply(hojas, function(i) {
    dt <- read_excel(archivo, sheet = i, skip = 5)
    dt <- dt[-1, ] %>% filter(!is.na(.[[2]]))
    names(dt) <- vt_names_raw
    dt %>% mutate(
      mes   = trimestre_a_mes(Trimestre),
      sexo  = toupper(i),
      fecha = make_date(year = Año, month = mes, day = 1)
    )
  }) %>%
    bind_rows() %>%
    select(all_of(vt_names_fin)) %>%
    mutate(across(
      -c(Año, mes, Trimestre, fecha, sexo),
      ~ suppressWarnings(as.numeric(str_replace_all(as.character(.), "[^0-9.,-]", "")))
    ))
}

# Hojas comunes para la mayoría de los archivos
hojas_completas <- c("AS","M","H","AP","TA","AN","AT","CO","VA","RM","LI","ML","NB","BI","AR","LR","LL","AI","MA")


# 1. Series temporales Estadigrafos ENE ####
## 1.1 Lectura y procesamiento de las series ####

### 1.1.1 Indicadores principales ####

dt_principal <- leer_serie(
  archivo      = paste0("BBDD/", Trim_Actual, "/series_ene/indicadores_principales.xlsx"),
  hojas        = hojas_completas,
  vt_names_raw = c(
    "Año","Trimestre",
    "p2","PET","p4","FT","p6","Ocupados","p8","Desocupados",
    "p10","Cesantes","p12","B_t_p_v","p14","FFT",
    "p16","FFT_Iniciadores","p18","FFT_Inactivos_PA",
    "p20","FFT_Inactivos_H","p22","T_desocupación",
    "p24","T_ocupación","p26","T_participación"
  ),
  vt_names_fin = c(
    "Año","Trimestre","mes","fecha","sexo",
    "PET","FT","Ocupados","Desocupados","Cesantes","B_t_p_v",
    "FFT","FFT_Iniciadores","FFT_Inactivos_PA","FFT_Inactivos_H",
    "T_desocupación","T_ocupación","T_participación"
  )
)

### 1.1.2 Indicadores Complementarios ####

dt_complementarios <- leer_serie(
  archivo      = paste0("BBDD/", Trim_Actual, "/series_ene/complementarios.xlsx"),
  hojas        = hojas_completas,
  vt_names_raw = c(
    "Año","Trimestre",
    "a","Fuerza de trabajo","b","Ocupados","c","Desocupados",
    "d","Iniciadoras disponibles","e","Tiempo parcial involuntario",
    "f","Ocupados que buscaron empleo","g","Fuerza de trabajo potencial",
    "h","Fuerza de trabajo ampliada","z","Desea trabajar",
    "j","Tasa de desocupación","k","Tasa presión laboral",
    "l","Tasa desocupación + ID (SU1)","m","Tasa desocupación + TPI (SU2)",
    "n","Tasa desocupación + FTP (SU3)","o","Tasa subutilización (SU4)"
  ),
  vt_names_fin = c(
    "Año","Trimestre","mes","fecha","sexo",
    "Iniciadoras disponibles","Tiempo parcial involuntario",
    "Ocupados que buscaron empleo","Fuerza de trabajo potencial",
    "Fuerza de trabajo ampliada","Desea trabajar","Tasa presión laboral",
    "Tasa desocupación + ID (SU1)","Tasa desocupación + TPI (SU2)",
    "Tasa desocupación + FTP (SU3)","Tasa subutilización (SU4)"
  )
)

### 1.1.3 Grupos Ocupacionales ####

dt_grupos <- leer_serie(
  archivo      = paste0("BBDD/", Trim_Actual, "/series_ene/grupo.xlsx"),
  hojas        = hojas_completas,
  vt_names_raw = c(
    "Año","Trimestre",
    "a","Población ocupada",
    "b","Directores, gerentes y administradores",
    "c","Profesionales, científicos e intelectuales",
    "d","Técnicos y profesionales de nivel medio",
    "e","Personal de apoyo administrativo",
    "f","Trabajadores de los servicios y vendedores de comercios y mercados",
    "g","Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros",
    "h","Artesanos y operarios de oficios",
    "z","Operadores de instalaciones, maquinas y ensambladores",
    "j","Ocupaciones elementales",
    "k","Otros no identificados",
    "l","No sabe - No responde"
  ),
  vt_names_fin = c(
    "Año","Trimestre","mes","fecha","sexo",
    "Directores, gerentes y administradores",
    "Profesionales, científicos e intelectuales",
    "Técnicos y profesionales de nivel medio",
    "Personal de apoyo administrativo",
    "Trabajadores de los servicios y vendedores de comercios y mercados",
    "Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros",
    "Artesanos y operarios de oficios",
    "Operadores de instalaciones, maquinas y ensambladores",
    "Ocupaciones elementales"
  )
)

### 1.1.4 Horas Laborales ####

dt_horas <- leer_serie(
  archivo      = paste0("BBDD/", Trim_Actual, "/series_ene/horas.xlsx"),
  hojas        = c("AS","M","H"),
  vt_names_raw = c(
    "Año","Trimestre",
    "a","Población ocupada",
    "b","Personas trabajaron 1 - 30 horas habituales",
    "c","Personas tiempo parcial voluntario (TPV)",
    "d","Personas tiempo parcial involuntario (TPI)",
    "e","Personas a tiempo parcial S-I voluntariedad",
    "f","Personas trabajaron 31 - 44 horas habituales",
    "g","Personas trabajaron 45 horas habituales",
    "h","Personas trabajaron más de 45 horas habituales",
    "z","Personas trabajaron más de 45 horas efectivas",
    "j","Personas declararon horas trabajadas",
    "k","Promedio horas efectivas a la semana (con ocupados ausentes)",
    "l","Promedio horas efectivas a la semana (sin ocupados ausentes)",
    "m","Promedio horas habitualmente a la semana"
  ),
  vt_names_fin = c(
    "Año","Trimestre","mes","fecha","sexo",
    "Personas trabajaron 1 - 30 horas habituales",
    "Personas tiempo parcial voluntario (TPV)",
    "Personas tiempo parcial involuntario (TPI)",
    "Personas a tiempo parcial S-I voluntariedad",
    "Personas trabajaron 31 - 44 horas habituales",
    "Personas trabajaron 45 horas habituales",
    "Personas trabajaron más de 45 horas habituales",
    "Personas trabajaron más de 45 horas efectivas",
    "Personas declararon horas trabajadas",
    "Promedio horas efectivas a la semana (con ocupados ausentes)",
    "Promedio horas efectivas a la semana (sin ocupados ausentes)",
    "Promedio horas habitualmente a la semana"
  )
)

### 1.1.5 Categoría Ocupacionales ####

dt_categoria <- leer_serie(
  archivo      = paste0("BBDD/", Trim_Actual, "/series_ene/categoria.xlsx"),
  hojas        = hojas_completas,
  vt_names_raw = c(
    "Año","Trimestre",
    "p2","ocupados","p4","Independientes","p6","Empleadores",
    "p8","Cuenta propia","p10","Familiares no rem","p12","Dependientes",
    "p14","Asalariados","p16","Asalariados privados","p18","Asalariados público",
    "p20","Servicio doméstico","p22","Servicio doméstico afuera","p24","Servicio doméstico Adentro"
  ),
  vt_names_fin = c(
    "Año","Trimestre","mes","fecha","sexo",
    "Independientes","Empleadores","Cuenta propia","Familiares no rem",
    "Dependientes","Asalariados","Asalariados privados","Asalariados público",
    "Servicio doméstico","Servicio doméstico afuera","Servicio doméstico Adentro"
  )
)

### 1.1.6 Sector Economico ####

dt_rama <- leer_serie(
  archivo      = paste0("BBDD/", Trim_Actual, "/series_ene/rama.xlsx"),
  hojas        = hojas_completas,
  vt_names_raw = c(
    "Año","Trimestre",
    "a","OcupadosR",
    "b","Agricultura, ganadería, silvicultura y pesca",
    "c","Explotación de minas y canteras",
    "d","Industrias manufactureras",
    "e","Suministro de electricidad, gas, vapor y aire acondicionado",
    "f","Suministro de agua",
    "g","Construcción",
    "h","Comercio al por mayor y al por menor",
    "z","Transporte y almacenamiento",
    "j","Actividades de alojamiento y de servicio de comidas",
    "k","Información y comunicaciones",
    "l","Actividades financieras y de seguros",
    "m","Actividades inmobiliarias",
    "n","Actividades profesionales, científicas y técnicas",
    "o","Actividades de servicios administrativos y de apoyo",
    "p","Administración pública y defensa",
    "q","Enseñanza",
    "r","Actividades de atención de la salud humana y de asistencia social",
    "s","Actividades artísticas, de entretenimiento y recreativas",
    "t","Otras actividades de servicios",
    "u","Actividades de los hogares como empleadores",
    "v","Actividades de organizaciones y órganos extraterritoriales",
    "w","No sabe - No responde"
  ),
  vt_names_fin = c(
    "Año","Trimestre","mes","fecha","sexo",
    "Agricultura, ganadería, silvicultura y pesca",
    "Explotación de minas y canteras",
    "Industrias manufactureras",
    "Suministro de electricidad, gas, vapor y aire acondicionado",
    "Suministro de agua","Construcción",
    "Comercio al por mayor y al por menor",
    "Transporte y almacenamiento",
    "Actividades de alojamiento y de servicio de comidas",
    "Información y comunicaciones",
    "Actividades financieras y de seguros",
    "Actividades inmobiliarias",
    "Actividades profesionales, científicas y técnicas",
    "Actividades de servicios administrativos y de apoyo",
    "Administración pública y defensa","Enseñanza",
    "Actividades de atención de la salud humana y de asistencia social",
    "Actividades artísticas, de entretenimiento y recreativas",
    "Otras actividades de servicios",
    "Actividades de los hogares como empleadores",
    "Actividades de organizaciones y órganos extraterritoriales"
  )
)

### 1.1.7 Series Desestacionalizadas ####
# Lógica distinta: combina dos hojas horizontalmente (tasas + niveles)

hojas_d <- c("tasa_as","tasa_h","tasa_m","niveles_as","niveles_h","niveles_m")
sexos_d  <- c("AS","H","M")

dt_Desest <- lapply(1:3, function(i) {
  dt1 <- read_excel(paste0("BBDD/", Trim_Actual, "/series_ene/ajuste_estacional_historico.xlsx"),
                    sheet = hojas_d[i], skip = 5)
  dt2 <- read_excel(paste0("BBDD/", Trim_Actual, "/series_ene/ajuste_estacional_historico.xlsx"),
                    sheet = hojas_d[i + 3], skip = 5)
  dt1 <- dt1[, c(1, 2, ncol(dt1))]
  dt2 <- dt2[, (ncol(dt2) - 2):ncol(dt2)]
  dt  <- cbind(dt1, dt2)[-1, ]
  dt  <- dt %>% filter(!is.na(.[[2]]))
  names(dt) <- c("Año","Trimestre","T_desocupación_d","FT_d","Ocupados_d","Desocupados_d")
  dt %>% mutate(
    mes   = trimestre_a_mes(Trimestre),
    sexo  = sexos_d[i],
    fecha = make_date(year = Año, month = mes, day = 1)
  )
}) %>%
  bind_rows() %>%
  select(Año, Trimestre, mes, fecha, sexo, T_desocupación_d, FT_d, Ocupados_d, Desocupados_d) %>%
  mutate(across(T_desocupación_d:Desocupados_d,
                ~ suppressWarnings(as.numeric(str_replace_all(as.character(.), "[^0-9.,-]", "")))))

### 1.1.8 Ocupados Ausentes ####

dt_ausentes <- leer_serie(
  archivo      = paste0("BBDD/", Trim_Actual, "/series_ene/ocupados_ausentes.xlsx"),
  hojas        = c("nacional"),
  vt_names_raw = c(
    "Año","Trimestre",
    "n1","ocupadas","n2","ocupadas presentes","n3","ocupadas ausentes",
    "n4","ocupadas ausentes vinculo efectivo",
    "n5","ocupadas ausentes pronto retorno",
    "n6","ocupadas ausentes sueldo o ganancia"
  ),
  vt_names_fin = c(
    "Año","Trimestre","mes","fecha","sexo",
    "ocupadas","ocupadas presentes","ocupadas ausentes",
    "ocupadas ausentes vinculo efectivo",
    "ocupadas ausentes pronto retorno",
    "ocupadas ausentes sueldo o ganancia"
  )
) %>%
  mutate(sexo = "AS")   # hoja "nacional" no codifica sexo, se fija manualmente

### 1.1.9 Informalidad Categoría ####

dt_Form_Categoria <- leer_serie(
  archivo      = paste0("BBDD/", Trim_Actual, "/series_informalidad/informalidad_categoria.xlsx"),
  hojas        = hojas_completas,
  vt_names_raw = c(
    "Año","Trimestre",
    "paso","Ocupados formales",
    "1","Empleadoras formales","2","Cuenta propia formales",
    "3","Asalariados privado formales","4","Asalariados público formales",
    "5","Servicio doméstico formales",
    "6","Ocupados informales",
    "7","Empleadoras informales","8","Cuenta propia informales",
    "12","Familiar no rem informales",
    "9","Asalariados privado informales","10","Asalariados público informales",
    "11","Servicio doméstico informales"
  ),
  vt_names_fin = c(
    "Año","Trimestre","mes","fecha","sexo",
    "Ocupados formales","Empleadoras formales","Cuenta propia formales",
    "Asalariados privado formales","Asalariados público formales",
    "Servicio doméstico formales","Ocupados informales",
    "Empleadoras informales","Cuenta propia informales",
    "Familiar no rem informales","Asalariados privado informales",
    "Asalariados público informales","Servicio doméstico informales"
  )
)

### 1.1.10 Informalidad Sector Económico ####

dt_form_rama <- leer_serie(
  archivo      = paste0("BBDD/", Trim_Actual, "/series_informalidad/informalidad_rama.xlsx"),
  hojas        = hojas_completas,
  vt_names_raw = c(
    "Año","Trimestre",
    "a","Ocupados formal",
    "b","Agricultura, ganadería, silvicultura y pesca formal",
    "c","Explotación de minas y canteras formal",
    "d","Industrias manufactureras formal",
    "e","Suministro de electricidad, gas, vapor y aire acondicionado formal",
    "f","Suministro de agua formal","g","Construcción formal",
    "h","Comercio al por mayor y al por menor formal",
    "z2","Transporte y almacenamiento formal",
    "j","Actividades de alojamiento y de servicio de comidas formal",
    "k","Información y comunicaciones formal",
    "l","Actividades financieras y de seguros formal",
    "m","Actividades inmobiliarias formal",
    "n","Actividades profesionales, científicas y técnicas formal",
    "o","Actividades de servicios administrativos y de apoyo formal",
    "p","Administración pública y defensa formal","q","Enseñanza formal",
    "r","Actividades de atención de la salud humana y de asistencia social formal",
    "s","Actividades artísticas, de entretenimiento y recreativas formal",
    "t","Otras actividades de servicios formal",
    "u","Actividades de los hogares como empleadores formal",
    "v","Actividades de organizaciones y órganos extraterritoriales formal",
    "w","No sabe - No responde formal",
    "x","Ocupados informal",
    "y","Agricultura, ganadería, silvicultura y pesca informal",
    "z","Explotación de minas y canteras informal",
    "aa","Industrias manufactureras informal",
    "ab","Suministro de electricidad, gas, vapor y aire acondicionado informal",
    "ac","Suministro de agua informal","ad","Construcción informal",
    "ae","Comercio al por mayor y al por menor informal",
    "af","Transporte y almacenamiento informal",
    "ag","Actividades de alojamiento y de servicio de comidas informal",
    "ah","Información y comunicaciones informal",
    "az","Actividades financieras y de seguros informal",
    "aj","Actividades inmobiliarias informal",
    "ak","Actividades profesionales, científicas y técnicas informal",
    "al","Actividades de servicios administrativos y de apoyo informal",
    "am","Administración pública y defensa informal","an","Enseñanza informal",
    "ao","Actividades de atención de la salud humana y de asistencia social informal",
    "ap","Actividades artísticas, de entretenimiento y recreativas informal",
    "aq","Otras actividades de servicios informal",
    "ar","Actividades de los hogares como empleadores informal",
    "as","Actividades de organizaciones y órganos extraterritoriales informal",
    "at","No sabe - No responde informal"
  ),
  vt_names_fin = c(
    "Año","Trimestre","mes","fecha","sexo",
    "Agricultura, ganadería, silvicultura y pesca formal",
    "Explotación de minas y canteras formal",
    "Industrias manufactureras formal",
    "Suministro de electricidad, gas, vapor y aire acondicionado formal",
    "Suministro de agua formal","Construcción formal",
    "Comercio al por mayor y al por menor formal",
    "Transporte y almacenamiento formal",
    "Actividades de alojamiento y de servicio de comidas formal",
    "Información y comunicaciones formal",
    "Actividades financieras y de seguros formal",
    "Actividades inmobiliarias formal",
    "Actividades profesionales, científicas y técnicas formal",
    "Actividades de servicios administrativos y de apoyo formal",
    "Administración pública y defensa formal","Enseñanza formal",
    "Actividades de atención de la salud humana y de asistencia social formal",
    "Actividades artísticas, de entretenimiento y recreativas formal",
    "Otras actividades de servicios formal",
    "Actividades de los hogares como empleadores formal",
    "Actividades de organizaciones y órganos extraterritoriales formal",
    "Agricultura, ganadería, silvicultura y pesca informal",
    "Explotación de minas y canteras informal",
    "Industrias manufactureras informal",
    "Suministro de electricidad, gas, vapor y aire acondicionado informal",
    "Suministro de agua informal","Construcción informal",
    "Comercio al por mayor y al por menor informal",
    "Transporte y almacenamiento informal",
    "Actividades de alojamiento y de servicio de comidas informal",
    "Información y comunicaciones informal",
    "Actividades financieras y de seguros informal",
    "Actividades inmobiliarias informal",
    "Actividades profesionales, científicas y técnicas informal",
    "Actividades de servicios administrativos y de apoyo informal",
    "Administración pública y defensa informal","Enseñanza informal",
    "Actividades de atención de la salud humana y de asistencia social informal",
    "Actividades artísticas, de entretenimiento y recreativas informal",
    "Otras actividades de servicios informal",
    "Actividades de los hogares como empleadores informal",
    "Actividades de organizaciones y órganos extraterritoriales informal"
  )
)

### 1.1.11 Informalidad Grupos Ocupacionales ####

dt_form_grupos <- leer_serie(
  archivo      = paste0("BBDD/", Trim_Actual, "/series_informalidad/informalidad_grupo.xlsx"),
  hojas        = hojas_completas,
  vt_names_raw = c(
    "Año","Trimestre",
    "n1","Población ocupada formal",
    "n2","Directores, gerentes y administradores formal",
    "n3","Profesionales, científicos e intelectuales formal",
    "n4","Técnicos y profesionales de nivel medio formal",
    "n5","Personal de apoyo administrativo formal",
    "n6","Trabajadores de los servicios y vendedores de comercios y mercados formal",
    "n7","Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros formal",
    "n8","Artesanos y operarios de oficios formal",
    "n9","Operadores de instalaciones, maquinas y ensambladores formal",
    "n10","Ocupaciones elementales formal",
    "n11","Otros no identificados formal",
    "n12","No sabe - No responde formal",
    "n13","Población ocupada informal",
    "n14","Directores, gerentes y administradores informal",
    "n15","Profesionales, científicos e intelectuales informal",
    "n16","Técnicos y profesionales de nivel medio informal",
    "n17","Personal de apoyo administrativo informal",
    "n18","Trabajadores de los servicios y vendedores de comercios y mercados informal",
    "n19","Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros informal",
    "n20","Artesanos y operarios de oficios informal",
    "n21","Operadores de instalaciones, maquinas y ensambladores informal",
    "n22","Ocupaciones elementales informal",
    "n23","Otros no identificados informal",
    "n24","No sabe - No responde informal"
  ),
  vt_names_fin = c(
    "Año","Trimestre","mes","fecha","sexo",
    "Directores, gerentes y administradores formal",
    "Profesionales, científicos e intelectuales formal",
    "Técnicos y profesionales de nivel medio formal",
    "Personal de apoyo administrativo formal",
    "Trabajadores de los servicios y vendedores de comercios y mercados formal",
    "Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros formal",
    "Artesanos y operarios de oficios formal",
    "Operadores de instalaciones, maquinas y ensambladores formal",
    "Ocupaciones elementales formal",
    "Directores, gerentes y administradores informal",
    "Profesionales, científicos e intelectuales informal",
    "Técnicos y profesionales de nivel medio informal",
    "Personal de apoyo administrativo informal",
    "Trabajadores de los servicios y vendedores de comercios y mercados informal",
    "Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros informal",
    "Artesanos y operarios de oficios informal",
    "Operadores de instalaciones, maquinas y ensambladores informal",
    "Ocupaciones elementales informal"
  )
)


## 1.2 Unir BBDD integrada ####

dt_unida <- dt_categoria %>%
  left_join(dt_complementarios, by = c("Año","Trimestre","mes","fecha","sexo")) %>%
  left_join(dt_Desest,          by = c("Año","Trimestre","mes","fecha","sexo")) %>%
  left_join(dt_Form_Categoria,  by = c("Año","Trimestre","mes","fecha","sexo")) %>%
  left_join(dt_form_grupos,     by = c("Año","Trimestre","mes","fecha","sexo")) %>%
  left_join(dt_form_rama,       by = c("Año","Trimestre","mes","fecha","sexo")) %>%
  left_join(dt_grupos,          by = c("Año","Trimestre","mes","fecha","sexo")) %>%
  left_join(dt_horas,           by = c("Año","Trimestre","mes","fecha","sexo")) %>%
  left_join(dt_principal,       by = c("Año","Trimestre","mes","fecha","sexo")) %>%
  left_join(dt_rama,            by = c("Año","Trimestre","mes","fecha","sexo")) %>%
  left_join(dt_ausentes,        by = c("Año","Trimestre","mes","fecha","sexo"))

## 1.2.2 Calcular variables complementarias y factorizar la serie ####

fecha_Inicio_Covid <- as.Date("2020-01-01")

factores_base <- dt_unida %>%
  filter(fecha == fecha_Inicio_Covid) %>%
  select(sexo, `Ocupados formales`, `Ocupados informales`, PET)

dt_unida <- dt_unida %>%
  mutate(across(PET:Desocupados_d, ~ as.numeric(.))) %>%
  left_join(factores_base, by = "sexo", suffix = c("", "_base")) %>%
  mutate(across(ends_with("_base"), ~ as.numeric(.))) %>%
  mutate(
    Asalariados_dep_Form   = `Asalariados privado formales`+`Asalariados público formales`+`Servicio doméstico formales`,
    Asalariados_dep_Inform = `Asalariados privado informales`+`Asalariados público informales`+`Servicio doméstico informales`,
    Asalariados_dep    = Asalariados_dep_Form+Asalariados_dep_Inform,
    T_Informal         = ifelse(!is.na(`Ocupados informales`) & !is.na(Ocupados),
                             `Ocupados informales` / Ocupados * 100, NA_real_),
    T_Cesantia         = ifelse(!is.na(Cesantes) & !is.na(FT), Cesantes / FT * 100, NA_real_),
    T_Btrab            = ifelse(!is.na(B_t_p_v) & !is.na(FT), B_t_p_v / FT * 100, NA_real_),
    FFT_d              = ifelse(!is.na(FT_d) & !is.na(PET), PET - FT_d / 1000, NA_real_),
    Form_PET        = ifelse(!is.na(`Ocupados formales`) & !is.na(PET),
                             `Ocupados formales` / PET * 100, NA_real_),
    Inform_PET      = ifelse(!is.na(`Ocupados informales`) & !is.na(PET),
                             `Ocupados informales` / PET * 100, NA_real_),
    Ocupado_Privado = ifelse(!is.na(`Asalariados público`) & !is.na(Ocupados),
                             Ocupados - `Asalariados público`, NA_real_),
    Deficit_form    = ifelse(!is.na(`Ocupados formales`) & !is.na(PET),
                             `Ocupados formales` - (`Ocupados formales_base` / PET_base) * PET, NA_real_),
    Deficit_inform  = ifelse(!is.na(`Ocupados informales`) & !is.na(PET),
                             `Ocupados informales` - (`Ocupados informales_base` / PET_base) * PET, NA_real_)
  )

## 1.3 Construir BBDD final ####

Excluir <- c(
  "T_desocupación","T_desocupación_d","T_ocupación","T_Cesantia","T_Btrab",
  "T_Informal","T_participación","Form_PET","Inform_PET","Tasa presión laboral",
  "Tasa desocupación + ID (SU1)","Tasa desocupación + TPI (SU2)",
  "Tasa desocupación + FTP (SU3)","Tasa subutilización (SU4)",
  "FT_d","Ocupados_d","Desocupados_d",
  "Promedio horas efectivas a la semana (con ocupados ausentes)",
  "Promedio horas efectivas a la semana (sin ocupados ausentes)",
  "Promedio horas habitualmente a la semana"
)

dt_completo <- dt_unida %>%
  pivot_longer(
    cols      = c(Independientes:`Deficit_inform`),
    names_to  = "categoria",
    values_to = "valor"
  ) %>%
  filter(!is.na(valor)) %>%
  select(-Año, -Trimestre, -mes) %>%
  mutate(valor = if_else(!(categoria %in% Excluir), valor * 1000, valor))

### Separación por SEXO ####

dt_ENE_sexo <- dt_completo %>%
  filter(sexo %in% c("AS","M","H")) %>%
  mutate(
    sexo_label = case_when(
      sexo == "AS" ~ "Ambos Sexos",
      sexo == "M"  ~ "Mujeres",
      sexo == "H"  ~ "Hombres",
      TRUE ~ sexo
    )
  )

### Separación por REGIÓN ####

dt_ENE_region <- dt_completo %>%
  filter(!sexo %in% c("AS","M","H")) %>%
  mutate(
    region = sexo,
    region_label = case_when(
      sexo == "AP" ~ "Arica y Parinacota",
      sexo == "TA" ~ "Tarapacá",
      sexo == "AN" ~ "Antofagasta",
      sexo == "AT" ~ "Atacama",
      sexo == "CO" ~ "Coquimbo",
      sexo == "VA" ~ "Valparaíso",
      sexo == "RM" ~ "Metropolitana",
      sexo == "LI" ~ "O'Higgins",
      sexo == "ML" ~ "Maule",
      sexo == "NB" ~ "Ñuble",
      sexo == "BI" ~ "Biobío",
      sexo == "AR" ~ "La Araucanía",
      sexo == "LR" ~ "Los Ríos",
      sexo == "LL" ~ "Los Lagos",
      sexo == "AI" ~ "Aysén",
      sexo == "MA" ~ "Magallanes",
      TRUE ~ sexo
    )
  ) %>%
  select(-sexo)

#rm(unida, factores_base,    dt_principal, dt_Form_Categoria, dt_categoria, dt_Desest,   dt_complementarios, dt_grupos, dt_horas, dt_ausentes,   dt_rama, dt_form_grupos, dt_form_rama, dt_completo,   fecha_Inicio_Covid, Excluir, hojas_completas, hojas_d, sexos_d)
