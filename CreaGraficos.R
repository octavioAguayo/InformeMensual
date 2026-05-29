# ==============================================================================
# CreaGraficos.R
# Crea gráficos ggplot exportados al informe
# Depende de: Funciones_ENE.R, Variables_ENE.R, CreaTablas.R (datos base compartidos)
# ==============================================================================

# Librerías ####
library(openxlsx)
library(ggplot2)
library(ggrepel)
library(ggpmisc)
library(patchwork)
library(ragg)
library(scales)
library(dplyr)
library(tidyr)
library(lubridate)

# ==============================================================================
# GRÁFICO 0: Creación de empleo post-COVID ####
# ==============================================================================

base_gr1_empleo_covid <- dt_ENE_sexo %>%
  filter(
    (sexo_label == "Ambos Sexos" &
       categoria %in% c("Ocupados", "Ocupados formales", "Ocupados_d")) |
      (sexo_label == "Mujeres" & categoria == "Ocupados")
  ) %>%
  mutate(
    categoria = case_when(
      categoria == "Ocupados_d"                          ~ "Ocupados desestacionalizado",
      sexo_label == "Mujeres" & categoria == "Ocupados"  ~ "Ocupadas Mujeres",
      TRUE                                               ~ categoria
    )
  ) %>%
  group_by(sexo, categoria) %>%
  mutate(valor = as.numeric(valor) - valor[fecha == Fechas["Fin_Covid"]][1]) %>%
  ungroup() %>%
  filter(fecha >= Fechas["Fin_Covid"])

tabla_xls_gr1_empleo_covid <- base_gr1_empleo_covid %>%
  select(fecha, categoria, valor) %>%
  mutate(valor = round(valor, 0), fecha = format(fecha, "%Y-%m")) %>%
  pivot_wider(names_from = categoria, values_from = valor)

puntos_gr1_empleo <- base_gr1_empleo_covid %>%
  group_by(categoria) %>%
  filter(fecha %in% Fechas[c("actual", "a_1_anno", "a_2_annos", "a_3_annos", "a_4_annos")]) %>%
  ungroup()

tabla_interna_gr1 <- puntos_gr1_empleo %>%
  select(fecha, Categoría = categoria, valor) %>%
  mutate(valor = round(valor, 0), fecha = format(fecha, "%Y-%m")) %>%
  pivot_wider(names_from = fecha, values_from = valor)

tabla_interna_gr1_fmt <- tabla_interna_gr1 %>%
  mutate(across(where(is.numeric),
                ~ formatC(.x, format = "f", digits = 0, big.mark = ".", decimal.mark = ",")))

tema_tabla_gr1 <- fc_crear_tema_tabla(tabla_interna_gr1)
x_pos_gr1 <- min(base_gr1_empleo_covid$fecha) %m+% months(22)
y_pos_gr1 <- min(base_gr1_empleo_covid$valor) + 230000

gr_Delta_Empleo_COVID <- ggplot(base_gr1_empleo_covid, aes(x = fecha, y = valor, color = categoria)) +
  geom_line(size = 1.2) +
  geom_point(size = 0.5) +
  labs(
    title    = "Gráfico 0. Creación de empleo entre fin del COVID (JJA - 2021) y el trimestre actual",
    subtitle = "Diferencial respecto de la base",
    x = "Fecha", y = "Cambio", color = "Categoría",
    caption  = "Fuente: Elaboración propia en base a datos ENE."
  ) +
  theme_minimal() +
  theme(
    text            = element_text(size = 8),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title    = element_text(size = 11),
    legend.text     = element_text(size = 8),
    plot.caption    = element_text(size = 8, color = "gray40", hjust = 1, face = "italic")
  ) +
  guides(color = guide_legend(nrow = 1)) +
  geom_table(
    data = data.frame(x = x_pos_gr1, y = y_pos_gr1),
    aes(x = x, y = y, label = list(tabla_interna_gr1_fmt)),
    table.theme = tema_tabla_gr1,
    vjust = 0.6, hjust = 0.1
  ) +
  expand_limits(y = c(min(base_gr1_empleo_covid$valor, na.rm = TRUE) - 3, NA))

# ==============================================================================
# GRÁFICO 1: Tasa de desocupación 2010 a la fecha ####
# ==============================================================================

base_gr2_tdesocup <- dt_ENE_sexo %>%
  filter(categoria %in% c("T_desocupación"))

tabla_xls_gr2_tdesocup <- base_gr2_tdesocup %>%
  select(fecha, sexo_label, valor) %>%
  mutate(valor = round(valor, 1), fecha = format(fecha, "%Y-%m")) %>%
  pivot_wider(names_from = sexo_label, values_from = valor)

pred_gr2_tdesocup <- fc_generar_predicciones(base_gr2_tdesocup, "valor", "sexo_label", cortes)

puntos_gr2_tdesocup <- base_gr2_tdesocup %>%
  group_by(categoria, sexo_label) %>%
  filter(fecha %in% Fechas[c("actual", "a_2_annos", "inicio_gob", "a_6_annos", "a_13_annos")]) %>%
  ungroup()

tabla_interna_gr2 <- puntos_gr2_tdesocup %>%
  select(fecha, Sexo = sexo_label, valor) %>%
  mutate(valor = round(valor, 1), fecha = format(fecha, "%Y-%m")) %>%
  pivot_wider(names_from = fecha, values_from = valor)

tabla_interna_gr2_fmt <- tabla_interna_gr2 %>%
  mutate(across(where(is.numeric),
                ~ formatC(.x, format = "f", digits = 1, big.mark = ".", decimal.mark = ",")))

tema_tabla_gr2 <- fc_crear_tema_tabla(tabla_interna_gr2)
x_pos_gr2 <- min(base_gr2_tdesocup$fecha) %m+% years(9)
y_pos_gr2 <- min(base_gr2_tdesocup$valor) * 1.1

gr_Tasa_Desocup_2010_Actual <- ggplot(base_gr2_tdesocup, aes(x = fecha, y = valor, color = sexo_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 0.5) +
  geom_line(data = pred_gr2_tdesocup,
            aes(x = fecha, y = prediccion, color = sexo_label, linetype = corte),
            linewidth = 0.8) +
  labs(
    title    = "Gráfico 1. Tasa de desocupación entre 2010 a la Fecha, por sexo",
    subtitle = "Series y regresiones por Sexo, con cambios estructurales",
    x = "Fecha", y = "Porcentaje", color = "Sexo",
    caption  = "Fuente: Elaboración propia en base a datos ENE."
  ) +
  theme_minimal() +
  theme(
    text            = element_text(size = 10),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title    = element_text(size = 9),
    legend.text     = element_text(size = 8),
    plot.caption    = element_text(size = 8, color = "gray40", hjust = 1, face = "italic")
  ) +
  guides(color = guide_legend(nrow = 1)) +
  geom_table(
    data = data.frame(x = x_pos_gr2, y = y_pos_gr2),
    aes(x = x, y = y, label = list(tabla_interna_gr2_fmt)),
    table.theme = tema_tabla_gr2,
    vjust = 1.2, hjust = 0.1
  ) +
  expand_limits(y = c(min(base_gr2_tdesocup$valor, na.rm = TRUE) - 3, NA))

# ==============================================================================
# GRÁFICO 2: Tasa de participación 2010 a la fecha ####
# ==============================================================================

base_gr3_tparticip <- dt_ENE_sexo %>%
  filter(categoria %in% c("T_participación"))

tabla_xls_gr3_tparticip <- base_gr3_tparticip %>%
  select(fecha, sexo_label, valor) %>%
  mutate(valor = round(valor, 1), fecha = format(fecha, "%Y-%m")) %>%
  pivot_wider(names_from = sexo_label, values_from = valor)

pred_gr3_tparticip <- fc_generar_predicciones(base_gr3_tparticip, "valor", "sexo_label", cortes)

puntos_gr3_tparticip <- base_gr3_tparticip %>%
  group_by(categoria, sexo_label) %>%
  filter(fecha %in% Fechas[c("actual", "a_2_annos", "inicio_gob", "a_6_annos", "a_13_annos")]) %>%
  ungroup()

tabla_interna_gr3 <- puntos_gr3_tparticip %>%
  select(fecha, Sexo = sexo_label, valor) %>%
  mutate(valor = round(valor, 1), fecha = format(fecha, "%Y-%m")) %>%
  pivot_wider(names_from = fecha, values_from = valor)

tabla_interna_gr3_fmt <- tabla_interna_gr3 %>%
  mutate(across(where(is.numeric),
                ~ formatC(.x, format = "f", digits = 1, big.mark = ".", decimal.mark = ",")))

tema_tabla_gr3 <- fc_crear_tema_tabla(tabla_interna_gr3)
x_pos_gr3 <- min(base_gr3_tparticip$fecha) %m+% years(1)
y_pos_gr3 <- min(base_gr3_tparticip$valor) * 0.8

gr_Tasa_Particip_2010_Actual <- ggplot(base_gr3_tparticip, aes(x = fecha, y = valor, color = sexo_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 0.5) +
  geom_line(data = pred_gr3_tparticip,
            aes(x = fecha, y = prediccion, color = sexo_label, linetype = corte),
            linewidth = 0.8) +
  labs(
    title    = "Gráfico 2. Tasa de participación en Chile 2010-2026",
    subtitle = "Por Sexo y lineas de tendencia",
    x = "Fecha", y = "Porcentaje", color = "Sexo",
    caption  = "Fuente: Elaboración propia en base a datos ENE."
  ) +
  theme_minimal() +
  theme(
    text            = element_text(size = 10),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title    = element_text(size = 9),
    legend.text     = element_text(size = 8),
    plot.caption    = element_text(size = 8, color = "gray40", hjust = 1, face = "italic")
  ) +
  guides(color = guide_legend(nrow = 1)) +
  geom_table(
    data = data.frame(x = x_pos_gr3, y = y_pos_gr3),
    aes(x = x, y = y, label = list(tabla_interna_gr3_fmt)),
    table.theme = tema_tabla_gr3,
    vjust = 0, hjust = 0
  ) +
  expand_limits(y = c(y_pos_gr3 - 5, NA))

# ==============================================================================
# GRÁFICO 3: Razones para no participar en la FT (FFT) ####
# ==============================================================================

gr_Razon_No_part <- dt_Coyuntural %>%
  filter(
    tipo_categoria == "fft ampliada",
    valor_eje      == "Total nacional",
    sexo_label     != "Ambos sexos"
  ) %>%
  group_by(categoria) %>%
  mutate(
    porcentaje = valor / sum(valor, na.rm = TRUE) * 100,
    porcentaje = if_else(sexo_label == "Mujeres", -porcentaje, porcentaje)
  ) %>%
  ungroup() %>%
  ggplot(aes(x = categoria, y = porcentaje, fill = sexo_label)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = paste0(abs(round(porcentaje, 1)), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
  labs(
    title    = "Gráfico 3. Caracterización de razones para no participar en la fuerza de trabajo",
    subtitle = "Porcentaje dentro de cada categoría (mujeres a la izquierda, hombres a la derecha)",
    x = NULL, y = "Porcentaje"
  ) +
  theme_minimal()

# ==============================================================================
# GRÁFICO 4: Ocupados por formalidad detallada desde JAS-2017 ####
# ==============================================================================

cats_gr4_detalle <- c(
  "Ocupados informales", "Ocupados formales",
  "Cuenta propia informales",
  "Asalariados privado informales", "Asalariados público informales"
)

base_gr4_wide <- dt_ENE_sexo %>%
  filter(sexo_label == "Ambos Sexos", categoria %in% cats_gr4_detalle) %>%
  pivot_wider(names_from = categoria, values_from = valor) %>%
  arrange(fecha) %>%
  mutate(
    `Asalariados informales`    = `Asalariados privado informales` + `Asalariados público informales`,
    `Resto ocupados informales` = `Ocupados informales` - `Cuenta propia informales` - `Asalariados informales`
  ) %>%
  filter(fecha >= fecha_Formalidad)

base_gr4_ocup_form <- base_gr4_wide %>%
  select(fecha,
         `Ocupados formales`,
         `Resto ocupados informales`,
         `Asalariados informales`,
         `Cuenta propia informales`) %>%
  pivot_longer(-fecha, names_to = "categoria", values_to = "valor") %>%
  mutate(categoria = factor(categoria, levels = c(
    "Ocupados formales",
    "Resto ocupados informales",
    "Asalariados informales",
    "Cuenta propia informales"
  )))

colores_gr4 <- c(
  "Ocupados formales"          = "#1B7837",
  "Resto ocupados informales"  = "#FDAE61",
  "Asalariados informales"     = "#EB8C1B",
  "Cuenta propia informales"   = "#B35806"
)

colores_gr4_punto <- c(
  "Ocupados formales"          = "#145a22",
  "Resto ocupados informales"  = "#d48000",
  "Asalariados informales"     = "#a05c00",
  "Cuenta propia informales"   = "#7f3e00"
)

base_gr4_apilada <- base_gr4_ocup_form %>%
  mutate(categoria = factor(categoria, levels = rev(levels(categoria)))) %>%
  arrange(fecha, categoria) %>%
  group_by(fecha) %>%
  mutate(y_pos = cumsum(valor)) %>%
  mutate(categoria = factor(categoria, levels = rev(levels(categoria)))) %>%
  ungroup()

puntos_gr4_ocup <- base_gr4_apilada %>%
  filter(fecha %in% Fechas[c("actual", "a_1_anno", "a_2_annos", "a_3_annos", "a_4_annos",
                             "a_5_annos", "fecha_Covid", "a_6_annos", "a_7_annos", "a_8_annos",
                             "a_9_annos", "a_10_annos")])

tabla_xls_gr4_ocup_form <- base_gr4_ocup_form %>%
  mutate(valor = round(valor, 0), fecha = format(fecha, "%Y-%m")) %>%
  pivot_wider(names_from = categoria, values_from = valor)

gr_Ocupacion_2017_Actual <- base_gr4_ocup_form %>%
  ggplot(aes(x = fecha, y = valor, fill = categoria)) +
  geom_area(position = "stack", alpha = 0.8) +
  geom_point(data = puntos_gr4_ocup,
             aes(x = fecha, y = y_pos, color = categoria),
             size = 2.5, show.legend = FALSE) +
  geom_label(data = puntos_gr4_ocup,
             aes(x = fecha, y = y_pos, color = categoria,
                 label = scales::comma(round(valor / 1000, 0), big.mark = ".", decimal.mark = ",")),
             size = 2.0, fontface = "bold", label.padding = unit(0.12, "lines"),
             fill = "white", show.legend = FALSE, vjust = -0.6) +
  scale_fill_manual(values  = colores_gr4) +
  scale_color_manual(values = colores_gr4_punto) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(
    title    = "Gráfico 4. Personas ocupadas en Chile: JAS-2017 a la fecha",
    subtitle = "Desglose de Ocupados por Formalidad e Informalidad",
    x = NULL, y = "Personas", fill = "Categoría",
    caption  = "Fuente: Elaboración propia en base a datos ENE."
  ) +
  theme_minimal() +
  theme(
    text             = element_text(size = 8),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "right",
    legend.text      = element_text(size = 8),
    legend.title     = element_text(size = 9),
    plot.caption     = element_text(size = 7, color = "gray40", hjust = 1, face = "italic"),
    panel.grid.minor = element_blank()
  ) +
  guides(fill  = guide_legend(ncol = 1, byrow = TRUE, label.hjust = 0),
         color = "none")

# ==============================================================================
# GRÁFICO 5: Variación 12 meses ocupados por formalidad ####
# ==============================================================================

base_gr6_ocup_form_12m <- dt_ENE_sexo %>%
  filter(
    categoria  %in% c("Ocupados formales", "Ocupados informales"),
    sexo_label == "Ambos Sexos"
  ) %>%
  mutate(fecha = as.Date(fecha)) %>%
  group_by(categoria, fecha) %>%
  summarise(valorOri = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  arrange(categoria, fecha) %>%
  group_by(categoria) %>%
  mutate(
    valor_12m_antes = dplyr::lag(valorOri, 12),
    valor           = valorOri - valor_12m_antes
  ) %>%
  ungroup() %>%
  filter(fecha >= as.Date("2019-01-01"))

tabla_xls_gr6_ocup_form_12m <- base_gr6_ocup_form_12m %>%
  select(fecha, categoria, valor) %>%
  mutate(valor = round(valor, 0), fecha = format(fecha, "%Y-%m")) %>%
  pivot_wider(names_from = categoria, values_from = valor)

colores_gr6 <- c("Ocupados formales" = "#1B7837", "Ocupados informales" = "#E6550D")

puntos_gr6_ocup_12m <- base_gr6_ocup_form_12m %>%
  group_by(categoria) %>%
  filter(fecha %in% Fechas[c("actual", "a_1_anno", "a_2_annos", "a_3_annos",
                             "a_4_annos", "a_5_annos", "a_6_annos", "a_7_annos")]) %>%
  ungroup()

tabla_interna_gr6 <- puntos_gr6_ocup_12m %>%
  select(fecha, Categoría = categoria, valor) %>%
  mutate(valor = round(valor, 0), fecha = format(fecha, "%Y-%m")) %>%
  pivot_wider(names_from = fecha, values_from = valor) %>%
  bind_rows(summarise(., Categoría = "Deficit-Superávit",
                      across(where(is.numeric), sum, na.rm = TRUE)))

tabla_interna_gr6_fmt <- tabla_interna_gr6 %>%
  mutate(across(where(is.numeric),
                ~ formatC(.x, format = "f", digits = 0, big.mark = ".", decimal.mark = ",")))

tema_tabla_gr6 <- fc_crear_tema_tabla(tabla_interna_gr6_fmt)
x_pos_gr6 <- min(base_gr6_ocup_form_12m$fecha) %m+% months(18)
y_pos_gr6 <- max(base_gr6_ocup_form_12m$valor) * 0.95

gr_Delta_Ocupados_Inf_12m <- base_gr6_ocup_form_12m %>%
  ggplot(aes(x = fecha, y = valor, fill = categoria)) +
  geom_col(position = "stack", alpha = 0.8, color = NA) +
  scale_fill_manual(values = colores_gr6) +
  labs(
    title    = "Gráfico 5: Desglose Ocupados medidos en Cambio 12 meses",
    subtitle = "Separados por formalidad",
    x = "Fecha", y = "Valor", fill = "Categoría"
  ) +
  theme_minimal() +
  theme(
    text              = element_text(size = 8),
    axis.text.x       = element_text(angle = 45, hjust = 1),
    legend.title      = element_text(size = 9),
    legend.text       = element_text(size = 8),
    legend.position   = "right",
    legend.box        = "vertical",
    legend.spacing.y  = unit(0.3, "cm"),
    legend.text.align = 0
  ) +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE, label.hjust = 0,
                             title.position = "top", title.hjust = 0.5)) +
  geom_table(
    data = data.frame(x = x_pos_gr6, y = y_pos_gr6),
    aes(x = x, y = y, label = list(tabla_interna_gr6_fmt)),
    table.theme = tema_tabla_gr6,
    vjust = 4, hjust = -0.2
  ) +
  expand_limits(y = c(y_pos_gr6 - 5, NA))

# ==============================================================================
# GRÁFICO 6: Tasa asalariados dependientes informales por sexo ####
# ==============================================================================

base_gr7_asal_dep_inf <- dt_ENE_sexo %>%
  filter(sexo %in% c("AS", "H", "M"),
         categoria %in% c("Asalariados_dep_Inform", "Asalariados_dep")) %>%
  pivot_wider(names_from = categoria, values_from = valor) %>%
  filter(!is.na(Asalariados_dep_Inform), !is.na(Asalariados_dep), Asalariados_dep > 0) %>%
  mutate(
    tasa       = round(Asalariados_dep_Inform / Asalariados_dep * 100, 1),
    sexo_label = factor(sexo, levels = c("H", "M", "AS"),
                        labels = c("Tasa hombres", "Tasa mujeres", "Tasa total país"))
  ) %>%
  arrange(sexo_label, fecha)

fechas_puntos_gr7 <- Fechas[c("actual", "a_1_anno", "a_2_annos", "a_3_annos", "a_4_annos",
                              "a_5_annos", "a_6_annos", "a_7_annos", "a_8_annos",
                              "a_9_annos", "a_10_annos")]
puntos_gr7_asal <- base_gr7_asal_dep_inf %>% filter(fecha %in% fechas_puntos_gr7)

colores_gr7 <- c("Tasa hombres"    = "#1f77b4",
                 "Tasa mujeres"    = "#2ca02c",
                 "Tasa total país" = "#d62728")

gr_asal_dep_inf_Sexo <- base_gr7_asal_dep_inf %>%
  ggplot(aes(x = fecha, y = tasa, color = sexo_label)) +
  geom_line(linewidth = 0.7) +
  geom_point(data = puntos_gr7_asal, aes(x = fecha, y = tasa),
             size = 2.5, show.legend = FALSE) +
  geom_label(data = puntos_gr7_asal,
             aes(x = fecha, y = tasa, label = paste0(format(tasa, nsmall = 1), "%")),
             size = 2.2, fontface = "bold", label.padding = unit(0.15, "lines"),
             show.legend = FALSE, vjust = -0.6) +
  scale_color_manual(values = colores_gr7) +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = "%")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  labs(
    title    = "Gráfico 6: Evolución tasa asalariados informales según sexo y total país",
    subtitle = "Asalariados Dependientes: Asalariados Privados, Públicos y Servicios personales",
    x = NULL, y = "Tasa asalariados informales (%)", color = NULL, caption = NULL
  ) +
  theme_minimal(base_size = 8) +
  theme(
    plot.title       = element_text(face = "bold", size = 9, hjust = 0),
    plot.subtitle    = element_text(size = 7, color = "gray40", hjust = 0),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 6),
    axis.title.y     = element_text(size = 7),
    legend.position  = "bottom",
    legend.text      = element_text(size = 8),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5)))

# ==============================================================================
# GRÁFICO 7: Variación interanual ocupados e incidencias por categoría ####
# ==============================================================================

cats_ocup_incid <- c(
  "Ocupados",
  "Cuenta propia formales", "Cuenta propia informales",
  "Asalariados privado formales", "Asalariados público formales", "Servicio doméstico formales",
  "Asalariados privado informales", "Asalariados público informales", "Servicio doméstico informales"
)

base_gr8_incid <- dt_ENE_sexo %>%
  filter(sexo == "AS", categoria %in% cats_ocup_incid, fecha >= as.Date("2017-07-01")) %>%
  pivot_wider(names_from = categoria, values_from = valor) %>%
  arrange(fecha) %>%
  mutate(
    Asal_Form   = `Asalariados privado formales` + `Asalariados público formales` + `Servicio doméstico formales`,
    Asal_Inform = `Asalariados privado informales` + `Asalariados público informales` + `Servicio doméstico informales`,
    TCP_total   = `Cuenta propia formales` + `Cuenta propia informales`,
    Ocup_lag        = lag(Ocupados, 12),
    dOcup           = Ocupados    - lag(Ocupados,    12),
    dAsal_Form      = Asal_Form   - lag(Asal_Form,   12),
    dAsal_Inform    = Asal_Inform - lag(Asal_Inform, 12),
    dTCP            = TCP_total   - lag(TCP_total,   12),
    Var_Ocup_pct    = dOcup / Ocup_lag * 100,
    Inc_Asal_Form   = dAsal_Form   / Ocup_lag * 100,
    Inc_Asal_Inform = dAsal_Inform / Ocup_lag * 100,
    Inc_TCP         = dTCP         / Ocup_lag * 100
  ) %>%
  filter(!is.na(Var_Ocup_pct))

punto_actual_gr8 <- base_gr8_incid %>% filter(fecha == fecha_Actual)

base_gr8_barras <- base_gr8_incid %>%
  select(fecha, Inc_Asal_Form, Inc_Asal_Inform, Inc_TCP) %>%
  pivot_longer(cols = -fecha, names_to = "componente", values_to = "valor") %>%
  mutate(componente = factor(componente,
                             levels = c("Inc_Asal_Form", "Inc_Asal_Inform", "Inc_TCP"),
                             labels = c("Inc. Asalariados formales",
                                        "Inc. Asalariados informales",
                                        "Inc. Trabajadores por cuenta propia")))

colores_gr8 <- c(
  "Inc. Asalariados formales"           = "#1f77b4",
  "Inc. Asalariados informales"         = "#17becf",
  "Inc. Trabajadores por cuenta propia" = "#d62728"
)

gr_delta_Ocup_incid_categ <- ggplot() +
  geom_col(data = base_gr8_barras,
           aes(x = fecha, y = valor, fill = componente),
           position = "stack", alpha = 0.85, width = 25) +
  geom_line(data = base_gr8_incid,
            aes(x = fecha, y = Var_Ocup_pct, color = "Variación % 12 meses Ocupados"),
            linewidth = 0.8) +
  geom_point(data = punto_actual_gr8, aes(x = fecha, y = Var_Ocup_pct),
             color = "#2ca02c", size = 3, show.legend = FALSE) +
  geom_label(data = punto_actual_gr8,
             aes(x = fecha, y = Var_Ocup_pct,
                 label = paste0(format(round(Var_Ocup_pct, 1), nsmall = 1), "%")),
             color = "#2ca02c", size = 2.5, fontface = "bold",
             label.padding = unit(0.2, "lines"), vjust = -0.6, show.legend = FALSE) +
  scale_fill_manual(values = colores_gr8) +
  scale_color_manual(values = c("Variación % 12 meses Ocupados" = "#2ca02c")) +
  scale_y_continuous(labels = scales::number_format(suffix = "%", decimal.mark = ",")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y\n%b") +
  labs(
    title    = "Gráfico 7: Variación interanual ocupados e incidencias según categoría ocupacional",
    subtitle = "Series líderes de categorías",
    x = NULL, y = "Variación / Incidencia (%)", fill = NULL, color = NULL
  ) +
  theme_minimal(base_size = 8) +
  theme(
    plot.title       = element_text(face = "bold", size = 9, hjust = 0),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 6),
    axis.title.y     = element_text(size = 7),
    legend.position  = "bottom",
    legend.text      = element_text(size = 7.5),
    legend.key.width = unit(1, "cm"),
    panel.grid.minor = element_blank()
  ) +
  guides(fill  = guide_legend(nrow = 2, byrow = TRUE),
         color = guide_legend(override.aes = list(linewidth = 1.5)))

# ==============================================================================
# EXPORTAR A EXCEL ####
# ==============================================================================

## Preparar tablas faltantes ####

tabla_xls_gr7_asal_dep_inf <- base_gr7_asal_dep_inf %>%
  select(fecha, sexo_label, tasa) %>%
  mutate(fecha = format(fecha, "%Y-%m")) %>%
  pivot_wider(names_from = sexo_label, values_from = tasa)

tabla_xls_gr8_incid <- base_gr8_incid %>%
  select(fecha, Var_Ocup_pct, Inc_Asal_Form, Inc_Asal_Inform, Inc_TCP) %>%
  mutate(fecha = format(fecha, "%Y-%m"),
         across(where(is.numeric), ~ round(.x, 2))) %>%
  rename(
    `Variacion % Ocupados 12m`       = Var_Ocup_pct,
    `Incidencia Asal. Formales`      = Inc_Asal_Form,
    `Incidencia Asal. Informales`    = Inc_Asal_Inform,
    `Incidencia Trab. Cuenta Propia` = Inc_TCP
  )

completa_xls <- dt_ENE_sexo %>%
  pivot_wider(names_from = categoria, values_from = valor) %>%
  arrange(sexo, fecha)

## Definición de hojas ####
hojas <- list(
  list(nombre = "G0_Empleo_postCovid",   titulo = "Gráfico 0. Creación de empleo post-COVID (diferencial desde JJA-2021)",               datos = tabla_xls_gr1_empleo_covid),
  list(nombre = "G1_Tasa_Desocupacion",  titulo = "Gráfico 1. Tasa de desocupación 2010 a la fecha, por sexo",                           datos = tabla_xls_gr2_tdesocup),
  list(nombre = "G2_Tasa_Participacion", titulo = "Gráfico 2. Tasa de participación 2010 a la fecha, por sexo",                          datos = tabla_xls_gr3_tparticip),
  list(nombre = "G3_Razones_FFT",        titulo = "Gráfico 3. Razones para no participar en la fuerza de trabajo, por sexo",             datos = fft_tabla),
  list(nombre = "G4_Ocup_Form_Detalle",  titulo = "Gráfico 4. Ocupados por formalidad detallada desde JAS-2017",                         datos = tabla_xls_gr4_ocup_form),
  list(nombre = "G5_Var12m_Formalidad",  titulo = "Gráfico 5. Variación 12 meses de ocupados por formalidad",                           datos = tabla_xls_gr6_ocup_form_12m),
  list(nombre = "G6_Asal_dep_Informal",  titulo = "Gráfico 6. Tasa de asalariados dependientes informales por sexo",                    datos = tabla_xls_gr7_asal_dep_inf),
  list(nombre = "G7_Incidencias_Ocup",   titulo = "Gráfico 7. Variación interanual de ocupados e incidencias por categoría ocupacional", datos = tabla_xls_gr8_incid),
  list(nombre = "Serie_H",               titulo = "Serie completa Hombres (validación)",                                                datos = completa_xls[completa_xls$sexo == "H",  ]),
  list(nombre = "Serie_M",               titulo = "Serie completa Mujeres (validación)",                                                datos = completa_xls[completa_xls$sexo == "M",  ]),
  list(nombre = "Serie_AS",              titulo = "Serie completa Ambos Sexos (validación)",                                            datos = completa_xls[completa_xls$sexo == "AS", ])
)

## Crear workbook — Indice primero para que quede como primera hoja ####
wb <- createWorkbook()

estilo_titulo <- createStyle(fontSize = 11, fontColour = "#1a5276", textDecoration = "bold")
estilo_header <- createStyle(fontSize = 10, fontColour = "#FFFFFF", fgFill = "#1a5276",
                             halign = "center", textDecoration = "bold", wrapText = TRUE)
estilo_datos  <- createStyle(fontSize = 9, halign = "left")
estilo_link   <- createStyle(fontSize = 11, fontColour = "#1f618d", textDecoration = "underline")

addWorksheet(wb, "Indice")

## Escribir hojas de datos ####
for (h in hojas) {
  addWorksheet(wb, h$nombre)
  writeData(wb, h$nombre, h$titulo, startRow = 1, startCol = 1)
  addStyle(wb, h$nombre, estilo_titulo, rows = 1, cols = 1)
  writeData(wb, h$nombre, h$datos, startRow = 3, startCol = 1, headerStyle = estilo_header)
  addStyle(wb, h$nombre, estilo_datos,
           rows = 4:(nrow(h$datos) + 3), cols = 1:ncol(h$datos), gridExpand = TRUE)
  setColWidths(wb, h$nombre, cols = 1:ncol(h$datos), widths = "auto")
}

## Poblar hoja índice ####
writeData(wb, "Indice",
          paste0("Datos de respaldo — Minuta Coyuntural ENE ", fc_fecha_a_trimestre(fecha_Actual)),
          startRow = 1, startCol = 1)
addStyle(wb, "Indice", createStyle(fontSize = 13, fontColour = "#1a5276", textDecoration = "bold"),
         rows = 1, cols = 1)

writeData(wb, "Indice", "Hoja",      startRow = 3, startCol = 1)
writeData(wb, "Indice", "Contenido", startRow = 3, startCol = 2)
addStyle(wb, "Indice", estilo_header, rows = 3, cols = 1:2)

for (i in seq_along(hojas)) {
  fila <- i + 3
  writeFormula(wb, "Indice",
               x = paste0('=HYPERLINK("#\'', hojas[[i]]$nombre, '\'!A1","', hojas[[i]]$nombre, '")'),
               startRow = fila, startCol = 1)
  writeData(wb, "Indice", hojas[[i]]$titulo, startRow = fila, startCol = 2)
  addStyle(wb, "Indice", estilo_link,  rows = fila, cols = 1)
  addStyle(wb, "Indice", estilo_datos, rows = fila, cols = 2)
}

setColWidths(wb, "Indice", cols = 1, widths = 28)
setColWidths(wb, "Indice", cols = 2, widths = 70)

## Guardar ####
saveWorkbook(wb, paste0("BBDD/", Trim_Actual, "/TablasGraficos.xlsx"), overwrite = TRUE)