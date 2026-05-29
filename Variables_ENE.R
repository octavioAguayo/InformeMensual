# variables
# Actual
Val_Act_PET                <- fc_Datos_ENE(fecha_Actual, "AS", "PET")
Val_Act_Ocupados           <- fc_Datos_ENE(fecha_Actual, "AS", "Ocupados")
Val_Act_Ocupados_Form      <- fc_Datos_ENE(fecha_Actual, "AS", "Ocupados formales")
Val_Act_Ocupados_Inform    <- fc_Datos_ENE(fecha_Actual, "AS", "Ocupados informales")
Val_Act_Ocupados_Fem       <- fc_Datos_ENE(fecha_Actual, "M",  "Ocupados")
Val_Act_Desocupados        <- fc_Datos_ENE(fecha_Actual, "AS", "Desocupados")
Val_Act_FT                 <- fc_Datos_ENE(fecha_Actual, "AS", "FT")
Tasa_Act_Inform            <- fc_Datos_ENE(fecha_Actual, "AS", "T_Informal")
Tasa_Act_Desocup           <- fc_Datos_ENE(fecha_Actual, "AS", "T_desocupación")
Tasa_Act_Desocup_Fem       <- fc_Datos_ENE(fecha_Actual, "M",  "T_desocupación")
Tasa_Act_Particip          <- fc_Datos_ENE(fecha_Actual, "AS", "T_participación")
Tasa_Act_Particip_Fem      <- fc_Datos_ENE(fecha_Actual, "M",  "T_participación")
Tasa_Act_Particip_Hom      <- fc_Datos_ENE(fecha_Actual, "H",  "T_participación")

Brecha_Act_Particip        <- fc_Brecha_ENE(fecha_Actual, "T_participación")
Brecha_Act_Desocup         <- fc_Brecha_ENE(fecha_Actual, "T_desocupación")

Val_Act_Asal_dep           <- fc_Datos_ENE(fecha_Actual, "AS", "Asalariados_dep")
Val_Act_Asal_dep_Inform    <- fc_Datos_ENE(fecha_Actual, "AS", "Asalariados_dep_Inform")
Val_Act_Asal_dep_Fem       <- fc_Datos_ENE(fecha_Actual, "M", "Asalariados_dep")
Val_Act_Asal_dep_Inform_Fem<- fc_Datos_ENE(fecha_Actual, "M", "Asalariados_dep_Inform")

Tasa_Act_Asal_dep_Inform   <- Val_Act_Asal_dep_Inform / Val_Act_Asal_dep * 100
Tasa_Act_Asal_dep_Inform_Fem<- Val_Act_Asal_dep_Inform_Fem / Val_Act_Asal_dep_Fem * 100

# Mes anterior
Val_Mes_PET                <- fc_Datos_ENE(fecha_MesAnterior, "AS", "PET")
Val_Mes_Ocupados_Form      <- fc_Datos_ENE(fecha_MesAnterior, "AS", "Ocupados formales")
Delta_Mes_Tasa_Desocup     <- fc_Delta(fecha_MesAnterior, "AS", "T_desocupación")
Delta_Mes_Tasa_Desocup_Fem <- fc_Delta(fecha_MesAnterior, "M",  "T_desocupación")
Delta_Mes_Ocupados         <- fc_Delta(fecha_MesAnterior, "AS", "Ocupados")
Delta_Mes_Ocupados_Fem     <- fc_Delta(fecha_MesAnterior, "M",  "Ocupados")
Delta_Mes_Desocupados      <- fc_Delta(fecha_MesAnterior, "AS", "Desocupados")
Delta_Mes_Desocupados_Fem  <- fc_Delta(fecha_MesAnterior, "M", "Desocupados")
Delta_Mes_Tasa_Particip    <- fc_Delta(fecha_MesAnterior, "AS", "T_participación")
Delta_Mes_Ocupados_Inform  <- fc_Delta(fecha_MesAnterior, "AS", "Ocupados informales")
Delta_Mes_Tasa_Inform      <- fc_Delta(fecha_MesAnterior, "AS", "T_Informal")

# Año anterior
Val_Año_Ocupados           <- fc_Datos_ENE(fecha1annos, "AS", "Ocupados")
Val_Año_Asal_dep           <- fc_Datos_ENE(fecha1annos, "AS", "Asalariados_dep")
Val_Año_Asal_dep_Inform    <- fc_Datos_ENE(fecha1annos, "AS", "Asalariados_dep_Inform")
Val_Año_Asal_dep_Fem       <- fc_Datos_ENE(fecha1annos, "M", "Asalariados_dep")
Val_Año_Asal_dep_Inform_Fem<- fc_Datos_ENE(fecha1annos, "M", "Asalariados_dep_Inform")
Delta_Año_Ocupados         <- fc_Delta(fecha1annos, "AS", "Ocupados")
Delta_Año_Ocupados_Fem     <- fc_Delta(fecha1annos, "M",  "Ocupados")
Delta_Año_Ocupados_Hom     <- fc_Delta(fecha1annos, "H",  "Ocupados")
Delta_Año_Desocupados      <- fc_Delta(fecha1annos, "AS", "Desocupados")
Delta_Año_Desocupados_Fem  <- fc_Delta(fecha1annos, "M", "Desocupados")
Delta_Año_PET              <- fc_Delta(fecha1annos, "AS", "PET")
Delta_Año_Ocupados_Form    <- fc_Delta(fecha1annos, "AS", "Ocupados formales")
Delta_Año_Ocupados_Form_Fem    <- fc_Delta(fecha1annos, "M", "Ocupados formales")
Delta_Año_Ocupados_Form_Hom    <- fc_Delta(fecha1annos, "H", "Ocupados formales")
Delta_Año_Ocupados_Inform  <- fc_Delta(fecha1annos, "AS", "Ocupados informales")
Delta_Año_Asal_Pub         <- fc_Delta(fecha1annos, "AS", "Asalariados público")
Delta_Año_Asal_Priv        <- fc_Delta(fecha1annos, "AS", "Asalariados privados")
Delta_Año_Asal_Sdom        <- fc_Delta(fecha1annos, "AS", "Servicio doméstico")
Delta_Año_Asal_Indep       <- fc_Delta(fecha1annos, "AS", "Independientes")
Delta_Año_Asal_Priv_Form   <- fc_Delta(fecha1annos, "AS", "Asalariados privado formales")
Delta_Año_Asal_Priv_Inform <- fc_Delta(fecha1annos, "AS", "Asalariados privado informales")
Delta_Año_Asal_Pub_Form    <- fc_Delta(fecha1annos, "AS", "Asalariados público informales")
Delta_Año_Serv_Dom_Form    <- fc_Delta(fecha1annos, "AS", "Servicio doméstico formales")
Delta_Año_Tasa_Desocup     <- fc_Delta(fecha1annos, "AS", "T_desocupación")
Delta_Año_Tasa_Desocup_Fem <- fc_Delta(fecha1annos, "M",  "T_desocupación")
Delta_Año_Tasa_Particip    <- fc_Delta(fecha1annos, "AS", "T_participación")
Delta_Año_Tasa_Particip_Fem<- fc_Delta(fecha1annos, "M",  "T_participación")
Delta_Año_Tasa_Particip_Hom<- fc_Delta(fecha1annos, "H",  "T_participación")
Delta_Año_Tasa_Inform      <- fc_Delta(fecha1annos, "AS", "T_Informal")

Delta_Año_Asal_dep_Inform    <- fc_Delta(fecha1annos, "AS", "Asalariados_dep_Inform")
Delta_Año_Asal_dep_Inform_Fem<- fc_Delta(fecha1annos, "M", "Asalariados_dep_Inform")
Delta_Año_Asal_dep           <- fc_Delta(fecha1annos, "AS", "Asalariados_dep")
Tasa_Año_Asal_dep_Inform     <- Val_Año_Asal_dep_Inform / Val_Año_Asal_dep * 100
Tasa_Año_Asal_dep_Inform_Fem <- Val_Año_Asal_dep_Inform_Fem / Val_Año_Asal_dep_Fem * 100

Delta_Tasa_Asal_dep_Inform   <- Tasa_Act_Asal_dep_Inform - Tasa_Año_Asal_dep_Inform
Delta_Tasa_Asal_dep_Inform_Fem<- Tasa_Act_Asal_dep_Inform_Fem - Tasa_Año_Asal_dep_Inform_Fem


Tasa_Año_Asal_Pub          <- Delta_Año_Asal_Pub         / fc_Datos_ENE(fecha1annos, "AS", "Asalariados público")            * 100
Tasa_Año_Asal_Priv         <- Delta_Año_Asal_Priv        / fc_Datos_ENE(fecha1annos, "AS", "Asalariados privados")           * 100
Tasa_Año_Asal_Sdom         <- Delta_Año_Asal_Sdom        / fc_Datos_ENE(fecha1annos, "AS", "Servicio doméstico")             * 100
Tasa_Año_Asal_Indep        <- Delta_Año_Asal_Indep       / fc_Datos_ENE(fecha1annos, "AS", "Independientes")                 * 100
Tasa_Año_Asal_Priv_Form    <- Delta_Año_Asal_Priv_Form   / fc_Datos_ENE(fecha1annos, "AS", "Asalariados privado formales")   * 100
Tasa_Año_Asal_Priv_Inform  <- Delta_Año_Asal_Priv_Inform / fc_Datos_ENE(fecha1annos, "AS", "Asalariados privado informales") * 100

# Covid
Val_Covid_PET               <- fc_Datos_ENE(fecha_Inicio_Covid, "AS", "PET")
Val_Covid_Tasa_Inform       <- fc_Datos_ENE(fecha_Inicio_Covid, "AS", "T_Informal")
Val_Covid_Tasa_Particip     <- fc_Datos_ENE(fecha_Inicio_Covid, "AS", "T_participación")
Val_Covid_Tasa_Particip_Fem <- fc_Datos_ENE(fecha_Inicio_Covid, "M",  "T_participación")
Delta_Covid_Tasa_Desocup_Fem  <- fc_Delta(fecha_Inicio_Covid, "M",  "T_desocupación")
Delta_Covid_Tasa_Particip_Fem <- fc_Delta(fecha_Inicio_Covid, "M",  "T_participación")
Delta_Covid_Tasa_Particip     <- fc_Delta(fecha_Inicio_Covid, "AS", "T_participación")

# Gobierno
Delta_Gob_Ocupados          <- fc_Delta(fecha_Inicio_Gob, "AS", "Ocupados")
Delta_Gob_Ocupados_Form     <- fc_Delta(fecha_Inicio_Gob, "AS", "Ocupados formales")
Delta_Gob_Ocupados_Inform   <- fc_Delta(fecha_Inicio_Gob, "AS", "Ocupados informales")
Delta_Gob_Ocupados_Fem      <- fc_Delta(fecha_Inicio_Gob, "M",  "Ocupados")
Delta_Gob_Tasa_Particip_Fem <- fc_Delta(fecha_Inicio_Gob, "M",  "T_participación")
Delta_Gob_Tasa_Form         <- round(Delta_Gob_Ocupados_Form / Delta_Gob_Ocupados * 100, 1)
Delta_Gob_Tasa_Fem          <- round(Delta_Gob_Ocupados_Fem  / Delta_Gob_Ocupados * 100, 1)

# Fechas varias
Val_Act_Ocupados_2022       <- fc_Datos_ENE(fecha4annos, "AS", "Ocupados")


# Indicadores Coyuntural Ampliado del Mes ####

Val_Estudio_M <- fc_val_coyuntural(dt_Coyuntural,sexo_label = "Mujeres", categoria = "FFT Razones de estudio")
Val_Estudio_T <- fc_val_coyuntural(dt_Coyuntural,sexo_label = "Ambos sexos", categoria = "FFT Razones de estudio")
Porc_Estudio_M <- round(100*Val_Estudio_M/Val_Estudio_T,1)

Val_Raz_Familia_M <- fc_val_coyuntural(dt_Coyuntural,sexo_label = "Mujeres", categoria = "FFT Razones familiares permanentes")
Val_Raz_Familia_T <- fc_val_coyuntural(dt_Coyuntural,sexo_label = "Ambos sexos", categoria = "FFT Razones familiares permanentes")
Porc_Raz_Familia_M <- round(100*Val_Raz_Familia_M/Val_Raz_Familia_T,1)

Val_Raz_Familia_MM <- round(Val_Raz_Familia_M / 1e6, 1)

# FECHAS desde Trim_Actual ####

año_act  <- as.numeric(str_sub(Trim_Actual, 1, 4)) 
mes_act  <- as.numeric(str_sub(Trim_Actual, 5, 6))


