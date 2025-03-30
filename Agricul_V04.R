ruta_C3A_TR <- "D:/Documents/EZunigaA/03_IDEAM/IDEAM_2024/2024_Natura/BD_Incertidumbre/01_Agricultura/Tabla_Referencia/Definitivas"
ruta_C3A_DA <- "D:/Documents/EZunigaA/03_IDEAM/IDEAM_2024/2024_Natura/BD_Incertidumbre/01_Agricultura/BD_Basicos"

# Dato de actividad 

# 1. Leer los datos de actividad de departamento a región desde Excel
DAGOV01 <- read_excel(file.path(ruta_C3A_DA, "DA3A1A00_GBov_Dpto_1990_2021_sd.xlsx"))

# 2. Crear DAGOV02: agrupar por Ano, Cod_region e IPCC y sumar t_ganbov_dpto
DAGOV02 <- DAGOV01 %>%
  group_by(Ano, Cod_region, IPCC) %>%
  summarise(t_ganbov_r = sum(t_ganbov_dpto, na.rm = TRUE), .groups = "drop")

# 3. Ordenar DAGOV01 y DAGOV02
DAGOV01 <- DAGOV01 %>% arrange(Ano, Cod_region, IPCC)
DAGOV02 <- DAGOV02 %>% arrange(Ano, Cod_region, IPCC)

# 4. Crear DAGOV03: merge de DAGOV01 y DAGOV02 y calcular Var_tbov = t_ganbov_sd^2  
DAGOV03 <- DAGOV01 %>%
  left_join(DAGOV02, by = c("Ano", "Cod_region", "IPCC")) %>%
  mutate(Var_tbov = t_ganbov_sd^2)

# 5. Crear DAGOV04: agrupar por Ano, Cod_region e IPCC y sumar t_ganbov_dpto y Var_tbov
DAGOV04 <- DAGOV03 %>%
  group_by(Ano, Cod_region, IPCC) %>%
  summarise(t_ganbov = sum(t_ganbov_dpto, na.rm = TRUE),
            t_ganbov_var = sum(Var_tbov, na.rm = TRUE),
            .groups = "drop")

# 6. Crear DAGOV05: calcular t_ganbov_sd = sqrt(t_ganbov_var) y eliminar t_ganbov_var
DAGOV05 <- DAGOV04 %>%
  mutate(t_ganbov_sd = sqrt(t_ganbov_var)) %>%
  select(-t_ganbov_var)

# 7. Leer la tabla de referencia desde Excel
Tref_ipcc_ab_v01 <- read_excel(file.path(ruta_C3A_TR, "TR01_IPCC_3A_3B_GB_CH4_V01.xlsx"))

# 8. Ordenar y unir Tref_ipcc_ab_v01 y DAGOV05 por Cod_region e IPCC
Tref_ipcc_ab_v01 <- Tref_ipcc_ab_v01 %>% arrange(Cod_region, IPCC)
DAGOV05 <- DAGOV05 %>% arrange(Cod_region, IPCC)

Tref_ipcc_ab_v01 <- Tref_ipcc_ab_v01 %>%
  left_join(DAGOV05, by = c("Cod_region", "IPCC"))

# 9. Eliminar objetos intermedios
rm(DAGOV01, DAGOV02, DAGOV03, DAGOV04, DAGOV05)

# Función gei_rgn para la simulación
gei_rgn <- function(ano, reg, cat, cat1, n_sim = 2) {
  # Filtrar Tref_ipcc_ab_v01 según año, región y categoría
  TR <- Tref_ipcc_ab_v01 %>%
    filter(Ano == ano, Cod_region == reg, IPCC == cat)
  
  if (nrow(TR) == 0) {
    warning(paste("No se encontraron registros para:", ano, reg, cat))
    return(NULL)
  }
  
  # Asignar la tabla base filtrada
  assign(paste0("TR_A", ano, "_C", cat, "_R", reg, "_E02"), TR, envir = .GlobalEnv)
  
  # Se toma la primera fila para la simulación
  params <- TR[1, ]
  
  # Asegurarse de que Tmp_amb y Tmp_amb_sd sean numéricos
  params$Tmp_amb <- as.numeric(params$Tmp_amb)
  params$Tmp_amb_sd <- as.numeric(params$Tmp_amb_sd)
  
  # Valores por defecto
  mean_tmp_amb <- ifelse(is.na(params$Tmp_amb), 20, params$Tmp_amb)
  sd_tmp_amb <- ifelse(is.na(params$Tmp_amb_sd), 5, params$Tmp_amb_sd)
  
  # Simulación
  sim <- data.frame(
    Tmp_amb_mc = rnorm(n_sim, mean = params$Tmp_amb, sd = params$Tmp_amb_sd),
    CA01_peso_mc = rnorm(n_sim, mean = params$CA01_peso, sd = params$CA01_peso_sd),
    CA02_psa_mc = rnorm(n_sim, mean = params$CA02_psa, sd = params$CA02_psa_sd),
    CA03_pgn_mc = rnorm(n_sim, mean = params$CA03_pgn, sd = params$CA03_pgn_sd),
    CA04_pllc_mc = rnorm(n_sim, mean = params$CA04_pllc, sd = params$CA04_pllc_sd),
    CA05_cgl_mc = rnorm(n_sim, mean = params$CA05_cgl, sd = params$CA05_cgl_sd),
    CA06_clctl_mc = rnorm(n_sim, mean = params$CA06_clctl, sd = params$CA06_clctl_sd),
    CDP02_Dgt_mc = rnorm(n_sim, mean = params$CDP02_Dgt, sd = params$CDP02_Dgt_sd),
    CDP03_EB_mc = rnorm(n_sim, mean = params$CDP03_EB, sd = params$CDP03_EB_sd),
    CDP04_EDEB_mc = rnorm(n_sim, mean = params$CDP04_EDEB, sd = params$CDP04_EDEB_sd),
    CDP05_NEM_mc = rnorm(n_sim, mean = params$CDP05_NEM, sd = params$CDP05_NEM_sd),
    CDP06_pc_mc = rnorm(n_sim, mean = params$CDP06_pc, sd = params$CDP06_pc_sd),
    CDP07_Fnd_mc = rnorm(n_sim, mean = params$CDP07_Fnd, sd = params$CDP07_Fnd_sd),
    CDP08_Fda_mc = rnorm(n_sim, mean = params$CDP08_Fda, sd = params$CDP08_Fda_sd),
    CDP09_Cnz_mc = rnorm(n_sim, mean = params$CDP09_Cnz, sd = params$CDP09_Cnz_sd),
    CDS02_Dgt_mc = rnorm(n_sim, mean = params$CDS02_Dgt, sd = params$CDS02_Dgt_sd),
    CDS03_EB_mc = rnorm(n_sim, mean = params$CDS03_EB, sd = params$CDS03_EB_sd),
    CDS04_EDEB_mc = rnorm(n_sim, mean = params$CDS04_EDEB, sd = params$CDS04_EDEB_sd),
    CDS05_NEM_mc = rnorm(n_sim, mean = params$CDS05_NEM, sd = params$CDS05_NEM_sd),
    CDS06_pc_mc = rnorm(n_sim, mean = params$CDS06_pc, sd = params$CDS06_pc_sd),
    CDS07_Fnd_mc = rnorm(n_sim, mean = params$CDS07_Fnd, sd = params$CDS07_Fnd_sd),
    CDS08_Fda_mc = rnorm(n_sim, mean = params$CDS08_Fda, sd = params$CDS08_Fda_sd),
    CDS09_Cnz_mc = rnorm(n_sim, mean = params$CDS09_Cnz, sd = params$CDS09_Cnz_sd),
    CPD00_mc = rnorm(n_sim, mean = params$CPD00 / 100, sd = params$CPD00_sd / 100),
    t_ganbov_mc = rnorm(n_sim, mean = params$t_ganbov, sd = params$t_ganbov_sd)
  )
  
  # Calcular variables derivadas
  sim <- sim %>%
    mutate(
      CSD00_mc = 1 - CPD00_mc,
      Ano = params$Ano,
      Cod_region = params$Cod_region,
      IPCC = params$IPCC,
      TMB_ajus = params$TMB_ajus,
      Tmp_conf = params$Tmp_conf,
      TMB = params$TMB,
      CA07_hrs = params$CA07_hrs,
      CA = params$CA,
      CEF = params$CEF,
      RCEC = params$RCEC,
      CG = params$CG,
      AWMS01_lad = params$AWMS01_lad,
      AWMS02_cels = params$AWMS02_cels,
      MCF03_ppp = params$MCF03_ppp,
      BI = params$BI,
      BO = params$BO
    )
  
  # Asignar la tabla de simulación
  assign(paste0("A", ano, "_C", cat, "_R", reg, "_smc_gral"), sim, envir = .GlobalEnv)
  
  # Llamar a la función simulate_fermentation para calcular las emisiones GEI
  sim <- simulate_fermentation(sim, params)
  assign(paste0("A", ano, "_C", cat, "_R", reg, "_sm_E02_01"), sim, envir = .GlobalEnv)
  
  # Llamar a la función simulate_estiercol para calcular las emisiones GEI
  sim <- simulate_estiercol(sim, params)
  assign(paste0("A", ano, "_C", cat1, "_R", reg, "_sm_E02_01"), sim, envir = .GlobalEnv)
  
  # Eliminar la tabla base filtrada
  rm(list = paste0("TR_A", ano, "_C", cat, "_R", reg, "_E02"), envir = .GlobalEnv)
  
  invisible(sim)
}

# Función para calcular la fermentación y las emisiones GEI
simulate_fermentation <- function(sim, params) {
  sim <- sim %>%
    mutate(
      CDT03_EB = (CDP03_EB_mc * CPD00_mc) + (CDS03_EB_mc * CSD00_mc),
      CDT04_EDEB = (CDP04_EDEB_mc * CPD00_mc) + (CDS04_EDEB_mc * CSD00_mc),
      CDT05_NEM = (CDP05_NEM_mc * CPD00_mc) + (CDS05_NEM_mc * CSD00_mc),
      CDT07_Fnd = (CDP07_Fnd_mc * CPD00_mc) + (CDS07_Fnd_mc * CSD00_mc),
      CDT08_Fda = (CDP08_Fda_mc * CPD00_mc) + (CDS08_Fda_mc * CSD00_mc),
      NEM_tmp_aj = TMB_ajus * (Tmp_conf - Tmp_amb_mc),
      NEM = (CA01_peso_mc^0.75) * (TMB + NEM_tmp_aj),
      NEA = NEM * CA,
      NEG = 22.02 * ((CA01_peso_mc / (CEF * CA02_psa_mc))^0.75) * (CA03_pgn_mc^1.097),
      NEG = ifelse(is.na(NEG), 0, NEG),
      PD_Leche_dia = CA04_pllc_mc / 365,
      NEI = PD_Leche_dia * (1.47 + 0.4 * CA05_cgl_mc),
      NER = 0.1 * NEM * CA07_hrs,
      NEP = NEM * CG,
      REM = 1.123 - (4.092 * 0.001 * CDT04_EDEB) + (1.126 * 0.00001 * (CDT04_EDEB^2)) - (25.4 / CDT04_EDEB),
      REG = 1.164 - (5.160 * 0.001 * CDT04_EDEB) + (1.308 * 0.00001 * (CDT04_EDEB^2)) - (37.4 / CDT04_EDEB),
      EBC_leche = ((((44.01 * CA05_cgl_mc + 163.56) * 4.184) / 0.4536) * 0.001) * CA06_clctl_mc,
      EBC_leche = ifelse(is.na(EBC_leche), 0, EBC_leche),
      CEB_dia = (((NEM + NEA + NEI + NER + NEP) / REM) + (NEG / REG)) / (CDT04_EDEB / 100) - EBC_leche,
      CMS = CEB_dia / CDT03_EB,
      CH4 = 3.41 + 0.52 * CMS - 0.996 * CMS * (CDT08_Fda / 100) + 1.15 * (CMS * (CDT07_Fnd / 100)),
      YM = CH4 * 100 / CEB_dia,
      CMS = ifelse(IPCC %in% "3A1A50", {
        CMS1 = ((CA01_peso_mc + (CA03_pgn_mc * 365)) * 0.96)^0.75
        CMS2 = ((0.2435 * (CDT05_NEM / 4.184)) - (0.0466 * ((CDT05_NEM / 4.184)^2)) - 0.0869) / (CDT05_NEM / 4.184)
        ratio = ((CA01_peso_mc * 0.96) * 400) / (CA02_psa_mc * 0.96)
        CMS3 = ifelse(ratio >= 350, 0.7714 + 0.00196 * ratio - 0.00000371 * (ratio^2), 1)
        CMS4 = ifelse(Tmp_amb_mc > Tmp_conf, RCEC, 1)
        CMS5 = BI * (1 - (CMS4 / 100) * (Tmp_amb_mc - Tmp_conf))
        CMS1 * CMS2 * CMS3 * CMS4 * CMS5
      }, CMS),
      CH4 = ifelse(IPCC %in% "3A1A50", 3.41 + 0.52 * CMS - 0.996 * CMS * (CDT08_Fda / 100) + 1.15 * (CMS * (CDT07_Fnd / 100)), CH4),
      YM = ifelse(IPCC %in% "3A1A50", (CH4 * 100) / (CMS * CDT03_EB), YM),
      EFME = (CEB_dia * (YM / 100) * 365) / 55.65,
      EFME = ifelse(IPCC %in% "3A1A50", (CEB_dia * (YM / 100) * 273.75) / 55.65, EFME),
      GEI_FE = EFME * t_ganbov_mc * 28 / 1000000
    )
  return(sim)
}

# Función para calcular la gestión de estiércol y las emisiones GEI
simulate_estiercol <- function(sim, params) {
  sim <- sim %>%
    mutate(
      CDT02_Dgt = (CDP02_Dgt_mc * CPD00_mc) + (CDS02_Dgt_mc * CSD00_mc),
      CDT03_EB = (CDP03_EB_mc * CPD00_mc) + (CDS03_EB_mc * CSD00_mc),
      CDT04_EDEB = (CDP04_EDEB_mc * CPD00_mc) + (CDS04_EDEB_mc * CSD00_mc),
      CDT05_NEM = (CDP05_NEM_mc * CPD00_mc) + (CDS05_NEM_mc * CSD00_mc),
      CDT06_pc = (CDP06_pc_mc * CPD00_mc) + (CDS06_pc_mc * CSD00_mc),
      CDT09_Cnz = (CDP09_Cnz_mc * CPD00_mc) + (CDS09_Cnz_mc * CSD00_mc),
      MCF01_Lad = ifelse(IPCC %in% "3A1A10", ifelse(Tmp_amb_mc < 0, 0, (80.38 * (1 - exp(-0.17 * Tmp_amb_mc))) / 100), NA),
      MCF02_CELS = ifelse(IPCC %in% "3A1A20", ifelse(Tmp_amb_mc >= 0 & Tmp_amb_mc <= 9, 0, ifelse(Tmp_amb_mc >= 30, 0.02, ifelse(Tmp_amb_mc > 9 & Tmp_amb_mc < 30, -0.82 + 0.19 * Tmp_amb_mc - 0.0032 * (Tmp_amb_mc^2), NA))), NA),
      AWMS03_ppp = 1 - (AWMS01_lad + AWMS02_cels),
      NEM_tmp_aj = TMB_ajus * (Tmp_conf - Tmp_amb_mc),
      NEM = (CA01_peso_mc^0.75) * (TMB + NEM_tmp_aj),
      NEA = NEM * CA,
      NEG = 22.02 * ((CA01_peso_mc / (CEF * CA02_psa_mc))^0.75) * (CA03_pgn_mc^1.097),
      NEG = ifelse(is.na(NEG), 0, NEG),
      PD_Leche_dia = CA04_pllc_mc / 365,
      NEI = PD_Leche_dia * (1.47 + 0.4 * CA05_cgl_mc),
      NER = 0.1 * NEM * CA07_hrs,
      NEP = NEM * CG,
      REM = 1.123 - (4.092 * 0.001 * CDT04_EDEB) + (1.126 * 0.00001 * (CDT04_EDEB^2)) - (25.4 / CDT04_EDEB),
      REG = 1.164 - (5.160 * 0.001 * CDT04_EDEB) + (1.308 * 0.00001 * (CDT04_EDEB^2)) - (37.4 / CDT04_EDEB),
      EBC_leche = ((((44.01 * CA05_cgl_mc + 163.56) * 4.184) / 0.4536) * 0.001) * CA06_clctl_mc,
      EBC_leche = ifelse(is.na(EBC_leche), 0, EBC_leche),
      CEB_dia = (((NEM + NEA + NEI + NER + NEP) / REM) + (NEG / REG)) / (CDT04_EDEB / 100) - EBC_leche,
      CMS = CEB_dia / CDT03_EB,
      EU_MJ = -2.71 + 0.028 * (10 * CDP06_pc_mc) + 0.589 * CMS,
      MCF_AWMS_sm = (MCF01_Lad * AWMS01_lad) + (MCF02_CELS * AWMS02_cels) + ((MCF03_ppp / 100) * AWMS03_ppp),
      TE_VS = (CEB_dia * (1 - (CDT02_Dgt / 100)) + EU_MJ) * ((1 - (CDT09_Cnz / 100)) / 18.45),
      EGME = (TE_VS * 365) * (BO * 0.67 * MCF_AWMS_sm),
      GEI_GE = EGME * t_ganbov_mc * 28 / 1000000
    )
  return(sim)
}