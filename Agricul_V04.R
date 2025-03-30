title: "Incertidumbre Agricultura"
author: "Carlos Andres Pinzon"
date: "20/01/2025"
# Programa par el calculo de Incertidumbre agricultura BTR_1

# Instalacion de paquetes / funciones

#remove.packages("rJava")
install.packages("rJava")
install.packages("readxl")
install.packages("xlsx")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("haven")
install.packages("readr")
install.packages("tidy")
install.packages("ggplot")
install.packages("purrr")
install.packages("writexl")

library(rJava)
library(readxl)
library(xlsx)
library(dplyr)
library(tidyverse)
library(haven)
library(readr)
library(tidyr)  
library(ggplot2)
library(purrr)
library(writexl)


# 1. Definir rutas (equivalente a LIBNAME en SAS)
ruta_C3A_TR <- "D:\\Documents\\EZunigaA\\03_IDEAM\\IDEAM_2024\\2024_Natura\\BD_Incertidumbre\\01_Agricultura\\Tabla_Referencia\\Definitivas"
ruta_C3A_DA <- "D:\\Documents\\EZunigaA\\03_IDEAM\\IDEAM_2024\\2024_Natura\\BD_Incertidumbre\\01_Agricultura\\BD_Basicos"

#Dato de actividad 

# 2. Leer los datos de actividad de departamento a región desde Excel
# Se asume que el archivo se llama "DA_GanBov_Dpto_1990_2021_sd.xlsx"
DAGOV01 <- read_excel(file.path(ruta_C3A_DA, "DA3A1A00_GBov_Dpto_1990_2021_sd.xlsx"))

# 3. Crear DAGOV02: agrupar por Ano, Cod_region e IPCC y sumar t_ganbov_dpto
DAGOV02 <- DAGOV01 %>%
  group_by(Ano, Cod_region, IPCC) %>%
  summarise(t_ganbov_r = sum(t_ganbov_dpto, na.rm = TRUE), .groups = "drop") %>%
  distinct()

# 4. Ordenar DAGOV01 y DAGOV02 (equivalente a PROC SORT)
DAGOV01 <- DAGOV01 %>% arrange(Ano, Cod_region, IPCC)
DAGOV02 <- DAGOV02 %>% arrange(Ano, Cod_region, IPCC)

# 5. Crear DAGOV03: merge de DAGOV01 y DAGOV02 y calcular Var_tbov = t_ganbov_sd^2  
# Se asume que en DAGOV01 existe la variable t_ganbov_sd
DAGOV03 <- DAGOV01 %>%
  left_join(DAGOV02, by = c("Ano", "Cod_region", "IPCC")) %>%
  mutate(Var_tbov = t_ganbov_sd^2)

# 6. Crear DAGOV04: agrupar por Ano, Cod_region e IPCC y sumar t_ganbov_dpto y Var_tbov
DAGOV04 <- DAGOV03 %>%
  group_by(Ano, Cod_region, IPCC) %>%
  summarise(t_ganbov = sum(t_ganbov_dpto, na.rm = TRUE),
            t_ganbov_var = sum(Var_tbov, na.rm = TRUE),
            .groups = "drop") %>%
  distinct()

# 7. Crear DAGOV05: calcular t_ganbov_sd = sqrt(t_ganbov_var) y eliminar t_ganbov_var
DAGOV05 <- DAGOV04 %>%
  mutate(t_ganbov_sd = sqrt(t_ganbov_var)) %>%
  select(-t_ganbov_var)

# 8. Leer la tabla de referencia desde Excel
# Se asume que el archivo se llama "Tref_ipcc_ab_v01.xlsx"
Tref_ipcc_ab_v01 <- read_excel(file.path(ruta_C3A_TR, "TR01_IPCC_3A_3B_GB_CH4_V01.xlsx"))

# 9. Ordenar y unir Tref_ipcc_ab_v01 y DAGOV05 por Cod_region e IPCC
Tref_ipcc_ab_v01 <- Tref_ipcc_ab_v01 %>% arrange(Cod_region, IPCC)
DAGOV05 <- DAGOV05 %>% arrange(Cod_region, IPCC)

Tref_ipcc_ab_v01 <- Tref_ipcc_ab_v01 %>%
  left_join(DAGOV05, by = c("Cod_region", "IPCC"))


# 10. Eliminar objetos intermedios (similar a PROC DELETE en SAS)
rm(DAGOV01, DAGOV02, DAGOV03, DAGOV04, DAGOV05)

#------------------------------
# Función gei_rgn para la simulación
gei_rgn <- function(ano, reg, cat, cat1, n_sim = 2) {
  
  # Filtrar Tref_ipcc_ab_v01 según año, región y categoría
  TR <- Tref_ipcc_ab_v01 %>%
    filter(Ano == ano, Cod_region == reg, IPCC == cat)
  
  if (nrow(TR) == 0) {
    warning(paste("No se encontraron registros para:", ano, reg, cat))
    return(NULL)
  }
  
  # Asignar la tabla base filtrada con nombre dinámico similar a TR_A&ano._C&cat._R&reg._E02
  obj_name_TR <- paste0("TR_A", ano, "_C", cat, "_R", reg, "_E02")
  assign(obj_name_TR, TR, envir = .GlobalEnv)
  
  # Se asume que TR tiene los parámetros necesarios; se toma la primera fila para la simulación
  params <- TR[1, ]
  
  # Asegurarse de que Tmp_amb y Tmp_amb_sd sean numéricos
  params$Tmp_amb <- as.numeric(params$Tmp_amb)
  params$Tmp_amb_sd <- as.numeric(params$Tmp_amb_sd)
  
  # Verificar si Tmp_amb o Tmp_amb_sd son NA o no válidos, en tal caso reemplazarlos con un valor por defecto
  mean_tmp_amb <- ifelse(is.na(params$Tmp_amb), 20, params$Tmp_amb)  # Valor predeterminado para Tmp_amb
  sd_tmp_amb <- ifelse(is.na(params$Tmp_amb_sd), 5, params$Tmp_amb_sd)  # Valor predeterminado para Tmp_amb_sd
  
  # Realizar la simulación para n_sim iteraciones usando rnorm() para cada variable
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
      Tmp_conf=params$Tmp_conf,
      TMB=params$TMB,
      CA07_hrs = params$CA07_hrs,
      CA = params$CA,
      CEF=params$CEF,
      RCEC=params$RCEC,
      CG=params$CG,
      AWMS01_lad=params$AWMS01_lad,
      AWMS02_cels=params$AWMS02_cels,
      MCF03_ppp=params$MCF03_ppp,
      BI=params$BI,
      BO=params$BO
    )
  
  # Asignar la tabla de simulación con nombre dinámico similar a A&ano._C&cat._R&reg._smc_gral
  obj_name_sim <- paste0("A", ano, "_C", cat, "_R", reg, "_smc_gral")
  assign(obj_name_sim, sim, envir = .GlobalEnv)
  
  sim_data <- get(obj_name_sim, envir = .GlobalEnv)
  print(paste("Visualizando el dataframe:", obj_name_sim))
  head(sim_data)  
  #-----------------------------------------------------------------------#
  #          Función para Fermentación entérica                           #
  #-----------------------------------------------------------------------#    
  
  # Llamar a la función simulate_fermentation para calcular las emisiones GEI
  sim <- simulate_fermentation(sim, params)
  
  # Asignar el dataframe con el nombre dinámico de fermentación
  ferment_name <- paste0("A", ano, "_C", cat, "_R", reg, "_sm_E02_01")
  assign(ferment_name, sim, envir = .GlobalEnv)
  
  # Mostrar el dataframe de fermentación para verificación
  ferment_data <- get(ferment_name, envir = .GlobalEnv)
  print(paste("Visualizando el dataframe de fermentación:", ferment_name))
  head(ferment_data)
  
  #-----------------------------------------------------------------------#
  #           Función para gestión de estiércol                           #
  #-----------------------------------------------------------------------#  
  
  # Llamar a la función simulate_estiercol para calcular las emisiones GEI
  sim <- simulate_estiercol(sim, params)
  
  # Asignar el dataframe con el nombre dinámico de gestion
  estiercol_name <- paste0("A", ano, "_C", cat1, "_R", reg, "_sm_E02_01")
  assign(estiercol_name, sim, envir = .GlobalEnv)
  
  # Verificar que el dataframe de estiércol se asignó correctamente
  estiercol_data <- get(estiercol_name, envir = .GlobalEnv)
  print(paste("Visualizando el dataframe de estiércol:", estiercol_name))
  head(estiercol_data)  # Mostrar los primeros registros
  
  
  # Eliminar la tabla base filtrada (simulando proc delete)
  rm(list = obj_name_TR, envir = .GlobalEnv)
  
  # Devolver la tabla de simulación (opcional)
  invisible(sim)
}

# Función para calcular la fermentación y las emisiones GEI
simulate_fermentation <- function(sim, params) {
  sim <- sim %>%
    mutate(
      # Cálculos de caracterización de la dieta (enteric)
      CDT03_EB    = (CDP03_EB_mc * CPD00_mc) + (CDS03_EB_mc * CSD00_mc),
      CDT04_EDEB  = (CDP04_EDEB_mc * CPD00_mc) + (CDS04_EDEB_mc * CSD00_mc),
      CDT05_NEM   = (CDP05_NEM_mc * CPD00_mc) + (CDS05_NEM_mc * CSD00_mc),
      CDT07_Fnd   = (CDP07_Fnd_mc * CPD00_mc) + (CDS07_Fnd_mc * CSD00_mc),
      CDT08_Fda   = (CDP08_Fda_mc * CPD00_mc) + (CDS08_Fda_mc * CSD00_mc),
      
      NEM_tmp_aj  = TMB_ajus * (Tmp_conf - Tmp_amb_mc),
      NEM         = (CA01_peso_mc^0.75) * (TMB + NEM_tmp_aj),
      NEA         = NEM * CA,
      NEG         = 22.02 * ((CA01_peso_mc/(CEF*CA02_psa_mc))^0.75) * (CA03_pgn_mc^1.097),
      NEG         = ifelse(is.na(NEG), 0, NEG),
      PD_Leche_dia= CA04_pllc_mc / 365,
      NEI         = PD_Leche_dia * (1.47 + 0.4 * CA05_cgl_mc),
      NER         = 0.1 * NEM * CA07_hrs,
      NEP         = NEM * CG,
      REM         = 1.123 - (4.092 * 0.001 * CDT04_EDEB) +
        (1.126 * 0.00001 * (CDT04_EDEB^2)) - (25.4 / CDT04_EDEB),
      REG         = 1.164 - (5.160 * 0.001 * CDT04_EDEB) +
        (1.308 * 0.00001 * (CDT04_EDEB^2)) - (37.4 / CDT04_EDEB),
      EBC_leche   = ((((44.01*CA05_cgl_mc + 163.56)*4.184)/0.4536)*0.001)*CA06_clctl_mc,
      EBC_leche   = ifelse(is.na(EBC_leche), 0, EBC_leche),
      CEB_dia     = (((NEM + NEA + NEI + NER + NEP) / REM) + (NEG / REG)) / (CDT04_EDEB/100) - EBC_leche,
      CMS         = CEB_dia / CDT03_EB,
      CH4         = 3.41 + 0.52 * CMS - 0.996 * CMS * (CDT08_Fda / 100) + 1.15 * (CMS * (CDT07_Fnd / 100)),
      YM          = CH4 * 100 / CEB_dia
    ) %>%
    # Ajuste para el caso especial IPCC="3A1A50"
    mutate(
      CMS = ifelse(IPCC %in% "3A1A50",
                   {
                     CMS1 = ((CA01_peso_mc + (CA03_pgn_mc * 365)) * 0.96)^0.75
                     CMS2 = ((0.2435 * (CDT05_NEM / 4.184)) - (0.0466 * ((CDT05_NEM / 4.184)^2)) - 0.0869) /
                       (CDT05_NEM / 4.184)
                     ratio = ((CA01_peso_mc * 0.96) * 400) / (CA02_psa_mc * 0.96)
                     CMS3 = ifelse(ratio >= 350,
                                   0.7714 + 0.00196 * ratio - 0.00000371 * (ratio^2),
                                   1)
                     CMS4 = ifelse(Tmp_amb_mc > Tmp_conf, RCEC, 1)
                     CMS5 = BI * (1 - (CMS4/100) * (Tmp_amb_mc - Tmp_conf))
                     CMS1 * CMS2 * CMS3 * CMS4 * CMS5
                   },
                   CMS),
      CH4 = ifelse(IPCC %in% "3A1A50",
                   3.41 + 0.52 * CMS - 0.996 * CMS * (CDT08_Fda / 100) + 1.15 * (CMS * (CDT07_Fnd / 100)),
                   CH4),
      YM  = ifelse(IPCC %in% "3A1A50",
                   (CH4 * 100) / (CMS * CDT03_EB),
                   YM)
    ) %>%
    # Cálculo del factor de emisión EFME y de las emisiones GEI_FE
    mutate(
      EFME   = (CEB_dia * (YM/100) * 365) / 55.65,
      EFME   = ifelse(IPCC %in% "3A1A50", (CEB_dia * (YM/100) * 273.75) / 55.65, EFME),
      GEI_FE = EFME * t_ganbov_mc * 28 / 1000000
    ) 
  #Seleccionar (o descartar) las variables intermedias que no se necesiten
  
  #%>% select(-c(CDP02_Dgt, CDP02_Dgt_sd, CDP02_Dgt_mc, CDP01_Nmb, CDP06_pc, CDP06_pc_sd, CDP06_pc_mc,
  #          CDP09_Cnz, CDP09_Cnz_sd, CDP09_Cnz_mc, CDS01_Nmb, CDS02_Dgt, CDS02_Dgt_sd, CDS02_Dgt_mc,
  #          CDS06_pc, CDS06_pc_sd, CDS06_pc_mc, CDS09_Cnz, CDS09_Cnz_sd, CDS09_Cnz_mc,
  #         MCF03_ppp, AWMS01_lad, AWMS02_cels))
  
  return(sim)  
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  

# Función para calcular la fermentación y las emisiones GEI
simulate_estiercol <- function(sim, params) {
  sim <- sim %>%
    mutate(
      CDT02_Dgt = (CDP02_Dgt_mc * CPD00_mc) + (CDS02_Dgt_mc * CSD00_mc),
      CDT03_EB  = (CDP03_EB_mc * CPD00_mc) + (CDS03_EB_mc * CSD00_mc),
      CDT04_EDEB= (CDP04_EDEB_mc * CPD00_mc) + (CDS04_EDEB_mc * CSD00_mc),
      CDT05_NEM = (CDP05_NEM_mc * CPD00_mc) + (CDS05_NEM_mc * CSD00_mc),
      CDT06_pc  = (CDP06_pc_mc * CPD00_mc) + (CDS06_pc_mc * CSD00_mc),
      CDT09_Cnz = (CDP09_Cnz_mc * CPD00_mc) + (CDS09_Cnz_mc * CSD00_mc)
    ) %>%
    mutate(
      MCF01_Lad = ifelse(IPCC %in% "3A1A10",
                         ifelse(Tmp_amb_mc < 0, 0, (80.38*(1 - exp(-0.17 * Tmp_amb_mc)))/100),
                         NA),
      MCF02_CELS = ifelse(IPCC %in% "3A1A20",
                          ifelse(Tmp_amb_mc >= 0 & Tmp_amb_mc <= 9, 0,
                                 ifelse(Tmp_amb_mc >= 30, 0.02,
                                        ifelse(Tmp_amb_mc > 9 & Tmp_amb_mc < 30,
                                               -0.82 + 0.19*Tmp_amb_mc - 0.0032*(Tmp_amb_mc^2),
                                               NA))),
                          NA),
      AWMS03_ppp = 1 - (AWMS01_lad + AWMS02_cels),
      NEM_tmp_aj = TMB_ajus * (Tmp_conf - Tmp_amb_mc),
      NEM = (CA01_peso_mc^0.75) * (TMB + NEM_tmp_aj),
      NEA = NEM * CA,
      NEG = 22.02 * ((CA01_peso_mc/(CEF*CA02_psa_mc))^0.75) * (CA03_pgn_mc^1.097),
      NEG = ifelse(is.na(NEG), 0, NEG),
      PD_Leche_dia = CA04_pllc_mc / 365,
      NEI = PD_Leche_dia * (1.47 + 0.4 * CA05_cgl_mc),
      NER = 0.1 * NEM * CA07_hrs,
      NEP = NEM * CG,
      REM = 1.123 - (4.092 * 0.001 * CDT04_EDEB) + (1.126 * 0.00001 * (CDT04_EDEB^2)) - (25.4 / CDT04_EDEB),
      REG = 1.164 - (5.160 * 0.001 * CDT04_EDEB) + (1.308 * 0.00001 * (CDT04_EDEB^2)) - (37.4 / CDT04_EDEB),
      EBC_leche = ((((44.01 * CA05_cgl_mc + 163.56)*4.184)/0.4536)*0.001)*CA06_clctl_mc,
      EBC_leche = ifelse(is.na(EBC_leche), 0, EBC_leche),
      CEB_dia = (((NEM + NEA + NEI + NER + NEP) / REM) + (NEG / REG)) / (CDT04_EDEB/100) - EBC_leche,
      CMS = CEB_dia / CDT03_EB,
      EU_MJ = -2.71 + 0.028*(10*CDP06_pc_mc) + 0.589*CMS,
      MCF_AWMS_sm = (MCF01_Lad*AWMS01_lad) + (MCF02_CELS*AWMS02_cels) + ((MCF03_ppp/100)*AWMS03_ppp),
      TE_VS = (CEB_dia*(1 - (CDT02_Dgt/100)) + EU_MJ) * ((1 - (CDT09_Cnz/100))/18.45),
      EGME = (TE_VS * 365) * (BO * 0.67 * MCF_AWMS_sm),
      GEI_GE = EGME * t_ganbov_mc * 28 / 1000000
    ) 
  # %>% select(-c(CDP07_Fnd, CDP07_Fnd_sd, CDP07_Fnd_mc, CDP08_Fda, CDP08_Fda_sd, CDP08_Fda_mc,
  #       CDS07_Fnd, CDS07_Fnd_sd, CDS07_Fnd_mc, CDS08_Fda, CDS08_Fda_sd, CDS08_Fda_mc, RCEC))
  
  return(sim)
}


# -------------------------------
# 13. Función 'tano': Llama a gei_rgn para distintas categorías
# -------------------------------
tano <- function(ano, reg) {
  gei_rgn(ano, reg, "3A1A10", "3B2A10")
  gei_rgn(ano, reg, "3A1A20", "3B2A20")
  gei_rgn(ano, reg, "3A1A30", "3B2A30")
  gei_rgn(ano, reg, "3A1A40", "3B2A40")
  gei_rgn(ano, reg, "3A1A50", "3B2A50")
  gei_rgn(ano, reg, "3A1A60", "3B2A60")
  gei_rgn(ano, reg, "3A1A70", "3B2A70")
}


# -------------------------------
# 14. Función 'treg': Llama a tano para varios años
# -------------------------------
treg <- function(reg) {
  tano(1990, reg)
  # tano(1991, reg)  
  # tano(1992, reg)  # Comentado
  # tano(1993, reg)  # Comentado
  # tano(1994, reg)  # Comentado
  # tano(1995, reg)  # Comentado
  # tano(1996, reg)  # Comentado
  # tano(1997, reg)  # Comentado
  # tano(1998, reg)  # Comentado
  # tano(1999, reg)  # Comentado
  # tano(2000, reg)  # Comentado
  # tano(2001, reg)  # Comentado
  # tano(2002, reg)  # Comentado
  # tano(2003, reg)  # Comentado
  # tano(2004, reg)  # Comentado
  # tano(2005, reg)  # Comentado
  # tano(2006, reg)  # Comentado
  # tano(2007, reg)  # Comentado
  # tano(2008, reg)  # Comentado
  # tano(2009, reg)  # Comentado
  # tano(2010, reg)  # Comentado
  # tano(2011, reg)  # Comentado
  # tano(2012, reg)  # Comentado
  # tano(2013, reg)  # Comentado
  # tano(2014, reg)  # Comentado
  # tano(2015, reg)  # Comentado
  # tano(2016, reg)  # Comentado
  # tano(2017, reg)  # Comentado
  # tano(2018, reg)  # Comentado
  # tano(2019, reg)  # Comentado
  # tano(2020, reg)  # Comentado
  # tano(2021, reg)  # Comentado
}

# -------------------------------
# 15. Ejecutar las simulaciones para las distintas regiones
# -------------------------------
treg("01")
treg("02")
treg("03")
treg("04")
treg("05")
treg("06")
treg("07")
treg("08")
treg("09")
treg("10")

# Función para combinar y guardar resultados de fermentación
combine_and_save_fermentation_results <- function(ano, cat, output_dir) {
  # Verificar si la ruta de salida existe, si no, crearla
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    print(paste("Se ha creado la carpeta:", output_dir))
  }
  
  all_fermentation_data <- list()
  
  # Combinar todos los dataframes para las 10 regiones
  for (reg in 1:10) {
    ferment_name <- paste0("A", ano, "_C", cat, "_R", sprintf("%02d", reg), "_sm_E02_01")
    
    # Verificar si el dataframe existe en el entorno global
    if (exists(ferment_name, envir = .GlobalEnv)) {
      print(paste(ferment_name, "existe en el entorno global"))
      all_fermentation_data[[length(all_fermentation_data) + 1]] <- get(ferment_name, envir = .GlobalEnv)
    } else {
      print(paste(ferment_name, "NO existe en el entorno global"))
    }
  }
  
  # Verificar si hay datos para combinar
  if (length(all_fermentation_data) > 0) {
    # Unir todos los dataframes en uno solo
    combined_fermentation <- bind_rows(all_fermentation_data)
    print(paste("Número de filas combinadas:", nrow(combined_fermentation)))
    
    # Verificar si los datos combinados no están vacíos
    if (nrow(combined_fermentation) > 0) {
      print("Los datos combinados no están vacíos. Procediendo a guardar el archivo CSV.")
      
      # Crear el nombre del archivo CSV con la ruta especificada
      csv_file_name <- paste0(output_dir, "/A", ano, "_C", cat, "_R00_sm_E02_01.csv")
      
      # Guardar el dataframe combinado en un archivo CSV
      write.csv(combined_fermentation, file = csv_file_name, row.names = FALSE)
      print(paste("Archivo CSV guardado como:", csv_file_name))
    } else {
      print("El dataframe combinado está vacío. No se guardó el archivo CSV.")
    }
  } else {
    print("No se encontraron dataframes para combinar.")
  }
}

# Función para procesar múltiples combinaciones de años y categorías
process_multiple_years_and_categories <- function(years, categories, output_dir) {
  for (ano in years) {
    for (cat in categories) {
      print(paste("Procesando año:", ano, "y categoría:", cat))
      combine_and_save_fermentation_results(ano, cat, output_dir)
    }
  }
}

output_dir <- "D:/Documents/EZunigaA/03_IDEAM/IDEAM_2024/Prueba/A1990"

# Especificar los años y las categorías que quieres procesar
years <- c(1990)  
categories <- c("3A1A10", "3B2A10",
                "3A1A20", "3B2A20",
                "3A1A30", "3B2A30",
                "3A1A40", "3B2A40",
                "3A1A50", "3B2A50",
                "3A1A60", "3B2A60",
                "3A1A70", "3B2A70")

# Llamar a la función para procesar todos los años y categorías
process_multiple_years_and_categories(years, categories, output_dir)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Hasta aquí los resultados a nivel de simulación-------------------------------------------
combine_fermentation_data <- function(ano, cat, output_dir) {
  all_data <- list()
  
  # Intentar cargar el archivo con el sufijo R00
  file_name_r00 <- paste0(output_dir, "/A", ano, "_C", cat, "_R00_sm_E02_01.csv")
  print(paste("Buscando archivo:", file_name_r00))  # Verificación detallada de la búsqueda
  if (file.exists(file_name_r00)) {
    print(paste(file_name_r00, "existe. Cargando los datos..."))
    data <- read.csv(file_name_r00)
    all_data[[length(all_data) + 1]] <- data
  } else {
    print(paste(file_name_r00, "NO existe"))
  }
  
  # Ahora intentar cargar los otros archivos R01, R02, ..., R10
  for (reg in 1:10) {
    if (reg != 0) {  # Solo cargar los archivos R01, R02,..., R10 si existen
      file_name <- paste0(output_dir, "/A", ano, "_C", cat, "_R", sprintf("%02d", reg), "_sm_E02_01.csv")
      print(paste("Buscando archivo:", file_name))  # Verificación detallada de la búsqueda
      
      if (file.exists(file_name)) {
        print(paste(file_name, "existe. Cargando los datos..."))
        data <- read.csv(file_name)
        all_data[[length(all_data) + 1]] <- data
      } else {
        print(paste(file_name, "NO existe"))
      }
    }
  }
  
  # Verificar si hay datos para combinar
  if (length(all_data) > 0) {
    combined_data <- bind_rows(all_data)
    
    # Guardar el archivo combinado
    output_file <- paste0(output_dir, "/A", ano, "_C", cat, "_R00_sm_E02_01.csv")
    write.csv(combined_data, output_file, row.names = FALSE)
    print(paste("Archivo combinado guardado como:", output_file))
    
    return(combined_data)
  } else {
    print("No se encontraron archivos para combinar.")
    return(NULL)
  }
}

# Función para calcular las estadísticas por grupo
calculate_statistics <- function(data) {
  stats <- data %>%
    group_by(Ano, IPCC, Cod_region) %>%
    summarise(
      Fc_emi_md = mean(EFME, na.rm = TRUE),
      Fc_emi_sd = sd(EFME, na.rm = TRUE),
      DA_md = mean(t_ganbov_mc, na.rm = TRUE),
      DA_sd = sd(t_ganbov_mc, na.rm = TRUE),
      GEI_md = mean(GEI_FE, na.rm = TRUE),
      GEI_sd = sd(GEI_FE, na.rm = TRUE)
    ) %>%
    mutate(
      U_Fc_emi = Fc_emi_sd * 1.96 / Fc_emi_md,
      U_DA = DA_sd * 1.96 / DA_md,
      U_GEI = GEI_sd * 1.96 / GEI_md
    )
  
  print("Estadísticas calculadas:")
  print(head(stats))  # Verificar las primeras filas de los resultados
  
  return(stats)
}

# Función para guardar los resultados en un archivo Excel
save_statistics <- function(stats, ano, cat, output_dir) {
  # Verificar si la ruta de salida existe, si no, crearla
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    print(paste("Se ha creado la carpeta:", output_dir))
  }
  
  # Crear el nombre dinámico del archivo Excel
  output_file <- paste0(output_dir, "/A", ano, "_C", cat, "_R00_PE_E02.xlsx")
  
  # Verificar si 'stats' no está vacío antes de escribirlo en un archivo
  if (nrow(stats) > 0) {
    # Guardar los datos en el archivo Excel
    write_xlsx(stats, path = output_file)
    print(paste("Archivo de estadísticas guardado como:", output_file))
  } else {
    print("Los datos no contienen estadísticas, no se guardó el archivo.")
  }
}

# Función principal que ejecuta todo el proceso
process_fermentation <- function(ano, cat, output_dir) {
  # Paso 1: Combinar los datos
  combined_data <- combine_fermentation_data(ano, cat, output_dir)
  
  if (!is.null(combined_data)) {
    # Paso 2: Calcular las estadísticas
    stats <- calculate_statistics(combined_data)
    
    # Paso 3: Guardar los resultados
    save_statistics(stats, ano, cat, output_dir)
  } else {
    print("No se pudo procesar los datos.")
  }
}

# Ejecutar para diferentes años y categorías
output_dir <- "D:/Documents/EZunigaA/03_IDEAM/IDEAM_2024/Prueba/A1990"
anos <- c(1990)
categories <- c("3A1A10", "3B2A10",
                "3A1A20", "3B2A20",
                "3A1A30", "3B2A30",
                "3A1A40", "3B2A40",
                "3A1A50", "3B2A50",
                "3A1A60", "3B2A60",
                "3A1A70", "3B2A70")
# Procesar todos los años y categorías
for (ano in anos) {
  for (cat in categories) {
    process_fermentation(ano, cat, output_dir)
  }
}


#---- Nacional  ------

file_path_excel <- file.path(ruta_C3A, paste0("A", ano3, "_C3A1A00_R00_pe_E02.xlsx"))

# Verificar si el archivo existe
if (file.exists(file_path_excel)) {
  print("El archivo existe. Procediendo a leerlo.")
  
  # Leer los datos desde el archivo Excel
  df_C3A1A00_R00 <- read_excel(file_path_excel)
  
  # Parte 1: Calcular las nuevas variables
  df_C3A1A00_N00 <- df_C3A1A00_R00 %>%
    mutate(
      S2_FE_CH4 = (U_Fc_emi * Fc_emi_md)^2,
      S2_DA = (U_DA * DA_md)^2,
      S2_GEI_Emi_CH4 = (U_GEI * GEI_md)^2
    )
  
  # Parte 2: Agrupar por Ano, IPCC y hacer las sumas y medias
  df_C3A1A00_N00_02 <- df_C3A1A00_N00 %>%
    group_by(Ano, IPCC) %>%
    summarise(
      DA_nal = sum(DA_md, na.rm = TRUE),
      S2_DA_nal = sum(S2_DA, na.rm = TRUE),
      FE_CH4_nal = mean(Fc_emi_md, na.rm = TRUE),
      S2_FE_CH4_nal = sum(S2_FE_CH4, na.rm = TRUE),
      GEI_CH4_nal = sum(GEI_md, na.rm = TRUE),
      S2_GEI_CH4_nal = sum(S2_GEI_Emi_CH4, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Parte 3: Calcular las nuevas variables basadas en los cálculos anteriores
  df_C3A1A00_N00_03 <- df_C3A1A00_N00_02 %>%
    mutate(
      U_Emi_DA = sqrt(S2_DA_nal) / DA_nal,
      U_Emi_CH4 = sqrt(S2_FE_CH4_nal) / FE_CH4_nal,
      U_GEI_Emi_Eq_CH4 = sqrt(S2_GEI_CH4_nal) / GEI_CH4_nal
    )
  
  # Parte 4: Guardar el resultado final en un archivo CSV
  write.csv(df_C3A1A00_N00_03, file.path(ruta_C3A, paste0("A", ano3, "_C3A1A00_N00_pe_E02_03.csv")), row.names = FALSE)
  
} else {
  print(paste("El archivo no existe en la ruta especificada:", file_path_excel))
}



#######--












ruta_C3A <- "D:/Documents/EZunigaA/03_IDEAM/IDEAM_2024/Prueba"
ano3 <- 1990  # Año para el ejemplo

# Función para combinar los archivos por categoría y generar solo el archivo consolidado
combine_files_by_category <- function(ano, cat, output_dir) {
  all_data <- list()
  
  # Solo cargar los archivos con el sufijo "R00_sm_E02_01.csv"
  file_name <- file.path(output_dir, paste0("A", ano, "_C", cat, "_R00_sm_E02_01.csv"))
  
  # Verificar si el archivo existe, si es así cargarlo
  if (file.exists(file_name)) {
    print(paste(file_name, "existe. Cargando los datos..."))
    data <- read.csv(file_name)
    all_data[[length(all_data) + 1]] <- data
  } else {
    print(paste(file_name, "NO existe"))
  }
  
  # Si hay datos para combinar, devolver el dataframe combinado
  if (length(all_data) > 0) {
    combined_data <- bind_rows(all_data)
    return(combined_data)
  } else {
    print("No se encontraron archivos para combinar.")
    return(NULL)
  }
}

# Función para calcular las estadísticas
calculate_statistics <- function(data) {
  stats <- data %>%
    group_by(Ano, IPCC, Cod_region) %>%
    summarise(
      Fc_emi_md = mean(EFME, na.rm = TRUE),
      Fc_emi_sd = sd(EFME, na.rm = TRUE),
      DA_md = mean(t_ganbov_mc, na.rm = TRUE),
      DA_sd = sd(t_ganbov_mc, na.rm = TRUE),
      GEI_md = mean(GEI_FE, na.rm = TRUE),
      GEI_sd = sd(GEI_FE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      U_Fc_emi = Fc_emi_sd * 1.96 / Fc_emi_md,
      U_DA = DA_sd * 1.96 / DA_md,
      U_GEI = GEI_sd * 1.96 / GEI_md
    )
  
  return(stats)
}

# Función para guardar los resultados en un archivo Excel consolidado
save_statistics <- function(stats_list, ano, output_dir) {
  # Verificar si la ruta de salida existe, si no, crearla
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    print(paste("Se ha creado la carpeta:", output_dir))
  }
  
  # Consolidar todos los resultados de las categorías en un solo dataframe
  consolidated_stats <- bind_rows(stats_list)
  
  # Crear el nombre del archivo Excel consolidado
  file_path_excel <- file.path(output_dir, paste0("A", ano, "_C3A1A00_R00_pe_E02.xlsx"))
  
  # Verificar la ruta completa del archivo antes de guardar
  print(paste("Ruta del archivo a guardar:", file_path_excel))
  
  # Verificar si el dataframe consolidado no está vacío antes de escribirlo en un archivo
  if (nrow(consolidated_stats) > 0) {
    # Guardar los datos en el archivo Excel
    write_xlsx(consolidated_stats, path = file_path_excel)
    print(paste("Archivo de estadísticas consolidado guardado como:", file_path_excel))
  } else {
    print("Los datos no contienen estadísticas, no se guardó el archivo.")
  }
}

# Función principal que ejecuta todo el proceso
process_fermentation <- function(ano, cat, output_dir, stats_list) {
  # Paso 1: Combinar los datos
  combined_data <- combine_files_by_category(ano, cat, output_dir)
  
  if (!is.null(combined_data)) {
    # Paso 2: Calcular las estadísticas
    stats <- calculate_statistics(combined_data)
    
    # Agregar las estadísticas de esta categoría a la lista
    stats_list[[length(stats_list) + 1]] <- stats
  } else {
    print(paste("No se pudo procesar los datos para la categoría:", cat))
  }
}

# Procesar para varias categorías
output_dir <- "D:/Documents/EZunigaA/03_IDEAM/IDEAM_2024/Prueba/A1990"
stats_list <- list()

# Ejecutar para diferentes años y categorías
ano3 <- 1990
categories <- c("3A1A10", "3B2A10", "3A1A20", "3B2A20")

for (cat in categories) {
  process_fermentation(ano3, cat, output_dir, stats_list)
}

# Guardar el archivo consolidado
save_statistics(stats_list, ano3, output_dir)

print("Proceso completado.")


calculate_statistics <- function(data) {
  stats <- data %>%
    group_by(Ano, IPCC, Cod_region) %>%
    summarise(
      Fc_emi_md = mean(EFME, na.rm = TRUE),
      Fc_emi_sd = sd(EFME, na.rm = TRUE),
      DA_md = mean(t_ganbov_mc, na.rm = TRUE),
      DA_sd = sd(t_ganbov_mc, na.rm = TRUE),
      GEI_md = mean(GEI_FE, na.rm = TRUE),
      GEI_sd = sd(GEI_FE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      U_Fc_emi = Fc_emi_sd * 1.96 / Fc_emi_md,
      U_DA = DA_sd * 1.96 / DA_md,
      U_GEI = GEI_sd * 1.96 / GEI_md
    )
  
  return(stats)
}





ruta_C3A <- "D:/Documents/EZunigaA/03_IDEAM/IDEAM_2024/Prueba"
ano3 <- 1990  # Año para el ejemplo

# Función para combinar los archivos y agregar las estadísticas por año y categoría
combine_and_process_files <- function(ano, output_dir) {
  all_data <- list()
  
  # Archivos por categorías (A1, A2, ..., A7)
  categories <- c("C3A1A10", "C3A1A20", "C3A1A30", "C3A1A40", "C3A1A50", "C3A1A60", "C3A1A70")
  
  # Leer y combinar archivos por cada categoría
  for (scat in categories) {
    # Generar nombre de archivo basado en la categoría
    file_name <- file.path(output_dir, paste0("A", ano, "_", scat, "_R00_PE_E02.xlsx"))
    
    # Leer los archivos si existen
    if (file.exists(file_name)) {
      print(paste(file_name, "existe. Cargando los datos..."))
      data <- read_excel(file_name)
      
      # Asegúrate de que las columnas esperadas existen
      if ("DA_md" %in% colnames(data) && "Fc_emi_md" %in% colnames(data) && "GEI_md" %in% colnames(data)) {
        # Calcular las columnas necesarias si no existen
        data <- data %>%
          mutate(
            S2_DA = (U_DA * DA_md)^2,
            S2_FE_CH4 = (U_Fc_emi * Fc_emi_md)^2,
            S2_GEI_Emi_CH4 = (U_GEI * GEI_md)^2
          )
        
        # Añadir la categoría como una nueva columna
        data$Category <- scat
        all_data[[length(all_data) + 1]] <- data
      } else {
        print(paste("El archivo", file_name, "no contiene las columnas esperadas."))
      }
    } else {
      print(paste(file_name, "NO existe"))
    }
  }
  
  # Si hay datos para combinar, retornar el dataframe combinado
  if (length(all_data) > 0) {
    combined_data <- bind_rows(all_data)
    return(combined_data)
  } else {
    print("No se encontraron archivos para combinar.")
    return(NULL)
  }
}

# Función para calcular las estadísticas (sumas, medias, desviación estándar)
calculate_statistics <- function(data) {
  stats <- data %>%
    group_by(Ano, Category) %>%  # Agrupar por Año y Categoría
    summarise(
      DA_nal = sum(DA_md, na.rm = TRUE),
      S2_DA_nal = sum(S2_DA, na.rm = TRUE),
      FE_CH4_nal = mean(Fc_emi_md, na.rm = TRUE),
      S2_FE_CH4_nal = sum(S2_FE_CH4, na.rm = TRUE),
      GEI_CH4_nal = sum(GEI_md, na.rm = TRUE),
      S2_GEI_CH4_nal = sum(S2_GEI_Emi_CH4, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      U_Fc_emi = sqrt(S2_FE_CH4_nal) / FE_CH4_nal,
      U_DA = sqrt(S2_DA_nal) / DA_nal,
      U_GEI = sqrt(S2_GEI_CH4_nal) / GEI_CH4_nal
    )
  
  return(stats)
}

# Función para guardar el dataframe combinado en un archivo Excel
save_combined_data <- function(combined_data, ano, output_dir) {
  # Verificar si la ruta de salida existe, si no, crearla
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    print(paste("Se ha creado la carpeta:", output_dir))
  }
  
  # Crear el nombre del archivo Excel nacional
  file_path_excel <- file.path(output_dir, paste0("A", ano, "_C3A1A00_N00_pe_E02.xlsx"))
  
  # Verificar si el dataframe combinado no está vacío antes de escribirlo en un archivo
  print(paste("Ruta del archivo a guardar:", file_path_excel))
  
  if (nrow(combined_data) > 0) {
    # Guardar los datos en el archivo Excel
    write_xlsx(combined_data, path = file_path_excel)
    print(paste("Archivo nacional consolidado guardado como:", file_path_excel))
  } else {
    print("No se encontraron datos para guardar.")
  }
}

# Función principal que ejecuta todo el proceso
process_national_consolidated <- function(ano, output_dir) {
  # Paso 1: Combinar los datos de los archivos que comienzan con "C3A"
  combined_data <- combine_and_process_files(ano, output_dir)
  
  if (!is.null(combined_data)) {
    # Paso 2: Calcular las estadísticas
    stats <- calculate_statistics(combined_data)
    
    # Paso 3: Guardar los datos consolidados en un archivo Excel
    save_combined_data(stats, ano, output_dir)
  } else {
    print("No se pudo procesar los datos para la categoría C3A.")
  }
}

# Ejecutar para diferentes años
output_dir <- "D:/Documents/EZunigaA/03_IDEAM/IDEAM_2024/Prueba/A1990"

# Procesar el archivo nacional para el año especificado
process_national_consolidated(ano3, output_dir)

print("Proceso completado.")