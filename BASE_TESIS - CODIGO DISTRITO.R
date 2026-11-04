#!/usr/bin/env Rscript
# =============================================================================
# 02_construir_panel.R
# Construccion de la base panel armonizada EPH / EPHC - Paraguay (1998-2025)
# Tesis: impacto de la lluvia en las horas de trabajo
# -----------------------------------------------------------------------------
# Este script:
#   - Define unir_bases_anio()      -> une Personas (R02) con geografia de Vivienda (R01)
#   - Define procesar_ephc_anio()   -> homologa y armoniza variables clave
#   - Define construir_panel()      -> bucle maestro con tryCatch + bind_rows
#   - Guarda base_panel_2002_2025.csv
# =============================================================================

suppressPackageStartupMessages({
  library(haven)
  library(janitor)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
})

proj_dir <- normalizePath(getwd())

# -----------------------------------------------------------------------------
# 0. Utilidades
# -----------------------------------------------------------------------------

limpiar_nombres <- function(df) {
  df <- janitor::clean_names(df)
  names(df) <- tolower(names(df))
  df
}

primer_presente <- function(nms, alternativas) {
  hit <- alternativas[alternativas %in% nms]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

como_num <- function(x) {
  if (is.null(x)) return(NULL)
  suppressWarnings(as.numeric(haven::zap_labels(x)))
}

DEPTO_MAP <- c(
  "0"  = "asuncion",        "1"  = "concepcion",  "2"  = "san_pedro",
  "3"  = "cordillera",      "4"  = "guaira",      "5"  = "caaguazu",
  "6"  = "caazapa",         "7"  = "itapua",      "8"  = "misiones",
  "9"  = "paraguari",       "10" = "alto_parana", "11" = "central",
  "12" = "neembucu",        "13" = "amambay",     "14" = "canindeyu",
  "15" = "presidente_hayes","16" = "boqueron",    "17" = "alto_paraguay"
)

normalizar_depto_texto <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- dplyr::case_when(
    x %in% c("pdte_hayes", "pte_hayes", "presidente_hayes") ~ "presidente_hayes",
    TRUE ~ x
  )
  x
}

# =============================================================================
# PASO 2: FUNCION DE UNION (MERGE)  Personas (R02)  <-  Vivienda (R01)
# =============================================================================
unir_bases_anio <- function(archivo_personas, archivo_vivienda,
                            ruta = proj_dir, verbose = TRUE) {
  
  p <- limpiar_nombres(haven::read_sav(file.path(ruta, archivo_personas)))
  v <- limpiar_nombres(haven::read_sav(file.path(ruta, archivo_vivienda)))
  
  per_key <- primer_presente(names(p), c("upm", "ncontrol"))
  viv_key <- primer_presente(names(v), c("upm", "ncontrol"))
  if (is.na(per_key) || is.na(viv_key))
    stop("No se encontro llave upm/ncontrol en ", archivo_personas, " o ", archivo_vivienda)
  
  if (viv_key != per_key) {
    names(v)[names(v) == viv_key] <- per_key
  }
  
  llaves <- c(per_key, "nvivi", "nhoga")
  faltan <- setdiff(llaves, names(v))
  if (length(faltan))
    stop("Faltan llaves en vivienda: ", paste(faltan, collapse = ", "))
  
  # --- Geografia: departamento y distrito desde vivienda -----------------------
  geo_candidates <- c("depdis", "dptorep", "dpto", "departamento", "distrito", "distri")
  geo_present <- intersect(geo_candidates, names(v))
  if (verbose) message("   geo en vivienda: ", paste(geo_present, collapse = ", "))
  
  v_geo <- v %>%
    select(any_of(c(llaves, geo_present))) %>%
    distinct(!!!rlang::syms(llaves), .keep_all = TRUE)
  
  # --- RENOMBRADO ESTRICTO (AGREGADO) ----------------------------------------
  # 1. Distrito: depdis -> dist_code2
  if ("depdis" %in% names(v_geo)) {
    names(v_geo)[names(v_geo) == "depdis"] <- "dist_code2"
  }
  # 2. Otros renombrados (para no colisionar)
  if ("dptorep" %in% names(v_geo))  names(v_geo)[names(v_geo) == "dptorep"]  <- "dptorep_viv"
  if ("dpto" %in% names(v_geo))     names(v_geo)[names(v_geo) == "dpto"]     <- "dpto_viv"
  if ("distrito" %in% names(v_geo)) names(v_geo)[names(v_geo) == "distrito"] <- "distrito_viv"
  
  df <- dplyr::left_join(p, v_geo, by = llaves)
  
  if (verbose) {
    n_match <- mean(!is.na(df$dist_code2)) * 100
    message(sprintf("   filas=%d  match geografia=%.1f%%", nrow(df), n_match))
  }
  df
}

# =============================================================================
# PASO 3: HOMOLOGACION Y ARMONIZACION
# =============================================================================
procesar_ephc_anio <- function(anio, df_unido) {
  nms <- names(df_unido)
  n <- nrow(df_unido)
  
  # --- 3.1 HORAS DE TRABAJO (semanales) ------------------------------------
  horas <- rep(NA_real_, n)
  for (v in intersect(c("horabco", "horabc", "horab", "b06"), nms)) {
    x <- como_num(df_unido[[v]])
    if (is.null(x)) next
    fill <- is.na(horas) & !is.na(x) & x > 0
    horas[fill] <- x[fill]
  }
  diarias <- intersect(c("b03lu", "b03ma", "b03mi", "b03ju", "b03vi", "b03sa", "b03do"), nms)
  if (length(diarias)) {
    m <- sapply(df_unido[diarias], como_num)
    if (is.null(dim(m))) m <- matrix(m, ncol = length(diarias))
    suma_diaria <- rowSums(m, na.rm = TRUE)
    suma_diaria[apply(is.na(m), 1, all)] <- NA
    fill <- is.na(horas) & !is.na(suma_diaria) & suma_diaria > 0
    horas[fill] <- suma_diaria[fill]
  }
  for (v in intersect(c("p15", "p16"), nms)) {
    x <- como_num(df_unido[[v]])
    if (is.null(x)) next
    mx <- suppressWarnings(max(x, na.rm = TRUE))
    if (!is.finite(mx) || mx < 20 || mx > 168) next 
    fill <- is.na(horas) & !is.na(x) & x > 0
    horas[fill] <- x[fill]
  }
  df_unido$horas <- horas
  df_unido$log_horas <- log1p(df_unido$horas)
  
  # --- 3.2 EXPOSICION AGRO --------------------------------------------------
  rama <- primer_presente(nms, c("rama_pea", "a14rec", "a14", "p27"))
  ocup <- primer_presente(nms, c("ocup_pea", "a13rec", "a13", "p26"))
  
  codigos_agro <- function(x, patrones) {
    l <- attr(x, "labels")
    if (is.null(l)) return(integer(0))
    as.integer(l[grepl(paste(patrones, collapse = "|"), names(l), ignore.case = TRUE)])
  }
  
  rama_chr <- if (!is.na(rama)) as.character(como_num(df_unido[[rama]])) else rep(NA_character_, n)
  ocup_chr <- if (!is.na(ocup)) as.character(como_num(df_unido[[ocup]])) else rep(NA_character_, n)
  
  rama_agro_codes <- if (!is.na(rama)) codigos_agro(df_unido[[rama]], "agricultura") else integer(0)
  ocup_agro_codes <- if (!is.na(ocup)) codigos_agro(df_unido[[ocup]], c("agropec", "agricult")) else integer(0)
  
  if (length(rama_agro_codes)) {
    es_rama_agro <- rama_chr %in% as.character(rama_agro_codes)
  } else {
    es_rama_agro <- !is.na(rama_chr) & startsWith(rama_chr, "1")
  }
  if (length(ocup_agro_codes)) {
    es_ocup_agro <- ocup_chr %in% as.character(ocup_agro_codes)
  } else {
    es_ocup_agro <- !is.na(ocup_chr) & startsWith(ocup_chr, "6")
  }
  
  df_unido$agro <- as.integer(es_rama_agro | es_ocup_agro)
  df_unido$rama_code <- rama_chr
  df_unido$ocup_code <- ocup_chr
  
  # --- 3.3 GEOGRAFIA (DEPTOS) -----------------------------------------------
  dept_code <- rep(NA_integer_, n)
  if ("dist_code2" %in% nms) {
    # depdis suele ser el codigo de distrito con los primeros digitos como depto
    dept_code <- as.integer(floor(como_num(df_unido$dist_code2) / 100))
  }
  if ("dpto_viv" %in% nms) {
    alt <- as.integer(como_num(df_unido$dpto_viv))
    dept_code[is.na(dept_code)] <- alt[is.na(dept_code)]
  }
  per_dpto <- primer_presente(nms, c("dpto", "dptorep", "departamento"))
  if (!is.na(per_dpto)) {
    alt <- como_num(df_unido[[per_dpto]])
    alt[alt == 20] <- NA   
    dept_code[is.na(dept_code)] <- as.integer(alt[is.na(dept_code)])
  }
  df_unido$dept_code <- dept_code
  df_unido$dept_norm <- unname(DEPTO_MAP[as.character(dept_code)])
  
  # --- 3.4 CONTROLES INDIVIDUALES -------------------------------------------
  edad_v <- primer_presente(nms, c("p02", "edad", "p03"))
  df_unido$edad <- if (!is.na(edad_v)) como_num(df_unido[[edad_v]]) else NA_real_
  
  mujer <- rep(NA_integer_, n)
  for (v in intersect(c("p06", "p05"), nms)) {
    x <- como_num(df_unido[[v]])
    if (is.null(x)) next
    fill <- is.na(mujer) & !is.na(x)
    mujer[fill] <- as.integer(x[fill] == 6)
  }
  df_unido$mujer <- mujer
  
  casado <- rep(NA_integer_, n)
  for (v in intersect(c("p09", "p08"), nms)) {
    x <- como_num(df_unido[[v]])
    if (is.null(x)) next
    fill <- is.na(casado) & !is.na(x)
    casado[fill] <- as.integer(x[fill] %in% c(1, 2))
  }
  df_unido$casado <- casado
  
  est_v <- primer_presente(nms, c("anoest", "anioest", "ed0504"))
  df_unido$anios_est <- if (!is.na(est_v)) como_num(df_unido[[est_v]]) else NA_real_
  
  # --- 3.5 FACTOR DE EXPANSION ---------------------------------------------
  fac_v <- primer_presente(nms, c("fex", "fexajustado", "fex_2025", "fex_2024",
                                  "fex_2023", "fex_2022", "factor"))
  df_unido$fex <- if (!is.na(fac_v)) como_num(df_unido[[fac_v]]) else NA_real_
  
  # --- 3.6 ESTADO DE OCUPACION ---------------------------------------------
  ocupado <- rep(NA_integer_, n)
  if ("peaa" %in% nms) {
    x <- como_num(df_unido$peaa); ocupado <- as.integer(x == 1)
  } else if ("pead" %in% nms) {
    x <- como_num(df_unido$pead); ocupado <- as.integer(x == 1)
  }
  df_unido$ocupado <- ocupado
  
  # --- 3.7 SELECCION DE COLUMNAS Y FILTROS ---------------------------------
  # NOTA: Se incluye 'dist_code2' y se cambia 'anio' por 'year'
  out <- df_unido %>%
    transmute(
      year       = as.integer(anio),      # <--- CAMBIADO: anio -> year
      dist_code2 = dist_code2,            # <--- AGREGADO: codigo de distrito
      dept_norm  = dept_norm,
      dept_code  = dept_code,
      horas      = horas,
      log_horas  = log_horas,
      agro       = agro,
      edad       = edad,
      mujer      = mujer,
      casado     = casado,
      anios_est  = anios_est,
      fex        = fex,
      ocupado    = ocupado
    ) %>%
    filter(
      ocupado == 1,
      !is.na(edad), edad >= 15, edad <= 65,
      !is.na(horas), horas > 0, horas <= 168,
      !is.na(dept_norm),
      !is.na(fex), fex > 0
    )
  
  out
}

# =============================================================================
# PASO 4: BUCLE MAESTRO
# =============================================================================
CFG <- list(
  list(anio = 1998, per = "eph1997_1998.sav",  viv = "R01_EIH 1997-1998.sav"),
  list(anio = 1999, per = "eph1999.sav",       viv = "R01_EPH1999.sav"),
  list(anio = 2000, per = "eph2000_2001.sav",  viv = "R01_EIH2000-2001.sav"),
  list(anio = 2002, per = "eph2002.sav",       viv = "R01_EPH2002.sav"),
  list(anio = 2003, per = "eph2003.sav",       viv = "R01_EPH2003.sav"),
  list(anio = 2004, per = "eph2004.sav",       viv = "R01_EPH2004.sav"),
  list(anio = 2005, per = "eph2005.sav",       viv = "R01_EPH2005.SAV"),
  list(anio = 2006, per = "eph2006.sav",       viv = "R01_EPH2006.sav"),
  list(anio = 2007, per = "eph2007.sav",       viv = "R01_EPH2007.sav"),
  list(anio = 2008, per = "eph2008.sav",       viv = "R01_EPH2008.SAV"),
  list(anio = 2009, per = "eph2009.sav",       viv = "R01_EPH2009.SAV"),
  list(anio = 2010, per = "eph2010.sav",       viv = "R01_EPH2010.sav"),
  list(anio = 2011, per = "eph2011.sav",       viv = "R01_EPH2011.Sav"),
  list(anio = 2012, per = "eph2012.sav",       viv = "R01_EPH2012.SAV"),
  list(anio = 2013, per = "eph2013.sav",       viv = "R01_EPH2013.SAV"),
  list(anio = 2014, per = "eph2014.sav",       viv = "R01_EPH2014.SAV"),
  list(anio = 2015, per = "eph2015.sav",       viv = "R01_EPH2015.SAV"),
  list(anio = 2016, per = "eph2016.sav",       viv = "R01_EPH2016.sav"),
  list(anio = 2017, per = "eph2017.sav",       viv = "REG01_EPHC2017.SAV"),
  list(anio = 2018, per = "eph2018.sav",       viv = "REG01_EPHC2018.SAV"),
  list(anio = 2019, per = "eph2019.sav",       viv = "REG01_EPHC2019.SAV"),
  list(anio = 2020, per = "eph2020.sav",       viv = "REG01_EPHC2020.SAV"),
  list(anio = 2021, per = "eph2021.sav",       viv = "REG01_EPHC2021.SAV"),
  list(anio = 2022, per = "eph2022.SAV",       viv = "REG01_EPHC_ANUAL_2022.SAV"),
  list(anio = 2023, per = "ephc_2023.SAV",     viv = "REG01_EPHC_ANUAL_2023.SAV"),
  list(anio = 2024, per = "ephc_2024.SAV",     viv = "REG01_EPHC_ANUAL_2024.SAV"),
  list(anio = 2025, per = "ephc_2025.SAV",     viv = "REG01_EPHC_ANUAL_2025.SAV")
)

construir_panel <- function(cfg = CFG, ruta = proj_dir, solo_anios = NULL) {
  if (!is.null(solo_anios)) cfg <- Filter(function(x) x$anio %in% solo_anios, cfg)
  resultados <- vector("list", length(cfg))
  for (i in seq_along(cfg)) {
    cc <- cfg[[i]]
    message("\n--- Procesando anio ", cc$anio, " (", cc$per, ") ---")
    res <- tryCatch({
      df <- unir_bases_anio(cc$per, cc$viv, ruta = ruta, verbose = TRUE)
      proc <- procesar_ephc_anio(cc$anio, df)
      message("   observaciones finales: ", nrow(proc))
      proc
    }, error = function(e) {
      message("   [ERROR] anio ", cc$anio, ": ", conditionMessage(e))
      NULL
    })
    resultados[[i]] <- res
  }
  ok <- !vapply(resultados, is.null, logical(1))
  message("\nAnios procesados OK: ", sum(ok), " / ", length(cfg))
  dplyr::bind_rows(resultados)
}

# =============================================================================
# EJECUCION
# =============================================================================
if (sys.nframe() == 0) {
  # 1) PRUEBA con un solo anio (2023) antes del bucle completo
  message(">>> PRUEBA PREVIA: anio 2023")
  prueba <- construir_panel(solo_anios = 2023)
  if (nrow(prueba) > 0) {
    message(sprintf("   filas=%d | media horas=%.1f | prop agro=%.3f",
                    nrow(prueba), mean(prueba$horas, na.rm = TRUE),
                    mean(prueba$agro, na.rm = TRUE)))
    # Prueba de que dist_code2 no esté 100% vacío
    message(sprintf("   dist_code2 no nulos=%.1f%%", 
                    mean(!is.na(prueba$dist_code2))*100))
  }
  
  # 2) BUCLE COMPLETO
  message("\n>>> CONSTRUYENDO PANEL COMPLETO")
  base_panel <- construir_panel()
  
  # 3) GUARDAR
  saveRDS(base_panel, file.path(proj_dir, "base_panel_1998_2025.rds"))
  utils::write.csv(base_panel,
                   file.path(proj_dir, "base_panel_1998_2025.csv"), row.names = FALSE)
  message(">> Guardado: base_panel_1998_2025.rds / .csv")
  
  # 4) REPORTE DE CALIDAD
  # (Actualizado a 'year' y agregando el chequeo del distrito)
  rep <- base_panel %>%
    group_by(year) %>%
    summarise(
      n_obs          = n(),
      media_horas    = mean(horas, na.rm = TRUE),
      mediana_horas  = median(horas, na.rm = TRUE),
      prop_agro      = mean(agro, na.rm = TRUE),
      prop_mujer     = mean(mujer, na.rm = TRUE),
      edad_promedio  = mean(edad, na.rm = TRUE),
      anios_est_prom = mean(anios_est, na.rm = TRUE),
      n_deptos       = n_distinct(dept_norm),
      pct_dist_ok    = mean(!is.na(dist_code2)) * 100,
      .groups = "drop"
    )
  rep_lines <- capture.output(
    print(as.data.frame(rep), digits = 4, row.names = FALSE)
  )
  con <- file(file.path(proj_dir, "reporte_calidad.txt"), open = "wt", encoding = "UTF-8")
  writeLines("REPORTE DE CALIDAD - PANEL EPH/EPHC PARAGUAY", con)
  writeLines(sprintf("Generado: %s", Sys.time()), con)
  writeLines(sprintf("Observaciones totales: %d", nrow(base_panel)), con)
  writeLines(sprintf("Anios cubiertos: %d (%d-%d)", n_distinct(base_panel$year),
                     min(base_panel$year), max(base_panel$year)), con)
  writeLines("", con)
  writeLines("Observaciones, promedio de horas y proporcion de trabajadores agropecuarios por anio:", con)
  writeLines(rep_lines, con)
  close(con)
  message(">> Guardado: reporte_calidad.txt")
  
  print(as.data.frame(rep))
}
