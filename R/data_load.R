# =============================================================================
# data_load.R
# -----------------------------------------------------------------------------
# Carga las 3 bases limpias que produce build_dfbg_database_EN.R y prepara
# objetos auxiliares (income groups, lista de países, etc.).
#
# Espera encontrar en data/:
#   dfbg_agency.rds, dfbg_managers.rds, dfbg_systems.rds   (obligatorio)
#   CLASS_2025_10_07.xlsx                                  (opcional: income groups)
#
# Si no tenés los .rds a mano, podés exportarlos desde build_dfbg_database_EN.R
# (ya los guarda) o usar los .csv equivalentes — ver load_dfbg() abajo.
# =============================================================================

DATA_PATH <- file.path("data")

# --- Income groups -----------------------------------------------------------
# Replica el merge de DfBG_Agency_Income_Group.Rmd. Si no está el CLASS.xlsx,
# cae a un mapeo mínimo embebido para que el dashboard igual funcione.

load_income_groups <- function(path = DATA_PATH) {
  class_file <- list.files(path, pattern = "^CLASS.*\\.xlsx$", full.names = TRUE)
  if (length(class_file) >= 1) {
    cls <- readxl::read_excel(class_file[1]) |> janitor::clean_names()
    # columnas típicas del WB CLASS: economy, code, income_group
    code_col   <- intersect(c("code", "iso3c", "country_code"), names(cls))[1]
    income_col <- intersect(c("income_group", "income_group_1", "group"), names(cls))[1]
    if (!is.na(code_col) && !is.na(income_col)) {
      out <- cls |>
        dplyr::transmute(
          iso3c        = toupper(.data[[code_col]]),
          income_group = .data[[income_col]]
        ) |>
        dplyr::filter(!is.na(iso3c), !is.na(income_group)) |>
        dplyr::distinct()
      return(out)
    }
  }
  message("CLASS_*.xlsx not found or missing expected columns: ",
          "income groups will remain NA. Add the file to data/ to enable them.")
  tibble::tibble(iso3c = character(), income_group = character())
}

# --- Carga principal ---------------------------------------------------------

load_dfbg <- function(path = DATA_PATH) {

  read_one <- function(stub) {
    rds <- file.path(path, paste0(stub, ".rds"))
    csv <- file.path(path, paste0(stub, ".csv"))
    if (file.exists(rds)) return(readRDS(rds))
    if (file.exists(csv)) return(readr::read_csv(csv, show_col_types = FALSE))
    stop("Could not find ", stub, ".rds or ", stub, ".csv in ", path)
  }

  agency  <- read_one("dfbg_agency_public")
  manager <- read_one("dfbg_managers_public")
  systems <- read_one("dfbg_systems_public")

  # Normaliza nombres de columnas a minúsculas para empatar el diccionario
  agency  <- janitor::clean_names(agency)
  manager <- janitor::clean_names(manager)
  systems <- janitor::clean_names(systems)

  ig <- load_income_groups(path)
  if (nrow(ig) > 0) {
    join_ig <- function(df) {
      if ("iso3c" %in% names(df)) {
        df |> dplyr::left_join(ig, by = "iso3c")
      } else df
    }
    agency  <- join_ig(agency)
    manager <- join_ig(manager)
    systems <- join_ig(systems)
  } else {
    # Sin CLASS file: aseguramos que la columna exista (toda NA) en las 3 bases
    if (!"income_group" %in% names(agency))  agency$income_group  <- NA_character_
    if (!"income_group" %in% names(manager)) manager$income_group <- NA_character_
    if (!"income_group" %in% names(systems)) systems$income_group <- NA_character_
  }

  list(
    agency  = agency,
    manager = manager,
    systems = systems,
    income_groups = ig
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Lista de países disponibles (con al menos respuesta Agency)
country_choices <- function(data) {
  ag <- data$agency
  cc <- ag |>
    dplyr::filter(!is.na(country)) |>
    dplyr::distinct(country, income_group) |>
    dplyr::arrange(country)
  cc
}

# Metadatos de cabecera para un país (income group, # cuestionarios, agencias)
country_header <- function(data, country_name) {
  ag <- dplyr::filter(data$agency,  country == country_name)
  mg <- dplyr::filter(data$manager, country == country_name)
  sy <- dplyr::filter(data$systems, country == country_name)

  ig <- ag$income_group[1]
  if (is.na(ig) || length(ig) == 0) ig <- "Not classified"

  n_sys <- dplyr::n_distinct(sy$questionnaire)
  n_mgr <- dplyr::n_distinct(mg$questionnaire)
  n_ag  <- as.integer(nrow(ag) > 0)
  n_total <- n_ag + n_sys + n_mgr

  list(
    country      = country_name,
    income_group = ig,
    n_total      = n_total,
    n_agency     = n_ag,
    n_systems    = n_sys,
    n_managers   = n_mgr
  )
}
