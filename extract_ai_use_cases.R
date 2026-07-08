# =============================================================================
# extract_ai_use_cases.R
# -----------------------------------------------------------------------------
# Junta TODAS las respuestas de "aplicaciones de IA" (Agency q11, Manager q11,
# Systems q13) de TODOS los países y sectores, en un solo CSV para clasificar
# por categoría y usar en la pestaña "AI Use Cases".
# Correr desde la carpeta del proyecto (donde esta app.R).
# =============================================================================

source("R/question_dictionary.R")
source("R/manager_questions.R")
source("R/systems_questions.R")
source("R/data_load.R")

DATA <- load_dfbg()

collect_apps <- function(df, cols, context_col = NULL) {
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) return(tibble::tibble())
  purrr::map_dfr(cols, function(cc) {
    tibble::tibble(
      country = df$country,
      context = if (!is.null(context_col) && context_col %in% names(df))
                  stringr::str_remove(df[[context_col]], "^(Systems|Manager)_")
                else "Agency",
      text = df[[cc]]
    )
  })
}

agency_apps  <- collect_apps(DATA$agency,  c("q11_1", "q11_2", "q11_3"))
manager_apps <- collect_apps(DATA$manager, c("q11_1", "q11_2", "q11_3"), "questionnaire")
systems_apps <- collect_apps(DATA$systems, c("q13_1", "q13_2", "q13_3"), "questionnaire")

all_apps <- dplyr::bind_rows(
  dplyr::mutate(agency_apps,  questionnaire = "Agency"),
  dplyr::mutate(manager_apps, questionnaire = "Manager"),
  dplyr::mutate(systems_apps, questionnaire = "Systems")
) |>
  dplyr::filter(!is.na(text), trimws(text) != "") |>
  dplyr::filter(!tolower(trimws(text)) %in% c("na", "n/a", "none", "not sure", "-")) |>
  dplyr::distinct(country, questionnaire, context, text) |>
  dplyr::arrange(country, questionnaire, context)

cat("Total de aplicaciones de IA encontradas:", nrow(all_apps), "\n")
write.csv(all_apps, "ai_use_cases_raw.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Listo -> ai_use_cases_raw.csv\n")
