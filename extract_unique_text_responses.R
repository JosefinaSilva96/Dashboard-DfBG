# =============================================================================
# extract_unique_text_responses.R
# -----------------------------------------------------------------------------
# Junta TODAS las respuestas de texto libre (unicas, sin duplicados) de las
# tres bases (Agency, Managers, Systems) en un solo CSV, para mandarlas a
# traducir de una sola vez (sin necesidad de la API de Claude).
# Correr desde la carpeta del proyecto (donde esta app.R).
# =============================================================================

source("R/question_dictionary.R")
source("R/manager_questions.R")
source("R/systems_questions.R")
source("R/data_load.R")
source("R/plots.R")

DATA <- load_dfbg()

collect_texts <- function(df, qlist) {
  text_qids <- names(qlist)[vapply(qlist, function(q) identical(q$type, "text"), logical(1))]
  vals <- c()
  for (qid in text_qids) {
    cols <- text_cols(qlist[[qid]], df)
    if (length(cols) == 0) next
    v <- unlist(df[cols], use.names = FALSE)
    v <- v[!is.na(v) & trimws(v) != ""]
    v <- v[!tolower(trimws(v)) %in% c("na", "n/a", "none", "not sure", "-")]
    vals <- c(vals, v)
  }
  vals
}

all_vals <- c(
  collect_texts(DATA$agency,  AGENCY_QUESTIONS),
  collect_texts(DATA$manager, MANAGER_QUESTIONS),
  collect_texts(DATA$systems, SYSTEMS_QUESTIONS)
)

unique_vals <- sort(unique(trimws(all_vals)))
cat("Respuestas unicas encontradas:", length(unique_vals), "\n")

write.csv(data.frame(original_text = unique_vals),
          "unique_text_responses.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Listo -> unique_text_responses.csv\n")
