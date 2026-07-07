# =============================================================================
# systems_questions.R
# -----------------------------------------------------------------------------
# Diccionario completo del cuestionario de Systems (19 preguntas, sobre el MIS).
# Los 6 sectores (CivilService=HRMIS, Education=EDMIS, Health=HMIS,
# Procurement=PMIS, PublicFinance=PFMIS, Tax=TMIS) comparten estructura;
# solo varía la sigla del MIS en los títulos. El texto real lo trae la celda.
# =============================================================================

# Fallback: helpers que normalmente vienen de question_dictionary.R
if (!exists("recode_by_rules", mode = "function")) {
  recode_by_rules <- function(rules) {
    function(x) {
      xl <- tolower(as.character(x))
      out <- rep(NA_character_, length(xl))
      for (r in rules) {
        hit <- is.na(out) & grepl(r[[1]], xl)
        out[hit] <- r[[2]]
      }
      keep <- is.na(out) & !(is.na(xl) | xl == "")
      out[keep] <- as.character(x)[keep]
      out
    }
  }
}
if (!exists("PAL_BARRIER")) {
  PAL_BARRIER <- c(
    "Not barrier" = "#66c2a4", "Minor" = "#b2e2e2",
    "Moderate" = "#fcae91", "Major" = "#fb6a4a", "Not sure" = "#BDBDBD"
  )
}

PAL_MIS_DIGI <- c(
  "Partially digitized"           = "#D4813A",
  "Fully digitized (standalone)"  = "#8DC26F",
  "Fully digitized & interoperable" = "#2E8B57",
  "Paper-based"                   = "#8B2020",
  "Not sure"                      = "#BDBDBD"
)

PAL_ANALYTICS_FREQ <- c(
  "Regularly"     = "#2E8B57",
  "Occasionally"  = "#8DC26F",
  "Rarely"        = "#E9C46A",
  "No"            = "#8B2020",
  "Not sure"      = "#BDBDBD"
)

PAL_ML_STAGE <- c(
  "No"                         = "#8B2020",
  "Yes, in pilots"             = "#8DC26F",
  "Yes, in regular operations" = "#2E8B57",
  "Not sure"                   = "#BDBDBD"
)

PAL_GENAI_USE <- c(
  "No"                                   = "#8B2020",
  "Yes, informal / individual use"       = "#8DC26F",
  "Yes, systematically used by MIS team" = "#2E8B57",
  "Not sure"                             = "#BDBDBD"
)

PAL_GUIDANCE <- c(
  "No guidance"        = "#8B2020",
  "Informal guidance"  = "#8DC26F",
  "Formal guidance"    = "#2E8B57",
  "Not sure"           = "#BDBDBD"
)

# Recoders
recode_s_mis_exists <- recode_by_rules(list(
  c("^yes", "Yes"), c("^no", "No"), c("not sure", "Not sure")
))
recode_s_digi <- recode_by_rules(list(
  c("partially digitized",                              "Partially digitized"),
  c("fully digitized.*not yet integrated|fully digitized but not", "Fully digitized (standalone)"),
  c("fully digitized.*interoperable",                   "Fully digitized & interoperable"),
  c("paper based|paper-based",                          "Paper-based"),
  c("not sure",                                         "Not sure")
))
recode_s_analytics_freq <- recode_by_rules(list(
  c("yes, regularly|^regularly",     "Regularly"),
  c("yes, occasionally|occasionally","Occasionally"),
  c("rarely",                        "Rarely"),
  c("^no,|^no$|not produced",        "No"),
  c("not sure",                      "Not sure")
))
recode_s_yes_no <- recode_by_rules(list(
  c("^yes", "Yes"), c("^no", "No"), c("not sure", "Not sure")
))
recode_s_genai_use <- recode_by_rules(list(
  c("systematically used by",            "Yes, systematically used by MIS team"),
  c("informally|individual use",         "Yes, informal / individual use"),
  c("^no",                               "No"),
  c("not sure",                          "Not sure")
))
recode_s_ml_stage <- recode_by_rules(list(
  c("regular operations",     "Yes, in regular operations"),
  c("in pilots|^yes, in pilots", "Yes, in pilots"),
  c("^no",                    "No"),
  c("not sure",               "Not sure")
))
recode_s_guidance <- recode_by_rules(list(
  c("formal written|formal guidance", "Formal guidance"),
  c("informal",                       "Informal guidance"),
  c("no guidance",                    "No guidance"),
  c("not sure",                       "Not sure")
))

# =============================================================================
# SYSTEMS_QUESTIONS — 19 preguntas
# =============================================================================
SYSTEMS_QUESTIONS <- list(

  # -- MIS FOUNDATIONS ----------------------------------------------------
  q7 = list(
    id = "q7", section = "MIS foundations",
    title = "MIS in place in the ministry/agency",
    short = "MIS exists",
    type = "single", cols = "q7", recoder = recode_s_mis_exists,
    levels = c("Yes","No","Not sure"),
    palette = c("Yes" = "#2E8B57","No" = "#8B2020","Not sure" = "#BDBDBD"),
    in_brief = TRUE
  ),
  q7_1 = list(
    id = "q7_1", section = "MIS foundations",
    title = "Degree of digitization of the MIS",
    short = "MIS — digitization",
    type = "single", cols = "q7_1", recoder = recode_s_digi,
    levels = names(PAL_MIS_DIGI), palette = PAL_MIS_DIGI, in_brief = TRUE
  ),
  q8 = list(
    id = "q8", section = "MIS foundations",
    title = "Data element categories available in the MIS",
    short = "Data elements in MIS",
    type = "multi", cols = NULL, parent = "q8",
    in_brief = TRUE
  ),

  # -- ANALYTICAL OUTPUTS -------------------------------------------------
  q9 = list(
    id = "q9", section = "Analytical outputs",
    title = "Production of analytical outputs from MIS data",
    short = "Analytical outputs",
    type = "single", cols = "q9", recoder = recode_s_analytics_freq,
    levels = names(PAL_ANALYTICS_FREQ), palette = PAL_ANALYTICS_FREQ, in_brief = TRUE
  ),
  q9a = list(
    id = "q9a", section = "Analytical outputs",
    title = "Main uses of analytical products",
    short = "Uses of analytical products",
    type = "multi", cols = NULL, parent = "q9a",
    in_brief = TRUE
  ),

  # -- FUNDING / COLLABORATION -------------------------------------------
  q10 = list(
    id = "q10", section = "Funding & collaboration",
    title = "Internal funding for analytics / AI projects using MIS data",
    short = "Internal funding",
    type = "single", cols = "q10", recoder = recode_s_yes_no,
    levels = c("Yes","No","Not sure"),
    palette = c("Yes" = "#2E8B57","No" = "#8B2020","Not sure" = "#BDBDBD"),
    in_brief = TRUE
  ),
  q11 = list(
    id = "q11", section = "Funding & collaboration",
    title = "Strategy to collaborate on analytics/AI using MIS data",
    short = "Collaboration strategy",
    type = "single", cols = "q11", recoder = recode_s_yes_no,
    levels = c("Yes","No","Not sure"),
    palette = c("Yes" = "#2E8B57","No" = "#8B2020","Not sure" = "#BDBDBD"),
    in_brief = TRUE
  ),
  q11a = list(
    id = "q11a", section = "Funding & collaboration",
    title = "Main modes of collaboration",
    short = "Collaboration modes",
    type = "multi", cols = NULL, parent = "q11a",
    options = c(
      q11a_1 = "Technical assistance",
      q11a_2 = "Data access / sharing agreements",
      q11a_3 = "Joint innovation in analytics/AI methods",
      q11a_4 = "Financial support"
    ),
    in_brief = FALSE
  ),

  # -- AI/ML ADOPTION -----------------------------------------------------
  q12 = list(
    id = "q12", section = "AI/ML around the MIS",
    title = "AI/ML use linked to the MIS in the past 3 years",
    short = "AI/ML uses on MIS",
    type = "multi", cols = c("q12a","q12b","q12c","q12d","q12e","q12f","q12g"),
    options = c(
      q12a = "Predictive analytics / forecasting",
      q12b = "Risk scoring / anomaly / fraud detection",
      q12c = "Automated data quality checks",
      q12d = "Workflow automation in the MIS",
      q12e = "Decision support for staff / providers",
      q12f = "Chatbots / virtual assistants on MIS info",
      q12g = "Other AI/ML use"
    ),
    in_brief = TRUE
  ),
  q13 = list(
    id = "q13", section = "AI/ML around the MIS",
    title = "AI/ML applications using MIS data",
    short = "Key AI/ML applications",
    type = "text", cols = c("q13_1","q13_2","q13_3"), in_brief = TRUE
  ),

  # -- AI FOR MIS OPERATIONS ----------------------------------------------
  q14 = list(
    id = "q14", section = "AI for MIS operations",
    title = "MIS/IT staff using GenAI tools for technical work",
    short = "MIS staff use GenAI",
    type = "single", cols = "q14", recoder = recode_s_genai_use,
    levels = names(PAL_GENAI_USE), palette = PAL_GENAI_USE, in_brief = TRUE
  ),
  q14a = list(
    id = "q14a", section = "AI for MIS operations",
    title = "Tasks GenAI tools are used for",
    short = "GenAI tasks",
    type = "multi", cols = NULL, parent = "q14a",
    options = c(
      q14a_1 = "Writing / debugging code",
      q14a_2 = "Building / maintaining data pipelines",
      q14a_3 = "Drafting metadata / documentation",
      q14a_4 = "Creating / refining dashboards",
      q14a_5 = "Data cleaning suggestions",
      q14a_6 = "Helpdesk / user support"
    ),
    in_brief = FALSE
  ),
  q15 = list(
    id = "q15", section = "AI for MIS operations",
    title = "MIS/IT staff using ML models for MIS data / workflows",
    short = "MIS staff use ML",
    type = "single", cols = "q15", recoder = recode_s_ml_stage,
    levels = names(PAL_ML_STAGE), palette = PAL_ML_STAGE, in_brief = TRUE
  ),
  q15a = list(
    id = "q15a", section = "AI for MIS operations",
    title = "ML-enabled functions used",
    short = "ML functions",
    type = "multi", cols = NULL, parent = "q15a",
    options = c(
      q15a_1 = "Automated validation / QA rules",
      q15a_2 = "Record linkage / deduplication",
      q15a_3 = "Classification / tagging",
      q15a_4 = "Forecasting / early warning",
      q15a_5 = "Prioritization / triage",
      q15a_6 = "NLP of documents / forms"
    ),
    in_brief = FALSE
  ),
  q16 = list(
    id = "q16", section = "AI for MIS operations",
    title = "Technical guidance on data use with AI/ML/GenAI tools",
    short = "Technical AI guidance",
    type = "single", cols = "q16", recoder = recode_s_guidance,
    levels = names(PAL_GUIDANCE), palette = PAL_GUIDANCE, in_brief = TRUE
  ),
  q16a = list(
    id = "q16a", section = "AI for MIS operations",
    title = "What the guidance covers",
    short = "Guidance coverage",
    type = "multi", cols = NULL, parent = "q16a",
    options = c(
      q16a_1 = "Restrictions on sensitive data",
      q16a_2 = "Anonymization requirements",
      q16a_3 = "Approved AI tools / environments",
      q16a_4 = "Human oversight required",
      q16a_5 = "Security / logging requirements"
    ),
    in_brief = FALSE
  ),

  # -- BARRIERS & SAFEGUARDS ---------------------------------------------
  q17 = list(
    id = "q17", section = "Barriers",
    title = "Barriers limiting advanced analytics or AI on MIS data",
    short = "Barriers (Likert)",
    type = "barrier", cols = NULL, parent = "q17",
    items = c(
      q17a = "Data quality / completeness / timeliness",
      q17b = "Lack of interoperability / unique IDs",
      q17c = "Limited compute / cloud infrastructure",
      q17d = "Privacy / legal restrictions",
      q17e = "Lack of defined standards",
      q17f = "Skills gaps in MIS/IT team"
    ),
    levels = names(PAL_BARRIER), palette = PAL_BARRIER, in_brief = TRUE
  ),
  q18 = list(
    id = "q18", section = "Barriers",
    title = "Three main barriers",
    short = "Top 3 barriers",
    type = "multi", cols = NULL, parent = "q18",
    in_brief = TRUE
  ),
  q19 = list(
    id = "q19", section = "Safeguards",
    title = "Safeguards used for AI systems relying on MIS data",
    short = "Safeguards used",
    type = "multi", cols = NULL, parent = "q19",
    in_brief = TRUE
  )
)

SYSTEMS_SECTION_ORDER <- c(
  "MIS foundations", "Analytical outputs", "Funding & collaboration",
  "AI/ML around the MIS", "AI for MIS operations", "Barriers", "Safeguards"
)
