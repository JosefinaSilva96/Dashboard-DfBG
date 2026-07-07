# =============================================================================
# question_dictionary.R
# -----------------------------------------------------------------------------
# Diccionario central de preguntas del cuestionario Agency (DfBG).
#
# Esta es la pieza que hace que el dashboard y el country brief sean
# AUTOMATIZADOS: en vez de escribir un bloque a mano por pregunta (como en los
# .Rmd originales), describimos cada pregunta una sola vez aquí —su texto, su
# tipo, las columnas que usa, el orden de las categorías y los colores— y tanto
# el gráfico en el Shiny como la tabla del Word se generan a partir de esta
# especificación.
#
# Tipos soportados:
#   "single"    : una columna select_one (ej. q7, q8, q16). Gráfico de barras
#                 con conteo (ponderado o no) por categoría.
#   "multi"     : varias columnas dummy de un select_multiple (ej. q9a..q9h).
#                 Cada columna es una opción; se grafica Yes/No/Not sure o la
#                 etiqueta de la opción.
#   "barrier"   : baterías tipo Likert de barreras (q26, q27). Se grafican como
#                 heatmap / barras apiladas por constraint.
#   "text"      : respuestas abiertas (ej. q11, q30, q34). No se grafican; se
#                 muestran como texto/tabla y se vuelcan al brief.
#
# NOTA: los `value`/códigos esperados son los de la base YA decodificada por
# build_dfbg_database_EN.R (etiquetas en inglés), pero por robustez muchas
# entradas aceptan tanto el código crudo como la etiqueta.
# =============================================================================

# --- Paletas reutilizables (tomadas de los .Rmd originales) ------------------

PAL_ADOPTION <- c(
  "None"           = "#4A0A0A",
  "Basic"          = "#8B2020",
  "Active Adopter" = "#D4813A",
  "Adapter"        = "#8DC26F",
  "Builder"        = "#2E8B57"
)

PAL_YESNO <- c(
  "Yes"      = "#66c2a4",
  "No"       = "#fb6a4a",
  "Not sure" = "#E9C46A"
)

# Escala de barreras (Likert) — verde = no es barrera ... rojo = barrera mayor
# (mismos niveles y nomenclatura que el Rmd original: Major/Moderate/Minor/Not barrier)
PAL_BARRIER <- c(
  "Not barrier" = "#66c2a4",
  "Minor"       = "#b2e2e2",
  "Moderate"    = "#fcae91",
  "Major"       = "#fb6a4a",
  "Not sure"    = "#BDBDBD"
)

# Mapea valores crudos de las baterías de barreras a los niveles canónicos.
# Igual que el Rmd: NA y cualquier valor desconocido caen en "Not barrier".
normalize_barrier <- function(x) {
  x <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    x %in% c("major", "major barrier", "significant", "3", "severe")      ~ "Major",
    x %in% c("moderate", "moderate barrier", "2")                         ~ "Moderate",
    x %in% c("minor", "minor barrier", "1", "slight")                     ~ "Minor",
    x %in% c("not barrier", "not_barrier", "no barrier", "0", "none")     ~ "Not barrier",
    x %in% c("not sure", "n/a", "na", "don't know", "dont know", "900")   ~ "Not sure",
    is.na(x) | x == ""                                                    ~ "Not barrier",
    TRUE                                                                  ~ "Not barrier"
  )
}

# Helper: recodifica un vector con un named vector, dejando intacto lo que ya
# venga como etiqueta legible.
recode_keep <- function(x, map) {
  x <- as.character(x)
  out <- unname(map[x])
  ifelse(is.na(out), x, out)
}

# Matcher por palabra clave para las preguntas de adopción (q7/q8), cuyas
# respuestas en la base son frases largas, no códigos cortos. Mapea de menos a
# más maduro: None < Basic < Active Adopter < Adapter < Builder.
recode_adoption <- function(x) {
  xl <- tolower(as.character(x))
  dplyr::case_when(
    grepl("not really used|not used in citizen|not used in citizen-facing", xl) ~ "None",
    grepl("ad hoc|limited or ad hoc", xl)                                       ~ "Basic",
    grepl("commercial ai tools", xl)                                            ~ "Active Adopter",
    grepl("customize|customized", xl)                                           ~ "Adapter",
    grepl("in-house|in house|develop in-house|in-house ai", xl)                 ~ "Builder",
    is.na(xl) | xl == ""                                                        ~ NA_character_,
    TRUE                                                                        ~ as.character(x)
  )
}

# Recoder genérico por reglas (lista de pares c(patrón_regex, etiqueta)).
# La primera regla que matchea gana. Lo no matcheado se deja tal cual.
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

recode_q12 <- recode_by_rules(list(
  c("enterprise or government-wide|government-wide license", "Enterprise / government-wide"),
  c("some departments|limited licenses",                     "Limited / departmental"),
  c("case-by-case|individual staff",                         "Case-by-case"),
  c("has not provided|not provided|no .*licenses",           "No"),
  c("not sure",                                              "Not sure")
))
recode_q14 <- recode_by_rules(list(
  c("formal, ministry-wide|formal ministry-wide", "Formal, ministry-wide"),
  c("informal or temporary",                      "Informal / temporary"),
  c("being drafted|currently being",              "In draft"),
  c("no guidelines|there are no",                 "No"),
  c("not sure",                                   "Not sure")
))
recode_q16 <- recode_by_rules(list(
  c("widely used across",        "Adopted & widely used"),
  c("used by some entities",     "Adopted, used by some"),
  c("not yet used",              "Adopted, not yet used"),
  c("under development|in draft","In draft / development"),
  c("no national ai",            "No strategy"),
  c("not sure",                  "Not sure")
))
recode_q18 <- recode_by_rules(list(
  c("both public and private",   "Yes - public & private"),
  c("specifically public sector","Yes - public sector"),
  c("^no$|^no ",                 "No"),
  c("not sure",                  "Not sure")
))
recode_q20 <- recode_by_rules(list(
  c("comprehensive",            "Comprehensive"),
  c("broad",                    "Broad"),
  c("partial",                  "Partial"),
  c("limited",                  "Limited"),
  c("not applicable|not sure",  "Not applicable / Not sure")
))
recode_q32 <- recode_by_rules(list(
  c("clearly positive",  "Clearly positive"),
  c("somewhat positive", "Somewhat positive"),
  c("balanced",          "Balanced"),
  c("somewhat negative", "Somewhat negative"),
  c("clearly negative",  "Clearly negative"),
  c("not sure",          "Not sure")
))

# =============================================================================
# DICCIONARIO DE PREGUNTAS — AGENCY
# =============================================================================
# Cada elemento es una lista con:
#   id        : identificador corto (usado en inputs y como ancla)
#   section   : sección del brief donde aparece
#   title     : texto de la pregunta (encabezado)
#   short     : etiqueta corta para selectores e índices
#   type      : "single" | "multi" | "barrier" | "text"
#   cols      : columna(s) de la base
#   labels    : (single) named vector código->etiqueta
#   levels    : (single/multi) orden de las categorías
#   palette   : colores
#   options   : (multi) named vector col->etiqueta de la opción
#   in_brief  : TRUE si entra al country brief automatizado
# =============================================================================

AGENCY_QUESTIONS <- list(

  q7 = list(
    id = "q7", section = "AI adoption",
    title = "Government use of AI for internal operations (back-end)",
    short = "Backend AI usage",
    type = "single", cols = "q7",
    recoder = recode_adoption,
    levels = c("Builder", "Adapter", "Active Adopter", "Basic", "None"),
    palette = PAL_ADOPTION, in_brief = TRUE
  ),

  q8 = list(
    id = "q8", section = "AI adoption",
    title = "Government use of AI for citizen-facing public services",
    short = "Citizen-facing AI usage",
    type = "single", cols = "q8",
    recoder = recode_adoption,
    levels = c("Builder", "Adapter", "Active Adopter", "Basic", "None"),
    palette = PAL_ADOPTION, in_brief = TRUE
  ),

  q9 = list(
    id = "q9", section = "AI adoption",
    title = "Areas where AI is currently used in government work",
    short = "Areas of AI use",
    type = "multi", cols = c("q9a","q9b","q9c","q9d","q9e","q9f","q9g","q9h"),
    options = c(
      q9a = "Chatbots / virtual assistants for public queries",
      q9b = "Forecasting and predictive decision-making",
      q9c = "Allocation of resources",
      q9d = "Workflow automation and knowledge management",
      q9e = "Supporting frontline providers of public services",
      q9f = "Direct provision of services to citizens or businesses",
      q9g = "Citizen alerts",
      q9h = "Other areas"
    ),
    levels = c("Yes", "No", "Not sure"),
    palette = PAL_YESNO, in_brief = TRUE
  ),

  q11 = list(
    id = "q11", section = "AI adoption",
    title = "Key AI applications currently deployed or piloted",
    short = "Key AI applications",
    type = "text",
    cols = c("q11_1", "q11_2", "q11_3", "q11"),
    in_brief = TRUE
  ),

  q12 = list(
    id = "q12", section = "Generative AI guidelines and capabilities",
    title = "Government provides licenses/official access to generative AI tools",
    short = "GenAI licenses provided",
    type = "single", cols = "q12",
    recoder = recode_q12,
    levels = c("Enterprise / government-wide", "Limited / departmental",
               "Case-by-case", "No", "Not sure"),
    palette = c(
      "Enterprise / government-wide" = "#2E8B57",
      "Limited / departmental"       = "#8DC26F",
      "Case-by-case"                 = "#E9C46A",
      "No"                           = "#8B2020",
      "Not sure"                     = "#BDBDBD"
    ), in_brief = TRUE
  ),

  q14 = list(
    id = "q14", section = "Generative AI guidelines and capabilities",
    title = "Government has issued generative AI use guidelines/policies",
    short = "GenAI use guidelines",
    type = "single", cols = "q14",
    recoder = recode_q14,
    levels = c("Formal, ministry-wide", "Informal / temporary",
               "In draft", "No", "Not sure"),
    palette = c(
      "Formal, ministry-wide" = "#2E8B57",
      "Informal / temporary"  = "#8DC26F",
      "In draft"              = "#D4813A",
      "No"                    = "#8B2020",
      "Not sure"              = "#BDBDBD"
    ), in_brief = TRUE
  ),

  q15 = list(
    id = "q15", section = "Generative AI guidelines and capabilities",
    title = "Topics covered by AI guidelines",
    short = "Guideline topics",
    type = "multi", cols = NULL,           # se detecta automáticamente (q15_*)
    parent = "q15",
    levels = c("Yes", "No", "Not sure"),
    palette = PAL_YESNO, in_brief = TRUE
  ),

  q16 = list(
    id = "q16", section = "Generative AI guidelines and capabilities",
    title = "National AI strategy / policy framework guiding public-sector AI",
    short = "National AI strategy",
    type = "single", cols = "q16",
    recoder = recode_q16,
    levels = c("Adopted & widely used", "Adopted, used by some",
               "Adopted, not yet used", "In draft / development",
               "No strategy", "Not sure"),
    palette = c(
      "Adopted & widely used"  = "#2E8B57",
      "Adopted, used by some"  = "#8DC26F",
      "Adopted, not yet used"  = "#E9C46A",
      "In draft / development" = "#D4813A",
      "No strategy"            = "#8B2020",
      "Not sure"               = "#BDBDBD"
    ), in_brief = TRUE
  ),

  q17 = list(
    id = "q17", section = "Dedicated AI coordination",
    title = "Dedicated unit for AI development and coordination",
    short = "Dedicated AI unit",
    type = "multi", cols = NULL, parent = "q17",
    options = c(
      q17_central_unit      = "Central government AI / digital unit",
      q17_ministry_unit     = "Dedicated AI unit within ministries",
      q17_data_science_unit = "Dedicated data science / analytics unit",
      q17_multiple_units    = "Multiple AI units across ministries",
      q17_none_ad_hoc       = "No dedicated unit — handled ad hoc",
      q17_not_sure          = "Not sure"
    ),
    in_brief = TRUE
  ),

  q18 = list(
    id = "q18", section = "Dedicated AI coordination",
    title = "Body/committee overseeing AI deployment and governance",
    short = "AI oversight body",
    type = "single", cols = "q18",
    recoder = recode_q18,
    levels = c("Yes - public & private", "Yes - public sector", "No", "Not sure"),
    palette = c(
      "Yes - public & private" = "#2E8B57",
      "Yes - public sector"    = "#8DC26F",
      "No"                          = "#8B2020",
      "Not sure"                    = "#BDBDBD"
    ), in_brief = TRUE
  ),

  q20 = list(
    id = "q20", section = "Data infrastructure",
    title = "Implementation status of data platforms / exchanges",
    short = "Data platform status",
    type = "single", cols = "q20",
    recoder = recode_q20,
    levels = c("Comprehensive", "Broad", "Partial", "Limited",
               "Not applicable / Not sure"),
    palette = c(
      "Comprehensive"             = "#2E8B57",
      "Broad"                     = "#8DC26F",
      "Partial"                   = "#E9C46A",
      "Limited"                   = "#D4813A",
      "Not applicable / Not sure" = "#BDBDBD"
    ), in_brief = TRUE
  ),

  q22 = list(
    id = "q22", section = "Data infrastructure",
    title = "AI projects leveraging data sharing / MIS / exchange",
    short = "AI projects leverage data",
    type = "multi", cols = NULL, parent = "q22",
    levels = c("Yes", "No", "Not sure"),
    palette = PAL_YESNO, in_brief = TRUE
  ),

  q23 = list(
    id = "q23", section = "Dedicated budget lines",
    title = "Allocated budget for statistical analysis of government data",
    short = "Budget — data analysis",
    type = "single", cols = "q23",
    levels = c("Yes", "No", "Not sure"),
    palette = PAL_YESNO, in_brief = TRUE
  ),

  q24 = list(
    id = "q24", section = "Dedicated budget lines",
    title = "Allocated budget for government AI projects",
    short = "Budget — AI projects",
    type = "single", cols = "q24",
    levels = c("Yes", "No", "Not sure"),
    palette = PAL_YESNO, in_brief = TRUE
  ),

  q25 = list(
    id = "q25", section = "Dedicated budget lines",
    title = "How AI investment performance is evaluated",
    short = "AI performance evaluation",
    type = "multi", cols = NULL, parent = "q25",
    levels = c("Yes", "No", "Not sure"),
    palette = PAL_YESNO, in_brief = FALSE
  ),

  q26 = list(
    id = "q26", section = "Barriers to AI adoption",
    title = "Constraints limiting internal use of AI tools",
    short = "Barriers — internal AI",
    type = "barrier", cols = NULL, parent = "q26",
    # etiquetas reales de cada constraint (tomadas del Rmd original)
    items = c(
      q26a = "Lack of relevant policies, guidelines, frameworks or standards",
      q26b = "Data privacy, ethical, legal, security or social concerns",
      q26c = "Data quality and availability",
      q26d = "Budget constraints",
      q26e = "Lack of talent to design and develop AI solutions",
      q26f = "Lack of staff skills to effectively use AI tools",
      q26g = "Resistance to change and difficulty obtaining stakeholder support",
      q26h = "Limited political interest and leadership",
      q26i = "Legacy IT systems / interoperability issues",
      q26j = "Concern that AI tools may not work or meet needs",
      q26k = "Lack of clear use cases for the organization's work"
    ),
    levels = names(PAL_BARRIER),
    palette = PAL_BARRIER, in_brief = TRUE
  ),

  q27 = list(
    id = "q27", section = "Barriers to AI adoption",
    title = "Constraints limiting AI in front-facing public services",
    short = "Barriers — citizen-facing AI",
    type = "barrier", cols = NULL, parent = "q27",
    items = c(
      q27a = "Frontline workers' lack of connectivity or digital access",
      q27b = "Frontline workers' lack of skills or training",
      q27c = "Frontline workers' lack of trust in AI tools",
      q27d = "Citizens' limited connectivity or digital access",
      q27e = "Citizens' limitations of skills or training",
      q27f = "Citizens'/providers' preference for in-person services or limited trust",
      q27g = "Limited AI support for local languages"
    ),
    levels = names(PAL_BARRIER),
    palette = PAL_BARRIER, in_brief = TRUE
  ),

  q28 = list(
    id = "q28", section = "Barriers to AI adoption",
    title = "Three main barriers to responsible and effective AI use",
    short = "Top 3 barriers (open text)",
    type = "text", cols = c("q28"), in_brief = TRUE
  ),

  q30 = list(
    id = "q30", section = "Barriers to AI adoption",
    title = "Key initiatives / reforms / programs to address barriers",
    short = "Barrier initiatives (open text)",
    type = "text", cols = c("q30"), in_brief = FALSE
  ),

  q31 = list(
    id = "q31", section = "Responsible AI practices",
    title = "Responsible-AI practices used for deployed/piloted systems",
    short = "Responsible AI practices",
    type = "multi", cols = NULL, parent = "q31",
    levels = c("Yes", "No", "Not sure"),
    palette = PAL_YESNO, in_brief = FALSE
  ),

  q32 = list(
    id = "q32", section = "Responsible AI practices",
    title = "Overall assessment of return on investment of AI projects",
    short = "ROI assessment",
    type = "single", cols = "q32",
    recoder = recode_q32,
    levels = c("Clearly positive", "Somewhat positive", "Balanced",
               "Somewhat negative", "Clearly negative", "Not sure"),
    palette = c(
      "Clearly positive"  = "#2E8B57",
      "Somewhat positive" = "#8DC26F",
      "Balanced"          = "#E9C46A",
      "Somewhat negative" = "#D4813A",
      "Clearly negative"  = "#8B2020",
      "Not sure"          = "#BDBDBD"
    ), in_brief = FALSE
  ),

  q34 = list(
    id = "q34", section = "Top priority actions",
    title = "Three key actions for responsible AI implementation",
    short = "Top 3 priority actions (open text)",
    type = "text", cols = c("q34_1", "q34_2", "q34_3", "q34"), in_brief = TRUE
  )
)

# Orden lógico de secciones para el índice del Shiny y del brief
SECTION_ORDER <- c(
  "AI adoption",
  "Generative AI guidelines and capabilities",
  "Dedicated AI coordination",
  "Data infrastructure",
  "Dedicated budget lines",
  "Barriers to AI adoption",
  "Responsible AI practices",
  "Top priority actions"
)

# Devuelve solo las preguntas cuyas columnas existen realmente en la base.
# Permite que el dashboard no se rompa si una versión de la base no trae
# alguna columna (ej. q15 sin sub-items decodificados).
available_questions <- function(df, qlist = AGENCY_QUESTIONS) {
  keep <- purrr::map_lgl(qlist, function(q) {
    cols <- resolve_cols(q, df)
    length(cols) > 0 && any(cols %in% names(df))
  })
  qlist[keep]
}

# Mapa de familia -> nombre del elemento en el objeto `data` de load_dfbg().
FAMILY_BASE <- c(Agency = "agency", Managers = "manager", Systems = "systems")

# Devuelve el diccionario de preguntas disponible para una familia dada.
# Cada familia tiene SU PROPIO diccionario, porque los cuestionarios difieren:
#   - Agency  -> AGENCY_QUESTIONS  (preguntas q7..q34, sin sector)
#   - Managers -> MANAGER_QUESTIONS (35 preguntas, mismo set para los 6 sectores)
#   - Systems -> SYSTEMS_QUESTIONS (19 preguntas sobre el MIS sectorial)
# `available_questions()` filtra las que efectivamente tienen columnas en la base.
questions_for_family <- function(data, family) {
  base <- FAMILY_BASE[[family]]
  if (is.null(base) || is.null(data[[base]])) return(list())
  qlist <- switch(family,
    Agency   = AGENCY_QUESTIONS,
    Managers = if (exists("MANAGER_QUESTIONS")) MANAGER_QUESTIONS else AGENCY_QUESTIONS,
    Systems  = if (exists("SYSTEMS_QUESTIONS")) SYSTEMS_QUESTIONS else AGENCY_QUESTIONS,
    AGENCY_QUESTIONS
  )
  available_questions(data[[base]], qlist)
}

# Resuelve qué columnas de la base usa una pregunta.
# Prioridad:
#   1) `cols` explícito
#   2) nombres de `items` (barreras) u `options` (multi) que existan en la base
#      — esto cubre q26a/q26b/... y q9a/q9b/... que NO llevan guion bajo
#   3) patrón por prefijo del parent (q15_*, q17_*, ...) como último recurso
resolve_cols <- function(q, df) {
  if (!is.null(q$cols)) {
    return(intersect(q$cols, names(df)))
  }
  # items (barrier) u options (multi) definidos por nombre de columna
  named <- NULL
  if (!is.null(q$items))   named <- names(q$items)
  if (!is.null(q$options)) named <- names(q$options)
  if (!is.null(named)) {
    hit <- intersect(named, names(df))
    if (length(hit) > 0) return(hit)
  }
  # fallback: prefijo del parent con o sin guion bajo
  if (!is.null(q$parent)) {
    pat <- paste0("^", q$parent, "(_|[a-z])")
    return(grep(pat, names(df), value = TRUE))
  }
  character(0)
}
