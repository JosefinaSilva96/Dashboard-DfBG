# =============================================================================
# manager_questions.R
# -----------------------------------------------------------------------------
# Diccionario completo del cuestionario de Managers (35 preguntas).
# Los 6 sectores (CivilService, Education, Health, Procurement, PublicFinance,
# Tax) comparten la MISMA estructura — solo varía el wording dentro de los
# "(e.g., ...)" de algunas opciones (q9, q17, q22). Eso lo absorben los datos:
# como el texto real de la opción se guarda en la celda, los gráficos sectores
# muestran las etiquetas correctas.
# =============================================================================

# Fallback: definir recode_by_rules si no fue cargado todavía
# (normalmente viene de question_dictionary.R, pero esto hace el módulo robusto
#  al orden de los source()).
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

# Paletas específicas para Managers
PAL_ADOPTION_M <- c(
  "None"           = "#4A0A0A",
  "Basic"          = "#8B2020",
  "Active Adopter" = "#D4813A",
  "Adapter"        = "#8DC26F",
  "Builder"        = "#2E8B57",
  "Not sure"       = "#BDBDBD"
)

PAL_YESNO_M <- c("Yes" = "#66c2a4", "No" = "#fb6a4a", "Not sure" = "#E9C46A")

PAL_EFFECT <- c(
  "Strongly decreased" = "#8B2020",
  "Decreased"          = "#D4813A",
  "No effect"          = "#BDBDBD",
  "Increased"          = "#8DC26F",
  "Strongly increased" = "#2E8B57",
  "Not sure"           = "#E9C46A"
)

PAL_INSTIT <- c(
  "Not institutionalized"     = "#8B2020",
  "Partially institutionalized" = "#D4813A",
  "Institutionalized"         = "#8DC26F",
  "Fully institutionalized"   = "#2E8B57",
  "Not sure"                  = "#BDBDBD"
)

PAL_COVERAGE <- c(
  "Very limited"   = "#8B2020",
  "Basic"          = "#D4813A",
  "Moderate"       = "#E9C46A",
  "Broad"          = "#8DC26F",
  "Comprehensive"  = "#2E8B57",
  "Not sure"       = "#BDBDBD"
)

PAL_SPECIALIZATION <- c(
  "Basic"        = "#D4813A",
  "Intermediate" = "#8DC26F",
  "Advanced"     = "#2E8B57",
  "Not sure"     = "#BDBDBD"
)

# Recoders por patrón (mismo enfoque que en Agency)
recode_m_adoption <- function(x) {
  xl <- tolower(as.character(x))
  dplyr::case_when(
    grepl("not really used|not used in citizen|not used in citizen-facing", xl) ~ "None",
    grepl("ad hoc|limited or ad hoc", xl)                                       ~ "Basic",
    grepl("commercial ai tools", xl)                                            ~ "Active Adopter",
    grepl("customize|customized", xl)                                           ~ "Adapter",
    grepl("in-house|in house|develop in-house|in-house ai", xl)                 ~ "Builder",
    grepl("not sure", xl)                                                       ~ "Not sure",
    is.na(xl) | xl == ""                                                        ~ NA_character_,
    TRUE                                                                        ~ as.character(x)
  )
}

recode_m_q12 <- recode_by_rules(list(
  c("enterprise or ministry-wide|enterprise.*license", "Enterprise / ministry-wide"),
  c("some departments|limited licenses",               "Limited / departmental"),
  c("case-by-case|individual staff",                   "Case-by-case"),
  c("restricted or prohibited",                        "Restricted / prohibited"),
  c("has not provided|no .*licenses",                  "No"),
  c("not sure",                                        "Not sure")
))

recode_m_q13 <- recode_by_rules(list(
  c("formal, ministry-wide|formal ministry-wide", "Formal, ministry-wide"),
  c("informal or temporary",                      "Informal / temporary"),
  c("only at the central",                        "Central government only"),
  c("being drafted|currently being",              "In draft"),
  c("no guidelines|there are no",                 "No"),
  c("not sure",                                   "Not sure")
))

recode_m_q16 <- recode_by_rules(list(
  c("^yes$|^yes ", "Yes"),
  c("^no$|^no ",   "No"),
  c("not sure",    "Not sure")
))

recode_m_q18 <- recode_by_rules(list(
  c("mainly descriptive",        "Mainly descriptive"),
  c("mainly diagnostic",         "Mainly diagnostic"),
  c("mainly predictive",         "Mainly predictive"),
  c("mix of descriptive",        "Mix (descriptive/diagnostic/predictive)"),
  c("don.t know|not sure",       "Not sure")
))

recode_m_effect <- recode_by_rules(list(
  c("strongly increased", "Strongly increased"),
  c("strongly decreased", "Strongly decreased"),
  c("^increased",         "Increased"),
  c("^decreased",         "Decreased"),
  c("no effect",          "No effect"),
  c("not sure",           "Not sure")
))

recode_m_yes_no <- recode_by_rules(list(
  c("^yes",     "Yes"),
  c("^no",      "No"),
  c("not sure", "Not sure")
))

recode_m_instit <- recode_by_rules(list(
  c("fully institutionalized",     "Fully institutionalized"),
  c("partially institutionalized", "Partially institutionalized"),
  c("^institutionalized",          "Institutionalized"),
  c("not institutionalized",       "Not institutionalized"),
  c("not sure",                    "Not sure")
))

recode_m_coverage <- recode_by_rules(list(
  c("comprehensive", "Comprehensive"),
  c("very limited",  "Very limited"),
  c("^broad",        "Broad"),
  c("moderate",      "Moderate"),
  c("^basic",        "Basic"),
  c("not sure",      "Not sure")
))

recode_m_specialization <- recode_by_rules(list(
  c("^advanced",     "Advanced"),
  c("^intermediate", "Intermediate"),
  c("^basic",        "Basic"),
  c("not sure",      "Not sure")
))

recode_m_emphasis <- recode_by_rules(list(
  c("responsible and safe",        "Responsible / safe use"),
  c("practical use for",           "Practical use"),
  c("balanced mix",                "Balanced"),
  c("not sure",                    "Not sure")
))

recode_m_unit <- recode_by_rules(list(
  c("central government ai|central .*digital innovation",     "Central AI / digital unit"),
  c("dedicated ai unit within my",                            "Dedicated AI unit (ministry)"),
  c("dedicated data science|analytics unit",                  "Data science / analytics unit"),
  c("multiple ai or data units",                              "Multiple units across ministries"),
  c("no dedicated unit|handled ad hoc",                       "No dedicated unit"),
  c("not sure",                                               "Not sure")
))

# =============================================================================
# MANAGER_QUESTIONS — 35 preguntas
# =============================================================================
MANAGER_QUESTIONS <- list(

  # -- AI ADOPTION ---------------------------------------------------------
  q7 = list(
    id = "q7", section = "AI adoption",
    title = "Use of AI for internal operations (back-end)",
    short = "Backend AI usage",
    type = "single", cols = "q7", recoder = recode_m_adoption,
    levels = c("Builder","Adapter","Active Adopter","Basic","None","Not sure"),
    palette = PAL_ADOPTION_M, in_brief = TRUE
  ),
  q8 = list(
    id = "q8", section = "AI adoption",
    title = "Use of AI for citizen/firm-facing public services",
    short = "Citizen-facing AI usage",
    type = "single", cols = "q8", recoder = recode_m_adoption,
    levels = c("Builder","Adapter","Active Adopter","Basic","None","Not sure"),
    palette = PAL_ADOPTION_M, in_brief = TRUE
  ),
  q9 = list(
    id = "q9", section = "AI adoption",
    title = "Areas where AI has been used in the past 3 years",
    short = "Areas of AI use",
    type = "multi", cols = c("q9a","q9b","q9c","q9d","q9e","q9f","q9g"),
    options = c(
      q9a = "Chatbots / virtual assistants",
      q9b = "Forecasting and predictive decision-making",
      q9c = "Allocation of resources",
      q9d = "Workflow automation / knowledge management",
      q9e = "Supporting frontline providers",
      q9f = "Direct provision of services",
      q9g = "Citizen or sector alerts"
    ),
    in_brief = TRUE
  ),
  q10 = list(id = "q10", section = "AI adoption", title = "Other area of AI use",
             short = "Other AI use (text)", type = "text", cols = "q10", in_brief = FALSE),
  q11 = list(
    id = "q11", section = "AI adoption",
    title = "Top AI applications deployed or piloted",
    short = "Key AI applications",
    type = "text", cols = c("q11_1","q11_2","q11_3"), in_brief = TRUE
  ),

  # -- GENERATIVE AI GOVERNANCE -------------------------------------------
  q12 = list(
    id = "q12", section = "Generative AI",
    title = "Licenses or official access to generative AI tools",
    short = "GenAI licenses",
    type = "single", cols = "q12", recoder = recode_m_q12,
    levels = c("Enterprise / ministry-wide","Limited / departmental","Case-by-case",
               "No","Restricted / prohibited","Not sure"),
    palette = c(
      "Enterprise / ministry-wide" = "#2E8B57", "Limited / departmental" = "#8DC26F",
      "Case-by-case" = "#E9C46A", "No" = "#8B2020",
      "Restricted / prohibited" = "#4A0A0A", "Not sure" = "#BDBDBD"
    ), in_brief = TRUE
  ),
  q13 = list(
    id = "q13", section = "Generative AI",
    title = "Guidelines on generative AI use by civil servants",
    short = "GenAI guidelines",
    type = "single", cols = "q13", recoder = recode_m_q13,
    levels = c("Formal, ministry-wide","Informal / temporary","Central government only",
               "In draft","No","Not sure"),
    palette = c(
      "Formal, ministry-wide" = "#2E8B57", "Informal / temporary" = "#8DC26F",
      "Central government only" = "#E9C46A", "In draft" = "#D4813A",
      "No" = "#8B2020", "Not sure" = "#BDBDBD"
    ), in_brief = TRUE
  ),
  q14 = list(
    id = "q14", section = "Generative AI",
    title = "Topics covered by GenAI guidelines",
    short = "Topics of GenAI guidelines",
    type = "multi", cols = NULL, parent = "q14",
    options = c(
      q14_1 = "Data protection / privacy",
      q14_2 = "Restrictions on sensitive data",
      q14_3 = "Acceptable use cases",
      q14_4 = "Prohibited use cases",
      q14_5 = "Human review / oversight",
      q14_6 = "Vendor restrictions"
    ),
    in_brief = FALSE
  ),
  q15 = list(id = "q15", section = "Generative AI", title = "Other GenAI topic (specify)",
             short = "Other GenAI topic", type = "text", cols = "q15", in_brief = FALSE),

  # -- DATA ANALYTICS -----------------------------------------------------
  q16 = list(
    id = "q16", section = "Data analytics",
    title = "Currently using data analytics products to support decisions",
    short = "Uses data analytics products",
    type = "single", cols = "q16", recoder = recode_m_q16,
    levels = c("Yes","No","Not sure"), palette = PAL_YESNO_M, in_brief = TRUE
  ),
  q17 = list(
    id = "q17", section = "Data analytics",
    title = "Main uses of data analytics products",
    short = "Uses of data analytics",
    type = "multi", cols = c("q17_1","q17_2","q17_3","q17_4","q17_5","q17_6"),
    options = c(
      q17_1 = "Monitoring",
      q17_2 = "Accountability",
      q17_3 = "Transparency toward citizens",
      q17_4 = "Policy evaluation",
      q17_5 = "Policy design",
      q17_6 = "Operational management"
    ),
    in_brief = TRUE
  ),
  q18 = list(
    id = "q18", section = "Data analytics",
    title = "Predominant type of data analytics",
    short = "Type of analytics",
    type = "single", cols = "q18", recoder = recode_m_q18,
    levels = c("Mainly descriptive","Mainly diagnostic","Mainly predictive",
               "Mix (descriptive/diagnostic/predictive)","Not sure"),
    palette = c(
      "Mainly descriptive" = "#BDBDBD","Mainly diagnostic" = "#E9C46A",
      "Mainly predictive" = "#2E8B57",
      "Mix (descriptive/diagnostic/predictive)" = "#8DC26F","Not sure" = "#4A0A0A"
    ), in_brief = TRUE
  ),

  # -- BARRIERS -----------------------------------------------------------
  q19 = list(
    id = "q19", section = "Barriers",
    title = "Severity of barriers to AI use (Likert)",
    short = "Barrier severity",
    type = "barrier", cols = NULL, parent = "q19",
    items = c(
      q19a = "Lack of policies/guidelines",
      q19b = "Data privacy / ethical concerns",
      q19c = "Data quality / availability",
      q19d = "Budget constraints",
      q19e = "Lack of talent (design/develop)",
      q19f = "Lack of staff skills (use)",
      q19g = "Resistance to change",
      q19h = "Limited political interest",
      q19i = "Legacy IT / interoperability",
      q19j = "Concern AI may not meet needs"
    ),
    levels = names(PAL_BARRIER), palette = PAL_BARRIER, in_brief = TRUE
  ),
  q20 = list(
    id = "q20", section = "Barriers",
    title = "Top three barriers to responsible AI use",
    short = "Top 3 barriers",
    type = "multi", cols = NULL, parent = "q20",
    options = c(
      q20_1 = "Lack of policies/guidelines",
      q20_2 = "Data privacy / ethical concerns",
      q20_3 = "Data quality / availability",
      q20_4 = "Budget constraints",
      q20_5 = "Insufficient technical expertise",
      q20_6 = "Resistance to change",
      q20_7 = "Limited political interest",
      q20_8 = "Legacy IT / interoperability",
      q20_9 = "Concern AI may not meet needs"
    ),
    in_brief = TRUE
  ),

  # -- SAFEGUARDS / POTENTIAL --------------------------------------------
  q21 = list(
    id = "q21", section = "Responsible AI",
    title = "Responsible-AI practices used for deployed/piloted systems",
    short = "Responsible AI practices",
    type = "multi", cols = NULL, parent = "q21",
    options = c(
      q21_1 = "Formal risk / impact assessments",
      q21_2 = "Data protection / privacy impact assessments",
      q21_3 = "Ethical / human-rights assessments",
      q21_4 = "Performance / robustness / security testing",
      q21_5 = "Checks for bias",
      q21_6 = "Public communication on AI systems",
      q21_8 = "Consultation with citizens / civil society",
      q21_9 = "Citizen notice / opt-out",
      q21_10 = "None of the above"
    ),
    in_brief = FALSE
  ),
  q22 = list(
    id = "q22", section = "Potential",
    title = "Greatest potential areas for AI to improve performance",
    short = "AI potential areas",
    type = "multi", cols = NULL, parent = "q22",
    in_brief = FALSE
  ),

  # -- CHANGE IN WORK -----------------------------------------------------
  q23 = list(
    id = "q23", section = "Impact of AI",
    title = "How AI has changed your ministry/agency's work",
    short = "Impact of AI on work",
    type = "barrier", cols = NULL, parent = "q23",
    items = c(
      q23a = "Efficiency / time savings",
      q23b = "Quality of work",
      q23c = "Quantity / volume of work",
      q23d = "Delivery of public services"
    ),
    levels = names(PAL_EFFECT), palette = PAL_EFFECT, in_brief = TRUE
  ),

  # -- SUPPORT NEEDED -----------------------------------------------------
  q24 = list(
    id = "q24", section = "Support needed",
    title = "Most useful support to realize AI opportunities",
    short = "Support needed",
    type = "multi", cols = NULL, parent = "q24",
    options = c(
      q24_1 = "Training and capacity building",
      q24_2 = "Better access to data / computing",
      q24_3 = "Partnerships with universities / private",
      q24_4 = "Funding for pilots",
      q24_5 = "Guidance on ethics / responsible AI",
      q24_6 = "Central technical support",
      q24_7 = "Hiring AI development experts",
      q24_8 = "Hiring AI governance experts"
    ),
    in_brief = TRUE
  ),

  # -- SKILLS, ROLES, INSTITUTIONAL CAPACITY ------------------------------
  q25 = list(
    id = "q25", section = "Skills & training",
    title = "Internal training that supports statistical analysis",
    short = "Data analytics training",
    type = "single", cols = "q25", recoder = recode_m_yes_no,
    levels = c("Yes","No","Not sure"), palette = PAL_YESNO_M, in_brief = TRUE
  ),
  q26 = list(
    id = "q26", section = "Skills & training",
    title = "Institutionalization of the data analytics training",
    short = "DA training — institutionalization",
    type = "single", cols = "q26", recoder = recode_m_instit,
    levels = names(PAL_INSTIT), palette = PAL_INSTIT, in_brief = TRUE
  ),
  q27 = list(
    id = "q27", section = "Skills & training",
    title = "Coverage of the data analytics training",
    short = "DA training — coverage",
    type = "single", cols = "q27", recoder = recode_m_coverage,
    levels = names(PAL_COVERAGE), palette = PAL_COVERAGE, in_brief = FALSE
  ),
  q28 = list(
    id = "q28", section = "Skills & training",
    title = "Specialization of the data analytics training",
    short = "DA training — level",
    type = "single", cols = "q28", recoder = recode_m_specialization,
    levels = names(PAL_SPECIALIZATION), palette = PAL_SPECIALIZATION, in_brief = FALSE
  ),
  q29 = list(
    id = "q29", section = "Skills & training",
    title = "Internal training that supports AI literacy",
    short = "AI training available",
    type = "single", cols = "q29", recoder = recode_m_yes_no,
    levels = c("Yes","No","Not sure"), palette = PAL_YESNO_M, in_brief = TRUE
  ),
  q30 = list(
    id = "q30", section = "Skills & training",
    title = "Institutionalization of the AI training",
    short = "AI training — institutionalization",
    type = "single", cols = "q30", recoder = recode_m_instit,
    levels = names(PAL_INSTIT), palette = PAL_INSTIT, in_brief = TRUE
  ),
  q31 = list(
    id = "q31", section = "Skills & training",
    title = "Coverage of the AI training",
    short = "AI training — coverage",
    type = "single", cols = "q31", recoder = recode_m_coverage,
    levels = names(PAL_COVERAGE), palette = PAL_COVERAGE, in_brief = FALSE
  ),
  q32 = list(
    id = "q32", section = "Skills & training",
    title = "Specialization of the AI training",
    short = "AI training — level",
    type = "single", cols = "q32", recoder = recode_m_specialization,
    levels = names(PAL_SPECIALIZATION), palette = PAL_SPECIALIZATION, in_brief = FALSE
  ),

  # -- ROLES / UNITS ------------------------------------------------------
  q33 = list(
    id = "q33", section = "Roles & units",
    title = "Defined job profiles with AI / data analytics responsibilities",
    short = "AI/DA job profiles defined",
    type = "single", cols = "q33", recoder = recode_m_q16,
    levels = c("Yes","No","Not sure"), palette = PAL_YESNO_M, in_brief = TRUE
  ),
  q34 = list(
    id = "q34", section = "Roles & units",
    title = "AI / data units relevant for the ministry's work",
    short = "AI / data units",
    type = "multi", cols = NULL, parent = "q34",
    options = c(
      q34_1 = "Central AI / digital innovation unit",
      q34_2 = "Dedicated AI unit (ministry)",
      q34_3 = "Data science / analytics unit",
      q34_4 = "Multiple units across ministries",
      q34_5 = "No dedicated unit (ad hoc)"
    ),
    in_brief = TRUE
  ),
  q35 = list(
    id = "q35", section = "Roles & units",
    title = "Profiles or skills most in demand",
    short = "Skills in demand",
    type = "multi", cols = NULL, parent = "q35",
    options = c(
      q35_1 = "Data and analytics",
      q35_2 = "Predictive AI",
      q35_3 = "Generative AI",
      q35_4 = "Technical / engineering",
      q35_5 = "Governance / ethics / strategy"
    ),
    in_brief = TRUE
  )
)

MANAGER_SECTION_ORDER <- c(
  "AI adoption", "Generative AI", "Data analytics", "Barriers",
  "Responsible AI", "Potential", "Impact of AI", "Support needed",
  "Skills & training", "Roles & units"
)
