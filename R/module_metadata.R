# =============================================================================
# module_metadata.R
# -----------------------------------------------------------------------------
# Metadatos que vienen DIRECTO de los cuestionarios XLSForm reales (no de la
# base de datos): a qué módulo pertenece cada pregunta, y su texto completo
# exacto (para el footnote de cada gráfico, con "Question N. <texto>").
#
# OJO — cobertura: estas listas solo incluyen las preguntas que YA están
# graficadas en el dashboard (question_dictionary.R / manager_questions.R /
# systems_questions.R). Hay algunas preguntas del instrumento real que todavía
# no están wireadas (Agency: q13, q19, q21, q33/q33a; Systems: todo el módulo
# "Opportunities and Support Needs" — q20, q21, q22). Para agregarlas hace
# falta además la hoja "choices" del XLSForm (las opciones de cada
# select_one/select_multiple), que todavía no tenemos.
#
# Cada módulo es list(title = "<nombre oficial>", qids = c("q7", "q8", ...))
# en el ORDEN real del cuestionario.
# =============================================================================

# --- AGENCY ------------------------------------------------------------------

AGENCY_MODULES <- list(
  list(title = "Adoption of AI in Government",
       qids  = c("q7", "q8", "q9", "q11")),
  list(title = "Generative AI Tools: Licensing and Guidelines",
       qids  = c("q12", "q14", "q15")),
  list(title = "Organizational Arrangements and Governance",
       qids  = c("q16", "q17", "q18")),
  list(title = "Data and Digital Infrastructure",
       qids  = c("q20", "q22")),
  list(title = "Funding and Performance Evaluation",
       qids  = c("q23", "q24", "q25")),
  list(title = "Barriers and Risks",
       qids  = c("q26", "q27", "q28", "q30", "q31")),
  list(title = "Opportunities and Expected Impacts",
       qids  = c("q32", "q34"))
)

AGENCY_QTEXT <- c(
  q7  = "How would you describe your government\u2019s use of AI for internal operations (back-end)?",
  q8  = "How would you describe your government\u2019s use of AI for citizen-facing public services (e.g., health, education, social services, etc.)?",
  q9  = "In which of the following areas is AI currently used in your government\u2019s work?",
  q11 = "Please list up to three of the most important AI applications currently deployed or piloted in your government and their main purpose.",
  q12 = "Does your government provide licenses or official access to generative AI tools for civil servants (e.g., Microsoft Copilot, ChatGPT Enterprise, Gemini for Workspace)?",
  q14 = "Has your government issued guidelines or policies regarding the use of generative AI tools (e.g., Copilot, ChatGPT, Gemini, Claude) by civil servants?",
  q15 = "What do the guidelines cover?",
  q16 = "Is there a national AI strategy or policy framework that guides AI use in the public sector, and is it used?",
  q17 = "Is there a unit dedicated to AI development and coordination in your government?",
  q18 = "Is there a body or committee responsible for overseeing AI deployment and governance in the public sector?",
  q20 = "What is the status of implementation of these data platforms or exchanges?",
  q22 = "Are there any AI projects in your government that leverage any of the following?",
  q23 = "Is there allocated budget for the statistical analysis of government data by public servants?",
  q24 = "Is there allocated budget for the government's AI projects?",
  q25 = "How are you (or relevant bodies) evaluating the performance of AI investments in government?",
  q26 = "To what extent do the following constraints prevent or limit your government\u2019s internal use of AI tools?",
  q27 = "To what extent do the following constraints prevent or limit your government's use of AI in the delivery of front-facing public services?",
  q28 = "Which of those are the three main barriers to responsible and effective AI use in your government?",
  q30 = "Please briefly describe any key initiatives, reforms, or programs to address these barriers.",
  q31 = "For AI systems deployed or piloted by your government, which of the following practices are used?",
  q32 = "Overall, thinking about costs (financial, organizational, political) and benefits, how do you assess the return on investment of AI projects in your government so far?",
  q34 = "What are the three key actions that would make the biggest difference for responsible AI implementation in your government?"
)

# --- MANAGERS ------------------------------------------------------------------

MANAGER_MODULES <- list(
  list(title = "AI Adoption in Your Ministry/Agency",
       qids  = c("q7", "q8", "q9", "q10", "q11", "q12", "q13", "q14", "q15",
                 "q16", "q17", "q18")),
  list(title = "Barriers, Risks and Safeguards",
       qids  = c("q19", "q20", "q21", "q22", "q23", "q24")),
  list(title = "Skills, Roles and Institutional Capacity",
       qids  = c("q25", "q26", "q27", "q28", "q29", "q30", "q31", "q32",
                 "q33", "q34", "q35"))
)

MANAGER_QTEXT <- c(
  q7  = "How would you describe your ministry/agency\u2019s use of AI for internal operations (back-end)?",
  q8  = "How would you describe your ministry/agency\u2019s use of AI for citizen- or firm-facing public services? (e.g., applications, renewals, service portals, case handling)",
  q9  = "In which areas has AI been used in your ministry/agency\u2019s work in the past 3 years?",
  q10 = "Other area of AI use (please specify).",
  q11 = "Please list up to three of the most important AI applications currently deployed or piloted in your ministry/agency and their main purpose.",
  q12 = "Does your ministry/agency provide licenses or official access to generative AI tools for staff (e.g., Microsoft Copilot, ChatGPT Enterprise, Gemini for Workspace)?",
  q13 = "Has your ministry/agency issued guidelines or policies regarding the use of generative AI tools (e.g., Copilot, ChatGPT, Gemini, Claude) by civil servants?",
  q14 = "If you answered yes, what do the guidelines mainly cover?",
  q15 = "Other guideline content (please specify).",
  q16 = "Is your ministry/agency currently using data analytics products to support decisions? (e.g., dashboards, regular analytics reports, scorecards)",
  q17 = "What are the main uses of data analytics products in your ministry/agency?",
  q18 = "What best describes the predominant type of data analytics used in your ministry/agency?",
  q19 = "To what extent do the following barriers prevent or limit your ministry/agency\u2019s use of AI tools (for internal operations or public services)?",
  q20 = "Which of those are the three main barriers to responsible and effective AI use in your ministry/agency?",
  q21 = "For AI systems deployed or piloted by your ministry/agency, which of the following practices are used?",
  q22 = "In which internal areas do you see the greatest potential for AI to improve your ministry/agency\u2019s performance?",
  q23 = "Based on your experience so far, how has AI changed your ministry/agency's work?",
  q24 = "What kind of support would be most useful to help your ministry/agency realize these AI opportunities?",
  q25 = "Does your organization make internal training available to your staff that supports the statistical analysis of government data by public servants?",
  q26 = "How institutionalized is this data analytics training program?",
  q27 = "How would you rate the coverage of this data analytics training program?",
  q28 = "How would you characterize the level of specialization of this data analytics training program?",
  q29 = "Does your organization make internal training available to your staff that supports AI literacy?",
  q30 = "How institutionalized is this AI training program?",
  q31 = "How would you rate the coverage of this AI training program?",
  q32 = "How would you characterize the level of specialization of this AI training program?",
  q33 = "Does the ministry/agency have defined job profiles or occupational groups that include AI or data analytics responsibilities?",
  q34 = "Which of the following units or arrangements exist and are relevant for your ministry/agency\u2019s work on AI and data analytics?",
  q35 = "What types of profiles or skills are most in demand to support data analytics and AI work in your ministry/agency?"
)

# --- SYSTEMS ------------------------------------------------------------------
# OJO: falta el modulo real "Opportunities and Support Needs" (q20 potencial
# de IA, q21 efectos, q22 apoyo necesario) porque esas 3 preguntas todavia no
# estan en systems_questions.R.

SYSTEMS_MODULES <- list(
  list(title = "MIS Foundations and Analytics",
       qids  = c("q7", "q7_1", "q8", "q9", "q9a", "q10", "q11", "q11a")),
  list(title = "AI/ML Adoption Around the MIS",
       qids  = c("q12", "q13")),
  list(title = "AI for MIS Operations",
       qids  = c("q14", "q14a", "q15", "q15a", "q16", "q16a")),
  list(title = "Barriers, Risks and Safeguards",
       qids  = c("q17", "q18", "q19"))
)

SYSTEMS_QTEXT <- c(
  q7    = "Is there a Human Resource Management Information System (HRMIS) or other core civil service data system in place in your ministry/agency?",
  q7_1  = "If yes, is the HRMIS digitized?",
  q8    = "Which data element categories are available in the HRMIS?",
  q9    = "Does your organization produce analytical outputs based on HRMIS data to guide managerial or policy decisions?",
  q9a   = "If yes, what are the main uses of analytical products in your agency/organization?",
  q10   = "Are there internal funding opportunities to support data analytics or AI projects using HRMIS data?",
  q11   = "Is there a strategy to collaborate on data analytics or AI using HRMIS data with academics, NGOs, foundations, multilaterals, or the private sector?",
  q11a  = "If yes, what are the main modes of collaboration?",
  q12   = "In which areas has AI been used in your ministry/agency\u2019s work in the past 3 years?",
  q13   = "List up to three AI/ML applications currently piloted or deployed that use HRMIS data, and describe their purpose.",
  q14   = "Are MIS/IT staff using Generative AI tools (e.g., Copilot/ChatGPT/Gemini) to support MIS technical work?",
  q14a  = "If yes, what tasks are GenAI tools used for?",
  q15   = "Are MIS/IT staff using ML models to improve the MIS data structure or workflow automation?",
  q15a  = "If yes, what ML-enabled functions are used?",
  q16   = "Does your MIS team have technical guidance on what data can or cannot be used with AI/ML/GenAI tools?",
  q16a  = "If yes, what does that guidance cover?",
  q17   = "To what extent do the following barriers limit advanced analytics or AI using HRMIS data?",
  q18   = "What are the three main barriers?",
  q19   = "What safeguards are used for AI systems that rely on HRMIS data?"
)

# =============================================================================
# Helpers
# =============================================================================

modules_for_family <- function(family) {
  switch(family,
    "Agency"   = AGENCY_MODULES,
    "Managers" = MANAGER_MODULES,
    "Systems"  = SYSTEMS_MODULES,
    list()
  )
}

qtext_for_family <- function(family) {
  switch(family,
    "Agency"   = AGENCY_QTEXT,
    "Managers" = MANAGER_QTEXT,
    "Systems"  = SYSTEMS_QTEXT,
    character(0)
  )
}

# Numero de pregunta a mostrar: "q26" -> "26", "q7_1" -> "7.1", "q9a" -> "9a"
question_number <- function(qid) {
  n <- sub("^q", "", qid)
  gsub("_", ".", n)
}

# Texto completo para el footnote de un grafico: "Question 26. To what
# extent..." Si no tenemos el texto real todavia, cae a q$title (lo que ya
# usaba el dashboard) para no dejar el footnote vacio.
question_footnote <- function(family, qid, fallback_title = NULL) {
  qtext <- qtext_for_family(family)
  txt   <- if (qid %in% names(qtext)) qtext[[qid]] else fallback_title
  if (is.null(txt) || !nzchar(txt)) return(paste0("Question ", question_number(qid), "."))
  paste0("Question ", question_number(qid), ". ", txt)
}
