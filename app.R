# =============================================================================
# app.R — DfBG Country Dashboard
# -----------------------------------------------------------------------------
# Dashboard Shiny para el AI & Data for Better Governance Survey (World Bank).
#
# Permite:
#   1. Elegir un país.
#   2. Ver, pregunta por pregunta, los gráficos de la sección Agency, con vista
#      conmutable entre "solo el país" y "país vs. promedio de su income group".
#   3. Descargar el Country Brief automatizado en Word (.docx), replicando el
#      formato del brief de Austria.
#
# CÓMO CORRER (local):
#   1. Colocá en data/ los archivos que produce build_dfbg_database_EN.R:
#        dfbg_agency.rds, dfbg_managers.rds, dfbg_systems.rds
#      (también sirven los .csv equivalentes)
#      y, opcionalmente, CLASS_2025_10_07.xlsx para los income groups.
#   2. Abrí este proyecto en RStudio y ejecutá:  shiny::runApp()
#
# DESPLIEGUE (shinyapps.io / Posit Connect):
#   rsconnect::deployApp() con la carpeta del proyecto. Los .rds deben ir
#   incluidos en data/ (no rutas absolutas de OneDrive).
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(forcats)
library(tibble)
library(officer)
library(flextable)

# --- carga de módulos --------------------------------------------------------
source("R/question_dictionary.R", local = TRUE)
source("R/manager_questions.R",  local = TRUE)
source("R/systems_questions.R",  local = TRUE)
source("R/data_load.R",           local = TRUE)
source("R/translations_static.R", local = TRUE)
source("R/plots.R",               local = TRUE)
source("R/global_stats.R",        local = TRUE)
source("R/llm_narrative.R",       local = TRUE)
source("R/build_brief.R",         local = TRUE)

# --- datos (se cargan una vez al iniciar) ------------------------------------
DATA <- tryCatch(load_dfbg(), error = function(e) {
  warning("No se pudieron cargar los datos: ", conditionMessage(e))
  NULL
})

# preguntas realmente disponibles en la base actual
QLIST <- if (!is.null(DATA)) available_questions(DATA$agency) else AGENCY_QUESTIONS

# tema visual estilo World Bank
wb_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF", fg = "#002245",
  primary = "#002245", secondary = "#1F77B4",
  base_font = font_google("Open Sans"),
  heading_font = font_google("Open Sans")
)

# =============================================================================
# UI
# =============================================================================

ui <- page_sidebar(
  title = "AI & Data for Better Governance — Economies Dashboard",
  theme = wb_theme,

  # Librería para exportar el cuadro de Use Cases como PNG (rasteriza el DOM).
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"),
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('uc_download_png', function(msg) {",
      "  var node = document.getElementById('uc_capture');",
      "  if (!node || typeof html2canvas !== 'function') {",
      "    alert('Could not capture the image. Please reload the page and try again.');",
      "    return;",
      "  }",
      "  html2canvas(node, {",
      "    backgroundColor: '#ffffff', scale: 2,",
      "    ignoreElements: function(el) { return el.classList && el.classList.contains('uc-delete-btn'); }",
      "  }).then(function(canvas) {",
      "    var link = document.createElement('a');",
      "    link.download = msg.filename || 'ai_use_cases.png';",
      "    link.href = canvas.toDataURL('image/png');",
      "    document.body.appendChild(link); link.click(); document.body.removeChild(link);",
      "  });",
      "});"
    ))
  ),

  sidebar = sidebar(
    width = 320,
    if (is.null(DATA)) {
      div(class = "text-danger",
          "Data not found in data/. Please add dfbg_agency.rds, ",
          "dfbg_managers.rds and dfbg_systems.rds, then restart the app.")
    } else {
      tagList(
        selectInput("country", "Economy",
                    choices = c("Select an economy" = "",
                                sort(country_choices(DATA)$country)),
                    selected = ""),
        uiOutput("ig_badge"),
        hr(),
        radioButtons("family", "Questionnaire",
                     choices = c("Agency", "Managers", "Systems"),
                     selected = character(0), inline = TRUE),
        uiOutput("family_hint"),
        hr(),
        uiOutput("scope_picker"),
        uiOutput("sector_picker"),
        hr(),
        uiOutput("question_picker"),
        hr(),
        conditionalPanel(
          condition = "input.country != '' && typeof input.family !== 'undefined'",
          downloadButton("dl_brief", "Download economy brief (.docx)",
                         class = "btn-primary w-100")
        ),
        conditionalPanel(
          condition = "input.country == '' || typeof input.family === 'undefined'",
          div(class = "small text-muted",
              "Select an economy and a questionnaire to enable the download.")
        ),
        br(), br(),
        uiOutput("api_status")
      )
    }
  ),

  navset_card_tab(
    nav_panel(
      "About the survey",
      card(card_body(
        h4("The AI and Data for Better Governance Survey"),
        p("The AI and Data for Better Governance Survey was designed to ",
          "accomplish the following:"),
        tags$ul(
          tags$li("Measure the extent to which governments have already ",
                  "adopted AI"),
          tags$li("Evaluate governments\u2019 institutional readiness and ",
                  "capacity to adopt AI in the future"),
          tags$li("Document innovative approaches to applying AI and data ",
                  "analytics in public administration")
        ),
        p("The survey consisted of 13 questionnaires: one agency ",
          "questionnaire focusing on AI adoption, strategy, infrastructure, ",
          "and barriers from a whole-of-government perspective; six manager ",
          "questionnaires focusing on ministries and agencies in six sectors ",
          "(education, health care, public finance, procurement, taxation, ",
          "and civil service); and six system questionnaires focusing on ",
          "management information system (MIS) infrastructure, data ",
          "readiness, and AI/machine learning capabilities in these same ",
          "sectors."),
        hr(),
        h5("Questions or issues?"),
        p("If the dashboard isn\u2019t working as expected, please contact:"),
        p(tags$strong("Josefina Silva Fuentealba"), br(),
          tags$a(href = "mailto:jsilvafuentealba@worldbank.org",
                 "jsilvafuentealba@worldbank.org"))
      ))
    ),
    nav_panel(
      "How to use this dashboard",
      card(card_body(
        h4("What this dashboard does"),
        p("Pick an economy to explore its Agency-questionnaire responses ",
          "and download an automated economy brief in Word. ",
          "Use \u201cJump to question\u201d in the sidebar to switch charts."),
        tags$ul(
          tags$li("Start in the sidebar: choose an ", tags$strong("Economy"),
                  " and a ", tags$strong("Questionnaire"),
                  " (Agency, Managers, or Systems)."),
          tags$li("Charts can show the economy alone or compared with the ",
                  "(weighted) average of its World Bank income group, via ",
                  "the \u201cChart view\u201d option."),
          tags$li("For Managers and Systems, use the ", tags$strong("Sector"),
                  " picker to look at a single sector, or compare all ",
                  "sectors at once."),
          tags$li("Use \u201cJump to question\u201d to move between questions ",
                  "within the selected questionnaire."),
          tags$li("Each chart can be downloaded as a PNG image using the ",
                  tags$strong("Download chart (PNG)"),
                  " button below it."),
          tags$li("The ", tags$strong("Text responses"),
                  " tab shows the open-text answers for the selected ",
                  "economy and questionnaire."),
          tags$li("The ", tags$strong("AI Use Cases"),
                  " tab lets you browse and add examples of AI applications ",
                  "by category, and download the summary as an image."),
          tags$li("The ", tags$strong("Download economy brief"),
                  " button in the sidebar generates an automated Word brief ",
                  "for the selected economy."),
          tags$li("Each economy weighs 1/n in the income-group average, ",
                  "matching the original R Markdown analysis.")
        ),
        p(class = "text-muted",
          "Data: build_dfbg_database_EN.R outputs (Agency / Managers / Systems).")
      ))
    ),
    nav_panel(
      "Explore charts",
      uiOutput("header_cards"),
      card(
        card_header(textOutput("sq_title")),
        uiOutput("sq_plot_ui"),
        uiOutput("sq_text")
      )
    ),
    nav_panel(
      "Text responses",
      uiOutput("text_cards")
    ),
    nav_panel(
      "AI Use Cases",
      div(
        style = "padding:8px 12px;",

        # === Formulario "Add example" (NO entra en la captura) ===
        div(
          style = "background:#f5f7fa; border:1px solid #e0e4ea; border-radius:8px; padding:10px 14px; margin-bottom:14px;",
          div(style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:10px; margin-bottom:8px;",
              tags$span(style = "font-weight:600; font-size:15px;", "Add example to category:"),
              tags$button(
                id = "uc_download", type = "button",
                class = "action-button btn btn-primary",
                style = "background:#0F6E56; border-color:#0F6E56; font-weight:600;",
                HTML("&#x2B07;&#xFE0E; Download as PNG")
              )),
          fluidRow(
            column(3, selectizeInput("uc_country", NULL,
                                     choices = NULL,
                                     options = list(placeholder = "Type to search a country...",
                                                    onInitialize = I('function() { this.setValue(""); }')))),
            column(5, selectizeInput("uc_app", NULL, choices = NULL,
                                     options = list(placeholder = "AI apps for this country & category..."))),
            column(2, selectInput("uc_cat", NULL,
                                  choices = c("1. Staff Productivity Tools"     = "prod",
                                              "2. Citizen-Facing AI Services"   = "citizen",
                                              "3. AI in Health"                 = "health",
                                              "4. Tax Administration & Public Finance" = "tax",
                                              "5. AI in Education"              = "edu",
                                              "6. Data Infrastructure & Sovereignty" = "infra"))),
            column(2, actionButton("uc_add", "Add", class = "btn-primary w-100",
                                   style = "margin-top:0;"))
          )
        ),

        # === Capturable: titulo + grid (esto es lo que se descarga como PNG) ===
        div(
          id = "uc_capture",
          style = "background:#fff; padding:16px; border-radius:8px;",
          div(
            style = "display:flex; align-items:baseline; gap:14px; flex-wrap:wrap; margin-bottom:14px; border-bottom:1px solid #ddd; padding-bottom:10px;",
            h4(style = "margin:0; font-weight:700; color:#1a1a1a;",
               "AI Use Cases \u2014 Six Categories Across DfBG Questionnaires"),
            tags$span(style = "font-size:12px; color:#888;",
                      "Source: DfBG Agency & Manager Questionnaires")
          ),
          uiOutput("uc_grid")
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  req_data <- reactive({ validate(need(!is.null(DATA), "Data not loaded.")); DATA })

  output$family_hint <- renderUI({
    if (is.null(input$family)) {
      div(class = "small text-muted mt-1", "Select a questionnaire.")
    }
  })

  output$ig_badge <- renderUI({
    if (is.null(input$country) || !nzchar(input$country)) return(NULL)
    d <- req_data()
    ig <- d$agency$income_group[d$agency$country == input$country][1]
    hdr <- country_header(d, input$country)
    div(class = "mt-2",
        span(class = "badge bg-secondary", paste("Income group:", ifelse(is.na(ig), "n/a", ig))),
        br(), br(),
        span(class = "badge bg-primary", paste("Questionnaires:", hdr$n_total)),
        span(class = "badge bg-info text-dark", paste("Systems:", hdr$n_systems)),
        span(class = "badge bg-info text-dark", paste("Managers:", hdr$n_managers))
    )
  })

  output$header_cards <- renderUI({
    if (is.null(input$country) || !nzchar(input$country) || is.null(input$family)) {
      return(div(class = "text-muted p-4",
                 "Select an economy and a questionnaire from the left panel to begin."))
    }
    d <- req_data(); hdr <- country_header(d, input$country)
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Income group", hdr$income_group, theme = "primary"),
      value_box("Total questionnaires", hdr$n_total, theme = "secondary"),
      value_box("Systems", hdr$n_systems),
      value_box("Managers", hdr$n_managers)
    )
  })

  output$text_cards <- renderUI({
    if (is.null(input$country) || !nzchar(input$country) || is.null(input$family)) {
      return(div(class = "text-muted p-4",
                 "Select an economy and a questionnaire from the left panel to see text responses."))
    }
    text_cards_ui(plot_data(), input$country,
                  qlist        = qlist_fam(),
                  base         = base_fam(),
                  family_label = paste0(input$family,
                                        if (input$family != "Agency" && !is.null(input$sector))
                                          paste0(" \u00b7 ", input$sector) else ""))
  })

  # --- Reactivos de familia / sector / preguntas ---------------------------

  # Diccionario de preguntas disponible para la familia elegida
  qlist_fam <- reactive({
    req(input$family)
    questions_for_family(req_data(), input$family)
  })

  # Base de datos (nombre del slot) para la familia
  base_fam <- reactive({ req(input$family); FAMILY_BASE[[input$family]] })

  # Sectores disponibles para Managers/Systems (Agency no tiene sectores)
  sectors_fam <- reactive({
    req(input$family)
    if (input$family == "Agency") return(NULL)
    d <- req_data(); b <- base_fam()
    df <- dplyr::filter(d[[b]], country == input$country)
    if (nrow(df) == 0) return(character(0))
    sort(unique(stringr::str_remove(df$questionnaire, "^(Systems|Manager)_")))
  })

  # Selector de sector (solo Managers/Systems)
  output$sector_picker <- renderUI({
    if (is.null(input$family) || input$family == "Agency") return(NULL)
    if (is.null(input$scope) || input$scope != "country") return(NULL)
    secs <- sectors_fam()
    if (is.null(secs)) return(NULL)
    if (length(secs) == 0)
      return(div(class = "small text-muted", "No sector responses for this country."))
    selectInput("sector", "Sector", choices = secs, selected = secs[1])
  })

  # Selector de scope: label cambia segun la familia
  output$scope_picker <- renderUI({
    if (is.null(input$family)) return(NULL)
    if (input$family == "Agency") {
      radioButtons("scope", "Chart view",
                   choices = c("Economy vs. income-group average" = "compare",
                               "Economy only" = "country"),
                   selected = "compare")
    } else {
      radioButtons("scope", "Chart view",
                   choices = c("Compare all sectors" = "compare",
                               "One sector only"     = "country"),
                   selected = "compare")
    }
  })

  # Selector de preguntas (cambia con la familia)
  output$question_picker <- renderUI({
    if (is.null(input$family)) {
      return(div(class = "small text-muted", "Select a questionnaire above to choose a question."))
    }
    ql <- qlist_fam()
    if (length(ql) == 0) return(div(class = "small text-muted", "No questions available."))
    choices <- setNames(names(ql),
                        paste0(names(ql), " \u2014 ", vapply(ql, function(q) q$short, character(1))))
    selectInput("question", "Jump to question", choices = choices)
  })

  # Data efectiva para graficar:
  #  - Agency: la base completa (compara contra income group).
  #  - Managers/Systems en modo 'compare' (entre sectores): TODAS las filas
  #    del país (sin filtrar sector) → make_plot delega a plot_*_sectors.
  #  - Managers/Systems en modo 'country' (un sector solo): filtra al sector
  #    elegido y lo expone como base 'agency'-like para vista detallada.
  plot_data <- reactive({
    req(input$country)
    d <- req_data(); b <- base_fam()
    if (input$family == "Agency") return(d)

    # comparar entre sectores: no filtrar
    if (input$scope == "compare") return(d)

    # vista de un solo sector: filtrar
    sec <- input$sector
    if (is.null(sec)) return(d)
    sub <- dplyr::filter(d[[b]],
                         stringr::str_remove(questionnaire, "^(Systems|Manager)_") == sec)
    d2 <- d
    d2[[b]] <- sub
    d2
  })

  # Pregunta actualmente seleccionada (con guarda por si el input aún no existe)
  current_q <- reactive({
    ql <- qlist_fam()
    qid <- input$question
    if (is.null(qid) || !qid %in% names(ql)) return(NULL)
    ql[[qid]]
  })

  # Explorador: un solo gráfico por vez.
  output$sq_title <- renderText({
    if (is.null(input$country) || !nzchar(input$country) || is.null(input$family)) return("")
    q <- current_q(); if (is.null(q)) return("")
    paste0(input$question, " \u2014 ", q$title, " (", input$family,
           if (input$family != "Agency" && !is.null(input$sector)) paste0(" \u00b7 ", input$sector) else "", ")")
  })
  # Wrapper dinamico: muestra plotOutput solo cuando hay grafico que renderizar.
  # Cuando es una tabla-badge (single sectorial), lo oculta para no reservar espacio.
  output$sq_plot_ui <- renderUI({
    if (is.null(input$country) || !nzchar(input$country) || is.null(input$family)) {
      return(div(class = "text-muted p-4",
                 "Select an economy and a questionnaire from the sidebar to see charts."))
    }
    q <- current_q()
    use_sectoral <- base_fam() %in% c("manager", "systems") &&
                    !is.null(input$scope) && input$scope == "compare"
    show_table <- !is.null(q) && q$type == "single" && use_sectoral
    if (show_table || (!is.null(q) && q$type == "text")) return(NULL)
    tagList(
      plotOutput("sq_plot", height = "460px"),
      div(style = "text-align:right; margin-top:6px;",
          downloadButton("dl_plot_png", "Download chart (PNG)",
                         class = "btn-outline-secondary btn-sm"))
    )
  })

  # Objeto ggplot actualmente mostrado en el explorador (reutilizable para
  # renderPlot y para la descarga en PNG, asi no se duplica la logica).
  sq_plot_obj <- reactive({
    if (is.null(input$country) || !nzchar(input$country) || is.null(input$family)) return(NULL)
    q <- current_q()
    if (is.null(q) || q$type == "text") return(NULL)
    use_sectoral <- base_fam() %in% c("manager", "systems") &&
                    !is.null(input$scope) && input$scope == "compare"
    if (use_sectoral && q$type == "single") return(NULL)
    make_plot(plot_data(), q, input$country, scope = input$scope, base = base_fam())
  })

  output$sq_plot <- renderPlot({
    req(input$country); req(input$family)
    q <- current_q(); if (is.null(q)) return(empty_plot("", "Select a question"))
    if (q$type == "text") return(empty_plot(q$title, "Open-text question — see the Text responses tab"))
    p <- sq_plot_obj()
    if (is.null(p)) return(NULL)
    p
  })

  # Descarga del grafico actualmente mostrado como PNG
  output$dl_plot_png <- downloadHandler(
    filename = function() {
      qid <- if (!is.null(input$question)) input$question else "chart"
      paste0("dfbg_", tolower(gsub("[^A-Za-z]+", "_", input$country)),
             "_", qid, ".png")
    },
    content = function(file) {
      p <- sq_plot_obj()
      if (is.null(p)) p <- empty_plot("", "No chart to export")
      ggplot2::ggsave(filename = file, plot = p, device = "png",
                      width = 10, height = 6.5, dpi = 200, bg = "white")
    }
  )
  output$sq_text <- renderUI({
    if (is.null(input$country) || !nzchar(input$country) || is.null(input$family)) return(NULL)
    q <- current_q(); if (is.null(q)) return(NULL)
    # 1. Tabla badge para preguntas single sectoriales
    tbl_ui <- make_table_ui(plot_data(), q, input$country,
                             scope = input$scope, base = base_fam())
    if (!is.null(tbl_ui)) return(tbl_ui)
    # 2. Texto libre
    if (q$type != "text") return(NULL)
    tbl <- make_text(plot_data(), q, input$country, base = base_fam())
    if (nrow(tbl) == 0) return(p("No response."))
    tags$ul(map(tbl$Response, ~ tags$li(.x)))
  })

  output$api_status <- renderUI({
    if (have_api_key()) {
      div(class = "small text-success mb-2",
          tags$i(class = "bi bi-check-circle"),
          "Claude API detected — the brief will include LLM-written analysis.")
    } else {
      div(class = "small text-muted mb-2",
          "No ANTHROPIC_API_KEY set — the brief will use rule-based narrative. ",
          "Set the key and restart R to enable LLM analysis.")
    }
  })

  # Descarga del brief
  output$dl_brief <- downloadHandler(
    filename = function() {
      paste0("dfbg_", tolower(gsub("[^A-Za-z]+", "_", input$country)), "_brief.docx")
    },
    content = function(file) {
      msg <- if (have_api_key())
        "Generating brief with Claude analysis… (this may take ~30s)"
      else
        "Generating economy brief…"
      withProgress(message = msg, value = 0.3, {
        out <- generate_brief(req_data(), input$country, out_file = file)
        incProgress(0.7)
      })
    }
  )

  # ===========================================================================
  # AI USE CASES — cuadro editable de seis categorías
  # ===========================================================================

  UC_CATS <- list(
    prod    = list(title = "1. Staff Productivity Tools",
                   sub   = "Commercial AI, personal & ad hoc use",
                   color = "#1F4E5F"),
    citizen = list(title = "2. Citizen-Facing AI Services",
                   sub   = "Chatbots, portals & government assistants",
                   color = "#1E7A8C"),
    health  = list(title = "3. AI in Health",
                   sub   = "Diagnostics, medical imaging, telehealth",
                   color = "#6E4D8C"),
    tax     = list(title = "4. Tax Administration & Public Finance",
                   sub   = "Fraud detection, risk, compliance, automation",
                   color = "#BA8A0F"),
    edu     = list(title = "5. AI in Education",
                   sub   = "Adaptive tutoring, EMIS, personalized learning",
                   color = "#2F7A3A"),
    infra   = list(title = "6. Data Infrastructure & Tech Sovereignty",
                   sub   = "Local LLMs, translation, sovereign analytics",
                   color = "#8C2D2D")
  )

  # estado: lista de entradas por categoría
  use_cases <- reactiveValues(
    prod = list(
      list(country = "Canada",    text = "Microsoft Copilot, GitHub Copilot, DeepL Pro \u00b7 Civil Service, Procurement"),
      list(country = "Singapore", text = "SWEE (GenAI writing assistant) \u00b7 Taxation"),
      list(country = "Guatemala", text = "ChatGPT, Gemini as personal productivity assistants \u00b7 Civil Service"),
      list(country = "Hungary",   text = "MS Copilot pilot for office work \u00b7 Civil Service"),
      list(country = "Austria",   text = "Open WebUI: knowledge management & drafting \u00b7 Health")
    ),
    citizen = list(
      list(country = "Ukraine",   text = "Diia AI \u2014 world's first national AI assistant"),
      list(country = "Argentina", text = "Chatbot MIA \u2014 national-scale citizen query assistant"),
      list(country = "Panama",    text = "Virtual Assistant 3-1-1 \u2014 citizen service hotline"),
      list(country = "Hungary",   text = "1818 MIA Chatbot \u2014 multichannel public services"),
      list(country = "Burkina Faso", text = "LegiChat \u2014 citizen AI access to legislative corpus")
    ),
    health = list(
      list(country = "Egypt",   text = "MASA \u2014 breast cancer detection via computer vision \u00b7 Health"),
      list(country = "Jordan",  text = "ARK/Paxera \u2014 radiology imaging; CAD4TB \u2014 tuberculosis \u00b7 Health"),
      list(country = "Jordan",  text = "e-ICU \u2014 AI-assisted patient deterioration prediction \u00b7 Health"),
      list(country = "Hungary", text = "Brainomix (brain imaging), CRC (colorectal cancer detection) \u00b7 Health")
    ),
    tax = list(
      list(country = "Singapore", text = "iNAT \u2014 taxpayer network analysis; VICA chatbot \u00b7 Taxation"),
      list(country = "Malaysia",  text = "AI-powered income tax fraud detection \u00b7 Taxation"),
      list(country = "Peru",      text = "AI agents for audits & appeals processing \u00b7 Taxation"),
      list(country = "Nepal",     text = "Risk Engine, Report Engine, Tax chatbot \u00b7 Taxation"),
      list(country = "Jordan",    text = "Smart declarations + income & sales tax AI assistant \u00b7 Taxation")
    ),
    edu = list(
      list(country = "Jordan",    text = "Siraj \u2014 AI tutoring aligned to national curriculum \u00b7 Education"),
      list(country = "Kosovo",    text = "MIA \u2014 AI-powered learning for students & teachers \u00b7 Education"),
      list(country = "Lao PDR",   text = "Eduten \u2014 adaptive mathematics learning platform \u00b7 Education"),
      list(country = "Uruguay",   text = "Ceibal \u2014 virtual tutoring system \u00b7 Education")
    ),
    infra = list(
      list(country = "Nigeria",   text = "N-ATLAS \u2014 multilingual LLM: Yoruba, Igbo, Hausa, Pidgin"),
      list(country = "Cambodia",  text = "TranslateKH (Khmer\u2194EN translation); Sarika (Khmer TTS)"),
      list(country = "Bhutan",    text = "Dzongkha NLP + machine translation for service portals"),
      list(country = "Brazil",    text = "Open-source LLMs on sovereign infrastructure; fine-tuning"),
      list(country = "Singapore", text = "AIBots (internal GenAI platform); Transcribe (speech-to-text)")
    )
  )

  # Poblar selectize de paises (una sola vez, server-side para que sea rapido)
  observe({
    countries <- sort(country_choices(req_data())$country)
    updateSelectizeInput(session, "uc_country",
                         choices = countries,
                         server = TRUE,
                         selected = character(0))
  })

  # Apps reportadas por el pais + categoria elegidos. Junta q11 (Agency/Managers)
  # y q13 (Systems) y filtra segun el mapeo:
  #   prod    -> Civil Service (Managers + Systems) + Agency
  #   citizen -> Agency + cualquier app con q11_2 == 'front-facing'
  #   health  -> Health (Managers + Systems)
  #   tax     -> Tax + PublicFinance (Managers + Systems)
  #   edu     -> Education (Managers + Systems)
  #   infra   -> Systems (todos los sectores)
  CAT_SECTORS <- list(
    prod    = c("Civil_Service"),
    health  = c("Health"),
    tax     = c("Tax","Public_Finance"),
    edu     = c("Education"),
    infra   = c("Civil_Service","Health","Tax","Public_Finance","Education","Procurement")
  )

  uc_country_apps <- reactive({
    ctry <- input$uc_country
    cat_id <- input$uc_cat
    if (is.null(ctry) || !nzchar(trimws(ctry))) return(NULL)
    if (is.null(cat_id) || !cat_id %in% names(UC_CATS)) return(NULL)
    d <- req_data()
    apps <- list()

    # Helper: extrae las 3 apps de q11 con su flag internal/front-facing
    # Retorna df: app, source, facing (internal/front-facing/NA)
    pick_q11 <- function(row, source_label) {
      out <- list()
      for (i in 1:3) {
        app_col  <- paste0("q11_", i)
        face_col <- paste0("q11_", i, "_2")
        if (!app_col %in% names(row)) next
        app_v <- as.character(row[[app_col]][1])
        if (is.na(app_v) || !nzchar(trimws(app_v)) ||
            tolower(trimws(app_v)) %in% c("na","n/a","none","-","not sure")) next
        face <- NA_character_
        if (face_col %in% names(row)) {
          fv <- as.character(row[[face_col]][1])
          if (!is.na(fv)) face <- tolower(trimws(fv))
        }
        out[[length(out) + 1]] <- data.frame(
          app = trimws(app_v), source = source_label,
          facing = face, stringsAsFactors = FALSE)
      }
      if (length(out) == 0) return(NULL)
      do.call(rbind, out)
    }
    # Systems q13 (texto libre)
    pick_sys_apps <- function(row, source_label) {
      cols <- c("q13_1","q13_2","q13_3","q13","q13a")
      cols <- intersect(cols, names(row))
      if (length(cols) == 0) return(NULL)
      vals <- unlist(row[1, cols], use.names = FALSE)
      vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
      vals <- vals[!tolower(trimws(vals)) %in% c("na","n/a","none","-","not sure","1","yes","no")]
      if (length(vals) == 0) return(NULL)
      data.frame(app = trimws(as.character(vals)),
                 source = source_label, facing = NA_character_,
                 stringsAsFactors = FALSE)
    }

    # ----- recolectar TODO lo del pais, despues filtrar por categoria -----

    # Agency
    ag <- dplyr::filter(d$agency, country == ctry)
    if (nrow(ag) > 0) {
      out <- pick_q11(ag, "Agency")
      if (!is.null(out)) apps[["Agency"]] <- out
    }
    # Managers (1 fila por sector)
    if (!is.null(d$manager)) {
      mg <- dplyr::filter(d$manager, country == ctry)
      if (nrow(mg) > 0) {
        mg$sector_raw <- stringr::str_remove(mg$questionnaire, "^(Systems|Manager)_")
        for (i in seq_len(nrow(mg))) {
          out <- pick_q11(mg[i, , drop = FALSE],
                          paste0("Managers \u00b7 ", gsub("_", " ", mg$sector_raw[i])))
          if (!is.null(out)) {
            out$sector <- mg$sector_raw[i]
            out$family <- "manager"
            apps[[paste0("M_", mg$sector_raw[i])]] <- out
          }
        }
      }
    }
    # Systems (1 fila por sector)
    if (!is.null(d$systems)) {
      sy <- dplyr::filter(d$systems, country == ctry)
      if (nrow(sy) > 0) {
        sy$sector_raw <- stringr::str_remove(sy$questionnaire, "^(Systems|Manager)_")
        for (i in seq_len(nrow(sy))) {
          out <- pick_sys_apps(sy[i, , drop = FALSE],
                               paste0("Systems \u00b7 ", gsub("_", " ", sy$sector_raw[i])))
          if (!is.null(out)) {
            out$sector <- sy$sector_raw[i]
            out$family <- "systems"
            apps[[paste0("S_", sy$sector_raw[i])]] <- out
          }
        }
      }
    }

    if (length(apps) == 0) return(NULL)
    df <- dplyr::bind_rows(apps)
    if (!"sector" %in% names(df)) df$sector <- NA_character_
    if (!"family" %in% names(df)) df$family <- NA_character_
    if (!"facing" %in% names(df)) df$facing <- NA_character_

    # marca Agency con family=agency
    df$family[grepl("^Agency", df$source)] <- "agency"

    # ----- FILTRO POR CATEGORIA -----
    df <- switch(cat_id,
      prod    = dplyr::filter(df,
                  df$sector %in% CAT_SECTORS$prod | df$family == "agency"),
      citizen = dplyr::filter(df,
                  df$family == "agency" |
                  (!is.na(df$facing) & grepl("front", df$facing))),
      health  = dplyr::filter(df, df$sector %in% CAT_SECTORS$health),
      tax     = dplyr::filter(df, df$sector %in% CAT_SECTORS$tax),
      edu     = dplyr::filter(df, df$sector %in% CAT_SECTORS$edu),
      infra   = dplyr::filter(df, df$family == "systems"),
      df
    )

    if (nrow(df) == 0) return(NULL)
    df$short <- ifelse(nchar(df$app) > 80,
                       paste0(substr(df$app, 1, 77), "\u2026"), df$app)
    df$label <- paste0(df$short, "  \u2014  ", df$source)
    df$value <- paste0(df$app, "  \u2014  ", df$source)
    df
  })

  # Cuando cambia el pais O la categoria, repoblar el dropdown de apps
  observeEvent(list(input$uc_country, input$uc_cat), {
    apps <- uc_country_apps()
    if (is.null(apps) || nrow(apps) == 0) {
      updateSelectizeInput(session, "uc_app", choices = character(0),
                           server = TRUE, selected = character(0))
      return()
    }
    choices <- setNames(apps$value, apps$label)
    updateSelectizeInput(session, "uc_app", choices = choices,
                         server = TRUE, selected = character(0))
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # Descargar el cuadro como PNG (envia mensaje al cliente, que rasteriza el DOM)
  observeEvent(input$uc_download, {
    session$sendCustomMessage("uc_download_png",
                              list(filename = "dfbg_ai_use_cases.png"))
  })

  # Agregar entrada
  observeEvent(input$uc_add, {
    ctry   <- input$uc_country
    app_v  <- input$uc_app
    cat_id <- input$uc_cat

    if (is.null(ctry) || !nzchar(trimws(ctry))) {
      showNotification("Please pick a country first.", type = "warning", duration = 3)
      return()
    }
    if (is.null(app_v) || !nzchar(trimws(app_v))) {
      showNotification("This country has no AI applications reported, or none is selected.",
                       type = "warning", duration = 4)
      return()
    }
    if (is.null(cat_id) || !cat_id %in% names(UC_CATS)) return()

    current <- use_cases[[cat_id]]
    use_cases[[cat_id]] <- c(current,
                             list(list(country = ctry, text = trimws(app_v))))

    # limpiar para el proximo
    updateSelectizeInput(session, "uc_country", selected = character(0))
    updateSelectizeInput(session, "uc_app", choices = character(0),
                         selected = character(0))
    showNotification(paste0("Added to ", UC_CATS[[cat_id]]$title),
                     type = "message", duration = 2)
  })

  # Delete: cada botoncito en el grid llama a Shiny.setInputValue('uc_delete', ...)
  # con priority:'event' para que siempre dispare, incluso si el payload se repite.
  # Asi NO necesitamos registrar N observers (uno por entrada).
  observeEvent(input$uc_delete, {
    info <- input$uc_delete
    if (is.null(info) || is.null(info$cat) || is.null(info$idx)) return()
    cc <- info$cat; ii <- as.integer(info$idx)
    cur <- use_cases[[cc]]
    if (is.null(cur) || ii < 1 || ii > length(cur)) return()
    use_cases[[cc]] <- cur[-ii]
  })

  # Render del grid 3x2 con las seis categorías
  output$uc_grid <- renderUI({
    selected_country <- input$country
    cards <- lapply(names(UC_CATS), function(cc) {
      meta  <- UC_CATS[[cc]]
      items <- use_cases[[cc]]
      lis <- if (is.null(items) || length(items) == 0) {
        list(tags$li(style = "color:#aaa; font-style:italic;",
                     "No entries yet \u2014 use the form above to add one."))
      } else {
        lapply(seq_along(items), function(i) {
          it <- items[[i]]
          is_selected <- identical(it$country, selected_country)
          tags$li(
            style = paste0("margin-bottom:6px;",
                           if (is_selected) " background:#fff8d6; border-radius:4px; padding:2px 6px;" else ""),
            tags$span(style = paste0("font-weight:700; color:", meta$color, ";"),
                      it$country),
            tags$span(" \u2014 "),
            tags$span(it$text),
            tags$button(
              type = "button",
              class = "btn btn-link uc-delete-btn",
              style = "padding:0 0 0 6px; color:#bbb; font-size:14px; text-decoration:none;",
              onclick = sprintf(
                "Shiny.setInputValue('uc_delete', {cat:'%s', idx:%d, _nonce:Math.random()}, {priority:'event'});",
                cc, i),
              title = "Remove this entry",
              HTML("&times;")
            )
          )
        })
      }
      div(
        style = paste0("background:#fff; border:1px solid #e0e4ea; ",
                       "border-radius:10px; overflow:hidden; ",
                       "border-left: 6px solid ", meta$color, ";"),
        div(style = paste0("padding:10px 14px;"),
            tags$div(style = "font-weight:700; font-size:15px; color:#1a1a1a;",
                     meta$title)),
        div(style = paste0("background:", meta$color, "; color:#fff; ",
                           "padding:6px 14px; font-size:12px; font-weight:600; ",
                           "text-align:center;"),
            meta$sub),
        div(style = "padding:12px 14px; font-size:13px; line-height:1.5;",
            tags$ul(style = "margin:0; padding-left:18px;", lis))
      )
    })
    div(
      style = paste0("display:grid; grid-template-columns:",
                     "repeat(auto-fit, minmax(320px, 1fr)); gap:14px;"),
      cards
    )
  })
}

shinyApp(ui, server)
