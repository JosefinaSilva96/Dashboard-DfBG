# =============================================================================
# app.R — DfBG Country Dashboard
# -----------------------------------------------------------------------------
# Shiny dashboard for the AI & Data for Better Governance Survey (World Bank).
#
# Lets you:
#   1. Pick a country.
#   2. View, question by question, the Agency section charts, with a toggle
#      between "country only" and "country vs. income-group average".
#   3. Download the automated Country Brief in Word (.docx), replicating the
#      Austria brief format.
#
# HOW TO RUN (local):
#   1. Put the files produced by build_dfbg_database_EN.R into data/:
#        dfbg_agency.rds, dfbg_managers.rds, dfbg_systems.rds
#      (equivalent .csv files also work)
#      and, optionally, CLASS_2025_10_07.xlsx for the income groups.
#   2. Open this project in RStudio and run:  shiny::runApp()
#
# DEPLOYMENT (shinyapps.io / Posit Connect):
#   rsconnect::deployApp() with the project folder. The .rds files must be
#   included in data/ (no absolute OneDrive paths).
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

# --- module loading ----------------------------------------------------------
source("R/question_dictionary.R", local = TRUE)
source("R/manager_questions.R",  local = TRUE)
source("R/systems_questions.R",  local = TRUE)
source("R/module_metadata.R",     local = TRUE)
source("R/data_load.R",           local = TRUE)
source("R/translations_static.R", local = TRUE)
source("R/plots.R",               local = TRUE)
source("R/global_stats.R",        local = TRUE)
source("R/llm_narrative.R",       local = TRUE)
source("R/build_brief.R",         local = TRUE)

# --- data (loaded once at startup) -------------------------------------------
DATA <- tryCatch(load_dfbg(), error = function(e) {
  warning("Could not load the data: ", conditionMessage(e))
  NULL
})

# questions actually available in the current database
QLIST <- if (!is.null(DATA)) available_questions(DATA$agency) else AGENCY_QUESTIONS

# Feature flag: economy brief download disabled per a specific request
# ("for now"). To re-enable it, set this to TRUE - nothing else needs to
# change, the button and downloadHandler are already fully wired up.
BRIEF_DOWNLOAD_ENABLED <- FALSE

# World-Bank-style visual theme
wb_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF", fg = "#002245",
  primary = "#002245", secondary = "#1F77B4",
  base_font = font_google("Open Sans"),
  heading_font = font_google("Open Sans")
)

# =============================================================================
# Country -> region mapping (World Bank's 7-region classification), used for
# the region filter on the "AI Use Cases" tab. Covers common names and some
# official variants; if a country isn't in the map, it falls back to
# "Other / Not classified" (nothing breaks, it just won't filter well).
# =============================================================================

UC_REGIONS <- c("East Asia & Pacific", "Europe & Central Asia",
                "Latin America & Caribbean", "Middle East & North Africa",
                "North America", "South Asia", "Sub-Saharan Africa")

REGION_MAP <- c(
  # --- East Asia & Pacific -----------------------------------------------
  "Australia"="East Asia & Pacific","Brunei"="East Asia & Pacific","Brunei Darussalam"="East Asia & Pacific",
  "Cambodia"="East Asia & Pacific","China"="East Asia & Pacific","Fiji"="East Asia & Pacific",
  "Indonesia"="East Asia & Pacific","Japan"="East Asia & Pacific","Kiribati"="East Asia & Pacific",
  "Korea, Rep."="East Asia & Pacific","South Korea"="East Asia & Pacific","Korea"="East Asia & Pacific",
  "Lao PDR"="East Asia & Pacific","Laos"="East Asia & Pacific","Malaysia"="East Asia & Pacific",
  "Marshall Islands"="East Asia & Pacific","Micronesia, Fed. Sts."="East Asia & Pacific",
  "Micronesia"="East Asia & Pacific","Mongolia"="East Asia & Pacific","Myanmar"="East Asia & Pacific",
  "Nauru"="East Asia & Pacific","New Zealand"="East Asia & Pacific","Palau"="East Asia & Pacific",
  "Papua New Guinea"="East Asia & Pacific","Philippines"="East Asia & Pacific","Samoa"="East Asia & Pacific",
  "Singapore"="East Asia & Pacific","Solomon Islands"="East Asia & Pacific","Taiwan"="East Asia & Pacific",
  "Thailand"="East Asia & Pacific","Timor-Leste"="East Asia & Pacific","Tonga"="East Asia & Pacific",
  "Tuvalu"="East Asia & Pacific","Vanuatu"="East Asia & Pacific","Vietnam"="East Asia & Pacific",
  "Viet Nam"="East Asia & Pacific",

  # --- Europe & Central Asia ----------------------------------------------
  "Albania"="Europe & Central Asia","Andorra"="Europe & Central Asia","Armenia"="Europe & Central Asia",
  "Austria"="Europe & Central Asia","Azerbaijan"="Europe & Central Asia","Belarus"="Europe & Central Asia",
  "Belgium"="Europe & Central Asia","Bosnia and Herzegovina"="Europe & Central Asia",
  "Bulgaria"="Europe & Central Asia","Croatia"="Europe & Central Asia","Cyprus"="Europe & Central Asia",
  "Czechia"="Europe & Central Asia","Czech Republic"="Europe & Central Asia","Denmark"="Europe & Central Asia",
  "Estonia"="Europe & Central Asia","Finland"="Europe & Central Asia","France"="Europe & Central Asia",
  "Georgia"="Europe & Central Asia","Germany"="Europe & Central Asia","Greece"="Europe & Central Asia",
  "Hungary"="Europe & Central Asia","Iceland"="Europe & Central Asia","Ireland"="Europe & Central Asia",
  "Italy"="Europe & Central Asia","Kazakhstan"="Europe & Central Asia","Kosovo"="Europe & Central Asia",
  "Kyrgyz Republic"="Europe & Central Asia","Kyrgyzstan"="Europe & Central Asia","Latvia"="Europe & Central Asia",
  "Liechtenstein"="Europe & Central Asia","Lithuania"="Europe & Central Asia","Luxembourg"="Europe & Central Asia",
  "Malta"="Europe & Central Asia","Moldova"="Europe & Central Asia","Monaco"="Europe & Central Asia",
  "Montenegro"="Europe & Central Asia","Netherlands"="Europe & Central Asia",
  "North Macedonia"="Europe & Central Asia","Norway"="Europe & Central Asia","Poland"="Europe & Central Asia",
  "Portugal"="Europe & Central Asia","Romania"="Europe & Central Asia","Russia"="Europe & Central Asia",
  "Russian Federation"="Europe & Central Asia","San Marino"="Europe & Central Asia",
  "Serbia"="Europe & Central Asia","Slovak Republic"="Europe & Central Asia","Slovakia"="Europe & Central Asia",
  "Slovenia"="Europe & Central Asia","Spain"="Europe & Central Asia","Sweden"="Europe & Central Asia",
  "Switzerland"="Europe & Central Asia","Tajikistan"="Europe & Central Asia","Turkiye"="Europe & Central Asia",
  "Turkey"="Europe & Central Asia","Turkmenistan"="Europe & Central Asia","Ukraine"="Europe & Central Asia",
  "United Kingdom"="Europe & Central Asia","Uzbekistan"="Europe & Central Asia",

  # --- Latin America & Caribbean -------------------------------------------
  "Antigua and Barbuda"="Latin America & Caribbean","Argentina"="Latin America & Caribbean",
  "Bahamas, The"="Latin America & Caribbean","Bahamas"="Latin America & Caribbean",
  "Barbados"="Latin America & Caribbean","Belize"="Latin America & Caribbean",
  "Bolivia"="Latin America & Caribbean","Brazil"="Latin America & Caribbean",
  "Chile"="Latin America & Caribbean","Colombia"="Latin America & Caribbean",
  "Costa Rica"="Latin America & Caribbean","Cuba"="Latin America & Caribbean",
  "Dominica"="Latin America & Caribbean","Dominican Republic"="Latin America & Caribbean",
  "Ecuador"="Latin America & Caribbean","El Salvador"="Latin America & Caribbean",
  "Grenada"="Latin America & Caribbean","Guatemala"="Latin America & Caribbean",
  "Guyana"="Latin America & Caribbean","Haiti"="Latin America & Caribbean",
  "Honduras"="Latin America & Caribbean","Jamaica"="Latin America & Caribbean",
  "Mexico"="Latin America & Caribbean","Nicaragua"="Latin America & Caribbean",
  "Panama"="Latin America & Caribbean","Paraguay"="Latin America & Caribbean",
  "Peru"="Latin America & Caribbean","St. Kitts and Nevis"="Latin America & Caribbean",
  "St. Lucia"="Latin America & Caribbean","St. Vincent and the Grenadines"="Latin America & Caribbean",
  "Suriname"="Latin America & Caribbean","Trinidad and Tobago"="Latin America & Caribbean",
  "Uruguay"="Latin America & Caribbean","Venezuela"="Latin America & Caribbean",
  "Venezuela, RB"="Latin America & Caribbean",

  # --- Middle East & North Africa -------------------------------------------
  "Algeria"="Middle East & North Africa","Bahrain"="Middle East & North Africa",
  "Djibouti"="Middle East & North Africa","Egypt"="Middle East & North Africa",
  "Egypt, Arab Rep."="Middle East & North Africa","Iran"="Middle East & North Africa",
  "Iran, Islamic Rep."="Middle East & North Africa","Iraq"="Middle East & North Africa",
  "Israel"="Middle East & North Africa","Jordan"="Middle East & North Africa",
  "Kuwait"="Middle East & North Africa","Lebanon"="Middle East & North Africa",
  "Libya"="Middle East & North Africa","Malta"="Middle East & North Africa",
  "Morocco"="Middle East & North Africa","Oman"="Middle East & North Africa",
  "Qatar"="Middle East & North Africa","Saudi Arabia"="Middle East & North Africa",
  "Syria"="Middle East & North Africa","Syrian Arab Republic"="Middle East & North Africa",
  "Tunisia"="Middle East & North Africa","United Arab Emirates"="Middle East & North Africa",
  "West Bank and Gaza"="Middle East & North Africa","Yemen"="Middle East & North Africa",
  "Yemen, Rep."="Middle East & North Africa",

  # --- North America ---------------------------------------------------------
  "Canada"="North America","United States"="North America","United States of America"="North America",

  # --- South Asia --------------------------------------------------------------
  "Afghanistan"="South Asia","Bangladesh"="South Asia","Bhutan"="South Asia","India"="South Asia",
  "Maldives"="South Asia","Nepal"="South Asia","Pakistan"="South Asia","Sri Lanka"="South Asia",

  # --- Sub-Saharan Africa -----------------------------------------------------
  "Angola"="Sub-Saharan Africa","Benin"="Sub-Saharan Africa","Botswana"="Sub-Saharan Africa",
  "Burkina Faso"="Sub-Saharan Africa","Burundi"="Sub-Saharan Africa","Cabo Verde"="Sub-Saharan Africa",
  "Cameroon"="Sub-Saharan Africa","Central African Republic"="Sub-Saharan Africa","Chad"="Sub-Saharan Africa",
  "Comoros"="Sub-Saharan Africa","Congo, Dem. Rep."="Sub-Saharan Africa","Congo, Rep."="Sub-Saharan Africa",
  "DRC"="Sub-Saharan Africa","Cote d'Ivoire"="Sub-Saharan Africa","Ivory Coast"="Sub-Saharan Africa",
  "Equatorial Guinea"="Sub-Saharan Africa","Eritrea"="Sub-Saharan Africa","Eswatini"="Sub-Saharan Africa",
  "Ethiopia"="Sub-Saharan Africa","Gabon"="Sub-Saharan Africa","Gambia, The"="Sub-Saharan Africa",
  "Gambia"="Sub-Saharan Africa","Ghana"="Sub-Saharan Africa","Guinea"="Sub-Saharan Africa",
  "Guinea-Bissau"="Sub-Saharan Africa","Kenya"="Sub-Saharan Africa","Lesotho"="Sub-Saharan Africa",
  "Liberia"="Sub-Saharan Africa","Madagascar"="Sub-Saharan Africa","Malawi"="Sub-Saharan Africa",
  "Mali"="Sub-Saharan Africa","Mauritania"="Sub-Saharan Africa","Mauritius"="Sub-Saharan Africa",
  "Mozambique"="Sub-Saharan Africa","Namibia"="Sub-Saharan Africa","Niger"="Sub-Saharan Africa",
  "Nigeria"="Sub-Saharan Africa","Rwanda"="Sub-Saharan Africa","Sao Tome and Principe"="Sub-Saharan Africa",
  "Senegal"="Sub-Saharan Africa","Seychelles"="Sub-Saharan Africa","Sierra Leone"="Sub-Saharan Africa",
  "Somalia"="Sub-Saharan Africa","South Africa"="Sub-Saharan Africa","South Sudan"="Sub-Saharan Africa",
  "Sudan"="Sub-Saharan Africa","Tanzania"="Sub-Saharan Africa","Togo"="Sub-Saharan Africa",
  "Uganda"="Sub-Saharan Africa","Zambia"="Sub-Saharan Africa","Zimbabwe"="Sub-Saharan Africa"
)

# Returns a country's region; "Other / Not classified" if it's not in the map
# (e.g. if the exact name in your data differs from the variants above —
# let me know and I'll add it).
country_region <- function(cty) {
  r <- unname(REGION_MAP[cty])
  ifelse(is.na(r), "Other / Not classified", r)
}

# =============================================================================
# UI
# =============================================================================

ui <- page_sidebar(
  title = "AI & Data for Better Governance — Economies Dashboard",
  theme = wb_theme,

  # Library used to export the Use Cases grid as a PNG (rasterizes the DOM).
  # NOTE: served FROM the project's www/ folder (not from an external CDN)
  # because internal/corporate networks (e.g. *-int.worldbank.org domains)
  # often block external CDNs (cdnjs, unpkg, etc.), which meant html2canvas
  # never loaded and the "Download as PNG" button threw the error
  # "Could not capture the image". Shiny automatically serves any file
  # inside www/ at the app's root, so this works with or without internet
  # access.
  tags$head(
    tags$script(src = "html2canvas.min.js"),
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
                    selected = sort(country_choices(DATA)$country)[1]),
        uiOutput("ig_badge"),
        if (BRIEF_DOWNLOAD_ENABLED) {
          tagList(
            conditionalPanel(
              condition = "input.country != '' && typeof input.family !== 'undefined'",
              downloadButton("dl_brief", "Download economy brief (.docx)",
                             class = "btn-primary w-100"),
              div(class = "small text-muted mt-1",
                  "The narrative sections of this brief are AI-generated from survey data and have not been reviewed by World Bank staff.")
            ),
            conditionalPanel(
              condition = "input.country == '' || typeof input.family === 'undefined'",
              div(class = "small text-muted",
                  "Select an economy and a questionnaire to enable the download.")
            )
          )
        } else {
          tagList(
            tags$button("Download economy brief (.docx)", class = "btn btn-primary w-100",
                       disabled = "disabled"),
            div(class = "small text-muted mt-1", tags$em("Coming soon"))
          )
        },
        uiOutput("api_status"),
        hr(),
        radioButtons("family", "Questionnaire",
                     choices = c("Agency", "Managers", "Systems"),
                     selected = "Agency", inline = TRUE),
        uiOutput("family_hint"),
        hr(),
        uiOutput("scope_picker"),
        uiOutput("sector_picker")
      )
    }
  ),

  navset_card_tab(
    nav_panel(
      "About the survey",
      card(card_body(
        h4("About This Dashboard"),
        p("This dashboard lets you explore the results of the AI and Data for ",
          "Better Governance Survey economy by economy, compare an economy ",
          "against the average of its World Bank income group, browse ",
          "curated examples of AI use cases by region, and generate an ",
          "automated economy brief summarizing the survey results."),
        p(tags$em(
          "The survey responses shown throughout this dashboard reflect the ",
          "views and self-reported information of the participating ",
          "governments and respondents. They do not represent the views, ",
          "position, or endorsement of the World Bank Group, its Board of ",
          "Executive Directors, or the governments they represent."
        )),
        p("This survey was carried out as part of the ",
          tags$strong("World Development Report 2026: Decoding AI"),
          ". More information about the report is available at ",
          tags$a(href = "https://www.worldbank.org/en/publication/wdr2026",
                 target = "_blank",
                 "worldbank.org/en/publication/wdr2026"), ". The main results ",
          "from the survey will be published in a WDR background paper, ",
          "titled \u201cHow is Government Using AI?\u201d (by Timothy Lundy, Shyam ",
          "Jayanti Patel, Manuel Ramos-Maqueda, Daniel Rogger, Flavia Sacco ",
          "Capurro, and Josefina Silva Fuentealba)."),
        hr(),
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
        p(tags$strong("AI and Data for Better Governance team"), br(),
          tags$a(href = "mailto:dataforbettergovernance@worldbank.org",
                 "dataforbettergovernance@worldbank.org")), 
        p(tags$strong("Josefina Silva Fuentealba"), br(),
          tags$a(href = "mailto:jsilvafuentealba@worldbank.org",
                 "jsilvafuentealba@worldbank.org")),
      ))
    ),
    nav_panel(
      "How to use this dashboard",
      card(card_body(
        h4("What this dashboard does"),
        p("Pick an economy to explore its questionnaire responses ",
          "and download an automated economy brief in Word. In ",
          tags$strong("Explore charts"), ", scroll down to see all the ",
          "questionnaire's charts, grouped by module."),
        tags$ul(
          tags$li("Start in the sidebar: choose an ", tags$strong("Economy"),
                  " and a ", tags$strong("Questionnaire"),
                  " (Agency, Managers, or Systems)."),
          tags$li("Charts can show the economy alone or compared with the ",
                  "average of its World Bank income group, via ",
                  "the \u201cChart view\u201d option."),
          tags$li("For Managers and Systems, use the ", tags$strong("Sector"),
                  " picker to look at a single sector, or compare all ",
                  "sectors at once."),
          tags$li("Charts are organized into the ", tags$strong("modules"),
                  " of the real questionnaire (e.g. \u201cAdoption of AI in ",
                  "Government\u201d, \u201cBarriers and Risks\u201d) \u2014 just scroll ",
                  "down to move from one module to the next. Each chart has ",
                  "a footnote with the full question text and number."),
          tags$li("Each chart can be downloaded as a PNG image using the ",
                  tags$strong("Download chart (PNG)"),
                  " button below it."),
          tags$li("The ", tags$strong("Text responses"),
                  " tab shows the open-text answers for the selected ",
                  "economy and questionnaire."),
          tags$li("The ", tags$strong("AI Use Cases"),
                  " tab shows curated examples of AI applications by ",
                  "category. Use the region filter to narrow down to one ",
                  "World Bank region, and download the summary as an image."),
          tags$li("The ", tags$strong("Download economy brief"),
                  " button in the sidebar generates an automated Word brief ",
                  "for the selected economy. ",
                  tags$em("(Coming soon \u2014 temporarily disabled.)"))
        )
      ))
    ),
    nav_panel(
      "Explore charts",
      uiOutput("header_cards"),
      uiOutput("module_charts_ui")
    ),
    nav_panel(
      "Text responses",
      uiOutput("text_cards")
    ),
    nav_panel(
      "AI Use Cases",
      div(
        style = "padding:8px 12px;",

        # === Region filter + download (affects what's displayed) ===
        div(
          style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:10px; margin-bottom:14px;",
          div(style = "display:flex; align-items:center; gap:10px;",
              tags$span(style = "font-weight:600; font-size:14px;", "Filter by region:"),
              div(style = "min-width:280px;",
                  selectInput("uc_region", NULL,
                             choices = c("All regions", UC_REGIONS, "Other / Not classified"),
                             selected = "All regions", width = "100%"))),
          tags$button(
            id = "uc_download", type = "button",
            class = "action-button btn btn-primary",
            style = "background:#0F6E56; border-color:#0F6E56; font-weight:600;",
            HTML("&#x2B07;&#xFE0E; Download as PNG")
          )
        ),

        # === Capturable: title + grid (this is what gets downloaded as PNG) ===
        div(
          id = "uc_capture",
          style = "background:#fff; padding:16px; border-radius:8px;",
          div(
            style = "display:flex; align-items:baseline; gap:14px; flex-wrap:wrap; margin-bottom:14px; border-bottom:1px solid #ddd; padding-bottom:10px;",
            h4(style = "margin:0; font-weight:700; color:#1a1a1a;",
               "AI Use Cases \u2014 Six Categories Across DfBG Questionnaires"),
            tags$span(style = "font-size:12px; color:#888;",
                      "Source: DfBG Agency, Manager & Systems Questionnaires"),
            uiOutput("uc_region_label", inline = TRUE)
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

  # --- Family / sector / question reactives ---------------------------

  # Question dictionary available for the chosen family
  qlist_fam <- reactive({
    req(input$family)
    questions_for_family(req_data(), input$family)
  })

  # Database (slot name) for the family
  base_fam <- reactive({ req(input$family); FAMILY_BASE[[input$family]] })

  # Sectors available for Managers/Systems (Agency has no sectors)
  sectors_fam <- reactive({
    req(input$family)
    if (input$family == "Agency") return(NULL)
    d <- req_data(); b <- base_fam()
    df <- dplyr::filter(d[[b]], country == input$country)
    if (nrow(df) == 0) return(character(0))
    sort(unique(stringr::str_remove(df$questionnaire, "^(Systems|Manager)_")))
  })

  # Sector picker (Managers/Systems only)
  output$sector_picker <- renderUI({
    if (is.null(input$family) || input$family == "Agency") return(NULL)
    if (is.null(input$scope) || input$scope != "country") return(NULL)
    secs <- sectors_fam()
    if (is.null(secs)) return(NULL)
    if (length(secs) == 0)
      return(div(class = "small text-muted", "No sector responses for this country."))
    selectInput("sector", "Sector", choices = secs, selected = secs[1])
  })

  # Scope picker: label changes depending on the family
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

  # All modules available for the chosen family, in the real questionnaire
  # order, each with its qids already filtered down to what exists in the
  # database. A module is no longer picked one at a time: ALL of them are
  # shown, one after another, each with its own section header (the user
  # just scrolls).
  family_modules <- reactive({
    req(input$family)
    ql   <- qlist_fam()
    mods <- modules_for_family(input$family)
    mods <- lapply(mods, function(m) list(title = m$title, qids = intersect(m$qids, names(ql))))
    Filter(function(m) length(m$qids) > 0, mods)
  })

  # Effective data used for charting:
  #  - Agency: the full database (compares against the income group).
  #  - Managers/Systems in 'compare' mode (across sectors): ALL of the
  #    country's rows (sector not filtered) → make_plot delegates to
  #    plot_*_sectors.
  #  - Managers/Systems in 'country' mode (a single sector): filters down
  #    to the chosen sector and exposes it as an 'agency'-like base for the
  #    detailed view.
  plot_data <- reactive({
    req(input$country)
    d <- req_data(); b <- base_fam()
    if (input$family == "Agency") return(d)

    # comparing across sectors: don't filter
    if (input$scope == "compare") return(d)

    # single-sector view: filter
    sec <- input$sector
    if (is.null(sec)) return(d)
    sub <- dplyr::filter(d[[b]],
                         stringr::str_remove(questionnaire, "^(Systems|Manager)_") == sec)
    d2 <- d
    d2[[b]] <- sub
    d2
  })

  # All question ids, in order, across ALL modules concatenated (used to
  # index the dynamic outputs mod_plot_1, mod_plot_2, ...).
  all_qids <- reactive({
    unlist(lapply(family_modules(), function(m) m$qids))
  })

  # Main panel: every module of the questionnaire, each with its own section
  # header and ALL of its charts, one after another. The user scrolls
  # instead of picking a specific module.
  output$module_charts_ui <- renderUI({
    if (is.null(input$country) || !nzchar(input$country) || is.null(input$family)) {
      return(div(class = "text-muted p-4",
                 "Select an economy and a questionnaire from the sidebar to see charts."))
    }
    mods <- family_modules()
    if (length(mods) == 0) {
      return(div(class = "text-muted p-4", "No questions available for this questionnaire."))
    }
    idx <- 0
    tagList(lapply(mods, function(m) {
      cards <- lapply(m$qids, function(qid) {
        idx <<- idx + 1
        i <- idx
        div(
          class = "card mb-3",
          div(class = "card-header", textOutput(paste0("mod_title_", i))),
          div(class = "card-body", uiOutput(paste0("mod_body_", i))),
          div(class = "card-footer", uiOutput(paste0("mod_foot_", i)))
        )
      })
      tagList(
        tags$h5(class = "mt-3 mb-2", m$title,
                tags$span(class = "text-muted small",
                         paste0(" \u00b7 ", input$family,
                                if (input$family != "Agency" && !is.null(input$sector)) paste0(" \u00b7 ", input$sector) else ""))),
        cards
      )
    }))
  })

  # Dynamically generates the outputs (title/body/footer/plot/download) for
  # EACH question across ALL modules (running index). Standard Shiny pattern
  # for a variable number of outputs: they get recreated whenever the
  # family/country/etc. changes.
  MAX_MODULE_SLOTS <- 40
  observe({
    qids <- all_qids()
    ql   <- qlist_fam()
    fam  <- input$family

    for (i in seq_len(MAX_MODULE_SLOTS)) {
      local({
        ii <- i
        if (ii > length(qids)) {
          output[[paste0("mod_title_", ii)]] <- renderText("")
          output[[paste0("mod_body_", ii)]]  <- renderUI(NULL)
          output[[paste0("mod_foot_", ii)]]  <- renderUI(NULL)
          return()
        }
        qid <- qids[ii]
        q   <- ql[[qid]]

        output[[paste0("mod_title_", ii)]] <- renderText({
          paste0("Q", question_number(qid), " \u2014 ", q$short)
        })

        use_sectoral <- base_fam() %in% c("manager", "systems") &&
                        !is.null(input$scope) && input$scope == "compare"
        show_table <- q$type == "single" && use_sectoral

        # ggplot object: computed ONCE and reused to display it, to compute
        # the dynamic height (based on how many categories it has), and to
        # export it as PNG — this way it isn't recalculated 3 times, and
        # there's no fixed height squashing the labels when there are many
        # categories.
        plot_obj <- reactive({
          if (q$type == "text" || show_table) return(NULL)
          make_plot(plot_data(), q, input$country, scope = input$scope, base = base_fam())
        })

        output[[paste0("mod_body_", ii)]] <- renderUI({
          if (q$type == "text") {
            tbl <- make_text(plot_data(), q, input$country, base = base_fam())
            return(if (nrow(tbl) == 0) p("No response.") else tags$ul(map(tbl$Response, ~ tags$li(.x))))
          }
          tbl_ui <- make_table_ui(plot_data(), q, input$country,
                                  scope = input$scope, base = base_fam())
          if (!is.null(tbl_ui)) return(tbl_ui)

          gg <- plot_obj()
          n_items <- attr(gg, "n_items")
          if (is.null(n_items)) n_items <- length(q$levels %||% character(0))
          if (n_items == 0) n_items <- 6
          px_height <- max(320, min(900, round(90 + n_items * 34)))

          tagList(
            plotOutput(paste0("mod_plot_", ii), height = paste0(px_height, "px")),
            div(style = "text-align:right; margin-top:6px;",
                downloadButton(paste0("mod_dl_", ii), "Download chart (PNG)",
                               class = "btn-outline-secondary btn-sm"))
          )
        })

        output[[paste0("mod_plot_", ii)]] <- renderPlot({
          plot_obj()
        })

        output[[paste0("mod_dl_", ii)]] <- downloadHandler(
          filename = function() {
            paste0("dfbg_", tolower(gsub("[^A-Za-z]+", "_", input$country)), "_", qid, ".png")
          },
          content = function(file) {
            p <- plot_obj()
            if (is.null(p)) p <- empty_plot("", "No chart to export")
            n_items <- attr(p, "n_items")
            if (is.null(n_items)) n_items <- length(q$levels %||% character(0))
            if (n_items == 0) n_items <- 6
            h_in <- max(4.2, min(11, 1.6 + n_items * 0.55))
            ggplot2::ggsave(filename = file, plot = p, device = "png",
                            width = 10, height = h_in, dpi = 200, bg = "white")
          }
        )

        output[[paste0("mod_foot_", ii)]] <- renderUI({
          tags$p(class = "small text-muted mb-0",
                 question_footnote(fam, qid, fallback_title = q$title))
        })
      })
    }
  })

  output$api_status <- renderUI({
    if (have_api_key()) {
      div(class = "small text-success mb-2",
          tags$i(class = "bi bi-check-circle"),
          "Claude API detected — the brief will include LLM-written analysis.")
    } else {
      NULL
    }
  })

  # Brief download
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
  # AI USE CASES — editable grid with six categories
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

  # state: list of entries per category, loaded from data/ai_use_cases.csv
  # (REAL responses to the "Key AI applications" questions from Agency,
  # Managers, and Systems, classified into the 6 categories). If that file
  # doesn't exist yet, it falls back to an empty list per category (doesn't
  # break the app).
  load_use_cases <- function(path = "data/ai_use_cases.csv") {
    empty <- setNames(rep(list(list()), length(UC_CATS)), names(UC_CATS))
    if (!file.exists(path)) return(empty)
    tbl <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8"),
                    error = function(e) NULL)
    if (is.null(tbl) || !all(c("country", "category", "text") %in% names(tbl))) return(empty)
    out <- empty
    for (cc in names(UC_CATS)) {
      sub <- tbl[tbl$category == cc, ]
      if (nrow(sub) == 0) next
      out[[cc]] <- lapply(seq_len(nrow(sub)), function(i) {
        list(country = sub$country[i], text = sub$text[i])
      })
    }
    out
  }

  use_cases <- do.call(reactiveValues, load_use_cases())


  # Download the grid as PNG (sends a message to the client, which rasterizes the DOM)
  observeEvent(input$uc_download, {
    session$sendCustomMessage("uc_download_png",
                              list(filename = "dfbg_ai_use_cases.png"))
  })

  # Delete: each little button in the grid calls Shiny.setInputValue('uc_delete', ...)
  # with priority:'event' so it always fires, even if the payload repeats.
  # This way we do NOT need to register N observers (one per entry).
  observeEvent(input$uc_delete, {
    info <- input$uc_delete
    if (is.null(info) || is.null(info$cat) || is.null(info$idx)) return()
    cc <- info$cat; ii <- as.integer(info$idx)
    cur <- use_cases[[cc]]
    if (is.null(cur) || ii < 1 || ii > length(cur)) return()
    use_cases[[cc]] <- cur[-ii]
  })

  # Render the 3x2 grid with the six categories
  output$uc_region_label <- renderUI({
    rf <- input$uc_region %||% "All regions"
    note <- "\u00b7 Showing one example per economy per category"
    if (rf == "All regions") {
      return(tags$span(style = "font-size:12px; color:#888;", note))
    }
    tags$span(style = "font-size:12px; color:#888;",
              paste0("\u00b7 Region: ", rf, "  ", note))
  })

  # Truncates each case's text to a maximum of 2 sentences (with a
  # character-count backstop in case a single sentence runs on forever
  # without a period), so cards don't get huge, especially in "All regions".
  truncate_uc_text <- function(t, max_sentences = 2, max_chars = 200) {
    t <- trimws(t)
    parts <- unlist(stringr::str_split(t, "(?<=[.!?])\\s+"))
    parts <- parts[nzchar(parts)]
    if (length(parts) > max_sentences) {
      short <- paste(parts[seq_len(max_sentences)], collapse = " ")
    } else {
      short <- t
    }
    if (nchar(short) > max_chars) {
      short <- paste0(substr(short, 1, max_chars - 1), "\u2026")
    } else if (length(parts) > max_sentences) {
      short <- paste0(short, if (!grepl("[.!?\u2026]$", short)) "\u2026" else "")
    }
    short
  }

  output$uc_grid <- renderUI({
    selected_country <- input$country
    region_filter <- input$uc_region %||% "All regions"
    cards <- lapply(names(UC_CATS), function(cc) {
      meta  <- UC_CATS[[cc]]
      items <- use_cases[[cc]]

      # Indices that pass the region filter (the ORIGINAL index within
      # `items` is preserved, so the delete button keeps pointing to the
      # right entry even when the view is filtered by region).
      keep_idx <- if (is.null(items) || length(items) == 0) {
        integer(0)
      } else {
        idx <- Filter(function(i) {
          region_filter == "All regions" ||
            identical(country_region(items[[i]]$country), region_filter)
        }, seq_along(items))
        if (length(idx) > 0) {
          # A single country can have several entries in the same category
          # (one per sector/questionnaire) -> we show only ONE per country,
          # always (region chosen or not), so the card doesn't get huge.
          seen <- character(0)
          idx <- Filter(function(i) {
            cty <- items[[i]]$country
            if (cty %in% seen) return(FALSE)
            seen <<- c(seen, cty)
            TRUE
          }, idx)
        }
        idx
      }

      # Cap on items per card: some categories (e.g. Tax) have far more
      # economies than others (e.g. Infra), which made the exported image
      # very unbalanced (very long columns vs. short ones). With an even
      # cap, all cards end up a more comparable size.
      MAX_ITEMS_PER_CARD <- 12
      n_total_matches <- length(keep_idx)
      n_extra <- max(0, n_total_matches - MAX_ITEMS_PER_CARD)
      keep_idx <- utils::head(keep_idx, MAX_ITEMS_PER_CARD)

      lis <- if (length(keep_idx) == 0) {
        msg <- if (is.null(items) || length(items) == 0)
          "No entries yet \u2014 use the form above to add one."
        else
          paste0("No entries for ", region_filter, " yet.")
        list(tags$li(style = "color:#aaa; font-style:italic;", msg))
      } else {
        lapply(keep_idx, function(i) {
          it <- items[[i]]
          is_selected <- identical(it$country, selected_country)
          tags$li(
            style = paste0("margin-bottom:6px;",
                           if (is_selected) " background:#fff8d6; border-radius:4px; padding:2px 6px;" else ""),
            tags$span(style = paste0("font-weight:700; color:", meta$color, ";"),
                      it$country),
            tags$span(" \u2014 "),
            tags$span(title = it$text, truncate_uc_text(it$text)),
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
      if (n_extra > 0) {
        lis <- c(lis, list(tags$li(
          style = "color:#888; font-style:italic; list-style:none; margin-left:-18px; margin-top:4px;",
          paste0("+ ", n_extra, " more econom", if (n_extra == 1) "y" else "ies",
                " reported examples in this category")
        )))
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
