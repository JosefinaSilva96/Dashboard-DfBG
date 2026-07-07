# =============================================================================
# build_brief.R
# -----------------------------------------------------------------------------
# Genera el Economy Brief en Word (.docx), replicando la estructura del brief
# de referencia (Türkiye):
#
#   [sin portada — ver nota abajo]
#   - Disclaimer / boilerplate legal
#   - "The AI and Data for Better Governance Survey" (intro fija)
#   - "What Does AI Adoption Look Like in {economy}?"
#       - Table 1: cuestionarios completados por sector
#       - Current AI Adoption in Government (narrativa + figura)
#       - Institutional Readiness and Capacity (narrativa + tabla + tabla
#         de entrenamiento por sector)
#       - Barriers to AI Adoption (narrativa + heatmaps overall/sector/sistema)
#       - Priority actions
#   - "What Does AI Adoption Look Like Globally?"  (NUEVO: comparación entre
#     TODOS los países, por income group)
#   - "What Is the Future of AI in Government?"    (NUEVO: potencial de IA
#     por sector, agregado global, pregunta 22 de Managers)
#   - Appendix — Summary of Full Survey Responses  (el detalle completo por
#     sector que antes vivía en el cuerpo del brief)
#
# NOTA sobre la portada: se genera una portada simple (título, país, income
# group, cantidad de cuestionarios). NO reproduce la ilustración del brief de
# Türkiye (edificio + gráfico de barras): es arte original del Banco y no
# tenemos ese asset. Si más adelante Josefina consigue la imagen de fondo,
# se puede insertar con body_add_img() dentro de add_cover_page() en vez del
# bloque de texto actual.
#
# Usa `officer` + `flextable`. Los gráficos se insertan como PNG temporales
# generados con make_plot() / las funciones de global_stats.R.
# =============================================================================

library(officer)
library(flextable)

WB_NAVY    <- "#002245"
WB_BLUE    <- "#1F77B4"
WB_AMBER   <- "#B5651D"   # color de los títulos de sección (H1), tipo brief WB
GREY_BG    <- "#F2F2F2"
GREEN_OK   <- "#E2F0E9"

# =============================================================================
# Helpers de formato / estilo
# =============================================================================

ft_indicator <- function(df) {
  flextable(df) |>
    bg(part = "header", bg = WB_NAVY) |>
    color(part = "header", color = "white") |>
    bold(part = "header") |>
    border_outer(border = fp_border(color = "grey70", width = 1)) |>
    border_inner(border = fp_border(color = "grey85", width = 0.5)) |>
    padding(padding = 4, part = "all") |>
    fontsize(size = 9, part = "all") |>
    autofit()
}

add_plot <- function(doc, gg, w = 6.5, h = NULL) {
  if (is.null(gg)) return(doc)
  if (is.null(h)) {
    n <- attr(gg, "n_items")
    if (is.null(n)) {
      h <- 4.2
    } else {
      h <- max(4.2, 1.6 + n * 0.55)
      h <- min(h, 9.0)
    }
  }
  tmp <- tempfile(fileext = ".png")
  ggplot2::ggsave(tmp, gg, width = w, height = h, dpi = 220, bg = "white")
  doc <- body_add_img(doc, tmp, width = w, height = h)
  doc
}

# Título de sección principal (H1), estilo brief WB: color ámbar, grande
add_h1 <- function(doc, text) {
  doc <- body_add_par(doc, "", style = "Normal")
  body_add_fpar(doc, fpar(ftext(text, fp_text(bold = TRUE, font.size = 18, color = WB_AMBER))))
}

# Subtítulo (H2), estilo brief WB: azul, itálica
add_h2 <- function(doc, text) {
  doc <- body_add_par(doc, "", style = "Normal")
  body_add_fpar(doc, fpar(ftext(text, fp_text(bold = TRUE, italic = TRUE, font.size = 13, color = WB_BLUE))))
}

# Caption de tabla/figura: "Table N. ..." / "Figure N. ..." en negrita
add_caption <- function(doc, kind, n, text) {
  body_add_fpar(doc, fpar(ftext(paste0(kind, " ", n, ". ", text),
                                fp_text(bold = TRUE, font.size = 10.5))))
}

# Línea "Source: ..." / "Note: ..." en itálica gris, debajo de tablas/figuras
add_source_note <- function(doc, source_text, note_text = NULL) {
  doc <- body_add_fpar(doc, fpar(
    ftext("Source: ", fp_text(italic = TRUE, font.size = 9, color = "#666666")),
    ftext(source_text, fp_text(italic = TRUE, font.size = 9, color = "#666666"))
  ))
  if (!is.null(note_text) && nzchar(note_text)) {
    doc <- body_add_fpar(doc, fpar(
      ftext("Note: ", fp_text(italic = TRUE, font.size = 9, color = "#666666")),
      ftext(note_text, fp_text(italic = TRUE, font.size = 9, color = "#666666"))
    ))
  }
  doc <- body_add_par(doc, "", style = "Normal")
  doc
}

add_paragraphs <- function(doc, text) {
  if (is.null(text) || !nzchar(trimws(text))) return(doc)
  for (para in strsplit(text, "\n\n+")[[1]]) {
    if (nzchar(trimws(para))) doc <- body_add_par(doc, trimws(para), style = "Normal")
  }
  doc
}

# --- valor decodificado de una pregunta single para el país ------------------
country_value <- function(data, q, country_name) {
  ag <- dplyr::filter(data$agency, country == country_name)
  if (nrow(ag) == 0 || is.null(q$cols)) return(NA_character_)
  col <- q$cols[1]
  if (!col %in% names(ag)) return(NA_character_)
  raw <- ag[[col]][1]
  v <- if (!is.null(q$recoder)) q$recoder(raw) else recode_keep(raw, q$labels %||% character(0))
  if (is.na(v) || v == "") return(NA_character_) else v
}

# --- promedio modal del income group para una single ------------------------
ig_modal <- function(data, q, country_name) {
  ag  <- data$agency
  col <- q$cols[1]
  if (is.null(col) || !col %in% names(ag)) return(NA_character_)
  ig  <- ag$income_group[ag$country == country_name][1]
  peers <- ag |> dplyr::filter(!is.na(.data[[col]]))
  if (!is.na(ig)) peers <- dplyr::filter(peers, income_group == ig)
  if (nrow(peers) == 0) return(NA_character_)
  tab <- peers |>
    dplyr::mutate(.c = if (!is.null(q$recoder)) q$recoder(.data[[col]])
                       else recode_keep(.data[[col]], q$labels %||% character(0))) |>
    dplyr::count(.c, sort = TRUE)
  top <- tab$.c[1]
  pct <- round(100 * tab$n[1] / sum(tab$n))
  ig_lbl <- if (is.na(ig)) "all countries" else ig
  paste0(top, " (", pct, "% of ", ig_lbl, ")")
}

# =============================================================================
# Tabla indicador país vs income group para una lista de preguntas single
# =============================================================================

indicator_table <- function(data, qids, country_name) {
  rows <- purrr::map_dfr(qids, function(qid) {
    q <- AGENCY_QUESTIONS[[qid]]
    tibble::tibble(
      Indicator = gsub("\u2014", "-", q$short, useBytes = TRUE),  # "—" a veces sale mal en tablas Word/LibreOffice
      !!country_name := country_value(data, q, country_name) %||% "\u2014",
      `Income group average` = ig_modal(data, q, country_name) %||% "\u2014"
    )
  })
  rows |> dplyr::mutate(dplyr::across(everything(), ~ tidyr::replace_na(., "\u2014")))
}

# =============================================================================
# Table 1 — cuestionarios completados por sector (Manager / System)
# =============================================================================

SECTOR_LIST <- c("CivilService", "Education", "Health", "Procurement",
                 "PublicFinance", "Tax")

# Convierte el nombre crudo del sector (tal cual viene en `questionnaire`,
# ej. "CivilService", "PublicFinance", sin espacios ni guion bajo) en una
# etiqueta legible ("Civil Service", "Public Finance"). Soporta también la
# variante con guion bajo por si el formato cambia en el futuro.
pretty_sector <- function(s) {
  s <- gsub("_", " ", s)
  s <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", s, useBytes = TRUE)
  trimws(s)
}

questionnaire_grid <- function(data, country_name) {
  pretty <- pretty_sector
  mg <- dplyr::filter(data$manager, country == country_name)
  sy <- dplyr::filter(data$systems, country == country_name)
  mg_sectors <- if (nrow(mg) > 0) unique(stringr::str_remove(mg$questionnaire, "^(Systems|Manager)_")) else character(0)
  sy_sectors <- if (nrow(sy) > 0) unique(stringr::str_remove(sy$questionnaire, "^(Systems|Manager)_")) else character(0)

  mk_row <- function(label, sectors_done) {
    vals <- ifelse(SECTOR_LIST %in% sectors_done, "\u2713", "")
    row <- as.list(vals)
    names(row) <- pretty(SECTOR_LIST)
    tibble::as_tibble(c(list(` ` = label), row))
  }
  dplyr::bind_rows(mk_row("Manager", mg_sectors), mk_row("System", sy_sectors))
}

ft_questionnaire_grid <- function(df) {
  ft <- flextable(df) |>
    bold(j = 1) |>
    align(align = "center", part = "all", j = 2:ncol(df)) |>
    valign(valign = "center", part = "all") |>
    fontsize(size = 9, part = "all") |>
    padding(padding = 5, part = "all") |>
    border_outer(border = fp_border(color = "grey70")) |>
    border_inner(border = fp_border(color = "grey85")) |>
    autofit()
  for (j in 2:ncol(df)) {
    checked <- which(df[[j]] == "\u2713")
    if (length(checked)) ft <- bg(ft, i = checked, j = j, bg = GREEN_OK)
  }
  ft
}

# =============================================================================
# Table 2 — indicadores de entrenamiento / capacidad, por sector (Managers)
# =============================================================================

TRAINING_QIDS <- c("q25", "q26", "q27", "q28", "q29", "q30", "q31", "q32")

sector_indicator_table <- function(data, qids, country_name, base = "manager",
                                   qlist = MANAGER_QUESTIONS) {
  df <- dplyr::filter(data[[base]], country == country_name)
  if (nrow(df) == 0) return(NULL)
  df <- df |> dplyr::mutate(sector = stringr::str_remove(questionnaire, "^(Systems|Manager)_"))

  rows <- purrr::map_dfr(seq_len(nrow(df)), function(i) {
    sub <- df[i, ]
    vals <- lapply(qids, function(qid) {
      q <- qlist[[qid]]
      if (is.null(q) || is.null(q$cols)) return("N/A")
      col <- q$cols[1]
      if (!col %in% names(sub)) return("N/A")
      raw <- sub[[col]][1]
      v <- if (!is.null(q$recoder)) q$recoder(raw) else recode_keep(raw, q$labels %||% character(0))
      if (is.na(v) || v == "") "N/A" else v
    })
    names(vals) <- vapply(qids, function(qid) gsub("\u2014", "-", qlist[[qid]]$short %||% qid, useBytes = TRUE), character(1))
    tibble::as_tibble(c(list(Sector = pretty_sector(sub$sector[1])), vals))
  })
  rows
}

# =============================================================================
# Narrativa automática
# -----------------------------------------------------------------------------
# Genera un párrafo basado en reglas, a partir de los valores del país. Si hay
# ANTHROPIC_API_KEY configurada, se usa la narrativa por LLM en su lugar
# (ver llm_narrative.R); esto es siempre el fallback.
# =============================================================================

build_narrative <- function(data, country_name) {
  v  <- function(qid) country_value(data, AGENCY_QUESTIONS[[qid]], country_name)
  ig <- data$agency$income_group[data$agency$country == country_name][1]
  ig_lbl <- if (is.na(ig)) "its peers" else paste0(ig, " peers")

  parts <- c()

  back  <- v("q7"); front <- v("q8")
  if (!is.na(back) || !is.na(front)) {
    parts <- c(parts, sprintf(
      "%s reports %s for back-end AI use and %s for citizen-facing services.",
      country_name,
      ifelse(is.na(back),  "no clear stage", tolower(back)),
      ifelse(is.na(front), "no clear stage", tolower(front))
    ))
  }

  strat <- v("q16"); guide <- v("q14")
  if (!is.na(strat) || !is.na(guide)) {
    parts <- c(parts, sprintf(
      "On governance, the national AI strategy is %s and generative-AI guidelines are %s.",
      ifelse(is.na(strat), "not reported", tolower(strat)),
      ifelse(is.na(guide), "not reported", tolower(guide))
    ))
  }

  unit <- v("q18"); budg <- v("q24")
  parts <- c(parts, sprintf(
    "An AI oversight body is %s, and a dedicated budget for AI projects is %s.",
    ifelse(is.na(unit), "not reported", tolower(unit)),
    ifelse(is.na(budg), "not reported", tolower(budg))
  ))

  bar <- make_text(data, AGENCY_QUESTIONS[["q28"]], country_name)$Response
  if (length(bar) > 0) {
    parts <- c(parts, paste0("The main reported barriers are: ", paste(bar, collapse = "; "), "."))
  }

  act <- make_text(data, AGENCY_QUESTIONS[["q34"]], country_name)$Response
  if (length(act) > 0) {
    parts <- c(parts, paste0("Stated priority actions: ", paste(act, collapse = "; "), "."))
  }

  parts <- c(parts, sprintf(
    "These results are benchmarked against %s; see the figures below for the indicator-by-indicator comparison.",
    ig_lbl
  ))

  paste(parts, collapse = " ")
}

section_narrative <- function(data, country_name, section_title, qids, fallback_extra = "") {
  txt <- NULL
  if (have_api_key()) {
    txt <- llm_section_narrative(data, country_name, section_title, qids)
  }
  if (is.null(txt) || !nzchar(txt)) {
    txt <- paste(build_narrative(data, country_name), fallback_extra)
  }
  txt
}

# =============================================================================
# Texto estático — disclaimer y presentación de la encuesta
# =============================================================================

add_disclaimer_page <- function(doc) {
  disc <- c(
    "This work is a product of the staff of The World Bank with external contributions. The findings, interpretations, and conclusions expressed in this work do not necessarily reflect the views of The World Bank, its Board of Executive Directors, or the governments they represent.",
    "The World Bank does not guarantee the accuracy, completeness, or currency of the data included in this work and does not assume responsibility for any errors, omissions, or discrepancies in the information, or liability with respect to the use of or failure to use the information, methods, processes, or conclusions set forth. The boundaries, colors, denominations, links/footnotes, and other information shown in this work do not imply any judgment on the part of The World Bank concerning the legal status of any territory or the endorsement or acceptance of such boundaries. The citation of works authored by others does not mean The World Bank endorses the views expressed by those authors or the content of their works.",
    "Nothing herein shall constitute or be construed or considered to be a limitation upon or waiver of the privileges and immunities of The World Bank, all of which are specifically reserved."
  )
  for (p in disc) doc <- body_add_par(doc, p, style = "Normal")
  doc <- body_add_fpar(doc, fpar(ftext(
    "Economy briefs are only being shared with economy counterparts involved in survey activities and World Bank staff. We do not intend to make them publicly available, and they should be seen as working documents rather than formal publications.",
    fp_text(bold = TRUE)
  )))
  doc <- body_add_break(doc)
  doc
}

# Portada simple: título de la encuesta, "Economy Brief", nombre del país en
# grande, e indicadores clave (income group, cuestionarios completados). No
# reproduce la ilustración del brief de Türkiye (ver nota al inicio del
# archivo); usa solo texto y color, así que sale igual con o sin conexión a
# internet y sin depender de ningún asset externo.
add_cover_page <- function(doc, country_name, hdr) {
  blank <- function(d, n = 1) { for (i in seq_len(n)) d <- body_add_par(d, "", style = "Normal"); d }

  stat_line <- function(label, value) {
    fpar(
      ftext(paste0(label, ": "), fp_text(font.size = 11, color = "#333333")),
      ftext(value, fp_text(bold = TRUE, font.size = 11, color = WB_NAVY))
    )
  }

  doc <- blank(doc, 3)
  doc <- body_add_fpar(doc, fpar(ftext("AI and Data for Better Governance Survey",
                                       fp_text(font.size = 13, color = "#555555"))))
  doc <- body_add_fpar(doc, fpar(ftext("Economy Brief",
                                       fp_text(bold = TRUE, font.size = 16, color = WB_BLUE))))
  doc <- blank(doc, 1)
  doc <- body_add_fpar(doc, fpar(ftext(country_name,
                                       fp_text(bold = TRUE, font.size = 44, color = WB_NAVY))))
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_fpar(doc, fpar(ftext(strrep("\u2500", 18),
                                       fp_text(color = WB_BLUE, font.size = 10))))
  doc <- blank(doc, 2)

  doc <- body_add_fpar(doc, stat_line("Income group", hdr$income_group))
  doc <- blank(doc, 1)
  doc <- body_add_fpar(doc, stat_line("Number of questionnaires submitted", as.character(hdr$n_total)))
  doc <- body_add_fpar(doc, fpar(ftext(
    sprintf("(%d agency, %d out of 6 manager, %d out of 6 system)",
            hdr$n_agency, hdr$n_managers, hdr$n_systems),
    fp_text(italic = TRUE, font.size = 9.5, color = "#666666")
  )))

  doc <- blank(doc, 14)  # empuja el bloque de pie hacia la parte baja de la hoja

  doc <- body_add_fpar(doc, fpar(ftext(strrep("\u2500", 45), fp_text(color = "#cccccc", font.size = 8))))
  doc <- body_add_fpar(doc, fpar(ftext("WORLD BANK GROUP",
                                       fp_text(bold = TRUE, font.size = 12, color = WB_NAVY))))
  doc <- body_add_break(doc)
  doc
}

add_survey_intro <- function(doc) {
  doc <- add_h1(doc, "The AI and Data for Better Governance Survey")
  doc <- add_paragraphs(doc, paste(
    "Artificial intelligence (AI) is rapidly enabling new possibilities to enhance governance\u2014from designing",
    "and evaluating public policy to strengthening monitoring and transparency. In such a fast-moving field, it",
    "can be challenging for governments to evaluate their AI adoption compared to their peers, see what",
    "opportunities they might be leaving on the table, and strategize responses to common barriers. The World",
    "Bank Group aims to help governments meet this challenge by leveraging its position to offer a global",
    "picture of AI and data analytics use in government, as part of the World Development Report 2026:",
    "Decoding AI for Development."
  ))
  doc <- body_add_par(doc, "The AI and Data for Better Governance Survey was designed to accomplish the following:", style = "Normal")
  doc <- body_add_par(doc, "\u2022 Measure the extent to which governments have already adopted AI", style = "Normal")
  doc <- body_add_par(doc, "\u2022 Evaluate governments\u2019 institutional readiness and capacity to adopt AI in the future", style = "Normal")
  doc <- body_add_par(doc, "\u2022 Document innovative approaches to applying AI and data analytics in public administration", style = "Normal")
  doc <- add_paragraphs(doc, paste(
    "The survey consisted of 13 questionnaires: one agency questionnaire focusing on AI adoption, strategy,",
    "infrastructure, and barriers from a whole-of-government perspective; six manager questionnaires focusing",
    "on ministries and agencies in six sectors (education, health care, public finance, procurement, taxation,",
    "and civil service); and six system questionnaires focusing on management information system (MIS)",
    "infrastructure, data readiness, and AI/machine learning capabilities in these same sectors."
  ))
  doc <- add_paragraphs(doc, paste(
    "This economy brief presents findings from the survey along these three margins to help you compare your",
    "economy to the rest of the world."
  ))
  doc <- add_paragraphs(doc, paste(
    "Our aim is to make these survey data useful by giving our collaborators comparative information to",
    "inspire learning about other governments\u2019 efforts. We hope that, as stakeholders in this exercise, you",
    "feel empowered to use these data as the foundation for global collaboration to identify common challenges",
    "and share effective solutions. To that end, we are happy to put you in touch with others in the community",
    "so peer learning can continue."
  ))
  doc
}

# =============================================================================
# FUNCIÓN PRINCIPAL
# =============================================================================

generate_brief <- function(data, country_name, out_file = NULL) {

  hdr <- country_header(data, country_name)
  if (is.null(out_file)) {
    out_file <- file.path(tempdir(),
      paste0("dfbg_", tolower(gsub("[^A-Za-z]+", "_", country_name)), "_brief.docx"))
  }

  fig_n <- 0
  tbl_n <- 0

  doc <- read_docx()

  # ----- Portada -----
  doc <- add_cover_page(doc, country_name, hdr)

  # ----- Disclaimer + intro de la encuesta -----
  doc <- add_disclaimer_page(doc)
  doc <- add_survey_intro(doc)

  # ===========================================================================
  # SECTION — "What Does AI Adoption Look Like in {economy}?"
  # ===========================================================================
  doc <- add_h1(doc, paste0("What Does AI Adoption Look Like in ", country_name, "?"))

  n_sectors_total <- length(SECTOR_LIST)
  doc <- add_paragraphs(doc, sprintf(
    paste0("In %s, the AI and Data for Better Governance Survey involved participation across ",
           "government agencies. In total, %d questionnaire%s completed: the agency questionnaire%s, ",
           "%d out of %d manager questionnaires, and %d out of %d system questionnaires (table 1)."),
    country_name, hdr$n_total, ifelse(hdr$n_total == 1, " was", "s were"),
    ifelse(hdr$n_agency == 1, "", " (not completed)"),
    hdr$n_managers, n_sectors_total, hdr$n_systems, n_sectors_total
  ))

  tbl_n <- tbl_n + 1
  doc <- add_caption(doc, "Table", tbl_n, "Manager and System Questionnaires, by Sector")
  doc <- body_add_flextable(doc, ft_questionnaire_grid(questionnaire_grid(data, country_name)))
  doc <- add_source_note(doc, "AI and DfBG Survey.",
                         "Check marks indicate completed survey instruments for this economy.")

  doc <- add_paragraphs(doc, paste0(
    "Drawing on data from the agency, manager, and system questionnaires, this section provides a ",
    "snapshot of ", country_name, "'s self-reported AI adoption, institutional readiness, and barriers."
  ))

  # --- Current AI Adoption in Government ------------------------------------
  doc <- add_h2(doc, "Current AI Adoption in Government")
  doc <- add_paragraphs(doc, section_narrative(
    data, country_name, "Current AI Adoption in Government",
    c("q7", "q8", "q9", "q11")
  ))

  fig_n <- fig_n + 1
  q9 <- AGENCY_QUESTIONS[["q9"]]
  gg <- make_plot(data, q9, country_name, scope = "compare")
  doc <- add_caption(doc, "Figure", fig_n, paste0("AI Use Cases (", hdr$income_group, " Economies)"))
  doc <- add_plot(doc, gg)
  doc <- add_source_note(doc, "AI and DfBG Survey, agency questionnaire, question 9.",
    "Bars show the share of agencies in the same income group reporting each AI use case.")

  q15 <- AGENCY_QUESTIONS[["q15"]]
  if (!is.null(q15)) {
    fig_n <- fig_n + 1
    gg <- make_plot(data, q15, country_name, scope = "compare")
    doc <- add_caption(doc, "Figure", fig_n, paste0("Topics Covered by AI Guidelines (", hdr$income_group, " Economies)"))
    doc <- add_plot(doc, gg)
    doc <- add_source_note(doc, "AI and DfBG Survey, agency questionnaire, question 15.",
      "Bars show the share of agencies in the same income group whose AI guidelines cover each topic.")
  }

  # --- Institutional Readiness and Capacity ---------------------------------
  doc <- add_h2(doc, "Institutional Readiness and Capacity for AI Adoption")
  doc <- add_paragraphs(doc, section_narrative(
    data, country_name, "Institutional Readiness and Capacity for AI Adoption",
    c("q16", "q17", "q18", "q20", "q23", "q24")
  ))

  tbl_n <- tbl_n + 1
  readiness_qids <- c("q16", "q18", "q20", "q23", "q24")
  doc <- add_caption(doc, "Table", tbl_n, "Institutional Readiness Indicators")
  doc <- body_add_flextable(doc, ft_indicator(indicator_table(data, readiness_qids, country_name)))
  doc <- add_source_note(doc, "AI and DfBG Survey, agency questionnaire.")

  # La tabla de entrenamiento tiene 8 indicadores (q25-q32); en una sola tabla
  # de 9 columnas se corta en el margen derecho de la página. La partimos en
  # dos: Data Analytics training (q25-28) y AI training (q29-32), y usamos
  # fit_to_width() para que cada una entre en el ancho de la página.
  make_training_ft <- function(qids) {
    sect_train <- tryCatch(sector_indicator_table(data, qids, country_name), error = function(e) NULL)
    if (is.null(sect_train) || nrow(sect_train) == 0) return(NULL)
    flextable(sect_train) |>
      bold(j = 1) |> fontsize(size = 8, part = "all") |>
      padding(padding = 3, part = "all") |>
      bg(part = "header", bg = WB_NAVY) |> color(part = "header", color = "white") |>
      border_outer(border = fp_border(color = "grey70")) |>
      border_inner(border = fp_border(color = "grey85")) |>
      align(align = "center", part = "all", j = 2:(length(qids) + 1)) |>
      valign(valign = "center", part = "all") |>
      flextable::fit_to_width(max_width = 6.5)
  }

  ft_da <- make_training_ft(c("q25", "q26", "q27", "q28"))
  if (!is.null(ft_da)) {
    tbl_n <- tbl_n + 1
    doc <- add_caption(doc, "Table", tbl_n, "Data Analytics Training and Capacity, by Sector")
    doc <- body_add_flextable(doc, ft_da)
    doc <- add_source_note(doc, "AI and DfBG Survey, manager questionnaire, questions 25\u201328.",
      "\u201cN/A\u201d indicates the sector reported no program for that training category.")
  }

  ft_ai <- make_training_ft(c("q29", "q30", "q31", "q32"))
  if (!is.null(ft_ai)) {
    tbl_n <- tbl_n + 1
    doc <- add_caption(doc, "Table", tbl_n, "AI Training and Capacity, by Sector")
    doc <- body_add_flextable(doc, ft_ai)
    doc <- add_source_note(doc, "AI and DfBG Survey, manager questionnaire, questions 29\u201332.",
      "\u201cN/A\u201d indicates the sector reported no program for that training category.")
  }

  # --- Barriers to AI Adoption -----------------------------------------------
  doc <- add_h2(doc, "Barriers to AI Adoption")
  doc <- add_paragraphs(doc, section_narrative(
    data, country_name, "Barriers to AI Adoption", c("q26", "q27", "q28", "q30")
  ))

  q26 <- AGENCY_QUESTIONS[["q26"]]
  if (!is.null(q26)) {
    tbl_n <- tbl_n + 1
    gg <- make_plot(data, q26, country_name, scope = "country")
    doc <- add_caption(doc, "Table", tbl_n, "Main Barriers to AI Adoption, Overall")
    doc <- add_plot(doc, gg, h = 4.6)
    doc <- add_source_note(doc, "AI and DfBG Survey, agency questionnaire.",
      "Colors reflect barrier severity from green (not a barrier) to red (major barrier).")
  }

  q20_m <- MANAGER_QUESTIONS[["q20"]]
  if (!is.null(q20_m)) {
    fig_n <- fig_n + 1
    gg <- tryCatch(plot_multi_sectors(data, q20_m, country_name, base = "manager"),
                   error = function(e) NULL)
    doc <- add_caption(doc, "Figure", fig_n, "Main Barriers to AI Adoption, by Sector")
    doc <- add_plot(doc, gg)
    doc <- add_source_note(doc, "AI and DfBG Survey, manager questionnaire, question 20.",
      "Each sector identified up to three main barriers to AI adoption.")
  }

  q18_s <- SYSTEMS_QUESTIONS[["q18"]]
  if (!is.null(q18_s)) {
    fig_n <- fig_n + 1
    gg <- tryCatch(plot_multi_sectors(data, q18_s, country_name, base = "systems"),
                   error = function(e) NULL)
    doc <- add_caption(doc, "Figure", fig_n, "Main Barriers to AI Adoption, by System")
    doc <- add_plot(doc, gg)
    doc <- add_source_note(doc, "AI and DfBG Survey, system questionnaire, question 18.",
      "Each system identified up to three main barriers limiting advanced analytics or AI.")
  }

  act <- make_text(data, AGENCY_QUESTIONS[["q34"]], country_name)$Response
  if (length(act) > 0) {
    doc <- body_add_par(doc,
      paste0("In response to these barriers, ", country_name, " reported the following key priority actions:"),
      style = "Normal")
    for (i in seq_along(act)) doc <- body_add_par(doc, paste0(i, ". ", act[i]), style = "Normal")
  }

  # ===========================================================================
  # SECTION — "What Does AI Adoption Look Like Globally?"  (NUEVO)
  # ===========================================================================
  doc <- add_h1(doc, "What Does AI Adoption Look Like Globally?")
  doc <- add_paragraphs(doc, paste(
    "To put your government's responses to the AI and Data for Better Governance Survey in perspective,",
    "this section draws on data from the agency questionnaire across ALL participating economies to give a",
    "global snapshot of AI adoption, institutional readiness, and barriers."
  ))

  q7 <- AGENCY_QUESTIONS[["q7"]]
  df7 <- global_single_by_income(data, q7)
  if (nrow(df7) > 0) {
    fig_n <- fig_n + 1
    gg <- chart_by_income(df7, "category", "income_group",
                          q7$title, "Back-end (internal) AI adoption stage, by income level")
    doc <- add_caption(doc, "Figure", fig_n, "Levels of AI Adoption for Internal Operations (Back-End), by Income Level")
    doc <- add_plot(doc, gg)
    doc <- add_source_note(doc, "AI and DfBG Survey, agency questionnaire, question 7.")
  }

  q12 <- AGENCY_QUESTIONS[["q12"]]
  df12 <- global_single_by_income(data, q12)
  if (nrow(df12) > 0) {
    fig_n <- fig_n + 1
    gg <- chart_by_income(df12, "category", "income_group",
                          q12$title, "Type of license for generative AI tools, by income level")
    doc <- add_caption(doc, "Figure", fig_n, "Licenses for Generative AI Tools, by Income Level")
    doc <- add_plot(doc, gg)
    doc <- add_source_note(doc, "AI and DfBG Survey, agency questionnaire, question 12.")
  }

  q18_a <- AGENCY_QUESTIONS[["q18"]]
  df18 <- global_single_by_income(data, q18_a)
  if (nrow(df18) > 0) {
    fig_n <- fig_n + 1
    gg <- chart_by_income(df18, "category", "income_group",
                          q18_a$title, "Type of body overseeing AI deployment, by income level")
    doc <- add_caption(doc, "Figure", fig_n, "Type of Body Overseeing AI Deployment and Governance in the Public Sector")
    doc <- add_plot(doc, gg)
    doc <- add_source_note(doc, "AI and DfBG Survey, agency questionnaire, question 18.")
  }

  # narrativa global con cifras reales computadas de los datos
  # OJO: global_pct_single() recodifica con q$recoder ANTES de matchear, asi
  # que hay que buscar contra las ETIQUETAS resultantes (Builder/Adapter/...),
  # no contra el texto crudo de la encuesta.
  pct_custom <- global_pct_single(data, q7, "^(Builder|Adapter)$")
  pct_adhoc  <- global_pct_single(data, q7, "^Basic$")
  global_lines <- c()
  if (!is.na(pct_custom) || !is.na(pct_adhoc)) {
    global_lines <- c(global_lines, sprintf(
      paste0("Across all participating economies, government AI adoption is advancing but uneven. ",
             "%s of surveyed governments report using customized or in-house AI tools for internal ",
             "operations, while %s report using AI only for ad hoc tasks such as search and summarization."),
      ifelse(is.na(pct_custom), "A share", paste0(pct_custom, "%")),
      ifelse(is.na(pct_adhoc), "another share", paste0(pct_adhoc, "%"))
    ))
  }
  if (length(global_lines) > 0) doc <- add_paragraphs(doc, paste(global_lines, collapse = " "))

  # ===========================================================================
  # SECTION — "What Is the Future of AI in Government?"  (NUEVO)
  # ===========================================================================
  doc <- add_h1(doc, "What Is the Future of AI in Government?")
  doc <- add_paragraphs(doc, paste(
    "Beyond the AI applications they have already implemented, different sectors of government see",
    "potential for AI to improve their performance. The figures below draw on the manager questionnaire",
    "(question 22) and aggregate responses across ALL participating economies, sector by sector."
  ))

  q22_m <- MANAGER_QUESTIONS[["q22"]]
  if (!is.null(q22_m)) {
    sector_pot <- tryCatch(sector_multi_global(data, q22_m, base = "manager"),
                           error = function(e) NULL)
    if (!is.null(sector_pot) && nrow(sector_pot) > 0) {
      for (sec in sort(unique(sector_pot$sector))) {
        sub <- dplyr::filter(sector_pot, sector == sec) |> dplyr::slice_max(pct, n = 8)
        sec_lbl <- pretty_sector(sec)
        fig_n <- fig_n + 1
        gg <- chart_sector_potential(sub, sec_lbl)
        doc <- add_caption(doc, "Figure", fig_n, paste0("AI Potential in ", sec_lbl))
        doc <- add_plot(doc, gg)
        doc <- add_source_note(doc, "AI and DfBG Survey, manager questionnaire, question 22.")
      }
    }
  }

  # ===========================================================================
  # POSTSCRIPT — What Is Next for AI and Data for Better Governance?
  # ===========================================================================
  doc <- body_add_break(doc)
  doc <- add_h1(doc, "Postscript: What Is Next for AI and Data for Better Governance?")
  doc <- add_paragraphs(doc, paste(
    "We are grateful for your involvement in this data collection effort to understand the use of AI",
    "tools by governments worldwide, and we hope that the data reported here are useful as you map a",
    "course in the rapidly changing AI landscape. Please watch for the publication of the World",
    "Development Report 2026: Decoding AI for Development later this year, which will include results",
    "and analysis of the global survey."
  ))
  doc <- add_paragraphs(doc, paste(
    "Surveys can only capture one moment in time, but the global landscape of AI in government is",
    "changing quickly. For this reason, the World Bank Group aims to continue collecting data on AI",
    "use in government. We welcome suggestions for how to improve the survey process."
  ))
  doc <- add_paragraphs(doc, paste(
    "We realize that, given the fast pace of change in this area, these results will be most useful if",
    "you are able to apply them right away to understand and respond to key problems. We are eager to",
    "use this survey as a foundation for collaboration and exchange of ideas and solutions among",
    "economies. Please reach out if we can help connect you to other members of this community to share",
    "ideas and learn from one another."
  ))

  # ----- pie -----
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_fpar(doc, fpar(ftext("Official Use Only",
                                       fp_text(font.size = 8, italic = TRUE, color = "grey50"))))

  print(doc, target = out_file)
  out_file
}

# =============================================================================
# Sección por familia (Systems o Managers) — usada en el Apéndice: para cada
# sector con respuesta, genera la tabla de indicadores + los gráficos de las
# preguntas disponibles.
# =============================================================================
add_family_section <- function(doc, data, country_name, family, base, title) {
  doc <- add_h2(doc, title)
  df  <- data[[base]]
  if (is.null(df)) {
    doc <- body_add_par(doc, paste0("No ", family, " data available."), style = "Normal")
    return(doc)
  }
  d_country <- dplyr::filter(df, country == country_name)
  if (nrow(d_country) == 0) {
    doc <- body_add_par(doc,
      paste0("No ", family, " questionnaire responses for this economy."),
      style = "Normal")
    return(doc)
  }

  sectors <- d_country |>
    dplyr::mutate(sector = stringr::str_remove(questionnaire, "^(Systems|Manager)_")) |>
    dplyr::pull(sector) |> unique() |> sort()

  doc <- body_add_par(doc,
    paste0("Sector-level responses available for: ", paste(pretty_sector(sectors), collapse = ", "), "."),
    style = "Normal")

  qlist <- available_questions(df, AGENCY_QUESTIONS)
  if (length(qlist) == 0) {
    doc <- body_add_par(doc, "No graphable questions found for this questionnaire.", style = "Normal")
    return(doc)
  }

  for (sec in sectors) {
    doc <- body_add_par(doc, paste0(family, " \u00b7 ", pretty_sector(sec)), style = "heading 3")
    sub <- d_country |>
      dplyr::mutate(sector = stringr::str_remove(questionnaire, "^(Systems|Manager)_")) |>
      dplyr::filter(sector == sec)
    peers_sector <- df |>
      dplyr::mutate(sector = stringr::str_remove(questionnaire, "^(Systems|Manager)_")) |>
      dplyr::filter(sector == sec)
    d2 <- data
    d2[[base]] <- peers_sector

    qkeep <- qlist[purrr::map_lgl(qlist, ~ isTRUE(.x$in_brief) && .x$type != "text")]
    if (length(qkeep) == 0) qkeep <- qlist[purrr::map_lgl(qlist, ~ .x$type != "text")]

    for (qid in names(qkeep)) {
      gg <- make_plot(d2, qkeep[[qid]], country_name, scope = "compare", base = base)
      doc <- add_plot(doc, gg)
      doc <- body_add_par(doc, "", style = "Normal")
    }
  }
  doc
}
