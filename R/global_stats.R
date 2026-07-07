# =============================================================================
# global_stats.R
# -----------------------------------------------------------------------------
# Estadísticas GLOBALES (todos los países) y por SECTOR (todos los países,
# agregado por sector) para las secciones nuevas del country/economy brief:
#
#   - "What Does AI Adoption Look Like Globally?"   (país vs. TODO el mundo,
#      por income group)
#   - "What Is the Future of AI in Government?"     (potencial de IA por
#      sector, pregunta 22 de Managers, agregado global por sector)
#
# A diferencia de plots.R (que compara UN país contra su propio income group),
# estas funciones NO reciben country_name: agregan sobre todo el universo de
# respuestas disponibles. Se usan solo en build_brief.R.
# =============================================================================

# --- helpers de selección / etiquetas (mismo criterio que plot_multi) -------

gs_is_selected <- function(x) {
  xl <- tolower(trimws(as.character(x)))
  !is.na(x) & xl != "" & !xl %in% c("0", "no", "none", "not sure", "not_sure", "n/a", "na")
}

gs_prettify_code <- function(qid, cc) {
  s <- sub(paste0("^", qid, "_?"), "", cc)
  s <- gsub("_", " ", s)
  s <- trimws(s)
  if (s == "") return(cc)
  paste(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)), sep = "")
}

gs_clean_lab <- function(s, width = 40, max_lines = 2) {
  s <- gsub("\\s*\\(e\\.g\\.,?[^)]*\\)", "", s)
  s <- gsub("\\s*\\([^)]*\\)", "", s)
  s <- trimws(s)
  parts <- strwrap(s, width = width)
  if (length(parts) > max_lines) {
    parts <- parts[seq_len(max_lines)]
    last <- parts[max_lines]
    parts[max_lines] <- if (nchar(last) > width - 1)
      paste0(substr(last, 1, width - 1), "\u2026") else paste0(last, "\u2026")
  }
  paste(parts, collapse = "\n")
}

gs_derive_label <- function(df, qid, cc) {
  vals <- as.character(df[[cc]])
  vals <- vals[gs_is_selected(vals)]
  if (length(vals) == 0) return(gs_prettify_code(qid, cc))
  tb  <- sort(table(trimws(vals)), decreasing = TRUE)
  top <- names(tb)[1]
  if (tolower(top) %in% c("1", "yes", "true")) return(gs_prettify_code(qid, cc))
  top
}

gs_option_labels <- function(df, q, cols) {
  opt_lab <- q$options
  if (is.null(opt_lab)) {
    opt_lab <- setNames(vapply(cols, function(cc) gs_derive_label(df, q$id, cc), character(1)), cols)
  }
  setNames(vapply(opt_lab, gs_clean_lab, character(1)), names(opt_lab))
}

# =============================================================================
# Agregados
# =============================================================================

# % global (TODOS los países) que seleccionan cada opción de una pregunta MULTI
global_multi_pct <- function(data, q, base = "agency") {
  df   <- data[[base]]
  cols <- resolve_cols(q, df)
  cols <- cols[!grepl("(_900|_not_sure|_1)$", cols)]
  empty <- tibble::tibble(option = character(), pct = numeric())
  if (length(cols) == 0 || !"country" %in% names(df)) return(empty)

  opt_lab <- gs_option_labels(df, q, cols)

  long <- df |>
    dplyr::select(country, dplyr::all_of(cols)) |>
    tidyr::pivot_longer(dplyr::all_of(cols), names_to = "code", values_to = "resp") |>
    dplyr::mutate(option = ifelse(!is.na(opt_lab[code]), opt_lab[code], code),
                  sel = gs_is_selected(resp))

  long |>
    dplyr::group_by(country, option) |>
    dplyr::summarise(sel = any(sel), .groups = "drop") |>
    dplyr::group_by(option) |>
    dplyr::summarise(pct = 100 * mean(sel), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(pct))
}

# % de cada opción de una MULTI, agrupado por income_group (TODOS los países)
global_multi_by_income <- function(data, q, base = "agency") {
  df   <- data[[base]]
  cols <- resolve_cols(q, df)
  cols <- cols[!grepl("(_900|_not_sure|_1)$", cols)]
  empty <- tibble::tibble(income_group = character(), option = character(), pct = numeric())
  if (length(cols) == 0 || !"income_group" %in% names(df)) return(empty)

  opt_lab <- gs_option_labels(df, q, cols)

  long <- df |>
    dplyr::filter(!is.na(income_group)) |>
    dplyr::select(country, income_group, dplyr::all_of(cols)) |>
    tidyr::pivot_longer(dplyr::all_of(cols), names_to = "code", values_to = "resp") |>
    dplyr::mutate(option = ifelse(!is.na(opt_lab[code]), opt_lab[code], code),
                  sel = gs_is_selected(resp))

  long |>
    dplyr::group_by(income_group, country, option) |>
    dplyr::summarise(sel = any(sel), .groups = "drop") |>
    dplyr::group_by(income_group, option) |>
    dplyr::summarise(pct = 100 * mean(sel), .groups = "drop")
}

# % de cada categoría de una SINGLE, agrupado por income_group (TODOS los países)
global_single_by_income <- function(data, q, base = "agency") {
  df  <- data[[base]]
  col <- q$cols[1]
  empty <- tibble::tibble(income_group = character(), category = character(), pct = numeric())
  if (is.null(col) || !col %in% names(df) || !"income_group" %in% names(df)) return(empty)

  df |>
    dplyr::filter(!is.na(income_group)) |>
    dplyr::mutate(.cat = if (!is.null(q$recoder)) q$recoder(.data[[col]])
                         else recode_keep(.data[[col]], q$labels %||% setNames(q$levels, q$levels))) |>
    dplyr::filter(.cat %in% (q$levels %||% unique(.cat))) |>
    dplyr::group_by(income_group) |>
    dplyr::mutate(n_ig = dplyr::n()) |>
    dplyr::group_by(income_group, .cat) |>
    dplyr::summarise(pct = 100 * dplyr::n() / dplyr::first(n_ig), .groups = "drop") |>
    dplyr::rename(category = .cat)
}

# Atajo para frases narrativas ("62% of governments report..."): % global de
# countries seleccionando la opción cuyo texto matchea `option_match` (regex).
global_pct_selected <- function(data, q, option_match, base = "agency") {
  gm <- global_multi_pct(data, q, base = base)
  if (nrow(gm) == 0) return(NA_real_)
  hit <- gm[grepl(option_match, gm$option, ignore.case = TRUE), ]
  if (nrow(hit) == 0) return(NA_real_)
  round(hit$pct[1])
}

# % global de countries en una categoría dada de una SINGLE (para narrativa)
global_pct_single <- function(data, q, category_match, base = "agency") {
  df  <- data[[base]]
  col <- q$cols[1]
  if (is.null(col) || !col %in% names(df)) return(NA_real_)
  v <- if (!is.null(q$recoder)) q$recoder(df[[col]]) else recode_keep(df[[col]], q$labels %||% character(0))
  v <- v[!is.na(v) & v != ""]
  if (length(v) == 0) return(NA_real_)
  round(100 * mean(grepl(category_match, v, ignore.case = TRUE)))
}

# Potencial de IA por sector (Managers q22), agregando TODOS los países
sector_multi_global <- function(data, q, base = "manager") {
  df <- data[[base]]
  empty <- tibble::tibble(sector = character(), option = character(), pct = numeric())
  if (is.null(df) || !"questionnaire" %in% names(df)) return(empty)

  df <- df |> dplyr::mutate(sector = stringr::str_remove(questionnaire, "^(Systems|Manager)_"))
  cols <- resolve_cols(q, df)
  cols <- cols[!grepl("(_900|_not_sure|_1)$", cols)]
  if (length(cols) == 0) return(empty)

  opt_lab <- gs_option_labels(df, q, cols)

  long <- df |>
    dplyr::select(country, sector, dplyr::all_of(cols)) |>
    tidyr::pivot_longer(dplyr::all_of(cols), names_to = "code", values_to = "resp") |>
    dplyr::mutate(option = ifelse(!is.na(opt_lab[code]), opt_lab[code], code),
                  sel = gs_is_selected(resp))

  long |>
    dplyr::group_by(sector, country, option) |>
    dplyr::summarise(sel = any(sel), .groups = "drop") |>
    dplyr::group_by(sector, option) |>
    dplyr::summarise(pct = 100 * mean(sel), .groups = "drop") |>
    dplyr::arrange(sector, dplyr::desc(pct))
}

# =============================================================================
# Gráficos
# =============================================================================

INCOME_ORDER <- c("Low income", "Lower middle income", "Upper middle income", "High income")
INCOME_COLORS <- c(
  "Low income"          = "#BA7517",
  "Lower middle income" = "#D9A441",
  "Upper middle income" = "#1F77B4",
  "High income"         = "#0F6E56"
)

# Gráfico de barras agrupadas: opción/categoría x income group
chart_by_income <- function(df, value_col, group_col, title, subtitle = NULL) {
  if (nrow(df) == 0) return(empty_plot(title, "No data available"))
  df[[group_col]] <- factor(df[[group_col]],
                            levels = intersect(INCOME_ORDER, unique(df[[group_col]])))
  ggplot(df, aes(pct, forcats::fct_rev(factor(.data[[value_col]])), fill = .data[[group_col]])) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    scale_fill_manual(values = INCOME_COLORS, drop = TRUE) +
    scale_x_continuous(labels = function(x) paste0(round(x), "%"),
                       expand = expansion(mult = c(0, 0.10))) +
    labs(title = paste(strwrap(title, width = 70), collapse = "\n"),
         subtitle = subtitle, x = "Share of economies", y = NULL,
         caption = CAP_TXT) +
    theme_dfbg() +
    theme(axis.text.y = element_text(size = 9, lineheight = 0.95))
}

# Gráfico de barras horizontal simple para el potencial de IA de un sector
chart_sector_potential <- function(df, sector_name) {
  title <- paste("AI Potential in", sector_name)
  if (nrow(df) == 0) return(empty_plot(title, "No data available"))
  df <- df |>
    dplyr::arrange(pct) |>
    dplyr::mutate(option = factor(option, levels = option))
  ggplot(df, aes(pct, option)) +
    geom_col(fill = WB_BLUE, width = 0.7) +
    geom_text(aes(label = paste0(round(pct), "%")), hjust = -0.15, size = 3.4) +
    scale_x_continuous(labels = function(x) paste0(round(x), "%"),
                       expand = expansion(mult = c(0, 0.18)), limits = c(0, 100)) +
    labs(title = title,
         subtitle = "Share of manager respondents (all economies) identifying each area as high-potential",
         x = NULL, y = NULL, caption = CAP_TXT) +
    theme_dfbg()
}
