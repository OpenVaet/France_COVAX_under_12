library(tidyverse)
library(lubridate)
library(htmltools)
library(scales)
library(jsonlite)

# ═══════════════════════════════════════════════════════════════════════════════
# 0. Load & prepare data (same pipeline as main analysis)
# ═══════════════════════════════════════════════════════════════════════════════
df_raw <- read_delim(
  "data/covid_vax_reports.csv",
  delim = ";",
  col_types = cols(
    vaers_id             = col_character(),
    age_group_name       = col_character(),
    comp_date            = col_character(),
    deceased_date        = col_character(),
    hospitalized         = col_double(),
    life_threatening     = col_double(),
    onset_date           = col_character(),
    patient_died         = col_double(),
    permanent_disability = col_double(),
    vaers_reception_date = col_date(format = "%Y-%m-%d"),
    vax_date             = col_character(),
    symptom_text         = col_character(),
    symptoms_listed      = col_character(),
    vaccines_listed      = col_character(),
    sex_name             = col_character(),
    .default             = col_guess()
  ),
  quote = '"'
)

df_raw <- df_raw %>%
  mutate(age_group_name = na_if(trimws(age_group_name), ""))

# Keep known-age only
df <- df_raw %>% filter(!is.na(age_group_name))

# Tag severe
df <- df %>%
  mutate(
    severe = as.integer(
      hospitalized == 1 | life_threatening == 1 |
      patient_died == 1 | permanent_disability == 1
    ),
    severe_label = if_else(severe == 1, "Grave", "Non grave")
  )

# Identify <12 groups
age_order <- sort(unique(df$age_group_name))
is_under12 <- function(g) {
  nums <- as.numeric(str_extract_all(g, "[0-9]+")[[1]])
  if (length(nums) == 0) return(FALSE)
  max(nums) < 12
}
under12_groups <- age_order[sapply(age_order, is_under12)]

# Final dataset: severe + <12
df_severe <- df %>%
  filter(age_group_name %in% under12_groups, severe == 1) %>%
  arrange(desc(vaers_reception_date), vaers_id)

# ═══════════════════════════════════════════════════════════════════════════════
# 1. Compute summary stats for the header
# ═══════════════════════════════════════════════════════════════════════════════
n_total       <- nrow(df_severe)
n_died        <- sum(df_severe$patient_died == 1, na.rm = TRUE)
n_hosp        <- sum(df_severe$hospitalized == 1, na.rm = TRUE)
n_life_threat <- sum(df_severe$life_threatening == 1, na.rm = TRUE)
n_perm_disab  <- sum(df_severe$permanent_disability == 1, na.rm = TRUE)
date_range    <- paste(
  format(min(df_severe$vaers_reception_date, na.rm = TRUE), "%d/%m/%Y"),
  "\u2013",
  format(max(df_severe$vaers_reception_date, na.rm = TRUE), "%d/%m/%Y")
)
age_groups_str <- paste(under12_groups, collapse = ", ")

fmt <- function(x) format(x, big.mark = "\u202f")

# ═══════════════════════════════════════════════════════════════════════════════
# 2. Parse JSON columns into HTML fragments
# ═══════════════════════════════════════════════════════════════════════════════
flag_icon <- function(val) {
  ifelse(val == 1, "&#10005;", "")
}

# Parse symptoms_listed JSON -> pill tags
parse_symptoms <- function(json_str) {
  if (is.na(json_str) || trimws(json_str) == "") return("")
  tryCatch({
    items <- fromJSON(json_str)
    if (length(items) == 0) return("")
    paste0(
      sapply(items, function(s) paste0('<span class="pill pill-symptom">', htmlEscape(s), '</span>')),
      collapse = " "
    )
  }, error = function(e) htmlEscape(json_str))
}

# Parse vaccines_listed JSON -> pill tags with dose info
parse_vaccines <- function(json_str) {
  if (is.na(json_str) || trimws(json_str) == "") return("")
  tryCatch({
    vax_list <- fromJSON(json_str)
    if (is.data.frame(vax_list) && nrow(vax_list) == 0) return("")
    if (is.data.frame(vax_list)) {
      paste0(
        apply(vax_list, 1, function(row) {
          name <- row[["drug_short_name"]]
          if (is.null(name) || is.na(name)) name <- row[["vax_name"]]
          if (is.null(name) || is.na(name)) name <- "?"
          dose <- row[["dose"]]
          dose_str <- if (!is.null(dose) && !is.na(dose) && dose != "") paste0(" (D", dose, ")") else ""
          paste0('<span class="pill pill-vax">', htmlEscape(paste0(name, dose_str)), '</span>')
        }),
        collapse = " "
      )
    } else {
      htmlEscape(json_str)
    }
  }, error = function(e) htmlEscape(json_str))
}

symptoms_html  <- sapply(df_severe$symptoms_listed, parse_symptoms, USE.NAMES = FALSE)
vaccines_html  <- sapply(df_severe$vaccines_listed, parse_vaccines, USE.NAMES = FALSE)

# ═══════════════════════════════════════════════════════════════════════════════
# 3. Generate HTML table rows
# ═══════════════════════════════════════════════════════════════════════════════
n_main_cols <- 13  # number of columns in the main row

make_row <- function(i) {
  r <- df_severe[i, ]
  row_class <- if (r$patient_died == 1) "row-deceased" else ""

  # Symptom text (for the expandable detail row)
  stxt <- if (!is.na(r$symptom_text) && trimws(r$symptom_text) != "") {
    htmlEscape(r$symptom_text)
  } else {
    '<span class="text-muted">&mdash;</span>'
  }

  # Main row
  main_cells <- paste0(
    '<td>', htmlEscape(r$vaers_id), '</td>',
    '<td>', htmlEscape(if_else(is.na(r$sex_name), "", r$sex_name)), '</td>',
    '<td>', htmlEscape(if_else(is.na(r$age_group_name), "", r$age_group_name)), '</td>',
    '<td>', format(r$vaers_reception_date, "%d/%m/%Y"), '</td>',
    '<td>', htmlEscape(if_else(is.na(r$vax_date), "", r$vax_date)), '</td>',
    '<td>', htmlEscape(if_else(is.na(r$onset_date), "", r$onset_date)), '</td>',
    '<td class="flag-cell">', flag_icon(r$hospitalized), '</td>',
    '<td class="flag-cell">', flag_icon(r$life_threatening), '</td>',
    '<td class="flag-cell">', flag_icon(r$patient_died), '</td>',
    '<td>', htmlEscape(if_else(is.na(r$deceased_date), "", r$deceased_date)), '</td>',
    '<td class="flag-cell">', flag_icon(r$permanent_disability), '</td>',
    '<td class="cell-pills">', vaccines_html[i], '</td>',
    '<td class="cell-pills">', symptoms_html[i], '</td>'
  )

  # Detail row (symptom_text), hidden by default
  detail_row <- paste0(
    '<tr class="detail-row ', row_class, '" style="display:none;">',
    '<td colspan="', n_main_cols, '">',
    '<div class="detail-content">',
    '<span class="detail-label">Symptom narrative</span>',
    '<div class="detail-text">', stxt, '</div>',
    '</div></td></tr>'
  )

  paste0(
    '<tr class="main-row ', row_class, '" onclick="toggleDetail(this)">',
    main_cells,
    '</tr>\n',
    detail_row
  )
}

table_rows <- paste(sapply(seq_len(nrow(df_severe)), make_row), collapse = "\n")

# ═══════════════════════════════════════════════════════════════════════════════
# 4. Assemble HTML
# ═══════════════════════════════════════════════════════════════════════════════
html_out <- paste0('<!DOCTYPE html>
<html lang="fr">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Rapports VAERS graves &mdash; Enfants &lt;12 ans</title>
<style>
  @import url("https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@300;400;500;600&family=IBM+Plex+Mono:wght@400;500&display=swap");

  :root {
    --bg:          #F7F7F5;
    --surface:     #FFFFFF;
    --border:      #E2E0DC;
    --border-lt:   #EDEBE8;
    --text:        #2C2C2C;
    --text-muted:  #7A7872;
    --accent:      #5A7FA0;
    --accent-bg:   #EDF2F7;
    --rose:        #C26570;
    --rose-bg:     #FDF0F0;
    --rose-hover:  #FAE4E4;
    --amber:       #B08840;
    --amber-bg:    #FFF8EC;
    --sage:        #5E9A78;
    --sage-bg:     #F0F7F3;
    --slate:       #6B7C8A;
    --slate-bg:    #F1F4F6;
  }

  * { margin: 0; padding: 0; box-sizing: border-box; }

  body {
    font-family: "IBM Plex Sans", -apple-system, sans-serif;
    font-weight: 400;
    font-size: 13.5px;
    line-height: 1.55;
    color: var(--text);
    background: var(--bg);
    padding: 32px 40px 60px;
  }

  /* ── Header ── */
  .report-header {
    max-width: 1600px;
    margin: 0 auto 28px;
  }
  .report-header h1 {
    font-size: 22px;
    font-weight: 600;
    letter-spacing: -0.02em;
    color: var(--text);
    margin-bottom: 4px;
  }
  .report-header .subtitle {
    font-size: 13px;
    color: var(--text-muted);
    font-weight: 300;
  }

  /* ── KPI strip ── */
  .kpi-strip {
    display: flex;
    gap: 14px;
    max-width: 1600px;
    margin: 0 auto 28px;
    flex-wrap: wrap;
  }
  .kpi {
    flex: 1 1 140px;
    background: var(--surface);
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 16px 20px;
    min-width: 150px;
  }
  .kpi .value {
    font-family: "IBM Plex Mono", monospace;
    font-size: 26px;
    font-weight: 500;
    line-height: 1.15;
  }
  .kpi .label {
    font-size: 11.5px;
    font-weight: 500;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    color: var(--text-muted);
    margin-top: 4px;
  }
  .kpi.rose   { border-left: 3px solid var(--rose);  background: var(--rose-bg); }
  .kpi.rose .value   { color: var(--rose); }
  .kpi.amber  { border-left: 3px solid var(--amber); background: var(--amber-bg); }
  .kpi.amber .value  { color: var(--amber); }
  .kpi.accent { border-left: 3px solid var(--accent); background: var(--accent-bg); }
  .kpi.accent .value { color: var(--accent); }
  .kpi.sage   { border-left: 3px solid var(--sage);  background: var(--sage-bg); }
  .kpi.sage .value   { color: var(--sage); }
  .kpi.slate  { border-left: 3px solid var(--slate); background: var(--slate-bg); }
  .kpi.slate .value  { color: var(--slate); }

  /* ── Hint bar ── */
  .hint {
    max-width: 1600px;
    margin: 0 auto 10px;
    font-size: 11.5px;
    color: var(--text-muted);
    font-weight: 300;
    font-style: italic;
  }

  /* ── Table wrapper ── */
  .table-wrap {
    max-width: 1600px;
    margin: 0 auto;
    background: var(--surface);
    border: 1px solid var(--border);
    border-radius: 8px;
    overflow-x: auto;
  }
  table {
    width: 100%;
    border-collapse: collapse;
    font-size: 12.5px;
  }
  thead {
    position: sticky;
    top: 0;
    z-index: 2;
  }
  th {
    background: var(--bg);
    font-weight: 600;
    font-size: 10.5px;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    color: var(--text-muted);
    padding: 11px 12px;
    text-align: left;
    border-bottom: 2px solid var(--border);
    white-space: nowrap;
  }
  td {
    padding: 8px 12px;
    border-bottom: 1px solid var(--border-lt);
    vertical-align: top;
  }

  /* ── Main rows ── */
  .main-row { cursor: pointer; }
  .main-row:hover { background: #FAFAF8; }
  .main-row.row-deceased { background: var(--rose-bg); }
  .main-row.row-deceased:hover { background: var(--rose-hover); }

  /* ── Detail (expandable) rows ── */
  .detail-row td {
    padding: 0 12px 12px 12px;
    border-bottom: 2px solid var(--border-lt);
    background: #FCFCFB;
  }
  .detail-row.row-deceased td { background: #FEF6F6; }
  .detail-content {
    padding: 10px 14px;
    border-radius: 6px;
    background: var(--bg);
    border: 1px solid var(--border-lt);
  }
  .detail-label {
    display: block;
    font-size: 10px;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    color: var(--text-muted);
    margin-bottom: 5px;
  }
  .detail-text {
    font-size: 12px;
    line-height: 1.65;
    color: #3C3C3C;
    white-space: pre-wrap;
    word-break: break-word;
  }

  /* ── Flag cells ── */
  .flag-cell {
    text-align: center;
    font-size: 13px;
    color: var(--rose);
    font-weight: 600;
  }

  /* ── Pill tags ── */
  .cell-pills {
    white-space: normal;
    max-width: 300px;
    line-height: 1.8;
  }
  .pill {
    display: inline-block;
    font-size: 10.5px;
    font-weight: 500;
    padding: 2px 8px;
    border-radius: 4px;
    margin: 1px 2px;
    white-space: nowrap;
  }
  .pill-symptom {
    background: var(--amber-bg);
    color: #8A6A2E;
    border: 1px solid #E8D9B8;
  }
  .pill-vax {
    background: var(--accent-bg);
    color: #3E6585;
    border: 1px solid #C6D6E8;
  }

  .text-muted { color: var(--text-muted); }

  /* ── Footer ── */
  .report-footer {
    max-width: 1600px;
    margin: 20px auto 0;
    font-size: 11px;
    color: var(--text-muted);
    font-weight: 300;
  }
</style>
</head>
<body>

<div class="report-header">
  <h1>Rapports VAERS graves &mdash; Enfants &lt;12 ans</h1>
  <div class="subtitle">Groupes d&rsquo;&acirc;ge retenus : ', htmlEscape(age_groups_str),
  '&ensp;&middot;&ensp;P&eacute;riode : ', date_range, '</div>
</div>

<div class="kpi-strip">
  <div class="kpi accent">
    <div class="value">', fmt(n_total), '</div>
    <div class="label">Rapports graves</div>
  </div>
  <div class="kpi rose">
    <div class="value">', fmt(n_died), '</div>
    <div class="label">D&eacute;c&egrave;s</div>
  </div>
  <div class="kpi amber">
    <div class="value">', fmt(n_hosp), '</div>
    <div class="label">Hospitalisations</div>
  </div>
  <div class="kpi sage">
    <div class="value">', fmt(n_life_threat), '</div>
    <div class="label">Pronostic vital engag&eacute;</div>
  </div>
  <div class="kpi slate">
    <div class="value">', fmt(n_perm_disab), '</div>
    <div class="label">Handicap permanent</div>
  </div>
</div>

<div class="hint">Cliquer sur une ligne pour afficher le r&eacute;cit des sympt&ocirc;mes (symptom narrative).</div>

<div class="table-wrap">
<table>
<thead>
<tr>
  <th>VAERS ID</th>
  <th>Sexe</th>
  <th>Groupe d&rsquo;&acirc;ge</th>
  <th>R&eacute;ception</th>
  <th>Vaccination</th>
  <th>D&eacute;but sympt.</th>
  <th>Hosp.</th>
  <th>Pronostic</th>
  <th>D&eacute;c&egrave;s</th>
  <th>Date d&eacute;c&egrave;s</th>
  <th>Handicap</th>
  <th>Vaccins</th>
  <th>Sympt&ocirc;mes</th>
</tr>
</thead>
<tbody>
', table_rows, '
</tbody>
</table>
</div>

<div class="report-footer">
  G&eacute;n&eacute;r&eacute; le ', format(Sys.time(), "%d/%m/%Y &agrave; %H:%M"),
  ' &mdash; Source : VAERS (CDC/FDA) &mdash; Crit&egrave;res de gravit&eacute; : hospitalisation, pronostic vital engag&eacute;, d&eacute;c&egrave;s, handicap permanent.
</div>

<script>
function toggleDetail(mainRow) {
  var detail = mainRow.nextElementSibling;
  if (detail && detail.classList.contains("detail-row")) {
    detail.style.display = detail.style.display === "none" ? "table-row" : "none";
  }
}
</script>

</body>
</html>')

# ═══════════════════════════════════════════════════════════════════════════════
# 5. Write to disk
# ═══════════════════════════════════════════════════════════════════════════════
writeLines(html_out, "vaers_severe_under12_report.html", useBytes = TRUE)
message("HTML report written: vaers_severe_under12_report.html")
message("Severe reports <12 ans: ", n_total,
        " (", n_died, " d\u00e9c\u00e8s, ", n_hosp, " hospitalisations, ",
        n_life_threat, " pronostic vital, ", n_perm_disab, " handicap permanent)")