library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)

# ═══════════════════════════════════════════════════════════════════════════════
# Shared visual theme — professional, pastel, clean
# ═══════════════════════════════════════════════════════════════════════════════
theme_vaers <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      text               = element_text(colour = "#3C3C3C"),
      plot.title         = element_text(size = rel(1.15), face = "bold",
                                        colour = "#2B2B2B", margin = margin(b = 4)),
      plot.subtitle      = element_text(size = rel(0.88), colour = "#6B6B6B",
                                        margin = margin(b = 10)),
      plot.title.position = "plot",
      axis.title         = element_text(size = rel(0.9), colour = "#505050"),
      axis.text          = element_text(size = rel(0.82), colour = "#606060"),
      legend.position    = "bottom",
      legend.text        = element_text(size = rel(0.85), colour = "#505050"),
      legend.key.size    = unit(0.45, "cm"),
      panel.grid.major.y = element_line(colour = "#E8E8E8", linewidth = 0.35),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.background    = element_rect(fill = "#FAFAFA", colour = NA),
      panel.background   = element_rect(fill = "#FAFAFA", colour = NA),
      plot.margin        = margin(12, 14, 8, 10)
    )
}

# ── Pastel colour constants ──────────────────────────────────────────────────
col_non_grave <- "#89B8E0"   # soft steel-blue
col_grave     <- "#E0838C"   # muted rose
col_doses     <- "#7BC5A0"   # sage green
col_known     <- "#89B8E0"   # same family as Non grave
col_unknown   <- "#E8B87D"   # warm sand / apricot

# ═══════════════════════════════════════════════════════════════════════════════
# 0. Load data
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
    .default             = col_guess()
  ),
  quote = '"'
)

# Normalise age_group_name: blank / whitespace → NA
df_raw <- df_raw %>%
  mutate(age_group_name = na_if(trimws(age_group_name), ""))

# ═══════════════════════════════════════════════════════════════════════════════
# 1. Stacked column: known vs unknown age_group_name, by reception week
# ═══════════════════════════════════════════════════════════════════════════════
df_age_known <- df_raw %>%
  mutate(
    week      = floor_date(vaers_reception_date, "week", week_start = 1),
    age_known = if_else(is.na(age_group_name), "Âge inconnu", "Âge connu")
  ) %>%
  count(week, age_known)

p1 <- ggplot(df_age_known, aes(x = week, y = n, fill = age_known)) +
  geom_col(width = 6) +
  scale_fill_manual(
    values = c("Âge connu" = col_known, "Âge inconnu" = col_unknown),
    name   = NULL
  ) +
  scale_y_continuous(labels = label_comma(big.mark = "\u202f")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  theme_vaers() +
  labs(
    title   = "1. Rapports VAERS — âge connu vs inconnu (hebdomadaire)",
    x = NULL, y = "Nombre de rapports"
  )

p1

# ═══════════════════════════════════════════════════════════════════════════════
# 2. Count & % unknown, then exclude
# ═══════════════════════════════════════════════════════════════════════════════
total_reports   <- nrow(df_raw)
unknown_reports <- sum(is.na(df_raw$age_group_name))
pct_unknown     <- round(100 * unknown_reports / total_reports, 1)

message(
  "Total rapports      : ", total_reports,
  "\nÂge inconnu         : ", unknown_reports,
  " (", pct_unknown, "% du total)",
  "\nExclus pour analyse : âge inconnu"
)

df <- df_raw %>% filter(!is.na(age_group_name))

# ═══════════════════════════════════════════════════════════════════════════════
# 3. Tag "severe"
# ═══════════════════════════════════════════════════════════════════════════════
df <- df %>%
  mutate(
    severe = as.integer(
      hospitalized == 1 | life_threatening == 1 |
      patient_died == 1 | permanent_disability == 1
    ),
    severe_label = if_else(severe == 1, "Grave", "Non grave")
  )

# ═══════════════════════════════════════════════════════════════════════════════
# 4. Stacked column: weekly reports by age_group_name
# ═══════════════════════════════════════════════════════════════════════════════
# Order age groups sensibly
age_order <- df %>%
  distinct(age_group_name) %>%
  arrange(age_group_name) %>%
  pull(age_group_name)

df <- df %>%
  mutate(
    week           = floor_date(vaers_reception_date, "week", week_start = 1),
    age_group_name = factor(age_group_name, levels = age_order)
  )

weekly_by_age <- df %>%
  count(week, age_group_name)

age_palette <- colorRampPalette(
  c("#89B8E0", "#A8D5BA", "#E8B87D", "#E0838C", "#B8A9D4", "#C2A68C", "#D4B6CC", "#B0B0B0")
)(length(age_order))
names(age_palette) <- age_order

p4 <- ggplot(weekly_by_age, aes(x = week, y = n, fill = age_group_name)) +
  geom_col(width = 6) +
  scale_fill_manual(values = age_palette, name = "Groupe d'âge") +
  scale_y_continuous(labels = label_comma(big.mark = "\u202f")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  theme_vaers() +
  labs(
    title = "4. Rapports VAERS hebdomadaires par groupe d'âge (âge connu)",
    x = NULL, y = "Nombre de rapports"
  )

p4

# ═══════════════════════════════════════════════════════════════════════════════
# 5. Exclude >= 12 years old
# ═══════════════════════════════════════════════════════════════════════════════
under12_groups <- age_order[str_detect(age_order, regex("^(0|1|2|3|4|5|6|7|8|9|10|11)\\b|<\\s*12|infant|neonate|child.*[0-9]", ignore_case = TRUE))]

# Safer: keep only groups whose numeric upper bound is < 12
# Parse groups like "2-11", "0-1", "6-11" etc.
is_under12 <- function(g) {
  nums <- as.numeric(str_extract_all(g, "[0-9]+")[[1]])
  if (length(nums) == 0) return(FALSE)
  max(nums) < 12
}

under12_groups <- age_order[sapply(age_order, is_under12)]
message("Groups <12 retained: ", paste(under12_groups, collapse = ", "))

df_u12 <- df %>% filter(age_group_name %in% under12_groups)
message("Rapports <12 ans : ", nrow(df_u12))

# ═══════════════════════════════════════════════════════════════════════════════
# 6. <12 reports vs doses administered — side-by-side weekly columns
# ═══════════════════════════════════════════════════════════════════════════════
doses <- read_csv("data/weekly_doses_under_12.csv",
                  col_types = cols(week = col_date(), .default = col_double()))

weekly_u12_reports <- df_u12 %>%
  count(week, severe_label) %>%
  mutate(severe_label = factor(severe_label, levels = c("Non grave", "Grave")))

weekly_u12_doses <- doses %>%
  mutate(total_doses = dose1_hebdo + dose2_hebdo + dose3_hebdo) %>%
  select(week, total_doses)

# Align date ranges to the intersection
date_min <- max(min(weekly_u12_reports$week), min(weekly_u12_doses$week))
date_max <- min(max(weekly_u12_reports$week), max(weekly_u12_doses$week))

weekly_u12_reports <- weekly_u12_reports %>% filter(week >= date_min, week <= date_max)
weekly_u12_doses   <- weekly_u12_doses   %>% filter(week >= date_min, week <= date_max)

# Split date: first authorisation for 5-11 year olds
SPLIT_DATE <- as.Date("2021-12-21")

# ── Helper: build one period's plot ──────────────────────────────────────────
# Side-by-side columns per week:
#   LEFT column  = stacked reports (Non grave / Grave)  → left y-axis
#   RIGHT column = weekly doses administered             → right y-axis
# No cumulative line.

make_p6 <- function(rep_data, dose_data, period_label) {

  # Snap both to same Monday grid
  rep_data  <- rep_data  %>% mutate(week = floor_date(week,  "week", week_start = 1))
  dose_data <- dose_data %>% mutate(week = floor_date(week, "week", week_start = 1))

  # ── Scale factor: map weekly doses onto the reports y-axis ──
  max_rep  <- rep_data %>%
    group_by(week) %>%
    summarise(t = sum(n), .groups = "drop") %>%
    pull(t) %>% max(na.rm = TRUE)
  max_dose <- max(dose_data$total_doses, na.rm = TRUE)
  sk <- max_rep / max_dose          # doses × sk → reports-axis units

  # ── Column geometry ──
  bar_w   <- 2.5   # width of each sub-column (days)
  offset  <- 2     # half-gap between the two columns (days)

  # Axis-label colours (slightly darker than the bar fill for readability)
  col_ax_reports <- "#5A92BF"
  col_ax_doses   <- "#4DA07A"

  ggplot() +

    # RIGHT sub-column: weekly doses
    geom_col(
      data  = dose_data,
      aes(x = week + days(offset), y = total_doses * sk),
      fill  = col_doses,
      width = bar_w
    ) +

    # LEFT sub-column: stacked reports
    geom_col(
      data     = rep_data,
      aes(x = week - days(offset), y = n, fill = severe_label),
      width    = bar_w,
      position = position_stack()
    ) +

    scale_fill_manual(
      values = c("Non grave" = col_non_grave, "Grave" = col_grave),
      breaks = c("Non grave", "Grave"),
      name   = NULL
    ) +

    # Invisible point → legend entry for doses
    geom_point(
      data = data.frame(x = as.Date(NA_real_), y = NA_real_),
      aes(x = x, y = y, colour = "Doses hebdo. <12 ans"),
      size = 0, show.legend = TRUE
    ) +
    scale_colour_manual(
      values = c("Doses hebdo. <12 ans" = col_doses),
      name   = NULL,
      guide  = guide_legend(override.aes = list(
        shape = 15, size = 5, alpha = 1
      ))
    ) +

    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%d %b\n%Y",
      expand      = expansion(mult = 0.02)
    ) +
    scale_y_continuous(
      name   = "Rapports VAERS hebdomadaires",
      labels = label_comma(big.mark = "\u202f"),
      expand = expansion(mult = c(0, 0.14)),
      sec.axis = sec_axis(
        transform = ~ . / sk,
        name      = "Doses hebdomadaires administrées <12 ans",
        labels    = label_comma(big.mark = "\u202f")
      )
    ) +
    theme_vaers() +
    theme(
      axis.title.y.left  = element_text(colour = col_ax_reports, margin = margin(r = 8)),
      axis.text.y.left   = element_text(colour = col_ax_reports),
      axis.title.y.right = element_text(colour = col_ax_doses,   margin = margin(l = 8)),
      axis.text.y.right  = element_text(colour = col_ax_doses)
    ) +
    labs(
      title    = paste0("6. Rapports VAERS <12 ans vs doses administrées — ", period_label),
      subtitle = "Colonnes gauches = rapports empilés (Non grave / Grave)  ·  Colonnes droites = doses hebdomadaires",
      x = NULL
    )
}

# ── Period A: up to Dec 21 2021 (not included) ───────────────────────────────
rep_A  <- weekly_u12_reports %>% filter(week <  SPLIT_DATE)
dose_A <- weekly_u12_doses   %>% filter(week <  SPLIT_DATE)
p6a <- make_p6(rep_A, dose_A, "avant le 21 déc. 2021")

p6a

# ── Period B: from Dec 21 2021 onwards ───────────────────────────────────────
rep_B  <- weekly_u12_reports %>% filter(week >= SPLIT_DATE)
dose_B <- weekly_u12_doses   %>% filter(week >= SPLIT_DATE)
p6b <- make_p6(rep_B, dose_B, "à partir du 21 déc. 2021")

p6b

ggsave("vaers_plot6a_u12_before_dec21.png", p6a, width = 14, height = 7, dpi = 150, bg = "white")
ggsave("vaers_plot6b_u12_after_dec21.png",  p6b, width = 14, height = 7, dpi = 150, bg = "white")

message("All plots saved.")