library(tidyverse)
library(scales)
library(lubridate)

# ═══════════════════════════════════════════════════════════════════════════════
# Shared visual theme — professional, pastel, clean
# ═══════════════════════════════════════════════════════════════════════════════
theme_insee <- function(base_size = 11) {
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
col_deaths    <- "#89B8E0"   # soft steel-blue  (deaths columns)
col_doses     <- "#7BC5A0"   # sage green       (doses columns)
col_baseline  <- "#C5B8D9"   # lavender         (baseline ribbon fill)
col_bl_line   <- "#9A89B5"   # deeper lavender  (baseline median line)
col_ax_deaths <- "#5A92BF"   # left-axis labels
col_ax_doses  <- "#4DA07A"   # right-axis labels

# ═══════════════════════════════════════════════════════════════════════════════
# 0. Load data
# ═══════════════════════════════════════════════════════════════════════════════
deaths_daily <- read_csv(
  "data/insee/insee_daily_deaths_5_to_11.csv",
  col_types = cols(date = col_date(format = "%Y-%m-%d"),
                   total_deaths = col_double())
)

doses_weekly <- read_csv(
  "data/weekly_doses_5_to_11.csv",
  col_types = cols(week = col_date(), .default = col_double())
) %>%
  mutate(total_doses = dose1_hebdo + dose2_hebdo + dose3_hebdo) %>%
  select(week, total_doses)

# ═══════════════════════════════════════════════════════════════════════════════
# 1. Aggregate daily deaths → ISO weeks (Monday start)
# ═══════════════════════════════════════════════════════════════════════════════
deaths_weekly <- deaths_daily %>%
  mutate(week = floor_date(date, "week", week_start = 1)) %>%
  group_by(week) %>%
  summarise(weekly_deaths = sum(total_deaths, na.rm = TRUE), .groups = "drop")

# ═══════════════════════════════════════════════════════════════════════════════
# 2. Build seasonal baseline from 2010-2020
#    Key: iso_week (1–53). For each iso_week compute median, q10, q90.
# ═══════════════════════════════════════════════════════════════════════════════
baseline_years <- 2010:2020

baseline_raw <- deaths_weekly %>%
  filter(year(week) %in% baseline_years) %>%
  mutate(iso_wk = isoweek(week))

baseline <- baseline_raw %>%
  group_by(iso_wk) %>%
  summarise(
    bl_median = median(weekly_deaths, na.rm = TRUE),
    bl_q10    = quantile(weekly_deaths, 0.10, na.rm = TRUE),
    bl_q90    = quantile(weekly_deaths, 0.90, na.rm = TRUE),
    .groups   = "drop"
  )

message("Baseline built from ", min(baseline_years), "-", max(baseline_years),
        " (", n_distinct(baseline_raw$week), " weeks)")

# ═══════════════════════════════════════════════════════════════════════════════
# 3. Helper: build one year's plot
# ═══════════════════════════════════════════════════════════════════════════════
make_plot <- function(target_year) {

  # ── Deaths for the target year ──
  yr_deaths <- deaths_weekly %>%
    filter(year(week) == target_year) %>%
    mutate(iso_wk = isoweek(week))

  # ── Doses for the target year ──
  yr_doses <- doses_weekly %>%
    mutate(week = floor_date(week, "week", week_start = 1)) %>%
    filter(year(week) == target_year)

  # ── Join baseline onto the year's week grid ──
  yr_bl <- yr_deaths %>%
    select(week, iso_wk) %>%
    left_join(baseline, by = "iso_wk")

  # ── Scale factor: map doses onto deaths y-axis ──
  max_deaths <- max(yr_deaths$weekly_deaths, na.rm = TRUE)
  max_doses  <- max(yr_doses$total_doses, 1, na.rm = TRUE)
  # Give doses ~70% of the vertical span so both series breathe
  sk <- (max_deaths * 0.7) / max_doses

  # ── Column geometry ──
  bar_w  <- 2.5
  offset <- 2

  ggplot() +

    # Baseline ribbon (behind everything)
    geom_ribbon(
      data = yr_bl,
      aes(x = week, ymin = bl_q10, ymax = bl_q90),
      fill  = col_baseline,
      alpha = 0.45
    ) +
    geom_line(
      data = yr_bl,
      aes(x = week, y = bl_median, linetype = "Référence 2010-2020 (médiane)"),
      colour    = col_bl_line,
      linewidth = 0.7
    ) +

    # RIGHT sub-column: weekly doses (green)
    geom_col(
      data  = yr_doses,
      aes(x = week + days(offset), y = total_doses * sk),
      fill  = col_doses,
      width = bar_w
    ) +

    # LEFT sub-column: weekly deaths (blue)
    geom_col(
      data  = yr_deaths,
      aes(x = week - days(offset), y = weekly_deaths, fill = "Décès hebdo. 5 à 11 ans"),
      width = bar_w
    ) +

    # ── Scales ──
    scale_fill_manual(
      values = c("Décès hebdo. 5 à 11 ans" = col_deaths),
      name   = NULL
    ) +
    scale_linetype_manual(
      values = c("Référence 2010-2020 (médiane)" = "solid"),
      name   = NULL
    ) +

    # Invisible point → legend entry for doses
    geom_point(
      data = data.frame(x = as.Date(NA_real_), y = NA_real_),
      aes(x = x, y = y, colour = "Doses hebdo. 5 à 11 ans"),
      size = 0, show.legend = TRUE
    ) +
    scale_colour_manual(
      values = c("Doses hebdo. 5 à 11 ans" = col_doses),
      name   = NULL,
      guide  = guide_legend(override.aes = list(shape = 15, size = 5, alpha = 1))
    ) +

    # Invisible rect → legend entry for baseline ribbon
    geom_rect(
      data = data.frame(xmin = as.Date(NA_real_), xmax = as.Date(NA_real_),
                        ymin = NA_real_, ymax = NA_real_),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
          fill2 = "Intervalle P10-P90 (2010-2020)"),
      stat = "identity", show.legend = FALSE
    ) +
    # We add a manual annotation in the subtitle instead for the ribbon

    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%d %b",
      expand      = expansion(mult = 0.02)
    ) +
    scale_y_continuous(
      name   = "Décès hebdomadaires 5 à 11 ans (INSEE)",
      labels = label_comma(big.mark = "\u202f"),
      expand = expansion(mult = c(0, 0.14)),
      sec.axis = sec_axis(
        transform = ~ . / sk,
        name      = "Doses hebdomadaires administrées 5 à 11 ans",
        labels    = label_comma(big.mark = "\u202f")
      )
    ) +

    theme_insee() +
    theme(
      axis.title.y.left  = element_text(colour = col_ax_deaths, margin = margin(r = 8)),
      axis.text.y.left   = element_text(colour = col_ax_deaths),
      axis.title.y.right = element_text(colour = col_ax_doses,  margin = margin(l = 8)),
      axis.text.y.right  = element_text(colour = col_ax_doses)
    ) +
    labs(
      title    = paste0("Décès INSEE 5 à 11 ans vs doses administrées — ", target_year),
      subtitle = paste0(
        "Colonnes bleues = décès hebdo.  ·  Colonnes vertes = doses hebdo.  ·  ",
        "Ruban mauve = intervalle P10–P90 (2010-2020)  ·  Ligne = médiane"
      ),
      x = NULL
    )
}

# ═══════════════════════════════════════════════════════════════════════════════
# 4. Generate & save plots
# ═══════════════════════════════════════════════════════════════════════════════
p2021 <- make_plot(2021)
p2022 <- make_plot(2022)

p2021
p2022

ggsave("insee_deaths_vs_doses_5_to_11_2021.png", p2021,
       width = 14, height = 7, dpi = 150, bg = "white")
ggsave("insee_deaths_vs_doses_5_to_11_2022.png", p2022,
       width = 14, height = 7, dpi = 150, bg = "white")

message("Plots saved.")
