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
col_deaths    <- "#89B8E0"
col_doses     <- "#7BC5A0"
col_baseline  <- "#C5B8D9"
col_bl_line   <- "#9A89B5"
col_ax_deaths <- "#5A92BF"
col_ax_doses  <- "#4DA07A"

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
# 1. Aggregate daily deaths → months
# ═══════════════════════════════════════════════════════════════════════════════
deaths_monthly <- deaths_daily %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(monthly_deaths = sum(total_deaths, na.rm = TRUE), .groups = "drop")

# ═══════════════════════════════════════════════════════════════════════════════
# 2. Aggregate weekly doses → months
# ═══════════════════════════════════════════════════════════════════════════════
doses_monthly <- doses_weekly %>%
  mutate(month = floor_date(week, "month")) %>%
  group_by(month) %>%
  summarise(monthly_doses = sum(total_doses, na.rm = TRUE), .groups = "drop")

# ═══════════════════════════════════════════════════════════════════════════════
# 3. Build seasonal baseline from 2010-2020 (monthly)
#    Key: month number (1–12). For each month compute median, q10, q90.
# ═══════════════════════════════════════════════════════════════════════════════
baseline_years <- 2010:2020

baseline <- deaths_monthly %>%
  filter(year(month) %in% baseline_years) %>%
  mutate(mo = month(month)) %>%
  group_by(mo) %>%
  summarise(
    bl_median = median(monthly_deaths, na.rm = TRUE),
    bl_q10    = quantile(monthly_deaths, 0.10, na.rm = TRUE),
    bl_q90    = quantile(monthly_deaths, 0.90, na.rm = TRUE),
    .groups   = "drop"
  )

message("Baseline built from ", min(baseline_years), "-", max(baseline_years))

# ═══════════════════════════════════════════════════════════════════════════════
# 4. Filter 2021-2022 and join baseline
# ═══════════════════════════════════════════════════════════════════════════════
target_years <- c(2021, 2022)

yr_deaths <- deaths_monthly %>%
  filter(year(month) %in% target_years) %>%
  mutate(mo = month(month))

yr_doses <- doses_monthly %>%
  filter(year(month) %in% target_years)

yr_bl <- yr_deaths %>%
  select(month, mo) %>%
  left_join(baseline, by = "mo")

# ═══════════════════════════════════════════════════════════════════════════════
# 5. Scale factor: map doses onto deaths y-axis
# ═══════════════════════════════════════════════════════════════════════════════
max_deaths <- max(yr_deaths$monthly_deaths, na.rm = TRUE)
max_doses  <- max(yr_doses$monthly_doses, 1, na.rm = TRUE)
sk <- (max_deaths * 0.7) / max_doses

# ═══════════════════════════════════════════════════════════════════════════════
# 6. Build single plot for 2021 + 2022
# ═══════════════════════════════════════════════════════════════════════════════
bar_w  <- 12        # bar width in days (~half a month)
offset <- 7         # offset in days to separate paired columns

p <- ggplot() +

  # Baseline ribbon
  geom_ribbon(
    data = yr_bl,
    aes(x = month, ymin = bl_q10, ymax = bl_q90),
    fill  = col_baseline,
    alpha = 0.45
  ) +
  geom_line(
    data = yr_bl,
    aes(x = month, y = bl_median, linetype = "Référence 2010-2020 (médiane)"),
    colour    = col_bl_line,
    linewidth = 0.7
  ) +

  # RIGHT sub-column: monthly doses (green)
  geom_col(
    data  = yr_doses,
    aes(x = month + days(offset), y = monthly_doses * sk),
    fill  = col_doses,
    width = bar_w
  ) +

  # LEFT sub-column: monthly deaths (blue)
  geom_col(
    data  = yr_deaths,
    aes(x = month - days(offset), y = monthly_deaths,
        fill = "Décès mensuels 5 à 11 ans"),
    width = bar_w
  ) +

  # ── Scales ──
  scale_fill_manual(
    values = c("Décès mensuels 5 à 11 ans" = col_deaths),
    name   = NULL
  ) +
  scale_linetype_manual(
    values = c("Référence 2010-2020 (médiane)" = "solid"),
    name   = NULL
  ) +

  # Invisible point → legend entry for doses
  geom_point(
    data = data.frame(x = as.Date(NA_real_), y = NA_real_),
    aes(x = x, y = y, colour = "Doses mensuelles 5 à 11 ans"),
    size = 0, show.legend = TRUE
  ) +
  scale_colour_manual(
    values = c("Doses mensuelles 5 à 11 ans" = col_doses),
    name   = NULL,
    guide  = guide_legend(override.aes = list(shape = 15, size = 5, alpha = 1))
  ) +

  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    expand      = expansion(mult = 0.02)
  ) +
  scale_y_continuous(
    name   = "Décès mensuels 5 à 11 ans (INSEE)",
    labels = label_comma(big.mark = "\u202f"),
    expand = expansion(mult = c(0, 0.14)),
    sec.axis = sec_axis(
      transform = ~ . / sk,
      name      = "Doses mensuelles administrées 5 à 11 ans",
      labels    = label_comma(big.mark = "\u202f")
    )
  ) +

  theme_insee() +
  theme(
    axis.title.y.left  = element_text(colour = col_ax_deaths, margin = margin(r = 8)),
    axis.text.y.left   = element_text(colour = col_ax_deaths),
    axis.title.y.right = element_text(colour = col_ax_doses,  margin = margin(l = 8)),
    axis.text.y.right  = element_text(colour = col_ax_doses),
    axis.text.x        = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title    = "Décès INSEE 5 à 11 ans vs doses administrées — 2021-2022 (vue mensuelle)",
    subtitle = paste0(
      "Colonnes bleues = décès mensuels  ·  Colonnes vertes = doses mensuelles  ·  ",
      "Ruban mauve = intervalle P10–P90 (2010-2020)  ·  Ligne = médiane"
    ),
    x = NULL
  )

# ═══════════════════════════════════════════════════════════════════════════════
# 7. Display & save
# ═══════════════════════════════════════════════════════════════════════════════
p

ggsave("insee_deaths_vs_doses_5_to_11_monthly_2021_2022.png", p,
       width = 16, height = 7, dpi = 150, bg = "white")

message("Plot saved.")