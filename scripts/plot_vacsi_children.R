library(tidyverse)
library(scales)
library(lubridate)

# ── 1. Load data ──────────────────────────────────────────────────────────────
df_raw <- read_delim(
  "data/vacsi-s-a-fra-2023-07-13-15h51.csv",
  delim = ";",
  col_types = cols(
    jour        = col_date(format = "%Y-%m-%d"),
    clage_vacsi = col_character(),
    .default    = col_double()
  )
)

message("Age groups found: ", paste(sort(unique(df_raw$clage_vacsi)), collapse = ", "))

# ── 2. Filter age groups 04 & 11, hard-stop Dec 31 2022 ──────────────────────
STOP_DATE <- as.Date("2022-12-31")

df <- df_raw %>%
  filter(clage_vacsi %in% c("11"),
         jour <= STOP_DATE) %>%
  mutate(week = floor_date(jour, "week", week_start = 1))

message("Rows after filter: ", nrow(df), "  |  dates: ", min(df$jour), " -> ", max(df$jour))

# ── 3. Weekly aggregation — both age groups merged ───────────────────────────
weekly <- df %>%
  group_by(week) %>%
  summarise(
    dose1 = sum(n_dose1_h   + n_dose1_f,   na.rm = TRUE),
    dose2 = sum(n_complet_h + n_complet_f, na.rm = TRUE),
    dose3 = sum(n_rappel_h  + n_rappel_f,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(week <= STOP_DATE)

message("max dose1/week: ", max(weekly$dose1))

# ── 4. Cumulative Dose-1 & Dose-2 ────────────────────────────────────────────
weekly_cum <- weekly %>%
  arrange(week) %>%
  mutate(
    cum_dose1 = cumsum(dose1),
    cum_dose2 = cumsum(dose2)
  )

# ── 5. Cumulative totals at event dates ───────────────────────────────────────
event_dates <- as.Date(c("2021-12-21", "2022-12-15"))

get_cum <- function(col, d) {
  weekly_cum %>%
    filter(week <= floor_date(d, "week", week_start = 1)) %>%
    pull({{ col }}) %>% max()
}

cum1_at_event <- map_dbl(event_dates, ~ get_cum(cum_dose1, .x))
cum2_at_event <- map_dbl(event_dates, ~ get_cum(cum_dose2, .x))

events <- tibble(
  date      = event_dates,
  cum1_val  = cum1_at_event,
  cum2_val  = cum2_at_event,
  label_top = c(
    "21 déc. 2021\nAutorisation Comirnaty\n5-11 ans",
    "15 déc. 2022\nExtension 0-4 ans\n(risque élevé)"
  )
)

message("Cum dose1 at events: ", paste(round(cum1_at_event), collapse = " / "))
message("Cum dose2 at events: ", paste(round(cum2_at_event), collapse = " / "))

# ── 6. Dual-axis scale factor ─────────────────────────────────────────────────
max_bar <- max(weekly$dose1, na.rm = TRUE)
max_cum  <- max(weekly_cum$cum_dose1, na.rm = TRUE)
scale_k  <- max_bar / max_cum

# ── 7. Plot ───────────────────────────────────────────────────────────────────
dose_colours <- c(dose1 = "#4E9AF1", dose2 = "#F4A742", dose3 = "#6DBF6A")
dose_labels  <- c(dose1 = "Dose 1", dose2 = "Dose 2", dose3 = "Dose 3")

p <- ggplot(weekly, aes(x = week)) +

  # Overlapping bars — widest (dose1) behind, narrowest (dose3) in front
  geom_col(aes(y = dose1, fill = "dose1"), width = 6.5) +
  geom_col(aes(y = dose2, fill = "dose2"), width = 5.0) +
  geom_col(aes(y = dose3, fill = "dose3"), width = 3.5) +

  # Cumulative line
  geom_line(data = weekly_cum,
            aes(y = cum_dose1 * scale_k, colour = "Cumul Dose 1"),
            linewidth = 1.3) +

  # Event verticals
  geom_vline(data = events, aes(xintercept = date),
             linetype = "dashed", colour = "firebrick", linewidth = 0.8) +

  # Event description label (top)
  geom_label(
    data  = events,
    aes(x = date, y = max_bar * 0.99, label = label_top),
    hjust = 1, vjust = 1, size = 3,
    colour = "firebrick", fill = alpha("white", 0.88),
    label.padding = unit(0.25, "lines"),
    nudge_x = -10
  ) +

  # Cumulative count label (dose1 + dose2, on the red line)
  geom_label(
    data  = events,
    aes(x = date,
        y = cum1_val * scale_k,
        label = paste0(
          formatC(round(cum1_val), format = "d", big.mark = "\u202f"),
          " enfants (dose 1)\n",
          formatC(round(cum2_val), format = "d", big.mark = "\u202f"),
          " enfants (dose 2)"
        )),
    hjust = 1, vjust = -0.15, size = 2.9,
    colour = "#8B0000", fill = alpha("white", 0.92),
    label.padding = unit(0.22, "lines"),
    nudge_x = -10
  ) +

  scale_fill_manual(values = dose_colours, labels = dose_labels,
                    name = "Type de dose") +
  scale_colour_manual(values = c("Cumul Dose 1" = "#C0392B"), name = NULL) +

  scale_y_continuous(
    name   = "Doses hebdomadaires administrées",
    labels = label_comma(big.mark = "\u202f"),
    expand = expansion(mult = c(0, 0.14)),
    sec.axis = sec_axis(
      transform = ~ . / scale_k,
      name      = "Dose 1 cumulée (enfants vaccinés)",
      labels    = label_comma(big.mark = "\u202f")
    )
  ) +
  scale_x_date(
    name        = NULL,
    date_breaks = "2 months",
    date_labels = "%b\n%Y",
    expand      = expansion(mult = 0.01)
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "grey40", size = 9.5),
    axis.title.y.left  = element_text(margin = margin(r = 8)),
    axis.title.y.right = element_text(margin = margin(l = 8), colour = "#C0392B"),
    axis.text.y.right  = element_text(colour = "#C0392B"),
    legend.position    = "bottom",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  ) +

  labs(
    title    = "Vaccination COVID-19 des enfants en France (5-11 ans)",
    subtitle = "Doses hebdomadaires superposées (Dose 1 derrière · Dose 3 devant)  ·  Ligne rouge = Dose 1 cumulée",
    caption  = "Source : Santé publique France — vacsi-s-a-fra-2023-07-13  |  Arrêt au 31 déc. 2022"
  )

p

ggsave("vacsi_children_weekly.png", plot = p,
       width = 14, height = 7, dpi = 150, bg = "white")

message("Saved -> vacsi_children_weekly.png")

# ── 10. CSV export ────────────────────────────────────────────────────────────
weekly_export <- weekly_cum %>%
  select(
    week,
    dose1_hebdo  = dose1,
    dose2_hebdo  = dose2,
    dose3_hebdo  = dose3,
    cum_dose1,
    cum_dose2
  )

write_csv(weekly_export, "data/weekly_doses_5_to_11.csv")
message("CSV saved -> data/weekly_doses_5_to_11.csv")