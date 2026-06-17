library(dplyr)
library(ggplot2)
library(showtext)
library(tidyr)
library(xml2)

base_url <- "https://www.euda.europa.eu/modules/sbdata/SBDataService2026.cfc"
output_dir <- "figures"
data_dir <- "euda_data"
raw_dir <- file.path(data_dir, "gps_raw")
trend_file <- file.path(data_dir, "netherlands_cannabis_prevalence_trends_euda_2026.csv")
trend_summary_file <- file.path(data_dir, "netherlands_cannabis_prevalence_trends_euda_2026_summary.csv")
plot_windows_file <- file.path(output_dir, "euda_netherlands_cannabis_prevalence_trends_windows.png")
plot_age_file <- file.path(output_dir, "euda_netherlands_cannabis_prevalence_trends_age_groups.png")
plot_recent_file <- file.path(output_dir, "euda_netherlands_cannabis_prevalence_trends_recent.png")

font_family <- "Palatino Linotype"
font_file <- "Palatino Linotype.ttf"
if (file.exists(font_file)) {
  font_add(family = font_family, regular = font_file)
  showtext_opts(dpi = 300)
  showtext_auto()
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

age_groups <- c(
  "All adults (15-64)",
  "Young adults (15-34)",
  "Aged 15-24",
  "Aged 25-34",
  "Aged 35-44",
  "Aged 45-54",
  "Aged 55-64"
)

trend_tables <- tibble::tibble(
  table_id = paste0("GPS-", 201:221),
  measure = rep(
    c("Lifetime prevalence", "Last year prevalence", "Last month prevalence"),
    each = length(age_groups)
  ),
  age_group = rep(age_groups, times = 3)
)

download_table <- function(table_id) {
  destination <- file.path(raw_dir, paste0(table_id, ".xml"))
  if (!file.exists(destination)) {
    url <- paste0(base_url, "?method=fetchxml&tableid=", table_id)
    download.file(url, destination, method = "curl", quiet = TRUE)
  }
  destination
}

read_xml_table <- function(table_id) {
  xml_file <- download_table(table_id)
  doc <- read_xml(xml_file)
  headers <- xml_text(xml_find_all(doc, ".//table-headers/line/column"))
  rows <- xml_find_all(doc, ".//table-body/line")
  parsed_rows <- lapply(rows, function(row) {
    values <- xml_text(xml_find_all(row, "./column"))
    length(values) <- length(headers)
    as.data.frame(
      as.list(stats::setNames(values, headers)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  bind_rows(parsed_rows)
}

parse_trend_table <- function(table_id, measure, age_group) {
  read_xml_table(table_id) %>%
    filter(.data$Country == "Netherlands") %>%
    select(matches("^[0-9]{4}$")) %>%
    pivot_longer(
      cols = everything(),
      names_to = "year",
      values_to = "prevalence"
    ) %>%
    transmute(
      table_id = table_id,
      measure = measure,
      age_group = age_group,
      year = as.integer(.data$year),
      prevalence = suppressWarnings(as.numeric(na_if(trimws(.data$prevalence), "")))
    ) %>%
    filter(!is.na(.data$prevalence)) %>%
    arrange(.data$year)
}

prevalence_trends <- purrr::pmap_dfr(trend_tables, parse_trend_table) %>%
  mutate(
    measure = factor(
      .data$measure,
      levels = c("Lifetime prevalence", "Last year prevalence", "Last month prevalence")
    ),
    age_group = factor(.data$age_group, levels = age_groups)
  )

write.csv(prevalence_trends, trend_file, row.names = FALSE)

trend_summary <- prevalence_trends %>%
  group_by(.data$measure, .data$age_group) %>%
  summarise(
    first_year = min(.data$year),
    first_value = .data$prevalence[which.min(.data$year)],
    latest_year = max(.data$year),
    latest_value = .data$prevalence[which.max(.data$year)],
    change_points = latest_value - first_value,
    observations = n(),
    .groups = "drop"
  )

write.csv(trend_summary, trend_summary_file, row.names = FALSE)

theme_euda <- theme_minimal(base_family = font_family, base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.caption = element_text(size = 7, hjust = 0, lineheight = 1.05),
    plot.margin = margin(10, 16, 10, 10)
  )

measure_colors <- c(
  "Lifetime prevalence" = "#341C1C",
  "Last year prevalence" = "#4B644A",
  "Last month prevalence" = "#7C6A3D"
)

age_colors <- c(
  "All adults (15-64)" = "#341C1C",
  "Young adults (15-34)" = "#4B644A",
  "Aged 15-24" = "#7C6A3D",
  "Aged 25-34" = "#2E5E70",
  "Aged 35-44" = "#9A5B45",
  "Aged 45-54" = "#5B4E6D",
  "Aged 55-64" = "#707070"
)

caption_text <- paste(
  strwrap(
    paste(
      "Source: EUDA Statistical Bulletin 2026, prevalence of drug use, trends tables.",
      "EUDA flags Netherlands trend data for caution because methodology changed in 2014 and 2018."
    ),
    width = 120
  ),
  collapse = "\n"
)

windows_plot <- prevalence_trends %>%
  filter(.data$age_group %in% c("All adults (15-64)", "Young adults (15-34)")) %>%
  ggplot(aes(x = .data$year, y = .data$prevalence, color = .data$measure)) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.7) +
  facet_wrap(~age_group, ncol = 1) +
  scale_color_manual(values = measure_colors) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, NA)) +
  labs(
    title = "Netherlands Cannabis Prevalence Trends (EUDA 2026)",
    subtitle = "National survey trend tables, adults and young adults",
    x = NULL,
    y = "Prevalence",
    caption = caption_text
  ) +
  theme_euda

age_plot <- prevalence_trends %>%
  filter(.data$measure != "Lifetime prevalence") %>%
  ggplot(aes(x = .data$year, y = .data$prevalence, color = .data$age_group)) +
  geom_line(linewidth = 0.95) +
  geom_point(size = 2.1) +
  facet_wrap(~measure, ncol = 1, scales = "free_y") +
  scale_color_manual(values = age_colors) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, NA)) +
  labs(
    title = "Netherlands Cannabis Use Trends by Age Group (EUDA 2026)",
    subtitle = "Last-year and last-month prevalence from national survey trend tables",
    x = NULL,
    y = "Prevalence",
    caption = caption_text
  ) +
  theme_euda +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

recent_plot <- prevalence_trends %>%
  filter(.data$year >= 2014) %>%
  ggplot(aes(x = .data$year, y = .data$prevalence, color = .data$age_group)) +
  geom_line(linewidth = 0.95) +
  geom_point(size = 2.1) +
  facet_wrap(~measure, ncol = 1, scales = "free_y") +
  scale_color_manual(values = age_colors) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, NA)) +
  labs(
    title = "Recent Netherlands Cannabis Prevalence Trends (EUDA 2026)",
    subtitle = "2014-2024, after the first EUDA-noted methodology change",
    x = NULL,
    y = "Prevalence",
    caption = caption_text
  ) +
  theme_euda +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

ggsave(plot_windows_file, windows_plot, width = 8, height = 7, dpi = 300, bg = "white")
ggsave(plot_age_file, age_plot, width = 8, height = 8, dpi = 300, bg = "white")
ggsave(plot_recent_file, recent_plot, width = 8, height = 8, dpi = 300, bg = "white")

message("Saved Netherlands cannabis trend data to ", trend_file)
message("Saved trend summary to ", trend_summary_file)
message("Saved trend plot to ", plot_windows_file)
message("Saved age-group trend plot to ", plot_age_file)
message("Saved recent trend plot to ", plot_recent_file)
