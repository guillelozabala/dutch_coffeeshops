library(dplyr)
library(ggplot2)
library(showtext)
library(tidyr)
library(xml2)

base_url <- "https://www.euda.europa.eu/modules/sbdata/SBDataService2026.cfc"
output_dir <- "figures"
data_dir <- "euda_data"
raw_dir <- file.path(data_dir, "gps_raw")
prevalence_file <- file.path(data_dir, "netherlands_cannabis_prevalence_euda_2026.csv")
frequency_file <- file.path(data_dir, "netherlands_cannabis_frequency_euda_2026.csv")
summary_file <- file.path(data_dir, "netherlands_cannabis_prevalence_euda_2026_summary.csv")
plot_total_file <- file.path(output_dir, "euda_netherlands_cannabis_prevalence_total.png")
plot_sex_file <- file.path(output_dir, "euda_netherlands_cannabis_prevalence_by_sex.png")
plot_frequency_file <- file.path(output_dir, "euda_netherlands_cannabis_frequency.png")

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

prevalence_tables <- tibble::tibble(
  table_id = paste0("GPS-", 1:21),
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

parse_prevalence_table <- function(table_id, measure, age_group) {
  read_xml_table(table_id) %>%
    filter(.data$Country == "Netherlands") %>%
    transmute(
      table_id = table_id,
      measure = measure,
      age_group = age_group,
      survey_year = as.integer(na_if(trimws(.data$Year), "")),
      sample_size = suppressWarnings(as.numeric(na_if(trimws(.data$`Sample size`), ""))),
      male = suppressWarnings(as.numeric(na_if(trimws(.data$Males), ""))),
      female = suppressWarnings(as.numeric(na_if(trimws(.data$Females), ""))),
      total = suppressWarnings(as.numeric(na_if(trimws(.data$Total), "")))
    )
}

prevalence <- purrr::pmap_dfr(prevalence_tables, parse_prevalence_table)
write.csv(prevalence, prevalence_file, row.names = FALSE)

frequency_raw <- read_xml_table("GPS-552-1") %>%
  filter(.data$Country == "Netherlands")

frequency <- frequency_raw %>%
  transmute(
    table_id = "GPS-552-1",
    survey_year = as.integer(na_if(trimws(.data$`Year of survey`), "")),
    sample_size = suppressWarnings(as.numeric(na_if(trimws(.data$`Sample size (15-64)`), ""))),
    last_month_prevalence_15_64 = suppressWarnings(as.numeric(na_if(trimws(.data$`Last month prevalence (15-64 years old)(%)`), ""))),
    users_last_30_days_15_64 = suppressWarnings(as.numeric(na_if(trimws(.data$`Number of users in last 30 days (15-64)`), ""))),
    days_1_to_3_pct = suppressWarnings(as.numeric(na_if(trimws(.data$`1 to 3 days/30 (%)`), ""))),
    days_4_to_9_pct = suppressWarnings(as.numeric(na_if(trimws(.data$`4 to 9 days/30 (%)`), ""))),
    days_10_to_19_pct = suppressWarnings(as.numeric(na_if(trimws(.data$`10 to 19 days/30 (%)`), ""))),
    days_20_plus_pct = suppressWarnings(as.numeric(na_if(trimws(.data$`20+ days/30 (%)`), ""))),
    daily_or_almost_daily_15_64 = suppressWarnings(as.numeric(na_if(trimws(.data$`Prevalence of daily or almost daily use (20 days or more/30) 15-64 years old(%)`), ""))),
    daily_or_almost_daily_15_34 = suppressWarnings(as.numeric(na_if(trimws(.data$`Prevalence of daily or almost daily use (20 days or more/30) 15-34 years old(%)`), "")))
  )
write.csv(frequency, frequency_file, row.names = FALSE)

summary_data <- prevalence %>%
  filter(age_group %in% c("All adults (15-64)", "Young adults (15-34)")) %>%
  select(measure, age_group, survey_year, sample_size, male, female, total) %>%
  arrange(factor(measure, levels = c("Lifetime prevalence", "Last year prevalence", "Last month prevalence")), age_group)
write.csv(summary_data, summary_file, row.names = FALSE)

theme_euda <- theme_minimal(base_family = font_family, base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 35, hjust = 1),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.caption = element_text(size = 8, hjust = 0)
  )

measure_levels <- c("Lifetime prevalence", "Last year prevalence", "Last month prevalence")
age_levels <- age_groups

total_plot_data <- prevalence %>%
  mutate(
    measure = factor(measure, levels = measure_levels),
    age_group = factor(age_group, levels = age_levels)
  )

total_plot <- ggplot(total_plot_data, aes(x = age_group, y = total, color = measure, group = measure)) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  geom_point(size = 3.1, na.rm = TRUE) +
  scale_color_manual(values = c(
    "Lifetime prevalence" = "#341C1C",
    "Last year prevalence" = "#4B644A",
    "Last month prevalence" = "#7C6A3D"
  )) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, NA)) +
  labs(
    title = "Netherlands Cannabis Prevalence (EUDA 2026)",
    subtitle = "Total prevalence by age group, latest available survey: 2024",
    x = NULL,
    y = "Prevalence",
    caption = "Source: EUDA Statistical Bulletin 2026, prevalence of drug use."
  ) +
  theme_euda

sex_plot_data <- prevalence %>%
  select(measure, age_group, survey_year, male, female, total) %>%
  pivot_longer(
    cols = c(male, female, total),
    names_to = "sex",
    values_to = "prevalence"
  ) %>%
  mutate(
    measure = factor(measure, levels = measure_levels),
    age_group = factor(age_group, levels = age_levels),
    sex = factor(sex, levels = c("total", "male", "female"), labels = c("Total", "Male", "Female"))
  )

sex_plot <- ggplot(sex_plot_data, aes(x = age_group, y = prevalence, color = sex, group = sex)) +
  geom_line(linewidth = 1.05, na.rm = TRUE) +
  geom_point(size = 2.7, na.rm = TRUE) +
  facet_wrap(~measure, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c(Total = "#341C1C", Male = "#4B644A", Female = "#7C6A3D")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, NA)) +
  labs(
    title = "Netherlands Cannabis Prevalence by Sex (EUDA 2026)",
    subtitle = "Latest available survey: 2024",
    x = NULL,
    y = "Prevalence",
    caption = "Source: EUDA Statistical Bulletin 2026, prevalence of drug use."
  ) +
  theme_euda

frequency_distribution <- frequency %>%
  select(days_1_to_3_pct, days_4_to_9_pct, days_10_to_19_pct, days_20_plus_pct) %>%
  pivot_longer(
    cols = everything(),
    names_to = "frequency",
    values_to = "percent"
  ) %>%
  mutate(
    frequency = factor(
      frequency,
      levels = c("days_1_to_3_pct", "days_4_to_9_pct", "days_10_to_19_pct", "days_20_plus_pct"),
      labels = c("1-3 days", "4-9 days", "10-19 days", "20+ days")
    )
  )

frequency_plot <- ggplot(frequency_distribution, aes(x = frequency, y = percent, fill = frequency)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = paste0(percent, "%")), vjust = -0.35, family = font_family, size = 4) +
  scale_fill_manual(values = c(
    "1-3 days" = "#C5BFAE",
    "4-9 days" = "#7C6A3D",
    "10-19 days" = "#4B644A",
    "20+ days" = "#341C1C"
  )) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, max(frequency_distribution$percent) * 1.18)) +
  labs(
    title = "Netherlands Frequency of Cannabis Use (EUDA 2026)",
    subtitle = "Distribution among users in the last 30 days, adults aged 15-64, survey year 2024",
    x = NULL,
    y = "Share of last-month users",
    caption = paste0(
      "Source: EUDA Statistical Bulletin 2026. ",
      "Last-month: ", frequency$last_month_prevalence_15_64, "%; daily/almost daily: ",
      frequency$daily_or_almost_daily_15_64, "% (15-64), ",
      frequency$daily_or_almost_daily_15_34, "% (15-34)."
    )
  ) +
  theme_euda +
  theme(legend.position = "none")

ggsave(plot_total_file, total_plot, width = 8, height = 6, dpi = 300, bg = "white")
ggsave(plot_sex_file, sex_plot, width = 8, height = 9, dpi = 300, bg = "white")
ggsave(plot_frequency_file, frequency_plot, width = 8, height = 5, dpi = 300, bg = "white")

message("Saved Netherlands cannabis prevalence data to ", prevalence_file)
message("Saved Netherlands cannabis frequency data to ", frequency_file)
message("Saved summary table to ", summary_file)
message("Saved total prevalence plot to ", plot_total_file)
message("Saved sex prevalence plot to ", plot_sex_file)
message("Saved frequency plot to ", plot_frequency_file)
