library(dplyr)
library(ggplot2)
library(showtext)
library(tidyr)
library(xml2)

base_url <- "https://www.euda.europa.eu/modules/sbdata/SBDataService2026.cfc"
output_dir <- "figures"
data_dir <- "euda_data"
raw_dir <- file.path(data_dir, "raw")
tidy_file <- file.path(data_dir, "netherlands_cannabis_seizures_euda_2026.csv")
summary_file <- file.path(data_dir, "netherlands_cannabis_seizures_euda_2026_summary.csv")
plot_counts_file <- file.path(output_dir, "euda_netherlands_cannabis_seizures_counts.png")
plot_market_file <- file.path(output_dir, "euda_netherlands_cannabis_market_level_seizures.png")
plot_quantity_file <- file.path(output_dir, "euda_netherlands_cannabis_seizures_quantities.png")

font_family <- "Palatino Linotype"
font_file <- "Palatino Linotype.ttf"
if (file.exists(font_file)) {
  font_add(family = font_family, regular = font_file)
  showtext_opts(dpi = 300)
  showtext_auto()
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

tables <- tibble::tribble(
  ~table_id, ~measure, ~unit, ~category,
  "SZR-1-1-1", "Number of seizures", "seizures", "Resin",
  "SZR-1-1-2", "Number of seizures", "seizures", "Herbal",
  "SZR-1-1-3", "Number of seizures", "seizures", "Plants",
  "SZR-1-1-4", "Number of seizures", "seizures", "Oil",
  "SZR-2-1-1", "Quantity seized", "kg", "Resin",
  "SZR-2-1-2", "Quantity seized", "kg", "Herbal",
  "SZR-2-1-4", "Quantity seized", "kg", "Plants",
  "SZR-2-1-5", "Quantity seized", "kg", "Oil",
  "SZR-2-1-3", "Quantity seized", "plants", "Plants",
  "SZR-2-1-6", "Quantity seized", "litre", "Oil",
  "SZR-3-1-1", "Number of market-level seizures", "seizures", "Resin",
  "SZR-3-1-2", "Number of market-level seizures", "seizures", "Herbal",
  "SZR-3-1-3", "Number of market-level seizures", "seizures", "Plants",
  "SZR-3-1-4", "Number of market-level seizures", "seizures", "Oil"
)

download_table <- function(table_id) {
  destination <- file.path(raw_dir, paste0(table_id, ".xml"))
  if (!file.exists(destination)) {
    url <- paste0(base_url, "?method=fetchxml&tableid=", table_id)
    download.file(url, destination, method = "curl", quiet = TRUE)
  }
  destination
}

parse_table <- function(table_id, measure, unit, category) {
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

  table_data <- bind_rows(parsed_rows)
  netherlands <- table_data %>% filter(.data$Country == "Netherlands")
  year_columns <- grep("^[0-9]{4}$", names(netherlands), value = TRUE)
  market_columns <- grep("^(Retail|Middle|Wholesale) [0-9]{4}$", names(netherlands), value = TRUE)

  if (length(year_columns) > 0) {
    parsed <- netherlands %>%
      select(all_of(year_columns)) %>%
      pivot_longer(
        cols = everything(),
        names_to = "year",
        values_to = "value"
      ) %>%
      mutate(
        market_level = "All",
        year = as.integer(year)
      )
  } else {
    parsed <- netherlands %>%
      select(all_of(market_columns)) %>%
      pivot_longer(
        cols = everything(),
        names_to = "market_year",
        values_to = "value"
      ) %>%
      mutate(
        market_level = sub(" [0-9]{4}$", "", market_year),
        year = as.integer(sub("^.* ", "", market_year))
      ) %>%
      select(-market_year)
  }

  parsed %>%
    mutate(
      table_id = table_id,
      measure = measure,
      unit = unit,
      category = category,
      value = na_if(trimws(value), ""),
      value = suppressWarnings(as.numeric(value))
    ) %>%
    select(table_id, measure, unit, category, market_level, year, value) %>%
    arrange(market_level, year)
}

seizures <- bind_rows(purrr::pmap(tables, parse_table))
write.csv(seizures, tidy_file, row.names = FALSE)

summary_data <- seizures %>%
  group_by(measure, unit, category, market_level) %>%
  summarise(
    observations = sum(!is.na(value)),
    first_year = if (observations > 0) min(year[!is.na(value)]) else NA_integer_,
    last_year = if (observations > 0) max(year[!is.na(value)]) else NA_integer_,
    first_value = if (observations > 0) value[match(first_year, year)] else NA_real_,
    last_value = if (observations > 0) value[match(last_year, year)] else NA_real_,
    max_value = if (observations > 0) max(value, na.rm = TRUE) else NA_real_,
    max_year = if (observations > 0) year[which.max(replace_na(value, -Inf))] else NA_integer_,
    .groups = "drop"
  )
write.csv(summary_data, summary_file, row.names = FALSE)

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
    plot.caption = element_text(size = 8, hjust = 0)
  )

counts_plot <- seizures %>%
  filter(measure == "Number of seizures") %>%
  ggplot(aes(x = year, y = value, color = category, group = category)) +
  geom_line(
    data = ~ .x %>%
      group_by(category) %>%
      filter(sum(!is.na(value)) > 1) %>%
      ungroup(),
    linewidth = 1.1,
    na.rm = TRUE
  ) +
  geom_point(size = 2.6, na.rm = TRUE) +
  facet_wrap(~measure, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c(
    Resin = "#341C1C",
    Herbal = "#4B644A",
    Plants = "#7C6A3D",
    Oil = "#006BFF"
  )) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  labs(
    title = "Netherlands Cannabis Seizures (EUDA 2026)",
    subtitle = "Reported seizure counts by cannabis product category",
    x = NULL,
    y = "Number of seizures",
    caption = "Source: EUDA Statistical Bulletin 2026, seizures of drugs; blank years are not reported."
  ) +
  theme_euda

market_plot <- seizures %>%
  filter(measure == "Number of market-level seizures") %>%
  ggplot(aes(x = year, y = value, color = category, group = category)) +
  geom_line(
    data = ~ .x %>%
      group_by(category, market_level) %>%
      filter(sum(!is.na(value)) > 1) %>%
      ungroup(),
    linewidth = 1.1,
    na.rm = TRUE
  ) +
  geom_point(size = 2.6, na.rm = TRUE) +
  facet_wrap(~market_level, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c(
    Resin = "#341C1C",
    Herbal = "#4B644A",
    Plants = "#7C6A3D",
    Oil = "#006BFF"
  )) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2)) +
  labs(
    title = "Netherlands Cannabis Market-Level Seizures (EUDA 2026)",
    subtitle = "Reported seizure counts by cannabis category and market level",
    x = NULL,
    y = "Number of market-level seizures",
    caption = "Source: EUDA Statistical Bulletin 2026, seizures of drugs; blank years are not reported."
  ) +
  theme_euda

quantity_plot <- seizures %>%
  filter(measure == "Quantity seized") %>%
  mutate(unit = factor(unit, levels = c("kg", "plants", "litre"))) %>%
  ggplot(aes(x = year, y = value, color = category, group = category)) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  geom_point(size = 2.6, na.rm = TRUE) +
  facet_wrap(~unit, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c(
    Resin = "#341C1C",
    Herbal = "#4B644A",
    Plants = "#7C6A3D",
    Oil = "#006BFF"
  )) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  labs(
    title = "Netherlands Cannabis Quantities Seized (EUDA 2026)",
    subtitle = "Reported quantities by cannabis product category and unit",
    x = NULL,
    y = "Quantity seized",
    caption = "Source: EUDA Statistical Bulletin 2026, seizures of drugs; blank years are not reported."
  ) +
  theme_euda

ggsave(plot_counts_file, counts_plot, width = 8, height = 7, dpi = 300, bg = "white")
ggsave(plot_market_file, market_plot, width = 8, height = 7, dpi = 300, bg = "white")
ggsave(plot_quantity_file, quantity_plot, width = 8, height = 8, dpi = 300, bg = "white")

message("Saved tidy Netherlands cannabis seizure data to ", tidy_file)
message("Saved summary table to ", summary_file)
message("Saved counts plot to ", plot_counts_file)
message("Saved market-level plot to ", plot_market_file)
message("Saved quantity plot to ", plot_quantity_file)
