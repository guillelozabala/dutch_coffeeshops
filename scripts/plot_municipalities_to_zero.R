library(ggplot2)
library(showtext)

input_file <- "data/coffeeshops.csv"
output_dir <- "figures"
output_file <- file.path(output_dir, "municipalities_to_zero_per_year.png")
output_file_no_title_no_source <- file.path(
  output_dir,
  "municipalities_to_zero_per_year_no_title_no_source.png"
)
font_family <- "Palatino Linotype"
font_file <- "Palatino Linotype.ttf"

font_add(family = font_family, regular = font_file)
showtext_opts(dpi = 300)
showtext_auto()

coffeeshops <- read.csv(
  input_file,
  check.names = FALSE,
  na.strings = "-"
)

year_columns <- setdiff(names(coffeeshops), "Gemeente")
years <- as.integer(year_columns)

transitions <- do.call(
  rbind,
  lapply(seq_len(nrow(coffeeshops)), function(row_index) {
    values <- as.numeric(coffeeshops[row_index, year_columns])
    observed <- !is.na(values)
    values <- values[observed]
    observed_years <- years[observed]

    if (length(values) < 2) {
      return(NULL)
    }

    changed_to_zero <- head(values, -1) > 0 & tail(values, -1) == 0

    if (!any(changed_to_zero)) {
      return(NULL)
    }

    data.frame(
      gemeente = coffeeshops$Gemeente[row_index],
      from_year = head(observed_years, -1)[changed_to_zero],
      year = tail(observed_years, -1)[changed_to_zero],
      stringsAsFactors = FALSE
    )
  })
)

if (is.null(transitions)) {
  municipalities_to_zero <- data.frame(
    year = years[-1],
    municipalities = 0
  )
} else {
  municipalities_to_zero <- aggregate(
    gemeente ~ year,
    data = transitions,
    FUN = length
  )
  names(municipalities_to_zero)[2] <- "municipalities"
  municipalities_to_zero <- merge(
    data.frame(year = years[-1]),
    municipalities_to_zero,
    by = "year",
    all.x = TRUE
  )
  municipalities_to_zero$municipalities[is.na(municipalities_to_zero$municipalities)] <- 0
}

max_municipalities <- max(municipalities_to_zero$municipalities)

plot <- ggplot(municipalities_to_zero, aes(x = year, y = municipalities)) +
  geom_col(fill = "#006BFF", width = 0.7) +
  geom_text(
    data = subset(municipalities_to_zero, municipalities > 0),
    aes(label = municipalities),
    vjust = -0.5,
    size = 5,
    family = font_family
  ) +
  scale_x_continuous(breaks = municipalities_to_zero$year) +
  scale_y_continuous(
    breaks = 0:max_municipalities,
    limits = c(0, max_municipalities + 0.5),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Municipalities Going from Coffeeshops to None",
    x = NULL,
    y = "Number of municipalities",
    caption = "Source: Breuer & Intraval coffeeshops report data"
  ) +
  theme_minimal(base_size = 12, base_family = font_family) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.y = element_text(size = 16, margin = margin(r = 16)),
    axis.text = element_text(size = 13),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_no_title_no_source <- plot +
  labs(title = NULL, caption = NULL) +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1))

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
ggsave(output_file, plot = plot, width = 9, height = 5, dpi = 300)
ggsave(
  output_file_no_title_no_source,
  plot = plot_no_title_no_source,
  width = 5,
  height = 5,
  dpi = 300
)

print(municipalities_to_zero)

if (!is.null(transitions)) {
  message(
    "Total municipalities that make at least one >0 to 0 transition: ",
    length(unique(transitions$gemeente))
  )
}

message("Saved figure to ", output_file)
message("Saved no-title/no-source figure to ", output_file_no_title_no_source)
