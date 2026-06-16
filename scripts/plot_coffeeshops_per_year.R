library(ggplot2)
library(showtext)

input_file <- "data/coffeeshops.csv"
output_dir <- "figures"
output_file <- file.path(output_dir, "coffeeshops_per_year.png")
output_file_no_title <- file.path(output_dir, "coffeeshops_per_year_no_title.png")
output_file_no_title_no_source <- file.path(
  output_dir,
  "coffeeshops_per_year_no_title_no_source.png"
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

coffeeshops_by_year <- data.frame(
  year = as.integer(year_columns),
  coffeeshops = colSums(coffeeshops[year_columns], na.rm = TRUE)
)

base_plot <- ggplot(coffeeshops_by_year, aes(x = year, y = coffeeshops)) +
  geom_line(color = "#006BFF", linewidth = 1.3) +
  geom_point(color = "#006BFF", size = 4) +
  scale_x_continuous(breaks = coffeeshops_by_year$year) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(500, 900)
  ) +
  labs(
    x = NULL,
    y = "Number of coffeeshops",
    caption = "Source: Breuer & Intraval coffeeshops report data"
  ) +
  theme_minimal(base_size = 12, base_family = font_family) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(size = 16, margin = margin(r = 16)),
    axis.text = element_text(size = 13),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot <- base_plot +
  labs(title = "Aggregate Number of Coffeeshops in the Netherlands")

plot_no_title <- base_plot

plot_no_title_no_source <- base_plot +
  labs(caption = NULL)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
ggsave(output_file, plot = plot, width = 9, height = 5, dpi = 300)
ggsave(output_file_no_title, plot = plot_no_title, width = 9, height = 5, dpi = 300)
ggsave(
  output_file_no_title_no_source,
  plot = plot_no_title_no_source,
  width = 9,
  height = 5,
  dpi = 300
)

message("Saved figure to ", output_file)
message("Saved no-title figure to ", output_file_no_title)
message("Saved no-title/no-source figure to ", output_file_no_title_no_source)
