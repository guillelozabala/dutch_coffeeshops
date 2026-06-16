library(ggplot2)
library(httr)
library(jsonlite)
library(showtext)

input_file <- "data/coffeeshops.csv"
geodata_dir <- "geodata"
output_dir <- "figures"
geojson_file <- file.path(geodata_dir, "cbs_gemeenten_2022_gegeneraliseerd.geojson")
output_file_1999 <- file.path(output_dir, "coffeeshops_map_1999.png")
output_file_2022 <- file.path(output_dir, "coffeeshops_map_2022.png")
output_file_change <- file.path(output_dir, "coffeeshops_map_relative_change_1999_2022.png")

font_family <- "Palatino Linotype"
font_file <- "Palatino Linotype.ttf"

font_add(family = font_family, regular = font_file)
showtext_opts(dpi = 300)
showtext_auto()

dir.create(geodata_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

cbs_wfs_url <- modify_url(
  "https://service.pdok.nl/cbs/gebiedsindelingen/2022/wfs/v1_0",
  query = list(
    service = "WFS",
    version = "2.0.0",
    request = "GetFeature",
    typeNames = "gebiedsindelingen:gemeente_gegeneraliseerd",
    outputFormat = "application/json",
    srsName = "EPSG:4326"
  )
)

if (!file.exists(geojson_file)) {
  response <- GET(cbs_wfs_url, timeout(60))
  stop_for_status(response)
  writeBin(content(response, "raw"), geojson_file)
}

normalize_name <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("&", " en ", x)
  x <- gsub("['`’]", "", x)
  x <- gsub("\\([0-9]+\\)", "", x)
  x <- gsub("\\.", "", x)
  x <- gsub(",", " ", x)
  x <- gsub("\\(", " ", x)
  x <- gsub("\\)", " ", x)
  x <- gsub("-", " ", x)
  x <- gsub(" a/d ", " aan den ", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

standardize_name <- function(x) {
  x <- normalize_name(x)
  recodes <- c(
    "den haag" = "s gravenhage",
    "geertruiden berg" = "geertruidenberg",
    "heilo" = "heiloo",
    "hellevoetssluis" = "hellevoetsluis",
    "noordoost polder" = "noordoostpolder",
    "nuenen ca" = "nuenen gerwen en nederwetten",
    "nuenen c a" = "nuenen gerwen en nederwetten",
    "oestgeest" = "oegstgeest",
    "valkenburg aan den geul" = "valkenburg aan de geul",
    "vijfheerenlande" = "vijfheerenlanden",
    "wijchem" = "wijchen"
  )
  matched <- match(x, names(recodes), nomatch = 0)
  x[matched > 0] <- recodes[matched[matched > 0]]
  x
}

geometry_to_rows <- function(feature) {
  geom <- feature$geometry
  if (is.null(geom) || is.null(geom$coordinates)) {
    return(NULL)
  }

  polygons <- switch(
    geom$type,
    Polygon = list(geom$coordinates),
    MultiPolygon = geom$coordinates,
    stop("Unsupported geometry type: ", geom$type)
  )

  do.call(
    rbind,
    lapply(seq_along(polygons), function(polygon_id) {
      outer_ring <- polygons[[polygon_id]][[1]]
      coords <- do.call(rbind, lapply(outer_ring, unlist))
      data.frame(
        statcode = feature$properties$statcode,
        statnaam = feature$properties$statnaam,
        polygon_id = polygon_id,
        order = seq_len(nrow(coords)),
        lon = coords[, 1],
        lat = coords[, 2],
        stringsAsFactors = FALSE
      )
    })
  )
}

geojson <- fromJSON(geojson_file, simplifyVector = FALSE)
municipality_polygons <- do.call(rbind, lapply(geojson$features, geometry_to_rows))
municipality_polygons$join_name <- standardize_name(municipality_polygons$statnaam)
municipality_polygons$group <- interaction(
  municipality_polygons$statcode,
  municipality_polygons$polygon_id,
  drop = TRUE
)

coffeeshops <- read.csv(
  input_file,
  check.names = FALSE,
  na.strings = "-",
  stringsAsFactors = FALSE
)

coffeeshops$join_name <- standardize_name(coffeeshops$Gemeente)
coffeeshops_1999_2022 <- coffeeshops[, c("Gemeente", "join_name", "1999", "2022")]
names(coffeeshops_1999_2022) <- c("gemeente", "join_name", "coffeeshops_1999", "coffeeshops_2022")

unmatched_coffeeshops <- setdiff(
  unique(coffeeshops_1999_2022$join_name),
  unique(municipality_polygons$join_name)
)
if (length(unmatched_coffeeshops) > 0) {
  warning(
    "Coffeeshop rows without a matched 2022 polygon: ",
    paste(coffeeshops_1999_2022$gemeente[
      coffeeshops_1999_2022$join_name %in% unmatched_coffeeshops
    ], collapse = ", ")
  )
}

map_data <- merge(
  municipality_polygons,
  coffeeshops_1999_2022,
  by = "join_name",
  all.x = TRUE
)
map_data <- map_data[order(map_data$group, map_data$order), ]

missing_polygon_values <- unique(map_data$statnaam[
  is.na(map_data$coffeeshops_1999) | is.na(map_data$coffeeshops_2022)
])
if (length(missing_polygon_values) > 0) {
  message(
    "Polygon rows without coffeeshop data treated as zero: ",
    paste(missing_polygon_values, collapse = ", ")
  )
}
map_data$coffeeshops_1999[is.na(map_data$coffeeshops_1999)] <- 0
map_data$coffeeshops_2022[is.na(map_data$coffeeshops_2022)] <- 0

max_coffeeshops <- max(
  map_data$coffeeshops_1999,
  map_data$coffeeshops_2022,
  na.rm = TRUE
)

change_levels <- c(
  "No shops both years",
  "Full loss (-100%)",
  "-99% to -50%",
  "-49% to -1%",
  "No change",
  "+1% to +49%",
  "+50% to +99%",
  "+100% or more",
  "New (0 to >0)"
)

classify_change <- function(coffeeshops_1999, coffeeshops_2022) {
  change_class <- rep(NA_character_, length(coffeeshops_1999))
  pct_change <- rep(NA_real_, length(coffeeshops_1999))

  existing <- coffeeshops_1999 > 0
  pct_change[existing] <- 100 * (coffeeshops_2022[existing] - coffeeshops_1999[existing]) /
    coffeeshops_1999[existing]

  change_class[coffeeshops_1999 == 0 & coffeeshops_2022 == 0] <- "No shops both years"
  change_class[coffeeshops_1999 == 0 & coffeeshops_2022 > 0] <- "New (0 to >0)"
  change_class[existing & coffeeshops_2022 == 0] <- "Full loss (-100%)"
  change_class[existing & pct_change > -100 & pct_change <= -50] <- "-99% to -50%"
  change_class[existing & pct_change > -50 & pct_change < 0] <- "-49% to -1%"
  change_class[existing & pct_change == 0] <- "No change"
  change_class[existing & pct_change > 0 & pct_change < 50] <- "+1% to +49%"
  change_class[existing & pct_change >= 50 & pct_change < 100] <- "+50% to +99%"
  change_class[existing & pct_change >= 100] <- "+100% or more"

  factor(change_class, levels = change_levels)
}

map_data$relative_change_class <- classify_change(
  map_data$coffeeshops_1999,
  map_data$coffeeshops_2022
)

make_map <- function(year, output_file) {
  value_column <- paste0("coffeeshops_", year)
  plot_data <- map_data
  plot_data$coffeeshops <- plot_data[[value_column]]

  map <- ggplot(plot_data) +
    geom_polygon(
      aes(x = lon, y = lat, group = group, fill = coffeeshops),
      color = "white",
      linewidth = 0.08
    ) +
    coord_quickmap(expand = FALSE) +
    scale_fill_gradientn(
      colors = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#006BFF", "#08306B"),
      trans = "sqrt",
      limits = c(0, max_coffeeshops),
      breaks = c(0, 1, 5, 10, 25, 50, 100, 200),
      na.value = "#E6E6E6",
      name = "Coffeeshops"
    ) +
    labs(
      title = paste("Coffeeshops per Municipality,", year),
      caption = "Boundaries: CBS/PDOK gemeente_gegeneraliseerd 2022"
    ) +
    theme_void(base_family = font_family) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      plot.caption = element_text(size = 9, hjust = 1),
      legend.position = "right",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      plot.margin = margin(10, 10, 10, 10)
    )

  ggsave(output_file, plot = map, width = 7, height = 8, dpi = 300, bg = "white")
}

make_relative_change_map <- function(output_file) {
  map <- ggplot(map_data) +
    geom_polygon(
      aes(x = lon, y = lat, group = group, fill = relative_change_class),
      color = "white",
      linewidth = 0.08
    ) +
    coord_quickmap(expand = FALSE) +
    scale_fill_manual(
      values = c(
        "No shops both years" = "#F5F5F5",
        "Full loss (-100%)" = "#9E0142",
        "-99% to -50%" = "#D53E4F",
        "-49% to -1%" = "#F46D43",
        "No change" = "#FFFFBF",
        "+1% to +49%" = "#ABDDA4",
        "+50% to +99%" = "#66C2A5",
        "+100% or more" = "#006837",
        "New (0 to >0)" = "#006BFF"
      ),
      drop = FALSE,
      name = "Change"
    ) +
    labs(
      title = "Relative Change in Coffeeshops, 1999-2022",
      caption = "Percent change uses 1999 as baseline; zero-baseline gains are shown as New. Boundaries: CBS/PDOK 2022."
    ) +
    theme_void(base_family = font_family) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      plot.caption = element_text(size = 8, hjust = 1),
      legend.position = "right",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 9),
      plot.margin = margin(10, 10, 10, 10)
    )

  ggsave(output_file, plot = map, width = 7, height = 8, dpi = 300, bg = "white")
}

make_map(1999, output_file_1999)
make_map(2022, output_file_2022)
make_relative_change_map(output_file_change)

message("Saved 1999 map to ", output_file_1999)
message("Saved 2022 map to ", output_file_2022)
message("Saved relative-change map to ", output_file_change)
message("Cached CBS/PDOK polygons to ", geojson_file)
