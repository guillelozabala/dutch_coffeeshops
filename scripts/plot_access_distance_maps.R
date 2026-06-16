library(ggplot2)
library(httr)
library(jsonlite)
library(showtext)

input_file <- "data/coffeeshops.csv"
geodata_dir <- "geodata"
output_dir <- "figures"
geojson_file <- file.path(geodata_dir, "cbs_gemeenten_2022_gegeneraliseerd.geojson")
output_file_1999 <- file.path(output_dir, "coffeeshop_access_distance_1999.png")
output_file_2022 <- file.path(output_dir, "coffeeshop_access_distance_2022.png")
output_file_change <- file.path(output_dir, "coffeeshop_access_distance_change_1999_2022.png")

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

ring_centroid <- function(coords) {
  x <- coords[, 1]
  y <- coords[, 2]
  if (x[1] != x[length(x)] || y[1] != y[length(y)]) {
    x <- c(x, x[1])
    y <- c(y, y[1])
  }

  cross <- x[-length(x)] * y[-1] - x[-1] * y[-length(y)]
  area <- sum(cross) / 2

  if (abs(area) < 1e-12) {
    return(c(area = 0, lon = mean(x), lat = mean(y)))
  }

  lon <- sum((x[-length(x)] + x[-1]) * cross) / (6 * area)
  lat <- sum((y[-length(y)] + y[-1]) * cross) / (6 * area)
  c(area = abs(area), lon = lon, lat = lat)
}

feature_to_rows <- function(feature) {
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

feature_to_centroid <- function(feature) {
  geom <- feature$geometry
  polygons <- switch(
    geom$type,
    Polygon = list(geom$coordinates),
    MultiPolygon = geom$coordinates,
    stop("Unsupported geometry type: ", geom$type)
  )

  centroids <- do.call(
    rbind,
    lapply(polygons, function(polygon) {
      outer_ring <- polygon[[1]]
      coords <- do.call(rbind, lapply(outer_ring, unlist))
      ring_centroid(coords)
    })
  )

  if (sum(centroids[, "area"]) == 0) {
    lon <- mean(centroids[, "lon"])
    lat <- mean(centroids[, "lat"])
  } else {
    lon <- weighted.mean(centroids[, "lon"], centroids[, "area"])
    lat <- weighted.mean(centroids[, "lat"], centroids[, "area"])
  }

  data.frame(
    statcode = feature$properties$statcode,
    statnaam = feature$properties$statnaam,
    lon_centroid = lon,
    lat_centroid = lat,
    stringsAsFactors = FALSE
  )
}

haversine_km <- function(lon1, lat1, lon2, lat2) {
  radius_km <- 6371.0088
  to_rad <- pi / 180
  lon1 <- lon1 * to_rad
  lat1 <- lat1 * to_rad
  lon2 <- lon2 * to_rad
  lat2 <- lat2 * to_rad

  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  radius_km * 2 * atan2(sqrt(a), sqrt(1 - a))
}

nearest_access <- function(centroids, year) {
  value_column <- paste0("coffeeshops_", year)
  shop_locations <- centroids[centroids[[value_column]] > 0, ]
  if (nrow(shop_locations) == 0) {
    stop("No municipalities with coffeeshops in ", year)
  }

  nearest_distance <- numeric(nrow(centroids))
  nearest_municipality <- character(nrow(centroids))

  for (i in seq_len(nrow(centroids))) {
    distances <- haversine_km(
      centroids$lon_centroid[i],
      centroids$lat_centroid[i],
      shop_locations$lon_centroid,
      shop_locations$lat_centroid
    )
    closest <- which.min(distances)
    nearest_distance[i] <- distances[closest]
    nearest_municipality[i] <- shop_locations$statnaam[closest]
  }

  data.frame(
    join_name = centroids$join_name,
    nearest_municipality = nearest_municipality,
    distance_km = nearest_distance,
    stringsAsFactors = FALSE
  )
}

geojson <- fromJSON(geojson_file, simplifyVector = FALSE)
municipality_polygons <- do.call(rbind, lapply(geojson$features, feature_to_rows))
municipality_polygons$join_name <- standardize_name(municipality_polygons$statnaam)
municipality_polygons$group <- interaction(
  municipality_polygons$statcode,
  municipality_polygons$polygon_id,
  drop = TRUE
)

municipality_centroids <- do.call(rbind, lapply(geojson$features, feature_to_centroid))
municipality_centroids$join_name <- standardize_name(municipality_centroids$statnaam)

coffeeshops <- read.csv(
  input_file,
  check.names = FALSE,
  na.strings = "-",
  stringsAsFactors = FALSE
)
coffeeshops$join_name <- standardize_name(coffeeshops$Gemeente)
coffeeshops_1999_2022 <- coffeeshops[, c("Gemeente", "join_name", "1999", "2022")]
names(coffeeshops_1999_2022) <- c("gemeente", "join_name", "coffeeshops_1999", "coffeeshops_2022")

centroid_data <- merge(
  municipality_centroids,
  coffeeshops_1999_2022,
  by = "join_name",
  all.x = TRUE
)

missing_polygon_values <- unique(centroid_data$statnaam[
  is.na(centroid_data$coffeeshops_1999) | is.na(centroid_data$coffeeshops_2022)
])
if (length(missing_polygon_values) > 0) {
  message(
    "Polygon rows without coffeeshop data treated as zero: ",
    paste(missing_polygon_values, collapse = ", ")
  )
}
centroid_data$coffeeshops_1999[is.na(centroid_data$coffeeshops_1999)] <- 0
centroid_data$coffeeshops_2022[is.na(centroid_data$coffeeshops_2022)] <- 0

access_1999 <- nearest_access(centroid_data, 1999)
access_2022 <- nearest_access(centroid_data, 2022)
names(access_1999)[names(access_1999) == "nearest_municipality"] <- "nearest_municipality_1999"
names(access_1999)[names(access_1999) == "distance_km"] <- "distance_km_1999"
names(access_2022)[names(access_2022) == "nearest_municipality"] <- "nearest_municipality_2022"
names(access_2022)[names(access_2022) == "distance_km"] <- "distance_km_2022"

access_data <- merge(access_1999, access_2022, by = "join_name")
access_data$distance_change_km <- access_data$distance_km_2022 - access_data$distance_km_1999

map_data <- merge(municipality_polygons, access_data, by = "join_name", all.x = TRUE)
map_data <- map_data[order(map_data$group, map_data$order), ]

max_distance <- ceiling(max(map_data$distance_km_1999, map_data$distance_km_2022, na.rm = TRUE) / 10) * 10
max_abs_change <- ceiling(max(abs(map_data$distance_change_km), na.rm = TRUE) / 10) * 10

make_distance_map <- function(year, output_file) {
  distance_column <- paste0("distance_km_", year)
  plot_data <- map_data
  plot_data$distance_km <- plot_data[[distance_column]]

  map <- ggplot(plot_data) +
    geom_polygon(
      aes(x = lon, y = lat, group = group, fill = distance_km),
      color = "white",
      linewidth = 0.08
    ) +
    coord_quickmap(expand = FALSE) +
    scale_fill_gradientn(
      colors = c("#006BFF", "#BFD7FF", "#FFF7BC", "#F46D43", "#9E0142"),
      limits = c(0, max_distance),
      breaks = seq(0, max_distance, by = 25),
      name = "Distance\nto nearest\ncoffeeshop\nmunicipality (km)"
    ) +
    labs(
      title = paste("Access to Coffeeshop Municipalities,", year),
      caption = "Distance from 2022 municipality centroid to nearest municipality with >0 coffeeshops."
    ) +
    theme_void(base_family = font_family) +
    theme(
      plot.title = element_text(face = "bold", size = 19, hjust = 0.5),
      plot.caption = element_text(size = 8, hjust = 1),
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.margin = margin(10, 10, 10, 10)
    )

  ggsave(output_file, plot = map, width = 7, height = 8, dpi = 300, bg = "white")
}

make_change_map <- function(output_file) {
  map <- ggplot(map_data) +
    geom_polygon(
      aes(x = lon, y = lat, group = group, fill = distance_change_km),
      color = "white",
      linewidth = 0.08
    ) +
    coord_quickmap(expand = FALSE) +
    scale_fill_gradient2(
      low = "#006BFF",
      mid = "#F7F7F7",
      high = "#9E0142",
      midpoint = 0,
      limits = c(-max_abs_change, max_abs_change),
      breaks = seq(-max_abs_change, max_abs_change, length.out = 5),
      labels = function(x) sprintf("%g", x),
      name = "Change in\nnearest distance\n(km)"
    ) +
    labs(
      title = "Change in Access, 1999-2022",
      caption = "Negative values mean the nearest coffeeshop municipality became closer; positive values mean farther."
    ) +
    theme_void(base_family = font_family) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      plot.caption = element_text(size = 8, hjust = 1),
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.margin = margin(10, 10, 10, 10)
    )

  ggsave(output_file, plot = map, width = 7, height = 8, dpi = 300, bg = "white")
}

make_distance_map(1999, output_file_1999)
make_distance_map(2022, output_file_2022)
make_change_map(output_file_change)

message("Saved 1999 access-distance map to ", output_file_1999)
message("Saved 2022 access-distance map to ", output_file_2022)
message("Saved access-distance change map to ", output_file_change)
message("Maximum nearest distance in 1999: ", round(max(access_data$distance_km_1999), 1), " km")
message("Maximum nearest distance in 2022: ", round(max(access_data$distance_km_2022), 1), " km")
