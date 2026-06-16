library(ggplot2)
library(gstat)
library(httr)
library(sf)
library(showtext)

input_file <- "data/coffeeshops.csv"
geodata_dir <- "geodata"
output_dir <- "figures"
geojson_file <- file.path(geodata_dir, "cbs_gemeenten_2022_gegeneraliseerd.geojson")
output_file_1999 <- file.path(output_dir, "coffeeshop_access_kriging_1999.png")
output_file_2022 <- file.path(output_dir, "coffeeshop_access_kriging_2022.png")
output_file_change <- file.path(output_dir, "coffeeshop_access_kriging_change_1999_2022.png")

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

nearest_access_distance <- function(points, year) {
  value_column <- paste0("coffeeshops_", year)
  shop_points <- points[points[[value_column]] > 0, ]
  if (nrow(shop_points) == 0) {
    stop("No municipalities with coffeeshops in ", year)
  }

  distances <- st_distance(points, shop_points)
  as.numeric(apply(distances, 1, min)) / 1000
}

krige_distance <- function(points, prediction_grid, distance_column) {
  krige_points <- points[, distance_column]
  names(krige_points)[names(krige_points) == distance_column] <- "distance_km"

  empirical_variogram <- variogram(distance_km ~ 1, as(krige_points, "Spatial"))
  initial_model <- vgm(
    psill = stats::var(krige_points$distance_km, na.rm = TRUE),
    model = "Sph",
    range = 100000,
    nugget = 0.1
  )

  fitted_model <- tryCatch(
    fit.variogram(empirical_variogram, initial_model),
    error = function(e) {
      warning("Variogram fitting failed; using initial spherical model. ", conditionMessage(e))
      initial_model
    }
  )

  prediction <- krige(
    distance_km ~ 1,
    as(krige_points, "Spatial"),
    as(prediction_grid, "Spatial"),
    model = fitted_model
  )

  prediction_df <- cbind(
    as.data.frame(sp::coordinates(prediction)),
    as.data.frame(prediction)
  )
  names(prediction_df)[1:2] <- c("x", "y")
  names(prediction_df)[names(prediction_df) == "var1.pred"] <- "distance_km"
  prediction_df$distance_km <- pmax(prediction_df$distance_km, 0)
  prediction_df
}

gemeenten <- st_read(geojson_file, quiet = TRUE)
gemeenten$join_name <- standardize_name(gemeenten$statnaam)

coffeeshops <- read.csv(
  input_file,
  check.names = FALSE,
  na.strings = "-",
  stringsAsFactors = FALSE
)
coffeeshops$join_name <- standardize_name(coffeeshops$Gemeente)
coffeeshops_1999_2022 <- coffeeshops[, c("Gemeente", "join_name", "1999", "2022")]
names(coffeeshops_1999_2022) <- c("gemeente", "join_name", "coffeeshops_1999", "coffeeshops_2022")

gemeenten <- merge(gemeenten, coffeeshops_1999_2022, by = "join_name", all.x = TRUE)
missing_polygon_values <- unique(gemeenten$statnaam[
  is.na(gemeenten$coffeeshops_1999) | is.na(gemeenten$coffeeshops_2022)
])
if (length(missing_polygon_values) > 0) {
  message(
    "Polygon rows without coffeeshop data treated as zero: ",
    paste(missing_polygon_values, collapse = ", ")
  )
}
gemeenten$coffeeshops_1999[is.na(gemeenten$coffeeshops_1999)] <- 0
gemeenten$coffeeshops_2022[is.na(gemeenten$coffeeshops_2022)] <- 0

gemeenten_rd <- st_transform(st_make_valid(gemeenten), 28992)
centroid_points <- st_point_on_surface(gemeenten_rd)
centroid_points$distance_km_1999 <- nearest_access_distance(centroid_points, 1999)
centroid_points$distance_km_2022 <- nearest_access_distance(centroid_points, 2022)

netherlands_union <- st_union(gemeenten_rd)
prediction_grid <- st_make_grid(
  gemeenten_rd,
  cellsize = 5000,
  what = "centers",
  square = TRUE
)
prediction_grid <- st_sf(geometry = prediction_grid)
prediction_grid <- prediction_grid[st_intersects(prediction_grid, netherlands_union, sparse = FALSE), ]
st_crs(prediction_grid) <- st_crs(gemeenten_rd)

prediction_1999 <- krige_distance(centroid_points, prediction_grid, "distance_km_1999")
prediction_2022 <- krige_distance(centroid_points, prediction_grid, "distance_km_2022")
prediction_change <- prediction_2022
prediction_change$distance_change_km <- prediction_2022$distance_km - prediction_1999$distance_km

max_distance <- ceiling(max(prediction_1999$distance_km, prediction_2022$distance_km, na.rm = TRUE) / 10) * 10
max_abs_change <- ceiling(max(abs(prediction_change$distance_change_km), na.rm = TRUE) / 10) * 10

make_kriging_map <- function(prediction_df, year, output_file) {
  map <- ggplot() +
    geom_raster(
      data = prediction_df,
      aes(x = x, y = y, fill = distance_km),
      interpolate = TRUE
    ) +
    geom_sf(data = gemeenten_rd, fill = NA, color = "white", linewidth = 0.08) +
    coord_sf(expand = FALSE) +
    scale_fill_gradientn(
      colors = c("#006BFF", "#BFD7FF", "#FFF7BC", "#F46D43", "#9E0142"),
      limits = c(0, max_distance),
      name = "Kriged distance\nto nearest\ncoffeeshop\nmunicipality (km)"
    ) +
    labs(
      title = paste("Kriged Access to Coffeeshop Municipalities,", year),
      caption = "Ordinary kriging of municipality-centroid nearest-distance values; clipped to CBS/PDOK 2022 boundaries."
    ) +
    theme_void(base_family = font_family) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.caption = element_text(size = 8, hjust = 1),
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.margin = margin(10, 10, 10, 10)
    )

  ggsave(output_file, plot = map, width = 7, height = 8, dpi = 300, bg = "white")
}

make_kriging_change_map <- function(prediction_df, output_file) {
  map <- ggplot() +
    geom_raster(
      data = prediction_df,
      aes(x = x, y = y, fill = distance_change_km),
      interpolate = TRUE
    ) +
    geom_sf(data = gemeenten_rd, fill = NA, color = "white", linewidth = 0.08) +
    coord_sf(expand = FALSE) +
    scale_fill_gradient2(
      low = "#006BFF",
      mid = "#F7F7F7",
      high = "#9E0142",
      midpoint = 0,
      limits = c(-max_abs_change, max_abs_change),
      breaks = seq(-max_abs_change, max_abs_change, length.out = 5),
      labels = function(x) sprintf("%g", x),
      name = "Kriged change\nin nearest\ndistance (km)"
    ) +
    labs(
      title = "Kriged Change in Access, 1999-2022",
      caption = "Negative values mean the nearest coffeeshop municipality became closer; positive values mean farther."
    ) +
    theme_void(base_family = font_family) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.caption = element_text(size = 8, hjust = 1),
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.margin = margin(10, 10, 10, 10)
    )

  ggsave(output_file, plot = map, width = 7, height = 8, dpi = 300, bg = "white")
}

make_kriging_map(prediction_1999, 1999, output_file_1999)
make_kriging_map(prediction_2022, 2022, output_file_2022)
make_kriging_change_map(prediction_change, output_file_change)

message("Saved 1999 kriging access map to ", output_file_1999)
message("Saved 2022 kriging access map to ", output_file_2022)
message("Saved kriging access change map to ", output_file_change)
