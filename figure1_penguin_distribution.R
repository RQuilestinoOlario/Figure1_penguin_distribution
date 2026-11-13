# Figure 1. Global breeding distribution of penguins
# Spot the difference! Penguins around the world and how to tell them apart
# Raven Quilestino-Olario, Sonja Berg and Bernd Wagner
#
# Run from the folder containing the two input CSV files.
# Output: PDF and SVG versions of the figure.
# Locations are approximate; clustering and offsets are for plotting only.

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(readr)
library(ggtext)
library(stringi)
library(grid)
library(dbscan)

sf::sf_use_s2(FALSE)

# Breeding locations
# Both input CSV files should be in the working directory.
input_files <- c("MAPPPD.csv", "penguin_sites.csv")
if (any(!file.exists(input_files))) {
  stop("Missing input file(s): ",
       paste(input_files[!file.exists(input_files)], collapse = ", "))
}

# Base world map
world <- ne_countries(scale = "medium", returnclass = "sf") |>
  st_make_valid()

world_land <- st_as_sf(st_union(world))

world_coast <- ne_coastline(scale = "medium", returnclass = "sf") |>
  st_make_valid()

antarctica_land <- world |>
  filter(admin == "Antarctica") |>
  st_make_valid()

mapppd <- read_csv("MAPPPD.csv", show_col_types = FALSE) |>
  mutate(common_name_clean = stringi::stri_trans_general(
    tolower(common_name), "Latin-ASCII"
  ))

# Extract emperor and Adélie breeding locations from MAPPPD.
get_mapppd_sites <- function(common, species, scientific, genus) {
  mapppd |>
    filter(common_name_clean == .env$common) |>
    distinct(site_name, longitude_epsg_4326, latitude_epsg_4326) |>
    transmute(
      genus = .env$genus,
      species = .env$species,
      scientific = .env$scientific,
      place = site_name,
      lon = longitude_epsg_4326,
      lat = latitude_epsg_4326
    ) |>
    filter(!is.na(lon), !is.na(lat))
}

emperor_mapppd <- get_mapppd_sites(
  "emperor penguin", "Emperor penguin", "Aptenodytes forsteri", "Aptenodytes"
)

adelie_mapppd <- get_mapppd_sites(
  "adelie penguin", "Adélie penguin", "Pygoscelis adeliae", "Pygoscelis"
)

# Compiled breeding locations for the remaining penguins.
# Columns: genus, species, scientific, place, lon, lat (WGS84 degrees).
penguin_sites <- read_csv(
  "penguin_sites.csv",
  col_types = cols(
    genus = col_character(), species = col_character(),
    scientific = col_character(), place = col_character(),
    lon = col_double(), lat = col_double()
  )
)

required_cols <- c("genus", "species", "scientific", "place", "lon", "lat")
if (!all(required_cols %in% names(penguin_sites))) {
  stop("penguin_sites.csv must contain: ", paste(required_cols, collapse = ", "))
}

if (any(penguin_sites$species %in% c("Emperor penguin", "Adélie penguin"))) {
  stop("Emperor and Adélie locations are read from MAPPPD.csv.")
}

all_penguins <- bind_rows(emperor_mapppd, penguin_sites, adelie_mapppd) |>
  filter(!is.na(lon), !is.na(lat))

# Species order and groups
species_order <- c(
  "Adélie penguin",
  "African penguin",
  "Chinstrap penguin",
  "Emperor penguin",
  "Eastern rockhopper penguin",
  "Fiordland penguin",
  "Galápagos penguin",
  "Gentoo penguin",
  "Humboldt penguin",
  "King penguin",
  "Little penguin",
  "Macaroni penguin",
  "Magellanic penguin",
  "Northern rockhopper penguin",
  "Royal penguin",
  "Snares penguin",
  "Erect-crested penguin",
  "Yellow-eyed penguin",
  "White-flippered penguin",
  "Western rockhopper penguin"
)

species_group <- c(
  "Emperor penguin" = "Large penguins",
  "King penguin" = "Large penguins",
  "Adélie penguin" = "Brush-tailed penguins",
  "Chinstrap penguin" = "Brush-tailed penguins",
  "Gentoo penguin" = "Brush-tailed penguins",
  "African penguin" = "Banded penguins",
  "Galápagos penguin" = "Banded penguins",
  "Humboldt penguin" = "Banded penguins",
  "Magellanic penguin" = "Banded penguins",
  "Eastern rockhopper penguin" = "Crested penguins",
  "Western rockhopper penguin" = "Crested penguins",
  "Northern rockhopper penguin" = "Crested penguins",
  "Fiordland penguin" = "Crested penguins",
  "Macaroni penguin" = "Crested penguins",
  "Royal penguin" = "Crested penguins",
  "Snares penguin" = "Crested penguins",
  "Erect-crested penguin" = "Crested penguins",
  "Little penguin" = "Little penguins",
  "White-flippered penguin" = "Little penguins",
  "Yellow-eyed penguin" = "Yellow-eyed penguins"
)

group_plot_order <- c(
  "Large penguins",
  "Brush-tailed penguins",
  "Banded penguins",
  "Crested penguins",
  "Little penguins",
  "Yellow-eyed penguins"
)

# Check names before plotting so no species are silently dropped.
unknown <- setdiff(unique(all_penguins$species), species_order)
missing <- setdiff(species_order, unique(all_penguins$species))
if (length(unknown) > 0) {
  stop("Unrecognized species names: ", paste(unknown, collapse = ", "))
}
if (length(missing) > 0) {
  stop("Missing breeding locations for: ", paste(missing, collapse = ", "))
}

all_penguins <- all_penguins |>
  mutate(
    species = as.character(species),
    species = factor(species, levels = species_order),
    group = species_group[as.character(species)],
    group = factor(group, levels = group_plot_order)
  ) |>
  filter(!is.na(group))

# Cluster nearby breeding sites for display

species_simplify_distance_km <- c(
  "Emperor penguin" = 130,
  "King penguin" = 90,

  "Adélie penguin" = 75,
  "Chinstrap penguin" = 95,
  "Gentoo penguin" = 95,

  "African penguin" = 80,
  "Galápagos penguin" = 250,
  "Humboldt penguin" = 90,
  "Magellanic penguin" = 100,

  "Eastern rockhopper penguin" = 100,
  "Western rockhopper penguin" = 100,
  "Northern rockhopper penguin" = 100,
  "Fiordland penguin" = 140,
  "Macaroni penguin" = 100,
  "Royal penguin" = 100,
  "Snares penguin" = 140,
  "Erect-crested penguin" = 150,

  "Little penguin" = 130,
  "White-flippered penguin" = 130,
  "Yellow-eyed penguin" = 130
)

default_simplify_distance_km <- 90

antarctic_species <- c(
  "Emperor penguin",
  "Adélie penguin",
  "Chinstrap penguin",
  "Gentoo penguin"
)

make_local_aeqd_crs <- function(lon, lat) {

  lon0 <- mean(lon, na.rm = TRUE)
  lat0 <- mean(lat, na.rm = TRUE)

  paste0(
    "+proj=aeqd ",
    "+lat_0=", lat0, " ",
    "+lon_0=", lon0, " ",
    "+datum=WGS84 ",
    "+units=m ",
    "+no_defs"
  )
}

simplify_one_species <- function(x, distance_km, species_name) {

  if (nrow(x) <= 1) {
    return(
      x |>
        mutate(cluster_id = 1)
    )
  }

  if (species_name %in% antarctic_species) {
    target_crs <- 3031
  } else {
    target_crs <- make_local_aeqd_crs(x$lon, x$lat)
  }

  x_sf <- st_as_sf(
    x,
    coords = c("lon", "lat"),
    crs = 4326, remove = FALSE
  ) |>
    st_transform(target_crs)

  xy <- st_coordinates(x_sf)

  cl <- dbscan::dbscan(
    xy,
    eps = distance_km * 1000,
    minPts = 1
  )$cluster

  x |>
    mutate(cluster_id = cl)
}

point_penguins <- all_penguins |>
  mutate(
    species_chr = as.character(species),
    group_chr = as.character(group)
  ) |>
  group_by(species_chr) |>
  group_modify(~{

    this_species <- .y$species_chr[[1]]

    this_distance <- species_simplify_distance_km[this_species]

    if (is.na(this_distance)) {
      this_distance <- default_simplify_distance_km
    }

    simplify_one_species(
      .x,
      distance_km = this_distance,
      species_name = this_species
    )
  }) |>
  ungroup() |>
  group_by(species_chr, group_chr, cluster_id) |>
  summarise(
    genus = first(genus),
    species = first(species_chr),
    scientific = first(scientific),
    group = first(group_chr),
    place = paste(unique(place), collapse = "; "),
    lon = mean(lon, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    n_original_points = n(),
    .groups = "drop"
  ) |>
  mutate(
    species = factor(species, levels = species_order),
    group = factor(group, levels = group_plot_order)
  )

# Emperor penguin coastline line segments

coast_segment_vertices <- 18
coast_vertex_spacing_km <- 25
emperor_keep_distance_km <- 220

antarctica_coastline_raw <- antarctica_land |>
  st_boundary() |>
  st_collection_extract("LINESTRING") |>
  st_make_valid()

antarctica_coastline_3031 <- antarctica_coastline_raw |>
  st_transform(3031) |>
  st_cast("LINESTRING")

emperor_sites_3031 <- emperor_mapppd |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326, remove = FALSE
  ) |>
  st_transform(3031)

antarctica_coastline_dense_3031 <- antarctica_coastline_3031 |>
  st_segmentize(dfMaxLength = coast_vertex_spacing_km * 1000) |>
  st_cast("LINESTRING")

make_segments_from_linestring <- function(line_geom, vertices_per_segment = 18) {

  xy <- st_coordinates(line_geom)[, c("X", "Y"), drop = FALSE]

  if (nrow(xy) < 2) {
    return(NULL)
  }

  starts <- seq(1, nrow(xy) - 1, by = vertices_per_segment - 1)

  segments <- lapply(starts, function(s) {

    e <- min(s + vertices_per_segment - 1, nrow(xy))

    if ((e - s + 1) < 2) {
      return(NULL)
    }

    st_linestring(xy[s:e, , drop = FALSE])
  })

  segments <- segments[!vapply(segments, is.null, logical(1))]

  if (length(segments) == 0) {
    return(NULL)
  }

  st_sfc(segments, crs = st_crs(line_geom))
}

coast_segment_sfc_list <- lapply(
  st_geometry(antarctica_coastline_dense_3031),
  make_segments_from_linestring,
  vertices_per_segment = coast_segment_vertices
)

coast_segment_sfc_list <- coast_segment_sfc_list[
  !vapply(coast_segment_sfc_list, is.null, logical(1))
]

antarctica_coast_segments_3031 <- st_sf(
  coast_id = seq_along(do.call(c, coast_segment_sfc_list)),
  geometry = do.call(c, coast_segment_sfc_list)
)

st_crs(antarctica_coast_segments_3031) <- st_crs(antarctica_coastline_dense_3031)

nearest_emperor_distance_m <- st_distance(
  antarctica_coast_segments_3031,
  emperor_sites_3031
) |>
  apply(1, min)

emperor_coastline_segments <- antarctica_coast_segments_3031 |>
  mutate(
    nearest_emperor_distance_km = as.numeric(nearest_emperor_distance_m) / 1000,
    species = "Emperor penguin"
  ) |>
  filter(nearest_emperor_distance_km <= emperor_keep_distance_km) |>
  st_transform(4326)

# Separate overlapping symbols for display only

make_plot_offsets <- function(
    x,
    overlap_distance_km = 95, default_offset_radius_km = 75
) {

  x2 <- x |>
    mutate(
      lat_rad = lat * pi / 180,
      x_km = lon * 111.32 * pmax(cos(lat_rad), 0.25),
      y_km = lat * 110.57
    )

  cl <- dbscan::dbscan(
    as.matrix(x2[, c("x_km", "y_km")]),
    eps = overlap_distance_km,
    minPts = 1
  )$cluster

  x2 <- x2 |>
    mutate(overlap_cluster = cl) |>
    group_by(overlap_cluster) |>
    arrange(group, species, .by_group = TRUE) |>
    mutate(
      n_overlap = n(),
      overlap_i = row_number(),

      offset_radius_km = case_when(
        lon >= 160 & lon <= 180 & lat >= -58 & lat <= -35 ~ 130,
        lon >= -78 & lon <= -55 & lat >= -58 & lat <= -45 ~ 125,
        lon >= -75 & lon <= -40 & lat >= -70 & lat <= -58 ~ 110,
        lat <= -60 ~ 95,
        TRUE ~ default_offset_radius_km
      ),

      offset_angle = 2 * pi * (overlap_i - 1) / n_overlap + pi / 2,

      lon_offset = ifelse(
        n_overlap > 1,
        offset_radius_km * cos(offset_angle) / (111.32 * pmax(cos(lat_rad), 0.25)),
        0
      ),

      lat_offset = ifelse(
        n_overlap > 1,
        offset_radius_km * sin(offset_angle) / 110.57,
        0
      ),

      plot_lon = lon + lon_offset,
      plot_lat = lat + lat_offset
    ) |>
    ungroup() |>
    select(
      -lat_rad,
      -x_km,
      -y_km,
      -overlap_cluster,
      -n_overlap,
      -overlap_i,
      -offset_radius_km,
      -offset_angle,
      -lon_offset,
      -lat_offset
    )

  x2
}

point_penguins_plot <- point_penguins |>
  filter(as.character(species) != "Emperor penguin") |>
  make_plot_offsets(
    overlap_distance_km = 130, default_offset_radius_km = 115
  ) |>
  mutate(
    group = factor(group, levels = group_plot_order),
    species = factor(species, levels = species_order)
  ) |>
  arrange(group, species)

# Group colors
group_cols <- c(
  "Large penguins" = "red",
  "Brush-tailed penguins" = "black",
  "Banded penguins" = "forestgreen",
  "Crested penguins" = "darkorchid3",
  "Little penguins" = "dodgerblue3",
  "Yellow-eyed penguins" = "darkorange1"
)

penguin_cols <- setNames(
  unname(group_cols[species_group[species_order]]),
  species_order
)

penguin_shapes <- c(
  "Emperor penguin" = NA,
  "King penguin" = 1,

  "Adélie penguin" = 0,
  "Chinstrap penguin" = 1,
  "Gentoo penguin" = 2,

  "African penguin" = 0,
  "Galápagos penguin" = 1,
  "Humboldt penguin" = 2,
  "Magellanic penguin" = 5,

  "Eastern rockhopper penguin" = 0,
  "Western rockhopper penguin" = 1,
  "Northern rockhopper penguin" = 2,
  "Fiordland penguin" = 3,
  "Macaroni penguin" = 4,
  "Royal penguin" = 5,
  "Snares penguin" = 6,
  "Erect-crested penguin" = 17,

  "Little penguin" = 1,
  "White-flippered penguin" = 2,
  "Yellow-eyed penguin" = 5
)

# Shared positions for the place key and species legend
bottom_col_x <- c(0.30, 3.00, 5.70, 8.95)
bottom_symbol_offset <- 0.13
bottom_text_offset <- 0.50

bottom_symbol_x <- bottom_col_x + bottom_symbol_offset
bottom_text_x <- bottom_col_x + bottom_text_offset

# Labels
label_size_common <- 5

continent_labels <- data.frame(
  label = c(
    "North America",
    "South America",
    "Europe",
    "Africa",
    "Asia",
    "Australia",
    "Antarctica"
  ),
  lon = c(-98, -57, 15, 22, 82, 135, 30),
  lat = c(47, -20, 50.5, -15, 36, -25, -78)
)

country_labels <- data.frame(
  label = c("South Africa"),
  lon = c(24),
  lat = c(-29)
)

# Numbered place callouts

callout_lon_offset <- 5.2
callout_lat_offset <- 5.2

site_callouts <- data.frame(
  id = 1:19,
  label = c(
    "Falkland Islands",
    "South Georgia",
    "Galápagos Islands",
    "Crozet Islands",
    "Kerguelen Islands",
    "Heard Island",
    "Macquarie Island",
    "Tristan da Cunha",
    "Snares Islands",
    "Bouvet Island",
    "South Sandwich Islands",
    "Peter I Island",
    "Balleny Islands",
    "New Zealand",
    "South Orkney Islands",
    "Amsterdam & St Paul Islands",
    "Gough Island",
    "Prince Edward & Marion Islands",
    "Antarctic Peninsula"
  ),
  lon = c(
    -59.5, -36.7, -91.3, 51.8, 69.3,
    73.5, 158.9, -12.3, 166.6, 3.4,
    -26.3, -90.6, 163.0, 172.0, -45.5,
    77.5, -9.9, 37.8,
    -63.0
  ),
  lat = c(
    -51.8, -54.3, -0.4, -46.4, -49.3,
    -53.1, -54.6, -37.1, -48.0, -54.4,
    -57.8, -68.8, -66.9, -41.5, -60.6,
    -38.7, -40.3, -46.8,
    -65.0
  ),
  callout_direction = c(
    "upper_right",
    "upper_right",
    "lower_left",
    "upper_left",
    "upper_right",
    "lower_right",
    "upper_left",
    "upper_left",
    "upper_left",
    "upper_right",
    "upper_right",
    "upper_left",
    "upper_left",
    "upper_left",
    "lower_right",
    "upper_right",
    "lower_left",
    "upper_left",
    "lower_right"
  )
) |>
  mutate(
    label_lon = lon + if_else(
      callout_direction %in% c("upper_right", "lower_right"),
      callout_lon_offset, -callout_lon_offset
    ),
    label_lat = lat + if_else(
      callout_direction %in% c("upper_right", "upper_left"),
      callout_lat_offset, -callout_lat_offset
    )
  )

lon_labels <- function(x) {
  ifelse(
    x < 0, paste0(abs(x), "°W"),
    ifelse(x > 0, paste0(x, "°E"), "0°")
  )
}

lat_labels <- function(y) {
  ifelse(
    y < 0, paste0(abs(y), "°S"),
    ifelse(y > 0, paste0(y, "°N"), "0°")
  )
}

# Main map
p_map <- ggplot() +
  theme_void() +
  geom_sf(
    data = world_land,
    fill = "lightyellow", color = NA
  ) +
  geom_sf(
    data = world_coast,
    color = "lightyellow", linewidth = 0.32
  ) +
  geom_hline(
    yintercept = 0, linetype = "dashed", linewidth = 0.45, color = "gray25"
  ) +
  annotate(
    "text",
    x = -33, y = 3, label = "Equator", hjust = 0.5, size = label_size_common,
    color = "gray20"
  ) +
  geom_text(
    data = continent_labels,
    aes(x = lon, y = lat, label = label),
    color = "gray35", size = 5.1, fontface = "bold"
  ) +
  geom_text(
    data = country_labels,
    aes(x = lon, y = lat, label = label),
    color = "gray35", size = label_size_common
  ) +
  geom_segment(
    data = site_callouts,
    aes(x = label_lon, y = label_lat, xend = lon, yend = lat),
    color = "gray20", linewidth = 0.5, alpha = 0.65, lineend = "round"
  ) +
  geom_sf(
    data = emperor_coastline_segments,
    color = unname(penguin_cols["Emperor penguin"]),
    linewidth = 1, alpha = 1, lineend = "round", linejoin = "round", show.legend = FALSE
  ) +
  geom_point(
    data = point_penguins_plot,
    aes(x = plot_lon, y = plot_lat, color = species, shape = species),
    fill = NA, size = 2.8, stroke = 1.05, alpha = 0.98, show.legend = FALSE
  ) +
  geom_label(
    data = site_callouts,
    aes(x = label_lon, y = label_lat, label = id),
    size = label_size_common, fontface = "bold", color = "gray15", fill = "white",
    linewidth = 0.28, label.r = unit(0.06, "lines"), label.padding = unit(0.11, "lines")
  ) +
  scale_color_manual(values = penguin_cols, breaks = species_order, name = NULL) +
  scale_shape_manual(values = penguin_shapes, breaks = species_order, name = NULL) +
  scale_x_continuous(
    breaks = seq(-100, 160, by = 40),
    labels = lon_labels,
    sec.axis = dup_axis(
      breaks = seq(-100, 160, by = 40),
      labels = NULL, name = NULL
    )
  ) +
  scale_y_continuous(
    breaks = c(-60, -30, 0),
    labels = lat_labels,
    sec.axis = dup_axis(
      breaks = c(-60, -30, 0),
      labels = NULL, name = NULL
    )
  ) +
  coord_sf(
    xlim = c(-110, 180),
    ylim = c(-85, 6),
    expand = FALSE, clip = "on",
    default_crs = sf::st_crs(4326)
  ) +
  labs(
    title = "Global breeding distribution of penguins", x = NULL, y = NULL
  ) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "aliceblue", color = NA),
    panel.border = element_rect(
      fill = NA, color = "gray20", linewidth = 0.45
    ),
    plot.title = ggtext::element_markdown(
      face = "bold", size = 21, margin = margin(b = 8)
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x.bottom = element_text(
      size = 11, color = "gray20", margin = margin(t = 5)
    ),
    axis.text.x.top = element_blank(),
    axis.text.y.left = element_text(
      size = 11, color = "gray20", margin = margin(r = 5)
    ),
    axis.text.y.right = element_blank(),
    axis.ticks = element_line(color = "gray20", linewidth = 0.35),
    axis.ticks.x.bottom = element_line(color = "gray20", linewidth = 0.35),
    axis.ticks.x.top = element_line(color = "gray20", linewidth = 0.35),
    axis.ticks.y.left = element_line(color = "gray20", linewidth = 0.35),
    axis.ticks.y.right = element_line(color = "gray20", linewidth = 0.35),
    axis.ticks.length = unit(4, "pt"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(10, 12, 4, 12)
  )

# Custom numbered place key
site_key_df <- site_callouts |>
  mutate(
    key_col = floor((id - 1) / 5) + 1,
    key_row = ((id - 1) %% 5) + 1,
    key_symbol_x = bottom_symbol_x[key_col],
    key_text_x = bottom_text_x[key_col],
    key_y = 5.75 - key_row
  )

p_site_key <- ggplot() +
  theme_void() +
  geom_label(
    data = site_key_df,
    aes(x = key_symbol_x, y = key_y, label = id),
    size = label_size_common, fontface = "bold", color = "gray15", fill = "white",
    linewidth = 0.28, label.r = unit(0.06, "lines"), label.padding = unit(0.10, "lines")
  ) +
  geom_text(
    data = site_key_df,
    aes(x = key_text_x, y = key_y, label = label),
    hjust = 0, vjust = 0.5, size = label_size_common, color = "gray25"
  ) +
  coord_cartesian(
    xlim = c(0, 11.9),
    ylim = c(0.25, 4.95),
    expand = FALSE, clip = "off"
  ) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(2, 12, 8, 12)
  )

# Custom grouped species legend
legend_points <- data.frame(
  group = c(
    "Large penguins",
    "Brush-tailed penguins", "Brush-tailed penguins", "Brush-tailed penguins",
    "Banded penguins", "Banded penguins", "Banded penguins", "Banded penguins",
    "Crested penguins", "Crested penguins", "Crested penguins", "Crested penguins",
    "Crested penguins", "Crested penguins", "Crested penguins", "Crested penguins",
    "Little penguins", "Little penguins",
    "Yellow-eyed penguins"
  ),
  species = c(
    "King penguin",
    "Adélie penguin", "Chinstrap penguin", "Gentoo penguin",
    "African penguin", "Galápagos penguin", "Humboldt penguin", "Magellanic penguin",
    "Eastern rockhopper penguin", "Western rockhopper penguin", "Northern rockhopper penguin", "Fiordland penguin",
    "Macaroni penguin", "Royal penguin", "Snares penguin", "Erect-crested penguin",
    "Little penguin", "White-flippered penguin",
    "Yellow-eyed penguin"
  ),
  col_id = c(
    1,
    1, 1, 1,
    2, 2, 2, 2,
    3, 3, 3, 3, 3, 3, 3, 3,
    4, 4,
    4
  ),
  y = c(
    5.15,
    3.72, 3.02, 2.32,
    5.85, 5.15, 4.45, 3.75,
    5.85, 5.15, 4.45, 3.75, 3.05, 2.35, 1.65, 0.95,
    5.85, 5.15,
    3.72
  )
)

legend_points <- legend_points |>
  mutate(
    species = factor(species, levels = species_order),
    group = factor(group, levels = group_plot_order),
    symbol_x = bottom_symbol_x[col_id],
    text_x = bottom_text_x[col_id],
    color = unname(penguin_cols[as.character(species)]),
    shape = unname(penguin_shapes[as.character(species)])
  ) |>
  arrange(group, species)

legend_emperor_line <- data.frame(
  species = "Emperor penguin", col_id = 1,
  x = bottom_symbol_x[1] - 0.13,
  xend = bottom_symbol_x[1] + 0.13,
  y = 5.85, yend = 5.85
)

legend_headers <- data.frame(
  label = c(
    "Large penguins",
    "Brush-tailed penguins",
    "Banded penguins",
    "Crested penguins",
    "Little penguins",
    "Yellow-eyed penguins"
  ),
  col_id = c(1, 1, 2, 3, 4, 4),
  y = c(6.50, 4.38, 6.50, 6.50, 6.50, 4.38)
) |>
  mutate(
    label = factor(label, levels = group_plot_order),
    x = bottom_col_x[col_id],
    group_color = unname(group_cols[as.character(label)])
  )

p_legend <- ggplot() +
  theme_void() +
  geom_text(
    data = legend_headers,
    aes(x = x, y = y, label = label, color = I(group_color)),
    hjust = 0, size = label_size_common, fontface = "bold"
  ) +
  geom_segment(
    data = legend_emperor_line,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = unname(penguin_cols["Emperor penguin"]),
    linewidth = 1.35, lineend = "round"
  ) +
  geom_text(
    data = legend_emperor_line,
    aes(x = bottom_text_x[1], y = y, label = "Emperor"),
    hjust = 0, vjust = 0.5, size = label_size_common,
    color = unname(penguin_cols["Emperor penguin"])
  ) +
  geom_point(
    data = legend_points,
    aes(x = symbol_x, y = y, color = species, shape = species),
    size = 3.15, stroke = 1.05, fill = NA
  ) +
  geom_text(
    data = legend_points,
    aes(
      x = text_x,
      y = y,
      label = gsub(" penguin", "", as.character(species)),
      color = species
    ),
    hjust = 0, vjust = 0.5, size = label_size_common
  ) +
  scale_color_manual(
    values = penguin_cols,
    breaks = species_order,
    guide = "none"
  ) +
  scale_shape_manual(
    values = penguin_shapes,
    breaks = species_order,
    guide = "none"
  ) +
  coord_cartesian(
    xlim = c(0, 11.9),
    ylim = c(0.65, 6.75),
    expand = FALSE, clip = "off"
  ) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 12, 8, 12)
  )

# Assemble the figure
draw_penguin_distribution <- function() {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(
    nrow = 4, ncol = 1,
    heights = unit(c(0.58, 0.17, 0.025, 0.225), "npc")
  )))

  print(p_map, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p_site_key, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(p_legend, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))

  popViewport()
}

if (interactive()) draw_penguin_distribution()

# Save the figure
grDevices::pdf(
  "global_penguin_breeding_distribution.pdf",
  width = 15.33, height = 11.76, useDingbats = FALSE
)
draw_penguin_distribution()
grDevices::dev.off()

svglite::svglite(
  "global_penguin_breeding_distribution.svg",
  width = 15.33, height = 11.76
)
draw_penguin_distribution()
grDevices::dev.off()
