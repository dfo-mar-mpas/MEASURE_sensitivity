require(sf)
require(arcpullr)
require(readxl)
require(rvest)
require(xml2)
require(purrr)
require(stringr)
require(dplyr)
require(tidyr)


gullyzones <- get_spatial_layer(
  "https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/oceans_act_marine_protected_areas_zones_de_protection_marines_de_la_loi_sur_les_oceans/MapServer/0",
  where = "NAME_E LIKE '%Gully%'"
)


#bubblegum coral from https://data.mendeley.com/datasets/bzpzzw9tdc/1
download.file("https://data.mendeley.com/public-files/datasets/bzpzzw9tdc/files/7062b4f9-caea-4b8b-8bcd-c67521cdb835/file_downloaded",
              "data/Response_data_OccurrenceParagorgia_PA.csv")
bubble <- read.csv("data/Response_data_OccurrenceParagorgia_PA.csv") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)


# pull data from https://data.mendeley.com/datasets/z3v6jdnwh6/1
download.file(
  "https://data.mendeley.com/public-files/datasets/z3v6jdnwh6/files/4ce7d9ba-046f-4259-82ca-7fd32bff1d82/file_downloaded",
  "data/Gully_MPA_Photo_Archive_Metadata.xlsx", mode = "wb")
photos <- readxl::read_xlsx("data/Gully_MPA_Photo_Archive_Metadata.xlsx") |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

download.file(
  "https://data.mendeley.com/public-files/datasets/z3v6jdnwh6/files/b23d2b99-13d2-4587-8640-aa391fec7790/file_downloaded",
  "data/Gully_MPA_Video_Transect_Metadata.xlsx", mode = "wb")
videos <- readxl::read_xlsx("data/Gully_MPA_Video_Transect_Metadata.xlsx") |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)


# NOAA coral data from https://noaa.maps.arcgis.com/home/item.html?id=f465861aecac410980a7c601cfec7850#overview
noaa <- read.csv("data/DSCRTP_NatDB_20260416-1.csv")
testnoaa <- noaa |>
  filter(Country=="Canada") |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) |>
  st_filter(st_buffer(gullyzones,dist=10^6)) # filter to within 1000 km of Gully MPA


#Claude magic:
# -----------------------------------------------------------------------------
# Scrape the Gully MPA "New protection measures" (Fisheries Act variation order)
# groundfish closure polygons from the DFO web page and build an sf object.
#
# Source: https://www.dfo-mpo.gc.ca/oceans/mpa-zpm/gully/index-eng.html
# Page notes coordinates are NAD83 (EPSG:4269), degrees-minutes-seconds, and that
# the polygons are "enclosed by rhumb lines joining the following points in the
# order in which they are listed" (4VS and 4W portions).
# -----------------------------------------------------------------------------

url <- "https://www.dfo-mpo.gc.ca/oceans/mpa-zpm/gully/index-eng.html"

# --- helpers -----------------------------------------------------------------

# "43°53'13”N" -> 43.88694 ; "58°51'52”W" -> -58.86444
# Regex is deliberately tolerant of the odd quote/degree characters on the page
dms_to_dd <- function(x) {
  x <- str_squish(x)
  m <- str_match(x, "(\\d+)\\D+(\\d+)\\D+(\\d+(?:\\.\\d+)?)\\D*([NSEWnsew])")
  bad <- is.na(m[, 1])
  if (any(bad)) {
    stop("Could not parse DMS value(s): ", paste(x[bad], collapse = "; "))
  }
  dd <- as.numeric(m[, 2]) + as.numeric(m[, 3]) / 60 + as.numeric(m[, 4]) / 3600
  ifelse(toupper(m[, 5]) %in% c("S", "W"), -dd, dd)
}

# Find the nearest preceding paragraph/heading mentioning "Division XXX"
# (e.g. "The portion of Division 4VS enclosed by rhumb lines ...")
label_for_table <- function(node, i) {
  ctx <- xml_find_first(
    node,
    paste0(
      "preceding::*[(self::p or self::h2 or self::h3 or self::h4 or self::li)",
      " and contains(., 'Division')][1]"
    )
  )
  txt <- if (inherits(ctx, "xml_missing")) NA_character_ else xml_text(ctx)
  div <- str_match(txt, "Division\\s+(\\w+)")[, 2]
  if (is.na(div)) paste0("polygon_", i) else div
}

# One coordinate table -> sfg POLYGON (lon/lat), ring closed if needed
table_to_polygon <- function(d) {
  names(d) <- tolower(names(d))
  xy <- cbind(lon = dms_to_dd(d$longitude), lat = dms_to_dd(d$latitude))
  if (!isTRUE(all.equal(unname(xy[1, ]), unname(xy[nrow(xy), ])))) {
    xy <- rbind(xy, xy[1, ])
  }
  st_polygon(list(xy))
}

# --- scrape ------------------------------------------------------------------

# If the site ever rejects the default user agent, swap for:
# page <- session(url, httr::user_agent("Mozilla/5.0")) |> read_html()
page <- read_html(url)

tbl_nodes <- html_elements(page, "table")
tbls <- html_table(tbl_nodes)

# keep only tables that look like coordinate tables (Point / Latitude / Longitude)
keep <- map_lgl(tbls, \(d) {
  all(c("latitude", "longitude") %in% tolower(names(d)))
})
if (!any(keep)) {
  stop("No Latitude/Longitude tables found - page layout may have changed.")
}

tbl_nodes <- tbl_nodes[keep]
tbls <- tbls[keep]

labels <- map2_chr(as.list(tbl_nodes), seq_along(tbl_nodes), label_for_table)
polys <- map(tbls, table_to_polygon)

# --- build sf ----------------------------------------------------------------

gully_vo <- st_sf(
  Variation_Order = paste("Portion of", labels),
  n_vertices = map_int(polys, \(p) nrow(p[[1]]) - 1L), # excludes closing vertex
  source_url = url,
  date_scraped = Sys.Date(),
  geometry = st_sfc(polys, crs = 4269) # NAD83 geographic
) |>
  st_transform(st_crs(gullyzones))

coral_species <- c(
  "SEA CORALS (NS)",
  "SOFT CORAL UNIDENTIFIED",
  "ANTHOZOA",
  "LOPHELIA PERTUSA",
  "PARAMURICEA SPP",
  "PARAGORGIA ARBOREA",
  "KERATOISIS ORNATA",
  "CHRYSOGORGIA AGASSIZII",
  "PRIMNOA RESEDAEFORMIS",
  "FLABELLUM ALABASTRUM",
  "FLABELLUM SP",
  "CARYOPHYLLIIDAE F.",
  "ANTIPATHARIAN O.",
  "ANTHOMASTUS GRANDIFLORUS"
)

# rv from https://open.canada.ca/data/en/dataset/1366e1f1-e2c8-4905-89ae-e10f1be0a164
rv <- read.csv("data/SUMMER_2023_GSCAT.csv") |>
  left_join(read.csv("data/SUMMER_2023_GSINF.csv"),by = join_by(MISSION, SETNO)) |>
  left_join(read.csv("data/SUMMER_2023_GSSPECIES.csv"), by = c("SPEC"="CODE")) |>
  filter(SCI_NAME %in% coral_species) |>
  st_as_sf(coords = c("SLONG", "SLAT"), crs = 4326, remove = FALSE)

plot(gullyzones$geoms, col = NA, border = "black", lwd = 2, main = "Gully MPA and Groundfish Closure Polygons")
plot(gully_vo$geometry, col = "lightblue", border = "darkblue", add=T)
plot(testnoaa$geometry, col = "red", pch = 20, add = TRUE)
plot(bubble$geometry, col = "orange", border = "darkorange",add=T)
plot(photos$geometry, col = "green", pch = 20, add = TRUE)
plot(videos$geometry, col = "purple", pch = 20, add = TRUE)

plot(gullyzones$geoms, col = NA, border = "black", lwd = 2)
plot(rv$geometry, col = "yellow", pch = 20, add = TRUE)


#### Assign species to video and photo data

knowncoralbubbles <- bind_rows(
  rv |>
    mutate(sci_name=str_to_sentence(tolower(SCI_NAME)),
           date = SDATE,
           source = "RV") |>
    select(sci_name, date, source),

  bubble |>
    select(pa) |>
    filter(pa==1)  |>
    mutate(sci_name= "Paragorgia arborea",
           date = NA,
           source = "bubble") |>
    select(-pa),
  testnoaa |>
    mutate(sci_name=ScientificName,
           date = ObservationDate ,
           source = "NOAA") |>
    select(sci_name, date, source)

) |>
  st_buffer(dist = 100)


plot(gully_vo$geometry, col = "lightblue", border = "darkblue", main = "Gully MPA and Groundfish Closure Polygons")
plot(knowncoralbubbles$geometry, col = "red", pch = 20,add=T)



names(rv)
names(testnoaa)
names(bubble)

plot(gullyzones$geoms,
     col = NA,
     border = "black",
     lwd = 1)

plot(gully_vo$geometry,
     col = "lightblue",
     border = "darkblue",
     add = TRUE)

plot(knowncoralbubbles |>
       filter(sci_name != "Paragorgia arborea") |>
       st_geometry(),
     col = "green",
     cex = 1.5,
     pch = 20,
     add = TRUE)

plot(temp$geometry,
     col = "purple",
     pch = 20,
     add = TRUE)


##------------------------------
##  Calculating Occurrences
##------------------------------



# Finding RV coral occurence

total_rv <- read.csv("data/SUMMER_2023_GSCAT.csv") |>
  left_join(read.csv("data/SUMMER_2023_GSINF.csv"),by = join_by(MISSION, SETNO)) |>
  left_join(read.csv("data/SUMMER_2023_GSSPECIES.csv"), by = c("SPEC"="CODE")) |>
  st_as_sf(coords = c("SLONG", "SLAT"), crs = 4326, remove = FALSE)


rv <- read.csv("data/SUMMER_2023_GSCAT.csv") |>
  left_join(read.csv("data/SUMMER_2023_GSINF.csv"),by = join_by(MISSION, SETNO)) |>
  left_join(read.csv("data/SUMMER_2023_GSSPECIES.csv"), by = c("SPEC"="CODE")) |>
  filter(SCI_NAME %in% coral_species) |>
  st_as_sf(coords = c("SLONG", "SLAT"), crs = 4326, remove = FALSE)

rv_coral_occurrence <- nrow(rv) / nrow(total_rv)


# Finding bubblegum occurrence

total_bubble <- read.csv("data/Response_data_OccurrenceParagorgia_PA.csv") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

bubble <-  total_bubble |>
  select(pa) |>
  filter(pa==1)

bg_coral_occurrence <- nrow(bubble) / nrow(total_bubble)





TEST <- st_join(photos, knowncoralbubbles, left = FALSE) %>%   #Finding what photos fall in a known corals buffer
  st_drop_geometry() %>%
  count(ID, sci_name, name = "coral_buffer_count")


mega <- cross_join(
  data.frame(sci_name = unique(knowncoralbubbles$sci_name)),     #Creates a df where every photo is associated with every distinct species
  photos
)

mega2 <- left_join(mega, TEST, by = c("ID", "sci_name")) %>%
  mutate(coral_buffer_multiplier = ifelse(is.na(coral_buffer_count), 1, 2)) %>%
  select(ID, sci_name, coral_buffer_multiplier)

temp <- photos %>%
  st_join(gullyzones) %>%
  st_join(gully_vo) %>%
  right_join(mega2, by = "ID") %>%
  mutate(zone_multiplier = case_when(!is.na(Variation_Order) ~ 10,
                                     ZONE_E == "Zone 1" ~ 8,
                                     ZONE_E == "Zone 2" ~ 3,
                                     .default = 1),

         pa = runif(n()) < bg_coral_occurrence/100*zone_multiplier*coral_buffer_multiplier) %>%
  dplyr::select(names(photos), zone_multiplier, pa, Variation_Order, ZONE_E, coral_buffer_multiplier, sci_name) %>%
  mutate(density = ifelse(!is.na(Gear), runif(n(), 0, 5)*pa*zone_multiplier, NA),    #this is in corals per meter sq     #originally was set to on;y run if the gear was ropos but messes with thing is there is no ROPOS data
         size = ifelse(!is.na(Gear), runif(n(), 0, 20)*pa*zone_multiplier, NA),     #this is in cm
         per_colony_living = case_when(Year < 2004 & (ZONE_E == "Zone 1" | Variation_Order == "Portion of 4W") ~ rbeta(n(), 17, 3),    # This is the % living of a single colony----Self protected in canyons   (85%)
                                       Year >= 2004 & (ZONE_E == "Zone 1") ~ rbeta(n(), 19, 1),                                        # Once protections kick in for zone 1      (95%)
                                       Year >= 2004 & ZONE_E == "Zone 2" & is.na(Variation_Order) ~ rbeta(n(), 18, 2),                 # Ignoring VOs, protection kicks in for Zone 2 but its less than zone 1   (90%)
                                       Year >= 2004 & (ZONE_E == "Zone 3") ~ rbeta(n(), 15, 5),                                        # Protection kicks in for zone 3   (75%)
                                       Year >= 2019 & !is.na(Variation_Order) ~ rbeta(n(), 18, 2),                                     # extra protection kicks in once VOs are established    (75%)
                                       Year >= 2004 & Year <= 2019 & !is.na(Variation_Order) ~ rbeta(n(), 18, 2),                      # during this time VOs act as zone 2    (90%)
                                       .default = rbeta(n(), 10, 10), ),
         zoa_growth = runif(n(), 0, 1 - per_colony_living)
)


# 1. Before and after establishment (2004)
ggplot(
  temp %>%
    filter(ZONE_E == "Zone 1") %>%
    mutate(Period = if_else(Year < 2004, "Before 2004", "2004 onward")),
  aes(x = Period, y = per_colony_living)
) +
  geom_boxplot() +
  labs(x = NULL, y = "Live coral per colony",
       title = "Live Coral Before and After Establishment") +
  theme_minimal()


# 2. Before establishment, between establishment and Variation Order,
#    and after the Variation Order (2019)
ggplot(
  temp %>%
    filter(!is.na(Variation_Order)) %>%
    mutate(Period = case_when(
      Year < 2004 ~ "Before 2004",
      Year < 2019 ~ "2004–2018",
      TRUE ~ "2019 onward"
    )),
  aes(x = Period, y = per_colony_living)
) +
  geom_boxplot() +
  labs(x = NULL, y = "Live coral per colony",
       title = "Live Coral Across Protection Periods") +
  theme_minimal()


#-------------------------------
#   SCORING
#-------------------------------

#--------------------------------------------
# Scoring for Indicator 2 (coral diversity)
#-------------------------------------------

library(vegan)

hist_wide_corals <- temp %>%
  filter(Year != max(Year)) %>%
  mutate(numeric_pa = as.numeric(pa)) %>%
  st_drop_geometry() %>%
  pivot_wider(id_cols = ID, names_from = sci_name, values_from = numeric_pa)               #changing from long format to wide format

# Calculate sample-based species accumulation curve
sp_accum <- specaccum(as.matrix(wide_corals[, -1]), method = "random") # or method = "random"

# Plot the curve
plot(sp_accum, xlab = "Number of Sites (Sample Effort)", ylab = "Species Richness")

n_samples <- temp %>% filter(Year == max(Year)) %>% pull(ID) %>% unique() %>% length()      # get the unique number of samples
richness_est <- predict(sp_accum, newdata = n_samples)                                      # get the richness estimate based on the number of samples
sd_est <- approx(x = sp_accum$sites, y = sp_accum$sd, xout = n_samples)$y                   # the standard deviation estimate
S <- temp %>% filter(Year == max(Year), pa) %>% pull(sci_name) %>% unique() %>% length()    # Calculate observed Species richness

lower_95 <- richness_est - (1.96 * sd_est)                          #calculate the lower 95% confidence limit
score <- as.numeric(S > lower_95) * 100                             #calculate final score



#---------------------------------------
#Scoring for Indicator 1 (Distribution)
#---------------------------------------

coral_filtered <- temp %>%
  filter(
    !(!is.na(Variation_Order) & pa == TRUE &
        as.numeric(Year) == 2011)
  )

# Keep unique coral observations
coral_obs <- temp |>
  filter(pa == TRUE) |>
  distinct(ID, sci_name, .keep_all = TRUE)



# Calculate coverage by conservation area
coverage <- coral_obs |>
  sf::st_drop_geometry() |>
  summarise(
    coverage = n_distinct(ID[ZONE_E == "Zone 1"]) /
      n_distinct(ID)

  )

coverage <- coverage |>
  mutate(
    score = ifelse(coverage > 0.8, 100, coverage/0.8*100)  # if the coverage is above the min it gets 100%, if its below if gets the proportion of
  )

#---------------------------------------
#Scoring for Indicator 1 (Density)
#---------------------------------------

# temp_density <- temp %>%
#   rename(year_of_data_collection = Year) %>%
#   dplyr::select(geometry, year_of_data_collection, density,) %>%
#   filter(year_of_data_collection >= 2004)

# Assess linear trend in density over time
#density_trend <- lm(density ~ year_of_data_collection, data = temp_density)     #this type of scoring assumes multiple years of data-maybe we shouldnt...

# Extract p-value and slope
#p_value <- summary(density_trend)$coefficients[2, 4]
#slope <- coef(density_trend)[2]

# Assign score
#density_score <- if (p_value >= 0.05 | slope > 0) {
 # 100
#} else {
 # 0
#}

mpas <- MPAs %>%
  filter(NAME_E == "Gully Marine Protected Area")


#Create 1 km grid cell
# Transform to projected CRS
temp <- st_transform(temp, 32620)
mpas <- st_transform(mpas, 32620)

# Combine MPA polygons
mpa_union <- st_union(mpas)

# Create 1 km grid and clip to MPAs
grid <- st_make_grid(mpa_union, cellsize = 1000, square = TRUE) |>
  st_as_sf() |>
  mutate(grid_id = row_number()) |>
  st_intersection(mpa_union)


# Assign observations to grid cells
temp_grid <- st_join(temp, grid, join = st_within)

# Count unique sampling events per grid cell
grid_counts <- temp_grid |>
  st_drop_geometry() |>
  group_by(grid_id) |>
  summarise(
    n_samples = n_distinct(paste(Mission, Event)),
    .groups = "drop"
  )

# Retain cells with more than 3 sampling events
valid_grid <- grid_counts |>
  filter(n_samples >= 3) |>
  pull(grid_id)

# Filter observations
coral_filtered <- temp_grid |>
  filter(grid_id %in% valid_grid,
         pa) %>%
  mutate(Year = as.numeric(as.character(Year)))


# #Linear model accounting for spatial location
# density_model <- lm(
#   density ~ Year + factor(grid_id),
#   data = coral_filtered
# )
#
# # ANOVA
# anova(density_model)


# # Spatial model only
# model_spatial <- lm(
#   density ~ factor(grid_id),
#   data = coral_filtered
# )

# Spatial + temporal model
model_spatiotemporal <- lm(
  density ~ factor(grid_id) + Year,
  data = coral_filtered
 )
#
# # Test contribution of year
# anova(model_spatial, model_spatiotemporal)


slope_year <- coef(model_spatiotemporal)["Year"]
p_value <- summary(model_spatiotemporal)$coefficients["Year", "Pr(>|t|)"]

# Determine significance
significant_trend <- p_value < 0.05

#Assign score
density_score <- if (p_value >= 0.05 | slope_year > 0) {
100
} else {
0
}

#------------------------------------------
#Scoring for Indicator 1 (Size structure)
#------------------------------------------

temp <- st_transform(temp, 32620)
mpas <- st_transform(mpas, 32620)

# Combine MPA polygons
mpa_union <- st_union(mpas)

# Create 1 km grid and clip to MPAs
grid <- st_make_grid(mpa_union, cellsize = 1000, square = TRUE) |>
  st_as_sf() |>
  mutate(grid_id = row_number()) |>
  st_intersection(mpa_union)


# Assign observations to grid cells
temp_grid <- st_join(temp, grid, join = st_within)

# Count unique sampling events per grid cell
grid_counts <- temp_grid |>
  st_drop_geometry() |>
  group_by(grid_id) |>
  summarise(
    n_samples = n_distinct(paste(Mission, Event)),
    .groups = "drop"
  )

# Retain cells with more than 3 sampling events
valid_grid <- grid_counts |>
  filter(n_samples >= 3) |>
  pull(grid_id)

# Filter observations
coral_filtered <- temp_grid |>
  filter(grid_id %in% valid_grid,
         pa) %>%
  mutate(Year = as.numeric(as.character(Year)))


# Spatial + temporal model
model_spatiotemporal <- lm(
  size ~ factor(grid_id) + Year,
  data = coral_filtered
)


slope_year <- coef(model_spatiotemporal)["Year"]
p_value <- summary(model_spatiotemporal)$coefficients["Year", "Pr(>|t|)"]

# Determine significance
significant_trend <- p_value < 0.05

#Assign score
size_score <- if (p_value < 0.05 & slope_year < 0) {
  0
} else {
  100
}

#-----------------------------------------------------
#Scoring for Indicator 3 (Proportion of live vs Dead)
#-----------------------------------------------------

temp <- st_transform(temp, 32620)
mpas <- st_transform(mpas, 32620)

# Combine MPA polygons
mpa_union <- st_union(mpas)

# Create 1 km grid and clip to MPAs
grid <- st_make_grid(mpa_union, cellsize = 1000, square = TRUE) |>
  st_as_sf() |>
  mutate(grid_id = row_number()) |>
  st_intersection(mpa_union)


# Assign observations to grid cells
temp_grid <- st_join(temp, grid, join = st_within)

# Count unique sampling events per grid cell
grid_counts <- temp_grid |>
  st_drop_geometry() |>
  group_by(grid_id) |>
  summarise(
    n_samples = n_distinct(paste(Mission, Event)),
    .groups = "drop"
  )

# Retain cells with more than 3 sampling events
valid_grid <- grid_counts |>
  filter(n_samples >= 3) |>
  pull(grid_id)

# Filter observations
coral_filtered <- temp_grid |>
  filter(grid_id %in% valid_grid,
         pa) %>%
  mutate(Year = as.numeric(as.character(Year)))


# Spatial + temporal model
model_spatiotemporal <- lm(
  per_colony_living ~ factor(grid_id) + Year,
  data = coral_filtered
)


slope_year <- coef(model_spatiotemporal)["Year"]
p_value <- summary(model_spatiotemporal)$coefficients["Year", "Pr(>|t|)"]

# Determine significance
significant_trend <- p_value < 0.05

#Assign score
living_score <- if (p_value >= 0.05 | slope_year > 0) {
  100
} else {
  0
}


#---------------------------------------------------------
#Scoring for Indicator 4 (Proportion of Zoanthid Growth)
#--------------------------------------------------------

temp <- st_transform(temp, 32620)
mpas <- st_transform(mpas, 32620)

# Combine MPA polygons
mpa_union <- st_union(mpas)

# Create 1 km grid and clip to MPAs
grid <- st_make_grid(mpa_union, cellsize = 1000, square = TRUE) |>
  st_as_sf() |>
  mutate(grid_id = row_number()) |>
  st_intersection(mpa_union)


# Assign observations to grid cells
temp_grid <- st_join(temp, grid, join = st_within)

# Count unique sampling events per grid cell
grid_counts <- temp_grid |>
  st_drop_geometry() |>
  group_by(grid_id) |>
  summarise(
    n_samples = n_distinct(paste(Mission, Event)),
    .groups = "drop"
  )

# Retain cells with more than 3 sampling events
valid_grid <- grid_counts |>
  filter(n_samples >= 3) |>
  pull(grid_id)

# Filter observations
coral_filtered <- temp_grid |>
  filter(grid_id %in% valid_grid,
         pa) %>%
  mutate(Year = as.numeric(as.character(Year)))


# Spatial + temporal model
model_spatiotemporal <- lm(
  zoa_growth ~ factor(grid_id) + Year,
  data = coral_filtered
)


slope_year <- coef(model_spatiotemporal)["Year"]
p_value <- summary(model_spatiotemporal)$coefficients["Year", "Pr(>|t|)"]

# Determine significance
significant_trend <- p_value < 0.05

#Assign score
zoa_score <- if (p_value >= 0.05 & slope_year > 0) {  #Opposite to all the other schemes, if zoa growth is significantly increasing receives a score of 0%
  0
} else {
  100
}


