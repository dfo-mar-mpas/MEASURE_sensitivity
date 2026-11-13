require(sf)
require(arcpullr)
require(readxl)
require(rvest)
require(xml2)
require(purrr)
require(stringr)
require(tidyr)
require(vegan)
require(dplyr)


gullyzones <- get_spatial_layer(
  "https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/oceans_act_marine_protected_areas_zones_de_protection_marines_de_la_loi_sur_les_oceans/MapServer/0",
  where = "NAME_E LIKE '%Gully%'"
)

coral_presence <- read_excel("data/Data records for Sensitivity Analyses.xlsx") %>%
  rename(
    Latitude = Lat,
    Longitude = Long,
    sci_name = Taxa
  ) %>%
  mutate(pa = 1) %>%
  dplyr::select(Latitude, Longitude, sci_name, pa, Year) %>%
  st_as_sf(crs = 4326, coords = c("Longitude", "Latitude"), remove = FALSE)

url <-  "https://www.inter.dfo-mpo.gc.ca/en/mar-var-2023-110"          # "https://www.dfo-mpo.gc.ca/oceans/mpa-zpm/gully/index-eng.html"

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


page <- read_html(url)

page_text <- html_text2(page)


# --- Identify division sections ----------------------------------------------

division_matches <- str_locate_all(
  page_text,
  "Portion of 4VS:|Portion of 4W:"
)[[1]]

if (nrow(division_matches) == 0) {
  stop("No division headings found.")
}


# --- Extract coordinates -----------------------------------------------------

coord_df <- map_dfr(seq_len(nrow(division_matches)), \(i) {

  start <- division_matches[i, 1]

  end <- if (i < nrow(division_matches)) {
    division_matches[i + 1, 1] - 1
  } else {
    nchar(page_text)
  }

  section <- str_sub(page_text, start, end)

  # Identify division
  division <- str_match(
    section,
    "Portion of (4VS|4W)"
  )[, 2]

  # Extract latitude and longitude pairs
  coords <- str_match_all(
    section,
    "(\\d{1,2}°\\s*\\d{1,2}'\\s*\\d{1,2}[”\"]?N)\\s*(\\d{1,2}°\\s*\\d{1,2}'\\s*\\d{1,2}[”\"]?W)"
  )[[1]]

  if (nrow(coords) == 0) return(NULL)

  tibble(
    division = division,
    latitude = coords[, 2],
    longitude = coords[, 3]
  )
})


# --- Convert to decimal degrees -----------------------------------------------

coord_df <- coord_df |>
  mutate(
    latitude = dms_to_dd(latitude),
    longitude = dms_to_dd(longitude)
  )

labels <- unique(coord_df$division)

polys <- coord_df |>
  group_by(division) |>
  group_split() |>
  map(\(d) {
    xy <- cbind(
      lon = d$longitude,
      lat = d$latitude
    )

    if (!isTRUE(all.equal(
      unname(xy[1, ]),
      unname(xy[nrow(xy), ])
    ))) {
      xy <- rbind(xy, xy[1, ])
    }

    st_polygon(list(xy))
  })

# --- build sf ----------------------------------------------------------------

gully_vo <- st_sf(
  Variation_Order = paste("Portion of", labels),
  n_vertices = map_int(polys, \(p) nrow(p[[1]]) - 1L), # excludes closing vertex
  source_url = url,
  date_scraped = Sys.Date(),
  geometry = st_sfc(polys, crs = 4269) # NAD83 geographic
) %>%
  st_transform(st_crs(gullyzones))


coral_df <- coral_presence %>%
  st_join(gullyzones) %>%
  st_join(gully_vo) %>%
  right_join(coral_photos_df, by = "ID") %>%
  mutate(zone_multiplier = case_when(!is.na(Variation_Order) ~ 10,
                                     ZONE_E == "Zone 1" ~ 8,
                                     ZONE_E == "Zone 2" ~ 3,
                                     .default = 1) %>%
  dplyr::select(names(coral_presence), zone_multiplier, Variation_Order, ZONE_E, coral_buffer_multiplier, sci_name) %>%
  mutate(density = runif(n(), 0, 5)*pa*zone_multiplier,    #this is in corals per meter sq     #originally was set to on;y run if the gear was ropos but messes with thing is there is no ROPOS data
         size = runif(n(), 0, 20)*pa*zone_multiplier,     #this is in cm
         per_colony_living = case_when(Year < 2004 & (ZONE_E == "Zone 1" | Variation_Order == "Portion of 4W") ~ rbeta(n(), 17, 3),    # This is the % living of a single colony----Self protected in canyons   (85%)
                                       Year >= 2004 & (ZONE_E == "Zone 1") ~ rbeta(n(), 19, 1),                                        # Once protections kick in for zone 1      (95%)
                                       Year >= 2004 & ZONE_E == "Zone 2" & is.na(Variation_Order) ~ rbeta(n(), 18, 2),                 # Ignoring VOs, protection kicks in for Zone 2 but its less than zone 1   (90%)
                                       Year >= 2004 & (ZONE_E == "Zone 3") ~ rbeta(n(), 15, 5),                                        # Protection kicks in for zone 3   (75%)
                                       Year >= 2019 & !is.na(Variation_Order) ~ rbeta(n(), 18, 2),                                     # extra protection kicks in once VOs are established    (75%)
                                       Year >= 2004 & Year <= 2019 & !is.na(Variation_Order) ~ rbeta(n(), 18, 2),                      # during this time VOs act as zone 2    (90%)
                                       .default = rbeta(n(), 10, 10), ),
         zoa_growth = runif(n(), 0, 1 - per_colony_living),
         Year = as.numeric(as.character(Year))
  )




#--------------------------------------------
# Scoring for Indicator 2 (coral diversity)
#-------------------------------------------

hist_wide_corals <- coral_df %>%
  filter(Year != max(Year)) %>%
  mutate(numeric_pa = as.numeric(pa)) %>%
  st_drop_geometry() %>%
  pivot_wider(id_cols = ID, names_from = sci_name, values_from = numeric_pa)               #changing from long format to wide format

# Calculate sample-based species accumulation curve
sp_accum <- specaccum(as.matrix(wide_corals[, -1]), method = "random") # or method = "random"

# Plot the curve
plot(sp_accum, xlab = "Number of Sites (Sample Effort)", ylab = "Species Richness")

n_samples <- coral_df %>% filter(Year == max(Year)) %>% pull(ID) %>% unique() %>% length()      # get the unique number of samples
richness_est <- predict(sp_accum, newdata = n_samples)                                      # get the richness estimate based on the number of samples
sd_est <- approx(x = sp_accum$sites, y = sp_accum$sd, xout = n_samples)$y                   # the standard deviation estimate
S <- coral_df %>% filter(Year == max(Year), pa) %>% pull(sci_name) %>% unique() %>% length()    # Calculate observed Species richness

lower_95 <- richness_est - (1.96 * sd_est)                          #calculate the lower 95% confidence limit
score <- as.numeric(S > lower_95) * 100                             #calculate final score



#---------------------------------------
#Scoring for Indicator 1 (Distribution)
#---------------------------------------

coral_df_filtered <- coral_df %>%
  filter(
    !(!is.na(Variation_Order) & pa == TRUE &
        as.numeric(Year) == 2011)
  )

# Keep unique coral observations
coral_obs <- coral_df %>%
  filter(pa == TRUE) %>%
  distinct(ID, sci_name, .keep_all = TRUE)


# Calculate coverage by conservation area
coverage <- coral_obs %>%
  sf::st_drop_geometry() %>%
  summarise(
    coverage = n_distinct(ID[ZONE_E == "Zone 1"]) /
      n_distinct(ID)

  )

coverage <- coverage %>%
  mutate(
    score = ifelse(coverage > 0.8, 100, coverage/0.8*100)  # if the coverage is above the min it gets 100%, if its below if gets the proportion of
  )


#------------------------------------------
#Scoring for Indicator 1 (Size structure)
#------------------------------------------

coral_df <- st_transform(coral_df, 32620)
mpas <- st_transform(mpas, 32620)

# Combine MPA polygons
mpa_union <- st_union(mpas)

# Create 1 km grid and clip to MPAs
grid <- st_make_grid(mpa_union, cellsize = 1000, square = TRUE) %>%
  st_as_sf() %>%
  mutate(grid_id = row_number()) %>%
  st_intersection(mpa_union)


# Assign observations to grid cells
coral_df_grid <- st_join(coral_df, grid, join = st_within)

# Count unique sampling events per grid cell
grid_counts <- coral_df_grid %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(
    n_samples = n_distinct(paste(Mission, Event)),
    .groups = "drop"
  )

# Retain cells with more than 3 sampling events
valid_grid <- grid_counts %>%
  filter(n_samples >= 2) %>%
  pull(grid_id)

# Filter observations
coral_df_filtered <- coral_df_grid %>%
  filter(grid_id %in% valid_grid,
         pa) %>%
  mutate(Year = as.numeric(as.character(Year)))


# Spatial + coral_dforal model
model_spatiocoral_dforal <- lm(
  size ~ factor(grid_id) + Year,
  data = coral_df_filtered
)


slope_year <- coef(model_spatiocoral_dforal)["Year"]
p_value <- summary(model_spatiocoral_dforal)$coefficients["Year", "Pr(>|t|)"]

# Determine significance
significant_trend <- p_value < 0.05

#Assign score
size_score <- if (p_value < 0.05 & slope_year < 0) {
  0
} else {
  100
}






#-------------------------------------------------

#load shady boots MPAs script and filter
mpas <- MPAs %>%
  filter(NAME_E == "Gully Marine Protected Area")


#create spatial grid once
years <- seq(1997, 2026, 5)

coral_df <- st_transform(coral_df, 32620)
mpas <- st_transform(mpas, 32620)

# Create grid
mpa_union <- st_union(mpas)

grid <- st_make_grid(mpa_union, cellsize = 1000, square = TRUE) %>%
  st_as_sf() %>%
  mutate(grid_id = row_number()) %>%
  st_intersection(mpa_union)

# Assign observations to grid cells
coral_grid <- st_join(coral_df, grid, join = st_within)

# Identify valid grid cells (at least 3 sampling events)
valid_grid <- coral_grid %>%
  st_drop_geometry() %>%
  filter(!is.na(grid_id)) %>%
  group_by(grid_id) %>%
  summarise(
    n_samples = n_distinct(paste(Mission, Event)),
    .groups = "drop"
  ) %>%
  filter(n_samples >= 2) %>%
  pull(grid_id)



# Store results somewhere
coral_scores <- list()

# Outer loop: years
for (yr in years) {

  #--------------------------------------------
  #  Coral diversity
  #--------------------------------------------

  # Historical observations (before target year)
  hist_wide <- coral_df %>%
    filter(Year <= yr) %>%
    mutate(numeric_pa = as.numeric(pa)) %>%
    st_drop_geometry() %>%
    pivot_wider(
      id_cols = ID,
      names_from = sci_name,
      values_from = numeric_pa,
      values_fn = max,
      values_fill = 0
    )

  # Species accumulation curve
  sp_accum <- specaccum(
    as.matrix(hist_wide[, -1]),
    method = "random"
  )

  # Current year sample effort
  current_year <- coral_df %>% filter(Year <= yr) %>%
    filter(Year == max(Year))

  n_samples <- n_distinct(current_year$ID)

  richness_est <- predict(sp_accum, newdata = n_samples)

  sd_est <- approx(
    x = sp_accum$sites,
    y = sp_accum$sd,
    xout = n_samples,
    rule = 2
  )$y

  # Observed species richness
  S <- current_year %>%
    filter(pa == TRUE) %>%
    pull(sci_name) %>%
    unique() %>%
    length()

  lower_95 <- richness_est - (1.96 * sd_est)

  diversity_score <- as.numeric(S > lower_95) * 100


  #--------------------------------------------
  # Distribution
  #--------------------------------------------

  coral_obs <- current_year %>%
    filter(pa == TRUE)

  coverage <- coral_obs %>%
    st_drop_geometry() %>%
    group_by(sci_name) %>%
    summarise(
      coverage = n_distinct(ID[ZONE_E == "Zone 1"]) /
        n_distinct(ID)
    ) %>%
    pull(coverage, sci_name)

  distribution_scores <- coral_obs %>%
    st_drop_geometry() %>%
    group_by(sci_name) %>%
    summarise(
      coverage = n_distinct(ID[ZONE_E == "Zone 1"]) /
        n_distinct(ID),
      .groups = "drop"
    ) %>%
    mutate(
      Score = ifelse(
        coverage > 0.8,
        100,
        coverage / 0.8 * 100
      )
    ) %>%
    mutate(
      Year = yr,
      Indicator = "Distribution"
    ) %>%
    dplyr::select(Year, Indicator, Species = sci_name, Score, coverage)


  # Store diversity score

  coral_scores <- bind_rows(coral_scores, data.frame(
    Year = yr,
    Indicator = "Coral diversity",
    Species = NA_character_,
    Score = diversity_score
  )
  )
  # Store species-specific distribution scores
  coral_scores <- bind_rows(coral_scores, distribution_scores)

  # distribution_score <- ifelse(
  #   coverage > 0.8,
  #   100,
  #   coverage / 0.8 * 100
  # )
  #
  # distribution_score
  #
  # #retain scores for all years
  # coral_scores[[length(coral_scores) + 1]] <- data.frame(
  #   Year = c(yr, yr),
  #   Indicator = c("Coral diversity", "Distribution"),
  #   Score = c(diversity_score, distribution_score)
  # )

  #--------------------------------------------
  # Indicators: Living tissue, size, density
  #--------------------------------------------

  trend_scores <- c(
    per_colony_living = "positive" ,   #indicators and their desired trends
    size = "positive",
    density = "positive",
    zoa_growth = "negative"
  )

  for (indicator in names(trend_scores)) {

    expected_trend <- trend_scores[[indicator]]

    for (sp in unique(coral_grid$sci_name)) {

      # Filter observations up to target year
      coral_filtered <- coral_grid %>%
        filter(
          !is.na(grid_id),
          grid_id %in% valid_grid,
          pa == TRUE,
          as.numeric(Year) <= yr,
          sci_name == sp
        ) %>%
        mutate(Year = as.numeric(as.character(Year))) %>%
        st_drop_geometry() %>%
        filter(!is.na(.data[[indicator]]))

      # cat(
      #   "\nYear:", yr,
      #   "| Indicator:", indicator,
      #   "| Grid cells:", n_distinct(coral_filtered$grid_id),
      #   "| Observations:", nrow(coral_filtered)
      # )

      # Check whether there is enough data to fit model
      if (
        nrow(coral_filtered) > 0 &&
        n_distinct(coral_filtered$Year) > 2
      ) {

        # Spatial-temporal model
        model <- lm(
          as.formula(paste(indicator, "~ factor(grid_id) + Year")),
          data = coral_filtered
        )

        if ("Year" %in% row.names(summary(model)$coefficients)) {

          slope_year <- coef(model)["Year"]

          p_value <- summary(model)$coefficients[
            "Year", "Pr(>|t|)"
          ]

          indicator_score <-if (
            !is.na(p_value) &&
            !is.na(slope_year) &&
            p_value < 0.05 &&
            (
              (expected_trend == "negative" && slope_year > 0) ||
              (expected_trend == "positive" && slope_year < 0)
            )
          ) {
            0
          } else {

            100
          }

        } else {
          indicator_score <- NA_real_
        }

      } else {

        indicator_score <- NA_real_
      }


      # Store individual indicator score
      coral_scores <- bind_rows(coral_scores, data.frame(
        Year = yr,
        Indicator = indicator,
        Score = indicator_score,
        Species = sp
      ))
    }
  }
}

# Combine all scores into one dataframe
coral_scores <- bind_rows(coral_scores) %>%
  arrange(Year, Indicator)

mean_coral_scores <- coral_scores %>%
  group_by(Year, Indicator) %>%
  summarize(Score = mean(Score, na.rm = TRUE))
