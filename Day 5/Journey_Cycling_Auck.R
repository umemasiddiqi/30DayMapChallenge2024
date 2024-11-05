###############################################
###     30 Day Map Challenge                ###
###      Nov 05, 2024.                      ###
###      Day 5: A Journey                   ###
###      Umema A. Siddiqi                   ###
###############################################

#HUGE THANKS to MILOS AGATHON for the tutorial (https://www.youtube.com/watch?v=UNGzJrx8VrE)

# Install Packages and Libraries --------------------------------------------------------------
install.packages("remotes")
remotes::install_github(
  "GIScience/openrouteservice-r"
)

libs <- c(
  "tidyverse", "openrouteservice",
  "sf", "leaflet", "maptiles",
  "tidyterra"
)

installed_libs <- libs %in% rownames(
  installed.packages()
)

if(any(installed_libs == F)){
  install.packages(
    libs[!installed_libs]
  )
}

invisible(
  lapply(
    libs,
    library,
    character.only = T
  )
)

# 1. DEFINE MAIN PARAMETERS ---------------------------------------------------------------

openrouteservice::ors_profile()
lat <- -36.848
lon <- 174.763
api_key <- "********************************************************" # PLEASE INSERT HERE YOUR API KEY

# 2. QUERY -------------------------------------------------------------------------------

coords <- data.frame(lon, lat)

cycling_auk <- openrouteservice::ors_isochrones(
  locations = coords,
  profile = "cycling-regular",
  range = 3600,
  interval = 600,
  api_key = api_key,
  output = "sf"
)

# 3. DATA TRANSFORMATION ---------------------------------------------------------------------

sf::sf_use_s2(F)
cycling_auk$mins <- cycling_auk$value / 60
cycling_auk$mins <- factor(
  cycling_auk$mins
)

cycling_auk_cropped <- cycling_auk |>
  dplyr::group_by(mins) |>
  sf::st_intersection() |>
  dplyr::ungroup()

# 4. INTERACTIVE MAP OF CYCLING CATCHMENT AREA ---------------------------------------------------

pal_fact <- leaflet::colorFactor(
  "RdPu",
  domain = cycling_auk_cropped$mins,
  reverse = T,
  na.color = "transparent"
)

leaflet::leaflet(
  cycling_auk_cropped
) |>
  leaflet::addPolygons(
    fill = T,
    stroke = T,
    color = pal_fact,
    weight = .3,
    fillColor = ~pal_fact(mins),
    fillOpacity = .3
  ) |>
  leaflet::addProviderTiles(
    "CartoDB.Positron"
  ) |>
  leaflet::addLegend(
    "bottomright",
    pal = pal_fact,
    values = cycling_auk_cropped$mins,
    labels = cycling_auk_cropped$mins,
    opacity = .5,
    title = "Cycling distance in Auckland"
  )

# 5. STATIC MAP OF CYCLING CATCHMENT AREA ------------------------------------------------------------------
cycling_auk_merc <- sf::st_transform(
  cycling_auk_cropped,
  3857
)

auk_layer <- maptiles::get_tiles(
  cycling_auk_merc,
  provider = "CartoDB.Positron",
  zoom = 11
)


cycling_map <- ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = auk_layer
  ) +
  geom_sf(
    data = cycling_auk_merc,
    aes(
      fill = factor(mins),
      color = factor(mins),
      geometry = geometry
    ),
    size = .2,
    alpha = .50,
    inherit.aes = F
  ) +
  scale_fill_manual(
    name = "Minutes",
    values = hcl.colors(
      6, "RdPu"
    )
  ) +
  scale_color_manual(
    values = hcl.colors(
      6, "RdPu"
    )
  ) +
  guides(
    color = "none",
    fill = guide_legend(
      nrow = 1,
      byrow = T,
      keyheight = unit(5, "mm"),
      keywidth = unit(5, "mm"),
      title.position = "top",
      label.position = "bottom",
      label.hjust = 0.5
    )
  ) +
  theme_void() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, family = "serif", face = "bold", size = 16, color = "black"),
    plot.caption = element_text(hjust = 0.5, face = "italic"),
    plot.margin = unit(
      c(
        t = 0, r = 0,
        b = 0, l = 0
      ), "lines"
    )
  ) +
  labs(
    title = "Cycling distance in Auckland",
    caption = "#30DayMapChallenge | Day 5: A Journey | Designer: U. Siddiqi"
  )

cycling_map

# 6. SAVE MAP  ---------------------------------------------------------------------------

ggsave(paste0("NZ_Cycling_", format(Sys.time(), "%d%m%Y"), ".jpg"), dpi = 320, width = 6, height = 5.5)

