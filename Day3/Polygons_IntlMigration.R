###############################################
###     30 Day Map Challenge                ###
###      Nov 03, 2024.                      ###
###      Day 3: Polygons                    ###
###      Umema A. Siddiqi                   ###
###############################################

# Load necessary libraries ---------------------------------
library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(scales)
library(ggspatial)

# Get regional boundaries of New Zealand -------------------
nz_regions <- ne_states(country = "New Zealand", returnclass = "sf")

# Data of Net International Migration ----------------------
# Source : Stats NZ
migration_data <- data.frame(
  region = c("Northland", "Auckland", "Waikato", "Bay of Plenty", "Gisborne",
             "Hawke's Bay", "Taranaki", "Manawatu-Wanganui", "Wellington",
             "Tasman", "Nelson", "Marlborough", "West Coast", "Canterbury",
             "Otago", "Southland"),
  net_migration = c(1200, 18600, 4100, 3300, 450,
                    1800, 890, 1400, 3100,
                    440, 700, 470, 240, 4500,
                    2100, 980)
)

# Join the migration data with regional boundaries -----------
nz_regions <- nz_regions %>%
  left_join(migration_data, by = c("gn_name" = "region"))


# Calculate median
median_value <- median(migration_data$net_migration)

# Determine highest and lowest migration regions
highest_migration_region <- nz_regions %>% filter(net_migration == max(net_migration, na.rm = TRUE))
lowest_migration_region <- nz_regions %>% filter(net_migration == min(net_migration, na.rm = TRUE))

# Calculate centroids for text placement
highest_coords <- st_coordinates(st_centroid(highest_migration_region))
lowest_coords <- st_coordinates(st_centroid(lowest_migration_region))

# Plot the map with migration data ----------------------------
ggplot() +
  geom_sf(data = nz_regions, aes(fill = net_migration), color = "black") +  # Use net migration for fill
  scale_fill_gradient(low = "grey90", high = "red4", name = "Net Migration") +  # Gradient fill for legend
  theme(panel.grid = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
  labs(title = "Net International Migration", 
       subtitle = "From 2018-2023",
       caption = "#30DayMapChallenge2024 | Day 3: Polygons | Designer: U. Siddiqi") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif", face = "bold", size = 16, color = "black"),
    plot.subtitle = element_text(hjust = 0.5, family = "serif", face = "italic", size = 10, color = "black"),
    plot.caption = element_text(hjust = 0.5, face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = c(0.98, 0.15),  
    legend.justification = c("right", "bottom"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  coord_sf(xlim = c(166, 180), ylim = c(-48, -34), expand = TRUE) +
  
  # Add north arrow
  annotation_north_arrow(
    location = "tl",  
    which_north = "true",  
    style = north_arrow_fancy_orienteering()  # Style of the north arrow
  ) +
  
  # Add the custom text box
  annotate("text", x = 176.5, y = -40.41, 
           label = paste("Highest:", format(highest_migration_region$net_migration, big.mark = ",", scientific = FALSE), "\n",
                         "Median:", format(median_value, big.mark = ",", scientific = FALSE), "\n",
                         "Lowest:", format(lowest_migration_region$net_migration, big.mark = ",", scientific = FALSE)), 
           size = 3, color = "black", 
           hjust = 0, vjust = 5.8, 
           fontface = "italic") +
  
  # Labels to show highest and lowest migration regions
  geom_text(aes(x = highest_coords[1], y = highest_coords[2], 
                label = paste(highest_migration_region$gn_name, highest_migration_region$net_migration, sep = ": ")),
            size = 4, color = "red4", nudge_y = -0.4, nudge_x = -2.6) +
  geom_text(aes(x = lowest_coords[1], y = lowest_coords[2], 
                label = paste(lowest_migration_region$gn_name, lowest_migration_region$net_migration, sep = ": ")),
            size = 3, color = "grey50", nudge_y = 1.2, nudge_x = -1.1)

# Save the map -----------------------------------------------
ggsave(paste0("NZ_Migration_", format(Sys.time(), "%d%m%Y"), ".jpg"), dpi = 320, width = 5.2, height = 6)
