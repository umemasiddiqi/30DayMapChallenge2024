###############################################
###     30 Day Map Challenge                ###
###      Nov 01, 2024.                      ###
###      Day 1: Points                      ###
###      Umema A. Siddiqi                   ###
###############################################

# Load necessary libraries -----------------------------------
library(ggplot2)
library(maps)
library(dplyr)

# Create a data frame with region information ---------------
region_data <- data.frame(
  region = c("Auckland", "Canterbury", "Wellington", 
             "Waikato", "Bay of Plenty", "Manawatu-Wanganui", 
             "Otago", "Northland", "Hawke's Bay", 
             "Taranaki", "Southland", "Tasman", 
             "Nelson", "Gisborne", "Marlborough", 
             "West Coast"),
  population = c(1798300, 694400, 550600, 536200, 355200, 
                 263300, 257200, 204800, 185400, 130800, 
                 106100, 60000, 55200, 53300, 52300, 
                 34800),
  lat = c(-36.8485 + 0.1,  # Move Auckland up slightly so it doesn't overlap
          -43.5321, 
          -41.2865, 
          -37.7800, 
          -37.0 - 0.1,  # Move Bay of Plenty down slightly
          -40.3500, 
          -45.8742, 
          -35.7300, 
          -39.4900, 
          -39.0610, 
          -46.4130, 
          -41.2500, 
          -41.2700, 
          -38.6450, 
          -41.5120, 
          -42.0000),
  lon = c(174.7633 -0.4, 172.6362, 174.7762, 175.2000, 176.0, 
          175.6000, 170.5036, 174.3200, 176.8500, 174.0870, 
          168.3520, 172.8000, 173.2800, 178.0230, 173.9600, 
          171.0000)
)

# Get the top 10 regions by population -----------------------
top_regions <- region_data %>%
  arrange(desc(population)) %>%
  head(10)

# Create a map of New Zealand --------------------------------
nz_map <- map_data("nz")

# Create the bubble map --------------------------------------
bubble_map <- ggplot() +
  # Draw the New Zealand map 
  geom_polygon(data = nz_map, aes(x = long, y = lat, group = group), 
               fill = "white", color = "#7A2B2B") +  
  # Add region bubbles sized by population for top 10 regions
  geom_point(data = top_regions, aes(x = lon, y = lat, size = population),
             color = "orange", alpha = 0.6) + # Adjust opacity
  # Add region names
  geom_text(data = top_regions, aes(x = lon + ifelse(region == "Auckland", -0.4, 0.9), 
                                    y = lat, label = region),
            vjust = 1.5, size = 7.5, color = "#7A2B2B") +
  labs(title = "Top 10 Populous Regions in New Zealand",
       subtitle = "Source: Stats NZ",
       caption = "#30DayMapChallenge2024 | Day 1: Points | Designer: U. Siddiqi") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif", size = 22, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "serif", size = 16, face = "italic"),
    plot.caption = element_text(hjust = 0.5, family = "sansserif", face = "italic", size = 14),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_rect(color = "#7A2B2B", fill = NA, size = 1.5)  
  ) +
  coord_fixed(1.5) +  # Fix aspect ratio for a larger map
  xlim(c(165, 180)) +  # Limit x-axis to focus on NZ
  ylim(c(-47, -33.5)) +  # Limit y-axis to focus on NZ
  scale_size_continuous(range = c(5, 25))  # Adjust size range for region bubbles

# Save the plot as JPG --------------------------------------------
ggsave("NZ_Top_10_Regions.jpg", plot = bubble_map, width = 10, height = 14, dpi = 320)  
