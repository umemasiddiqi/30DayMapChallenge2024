##################################
###     30 Day Map Challenge   ###
###      Nov 4,  2024.         ###
###      Day 4: Hexagon        ###
###     Umema A. Siddiqi       ###
##################################

# libraries --------------------------------------------------------------------

library("sf")
library("tidyverse")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggplot2")
library("viridis")

# Load data --------------------------------------------------------------------

NZ <- ne_countries(scale = "large",
                   country = "New Zealand",
                   returnclass = "sf") 

hexgrid <- st_make_grid(NZ,
                        cellsize = 2e4, ## unit: metres
                        what = 'polygons',
                        square = FALSE ## !
) |>
  st_as_sf()

hexgrid_NZ <- hexgrid[c(unlist(st_contains(NZ, hexgrid)), 
                        unlist(st_overlaps(NZ, hexgrid))) ,] 

# Get the bounding box of New Zealand
bbox_NZ <- st_bbox(NZ)

# Create a polygon for the bounding box
bbox_polygon <- st_sfc(st_polygon(list(rbind(
  c(bbox_NZ["xmin"], bbox_NZ["ymin"]),
  c(bbox_NZ["xmin"], bbox_NZ["ymax"]),
  c(bbox_NZ["xmax"], bbox_NZ["ymax"]),
  c(bbox_NZ["xmax"], bbox_NZ["ymin"]),
  c(bbox_NZ["xmin"], bbox_NZ["ymin"])  
))), crs = st_crs(NZ))

# Create the plot  -----------------------------------------------------------
NZ |> ggplot() +
  # Add a black border rectangle
  geom_sf(data = bbox_polygon, fill = "black", color = NA) +  
  geom_sf(fill = "white", color = "darkblue") +  
  geom_sf(data = hexgrid_NZ, fill = "gold", color = "darkblue", show.legend = "point") +  
  labs(title = "Manuka Honey of New Zealand", 
       caption = "#30DayMapChallenge | Day 4: Hexagons | Designer: U. Siddiqi") +
  theme_void() +  
  theme(plot.title = element_text(hjust = 0.5, color = "gold", size = 16, face = "bold.italic"),  
        plot.caption = element_text(hjust = 0.5, color = "gold", size = 10, face = "italic"),
        plot.margin = margin(t = 2, r = 5, b = 2, l = 5)) +  
  coord_sf(xlim = c(166, 180), ylim = c(-48, -34), expand = TRUE)  

# Save this plot ---------------------------------------------------------------

ggsave(paste0("NZ_Hexagon_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)
