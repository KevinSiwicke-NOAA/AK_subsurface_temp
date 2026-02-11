##################
# Make a map that displays all summarized data
##################

# Load packages needed
pkgs <- c("ggplot2", "dplyr", "sf", "ggspatial", "akgfmaps")
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE, quietly = TRUE)

# Load temperature locations...this will need to be updated when finalized
bot_locs <- readRDS("data/bot4use.rds") %>% distinct(source, mod_reg, Year, Month, stn_id, longitude, latitude) %>% 
  mutate(Month = factor(ifelse(Month == 5, "MAY", 
                               ifelse(Month == 6, "JUN",
                                      ifelse(Month == 7, "JUL",
                                             ifelse(Month == 8, "AUG", "SEP")))),
                        levels = c("MAY", "JUN", "JUL", "AUG", "SEP")))

# Make bar plot of samples by month/region
bar_plot <- ggplot(bot_locs, aes(Month, fill = mod_reg)) + 
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  scale_y_continuous(limits = c(0, 7450), expand = c(0,0)) +
  labs(y = "Frequency", fill = "Region") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12),
        axis.title.x = element_blank())

# Load grid
# grid_50_sf <- readRDS("data/grid_50_sf.rds")

# Load gridded count data
grid_counts_sf <- readRDS(file = "data/50_k_grid_counts.rds")

ak_rus <- sf::st_read(system.file("extdata", "ak_russia.shp", package = "akgfmaps"), quiet = TRUE)
ak_can <- sf::st_read(system.file("extdata", "alaska_canada_dcw.shp", package = "akgfmaps"), quiet = TRUE)
map_bnd = grid_counts_sf |>
  sf::st_bbox(ak_rus)

# Ecosystem Status Report (ESR) regions
esr_areas <- get_esr_regions(
  select.region = "esr_area",
  set.crs = "EPSG:3338"
)

# Boundary Lines ... used the esr layer to get lines at the juncture as close as possible                   -
lns <- st_as_sfc(c("LINESTRING(-164 54.7,-164 52.2,
                  -164 54.7, -165 54.5,
                  -165 54.5, -167 54.5,
                  -167 54.5, -170 55,
                  -175.85 55.63, -170 55)")) |> 
  st_sf(ID = "line1", crs = "EPSG:4326") |> 
  st_transform(terra::crs(grid_counts_sf))

map <- ggplot(grid_counts_sf) +
  # geom_sf(data = esr_areas) + # just for getting the line approximate at the juncture
  # geom_sf(data = grid_50_sf, fill = NA, color = "red4") +
  geom_sf(aes(fill = points_per_cell)) + # can also look at mean_bias and bias_sd
  scale_fill_stepsn(colors = sp::bpy.colors(n = 10, cutoff.tails = 0.1, alpha = 1.0), n.breaks = 10) +
  geom_sf(data = ak_rus) +
  geom_sf(data = ak_can) +
  geom_sf(data = lns, col = "grey30", linewidth = 1, alpha = 0.5) +
  coord_sf(xlim = c(map_bnd$xmin, map_bnd$xmax),
           ylim = c(map_bnd$ymin, map_bnd$ymax), expand = TRUE) +
  annotate("text", x = 100000, y = 1500000, label= "Alaska", size = 16/.pt) +
  annotate("text", x = 500000, y = 900000, label= "GOA", size = 12/.pt) +
  annotate("text", x = -2000000, y = 500000, label= "AI", size = 12/.pt) + 
  annotate("text", x = -1350000, y = 1000000, label= "EBS", size = 12/.pt) + 
  annotation_scale(location = "br", width_hint = 0.4, height = unit(0.5, "cm"), pad_x = unit(3, "cm")) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering,
                         height = unit(2, "cm"), width = unit(2, "cm")) +
  labs(fill = 'N') +
  scale_x_continuous(name = "Longitude", breaks = c(160, 170, -180, -170, -160, -150, -140, -130)) + 
  scale_y_continuous(name = "Latitude", breaks = seq(48, 62, 2)) + 
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
       axis.title = element_text(size = 12))

plot_grid(map, NA, bar_plot, ncol = 1, rel_heights = c(1, -0.1, 0.5), align = 'v', labels = c("A", "", "B"))

ggsave(file = "results/plots/All_raster.png", bg = "white", height = 15, width = 24, units = "cm", dpi = 300)
