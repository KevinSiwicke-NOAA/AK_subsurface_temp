##################
# Make a map that displays all summarized data and plot by month
##################

# Load packages needed
pkgs <- c('ggplot2', 'dplyr', 'sf', 'terra', 'tidyterra', 'concaveman', 'ggspatial', 'akgfmaps', 'cowplot')
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE, quietly = TRUE)

# Load temperature locations (just bottom)
bot4use <- readRDS('data/bot4use.rds')

# Get a bathymetry layer for the area so that we can create a polygon using depth boundaries
# Download DEM for Alaska region (~1km resolution) directly from web using:
url <- 'http://research.cfos.uaf.edu/bathy/ARDEMv2.0.nc'
temp_nc <- tempfile(fileext = '.nc')
download.file(url, method = 'curl', temp_nc)

# crop to desired extent, and can filter by desired depths (in this case, depth or z was negative so we converted to positive)
r <- rast()
ext(r) <- c(170, 230, 48, 65) # extent (xmin, xmax, ymin, ymax)

crop_water <- crop(rast(temp_nc), r) |> 
  tidyterra::filter(z < 0, z > -2000) |> # depths between 0 and 2000 m
  tidyterra::mutate(z = ifelse(z < 0.00001 & z > -2000, -99, NA)) |> # convert negative depths to positive depths
  terra::project('EPSG:3338')
  
gc()

# Convert raster to an sf object, convert Alaska ALbers, and extract the largest consecutive polygon to clean up random outliers like lakes or seamounts
water_sf = st_as_sf(as.polygons(crop_water), trunc=FALSE, dissolve=FALSE, crs = crs(crop_water)) |> # sf for water polygon
  tibble::rowid_to_column() |> 
  st_cast('POLYGON') |> 
  mutate(area = st_area(geometry)) |> 
  group_by(rowid) |> 
  top_n(1, area) |> 
  select(z)
  
# Convert point data of bottom temps into an sf object in same CRS as above polygon
pts_sf <- st_as_sf(bot4use, coords = c('longitude', 'latitude'), crs = 'EPSG:4326') |> 
  st_transform(crs(water_sf)) #'+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs') 

# Get a buffer around the point data for clipping 
pts_buff <- pts_sf |> 
  concaveman(concavity = 1, length = 1000)  |> 
  st_buffer(100000) 

bound <- st_intersection(pts_buff, water_sf)

# 50 km grid for showing distribution of point data
grid_50_sf <- st_make_grid(bound, cellsize = 50000) |> # this is a 50km grid 
  st_intersection(bound) |> 
  st_sf(geometry = _) |>
  mutate(cell_id = row_number(), .before = 1)

# spatial join to match cell_id-s to points, then remove geometry and count samples per grid cell
counts_df <- st_join(pts_sf, grid_50_sf) |>
  st_drop_geometry() |>
  group_by(cell_id) |> # could look at it by mod_reg too if desired
  summarize(points_per_cell = n(),
            mean_bias = mean(T_diff2),
            bias_sd = sd(T_diff2))

# now add the count data back to the grid joining by cell_id
grid_counts_sf <- right_join(grid_50_sf, counts_df)
gc()

# Get some summary data of samples by month
bot_locs <- bot4use |> distinct(source, mod_reg, Year, Month, stn_id, longitude, latitude) |> 
  mutate(Month = factor(ifelse(Month == 5, 'MAY', 
                               ifelse(Month == 6, 'JUN',
                                      ifelse(Month == 7, 'JUL',
                                             ifelse(Month == 8, 'AUG', 'SEP')))),
                        levels = c('MAY', 'JUN', 'JUL', 'AUG', 'SEP')))

# Make bar plot of samples by month/region
bar_plot <- ggplot(bot_locs, aes(Month, fill = mod_reg)) + 
  geom_bar(stat = 'count', position = 'dodge') +
  scale_fill_manual(values = c('#0072B2', '#D55E00', '#009E73')) +
  scale_y_continuous(limits = c(0, 7450), expand = c(0,0)) +
  labs(y = 'Frequency', fill = 'Region') +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12),
        axis.title.x = element_blank())

# Get land in Alaska and Russia, and get boundary around data
ak_rus <- sf::st_read(system.file('extdata', 'ak_russia.shp', package = 'akgfmaps'), quiet = TRUE)
ak_can <- sf::st_read(system.file('extdata', 'alaska_canada_dcw.shp', package = 'akgfmaps'), quiet = TRUE)
map_bnd = grid_counts_sf |>
  sf::st_bbox(ak_rus)

# Ecosystem Status Report (ESR) regions, used for separating out EBS, AI, and GOA
esr_areas <- akgfmaps::get_esr_regions(
  select.region = 'esr_area',
  set.crs = 'EPSG:3338'
)

# Boundary Lines ... used the esr layer to get lines at the juncture as close as possible                   -
lns <- st_as_sfc(c('LINESTRING(-164 54.7,-164 52.2,
                  -164 54.7, -165 54.5,
                  -165 54.5, -167 54.5,
                  -167 54.5, -170 55,
                  -175.85 55.63, -170 55)')) |> 
  st_sf(ID = 'line1', crs = 'EPSG:4326') |> 
  st_transform(terra::crs(grid_counts_sf))

# Just to see how grid falls over (and out of) ESR regions
ggplot(grid_counts_sf) +
  geom_sf(data = esr_areas) + # just for getting the line approximate at the juncture
  geom_sf(data = grid_50_sf, fill = NA, color = 'red4')

map <- ggplot(grid_counts_sf) +
  geom_sf(aes(fill = points_per_cell)) + # can also look at mean_bias and bias_sd
  scale_fill_stepsn(colors = sp::bpy.colors(n = 10, cutoff.tails = 0.1, alpha = 1.0), n.breaks = 10) +
  geom_sf(data = ak_rus) +
  geom_sf(data = ak_can) +
  geom_sf(data = lns, col = 'grey30', linewidth = 1, alpha = 0.5) +
  coord_sf(xlim = c(map_bnd$xmin, map_bnd$xmax),
           ylim = c(map_bnd$ymin, map_bnd$ymax), expand = TRUE) +
  annotate('text', x = 100000, y = 1500000, label= 'Alaska', size = 16/.pt) +
  annotate('text', x = 500000, y = 900000, label= 'GOA', size = 12/.pt) +
  annotate('text', x = -2000000, y = 500000, label= 'AI', size = 12/.pt) + 
  annotate('text', x = -1350000, y = 1000000, label= 'EBS', size = 12/.pt) + 
  annotation_scale(location = 'br', width_hint = 0.4, height = unit(0.5, 'cm'), pad_x = unit(3, 'cm')) +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                         pad_x = unit(0.0, 'in'), pad_y = unit(0.2, 'in'),
                         style = north_arrow_fancy_orienteering,
                         height = unit(2, 'cm'), width = unit(2, 'cm')) +
  labs(fill = 'N') +
  scale_x_continuous(name = 'Longitude', breaks = c(160, 170, -180, -170, -160, -150, -140, -130)) + 
  scale_y_continuous(name = 'Latitude', breaks = seq(48, 62, 2)) + 
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
       axis.title = element_text(size = 12))

plot_grid(map, NA, bar_plot, ncol = 1, rel_heights = c(1, -0.1, 0.5), align = 'v', labels = c('A', '', 'B'))

ggsave(file = 'results/plots/Fig1_map_summary.png', bg = 'white', height = 15, width = 24, units = 'cm', dpi = 300)
