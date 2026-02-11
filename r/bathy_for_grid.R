library(sp)
library(sf)
library(terra)
library(ggplot2)
library(dplyr)
library(tidyterra)
library(concaveman)
library(ggspatial)
library(exactextractr)
library(pracma)
library(tictoc)

# load bottom data
bot4use <- readRDS("data/bot4use.rds")

# Start with high quality bathymetry made combining Zimmermans 100-m res with ARDEM for 100mx100m
# bathy_100m_3000max <- terra::rast("data/bathy_100m_3000max.tif")

# Just to clip an area with water between ~0 and 2000
ardem_water <- terra::rast("data/ARDEMv2.0.nc") |> 
  mutate(z = ifelse(z < 0.00001 & z > -2000, -99, NA))

# np_100m_2000max <- terra::rast("data/np_100m_2000max.tif")
# 
# ardem_depth <- terra::rast("data/ARDEMv2.0.nc") |>
#   filter(z < 0.00001, z > -2000)
crp <- rast(ext = c(170, 230, 45, 65))
crop_water <- crop(ardem_water, crp)

# plot(crop_water)

water_sf = st_as_sf(as.polygons(crop_water), trunc=FALSE, dissolve=FALSE, crs = "EPSG:4326") |>  # sf for water polygon
  st_transform("EPSG:3338") |> 
  tibble::rowid_to_column() |> 
  st_cast("POLYGON") |> 
  mutate(area = st_area(geometry)) |> 
  group_by(rowid) |> 
  top_n(1, area) # Converts mulitpolygon to multiple polygons, then selects the largest by area to clean it up
# plot(water_sf[1])

# water_proj <- project(crop_water, bathy_100m_3000max, align = TRUE)
# plot(water_proj)

# mask_depth <- mask(bathy_100m, water_sf)

pts_sf <- st_as_sf(bot4use, coords = c("longitude", "latitude"), crs = "EPSG:4326") |> 
  st_transform("EPSG:3338") #"+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") 
# plot(pts_sf)

pts_buff <- pts_sf |> 
  concaveman(concavity = 1, length = 1000)  |> 
  st_buffer(100000) 
# plot(pts_buff)

bound <- st_intersection(pts_buff, water_sf)

# 50 km grid for showing distribution of point data
grid_50_sf <- st_make_grid(bound, cellsize = 50000) |> # this is a 50km grid 
  st_intersection(bound) |> 
  st_sf(geometry = _) |>
  mutate(cell_id = row_number(), .before = 1)
# print(grid_sf, n = 3)

# saveRDS(grid_50_sf, file = "data/grid_50_sf.rds")

# spatial join to match cell_id-s to points,
counts_df <- st_join(pts_sf, grid_50_sf) |>
  st_drop_geometry() |>
  group_by(cell_id) |> # can look at it by mod_reg too
  summarize(points_per_cell = n(),
            mean_bias = mean(T_diff2),
            bias_sd = sd(T_diff2))

# join grid to shares for ploting
grid_counts_sf <- right_join(grid_50_sf, counts_df)

# saveRDS(grid_counts_sf, file = "data/50_k_grid_counts.rds")

ak_rus <- sf::st_read(system.file("extdata", "ak_russia.shp", package = "akgfmaps"), quiet = TRUE)
ak_can <- sf::st_read(system.file("extdata", "alaska_canada_dcw.shp", package = "akgfmaps"), quiet = TRUE)
map_bnd = grid_counts_sf %>% 
  sf::st_bbox(ak_rus)

ggplot(grid_counts_sf) +
  geom_sf(data = grid_50_sf, fill = NA, color = "red4") +
  geom_sf(aes(fill = points_per_cell)) + # can also look at mean_bias and bias_sd
  scale_fill_stepsn(colors = sp::bpy.colors(n = 10, cutoff.tails = 0.1, alpha = 1.0), n.breaks = 10) +
  geom_sf(data = ak_rus) +
  geom_sf(data = ak_can) +
  coord_sf(xlim = c(map_bnd$xmin, map_bnd$xmax),
           ylim = c(map_bnd$ymin, map_bnd$ymax), expand = TRUE) +
  annotation_scale(location = "br", width_hint = 0.4, height = unit(0.5, "cm"), pad_x = unit(3, "cm")) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering,
                         height = unit(2, "cm"), width = unit(2, "cm")) +
  labs(fill = 'N') +
  # facet_wrap(~mod_reg, scales = "free") + # Just to see that the model regions are going along with the grid
  theme_bw()

ggsave(file = "results/plots/All_raster.png", height = 10, width = 24, units = "cm")

################
# Make a 10 km grid for later use with likelihood and model
################

grid_10_sf <- st_make_grid(bound, cellsize = 10000) |> # this is a 10km grid 
  st_intersection(bound) |> 
  st_sf(geometry = _) |>
  mutate(cell_id = row_number())

grid_summ <- exact_extract(bathy_100m_3000max, grid_10_sf,
                           c('min', 'max', 'mean', 'stdev', 'count')) |> 
  mutate(cell_id = row_number())

grid_bathy_10km_sf <- left_join(grid_10_sf, grid_summ) |> 
  na.omit() |> 
  filter(stdev > 0)

# Extracts the value of smaller cells (100m x 100m in this case) for use in 
# kernel density estimates (KDE)
dat4kde <- exact_extract(bathy_100m_3000max, grid_bathy_10km_sf)

# Determines the KDE, then approximates a function for calculating densities 
# between two depths...
kde_fun <- lapply(X = dat4kde, function(x) {with(density(x[, 1], na.rm = T), approxfun(x, y, rule=1))})

# An example for calculating the likelihood that cell #100is between 990 and 1000 m
# trapzfun(kde_fun[100][[1]], 990, 1000)

# An example for calculating the likelihood that cell s between minz and maxz

minz = 816
maxz = 888

tic()
all <- tibble("likelihood" = rep(0, length(kde_fun)), "cell_id" = grid_bathy_10km_sf$cell_id)
for(i in 1:length(kde_fun)) {
  grid_dat <- grid_bathy_10km_sf[i,] |> st_drop_geometry()
  
  if(grid_dat$min > maxz & grid_dat$min > minz |
     grid_dat$max < maxz & grid_dat$max < minz) {
    next
  } 
  
  if(grid_dat$min > minz) {
    minz = grid_dat$min
  }
  
  if(grid_dat$max < maxz) {
    maxz = grid_dat$max
  }
  
  all$likelihood[i] <- trapzfun(kde_fun[i][[1]], minz, maxz)$value
}
toc()
all$likelihood <- all$likelihood / max(all$likelihood)

L_kde <- left_join(grid_bathy_10km_sf, all)
plot(L_kde["likelihood"] |> filter(likelihood > 0))

tic()
all2 <- tibble("likelihood" = rep(0, length(kde_fun)), "cell_id" = grid_bathy_10km_sf$cell_id)
all2$likelihood <- pnorm(maxz * rep(1, nrow(grid_bathy_10km_sf)),
                         mean = grid_bathy_10km_sf$mean, sd = grid_bathy_10km_sf$stdev) -
  pnorm(minz * rep(1, nrow(grid_bathy_10km_sf)),
        mean = grid_bathy_10km_sf$mean, sd = grid_bathy_10km_sf$stdev)
toc()
all2$likelihood <- all2$likelihood / max(all2$likelihood, na.rm = T)

L_norm <- left_join(grid_bathy_10km_sf, all2)
plot(L_norm["likelihood"] |> filter(likelihood > 0))

grid_bathy_10km_list <- list(grid_bathy_10km_sf, kde_fun)

saveRDS(grid_bathy_10km_list, file = "data/bathy_grid_10km_list.rds")

grid_bathy_10km_list <- readRDS(file = "data/bathy_grid_10km_list.rds")



















# Make a 50 km grid for later use with likelihood and model
################

# Start with high quality bathymetry made combining Zimmermans 100-m res with ARDEM for 100mx100m
bathy_100m_3000max <- project(rast("data/bathy_100m_3000max.tif"), "epsg:3338")

extracted_values <- exactextractr::exact_extract(bathy_100m_3000max, grid_50_sf,
                                                 c('min', 'max', 'mean', 'stdev', 'count')) |> 
  mutate(cell_id = row_number())

grid_bathy_10km_sf <- left_join(grid_10_sf, grid_summ) |> 
  na.omit() |> 
  filter(stdev > 0)
