library(doParallel)
library(dplyr)
library(terra)
library(tidyterra)
library(sf)
library(akgfmaps)

library(ggplot2)
library(forcats)
library(lubridate)
library(mgcv)

# library(cowplot)
# library(coldpool)

# Load HYCOM bathymetry
hycom_bathy <- readRDS("data/hycom_bathy.rds") # this is global so needs to be trimmed

# Set boundary for data
min_lat <- 48
max_lat <- 65
min_lon <- -190 # for west of the 180 line, keep going negative. e.g., 170 E = -190
max_lon <- -130

# Set max bottom depth to include
max_z <- 10000

# bat shrinks it down before moving to sf
h_bat <- hycom_bathy  |> 
  mutate(Latitude = round(Latitude, 4), Longitude = round(Longitude, 4)) |> 
  rename(bot_dep = bathymetry) |> 
  mutate(lat = Latitude, lon = Longitude - 360) |> 
  filter(lat > min_lat, lat < max_lat, 
         lon > min_lon, lon < max_lon,
         bot_dep > 10) |> 
  select(Latitude, Longitude, lon, lat, bot_dep)

# Load 50 km x 50 km grid to assign each HYCOM point to a cell 
bathy_rast <- unwrap(readRDS(file = "data/ak_50k_med.rds"))

h_bat_sf <- st_as_sf(h_bat, coords = c("Longitude", "Latitude"), crs = "EPSG:4326") |> 
  st_transform("EPSG:3338")

# Use akgfmaps to assign ESR and thus model regions ('auto' returns Alaska Albers)
esr_areas <- get_esr_regions(
  select.region = "esr_area",
  set.crs = "EPSG:3338"
)

bat_w_area <- st_join(h_bat_sf, esr_areas, left = TRUE) |> 
  mutate(mod_reg = ifelse(AREA_NAME == "Aleutian Islands", 'AI',
                          ifelse(AREA_NAME == "Eastern Bering Sea", 'EBS',
                                 ifelse(AREA_NAME == "Gulf of Alaska", 'GOA', NA))),
         mod_reg = ifelse(is.na(mod_reg) & lon > -155 & lat > 58, 'GOA', mod_reg),
         mod_reg = ifelse(is.na(mod_reg) & lon > -138, 'GOA', mod_reg)) |> 
  select(lon, lat, bot_dep, mod_reg) |> 
  distinct() |> 
  drop_na()

bathy_sf = st_as_sf(as.polygons(bathy_rast["cell_id"], trunc=FALSE, dissolve=FALSE)) 

grid <- st_join(bat_w_area, bathy_sf) |> 
  drop_na()

cell_join <- grid |> st_drop_geometry() 

# saveRDS(cell_join, "data/cell_join_20.rds")

##############
# Years without data do not have a specific year effect, and thus use the average year effect
# This is used inside the loop to change if needed
##############
dat_bot <- readRDS("data/bot4use.rds") |> 
  group_by(Year, mod_reg) |> 
  summarize(N = n()) |> 
  mutate(b = "bot")
dat_wc <- readRDS("data/dat4use.rds") |> 
  group_by(Year, mod_reg) |> 
  summarize(N = n()) |> 
  mutate(b = "wc")
check_dat <- bind_rows(dat_bot, dat_wc)

# Get daily data from HYCOM (stored on drive), then process through GAM etc.
# Insert the start and end dates
begin <- as.Date(paste0("2021-01-01")) # 1994-01-01
end <- as.Date(paste0("2023-12-31")) # 2019-06-07

days <- seq(begin, end, by = "day")
# Parallel processing...change cores as desired; this doesn't help when I'm pulling from an external hard drive
# registerDoParallel(parallel::detectCores() - 3)

# pull the previously downloaded HYCOM data 
# dat <- foreach(i = 1:length(days), .packages = c("lubridate", "tibble", "dplyr", "forcats")) %dopar% {
cells = list()
for(i in 1:length(days)) {
  date <- as.character(days[i])
  yr <- year(days[i])
  
  h_dat_bot <- readRDS(paste0("E:/HYCOM Data Bot/", yr, "_bot/", date, "_bot.rds")) |> 
    mutate(lon = ifelse(lon > 0, lon - 360, lon)) |>
    filter(lat > min_lat, lat < max_lat, 
           lon > min_lon, lon < max_lon)
  
  h_dat_bot_w <- readRDS(paste0("E:/HYCOM Data Bot West/", yr, "_bot_w/", date, "_bot_w.rds")) |> 
    mutate(lon = ifelse(lon > 0, lon - 360, lon)) |>
    filter(lat > min_lat, lat < max_lat, 
           lon > min_lon, lon < max_lon)
  
  h_dat_pro <- readRDS(paste0("E:/HYCOM Data/", yr, "/", date, ".rds")) |> 
    mutate(lon = ifelse(lon > 0, lon - 360, lon)) |>
    filter(lat > min_lat, lat < max_lat, 
           lon > min_lon, lon < max_lon,
           depth < (max_z + 1), depth > 10)
  
  h_dat_pro_w <- readRDS(paste0("E:/HYCOM Data West/", yr, "_w/", date, "_w.rds")) |> 
    mutate(lon = ifelse(lon > 0, lon - 360, lon)) |>
    filter(lat > min_lat, lat < max_lat, 
           lon > min_lon, lon < max_lon,
           depth < (max_z + 1), depth > 10)
  
  pro <- bind_rows(h_dat_pro, h_dat_pro_w) |> distinct() |> 
    mutate(bottom = 0, lat = round(lat, 4), lon = round(lon, 4)) |> select(!time) |> 
    left_join(cell_join) |> filter(!is.na(cell_id), !is.na(mod_reg)) |> select(!c(bot_dep))
  
  bot <- bind_rows(h_dat_bot, h_dat_bot_w) |> distinct() |> 
    mutate(lat = round(lat, 4), lon = round(lon, 4)) |> select(!time) |> 
    left_join(cell_join) |> filter(bot_dep > 10, !is.na(cell_id), !is.na(mod_reg)) |> 
    group_by(cell_id) |> 
    summarize(water_temp = mean(water_temp_bottom[bot_dep > (max(bot_dep) - 2)]),
              lon = mean(lon[bot_dep > (max(bot_dep) - 2)]),
              lat = mean(lat[bot_dep > (max(bot_dep) - 2)]),
              depth = (max(bot_dep) - 10),
              bottom = 1,
              mod_reg = mod_reg[bot_dep == max(bot_dep)])
  
  all_dat <- bind_rows(pro, bot) |> 
    mutate(date = as_date(days[i]))
  gc()
  
  all <- data.frame()
  for(b in c(0,1)) {
    mod_nm <- ifelse(b == 0, 'wc', 'bot')
    for(reg in c('AI', 'GOA', 'EBS')) {
      check <- check_dat |> filter(Year == year(date), mod_reg == reg, b == mod_nm)
      mod <- readRDS(paste0("results/model/", reg, "_", mod_nm, "_mod.rds"))

      pred_dat <- all_dat |>  
        filter(bottom == b, mod_reg == reg) |> 
        mutate(Year = year(date), Year_fct = as_factor(year(date)), Depth = depth, 
               Depth_fct2 = as_factor(ifelse(Depth < 250, "shallow", "deep")),
               HYCOM_temp = water_temp, DOY = yday(date),
               longitude = lon + 360, latitude = lat) |> 
        select(cell_id, date, Year, Year_fct, latitude, longitude, Depth, Depth_fct2, DOY, HYCOM_temp, bottom) |>  # Depth_fct, 
        distinct()
      
      if(nrow(check) > 0) {
        pred_dat$pred_diff <- predict(mod, newdata = pred_dat)
      }
        
      if(nrow(check) == 0) {
        pred_dat <- pred_dat |> 
          mutate(Year_fct = as_factor(2021))
        
        terms <- predict(mod, newdata=pred_dat, 
                         exclude = c("s(Year_fct):Depth_fct2shallow", "s(Year_fct):Depth_fct2deep"),
                         type = "terms",
                         newdata.guaranteed=TRUE)
        
        pred_dat$pred_diff <- rowSums(terms) + mod$coefficients[1]
      }
      
      pred_dat <- pred_dat |> 
        mutate(gam_temp = HYCOM_temp - pred_diff)  |>  
        mutate(gam_temp = ifelse(gam_temp < -1.8, -1.8, gam_temp)) |> 
        select(longitude, latitude, cell_id, date, Depth, HYCOM_temp, gam_temp, bottom) |> 
        rename(lon = longitude, lat = latitude, depth = Depth)
      
      all <- bind_rows(all, pred_dat)
      gc()
    }
  }
  cells[[i]] <- all |> group_by(date, cell_id, depth) |> 
    summarize(N= n(), mean_g = mean(gam_temp), mean_h = mean(HYCOM_temp),
              sd_g = sd(gam_temp), sd_h = sd(HYCOM_temp),
              min_g = min (gam_temp), min_h = min(HYCOM_temp),
              max_g = max(gam_temp), max_h =max(HYCOM_temp),
              lat = mean(lat), lon = mean(lon),
              is_bot = mean(bottom))
}

saveRDS(cells[1:365], file = "data/tdp_2021_50k.rds")
saveRDS(cells[366:730], file = "data/tdp_2022_50k.rds")
saveRDS(cells[731:1095], file = "data/tdp_2023_50k.rds")


saveRDS(cells, file = "data/tdp_21_23_50k.rds")
tdp1 <- readRDS(file = "data/tdp_98_02_50k.rds")
tdp2 <- readRDS(file = "data/tdp_data/tdp_02_03_50k.rds")
tdp3 <- readRDS(file = "data/tdp_data/tdp_03_04_50k.rds")
tdp4 <- readRDS(file = "data/tdp_data/tdp_04_09_50k.rds")

tdp <- c(tdp1, tdp2, tdp3, tdp4)
# doParallel::stopImplicitCluster()


ggplot(dat[[1]] |> filter(depth == 0), aes(lon, lat, col = mean_h)) + geom_point() + scale_color_viridis_c()
ggplot(dat[[1]] |> filter(depth == 0), aes(lon, lat, col = mean_g-mean_h)) + geom_point() + scale_color_viridis_c()
dat_df <- as.data.frame(dplyr::bind_rows(dat))




grid_50_sf <- readRDS(file = "data/grid_50_sf.rds")

dat_sf <- st_as_sf(dat, coords = c("lon", "lat"), crs = "EPSG:4326") |> 
  st_transform(crs(grid_50_sf))

day_grid <- st_join(grid_50_sf, dat_sf, join = st_nearest_feature) |>
  st_drop_geometry() |>
  group_by(cell_id, date, bottom, depth) |>
  summarize(hycom_T = mean(water_temp)) |> 
  mutate(date = dat_sf$date[1])

day_grid_sf <- left_join(grid_50_sf, day_grid)



