# This script laods bottom and water column temperature observations
# that is paired with HYCOM predicted temperature. Then by EBS, AI, and GOA
# data are spatially blocked for 10-fold cross validation.

library(ggplot2) # if you want to see some plots that are commented out

# load bottom data
bot4use <- readRDS("data/bot4use.rds")
b_stn <- bot4use |> 
  dplyr::distinct(stn_id, longitude, latitude, mod_reg)

# load water column data (remove 1994 and 1997 which have very few data points)
wc4use <- readRDS("data/dat4use.rds") 
wc_stn <- wc4use |> 
  dplyr::distinct(stn_id, longitude, latitude, mod_reg)

all_stn <- dplyr::full_join(b_stn, wc_stn)

# Get the variogram to determine a distance at which spatial autocorrelation is occurring
# This just uses 25m, 50m and 100m for the water column and 
# the bottom as that covers most data and only has 1 point per location
# and it encompasses many depths
# dat_sf <- wc4use |>
#   filter(Depth == 25) |> 
#   st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>
#   st_transform("EPSG:3338") # Alaska Albers projection
# 
# sac <- cv_spatial_autocor(x = dat_sf, column = "T_diff") # ~173 km
# 
# png(filename="results/semivariogram_25m.png", units = 'cm', height = 10, width = 20, res = 600)
# plot(sac$variograms[[1]]) # variogram
# dev.off()
# 
# dat_sf <- wc4use |>
#   filter(Depth == 50) |> 
#   st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>
#   st_transform("EPSG:3338") # Alaska Albers projection
# 
# sac <- cv_spatial_autocor(x = dat_sf, column = "T_diff") # ~97 km
# 
# png(filename="results/semivariogram_50m.png", units = 'cm', height = 10, width = 20, res = 600)
# plot(sac$variograms[[1]]) # variogram
# dev.off()
# 
# dat_sf <- wc4use |>
#   filter(Depth == 100) |> 
#   st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>
#   st_transform("EPSG:3338") # Alaska Albers projection
# 
# sac <- cv_spatial_autocor(x = dat_sf, column = "T_diff") # ~114 km
# 
# png(filename="results/semivariogram_100m.png", units = 'cm', height = 10, width = 20, res = 600)
# plot(sac$variograms[[1]]) # variogram
# dev.off()
# 
# dat_sf <- bot4use |>
#   st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>
#   st_transform("EPSG:3338") # Alaska Albers projection
# 
# sac <- cv_spatial_autocor(x = dat_sf, column = "T_diff2") # ~128 km
# 
# png(filename="results/semivariogram_bot.png", units = 'cm', height = 10, width = 20, res = 600)
# plot(sac$variograms[[1]]) # variogram
# dev.off()

# Set seed for reproducibility
set.seed(754813)

# Setup and block out by region
reg <- b_stn |> 
  dplyr::distinct(mod_reg)
b_dat_reg <- vector("list", nrow(reg))
wc_dat_reg <- vector("list", nrow(reg))

for( i in 1:nrow(reg) ) {
  block_dat_sf <- all_stn |> 
    dplyr::filter(mod_reg == reg$mod_reg[i]) |>  
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>  
    sf::st_transform("EPSG:3338")
  
  sp_cv <- blockCV::cv_spatial(block_dat_sf, size = 130000, k = 10, iteration = 500) # use 130 km per the estimated 128km above
  
  b_dat_sf <- b_stn |> 
    dplyr::filter(mod_reg == reg$mod_reg[i]) |>  
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>  
    sf::st_transform("EPSG:3338") |> 
    sf::st_join(sp_cv$blocks) |> 
    dplyr::mutate(grp = folds)
  
  wc_dat_sf <- wc_stn |> 
    dplyr::filter(mod_reg == reg$mod_reg[i]) |> 
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>  
    sf::st_transform("EPSG:3338") |> 
    sf::st_join(sp_cv$blocks) |> 
    dplyr::mutate(grp = folds)
  
  # ggplot(b_dat_sf) + 
  #   geom_sf(aes(col = factor(grp))) + 
  #   geom_sf(data = sp_cv$blocks, fill = NA) +
  #   theme_bw()
  # 
  # ggsave(paste0(file = "results/plots/bot_blocks_", reg$mod_reg[i], ".png"), dpi = 600)
  
  b_dat_reg[[i]] <- dplyr::left_join(bot4use |> dplyr::filter(mod_reg == reg$mod_reg[i]), 
                                     data.frame(b_dat_sf), relationship = "many-to-many")
  
  # ggplot(wc_dat_sf) +
  #   geom_sf(aes(col = factor(grp))) +
  #   geom_sf(data = sp_cv$blocks, fill = NA) +
  #   theme_bw()
  # 
  # ggsave(paste0(file = "results/plots/wc_blocks_", reg$mod_reg[i], ".png"), dpi = 600)
  
  wc_dat_reg[[i]] <- dplyr::left_join(wc4use |> dplyr::filter(mod_reg == reg$mod_reg[i]), 
                                      data.frame(wc_dat_sf), relationship = "many-to-many")
}

# For plotting all together, just to look at 
b_grp_dat = as.data.frame(dplyr::bind_rows(b_dat_reg)) 
wc_grp_dat = as.data.frame(dplyr::bind_rows(wc_dat_reg))

# ggplot(b_grp_dat, aes(longitude, latitude, col = factor(grp))) + geom_point() +
#   theme_bw()
# 
# ggplot(wc_grp_dat, aes(longitude, latitude, col = factor(grp))) + geom_point() +
#   theme_bw()

# ggplot(b_grp_dat) + geom_histogram(aes(grp)) +
#   facet_wrap(~mod_reg)
# 
# ggplot(wc_grp_dat) + geom_histogram(aes(grp)) +
#   facet_wrap(~mod_reg)