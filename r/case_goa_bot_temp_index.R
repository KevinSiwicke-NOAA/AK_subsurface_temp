##################
# Make daily goa bottom temp index
##################

# Load packages needed
pkgs <- c('ggplot2', 'dplyr', 'sf', 'terra', 'cowplot', 'doParallel', 'lubridate', 'forcats')
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE, quietly = TRUE)


# library(doParallel)
# library(dplyr)
# library(terra)
# library(tidyterra)
# library(sf)
# library(ggplot2)
# library(forcats)
# library(lubridate)
# library(mgcv)
# library(cowplot)

# set boundary parameters
lon_min <- -164
lon_max <- -130
lat_min <- 52
lat_max <- 62

z_min <- 20
z_max <- 250

# load functions for dealing with HYCOM
source('r/functions/hycom_functions.r/')

# Get bathymetry layer
hycom_bathy <- get_hycom_bathy(limits = c(lon_min, lon_max, lat_min, lat_max)) |> 
  filter(depth >= (z_min - 10), depth <= (z_max - 10)) |> # the depth of the temperature is 10m off bottom, so actual bottom depth is 20 to 250 m
  mutate(bot_dep = depth + 10, latitude = lat, longitude = lon,
         lat = round(lat, 4), lon = round(lon, 4)) # These are a second set that get converted into geometry

# Load map of GOA NMFS areas to assist in crop
goa_map <- akgfmaps::get_nmfs_areas(set.crs = 'auto') |> 
  filter(REP_AREA > 609 & !REP_AREA == 649 & !REP_AREA == 659) |> 
  mutate(esr = ifelse(REP_AREA < 635, "WGOA", "EGOA")) |> 
  select(!AREA_M2)

# convert bathymetry layer to sf
bat_sf <- st_as_sf(hycom_bathy, coords = c("longitude", "latitude"), crs = "EPSG:4326") |>
  st_transform(crs(goa_map))

# bat_goa includes the WGOA and EGOA only, with a depth of 20m to 250 m 
# This is the area included in this index
bat_goa <- st_join(bat_sf, goa_map) |> 
  filter(!is.na(esr)) |> 
  st_drop_geometry() 

# Insert the start and end dates for pulling daily data
begin <- as.Date(paste0("1994-01-01")) # 1994-01-01
end <- as.Date(paste0("2023-12-31")) 

days <- seq(begin, end, by="days")

# pull the previously downloaded HYCOM data, about 15 minutes if you already have it
# Parallel processing...change cores as desired
registerDoParallel(parallel::detectCores() - 2)

dat_bot <- foreach(i = 1:length(days), .packages = c("lubridate", "tibble", "dplyr")) %dopar% {
  date <- as.character(days[i])
  yr <- year(days[i])
  h_dat_all <- readRDS(paste0("D:/HYCOM Data Bot/", yr, "_bot/", date, "_bot.rds")) |> 
    mutate(date = as_date(days[i]),
           lon = ifelse(lon > 0, lon - 360, lon),
           lon = round(lon, 4), lat = round(lat, 4)) |>
    tibble::as_tibble() 
  
  bot_250 <- left_join(bat_goa, h_dat_all)
}

doParallel::stopImplicitCluster()

# Alternatively, you can download it, which will take a long time initially, probably run over night to accomplish
h_dat_all <- get_hycom_temp(date_begin = as.Date(begin), 
               date_end = as.Date(end), 
               lat_min = lat_min, lat_max = lat_max, 
               lon_min = lon_min, lon_max = lon_max,
               z_min = 0, z_max = 250,
               include_wc = FALSE, # make true if you want water column temperature profiles
               include_bot = TRUE)


  
  

# convert large list into a data frame
mhw_dat_bot_df <- as.data.frame(dplyr::bind_rows(dat_bot)) |> 
  select(!c(bot_dep, REP_AREA, time))

rm(dat_bot); gc()
# saveRDS(mhw_dat_bot_df, file = "results/mhw_dat_bot_df_250.rds", compress = TRUE)

########### Can load data detailed data (LARGE)
# mhw_dat_bot_df <- readRDS(file = "results/mhw_dat_bot_df_250.rds") # This one is subset to 250m


# dat_bot <- bot4use |> filter(mod_reg == "GOA")
#   
# k_yr = length(unique(dat_bot$Year))
# b_mod <- bam(T_diff2 ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) +
#                s(Year_fct, by = Depth_fct2, bs = "re"),
#              data = dat_bot, method = 'fREML',
#              nthreads = c(4, 1), discrete = TRUE)
# 
# saveRDS(b_mod, paste0("results/model/mhw_bot_mod.rds"))

# Prepare data for gam predictions
mhw_pred_dat <- mhw_dat_bot_df |>  
  mutate(Year_fct = factor(year(date)), 
         Depth_fct2 = "shallow", # since these are all in 250m or less
         DOY = yday(date)) |> 
  select(date, Year_fct, latitude = lat, longitude = lon, Depth = depth, Depth_fct2, DOY, HYCOM_temp = water_temp_bottom, esr) %>% # Depth_fct, 
  distinct()

mod_b_goa <- readRDS(paste0('results/model/GOA_bot_mod.rds'))
# Need to fill in years without data with the 'average' year effect
goa_yr_w_dat <- c(1996, 1999, 2001, 2003, 2005, 2006, 2007, 2008, 2009, 2010, 
                  2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)

yr_w_dat <- mhw_pred_dat |> filter(Year_fct %in% goa_yr_w_dat)
yr_w_dat$pred_diff <- predict(mod_b_goa, newdata = yr_w_dat)

# Now for years not in original data, we can predict temps using average effect of year
# Because year is a factor, we have to tell the model the year is one that existed in the data
# so we set it to 2021, but then we exclude the year effect term.
yr_wo_dat <- mhw_pred_dat |> filter(!Year_fct %in% goa_yr_w_dat) |> 
  mutate(Year_fct = as_factor(2021))

terms <- predict(mod_b_goa, newdata=yr_wo_dat, 
                 exclude = c("s(Year_fct):Depth_fct2shallow", "s(Year_fct):Depth_fct2deep"),
                 type = "terms",
                 newdata.guaranteed=TRUE)

yr_wo_dat$pred_diff <- rowSums(terms) + mod_b_goa$coefficients[1]

## ALT???
pred <- predict(mod_b_goa, newdata=yr_wo_dat,
               re.form = NA,  # Excludes random effects
               se.fit = TRUE)
# A plot of the random effect by year...
# plot(unique(goa_dat_bot$Year), mod_b_goa$coefficients[402:422])

# gam_wts <- c(seq(0, 1, 1/139), rep(1, 114), seq(1, 0, -1/111))
# hy_wts <- 1 - gam_wts

mhw_pred_dat <- bind_rows(yr_w_dat, yr_wo_dat) |> 
  mutate(gam_temp = HYCOM_temp - pred_diff) |>  
  mutate(gam_temp = ifelse(gam_temp < -1.8, -1.8, gam_temp)) |> 
  mutate(wt_temp = gam_temp * gam_wts[DOY] + HYCOM_temp * hy_wts[DOY])

# get the mean bottom temp by each day and WGOA/EGOA
point_mean <- mhw_pred_dat |> 
  mutate(esr = factor(esr, levels = c("WGOA", "EGOA"))) |> 
  group_by(esr, longitude, latitude) |> 
  summarize(hycom_mean = mean(HYCOM_temp),
            hycom_sd = sd(HYCOM_temp),
            gam_mean = mean(gam_temp),
            gam_sd = sd(gam_temp),
            wt_mean = mean(wt_temp),
            wt_sd = sd(wt_temp))

daily_scaled_anom <- left_join(mhw_pred_dat, point_mean) |> 
  mutate(hycom_anom = ((HYCOM_temp - hycom_mean) / hycom_sd),
         gam_anom = ((gam_temp - gam_mean) / gam_sd),
         wt_anom = ((wt_temp - wt_mean) / wt_sd))

daily_mean_anom <- daily_scaled_anom |> 
  mutate(esr = factor(esr, levels = c("WGOA", "EGOA"))) |> 
  group_by(esr, date) |> 
  summarize(t = min(date),
            hycom_temp = mean(hycom_anom),
            gam_temp = mean(gam_anom),
            wt_temp = mean(wt_anom)) |> 
  mutate(month=month(date),
         day=day(date),
         year=year(date),
         newdate = as.Date(paste("2000", month, day, sep = "-"), format = "%Y-%m-%d"))

saveRDS(daily_mean_anom, file = "results/mhw_daily_mean_anom_250.rds", compress = TRUE)

daily_mean <- mhw_pred_dat |> 
  mutate(esr = factor(esr, levels = c("WGOA", "EGOA"))) |> 
  group_by(esr, date) |> 
  summarize(t = min(date),
            hycom_temp = mean(HYCOM_temp),
            gam_temp = mean(gam_temp),
            wt_temp = mean(wt_temp)) |> 
  mutate(month=month(date),
         day=day(date),
         year=year(date),
         newdate = as.Date(paste("2000", month, day, sep = "-"), format = "%Y-%m-%d"))

saveRDS(daily_mean, file = "results/mhw_daily_mean_250.rds", compress = TRUE)

################
# Start here with daily means to save time
################
daily_mean <- readRDS(file = "results/mhw_daily_mean_250.rds") # 250 verions?
daily_mean_anom <- readRDS(file = "results/mhw_daily_mean_anom_250.rds") # 250 verions?

# Daily means no correction related to depth
mn <- ggplot(daily_mean) + 
  geom_line(aes(t, hycom_temp)) +
  geom_line(aes(t, gam_temp), col = "red") + 
  xlab("Year") +
  ylab(expression('Mean bottom temperature '~(degree*C))) +
  facet_wrap(~esr) +
  theme_bw()

mn
ggsave(paste0("results/plots/mhw_bt_mean_250.png"), bg = "white", width = 16, height = 8, units = 'cm', dpi = 300)

# Anomalies scaled at each point to account for depth difference
anom <- ggplot(daily_mean_anom) + 
  geom_line(aes(t, hycom_temp)) +
  geom_line(aes(t, gam_temp), col = "red") +
  geom_hline(aes(yintercept = 0), lty = 2) +
  xlab("Year") +
  ylab(expression('Scaled bottom temperature anomaly '~(degree*C))) +
  facet_wrap(~esr) +
  theme_bw()

anom
ggsave(paste0("results/plots/mhw_bt_scaled_anomaly.png"), bg = "white", width = 16, height = 8, units = 'cm', dpi = 300)

plot_grid(mn + theme(axis.title.x=element_blank(), axis.text.x=element_blank()), 
          anom + theme(strip.background = element_blank(), strip.text.x = element_blank()),
          ncol = 1)

ggsave(paste0("results/plots/mhw_final_figure.png"), bg = "white", width = 20, height = 18, units = 'cm', dpi = 600)

# Weighted by DOY of year GAM corrected 
ggplot(daily_mean) +
  geom_line(aes(t, hycom_temp)) +
  geom_line(aes(t, wt_temp), col = "blue") + 
  facet_wrap(~esr, scales = "free") +
  theme_bw()

# Anomalies scaled at each point to account for depth difference
ggplot(daily_mean_anom) + 
  geom_line(aes(t, hycom_temp)) +
  geom_line(aes(t, wt_temp), col = "blue") + 
  xlab("Year") +
  ylab(expression('Mean BT Anomaly '~(degree*C))) +
  facet_wrap(~esr) +
  theme_bw()

ggplot(daily_mean) + 
  geom_point(aes(gam_temp, wt_temp, col = year)) +
  facet_wrap(~esr) +
  theme_bw()

