################
# Figures for GAK1 exploration
################
# Load packages needed
pkgs <- c('ggplot2', 'dplyr', 'sf', 'ggspatial', 'cowplot', 'gratia', 'tidyr', 'lubridate', 'rerddap', 'stringr', 'data.table')
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE, quietly = TRUE)

# Top script commented out, methods used to download and process GAK1 data
# Years 2019 to 2023 changed data storage from .csv to .dat and were processed separately
# urls <- c("https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_1998-1999.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_1999-2000.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2000-2002.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2002-2003_corrected.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2004-2005.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2005-2006.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2006-2007.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2007-2008.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2008-2009.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2009-2010.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2010-2011.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2011-2012.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2012-2013.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2013-2014.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2014-2015.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2015-2016.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2016-2017.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2017-2018.zip",
#           "https://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_2018-2019.zip")
# 
# desired_cols <- c("Date_Time_.UTC.", "Depth_.m.", "Temperature_.C.")
# 
# # helper to canonicalize names for fuzzy matching (optional)
# canon <- function(x) tolower(gsub("[^a-z0-9]+", "_", trimws(x)))
# 
# desired_can <- canon(desired_cols)
# 
# # Final container
# final_list <- vector("list", length(urls))
# final_k <- 0L
# 
# for (u in urls) {
#   temp_zip <- tempfile(fileext = ".zip")
#   temp_dir  <- tempfile()
#   dir.create(temp_dir)
# 
#   try({
#     download.file(u, temp_zip, mode = "wb")
#     unzip(temp_zip, exdir = temp_dir)
# 
#     csv_files <- list.files(path = temp_dir, pattern = "\\.csv$", full.names = TRUE)
#     if (length(csv_files) == 0L) {
#       csv_files <- list.files(path = temp_dir, pattern = "\\.dat$", full.names = TRUE)
#       if (length(csv_files) == 0L) {
#         temp_dir <- paste0(temp_dir, "\\Processed")
#         csv_files <- list.files(path = temp_dir, pattern = "\\.dat$", full.names = TRUE)
#         if (length(csv_files) == 0L) {
#           message("no csv or dat in ", u)
#         next
#         }
#       }
#     }
# 
#     # per-zip list of selected rows
#     lst <- vector("list", length(csv_files))
#     k <- 0L
# 
#     for (f in csv_files) {
#       hdr <- names(fread(f, nrows = 0, showProgress = FALSE))
#       hdr_can <- canon(hdr)
#       take_idx <- match(desired_can, hdr_can)
#       present <- which(!is.na(take_idx))
#       if (length(present) == 0L) next
# 
#       take_names <- hdr[take_idx[present]]
#       dt <- fread(f, select = take_names, fill = TRUE, showProgress = FALSE)
# 
#       # rename read columns back to desired output names
#       read_to_desired <- setNames(desired_cols[present], take_names)
#       setnames(dt, names(read_to_desired), read_to_desired)
# 
#       missing <- setdiff(desired_cols, names(dt))
#       if (length(missing)) dt[, (missing) := NA]
#       setcolorder(dt, desired_cols)
# 
#       k <- k + 1L
#       lst[[k]] <- dt
#     }
# 
#     combined_zip <- if (k == 0L) data.table() else rbindlist(lst[1:k], use.names = TRUE, fill = TRUE)
# 
#     # append this zip's data to final_list
#     if (nrow(combined_zip) > 0L) {
#       final_k <- final_k + 1L
#       final_list[[final_k]] <- combined_zip
#     }
#   }, silent = FALSE)
# 
#   # cleanup
#   unlink(temp_zip)
#   unlink(temp_dir, recursive = TRUE)
#   gc()
# }
# 
# # combine everything
# combined_all <- if (final_k == 0L) data.table() else rbindlist(final_list[1:final_k], use.names = TRUE, fill = TRUE)
# 
# # Convert UTC to AK time, so we can group by day to get a sense of variance
# all_gak1 <- combined_all |>
#   mutate(date = as_date(with_tz(Date_Time_.UTC., "America/Anchorage"))) |>
#   rename(depth = Depth_.m., temperature = Temperature_.C.) |>
#   filter(depth >= 20, !is.nan(temperature)) |>
#   select(!Date_Time_.UTC.)
# 
# saveRDS(all_gak1, "data/all_gak1.rds")

# load GAK1 data preprocessed, and select all depths > 244 m to get a 'bottom' temperature
gak1_obs_bot <- readRDS(file = "data/all_gak1.rds") |> 
  filter(depth > 244) |>
  group_by(date) |> 
  summarize(OBS = mean(temperature), mn_dep = mean(depth)) |> 
  mutate(Level = '250m')

# load GAK1 data preprocessed, and select all depths between 170 and 200 to get a closer depth to the model hindcasts (184)
gak1_obs_alt <- readRDS(file = "data/all_gak1.rds") |> 
  filter(depth > 170, depth < 201) |>
  group_by(date) |> 
  summarize(OBS = mean(temperature), mn_dep = mean(depth)) |> 
  mutate(Level = '200m')

gak1_obs <- bind_rows(gak1_obs_bot, gak1_obs_alt)

#####
# Alternate way to get GAK1 data, but already heavily processed to day and interpolated depths, and has much less data. Still very similar.
# # 1. Define the AOOS ERDDAP server URL
# aoos_url <- "https://erddap.aoos.org/erddap/"
# 
# # 2. Fetch the GAK1 Mooring dataset 
# # We fetch the required variables: time, depth, and sea_water_temperature
# gak1_raw <- tabledap(
#   "gak1-mooring", 
#   url = aoos_url,
#   fields = c("time", "z", "sea_water_temperature")
# )
# 
# # 3. Process into a daily time step for bottom temperatures
# gak1_obs <- data.frame(gak1_raw) |> 
#   mutate(obs_depth = -z, date = as.Date(time)) |> 
#   rename(OBS = sea_water_temperature) |> 
#   filter(obs_depth == 245) |> 
#   select(date, obs_depth, OBS)

# Load daily hindcast of bottom temperature at GAK1 from HYCOM and BCM

daily_gak1_pred <- readRDS(file = "data/gak1_pred_bot_temp.rds") |> 
  rename(date = t, HYCOM_pred = hycom_temp, BCM_pred = gam_temp) 

# Combine hindcasts with observations, plot time series, and calculate the residual error of all the data
gak1_ts <- left_join(gak1_obs, daily_gak1_pred) |> 
  rename(BCM = BCM_pred, HYCOM = HYCOM_pred, Mooring = OBS) |> 
  pivot_longer(cols = c('HYCOM', 'BCM', 'Mooring'), names_to = c('Source'), values_to = "Temperature") |> 
  mutate(Source = factor(Source, levels = c('Mooring', 'HYCOM', 'BCM')))

ggplot(gak1_ts) +
  geom_point(aes(date, Temperature, col = Source), size = 0.3) +
  scale_color_manual(values = c('black', 'steelblue', 'firebrick')) +
  facet_wrap(~Level, ncol = 1) +
  theme_bw() +
  labs(y = expression('Temperature'~(degree*C)), x = 'Date', color = NULL) +
  ggtitle('GAK1') +
  guides(col = guide_legend(ncol = 3)) +
  theme(legend.position = 'inside', legend.text = element_text(size = 12), 
        legend.position.inside = c(0.8, 0.04), axis.title = element_text(size = 12),
        axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        strip.background = element_blank())

ggsave('results/plots/gak1_ts.png', units = 'cm', height = 28, width = 28, dpi = 300)

gak1_comp <- left_join(gak1_obs |> filter(Level == '250m'), daily_gak1_pred) |> 
  mutate(BCM = BCM_pred - OBS, HYCOM = HYCOM_pred - OBS) |> 
  mutate(best = ifelse(abs(HYCOM) < abs(BCM), HYCOM, BCM))

gak1_comp_long <- gak1_comp |> 
  pivot_longer(cols = c('HYCOM', 'BCM'), names_to = c('Source'), values_to = "resid") |> 
  mutate(Source = factor(Source, levels = c('HYCOM', 'BCM')))

ggplot(gak1_comp_long) +
  geom_tile(aes(x = DOY, y = factor(Year), fill = resid), lty = 2) +
  geom_rect(aes(xmin=136, xmax=273, ymin = 0.51, ymax = 25.49), fill = NA, col = "black", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "steelblue", 
    mid = "grey90", 
    high = "firebrick", 
    midpoint = 0,
    name = "Error",
    na.value = 'white',
    limits = c(-2.7, 2.7)
  ) +
  scale_x_continuous(breaks = seq(1, 365, by = 30), expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  facet_wrap(~Source, ncol = 1) +
  labs(x = "Day of Year", y = "Year") +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "bottom", 
        legend.key.width = unit(1.5, "cm"), strip.background = element_blank())

ggsave('results/plots/Fig6_hindcasts_errors_all_250.png', units = 'cm', height = 18, width = 24, dpi = 300)

# All error residuals with the 'best' highlighted.
ggplot(gak1_comp_long) + 
  geom_rect(aes(xmin=136, xmax=273, ymin = -2.7, ymax = 2.7), alpha=.1, fill = 'grey70') +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(aes(DOY, resid, col = Source), alpha = 0.1) +
  geom_point(data = gak1_comp_long |> filter(resid == best), aes(DOY, resid, col = Source)) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  xlab("Day of year") + 
  facet_wrap(~Year, scale = 'free_y') +
  scale_y_continuous(limits = c(-2.7, 2.7), expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 366), expand = c(0,0)) + 
  ylab(expression('Hindcast - observation'~(degree*C))) +
  ggtitle("GAK1 - 250 m") +
  theme_bw() +
  guides(col = guide_legend(nrow = 1)) +
  theme(panel.grid = element_blank(), legend.position = 'bottom', legend.text = element_text(size = 12),
        legend.title=element_blank(), strip.text = element_text(size = 12), axis.text = element_text(size = 12), axis.title = element_text(size = 12),
        strip.background = element_blank())

ggsave('results/plots/hindcast_error_all_yrs_250.png', units = 'cm', height = 20, width = 30, dpi = 300)

# Summarizre by DOY
# gak1_rmse <- gak1_comp |> group_by(DOY) |> summarize(HYCOM = sqrt(mean((HYCOM_pred - OBS)^2, na.rm = T)), BCM = sqrt(mean((BCM_pred - OBS)^2, na.rm = T))) |> 
#   mutate(best = ifelse(abs(HYCOM) < abs(BCM), HYCOM, BCM)) |> 
#   pivot_longer(cols = c('HYCOM', 'BCM'), names_to = c('Source'), values_to = "rmse") |> 
#   mutate(Source = factor(Source, levels = c('HYCOM', 'BCM')))
# 
# # Line plot with points indicating the 'best' model
# ggplot(gak1_rmse) + 
#   geom_rect(aes(xmin=136, xmax=273, ymin = 0, ymax = 1.1), alpha=.1, fill = 'grey70') +
#   geom_line(aes(DOY, rmse, col = Source)) +
#   geom_point(data = gak1_rmse |> filter(rmse == best), aes(DOY, rmse, col = Source)) +
#   scale_color_manual(values = c("steelblue", "firebrick")) + 
#   scale_x_continuous(limits = c(0, 366), expand = c(0,0)) + 
#   scale_y_continuous(limits = c(0, 1.1), expand = c(0,0)) + 
#   theme_bw() +
#   labs(x = 'Day of year', y = 'RMSE') +
#   theme(panel.grid = element_blank())
# 
# gak1_mae <- gak1_comp |> group_by(DOY) |> summarize(HYCOM = mean(abs(HYCOM_pred - OBS), na.rm = T), BCM = mean(abs(BCM_pred - OBS), na.rm = T)) |> 
#   mutate(best = ifelse(abs(HYCOM) < abs(BCM), HYCOM, BCM)) |> 
#   pivot_longer(cols = c('HYCOM', 'BCM'), names_to = c('Source'), values_to = "mae") |> 
#   mutate(Source = factor(Source, levels = c('HYCOM', 'BCM')))
# 
# ggplot(gak1_mae) + 
#   geom_rect(aes(xmin=136, xmax=273, ymin = 0, ymax = 0.8), alpha=.1, fill = 'grey70') +
#   geom_line(aes(DOY, mae, col = Source)) +
#   geom_point(data = gak1_mae |> filter(mae == best), aes(DOY, mae, col = Source)) +
#   scale_color_manual(values = c("steelblue", "firebrick")) + 
#   scale_x_continuous(limits = c(0, 366), expand = c(0,0)) + 
#   scale_y_continuous(limits = c(0, 0.8), expand = c(0,0)) + 
#   theme_bw() +
#   labs(x = 'Day of year', y = 'MAE') +
#   theme(panel.grid = element_blank())
# 
# gak1_bias_mn <- gak1_comp |> group_by(DOY) |> summarize(HYCOM = mean(HYCOM_pred - OBS, na.rm = T), BCM = mean(BCM_pred - OBS, na.rm = T)) |> 
#   mutate(best = ifelse(abs(HYCOM) < abs(BCM), HYCOM, BCM)) |> 
#   pivot_longer(cols = c('HYCOM', 'BCM'), names_to = c('Source'), values_to = "bias") |> 
#   mutate(Source = factor(Source, levels = c('HYCOM', 'BCM')))
# 
# gak1_bias_sd <- gak1_comp |> group_by(DOY) |> summarize(HYCOM = sd(OBS-HYCOM_pred, na.rm = T), BCM = sd(OBS-BCM_pred, na.rm = T)) |> 
#   pivot_longer(cols = c('HYCOM', 'BCM'), names_to = c('Source'), values_to = "sd") |> 
#   mutate(Source = factor(Source, levels = c('HYCOM', 'BCM')))
# 
# gak1_bias <- left_join(gak1_bias_mn, gak1_bias_sd)
# 
# ggplot(gak1_bias) + 
#   geom_rect(aes(xmin=136, xmax=273, ymin = -1.6, ymax = 1.4), alpha=.1, fill = 'grey70') +
#   geom_line(aes(DOY, bias, col = Source)) +
#   geom_ribbon(aes(x = DOY, ymin = bias - sd, ymax = bias + sd, fill = Source), alpha = 0.2) +
#   geom_point(data = gak1_bias |> filter(bias == best), aes(DOY, bias, col = Source)) +
#   scale_color_manual(values = c("steelblue", "firebrick")) + 
#   scale_fill_manual(values = c("steelblue", "firebrick")) + 
#   scale_x_continuous(limits = c(0, 366), expand = c(0,0)) + 
#   scale_y_continuous(limits = c(-1.6, 1.4), expand = c(0,0)) +
#   theme_bw() +
#   labs(x = 'Day of year', y = 'Bias') +
#   geom_hline(yintercept = 0, lty = 2) +
#   theme(panel.grid = element_blank())
# 
# gak1_bias_rmse <- left_join(gak1_bias_mn, gak1_rmse |> select(!best))
# 
# ggplot(gak1_bias_rmse) + 
#   geom_rect(aes(xmin=136, xmax=273, ymin = -2.1, ymax = 1.5), alpha=.1, fill = 'grey70') +
#   geom_line(aes(DOY, bias, col = Source)) +
#   geom_ribbon(aes(x = DOY, ymin = bias - rmse, ymax = bias + rmse, fill = Source), alpha = 0.2) +
#   geom_point(data = gak1_bias_rmse |> filter(bias == best), aes(DOY, bias, col = Source)) +
#   scale_color_manual(values = c("steelblue", "firebrick")) + 
#   scale_fill_manual(values = c("steelblue", "firebrick")) + 
#   scale_x_continuous(limits = c(0, 366), expand = c(0,0)) + 
#   scale_y_continuous(limits = c(-2.1, 1.5), expand = c(0,0)) +
#   theme_bw() +
#   ggtitle('GAK1 - 250-m') + 
#   labs(x = 'Day of year', y = 'Bias') +
#   geom_hline(yintercept = 0, lty = 2) +
#   theme(panel.grid = element_blank())

# ggsave('results/plots/hindcasts_errors_doy.png', units = 'cm', height = 12, width = 22, dpi = 300)

# # Summarize by month
# gak1_rmse <- gak1_comp |> group_by(Month = month(date), Year) |> summarize(HYCOM = Metrics::rmse(OBS, HYCOM), BCM = Metrics::rmse(OBS, BCM))
# ggplot(gak1_rmse, aes(Month, HYCOM)) + 
#   geom_line(col = 'steelblue', size = 1) +
#   geom_point(col = 'steelblue') +
#   geom_line(aes(Month, BCM), col = 'firebrick', size = 1) + 
#   geom_point(aes(Month, BCM), col = 'firebrick') +
#   scale_x_continuous(breaks = 1:12, labels = month.abb) +
#   facet_wrap(~Year) + 
#   theme_bw() +
#   theme(panel.grid = element_blank())
# 
# gak1_rmse_box <- gak1_rmse |>  ungroup() |> 
#   pivot_longer(cols = c('BCM', 'HYCOM'), names_to = 'source', values_to = 'err')
# 
# ggplot(gak1_rmse_box, aes(x = factor(Month), y = err, fill = source)) + 
#   geom_boxplot(position = position_dodge(0.8)) +
#   scale_fill_manual(values = c('firebrick', 'steelblue')) +
#   # scale_x_continuous(breaks = 1:12, labels = month.abb) +
#   theme_bw() +
#   theme(panel.grid = element_blank())
# 
# gak1_mae <- gak1_comp |> group_by(Month = month(date), Year) |> summarize(hy = Metrics::mae(OBS, HYCOM), bcm = Metrics::mae(OBS, BCM))
# 
# ggplot(gak1_mae, aes(Month, hy)) + 
#   geom_line(col = 'steelblue', size = 1) +
#   geom_point(col = 'steelblue') +
#   geom_line(aes(Month, bcm), col = 'firebrick', size = 1) + 
#   geom_point(aes(Month, bcm), col = 'firebrick') +
#   scale_x_continuous(breaks = 1:12, labels = month.abb) +
#   facet_wrap(~Year) + 
#   theme_bw() +
#   theme(panel.grid = element_blank())
# 
# gak1_bias <- gak1_comp |> group_by(Month = month(date), Year) |> summarize(HYCOM = Metrics::bias(OBS, HYCOM), BCM = Metrics::bias(OBS, BCM))
# ggplot(gak1_bias, aes(Month, HYCOM)) + 
#   geom_line(col = 'steelblue', size = 1) +
#   geom_point(col = 'steelblue') +
#   geom_line(aes(Month, BCM), col = 'firebrick', size = 1) + 
#   geom_point(aes(Month, BCM), col = 'firebrick') +
#   scale_x_continuous(breaks = 1:12, labels = month.abb) +
#   facet_wrap(~Year) + 
#   theme_bw() +
#   theme(panel.grid = element_blank()) + 
#   geom_hline(yintercept = 0, lty = 2)
# 
# gak1_bias_box <- gak1_bias |>  ungroup() |> 
#   pivot_longer(cols = c('BCM', 'HYCOM'), names_to = 'source', values_to = 'bias')
# 
# ggplot(gak1_bias_box, aes(x = factor(Month), y = bias, fill = source)) + 
#   geom_boxplot(position = position_dodge(0.8)) +
#   scale_fill_manual(values = c('firebrick', 'steelblue')) +
#   # scale_x_continuous(breaks = 1:12, labels = month.abb) +
#   theme_bw() +
#   theme(panel.grid = element_blank()) + 
#   geom_hline(yintercept = 0, lty = 2)

#######
# Alternatively, use a slightly shallower depth because of misalignment
#######
# Combine hindcasts with observations, and calculate the residual error of all the data
gak1_comp <- left_join(gak1_obs|> filter(Level == '200m'), daily_gak1_pred) |> 
  filter(!is.na(Year)) |> 
  mutate(BCM = BCM_pred - OBS, HYCOM = HYCOM_pred - OBS) |> 
  mutate(best = ifelse(abs(HYCOM) < abs(BCM), HYCOM, BCM))

gak1_comp_long <- gak1_comp |> 
  pivot_longer(cols = c('HYCOM', 'BCM'), names_to = c('Source'), values_to = "resid") |> 
  mutate(source = factor(Source, levels = c('HYCOM', 'BCM')))

ggplot(gak1_comp_long) +
  geom_tile(aes(x = DOY, y = factor(Year), fill = resid), lty = 2) +
  geom_rect(aes(xmin=136, xmax=273, ymin = 0.51, ymax = 26.49), fill = NA, col = "black", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "steelblue", 
    mid = "grey90", 
    high = "firebrick", 
    midpoint = 0,
    name = "Error",
    # na.value = 'grey80', 
    limits = c(-2.7, 2.7)
  ) +
  scale_x_continuous(breaks = seq(1, 365, by = 30), expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  facet_wrap(~source, ncol = 1) +
  labs(x = "Day of Year", y = "Year") +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "bottom", 
        legend.key.width = unit(1.5, "cm"), strip.background = element_blank())

ggsave('results/plots/hindcasts_errors_all_200.png', units = 'cm', height = 18, width = 24, dpi = 300)

# All error residuals with the 'best' highlighted.
ggplot(gak1_comp_long) + 
  geom_rect(aes(xmin=136, xmax=273, ymin = -2.7, ymax = 2.7), alpha=.1, fill = 'grey70') +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(aes(DOY, resid, col = Source), alpha = 0.1) +
  geom_point(data = gak1_comp_long |> filter(resid == best), aes(DOY, resid, col = Source)) +
  scale_color_manual(values = c("firebrick", "steelblue")) +
  xlab("Day of year") + 
  facet_wrap(~Year, scale = 'free_y') +
  scale_y_continuous(limits = c(-2.7, 2.7), expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 366), expand = c(0,0)) + 
  ylab(expression('Hindcast - observation'~(degree*C))) +
  theme_bw() +
  guides(col = guide_legend(nrow = 1)) +
  theme(panel.grid = element_blank(), legend.position = 'bottom', legend.text = element_text(size = 12),
        legend.title=element_blank(), strip.text = element_text(size = 12), axis.text = element_text(size = 12), axis.title = element_text(size = 12),
        strip.background = element_blank())

ggsave('results/plots/hindcast_error_all_yrs_200.png', units = 'cm', height = 20, width = 30, dpi = 300)
