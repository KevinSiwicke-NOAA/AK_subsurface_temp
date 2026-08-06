library(sf)
library(readr)
library(glue)
library(dplyr)
library(lubridate)
library(terra)
library(tidyterra)
library(ggplot2)
library(doParallel)
library(spdep)
library(Matrix)
library(TMB)
library(purrr)
library(rnaturalearth)
library(cowplot)

source("r/functions/geolocate_functions.r")

# for maps later
world <- ne_countries(scale='medium', returnclass = 'sf')

# Load bathymetry
bat <- readRDS("data/bathy_data/ebs_cod_10km.rds") # ai and ebs are same here

# Load release and pop-off info for Aleutian Islands example
ai_num <- 178690
ai_rel_date <- as.Date("2019-02-21")
ai_rel_long <- -173.85817
ai_rel_lat <- 52.35483
ai_rec_date <- as.Date("2019-05-23")
ai_rec_long <- -179.6535
ai_rec_lat <- 52.5473	

ai_mpt_sf <- readRDS(file = glue("results/cod/{ai_num}/{ai_num}_mpt.rds")) |> 
  mutate(mod = ifelse(mod == 'GAM-based', 'BCM-based', mod),
         mod = factor(mod, levels = c("Light-based", "HYCOM-based", 'BCM-based')))
ai_yearly_p <- readRDS(file = glue("results/cod/{ai_num}/{ai_num}_yr_prob.rds")) |> 
  mutate(mod = ifelse(mod == 'GAM-based', 'BCM-based', mod),
         mod = factor(mod, levels = c("Light-based", "HYCOM-based", 'BCM-based')))
ai_daily_p <- readRDS(file = glue("results/cod/{ai_num}/{ai_num}_day_prob.rds"))
ai_prefs <- readRDS(file = glue("results/cod/{ai_num}/{ai_num}_prefs.rds"))

ai_zoom <- st_bbox(ai_yearly_p) # for zoomed box

# Most probable track plot using Viterbi
ai_maps <- ggplot() + 
  geom_sf(data = ai_yearly_p |> filter(prob > 0), aes(fill = norm_prob), alpha = 0.75) +
  geom_sf(data = ai_yearly_p |> filter(cell_id == 14958), fill = NA, col = 'green', linewidth = 0.5) +
  geom_sf(data = ai_yearly_p |> filter(cell_id == 11903), fill = NA, col = "red", linewidth = 0.5) +
  geom_sf(data = world, fill = "cornsilk") +
  geom_sf(data = ai_mpt_sf, col = "blue", size = 0.75) +
  scale_fill_viridis_c(na.value = 'white') +
  labs(x = "Longitude", y = "Latitude", fill = 'Normalized \ncumulative \nprobability', col = 'Month', lty = 'Isobath (m)') +
  theme_bw() +
  facet_wrap(~mod, ncol = 3) + 
  ggtitle(glue("AI ({ai_rel_date} to {ai_rec_date})")) +
  coord_sf(crs = st_crs(3338), xlim = c(ai_zoom$xmin, ai_zoom$xmax), 
           ylim = c(ai_zoom$ymin, ai_zoom$ymax), expand = FALSE) 

# Load release and pop-off info for Bering Sea example
bs_num <- 215398
bs_rel_date <- as.Date("2021-06-27")
bs_rel_long<- -169.946983
bs_rel_lat<- 57.3321
bs_rec_date <- as.Date("2021-09-26")
bs_rec_long<-  -172.56370
bs_rec_lat<-  57.34400

bs_mpt_sf <- readRDS(file = glue("results/cod/{bs_num}/{bs_num}_mpt.rds")) |> 
  mutate(mod = ifelse(mod == 'GAM-based', 'BCM-based', mod),
         mod = factor(mod, levels = c("Light-based", "HYCOM-based", 'BCM-based')))
bs_yearly_p <- readRDS(file = glue("results/cod/{bs_num}/{bs_num}_yr_prob.rds")) |> 
  mutate(mod = ifelse(mod == 'GAM-based', 'BCM-based', mod),
         mod = factor(mod, levels = c("Light-based", "HYCOM-based", 'BCM-based')))
bs_daily_p <- readRDS(file = glue("results/cod/{bs_num}/{bs_num}_day_prob.rds"))

bs_zoom <- st_bbox(bs_yearly_p) # for zoomed box

bs_maps <- ggplot() + 
  geom_sf(data = bs_yearly_p |> filter(prob > 0), aes(fill = norm_prob), alpha = 0.75) +
  geom_sf(data = bs_yearly_p |> filter(cell_id == 6236), fill = NA, col = 'green', linewidth = 0.5) +
  geom_sf(data = bs_yearly_p |> filter(cell_id == 5669), fill = NA, col = "red", linewidth = 0.5) +
  geom_sf(data = world, fill = "cornsilk") +
  geom_sf(data = bs_mpt_sf, col = "blue", size = 0.5) +
  scale_fill_viridis_c(na.value = 'white') +
  labs(x = "Longitude", y = "Latitude", fill = 'Normalized \ncumulative \nprobability', col = 'Month', lty = 'Isobath (m)') +
  theme_bw() +
  facet_wrap(~mod, ncol = 3) + 
  ggtitle(glue("EBS ({bs_rel_date} to {bs_rec_date})")) +
  coord_sf(crs = st_crs(3338), xlim = c(bs_zoom$xmin, bs_zoom$xmax), 
           ylim = c(bs_zoom$ymin, bs_zoom$ymax), expand = FALSE) +
  theme(legend.position="none")

bathy_sf <- st_as_sf(as.polygons(bat, trunc=FALSE, dissolve=FALSE))
big_map <- ggplot() + 
  geom_sf(data = bathy_sf, aes(fill = mean_z), col = NA) +
  scale_fill_viridis_c(option = 'mako', trans='reverse') +
  geom_sf(data = ai_yearly_p |> filter(cell_id == 14958), fill = NA, col = 'green', linewidth = 1) +
  geom_sf(data = ai_yearly_p |> filter(cell_id == 11903), fill = NA, col = "red", linewidth = 1) +
  geom_sf(data = bs_yearly_p |> filter(cell_id == 6236), fill = NA, col = 'green', linewidth = 1) +
  geom_sf(data = bs_yearly_p |> filter(cell_id == 5669), fill = NA, col = "red", linewidth = 1) +
  geom_sf(data = world, fill = "cornsilk") + 
  scale_x_continuous(breaks = c(170, 180, 190, 200)) +
  labs(x = "Longitude", y = "Latitude", fill = "Bathymetry (m)") +
  theme_bw() +
  coord_sf(crs = st_crs(3338), xlim = c(st_bbox(bathy_sf)$xmin, st_bbox(bathy_sf)$xmax), 
           ylim = c(st_bbox(bathy_sf)$ymin, st_bbox(bathy_sf)$ymax), expand = FALSE) +
  geom_rect(aes(xmin = bs_zoom$xmin, ymin = bs_zoom$ymin, xmax = bs_zoom$xmax, ymax = bs_zoom$ymax),
            fill = NA, colour = "black", linewidth = 2) +
  annotate("text", x = bs_zoom$xmin + 150000, y = bs_zoom$ymax + 100000, label= "EBS", size = 16/.pt) +
  geom_rect(aes(xmin = ai_zoom$xmin, ymin = ai_zoom$ymin, xmax = ai_zoom$xmax, ymax = ai_zoom$ymax),
            fill = NA, colour = "black", linewidth = 2) +
  annotate("text", x = ai_zoom$xmin + 200000, y = ai_zoom$ymax + 100000, label= "AI", size = 16/.pt) +
  theme(legend.position="bottom") 

top_maps <- plot_grid(big_map, bs_maps, nrow = 1, rel_widths = c(0.3, 0.7), labels = c('A', 'B'))

plot_grid(top_maps, ai_maps, ncol = 1, labels = c('', 'C'))

ggsave(file = "results/plots/fig10_cod_maps.png", bg = "white", height = 8, width = 14, dpi = 300)

# Alternate alignment if needed
# sm_maps <- plot_grid(bs_maps, ai_maps, ncol = 1, labels = c('B', 'C'), align = 'v')
# plot_grid(big_map, sm_maps, nrow = 1, labels = c('A', ''), rel_widths = c(0.3, 0.7))

##############
# LONGITUDE
##############
bs_long_dat <- read_csv(file = glue("data/cod/{bs_num}/{bs_num}-LightLoc.csv")) |> 
  mutate(date_time = as.POSIXct(glue("{Time} {Day}"), "%H:%M:%S %d-%b-%Y", tz="GMT"),
         date_time_geo = as.POSIXct(glue("{GeoTime} {GeoDay}"), "%H:%M:%S %d-%b-%Y", tz="GMT")) |>
  filter(date_time >= bs_rel_date, date_time <= bs_rec_date) |>
  mutate(date = as_date(date_time_geo),
         GeoLong = ifelse(GeoLong < 0, GeoLong + 360, GeoLong)) |> # = as_date(date_time), date_geo
  group_by(date) |>
  summarize(value = mean(GeoLong, na.rm = TRUE), err = mean(GeoLongError, na.rm = TRUE)) |>
  mutate(err = ifelse(err < 1.5, 1.5, err)) |> 
  na.omit()
# These are to make sure that when no data exists, there's still a row with NAs
date <- seq(bs_rel_date, bs_rec_date, by="day")
bs_date <- as.data.frame(date)
bs_long_dat <- left_join(bs_date, bs_long_dat)

bs_mpt_dat <- readRDS(file = glue("results/cod/{bs_num}/{bs_num}_mpt_dat.rds")) |> 
  mutate(mod = ifelse(mod == 'GAM-based', 'BCM-based', mod),
         mod = factor(mod, levels = c("Light-based", "HYCOM-based", 'BCM-based')))

bs_lon_plot <- ggplot() + 
  geom_errorbar(data = bs_long_dat, aes(x = date, ymin = value - err, ymax = value + err), col = 'grey70', width = 0) +
  geom_point(data = bs_long_dat, aes(date, value)) + 
  geom_line(data = bs_mpt_dat, aes(date, long, col = mod), linewidth = 1) +
  geom_ribbon(data = bs_mpt_dat, aes(x=date, ymin=xmin, ymax=xmax, fill = mod), alpha = 0.25) +
  scale_color_manual(values = c("black", "steelblue", "firebrick"), guide = "none") +
  scale_fill_manual(values = c("black", "steelblue", "firebrick"), guide = "none") +
  scale_x_date(date_breaks = "2 weeks", date_labels =  "%b-%d") +
  geom_point(aes(x = bs_rel_date, y = bs_rel_long + 360), col = 'green', size = 2.5) +
  geom_point(aes(x = bs_rec_date, y = bs_rec_long + 360), col = 'red', size = 2.5) +
  labs(x = "Date", y = "Longitude (0 to 360)", col = "Model") +
  ggtitle(glue("EBS ({bs_rel_date} to {bs_rec_date})")) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) # , axis.text.x=element_blank(), axis.ticks.x = element_blank()

bs_lon_plt <- bs_lon_plot + theme(legend.justification.inside = c(.9, .9))

ai_long_dat <- read_csv(file = glue("data/cod/{ai_num}/{ai_num}-LightLoc.csv")) |> 
  mutate(date_time = as.POSIXct(glue("{Time} {Day}"), "%H:%M:%S %d-%b-%Y", tz="GMT"),
         date_time_geo = as.POSIXct(glue("{GeoTime} {GeoDay}"), "%H:%M:%S %d-%b-%Y", tz="GMT")) |>
  filter(date_time >= ai_rel_date, date_time <= ai_rec_date) |>
  mutate(date = as_date(date_time_geo),
         GeoLong = ifelse(GeoLong < 0, GeoLong + 360, GeoLong)) |> # = as_date(date_time), date_geo
  group_by(date) |>
  summarize(value = mean(GeoLong, na.rm = TRUE), err = mean(GeoLongError, na.rm = TRUE)) |>
  mutate(err = ifelse(err < 1.5, 1.5, err)) |> 
  na.omit()

# These are to make sure that when no data exists, there's still a row with NAs
date <- seq(ai_rel_date, ai_rec_date, by="day")
ai_date <- as.data.frame(date)
ai_long_dat <- left_join(ai_date, ai_long_dat)

ai_mpt_dat <- readRDS(file = glue("results/cod/{ai_num}/{ai_num}_mpt_dat.rds")) |> 
  mutate(mod = ifelse(mod == 'GAM-based', 'BCM-based', mod),
         mod = factor(mod, levels = c("Light-based", "HYCOM-based", 'BCM-based')))

ai_lon_plot <- ggplot() + 
  geom_errorbar(data = ai_long_dat, aes(x = date, ymin = value - err, ymax = value + err), col = 'grey70', width = 0) +
  geom_point(data = ai_long_dat, aes(date, value)) + 
  geom_line(data = ai_mpt_dat, aes(date, long, col = mod), linewidth = 1) +
  geom_ribbon(data = ai_mpt_dat, aes(x=date, ymin=xmin, ymax=xmax, fill = mod), alpha = 0.25) +
  scale_color_manual(values = c("black", "steelblue", "firebrick")) +
  scale_fill_manual(values = c("black", "steelblue", "firebrick"), guide = "none") +
  scale_x_date(date_breaks = "2 weeks", date_labels =  "%b-%d") +
  geom_point(aes(x = ai_rel_date, y = ai_rel_long + 360), col = 'green', size = 2.5) +
  geom_point(aes(x = ai_rec_date, y = ai_rec_long + 360), col = 'red', size = 2.5) +
  labs(x = "Date", y = "Longitude (0 to 360)", col = "Model") +
  guides(color = guide_legend(position = "inside", ncol = 1)) +
  ggtitle(glue("AI ({ai_rel_date} to {ai_rec_date})")) +
  theme_bw() + 
  theme(axis.title = element_blank()) # , axis.title.x = element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank()

ai_lon_plt <- ai_lon_plot + theme(legend.justification.inside = c(.93, .93))

plot_grid(bs_lon_plt, ai_lon_plt, nrow = 1, align = 'hv')

ggsave("results/plots/Fig11_cod_lon.png", height = 4, width = 10, units = 'in', dpi = 300)

bs_overlap <- bs_daily_p |>
  group_by(date, cell_id, mod) |> 
  summarize(prob = sum(p)) |> 
  mutate(keep = ifelse(prob == 0, 0, 1)) |> 
  filter(keep == 1) |> 
  pivot_wider(names_from = mod, values_from = prob) |> 
  rename(BCM = 'GAM-based',
         HYCOM = 'HYCOM-based',
         Longitude = 'Light-based') |> 
  mutate(BCM = ifelse(is.na(BCM), 0, 1),
         HYCOM = ifelse(is.na(HYCOM), 0, 1),
         Longitude = ifelse(is.na(Longitude), 0, 1),
         g_same = BCM + Longitude,
         h_same = HYCOM + Longitude)

bs_g_summ <- bs_overlap |> 
  group_by(date, g_same, Longitude) |> 
  summarize(num_cells = n()) |> 
  filter(!(g_same == 0 & Longitude == 0)) |> 
  mutate(group = factor(ifelse(g_same == 1 & Longitude == 0, 'T only',
                               ifelse(g_same == 1 & Longitude == 1, "L only",
                                      ifelse(g_same == 2 & Longitude == 1, 'L and T', NA))),
                        levels = c('L only', 'L and T', 'T only'))) |> 
  select(date, group, num_cells) |> 
  mutate(mod = 'BCM-based')

bs_h_summ <- bs_overlap |> 
  group_by(date, h_same, Longitude) |> 
  summarize(num_cells = n()) |> 
  filter(!(h_same == 0 & Longitude == 0)) |> 
  mutate(group = factor(ifelse(h_same == 1 & Longitude == 0, 'T only',
                               ifelse(h_same == 1 & Longitude == 1, "L only",
                                      ifelse(h_same == 2 & Longitude == 1, 'L and T', NA))), 
                        levels = c('L only', 'L and T', 'T only'))) |> 
  select(date, group, num_cells) |> 
  mutate(mod = 'HYCOM-based')

bs_per_overlap = rbind(bs_g_summ, bs_h_summ) |> 
  mutate(date = as.Date(date),
         mod = factor(mod, levels = c('HYCOM-based', 'BCM-based')),
         reg = glue("EBS ({bs_rel_date} to {bs_rec_date})"))

bs_over_plot <- ggplot(bs_per_overlap, aes(x = date, y = num_cells, fill = group, col = group)) + 
  geom_bar(stat = 'identity', position = "fill") +
  facet_grid(mod~reg) +
  scale_fill_manual(values = c("grey30", "black", "grey80")) +
  scale_color_manual(values = c("grey30", "black", "grey80")) +
  scale_x_date(date_breaks = "2 weeks", date_labels =  "%b-%d") +
  labs(x = 'Date', y = "Proportion", fill = "Group", col = "Group") +
  theme_bw() +
  ggtitle(glue("EBS ({bs_rel_date} to {bs_rec_date})")) +
  theme(strip.background = element_blank(), strip.text = element_blank(), 
        axis.title.x = element_blank(), legend.position = "none")

ai_overlap <- ai_daily_p |>
  group_by(date, cell_id, mod) |> 
  summarize(prob = sum(p)) |> 
  mutate(keep = ifelse(prob == 0, 0, 1)) |> 
  filter(keep == 1) |> 
  pivot_wider(names_from = mod, values_from = prob) |> 
  rename(BCM = 'GAM-based',
         HYCOM = 'HYCOM-based',
         Longitude = 'Light-based') |> 
  mutate(BCM = ifelse(is.na(BCM), 0, 1),
         HYCOM = ifelse(is.na(HYCOM), 0, 1),
         Longitude = ifelse(is.na(Longitude), 0, 1),
         g_same = BCM + Longitude,
         h_same = HYCOM + Longitude)

ai_g_summ <- ai_overlap |> 
  group_by(date, g_same, Longitude) |> 
  summarize(num_cells = n()) |> 
  filter(!(g_same == 0 & Longitude == 0)) |> 
  mutate(group = factor(ifelse(g_same == 1 & Longitude == 0, 'T only',
                               ifelse(g_same == 1 & Longitude == 1, "L only",
                                      ifelse(g_same == 2 & Longitude == 1, 'L and T', NA))),
                        levels = c('L only', 'L and T', 'T only'))) |> 
  select(date, group, num_cells) |> 
  mutate(mod = 'BCM-based')

ai_h_summ <- ai_overlap |> 
  group_by(date, h_same, Longitude) |> 
  summarize(num_cells = n()) |> 
  filter(!(h_same == 0 & Longitude == 0)) |> 
  mutate(group = factor(ifelse(h_same == 1 & Longitude == 0, 'T only',
                               ifelse(h_same == 1 & Longitude == 1, "L only",
                                      ifelse(h_same == 2 & Longitude == 1, 'L and T', NA))), 
                        levels = c('L only', 'L and T', 'T only'))) |> 
  select(date, group, num_cells) |> 
  mutate(mod = 'HYCOM-based')

ai_per_overlap = rbind(ai_g_summ, ai_h_summ) |> 
  mutate(date = as.Date(date),
         mod = factor(mod, levels = c('HYCOM-based', 'BCM-based')),
         reg = glue("AI ({ai_rel_date} to {ai_rec_date})"))

ai_over_plot <- ggplot(ai_per_overlap, aes(x = date, y = num_cells, fill = group, col = group)) + 
  geom_bar(stat = 'identity', position = "fill") +
  facet_grid(mod~reg) +
  scale_fill_manual(values = c("grey30", "black", "grey80")) +
  scale_color_manual(values = c("grey30", "black", "grey80")) +
  scale_x_date(date_breaks = "2 weeks", date_labels =  "%b-%d") +
  labs(x = 'Date', y = "Proportion", fill = "Group", col = "Group") +
  theme_bw() +
  ggtitle(glue("AI ({ai_rel_date} to {ai_rec_date})")) +
  theme(strip.background = element_blank(), 
        strip.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

legend <- get_plot_component(ai_over_plot + theme(legend.position = "bottom"), 'guide-box-bottom', return_all = TRUE)
over_plots <- plot_grid(bs_over_plot + theme(legend.position="none"), NULL,
                        ai_over_plot + theme(legend.position="none"), 
                        align = 'hv', rel_widths = c(1, -0.05, 1), nrow = 1)

plot_grid(over_plots, legend, ncol = 1, rel_heights = c(1, 0.1))

ggsave("results/plots/fig12_cod_overlap.png", height = 6, width = 10, units = 'in', dpi = 300, bg = "white")
