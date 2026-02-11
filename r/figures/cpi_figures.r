##################
# Make two figures used for example of Cold Pool Index in the Eastern Bering Sea
##################

# Load packages needed
pkgs <- c("ggplot2", "dplyr", "lubridate", "sf", "cowplot", "Metrics", "tidyr", "coldpool", "tidyterra")
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE, quietly = TRUE)

##################
# Make plot for Cold Pool Index
##################
# Get the survey area at which the cold pool indices are produced
map_layer <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "EPSG:3338")

tot_area <- map_layer$survey.area$AREA_M2 / 1000000

# ggplot() +
#   geom_sf(data = map_layer$survey.area)

# Load gridded data (5 km x 5 km) and use determine which cells meet the criteria of <= 2 or <= 0 degrees C
pred_grid <- readRDS(file = "data/gridded_cpi_data.rds") |> 
  mutate(year = year(date),
         h_sub_2 = ifelse(hycom_T > 2, 0, 1),
         g_sub_2 = ifelse(gam_T > 2, 0, 1),
         h_sub_0 = ifelse(hycom_T > 0, 0, 1),
         g_sub_0 = ifelse(gam_T > 0, 0, 1))

# pred_grid_cpi <- st_intersection(pred_grid, map_layer$survey.area)
# saveRDS(pred_grid_cpi, file = "data/cropped_gridded_cpi_data.rds")
pred_grid_cpi <- readRDS(file = "data/cropped_gridded_cpi_data.rds") |> 
  mutate(area_km = area / 1000000)

# Load Groundfish Assessment Program coldpool package and temperature data for their annual means, <= 2 and <= 0 degrees C areas
cpi_bts <- coldpool::cold_pool_index |> 
  filter(YEAR > 1993, YEAR < 2024) |> 
  mutate(year = YEAR, src = "Survey", bt = MEAN_GEAR_TEMPERATURE,
         sub2_area = (AREA_LTE2_KM2 / tot_area), sub0_area = (AREA_LTE0_KM2 / tot_area)) |> 
  select(year, src, bt, sub2_area, sub0_area) |> 
  bind_rows(data.frame(year = 2020, src = "Survey", bt = NA, sub2_area = NA, sub0_area = NA)) # add because there was no survey data in 2020

# Calculate mean bottom temperature for HYCOM and GAM-corrected output
mean_bt <- pred_grid_cpi |>
  st_drop_geometry() |>
  group_by(year) |> 
  summarize(HYCOM = mean(hycom_T),
            BCM = mean(gam_T)) |> 
  tidyr::pivot_longer(cols = 2:3, names_to = "src", values_to = "bt")

mn_dat <- bind_rows(mean_bt, cpi_bts |> select(year, src, bt))

mn_comp <- mn_dat |> 
  pivot_wider(names_from = src, values_from = bt) |> 
  na.omit()

# Calculate area with bottom temperature <= 2 degrees C for HYCOM and GAM-corrected output
h_sub_2 <- pred_grid_cpi |> 
  filter(h_sub_2 == 1) |> 
  st_drop_geometry() |>
  group_by(year) |> 
  summarize(sub2_area = as.numeric(sum(area_km) / tot_area)) |> # convert to sq km
  mutate(src = "HYCOM")

g_sub_2 <- pred_grid_cpi |> 
  filter(g_sub_2 == 1) |> 
  st_drop_geometry() |>
  group_by(year) |> 
  summarize(sub2_area = as.numeric(sum(area_km) / tot_area)) |> # convert to sq km
  mutate(src = "BCM")

sub2_dat <- bind_rows(h_sub_2, g_sub_2, cpi_bts |> select(year, src, sub2_area))

sub2_comp <- sub2_dat |> 
  pivot_wider(names_from = src, values_from = sub2_area) |> 
  na.omit()

# Calculate area with bottom temperature <= 0 degrees C for HYCOM and GAM-corrected output
h_sub_0 <- pred_grid_cpi |> 
  filter(h_sub_0 == 1) |> 
  st_drop_geometry() |>
  group_by(year) |> 
  summarize(sub0_area = as.numeric(sum(area_km) / tot_area)) |> # convert to sq km
  mutate(src = "HYCOM") |> 
  bind_rows(data.frame(year = 2016, sub0_area = 0, src = "HYCOM")) # Needed because there are no cells <= 0 for 2016

g_sub_0 <- pred_grid_cpi |> 
  filter(g_sub_0 == 1) |> 
  st_drop_geometry() |>
  group_by(year) |> 
  summarize(sub0_area = as.numeric(sum(area_km) / tot_area)) |> # convert to sq km
  mutate(src = "BCM") |> 
  bind_rows(data.frame(year = c(2016, 2018, 2019), sub0_area = c(0,0,0), src = c("BCM","BCM","BCM"))) # Needed because there are no cells <= 0 for 2016, 2018 and 2019

sub0_dat <- bind_rows(h_sub_0, g_sub_0, cpi_bts|> select(year, src, sub0_area))

sub0_comp <- sub0_dat |> 
  pivot_wider(names_from = src, values_from = sub0_area) |> 
  na.omit() 

# Create each panel for the plot
theme_white <- theme(panel.background=element_blank(),
                     panel.border=element_rect(color="transparent"),
                     panel.grid.major.y=element_blank(),
                     panel.grid.major.x=element_blank(),
                     panel.grid.minor.x=element_blank(),
                     panel.grid.minor.y=element_blank(),
                     axis.line.x=element_line(color="black", linewidth=0.5),
                     axis.line.y=element_line(color="black", linewidth=0.5),
                     axis.text=element_text(size=12),
                     axis.title=element_text(size=12),
                     legend.text = element_text(size=12),
                     legend.title = element_text(size=12),
                     axis.ticks=element_line(color="black", size=0.5)
)
# Top panel is mean bottom temperature

mn_bt_plot <- ggplot(mn_dat, aes(year, bt, col = factor(src, levels = c("Survey", "HYCOM", "BCM")))) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("black", "steelblue", "firebrick")) +
  scale_x_continuous(breaks = seq(1995, 2024, 5), limits = c(1993, 2024), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,5)) +
  labs(y = expression(Mean~bottom~temperature~(degree*C)),
       col = "Source") +
  theme_bw() +
  theme(legend.position = "top", axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme_white

mn = ggplotGrob(mn_bt_plot + theme(legend.position = "none"))
mn$layout$clip[mn$layout$name=="panel"] <- "off"
mn$layout$z[mn$layout$name=="panel"] = 17  

# Middle panel is area of EBS that is <= to 2 degrees C

sub2_plot <- ggplot(sub2_dat, aes(year, sub2_area, col = factor(src, levels = c("Survey", "HYCOM", "BCM")))) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1995, 2024, 5), limits = c(1993, 2024), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,1,.2), expand = c(0,0), limits = c(0,1)) +
  scale_color_manual(values = c("black", "steelblue", "firebrick")) +
  labs(y = expression(Proportion~'\u2264 2'*degree*C), col = "Source") +
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        legend.position="none") +
  theme_white

sub2 = ggplotGrob(sub2_plot)
sub2$layout$clip[sub2$layout$name=="panel"] <- "off"
sub2$layout$z[sub2$layout$name=="panel"] = 17  

# Bottom panel is area of EBS that is <= to 0 degrees C

sub0_plot <- ggplot(sub0_dat, aes(year, sub0_area, col = factor(src, levels = c("Survey", "HYCOM", "BCM")))) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(0,0.5,.1), expand = c(0,0), limits = c(0,0.5)) +
  scale_x_continuous(breaks = seq(1995, 2024, 5), limits = c(1993, 2024), expand = c(0,0)) +
  scale_color_manual(values = c("black", "steelblue", "firebrick")) +
  labs(x = "Year", y = expression(Proportion~'\u2264 0'*degree*C), col = "Source") +
  theme_bw() +
  theme(legend.position="none") +
  theme_white

sub0 = ggplotGrob(sub0_plot)
sub0$layout$clip[sub0$layout$name=="panel"] <- "off"
sub0$layout$z[sub0$layout$name=="panel"] = 17  

# Combine plots and save
plots <- plot_grid(mn,  
                   sub2, 
                   sub0,
                   ncol = 1, align = 'v', rel_heights = c(1,1,1.25))

legend <- get_plot_component(mn_bt_plot, 'guide-box-top', return_all = TRUE)

cpi_plots <- plot_grid(legend, plots, ncol = 1, rel_heights = c(0.1, 1))

cor <- ggdraw() +
  draw_label("Cor", fontface = 'bold', size = 12)

# Create text "plot" for Column 1 content
cor1h <- ggdraw() +
  draw_label(sprintf("%.3f", round(cor(mn_comp$HYCOM, mn_comp$Survey), 3)), 
             size = 12, color = "steelblue", vjust = -1) # Adjust x, hjust, vjust for positioning
cor1g <- ggdraw() +
  draw_label(sprintf("%.3f", round(cor(mn_comp$BCM, mn_comp$Survey), 3)),
             size = 12, color = "firebrick", vjust = -7) # Adjust x, hjust, vjust for positioning
cor2h <- ggdraw() +
  draw_label(sprintf("%.3f", round(cor(sub2_comp$HYCOM, sub2_comp$Survey), 3)), 
             size = 12, color = "steelblue", vjust = 0) # Adjust x, hjust, vjust for positioning
cor2g <- ggdraw() +
  draw_label(sprintf("%.3f", round(cor(sub2_comp$BCM, sub2_comp$Survey), 3)),
             size = 12, color = "firebrick", vjust = -6) # Adjust x, hjust, vjust for positioning
cor3h <- ggdraw() +
  draw_label(sprintf("%.3f", round(cor(sub0_comp$HYCOM, sub0_comp$Survey), 3)), 
             size = 12, color = "steelblue", vjust = 1) # Adjust x, hjust, vjust for positioning
cor3g <- ggdraw() +
  draw_label(sprintf("%.3f", round(cor(sub0_comp$BCM, sub0_comp$Survey), 3)),
             size = 12, color = "firebrick", vjust = -5) # Adjust x, hjust, vjust for positioning

cor_col <- plot_grid(cor, cor1h, cor1g, cor2h, cor2g, cor3h, cor3g, ncol = 1)

# RMSD
rmsd <- ggdraw() +
  draw_label("RMSD", fontface = 'bold', size = 12)

# Create text "plot" for Column 1 content
rmsd1h <- ggdraw() +
  draw_label(sprintf("%.3f", round(rmse(mn_comp$HYCOM, mn_comp$Survey), 3)),
             size = 12, color = "steelblue", vjust = -1) # Adjust x, hjust, vjust for positioning
rmsd1g <- ggdraw() +
  draw_label(sprintf("%.3f", round(rmse(mn_comp$BCM, mn_comp$Survey), 3)),
             size = 12, color = "firebrick", vjust = -7) # Adjust x, hjust, vjust for positioning
rmsd2h <- ggdraw() +
  draw_label(sprintf("%.3f", round(rmse(sub2_comp$HYCOM, sub2_comp$Survey), 3)),
             size = 12, color = "steelblue", vjust = 0) # Adjust x, hjust, vjust for positioning
rmsd2g <- ggdraw() +
  draw_label(sprintf("%.3f", round(rmse(sub2_comp$BCM, sub2_comp$Survey), 3)),
             size = 12, color = "firebrick", vjust = -6) # Adjust x, hjust, vjust for positioning
rmsd3h <- ggdraw() +
  draw_label(sprintf("%.3f", round(rmse(sub0_comp$HYCOM, sub0_comp$Survey), 3)),
             size = 12, color = "steelblue", vjust = 1) # Adjust x, hjust, vjust for positioning
rmsd3g <- ggdraw() +
  draw_label(sprintf("%.3f", round(rmse(sub0_comp$BCM, sub0_comp$Survey), 3)),
             size = 12, color = "firebrick", vjust = -5) # Adjust x, hjust, vjust for positioning

rmsd_col <- plot_grid(rmsd, rmsd1h, rmsd1g, rmsd2h, rmsd2g, rmsd3h, rmsd3g, ncol = 1)

# RMSD
bias <- ggdraw() +
  draw_label("Bias", fontface = 'bold', size = 12)

# Create text "plot" for Column 1 content
bias1h <- ggdraw() +
  draw_label(sprintf("%.3f", round(bias(mn_comp$HYCOM, mn_comp$Survey), 3)),
             size = 12, color = "steelblue", vjust = -1) # Adjust x, hjust, vjust for positioning
bias1g <- ggdraw() +
  draw_label(sprintf("%.3f", round(bias(mn_comp$BCM, mn_comp$Survey), 3)),
             size = 12, color = "firebrick", vjust = -7) # Adjust x, hjust, vjust for positioning
bias2h <- ggdraw() +
  draw_label(sprintf("%.3f", round(bias(sub2_comp$HYCOM, sub2_comp$Survey), 3)),
             size = 12, color = "steelblue", vjust = 0) # Adjust x, hjust, vjust for positioning
bias2g <- ggdraw() +
  draw_label(sprintf("%.3f", round(bias(sub2_comp$BCM, sub2_comp$Survey), 3)),
             size = 12, color = "firebrick", vjust = -6) # Adjust x, hjust, vjust for positioning
bias3h <- ggdraw() +
  draw_label(sprintf("%.3f", round(bias(sub0_comp$HYCOM, sub0_comp$Survey), 3)),
             size = 12, color = "steelblue", vjust = 1) # Adjust x, hjust, vjust for positioning
bias3g <- ggdraw() +
  draw_label(sprintf("%.3f", round(bias(sub0_comp$BCM, sub0_comp$Survey), 3)),
             size = 12, color = "firebrick", vjust = -5) # Adjust x, hjust, vjust for positioning

bias_col <- plot_grid(bias, bias1h, bias1g, bias2h, bias2g, bias3h, bias3g, ncol = 1)

plot_grid(cpi_plots, cor_col, rmsd_col, bias_col, rel_widths = c(1, 0.09, 0.09, 0.09), nrow = 1)

ggsave(paste0("results/plots/cpi_indices.png"), bg = "white", 
       width = 20, height = 20, units = 'cm', dpi = 300)

##################
# Example small cold pool (2018( versus large cold pool (2012)
##################
# Load map of EBS for calculating mean bottom temperature in outer shelf strata using Groundfish Assessment Program akgfmaps package
map_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "EPSG:3338")

gap_12 <- unwrap(coldpool::ebs_bottom_temperature) |> 
  tidyterra::select(Survey_temp = 3) |> 
  tidyterra::mutate(year = 2012, source = "Survey")
gap_12_sf <- st_as_sf(as.polygons(gap_12, trunc=FALSE, dissolve=FALSE))

gap_18 <- unwrap(coldpool::nbs_ebs_bottom_temperature) |> 
  tidyterra::select(Survey_temp = 3) |> 
  tidyterra::mutate(year = 2018, source = "Survey")
gap_18_sf <- st_as_sf(as.polygons(gap_18, trunc=FALSE, dissolve=FALSE))
gap_sf <- rbind(gap_12_sf, gap_18_sf)

s <- ggplot() +
  geom_sf(data = map_layers$bathymetry) +
  geom_sf(data = gap_sf,
          aes(fill = Survey_temp, col = Survey_temp)) +
  scale_fill_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
  scale_color_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
  geom_sf(data = map_layers$bathymetry) +
  geom_sf(data = map_layers$akland) +
  labs(fill = "Temperature", col = '') +
  facet_grid(source~year) +
  geom_sf(data = map_layers$graticule, color = "grey70", alpha = 0.5) +
  coord_sf(xlim = map_layers$plot.boundary$x,
           ylim = map_layers$plot.boundary$y) +
  scale_x_continuous(name = "Longitude",
                     breaks = map_layers$lon.breaks) +
  scale_y_continuous(name = "Latitude",
                     breaks = map_layers$lat.breaks) +
  theme_bw() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 10)) +
  guides(color = "none")

h <- ggplot() +
  geom_sf(data = map_layers$bathymetry) +
  geom_sf(data = pred_grid |> filter(year(date) %in% c(2012, 2018)) |> mutate(source = 'HYCOM'),
          aes(fill = hycom_T, col = hycom_T)) +
  scale_fill_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
  scale_color_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
  geom_sf(data = map_layers$bathymetry) +
  geom_sf(data = map_layers$akland) +
  labs(fill = "Temperature", col = '') +
  facet_grid(source~year(date)) +
  geom_sf(data = map_layers$graticule, color = "grey70", alpha = 0.5) +
  coord_sf(xlim = map_layers$plot.boundary$x,
           ylim = map_layers$plot.boundary$y) +
  scale_x_continuous(name = "Longitude",
                     breaks = map_layers$lon.breaks) +
  scale_y_continuous(name = "Latitude",
                     breaks = map_layers$lat.breaks) +
  theme_bw() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 10)) +
  guides(color = "none")

g <- ggplot() +
  geom_sf(data = map_layers$bathymetry) +
  geom_sf(data = pred_grid |> filter(year(date) %in% c(2012, 2018))|> mutate(source = 'BCM'),
          aes(fill = gam_T, col = gam_T)) +
  scale_fill_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
  scale_color_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
  geom_sf(data = map_layers$bathymetry) +
  geom_sf(data = map_layers$akland) +
  labs(fill = "Temperature", col = '') +
  facet_grid(source~year(date)) +
  geom_sf(data = map_layers$graticule, color = "grey70", alpha = 0.5) +
  coord_sf(xlim = map_layers$plot.boundary$x,
           ylim = map_layers$plot.boundary$y) +
  scale_x_continuous(name = "Longitude",
                     breaks = map_layers$lon.breaks) +
  scale_y_continuous(name = "Latitude",
                     breaks = map_layers$lat.breaks) +
  theme_bw() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 10)) +
  guides(color = "none")

plots <- plot_grid(s + theme(legend.position = "none", strip.background = element_blank(), 
                             axis.title=element_blank(), axis.text.x=element_blank()),
                   NULL,
                   h + theme(legend.position = "none", strip.background = element_blank(), 
                             axis.title.x=element_blank(), axis.text.x=element_blank(), strip.text.x = element_blank()), 
                   NULL,
                   g + theme(legend.position = "none", strip.background = element_blank(), 
                             axis.title.y=element_blank(),strip.text.x = element_blank()), 
                   ncol = 1, align = "hv", rel_heights = c(1, -0.1, 1, -0.1, 1))

legend <- get_plot_component(h, 'guide-box-right', return_all = TRUE)

plot_grid(plots, legend, ncol = 2, rel_widths = c(1, 0.1))

ggsave(paste0("results/plots/cpi_ex_figure.png"), bg = "white", 
       width = 16, height = 24, units = 'cm', dpi = 300)

# Make annual plots of HYCOM prediction and corrected cold pool to include in the supplemental
gap <- unwrap(coldpool::ebs_bottom_temperature) |> 
  tidyterra::select(Survey_temp = 13) |> 
  tidyterra::mutate(year = 1994, source = "Survey")
gap_sf <- st_as_sf(as.polygons(gap, trunc=FALSE, dissolve=FALSE))

for(y in c(1995:2023)) {
  if(y %in% c(2010, 2017, 2018, 2019, 2021, 2022, 2023)) {
    relation <- c(2010, 2017, 2018, 2019, 2021, 2022, 2023)
    index <- which(relation == y)
    dat <- unwrap(coldpool::nbs_ebs_bottom_temperature) |> 
      tidyterra::select(Survey_temp = index) |> 
      tidyterra::mutate(year = y, source = "Survey")
  }
  else {
    dat <- unwrap(coldpool::ebs_bottom_temperature) |> 
      tidyterra::select(Survey_temp = (y - 1981)) |> 
      tidyterra::mutate(year = y, source = "Survey")
  }
  
  if(y == 2020) {
    dat$Survey_temp = NA
    dat$Survey_temp[1:10] = 1 # this is to allow 2020 to have a map even though there was no survey
  }
  
  dat_sf <- st_as_sf(as.polygons(dat, trunc=FALSE, dissolve=FALSE))
  gap_sf <- rbind(gap_sf, dat_sf)
}

for(i in 0:4) {
  s <- ggplot() +
    geom_sf(data = map_layers$bathymetry) +
    geom_sf(data = gap_sf |> filter(year %in% c((1994+6*i):(1999+6*i))),
            aes(fill = Survey_temp, col = Survey_temp)) +
    scale_fill_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
    scale_color_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
    geom_sf(data = map_layers$bathymetry) +
    geom_sf(data = map_layers$akland) +
    labs(fill = "Temperature", col = '') +
    facet_grid(source~year) +
    geom_sf(data = map_layers$graticule, color = "grey70", alpha = 0.5) +
    coord_sf(xlim = map_layers$plot.boundary$x,
             ylim = map_layers$plot.boundary$y) +
    scale_x_continuous(name = "Longitude",
                       breaks = map_layers$lon.breaks) +
    scale_y_continuous(name = "Latitude",
                       breaks = map_layers$lat.breaks) +
    theme_bw() +
    theme(axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          strip.text = element_text(size = 10)) +
    guides(color = "none")
  
  h <- ggplot() +
    geom_sf(data = map_layers$bathymetry) +
    geom_sf(data = pred_grid |> filter(year(date) %in% c((1994+6*i):(1999+6*i)))|> mutate(source = 'HYCOM'),
            aes(fill = hycom_T, col = hycom_T)) +
    scale_fill_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
    scale_color_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
    geom_sf(data = map_layers$bathymetry) +
    geom_sf(data = map_layers$akland) +
    labs(fill = "Temperature", col = '') +
    facet_grid(source~year(date)) +
    geom_sf(data = map_layers$graticule, color = "grey70", alpha = 0.5) +
    coord_sf(xlim = map_layers$plot.boundary$x,
             ylim = map_layers$plot.boundary$y) +
    scale_x_continuous(name = "Longitude",
                       breaks = map_layers$lon.breaks) +
    scale_y_continuous(name = "Latitude",
                       breaks = map_layers$lat.breaks) +
    theme_bw() +
    theme(axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          strip.text = element_text(size = 10)) +
    guides(color = "none")
  
  g <- ggplot() +
    geom_sf(data = map_layers$bathymetry) +
    geom_sf(data = pred_grid |> filter(year(date) %in% c((1994+6*i):(1999+6*i)))|> mutate(source = 'BCM'),
            aes(fill = gam_T, col = gam_T)) +
    scale_fill_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
    scale_color_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
    geom_sf(data = map_layers$bathymetry) +
    geom_sf(data = map_layers$akland) +
    labs(fill = "Temperature", col = '') +
    facet_grid(source~year(date)) +
    geom_sf(data = map_layers$graticule, color = "grey70", alpha = 0.5) +
    coord_sf(xlim = map_layers$plot.boundary$x,
             ylim = map_layers$plot.boundary$y) +
    scale_x_continuous(name = "Longitude",
                       breaks = map_layers$lon.breaks) +
    scale_y_continuous(name = "Latitude",
                       breaks = map_layers$lat.breaks) +
    theme_bw() +
    theme(axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          strip.text = element_text(size = 10)) +
    guides(color = "none")
  
  plots <- plot_grid(s + theme(legend.position = "none", strip.background = element_blank(), 
                               axis.title=element_blank(), axis.text.x=element_blank()),
                     NULL,
                     h + theme(legend.position = "none", strip.background = element_blank(), 
                               axis.title.x=element_blank(), axis.text.x=element_blank(), strip.text.x = element_blank()), 
                     NULL,
                     g + theme(legend.position = "none", strip.background = element_blank(), 
                               axis.title.y=element_blank(),strip.text.x = element_blank()), 
                     ncol = 1, align = "hv", rel_heights = c(1, -0.1, 1, -0.1, 1))
  
  plot_grid(plots, legend, ncol = 2, rel_widths = c(1, 0.1))
  
  ggsave(paste0("results/plots/cpi_suppl_", i + 1,".png"), bg = "white", 
         width = 32, height = 21, units = 'cm', dpi = 300)
}
