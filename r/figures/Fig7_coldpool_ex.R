##################
# Make figures for the Cold Pool in the Eastern Bering Sea
##################

# Install AFSC GAP program's coldpool package first
# devtools::install_github("afsc-gap-products/coldpool")

# Load packages needed
pkgs <- c("ggplot2", "dplyr", "sf", "cowplot", "lubridate","coldpool", "tidyterra")
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE, quietly = TRUE)

##################
# Example small cold pool (2018( versus large cold pool (2012)
##################
# Load map of EBS for calculating mean bottom temperature in outer shelf strata using Groundfish Assessment Program akgfmaps package
# Map includes the northern Bering Sea when sampled and in all predictions, but this region is not used in the calculated metrics
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

# Load gridded data (5 km x 5 km) and use determine which cells meet the criteria of <= 2 or <= 0 degrees C
pred_grid <- readRDS(file = "data/gridded_cpi_data.rds") |> 
  mutate(year = year(date),
         h_sub_2 = ifelse(hycom_T > 2, 0, 1),
         g_sub_2 = ifelse(gam_T > 2, 0, 1),
         h_sub_0 = ifelse(hycom_T > 0, 0, 1),
         g_sub_0 = ifelse(gam_T > 0, 0, 1))

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

ggsave(paste0("results/plots/Fig7_coldpool_ex.png"), bg = "white", 
       width = 16, height = 24, units = 'cm', dpi = 300)

# Make annual plots of HYCOM prediction and corrected cold pool to include in the supplemental
# gap <- unwrap(coldpool::ebs_bottom_temperature) |> 
#   tidyterra::select(Survey_temp = 13) |> 
#   tidyterra::mutate(year = 1994, source = "Survey")
# gap_sf <- st_as_sf(as.polygons(gap, trunc=FALSE, dissolve=FALSE))
# 
# for(y in c(1995:2023)) {
#   if(y %in% c(2010, 2017, 2018, 2019, 2021, 2022, 2023)) {
#     relation <- c(2010, 2017, 2018, 2019, 2021, 2022, 2023)
#     index <- which(relation == y)
#     dat <- unwrap(coldpool::nbs_ebs_bottom_temperature) |> 
#       tidyterra::select(Survey_temp = index) |> 
#       tidyterra::mutate(year = y, source = "Survey")
#   }
#   else {
#     dat <- unwrap(coldpool::ebs_bottom_temperature) |> 
#       tidyterra::select(Survey_temp = (y - 1981)) |> 
#       tidyterra::mutate(year = y, source = "Survey")
#   }
#   
#   if(y == 2020) {
#     dat$Survey_temp = NA
#     dat$Survey_temp[1:10] = 1 # this is to allow 2020 to have a map even though there was no survey
#   }
#   
#   dat_sf <- st_as_sf(as.polygons(dat, trunc=FALSE, dissolve=FALSE))
#   gap_sf <- rbind(gap_sf, dat_sf)
# }
# 
# for(i in 0:4) {
#   s <- ggplot() +
#     geom_sf(data = map_layers$bathymetry) +
#     geom_sf(data = gap_sf |> filter(year %in% c((1994+6*i):(1999+6*i))),
#             aes(fill = Survey_temp, col = Survey_temp)) +
#     scale_fill_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
#     scale_color_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
#     geom_sf(data = map_layers$bathymetry) +
#     geom_sf(data = map_layers$akland) +
#     labs(fill = "Temperature", col = '') +
#     facet_grid(source~year) +
#     geom_sf(data = map_layers$graticule, color = "grey70", alpha = 0.5) +
#     coord_sf(xlim = map_layers$plot.boundary$x,
#              ylim = map_layers$plot.boundary$y) +
#     scale_x_continuous(name = "Longitude",
#                        breaks = map_layers$lon.breaks) +
#     scale_y_continuous(name = "Latitude",
#                        breaks = map_layers$lat.breaks) +
#     theme_bw() +
#     theme(axis.title = element_text(size = 10),
#           axis.text = element_text(size = 10),
#           legend.text = element_text(size = 10),
#           legend.title = element_text(size = 10),
#           strip.text = element_text(size = 10)) +
#     guides(color = "none")
#   
#   h <- ggplot() +
#     geom_sf(data = map_layers$bathymetry) +
#     geom_sf(data = pred_grid |> filter(year(date) %in% c((1994+6*i):(1999+6*i)))|> mutate(source = 'HYCOM'),
#             aes(fill = hycom_T, col = hycom_T)) +
#     scale_fill_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
#     scale_color_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
#     geom_sf(data = map_layers$bathymetry) +
#     geom_sf(data = map_layers$akland) +
#     labs(fill = "Temperature", col = '') +
#     facet_grid(source~year(date)) +
#     geom_sf(data = map_layers$graticule, color = "grey70", alpha = 0.5) +
#     coord_sf(xlim = map_layers$plot.boundary$x,
#              ylim = map_layers$plot.boundary$y) +
#     scale_x_continuous(name = "Longitude",
#                        breaks = map_layers$lon.breaks) +
#     scale_y_continuous(name = "Latitude",
#                        breaks = map_layers$lat.breaks) +
#     theme_bw() +
#     theme(axis.title = element_text(size = 10),
#           axis.text = element_text(size = 10),
#           legend.text = element_text(size = 10),
#           legend.title = element_text(size = 10),
#           strip.text = element_text(size = 10)) +
#     guides(color = "none")
#   
#   g <- ggplot() +
#     geom_sf(data = map_layers$bathymetry) +
#     geom_sf(data = pred_grid |> filter(year(date) %in% c((1994+6*i):(1999+6*i)))|> mutate(source = 'BCM'),
#             aes(fill = gam_T, col = gam_T)) +
#     scale_fill_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
#     scale_color_viridis_b(breaks = seq(-1, 8, 1), name = expression('BT'~(degree*C)), option = "H") +
#     geom_sf(data = map_layers$bathymetry) +
#     geom_sf(data = map_layers$akland) +
#     labs(fill = "Temperature", col = '') +
#     facet_grid(source~year(date)) +
#     geom_sf(data = map_layers$graticule, color = "grey70", alpha = 0.5) +
#     coord_sf(xlim = map_layers$plot.boundary$x,
#              ylim = map_layers$plot.boundary$y) +
#     scale_x_continuous(name = "Longitude",
#                        breaks = map_layers$lon.breaks) +
#     scale_y_continuous(name = "Latitude",
#                        breaks = map_layers$lat.breaks) +
#     theme_bw() +
#     theme(axis.title = element_text(size = 10),
#           axis.text = element_text(size = 10),
#           legend.text = element_text(size = 10),
#           legend.title = element_text(size = 10),
#           strip.text = element_text(size = 10)) +
#     guides(color = "none")
#   
#   plots <- plot_grid(s + theme(legend.position = "none", strip.background = element_blank(), 
#                                axis.title=element_blank(), axis.text.x=element_blank()),
#                      NULL,
#                      h + theme(legend.position = "none", strip.background = element_blank(), 
#                                axis.title.x=element_blank(), axis.text.x=element_blank(), strip.text.x = element_blank()), 
#                      NULL,
#                      g + theme(legend.position = "none", strip.background = element_blank(), 
#                                axis.title.y=element_blank(),strip.text.x = element_blank()), 
#                      ncol = 1, align = "hv", rel_heights = c(1, -0.1, 1, -0.1, 1))
#   
#   plot_grid(plots, legend, ncol = 2, rel_widths = c(1, 0.1))
#   
#   ggsave(paste0("results/plots/cpi_suppl_", i + 1,".png"), bg = "white", 
#          width = 32, height = 21, units = 'cm', dpi = 300)
# }
