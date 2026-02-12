##################
# Make year and EBS spatial plots from partial residuals
##################

# Load packages needed
pkgs <- c('ggplot2', 'dplyr', 'sf', 'ggspatial', 'cowplot', 'gratia')
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE, quietly = TRUE)

# Load water column models 
mod_wc_goa <- readRDS(paste0('results/model/GOA_wc_mod.rds'))
mod_wc_ai <- readRDS(paste0('results/model/AI_wc_mod.rds'))
mod_wc_ebs <- readRDS(paste0('results/model/EBS_wc_mod.rds'))

# Now pull bottom temp models
mod_b_goa <- readRDS(paste0('results/model/GOA_bot_mod.rds'))
mod_b_ai <- readRDS(paste0('results/model/AI_bot_mod.rds'))
mod_b_ebs <- readRDS(paste0('results/model/EBS_bot_mod.rds'))

# Year random effects for shallow (< 250m) water column
sm_goa_sh <- smooth_estimates(mod_wc_goa, select = 's(Year_fct):Depth_fct2shallow') |> 
  mutate(area = 'Water column', mod_reg = 'GOA')
sm_ai_sh <- smooth_estimates(mod_wc_ai, select = 's(Year_fct):Depth_fct2shallow') |> 
  mutate(area = 'Water column', mod_reg = 'AI')
sm_ebs_sh <- smooth_estimates(mod_wc_ebs, select = 's(Year_fct):Depth_fct2shallow') |> 
  mutate(area = 'Water column', mod_reg = 'EBS')

# Year random effects from Shallow (< 250m) for bottom
sm_goa_b_sh <- smooth_estimates(mod_b_goa, select = 's(Year_fct):Depth_fct2shallow') |> 
  mutate(area = 'Bottom', mod_reg = 'GOA')
sm_ai_b_sh <- smooth_estimates(mod_b_ai, select = 's(Year_fct):Depth_fct2shallow') |> 
  mutate(area = 'Bottom', mod_reg = 'AI')
sm_ebs_b_sh <- smooth_estimates(mod_b_ebs, select = 's(Year_fct):Depth_fct2shallow') |> 
  mutate(area = 'Bottom', mod_reg = 'EBS')

# Year random effects for deep (> 250m) water column
sm_goa_dp <- smooth_estimates(mod_wc_goa, select = 's(Year_fct):Depth_fct2deep') |> 
  mutate(area = 'Water column', mod_reg = 'GOA')
sm_ai_dp <- smooth_estimates(mod_wc_ai, select = 's(Year_fct):Depth_fct2deep') |> 
  mutate(area = 'Water column', mod_reg = 'AI')
sm_ebs_dp <- smooth_estimates(mod_wc_ebs, select = 's(Year_fct):Depth_fct2deep') |> 
  mutate(area = 'Water column', mod_reg = 'EBS')

# Year random effects from deep (> 250m) for bottom
sm_goa_b_dp <- smooth_estimates(mod_b_goa, select = 's(Year_fct):Depth_fct2deep') |> 
  mutate(area = 'Bottom', mod_reg = 'GOA')
sm_ai_b_dp <- smooth_estimates(mod_b_ai, select = 's(Year_fct):Depth_fct2deep') |> 
  mutate(area = 'Bottom', mod_reg = 'AI')
sm_ebs_b_dp <- smooth_estimates(mod_b_ebs, select = 's(Year_fct):Depth_fct2deep') |> 
  mutate(area = 'Bottom', mod_reg = 'EBS')

year_all <- bind_rows(sm_goa_sh, sm_ai_sh, sm_ebs_sh, sm_goa_b_sh, sm_ai_b_sh, sm_ebs_b_sh,
                      sm_goa_dp, sm_ai_dp, sm_ebs_dp, sm_goa_b_dp, sm_ai_b_dp, sm_ebs_b_dp) |> 
  mutate(year = as.numeric(as.character(Year_fct)),
         Depth_fct2 = factor(ifelse(Depth_fct2 == 'deep', '250+ m', '20 to 250 m'),
                             levels = c('20 to 250 m', '250+ m')),
         area = factor(area, levels = c('Water column', 'Bottom')))

ggplot(year_all %>% add_confint, aes(y = .estimate, x = year, col = mod_reg)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = .lower_ci, ymax = .upper_ci, col = mod_reg), width = 0,
                position = position_dodge(width=0.5)) +
  scale_color_manual(values = c('#0072B2', '#D55E00', '#009E73')) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 0.5, col = 'grey30') +
  facet_grid(area~Depth_fct2) +
  labs(x = 'Year', y = 'Partial effect', col = 'Region') +
  scale_x_continuous(minor_breaks = seq(1995, 2020, 5), breaks = seq(1995, 2020, 5)) +
  scale_y_continuous(minor_breaks = seq(-2, 2, 1), breaks = seq(-2, 2, 1), limits = c(-2.1, 2.1)) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.background = element_blank(),
        strip.text = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position = 'top')

ggsave('results/plots/Fig4_yr_effect.png', bg = 'white', 
              width = 24, height = 20, units = 'cm', dpi = 300)
       
# Spatial effect in the EBS (bottom <250m shown)
# Get map data ('auto' returns Alaska Albers)
EBS <- akgfmaps::get_base_layers(select.region = 'ebs', set.crs = 'auto')

# Bottom
sm_ebs_b_sh <- smooth_estimates(mod_b_ebs, dist = 0.025, select = 'te(longitude,latitude):Depth_fct2shallow') |> 
  select(.estimate, latitude, longitude)

ebs_b_sf <- st_as_sf(sm_ebs_b_sh |> na.omit(), coords = c('longitude', 'latitude'), crs = 'EPSG:4326') |> 
  akgfmaps::transform_data_frame_crs(in.crs = '+proj=longlat', out.crs = EBS$crs)

ggplot() +
  geom_sf(data = EBS$bathymetry) +
  geom_point(data = ebs_b_sf, aes(x, y, col = .estimate), size = 2) +
  scale_color_gradient2(low = 'steelblue', high = 'firebrick2') +
  geom_sf(data = EBS$bathymetry) +
  geom_sf(data = EBS$akland) +
  labs(col = 'Partial effect') +
  geom_sf(data = EBS$graticule, color = 'grey70', alpha = 0.5) +
  coord_sf(xlim = EBS$plot.boundary$x,
           ylim = EBS$plot.boundary$y) +
  scale_x_continuous(name = 'Longitude', 
                     breaks = EBS$lon.breaks) + 
  scale_y_continuous(name = 'Latitude', 
                     breaks = EBS$lat.breaks) + 
  annotation_scale(location = 'bl', width_hint = 0.4, height = unit(0.5, 'cm'), pad_x = unit(1, 'cm')) +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                         pad_x = unit(0.0, 'in'), pad_y = unit(0.2, 'in'),
                         style = north_arrow_fancy_orienteering,
                         height = unit(2, 'cm'), width = unit(2, 'cm')) +
  theme_bw()

ggsave(file = 'results/plots/Fig5_ebs_map_bot.png', height = 20, width = 20, units = 'cm')

# Effect by region and depth
ebs_b_hy_sm <- smooth_estimates(mod_b_ebs,  dist = 0.1,
                                select = 'te(HYCOM_temp,Depth)') |> 
  select(.estimate, .se, HYCOM_temp, Depth) %>% 
  mutate(region = 'EBS', type = 'Bottom')

ebs_b_hy_sm$new_est = ifelse(ebs_b_hy_sm$Depth < 250, coef(mod_b_ebs)[1] + ebs_b_hy_sm$.estimate,  (coef(mod_b_ebs)[2] + coef(mod_b_ebs)[1]) + ebs_b_hy_sm$.estimate)

goa_b_hy_sm <- smooth_estimates(mod_b_goa,  dist = 0.1,
                                select = 'te(HYCOM_temp,Depth)') |> 
  select(.estimate, .se, HYCOM_temp, Depth)%>% 
  mutate(region = 'GOA', type = 'Bottom')

goa_b_hy_sm$new_est = ifelse(goa_b_hy_sm$Depth < 250, coef(mod_b_goa)[1] + goa_b_hy_sm$.estimate,  (coef(mod_b_goa)[2] + coef(mod_b_goa)[1]) + goa_b_hy_sm$.estimate)

ai_b_hy_sm <- smooth_estimates(mod_b_ai,  dist = 0.1,
                                select = 'te(HYCOM_temp,Depth)') |> 
  select(.estimate, .se, HYCOM_temp, Depth)%>% 
  mutate(region = 'AI', type = 'Bottom')

ai_b_hy_sm$new_est = ifelse(ai_b_hy_sm$Depth < 250, coef(mod_b_ai)[1] + ai_b_hy_sm$.estimate,  (coef(mod_b_ai)[2] + coef(mod_b_ai)[1]) + ai_b_hy_sm$.estimate)

ebs_wc_hy_sm <- smooth_estimates(mod_wc_ebs,  dist = 0.1,
                                select = c('te(HYCOM_temp,Depth)')) |> 
  select(.estimate, .se, HYCOM_temp, Depth) %>% 
  mutate(region = 'EBS', type = 'Water Column')

ebs_wc_hy_sm$new_est = ifelse(ebs_wc_hy_sm$Depth < 250, coef(mod_wc_ebs)[1] + ebs_wc_hy_sm$.estimate,  (coef(mod_wc_ebs)[2] + coef(mod_wc_ebs)[1]) + ebs_wc_hy_sm$.estimate)

goa_wc_hy_sm <- smooth_estimates(mod_wc_goa,  dist = 0.1,
                                select = 'te(HYCOM_temp,Depth)') |> 
  select(.estimate, .se, HYCOM_temp, Depth) %>% 
  mutate(region = 'GOA', type = 'Water Column')

goa_wc_hy_sm$new_est = ifelse(goa_wc_hy_sm$Depth < 250, coef(mod_wc_goa)[1] + goa_wc_hy_sm$.estimate,  (coef(mod_wc_goa)[2] + coef(mod_wc_goa)[1]) + goa_wc_hy_sm$.estimate)

ai_wc_hy_sm <- smooth_estimates(mod_wc_ai, dist = 0.1,
                               select = 'te(HYCOM_temp,Depth)') |> 
  select(.estimate, .se, HYCOM_temp, Depth) %>% 
  mutate(region = 'AI', type = 'Water Column')

ai_wc_hy_sm$new_est = ifelse(ai_wc_hy_sm$Depth < 250, coef(mod_wc_ai)[1] + ai_wc_hy_sm$.estimate,  (coef(mod_wc_ai)[2] + coef(mod_wc_ai)[1]) + ai_wc_hy_sm$.estimate)

all_hy_sm <- bind_rows(ebs_b_hy_sm, goa_b_hy_sm, ai_b_hy_sm, ebs_wc_hy_sm, goa_wc_hy_sm, ai_wc_hy_sm)

ggplot(data = all_hy_sm, aes(HYCOM_temp, Depth, z = new_est)) +
  geom_contour_filled(alpha = 0.8) +
  scale_fill_manual(values = c(colorRampPalette(c('steelblue', '#E7EFF5'))(8), colorRampPalette(c('#F9B8B8', 'firebrick2'))(3))) +
  labs(x = 'HYCOM temperature (°C)', y = 'Depth (m)', fill = 'Partial effect') +
  facet_grid(factor(type, levels = c('Water Column', 'Bottom'))~region) +
  geom_contour(breaks = c(0), color = 'black', size = 1) +
  scale_y_reverse() + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14))

ggsave(file = 'results/plots/FigS14_hycom_by_depth_effect.png', height = 20, width = 22, units = 'cm')
