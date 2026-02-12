##################
# Make figure of HYCOM data and model results compared to observations
##################

# load packages
# Load packages needed
pkgs <- c('ggplot2', 'ggnewscale', 'cowplot', 'dplyr', 'mgcv')
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE, quietly = TRUE)

# Get all data to run selected models:
# load bottom data
bot4use <- readRDS('data/bot4use.rds')

#load water column data
dat4use <- readRDS('data/dat4use.rds')

reg <- dat4use |> distinct(mod_reg)

# Run and save selected models (EBS = mod 5, AI/GOA mod 3)
wc_dat <- data.frame()
b_dat <- data.frame()
for( i in 1:nrow(reg) ) {
  dat = dat4use |> filter(mod_reg == reg$mod_reg[i])
  dat_bot <- bot4use |> filter(mod_reg == reg$mod_reg[i])
  
  k_yr = length(unique(dat$Year))
  if( reg$mod_reg[i] == 'EBS') {
    wc_mod <- bam(T_diff ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + 
                    s(Year_fct, by = Depth_fct2, bs = 're') + 
                    te(longitude, latitude, by = Depth_fct2, m = 1, k = 20),
                  data = dat, method = 'fREML', 
                  nthreads = 10, discrete = TRUE)
    b_mod <- bam(T_diff2 ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + 
                   s(Year_fct, by = Depth_fct2, bs = 're') + 
                   te(longitude, latitude, by = Depth_fct2, m = 1, k = 20),
                 data = dat_bot, method = 'fREML', 
                 nthreads = c(4, 1), discrete = TRUE)
  } else {
    wc_mod <- bam(T_diff ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + 
                    s(Year_fct, by = Depth_fct2, bs = 're'),
                  data = dat, method = 'fREML', 
                  nthreads = 10, discrete = TRUE)
    b_mod <- bam(T_diff2 ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + 
                   s(Year_fct, by = Depth_fct2, bs = 're'),
                 data = dat_bot, method = 'fREML', 
                 nthreads = c(4, 1), discrete = TRUE)
  }
  saveRDS(wc_mod, paste0('results/model/', reg$mod_reg[i], '_wc_mod.rds')) 
  saveRDS(b_mod, paste0('results/model/', reg$mod_reg[i], '_bot_mod.rds'))
  
  dat$pred_diff = wc_mod$fitted.values
  dat$pred_temp = dat$HYCOM_temp + dat$pred_diff
  
  dat_bot$pred_diff = b_mod$fitted.values
  dat_bot$pred_temp = dat_bot$HYCOM_temp + dat_bot$pred_diff
  
  wc_dat <- bind_rows(wc_dat, dat) |> 
    mutate(pred_temp = ifelse(pred_temp < -1.8, -1.8, pred_temp),
           area = 'Water column')
  b_dat <- bind_rows(b_dat, dat_bot) |> 
    mutate(pred_temp = ifelse(pred_temp < -1.8, -1.8, pred_temp),
           area = 'Bottom')
}

# Just to see mean values by region
wc_dat |> group_by(mod_reg) |> summarize(mn_temp = mean(obs_temp), sd_temp = sd(obs_temp))
b_dat |> group_by(mod_reg) |> summarize(mn_temp = mean(obs_temp), sd_temp = sd(obs_temp))

# Make plots of HYCOM predicted vs. observations 
wc <- ggplot(wc_dat, aes(HYCOM_temp, obs_temp)) + 
  geom_hex(binwidth = c(0.25, 0.25)) +
  scale_fill_gradient(low = "lightblue", high = "#002B7B") +
  labs(fill = NULL) +
  new_scale_fill() +
  geom_hex(aes(pred_temp, obs_temp), binwidth = c(0.25, 0.25), alpha = 0.5) +
  geom_abline(aes(slope=1, intercept=0), lty=2, linewidth = 1) +
  geom_smooth(col = "#002B7B", method = 'lm', linewidth = 1, se = FALSE) +
  geom_smooth(aes(pred_temp, obs_temp), method = 'lm',  col= "#A12223", linewidth = 1, se = FALSE) +
  scale_fill_gradient(low = "lightpink", high = "firebrick") +
  theme_bw() +
  facet_grid(area ~ mod_reg) +
  scale_x_continuous(breaks = c(0, 5, 10, 15), limits = c(-3.1, 17.5)) +
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(-3.1, 17.5)) +
  theme(legend.position = "none",
        strip.background.x = element_blank(),
        strip.background.y = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.text = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        axis.line = element_line(linewidth = .6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

b <- ggplot(b_dat, aes(HYCOM_temp, obs_temp)) + 
  geom_hex(binwidth = c(0.25, 0.25)) +
  scale_fill_gradient(low = "lightblue", high = "#002B7B") +
  labs(fill = NULL) +
  new_scale_fill() +
  geom_hex(aes(pred_temp, obs_temp), binwidth = c(0.25, 0.25), alpha = 0.5) +
  geom_abline(aes(slope=1, intercept=0), lty=2, linewidth = 1) +
  geom_smooth(col = "#002B7B", method = 'lm', linewidth = 1, se = FALSE) +
  geom_smooth(data = b_dat, aes(pred_temp, obs_temp), method = 'lm',  col= "#A12223", linewidth = 1, se = FALSE) +
  scale_fill_gradient(low = "lightpink", high = "firebrick") +
  theme_bw() +
  facet_grid(area ~ mod_reg) +
  scale_x_continuous(breaks = c(0, 5, 10, 15), limits = c(-3.1, 17.5)) +
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(-3.1, 17.5)) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_blank(),
        axis.line = element_line(linewidth = .6),
        strip.background.x = element_blank(),
        strip.background.y = element_blank(),
        strip.text.y = element_text(size = 14),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_grid(plot_grid(wc, b, labels=c("", ""), ncol = 1, align = 'v'), scale = 0.95) + #perhaps reduce this for a bit more space
  draw_label("Model predicted temperature (°C)", x=0.5, y=  0, vjust=-0.5, angle= 0) +
  draw_label("Observed temperature (°C)", x=  0, y=0.5, vjust= 1.5, angle=90) 

ggsave('results/plots/Fig2_obs_v_mod.png', width = 24, height = 16, units = 'cm', bg = 'white',  dpi = 300)

# For presenting, and just to see how HYCOM is alone without the BCM on top.
wc <- ggplot(wc_dat, aes(HYCOM_temp, obs_temp)) +
  geom_hex(binwidth = c(0.25, 0.25)) +
  scale_fill_gradient(low = "lightblue", high = "#002B7B") +
  labs(fill = NULL) +
  geom_abline(aes(slope=1, intercept=0), lty=2, linewidth = 1) +
  geom_smooth(col = '#002B7B', method = 'lm', linewidth = 1, se = FALSE) +
  theme_bw() +
  facet_grid(area ~ mod_reg) +
  scale_x_continuous(breaks = c(0, 5, 10, 15), limits = c(-3.1, 17.5)) +
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(-3.1, 17.5)) +
  theme(legend.position = 'none',
        strip.background.x = element_blank(),
        strip.background.y = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.text = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        axis.line = element_line(linewidth = .6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

b <- ggplot(b_dat, aes(HYCOM_temp, obs_temp)) +
  geom_hex(binwidth = c(0.25, 0.25)) +
  scale_fill_gradient(low = "lightblue", high = "#002B7B") +
  labs(fill = NULL) +
  geom_abline(aes(slope=1, intercept=0), lty=2, linewidth = 1) +
  geom_smooth(col = '#002B7B', method = 'lm', linewidth = 1, se = FALSE) +
  theme_bw() +
  facet_grid(area ~ mod_reg) +
  scale_x_continuous(breaks = c(0, 5, 10, 15), limits = c(-3.1, 17.5)) +
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(-3.1, 17.5)) +
  theme(legend.position = 'none',
        axis.text = element_text(size = 14),
        axis.title = element_blank(),
        axis.line = element_line(linewidth = .6),
        strip.background.x = element_blank(),
        strip.background.y = element_blank(),
        strip.text.y = element_text(size = 14),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_grid(plot_grid(wc, b, labels=c('', ''), ncol = 1, align = 'v'), scale = 0.95) + #perhaps reduce this for a bit more space
  draw_label('Model predicted temperature (°C)', x=0.5, y=  0, vjust=-0.5, angle= 0) +
  draw_label('Observed temperature (°C)', x=  0, y=0.5, vjust= 1.5, angle=90)
# 
# ggsave('results/plots/obs_v_mod_figure_hycom_only.png', width = 24, height = 16, units = 'cm', bg = 'white',  dpi = 300)
