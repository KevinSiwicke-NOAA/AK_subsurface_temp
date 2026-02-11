##################
# Make figure of HYCOM data and model results compared to observations
##################

# load packages
# Load packages needed
pkgs <- c("ggplot2", "ggnewscale", "cowplot", "nmfspalette", "dplyr")
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE, quietly = TRUE)

wc_dat <- readRDS("data/wc_dat.rds")
wc_dat |> group_by(mod_reg) |> summarize(mn_temp = mean(obs_temp), sd_temp = sd(obs_temp))
b_dat <- readRDS("data/b_dat.rds")
b_dat |> group_by(mod_reg) |> summarize(mn_temp = mean(obs_temp), sd_temp = sd(obs_temp))

wc <- ggplot(wc_dat, aes(HYCOM_temp, obs_temp)) + 
  geom_hex(binwidth = c(0.25, 0.25)) +
  scale_fill_nmfs(palette = "oceans", discrete = FALSE) +
  labs(fill = NULL) +
  new_scale_fill() +
  geom_hex(aes(pred_temp, obs_temp), binwidth = c(0.25, 0.25), alpha = 0.5) +
  geom_abline(aes(slope=1, intercept=0), lty=2, linewidth = 1) +
  geom_smooth(col = "#002B7B", method = 'gam', linewidth = 1, se = FALSE) +
  geom_smooth(aes(pred_temp, obs_temp), method = 'gam',  col= "#A12223", linewidth = 1, se = FALSE) +
  scale_fill_nmfs(palette = "coral", discrete = FALSE) +
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
  scale_fill_nmfs(palette = "oceans", discrete = FALSE) +
  labs(fill = NULL) +
  new_scale_fill() +
  geom_hex(aes(pred_temp, obs_temp), binwidth = c(0.25, 0.25), alpha = 0.5) +
  geom_abline(aes(slope=1, intercept=0), lty=2, linewidth = 1) +
  geom_smooth(col = "#002B7B", method = 'gam', linewidth = 1, se = FALSE) +
  geom_smooth(data = b_dat, aes(pred_temp, obs_temp), method = 'gam',  col= "#A12223", linewidth = 1, se = FALSE) +
  scale_fill_nmfs(palette = "coral", discrete = FALSE) +
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


ggsave("results/plots/obs_v_mod_figure_gams.png", width = 24, height = 16, units = 'cm', bg = "white", dpi = 300)

wc <- ggplot(wc_dat, aes(HYCOM_temp, obs_temp)) + 
  geom_hex(binwidth = c(0.25, 0.25)) +
  scale_fill_nmfs(palette = "oceans", discrete = FALSE) +
  labs(fill = NULL) +
  new_scale_fill() +
  geom_hex(aes(pred_temp, obs_temp), binwidth = c(0.25, 0.25), alpha = 0.5) +
  geom_abline(aes(slope=1, intercept=0), lty=2, linewidth = 1) +
  geom_smooth(col = "#002B7B", method = 'lm', linewidth = 1, se = FALSE) +
  geom_smooth(aes(pred_temp, obs_temp), method = 'lm',  col= "#A12223", linewidth = 1, se = FALSE) +
  scale_fill_nmfs(palette = "coral", discrete = FALSE) +
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
  scale_fill_nmfs(palette = "oceans", discrete = FALSE) +
  labs(fill = NULL) +
  new_scale_fill() +
  geom_hex(aes(pred_temp, obs_temp), binwidth = c(0.25, 0.25), alpha = 0.5) +
  geom_abline(aes(slope=1, intercept=0), lty=2, linewidth = 1) +
  geom_smooth(col = "#002B7B", method = 'lm', linewidth = 1, se = FALSE) +
  geom_smooth(data = b_dat, aes(pred_temp, obs_temp), method = 'lm',  col= "#A12223", linewidth = 1, se = FALSE) +
  scale_fill_nmfs(palette = "coral", discrete = FALSE) +
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

ggsave("results/plots/obs_v_mod_figure_lms.png", width = 24, height = 16, units = 'cm', bg = "white",  dpi = 300)

wc <- ggplot(wc_dat, aes(HYCOM_temp, obs_temp)) + 
  geom_hex(binwidth = c(0.25, 0.25)) +
  scale_fill_nmfs(palette = "oceans", discrete = FALSE) +
  labs(fill = NULL) +
  # new_scale_fill() +
  # geom_hex(aes(pred_temp, obs_temp), binwidth = c(0.25, 0.25), alpha = 0.5) +
  geom_abline(aes(slope=1, intercept=0), lty=2, linewidth = 1) +
  geom_smooth(col = "#002B7B", method = 'lm', linewidth = 1, se = FALSE) +
  # geom_smooth(aes(pred_temp, obs_temp), method = 'lm',  col= "#A12223", linewidth = 1, se = FALSE) +
  # scale_fill_nmfs(palette = "coral", discrete = FALSE) +
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
  scale_fill_nmfs(palette = "oceans", discrete = FALSE) +
  labs(fill = NULL) +
  # new_scale_fill() +
  # geom_hex(aes(pred_temp, obs_temp), binwidth = c(0.25, 0.25), alpha = 0.5) +
  geom_abline(aes(slope=1, intercept=0), lty=2, linewidth = 1) +
  geom_smooth(col = "#002B7B", method = 'lm', linewidth = 1, se = FALSE) +
  # geom_smooth(data = b_dat, aes(pred_temp, obs_temp), method = 'lm',  col= "#A12223", linewidth = 1, se = FALSE) +
  # scale_fill_nmfs(palette = "coral", discrete = FALSE) +
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

ggsave("results/plots/obs_v_mod_figure_hycom_only.png", width = 24, height = 16, units = 'cm', bg = "white",  dpi = 300)
