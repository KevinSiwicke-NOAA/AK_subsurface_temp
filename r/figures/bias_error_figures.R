##################
# Make figure of biasd and error plots
##################

pkgs <- c("mgcv", "ggplot2", "dplyr", "forcats", "tidyr", "gratia", "lubridate", 
          "Metrics", "corrplot", "readr", "ggh4x", "cowplot")
## install.packages(pkgs, Ncpus = 4)
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE,
       quietly = TRUE)

# Laod water column data 
info_reg_df <- readRDS("results/wc_info_reg_df_block.rds") %>% 
  mutate(area = "Water column")
depth_info_reg_df <- readRDS("results/wc_depth_info_reg_df_block.rds") %>% 
  mutate(area = "Water column")

# Load bottom data
bot_info_reg_df <- readRDS("results/bot_info_reg_df_block.rds")  %>% 
  mutate(area = "Bottom")
bot_depth_info_reg_df <-  readRDS("results/bot_depth_info_reg_df_block.rds") %>% 
  mutate(area = "Bottom")

info_reg <- bind_rows(info_reg_df, bot_info_reg_df) %>% 
  mutate(area = factor(area, levels = c("Water column", "Bottom")))

# Plot overall results
cor <- ggplot(info_reg) +
  geom_point(aes(mod_num, cor_mn, col = mod_reg),
             position=position_dodge(width=0.5)) +
  geom_errorbar(aes(mod_num, ymin = cor_mn - 1.96*cor_sd/sqrt(10),
                    ymax = cor_mn + 1.96*cor_sd/sqrt(10), col = mod_reg), width = 0,
                position=position_dodge(width=0.5)) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  facet_wrap(~area, ncol = 1) +
  labs(x = "Model Number", y = "Pearson's correlation") +
  theme_bw() +
  guides(col=guide_legend(title="Region")) +
  theme(legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

bias <- ggplot(info_reg) +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey30") +
  geom_point(aes(mod_num, bias_mn, col = mod_reg),
             position=position_dodge(width=0.5)) +
  geom_errorbar(aes(mod_num, ymin = bias_mn - 1.96*bias_sd/sqrt(10),
                    ymax = bias_mn + 1.96*bias_sd/sqrt(10), col = mod_reg), width = 0,
                position=position_dodge(width=0.5)) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  facet_wrap(~area, ncol = 1) +
  labs(x = "Model Number", y = "Bias") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

mae <- ggplot(info_reg) +
  geom_point(aes(mod_num, mae_mn, col = mod_reg),
             position=position_dodge(width=0.5)) +
  geom_errorbar(aes(mod_num, ymin = mae_mn - 1.96*mae_sd/sqrt(10),
                    ymax = mae_mn + 1.96*mae_sd/sqrt(10), col = mod_reg), width = 0,
                position=position_dodge(width=0.5)) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  facet_wrap(~area, ncol = 1) +
  labs(x = "Model Number", y = "Mean absolute error") +
  guides(col=guide_legend(title="Region")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plots <- plot_grid(bias + theme(legend.position = "none", strip.background = element_blank(), strip.text.x = element_blank(), axis.title.x = element_blank()), 
                   cor + theme(legend.position = "none", strip.background = element_blank(), strip.text.x = element_blank()), 
                   mae + theme(legend.position = "none", strip.background = element_blank(), strip.text.x = element_blank(), axis.title.x = element_blank()), 
                   ncol = 3, align = "hv") 

legend <- get_plot_component(cor, 'guide-box-top', return_all = TRUE)

plot_grid(legend, plots, ncol = 1, rel_heights = c(0.1, 1))
ggsave(file = "results/plots/mod_results_figure.png", height = 12, width = 20, units = 'cm', dpi = 300)
