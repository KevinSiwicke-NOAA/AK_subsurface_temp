pkgs <- c("ggplot2", "dplyr", "cowplot", "ggh4x",
          "forcats", "tidyr", "gratia", "lubridate", "mgcv", 
          "Metrics", "corrplot", "readr", "ggh4x", "cowplot")

## install.packages(pkgs, Ncpus = 4)
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE,
       quietly = TRUE)


# Laod water column data - a
info_reg_df <- readRDS("results/wc_info_reg_df_block.rds")|> 
  mutate(area = "Water column")
depth_info_reg_df <- readRDS("results/wc_depth_info_reg_df_block.rds")|> 
  mutate(area = "Water column")

# Load bottom data - b
bot_info_reg_df <- readRDS("results/bot_info_reg_df_block.rds") |> 
  mutate(area = "Bottom")
bot_depth_info_reg_df <-  readRDS("results/bot_depth_info_reg_df_block.rds")|> 
  mutate(area = "Bottom")

info_reg <- bind_rows(info_reg_df, bot_info_reg_df)|> 
  mutate(area = factor(area, levels = c("Water column", "Bottom")))

depth_info_reg <- bind_rows(depth_info_reg_df, bot_depth_info_reg_df)|> 
  mutate(area = factor(area, levels = c("Water column", "Bottom")),
         Depth_fct = factor(Depth_fct, 
                             levels = c('20', '25', '30', '35', '40', '45', '<50', '50', '60', 
                                        '70', '80', '90', '100', '125', '150', '200',
                                        '250', '300', '350', '400', '500', '600+')))

# Plot overall results
ggplot(info_reg) +
  geom_point(aes(mod_num, dev_expl_mn, col = mod_reg),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(mod_num, ymin = dev_expl_mn - dev_expl_sd,
                    ymax = dev_expl_mn + dev_expl_sd, col = mod_reg), width = 1,
                position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  facet_wrap(~area, ncol = 1) +
  labs(x = "Model Number", y = "Deviance Explained (%)", col = 'Region') +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.line = element_line(linewidth = 0.6),
        strip.text = element_text(size = 14))

# ggsave(file = "results/plots/cv_select/dev_expl.png", height = 20, width = 16, units = 'cm', dpi = 600)

cor <- ggplot(info_reg) +
  geom_point(aes(mod_num, cor_mn, col = mod_reg),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(mod_num, ymin = cor_mn - cor_sd,
                    ymax = cor_mn + cor_sd, col = mod_reg), width = 0.5,
                position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  facet_wrap(~area, ncol = 1) +
  labs(x = "Model Number", y = "Pearson's correlation", col = 'Region') +
  theme_bw() 

cor
# ggsave(file = "results/plots/cv_select/corr.png", height = 20, width = 16, units = 'cm', dpi = 600)

bias <- ggplot(info_reg) +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey30") +
  geom_point(aes(mod_num, bias_mn, col = mod_reg),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(mod_num, ymin = bias_mn - bias_sd,
                    ymax = bias_mn + bias_sd, col = mod_reg), width = 0.5,
                position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  facet_wrap(~area, ncol = 1) +
  labs(x = "Model Number", y = "Bias", col = 'Region') +
  theme_bw()

bias
# ggsave(file = "results/plots/cv_select/bias.png", height = 20, width = 16, units = 'cm', dpi = 600)

ggplot(info_reg) +
  geom_point(aes(mod_num, per_bias_mn, col = mod_reg),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(mod_num, ymin = per_bias_mn - per_bias_sd,
                    ymax = per_bias_mn + per_bias_sd, col = mod_reg), width = 0.5,
                position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  facet_wrap(~area, ncol = 1) +
  labs(x = "Model Number", y = "Percent Bias", col = 'Region') +
  theme_bw()

mae <- ggplot(info_reg) +
  geom_point(aes(mod_num, mae_mn, col = mod_reg),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(mod_num, ymin = mae_mn - mae_sd,
                    ymax = mae_mn + mae_sd, col = mod_reg), width = 0.5,
                position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  facet_wrap(~area, ncol = 1) +
  labs(x = "Model Number", y = "Mean absolute error", col = 'Region') +
  guides(col=guide_legend(title="Region")) +
  theme_bw()

mae  
# ggsave(file = "results/plots/cv_select/mae.png", height = 20, width = 16, units = 'cm', dpi = 600)

plots <- plot_grid(bias + theme(legend.position = "none", strip.background = element_blank(), strip.text.x = element_blank()), 
          cor + theme(legend.position = "none", strip.background = element_blank(), strip.text.x = element_blank()), 
          mae + theme(legend.position = "none", strip.background = element_blank(), strip.text.x = element_blank()), 
          ncol = 3) 

legend <- get_plot_component(mae, 'guide-box-right', return_all = TRUE)

plot_grid(plots, legend, ncol = 2, rel_widths = c(1, 0.1))
ggsave(file = "results/plots/mod_results_figure.png", height = 13, width = 20, units = 'cm', dpi = 600)

ggplot(info_reg) +
  geom_point(aes(mod_num, rmse_mn, col = mod_reg),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(mod_num, ymin = rmse_mn - rmse_sd,
                    ymax = rmse_mn + rmse_sd, col = mod_reg), width = 0.5,
                position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  facet_wrap(~area, ncol = 1) +
  labs(x = "Model Number", y = "Root mean square error") +
  theme_bw()

ggsave(file = "results/plots/cv_select/rmse.png", height = 20, width = 16, units = 'cm', dpi = 600)

ggplot(info_reg) +
  geom_point(aes(mod_num, mape_mn, col = mod_reg),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(mod_num, ymin = mape_mn - mape_sd,
                    ymax = mape_mn + mape_sd, col = mod_reg), width = 0.5,
                position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  facet_wrap(~area, ncol = 1) +
  labs(x = "Model Number", y = "Mean absolute percent error") +
  theme_bw()

# Now results by depth bins
ggplot(depth_info_reg|> filter(mod_num %in% c(0, 1, 2, 3, 4)), aes(cor_mn, Depth_fct, col = mod_num)) +
  geom_vline(aes(xintercept = 0), lty = 2, col = "grey30") +
  geom_vline(aes(xintercept = 1), lty = 2, col = "grey30") +
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = cor_mn - cor_sd,
                     xmax = cor_mn + cor_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("Pearson's correlation") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(file = "results/plots/cv_select/dep_corr_0to4.png", height = 20, width = 16, units = 'cm', dpi = 600)

ggplot(depth_info_reg|> filter(mod_num %in% c(0, 1, 2, 3, 4)), aes(bias_mn, Depth_fct, col = mod_num)) +
  geom_vline(aes(xintercept = 0), lty = 2, col = "grey30") +
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = bias_mn - bias_sd,
                     xmax = bias_mn + bias_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("Bias") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(file = "results/plots/cv_select/dep_bias_0to4.png", height = 20, width = 16, units = 'cm', dpi = 600)

ggplot(depth_info_reg|> filter(mod_num %in% c(0, 1, 2, 3, 4)), aes(per_bias_mn, Depth_fct, col = mod_num)) +
  geom_vline(aes(xintercept = 0), lty = 2, col = "grey30") +
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = per_bias_mn - per_bias_sd,
                     xmax = per_bias_mn + per_bias_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("Percent bias") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggplot(depth_info_reg|> filter(mod_num %in% c(0, 1, 2, 3, 4)), aes(mae_mn, Depth_fct, col = mod_num)) + 
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = mae_mn - mae_sd,
                     xmax = mae_mn + mae_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("MAE") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(file = "results/plots/cv_select/dep_mae_0to4.png", height = 20, width = 16, units = 'cm', dpi = 600)

ggplot(depth_info_reg|> filter(mod_num %in% c(0, 1, 2, 3, 4)), aes(rmse_mn, Depth_fct, col = mod_num)) + 
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = rmse_mn - rmse_sd,
                     xmax = rmse_mn + rmse_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("RMSE") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(file = "results/plots/cv_select/dep_rmse_0to4.png", height = 20, width = 16, units = 'cm', dpi = 600)

ggplot(depth_info_reg|> filter(mod_num %in% c(0, 1, 2, 3, 4)), aes(mape_mn, Depth_fct, col = mod_num)) + 
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = mape_mn - mape_sd,
                     xmax = mape_mn + mape_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("MAPE") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggplot(depth_info_reg|> filter(mod_num %in% c(5, 6, 7, 8)), aes(cor_mn, Depth_fct, col = mod_num)) +
  geom_vline(aes(xintercept = 0), lty = 2, col = "grey30") +
  geom_vline(aes(xintercept = 1), lty = 2, col = "grey30") +
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = cor_mn - cor_sd,
                     xmax = cor_mn + cor_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("Pearson's correlation") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(file = "results/plots/cv_select/dep_corr_5to8.png", height = 20, width = 16, units = 'cm', dpi = 600)

ggplot(depth_info_reg|> filter(mod_num %in% c(5, 6, 7, 8)), aes(bias_mn, Depth_fct, col = mod_num)) +
  geom_vline(aes(xintercept = 0), lty = 2, col = "grey30") +
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = bias_mn - bias_sd,
                     xmax = bias_mn + bias_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("Bias") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(file = "results/plots/cv_select/dep_bias_5to8.png", height = 20, width = 16, units = 'cm', dpi = 600)

ggplot(depth_info_reg|> filter(mod_num %in% c(5, 6, 7, 8)), aes(per_bias_mn, Depth_fct, col = mod_num)) +
  geom_vline(aes(xintercept = 0), lty = 2, col = "grey30") +
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = per_bias_mn - per_bias_sd,
                     xmax = per_bias_mn + per_bias_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("Percent bias") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggplot(depth_info_reg|> filter(mod_num %in% c(5, 6, 7, 8)), aes(mae_mn, Depth_fct, col = mod_num)) + 
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = mae_mn - mae_sd,
                     xmax = mae_mn + mae_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("MAE") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(file = "results/plots/cv_select/dep_mae_5to8.png", height = 20, width = 16, units = 'cm', dpi = 600)

ggplot(depth_info_reg|> filter(mod_num %in% c(5, 6, 7, 8)), aes(rmse_mn, Depth_fct, col = mod_num)) + 
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = rmse_mn - rmse_sd,
                     xmax = rmse_mn + rmse_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("RMSE") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(file = "results/plots/cv_select/dep_rmse_5to8.png", height = 20, width = 16, units = 'cm', dpi = 600)

ggplot(depth_info_reg|> filter(mod_num %in% c(5, 6, 7, 8)), aes(mape_mn, Depth_fct, col = mod_num)) + 
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = mape_mn - mape_sd,
                     xmax = mape_mn + mape_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("MAPE") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggplot(depth_info_reg|> filter(mod_num %in% c(5, 6, 7, 8)), aes(cor_mn, Depth_fct, col = mod_num)) +
  geom_vline(aes(xintercept = 0), lty = 2, col = "grey30") +
  geom_vline(aes(xintercept = 1), lty = 2, col = "grey30") +
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = cor_mn - cor_sd,
                     xmax = cor_mn + cor_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("Pearson's correlation") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(file = "results/plots/cv_select/dep_corr_5to8.png", height = 20, width = 16, units = 'cm', dpi = 600)

ggplot(depth_info_reg|> filter(mod_num %in% c(5, 6, 7, 8)), aes(bias_mn, Depth_fct, col = mod_num)) +
  geom_vline(aes(xintercept = 0), lty = 2, col = "grey30") +
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = bias_mn - bias_sd,
                     xmax = bias_mn + bias_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("Bias") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(file = "results/plots/cv_select/dep_bias_5to8.png", height = 20, width = 16, units = 'cm', dpi = 600)

ggplot(depth_info_reg|> filter(mod_num %in% c(5, 6, 7, 8)), aes(per_bias_mn, Depth_fct, col = mod_num)) +
  geom_vline(aes(xintercept = 0), lty = 2, col = "grey30") +
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = per_bias_mn - per_bias_sd,
                     xmax = per_bias_mn + per_bias_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("Percent bias") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggplot(depth_info_reg|> filter(mod_num %in% c(5, 6, 7, 8)), aes(mae_mn, Depth_fct, col = mod_num)) + 
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = mae_mn - mae_sd,
                     xmax = mae_mn + mae_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("MAE") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(file = "results/plots/cv_select/dep_mae_5to8.png", height = 20, width = 16, units = 'cm', dpi = 600)

ggplot(depth_info_reg|> filter(mod_num %in% c(5, 6, 7, 8)), aes(rmse_mn, Depth_fct, col = mod_num)) + 
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = rmse_mn - rmse_sd,
                     xmax = rmse_mn + rmse_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("RMSE") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(file = "results/plots/cv_select/dep_rmse_5to8.png", height = 20, width = 16, units = 'cm', dpi = 600)

ggplot(depth_info_reg|> filter(mod_num %in% c(5, 6, 7, 8)), aes(mape_mn, Depth_fct, col = mod_num)) + 
  geom_point(position = position_dodge(width = 0.75)) +
  facet_grid2(area~mod_reg, scales = "free_y") + 
  geom_errorbar(aes(y = Depth_fct, xmin = mape_mn - mape_sd,
                     xmax = mape_mn + mape_sd, col = mod_num), width = 0.25,
                 position = position_dodge(width = 0.75)) +
  xlab("MAPE") +
  ylab("Depth level (m)") +
  scale_y_discrete(limits=rev) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
