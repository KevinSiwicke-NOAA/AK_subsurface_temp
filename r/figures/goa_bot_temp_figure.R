library(doParallel)
library(dplyr)
library(terra)
library(tidyterra)
library(sf)
library(ggplot2)
library(forcats)
library(lubridate)
library(mgcv)
library(cowplot)

################
# Start here with daily means to save time
################
daily_mean <- readRDS(file = "results/mhw_daily_mean_250.rds") |> 
  rename(BCM = gam_temp, HYCOM = hycom_temp) |> 
  pivot_longer(cols = c('HYCOM', 'BCM'), names_to = c('source'), values_to = "temp")
daily_mean_anom <- readRDS(file = "results/mhw_daily_mean_anom_250.rds") |> 
  rename(BCM = gam_temp, HYCOM = hycom_temp) |> 
  pivot_longer(cols = c('HYCOM', 'BCM'), names_to = c('source'), values_to = "temp")

# Daily means no correction related to depth
mn <- ggplot(daily_mean) + 
  geom_line(aes(t, temp, group = source, col = source)) +
  scale_color_manual(values = c("firebrick", 'steelblue')) +
  xlab("Date") +
  ylab(expression('Bottom temperature '~(degree*C))) +
  facet_wrap(~esr) +
  scale_x_continuous(breaks = c(as_date("1995-01-01"), as_date("2000-01-01"), as_date("2005-01-01"), as_date("2010-01-01"), as_date("2015-01-01"), as_date("2020-01-01")),
                     expand = c(0,0)) +
  theme_bw() +
  guides(col = guide_legend(nrow = 1)) +
  theme(panel.grid = element_blank(), legend.position = 'inside',
        legend.position.inside = c(0.75, 0.1), legend.title=element_blank(),
        axis.title.x=element_blank(), axis.text.x=element_blank(), strip.background = element_blank())

# Anomalies scaled at each point to account for depth difference
anom <- ggplot(daily_mean_anom) + 
  geom_line(aes(t, temp, group = source, col = source)) +
  scale_color_manual(values = c("firebrick", 'steelblue')) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  xlab("Date") +
  ylab(expression('Bottom temperature anomaly '~(degree*C))) +
  facet_wrap(~esr) +
  scale_x_continuous(breaks = c(as_date("1995-01-01"), as_date("2000-01-01"), as_date("2005-01-01"), as_date("2010-01-01"), as_date("2015-01-01"), as_date("2020-01-01")),
                     labels = c("1/1/95", "1/1/00", "1/1/05", "1/1/10", "1/1/15", "1/1/20"),
                     expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "none",
        strip.background = element_blank(), strip.text.x = element_blank())

plot_grid(mn, anom, align = 'v', ncol = 1)

ggsave(paste0("results/plots/mhw_all_figure.png"), bg = "white", width = 28, height = 18, units = 'cm', dpi = 300)

# Look at a few years with highlighting of time with and without data
mn <- ggplot(daily_mean |> filter(year > 2011, year < 2018)) + 
  geom_rect(aes(xmin=as_date("2012-05-01"), xmax=as_date("2012-09-30"), ymin=2.1, ymax=8.7), alpha=.1, fill = 'grey70') +
  geom_rect(aes(xmin=as_date("2013-05-01"), xmax=as_date("2013-09-30"), ymin=2.1, ymax=8.7), alpha=.1, fill = 'grey70') +
  geom_rect(aes(xmin=as_date("2014-05-01"), xmax=as_date("2014-09-30"), ymin=2.1, ymax=8.7), alpha=.1, fill = 'grey70') +
  geom_rect(aes(xmin=as_date("2015-05-01"), xmax=as_date("2015-09-30"), ymin=2.1, ymax=8.7), alpha=.1, fill = 'grey70') +
  geom_rect(aes(xmin=as_date("2016-05-01"), xmax=as_date("2016-09-30"), ymin=2.1, ymax=8.7), alpha=.1, fill = 'grey70') +
  geom_rect(aes(xmin=as_date("2017-05-01"), xmax=as_date("2017-09-30"), ymin=2.1, ymax=8.7), alpha=.1, fill = 'grey70') +
  scale_y_continuous(limits = c(2.1, 8.7), expand = c(0,0))+
  scale_x_continuous(breaks = c(as_date("2012-01-01"), as_date("2013-01-01"), as_date("2014-01-01"), as_date("2015-01-01"), as_date("2016-01-01"), as_date("2017-01-01"), as_date("2018-01-01")),
                     expand = c(0,0)) +
  geom_line(aes(t, temp, group = source, col = source)) +
  scale_color_manual(values = c(c("firebrick", 'steelblue'))) +
  # labs(col = "") +
  xlab("Date") +
  ylab(expression('Bottom temperature '~(degree*C))) +
  facet_wrap(~esr) +
  theme_bw() +
  guides(col = guide_legend(nrow = 1)) +
  theme(panel.grid = element_blank(), legend.position = 'inside',
        legend.position.inside = c(0.25, 0.1), legend.title=element_blank())

# Anomalies scaled at each point to account for depth difference
anom <- ggplot(daily_mean_anom |> filter(year > 2011, year < 2018)) + 
  geom_rect(aes(xmin=as_date("2012-05-01"), xmax=as_date("2012-09-30"), ymin= -2.5, ymax=2.5), alpha=.1, fill = 'grey70') +
  geom_rect(aes(xmin=as_date("2013-05-01"), xmax=as_date("2013-09-30"), ymin= -2.5, ymax=2.5), alpha=.1, fill = 'grey70') +
  geom_rect(aes(xmin=as_date("2014-05-01"), xmax=as_date("2014-09-30"), ymin= -2.5, ymax=2.5), alpha=.1, fill = 'grey70') +
  geom_rect(aes(xmin=as_date("2015-05-01"), xmax=as_date("2015-09-30"), ymin= -2.5, ymax=2.5), alpha=.1, fill = 'grey70') +
  geom_rect(aes(xmin=as_date("2016-05-01"), xmax=as_date("2016-09-30"), ymin= -2.5, ymax=2.5), alpha=.1, fill = 'grey70') +
  geom_rect(aes(xmin=as_date("2017-05-01"), xmax=as_date("2017-09-30"), ymin= -2.5, ymax=2.5), alpha=.1, fill = 'grey70') +
  scale_y_continuous(limits = c(-2.5, 2.5), expand = c(0,0)) +
  scale_x_continuous(breaks = c(as_date("2012-01-01"), as_date("2013-01-01"), as_date("2014-01-01"), as_date("2015-01-01"), as_date("2016-01-01"), as_date("2017-01-01"), as_date("2018-01-01")),
                     labels = c("1/1/12", "1/1/13", "1/1/14", "1/1/15", "1/1/16", "1/1/17", "1/1/18"),
                     expand = c(0,0)) +
  geom_line(aes(t, temp, group = source, col = source)) +
  scale_color_manual(values = c(c("firebrick", 'steelblue'))) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  xlab("Date") +
  ylab(expression('Bottom temperature anomaly '~(degree*C))) +
  facet_wrap(~esr) +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = 'none')

plot_grid(mn + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), strip.background = element_blank()), 
          anom + theme(strip.background = element_blank(), strip.text.x = element_blank()),
          align = 'v', ncol = 1)

ggsave("results/plots/mhw_ex_yrs_figure.png", bg = "white", width = 16, height = 16, units = 'cm', dpi = 300)

ann_summ <- daily_mean |> 
  group_by(year,esr, source) |> 
  summarize(min_T = min(temp),
            max_T = max(temp)) |> 
  pivot_wider(names_from = source, values_from = c(min_T, max_T)) |> 
  mutate(delta_min = min_T_HYCOM - min_T_BCM,
         delta_max = max_T_HYCOM - max_T_BCM)

summary(ann_summ |> filter(esr == "EGOA"))
summary(ann_summ |> filter(esr == "WGOA"))
