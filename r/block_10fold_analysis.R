pkgs <- c("mgcv", "dplyr", "forcats", "tidyr", "lubridate", "Metrics", "readr", "sf") 

## install.packages(pkgs, Ncpus = 4)
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE,
       quietly = TRUE)

# This script loads the data, and spatially blocks it before analyses
source("r/block_CV.r")

# Setup model processing
# First create a list of formulas to run in the model
formulas <- list(
  T_diff2 ~ te(HYCOM_temp, Depth, k = 20, m = 1),
  T_diff2 ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + te(longitude, latitude, by = Depth_fct2, m = 1, k = 20),
  T_diff2 ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + s(Year_fct, by = Depth_fct2, bs = "re"),
  T_diff2 ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + s(DOY, by = Depth_fct2, bs = 'cc', k = 12),
  T_diff2 ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + s(Year_fct, by = Depth_fct2, bs = "re") + te(longitude, latitude, by = Depth_fct2, m = 1, k = 20),
  T_diff2 ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + te(longitude, latitude, by = Depth_fct2, m = 1, k = 20) + s(DOY, by = Depth_fct2, bs = 'cc', k = 12),
  T_diff2 ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + s(Year_fct, by = Depth_fct2, bs = "re") + s(DOY, by = Depth_fct2, bs = 'cc', k = 12),
  T_diff2 ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + s(Year_fct, by = Depth_fct2, bs = "re") + te(longitude, latitude, by = Depth_fct2, m = 1, k = 20) + s(DOY, by = Depth_fct2, bs = 'cc', k = 12)
)

info_reg_ls <- vector("list", nrow(reg))
depth_info_reg_ls <- vector("list", nrow(reg))

for( i in 1:nrow(reg) ) {
  info_grp_ls <- vector("list", 10)
  depth_info_grp_ls <- vector("list", 10)
  
  dat = b_dat_reg[[i]]
  for( j in 1:10 ) {
    train_data <- dat %>% dplyr::filter(!grp == j)
    test_data <- dat %>% dplyr::filter(grp == j)
    
    k_yr = length(unique(train_data$Year))
    mod_ls <- lapply(formulas, function(x) bam(x, data = train_data, method = 'fREML', 
                                               nthreads = 10, discrete = TRUE, knots = list(DOY = c(0.5, 366.5))))
    
    info_mod_ls <- vector("list", length(mod_ls) + 1)
    depth_info_mod_ls <- vector("list", length(mod_ls) + 1)
    
    for( k in c(0, seq_along(mod_ls)) ) { 
      if( k == 0 ) {
        test_data$pred_temp = test_data$HYCOM_temp
        dev_expl <- NA
      }
      if( !k == 0 ) {
        mod <- mod_ls[[k]]
        dev_expl <- ((mod$null.deviance - mod$deviance)/mod$null.deviance) * 100
        test_data$pred_diff = predict(mod, newdata = test_data)
        test_data$pred_temp = test_data$HYCOM_temp + test_data$pred_diff
      }   
      
      info_mod_ls[[k + 1]] <- test_data %>%
        mutate(mod_num = factor(k)) %>% 
        group_by(mod_reg, grp, mod_num) %>% 
        summarize(cor = cor(obs_temp, pred_temp),
                  bias = bias(obs_temp, pred_temp),
                  per_bias = percent_bias(obs_temp + 3, pred_temp + 3),
                  rmse = rmse(obs_temp, pred_temp),
                  mae = mae(obs_temp, pred_temp),
                  mape = mape(obs_temp + 3, pred_temp + 3)) %>% 
        mutate(dev_expl = dev_expl)
      
      depth_info_mod_ls[[k + 1]] <- test_data %>%
        mutate(mod_num = factor(k)) %>% 
        group_by(mod_reg, grp, mod_num, Depth_fct) %>% 
        summarize(cor = cor(obs_temp, pred_temp),
                  bias = bias(obs_temp, pred_temp),
                  per_bias = percent_bias(obs_temp + 3, pred_temp + 3),
                  rmse = rmse(obs_temp, pred_temp),
                  mae = mae(obs_temp, pred_temp),
                  mape = mape(obs_temp + 3, pred_temp + 3)) 
    }    
    info_grp_ls[[j]] <- as.data.frame(dplyr::bind_rows(info_mod_ls))
    depth_info_grp_ls[[j]] <- as.data.frame(dplyr::bind_rows(depth_info_mod_ls)) 
  }
  
  info_grp_df <- as.data.frame(dplyr::bind_rows(info_grp_ls))
  info_reg_ls[[i]] <- info_grp_df %>%
    group_by(mod_reg, mod_num) %>% 
    summarize(cor_mn = mean(cor, na.rm = T), cor_sd = sd(cor, na.rm = T),
              bias_mn = mean(bias), bias_sd = sd(bias),
              per_bias_mn = mean(per_bias), per_bias_sd = sd(per_bias),
              rmse_mn = mean(rmse), rmse_sd = sd(rmse),
              mae_mn = mean(mae), mae_sd = sd(mae),
              mape_mn = mean(mape), mape_sd = sd(mape),
              dev_expl_mn = mean(dev_expl), dev_expl_sd = sd(dev_expl))
  
  depth_info_grp_df <- as.data.frame(dplyr::bind_rows(depth_info_grp_ls)) 
  depth_info_reg_ls[[i]] <- depth_info_grp_df  %>% 
    group_by(mod_reg, mod_num, Depth_fct) %>% 
    summarize(cor_mn = mean(cor, na.rm = T), cor_sd = sd(cor, na.rm = T), 
              bias_mn = mean(bias), bias_sd = sd(bias),
              per_bias_mn = mean(per_bias), per_bias_sd = sd(per_bias),
              rmse_mn = mean(rmse), rmse_sd = sd(rmse),
              mae_mn = mean(mae), mae_sd = sd(mae),
              mape_mn = mean(mape), mape_sd = sd(mape))
}

bot_info_reg_df <- as.data.frame(dplyr::bind_rows(info_reg_ls)) %>% 
  write_csv(file = "results/bot_info_region_block.csv")
bot_depth_info_reg_df <- as.data.frame(dplyr::bind_rows(depth_info_reg_ls)) %>% 
  write_csv(file = "results/bot_depth_info_region_block.csv")

saveRDS(bot_info_reg_df, "results/bot_info_reg_df_block.rds")
saveRDS(bot_depth_info_reg_df, "results/bot_depth_info_reg_df_block.rds")

# Setup model processing for water column
# First create a list of formulas to run in the model
formulas <- list(
  T_diff ~ te(HYCOM_temp, Depth, k = 20, m = 1),
  T_diff ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + te(longitude, latitude, by = Depth_fct2, m = 1, k = 20),
  T_diff ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + s(Year_fct, by = Depth_fct2, bs = "re"),
  T_diff ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + s(DOY, by = Depth_fct2, bs = 'cc', k = 12),
  T_diff ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + s(Year_fct, by = Depth_fct2, bs = "re") + te(longitude, latitude, by = Depth_fct2, m = 1, k = 20),
  T_diff ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + te(longitude, latitude, by = Depth_fct2, m = 1, k = 20) + s(DOY, by = Depth_fct2, bs = 'cc', k = 12),
  T_diff ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + s(Year_fct, by = Depth_fct2, bs = "re") + s(DOY, by = Depth_fct2, bs = 'cc', k = 12),
  T_diff ~ Depth_fct2 + te(HYCOM_temp, Depth, k = 20, m = 1) + s(Year_fct, by = Depth_fct2, bs = "re") + te(longitude, latitude, by = Depth_fct2, m = 1, k = 20) + s(DOY, by = Depth_fct2, bs = 'cc', k = 12)
)

info_reg_ls <- vector("list", nrow(reg))
depth_info_reg_ls <- vector("list", nrow(reg))

for( i in 1:nrow(reg) ) {
  info_grp_ls <- vector("list", 10)
  depth_info_grp_ls <- vector("list", 10)
  
  dat = wc_dat_reg[[i]]
  for( j in 1:10 ) {
    train_data <- dat %>% filter(!grp == j)
    test_data <- dat %>% filter(grp == j)
    
    k_yr = length(unique(train_data$Year))
    mod_ls <- lapply(formulas, function(x) bam(x, data = train_data, method = 'fREML', 
                                               nthreads = 10, discrete = TRUE, knots = list(DOY = c(0.5, 366.5))))
    
    info_mod_ls <- vector("list", length(mod_ls) + 1)
    depth_info_mod_ls <- vector("list", length(mod_ls) + 1)
    
    for( k in c(0, seq_along(mod_ls)) ) { 
      if( k == 0 ) {
        test_data$pred_temp = test_data$HYCOM_temp
        dev_expl <- NA
      }
      if( !k == 0 ) {
        mod <- mod_ls[[k]]
        dev_expl <- ((mod$null.deviance - mod$deviance)/mod$null.deviance) * 100
        test_data$pred_diff = predict(mod, newdata = test_data)
        test_data$pred_temp = test_data$HYCOM_temp + test_data$pred_diff
      }   
      
      info_mod_ls[[k + 1]] <- test_data %>%
        mutate(mod_num = factor(k)) %>% 
        group_by(mod_reg, grp, mod_num) %>% 
        summarize(cor = cor(obs_temp, pred_temp),
                  bias = bias(obs_temp, pred_temp),
                  per_bias = percent_bias(obs_temp + 3, pred_temp + 3),
                  rmse = rmse(obs_temp, pred_temp),
                  mae = mae(obs_temp, pred_temp),
                  mape = mape(obs_temp + 3, pred_temp + 3)) %>% 
        mutate(dev_expl = dev_expl)
      
      depth_info_mod_ls[[k + 1]] <- test_data %>%
        mutate(mod_num = factor(k)) %>% 
        group_by(mod_reg, grp, mod_num, Depth_fct) %>% 
        summarize(cor = cor(obs_temp, pred_temp),
                  bias = bias(obs_temp, pred_temp),
                  per_bias = percent_bias(obs_temp + 3, pred_temp + 3),
                  rmse = rmse(obs_temp, pred_temp),
                  mae = mae(obs_temp, pred_temp),
                  mape = mape(obs_temp + 3, pred_temp + 3)) 
    }    
    info_grp_ls[[j]] <- as.data.frame(dplyr::bind_rows(info_mod_ls))
    depth_info_grp_ls[[j]] <- as.data.frame(dplyr::bind_rows(depth_info_mod_ls)) 
  }
  
  info_grp_df <- as.data.frame(dplyr::bind_rows(info_grp_ls))
  info_reg_ls[[i]] <- info_grp_df %>%
    group_by(mod_reg, mod_num) %>% 
    summarize(cor_mn = mean(cor, na.rm = T), cor_sd = sd(cor, na.rm = T),
              bias_mn = mean(bias), bias_sd = sd(bias),
              per_bias_mn = mean(per_bias), per_bias_sd = sd(per_bias),
              rmse_mn = mean(rmse), rmse_sd = sd(rmse),
              mae_mn = mean(mae), mae_sd = sd(mae),
              mape_mn = mean(mape), mape_sd = sd(mape),
              dev_expl_mn = mean(dev_expl), dev_expl_sd = sd(dev_expl))
  
  depth_info_grp_df <- as.data.frame(dplyr::bind_rows(depth_info_grp_ls)) 
  depth_info_reg_ls[[i]] <- depth_info_grp_df  %>% 
    group_by(mod_reg, mod_num, Depth_fct) %>% 
    summarize(cor_mn = mean(cor, na.rm = T), cor_sd = sd(cor, na.rm = T), 
              bias_mn = mean(bias), bias_sd = sd(bias),
              per_bias_mn = mean(per_bias), per_bias_sd = sd(per_bias),
              rmse_mn = mean(rmse), rmse_sd = sd(rmse),
              mae_mn = mean(mae), mae_sd = sd(mae),
              mape_mn = mean(mape), mape_sd = sd(mape))
}

wc_info_reg_df <- as.data.frame(dplyr::bind_rows(info_reg_ls)) %>% 
  write_csv(file = "results/wc_info_region_block.csv")
wc_depth_info_reg_df <- as.data.frame(dplyr::bind_rows(depth_info_reg_ls)) %>% 
  write_csv(file = "results/wc_depth_info_region_block.csv")

saveRDS(wc_info_reg_df, "results/wc_info_reg_df_block.rds")
saveRDS(wc_depth_info_reg_df, "results/wc_depth_info_reg_df_block.rds")
