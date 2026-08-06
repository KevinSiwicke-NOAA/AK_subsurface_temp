gak1_wc <- readRDS(file = "data/all_gak1.rds") |> 
  filter(depth < 225) |> 
  group_by(date) |> 
  summarize(obs_temp = mean(temperature)) |> 
  ungroup() |> 
  mutate(Year = as.integer(year(date)), Year_fct = as_factor(Year), Depth = depth, 
         Depth_fct = factor(ifelse(depth < 600, depth, '600+'),
                            levels = c('20', '25', '30', '35', '40', '45', '50', '60',
                                       '70', '80', '90', '100', '125', '150', '200',
                                       '250', '300', '350', '400', '500', '600+')),
         Depth_fct2 = as_factor(ifelse(depth > 200, "deep", "shallow")),
         DOY = yday(date), source = 'GAK1', latitude = 59.845, longitude = -149.4667, 
         Month = month(date), bot_dep = 264, esr = 'GOA', mod_reg = 'GOA', stn_id = 'gak1'
         
                          HYCOM_temp = water_temp, 
                  T_diff = water_temp - obs_temp) %>%
         #   select(Year, Year_fct, source, latitude, longitude, Depth, Depth_fct, Depth_fct2, DOY, Month, bot_dep, T_diff, obs_temp, HYCOM_temp, esr, mod_reg, stn_id) %>% 
         #   distinct()
  

gak1_bot <- readRDS(file = "data/all_gak1.rds") |> 
  filter(depth > 225) |> 
  group_by(date) |> 
  summarize(obs_temp = mean(temperature))