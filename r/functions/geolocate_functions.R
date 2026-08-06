
########
# Add temperature depth profile data to create another data likelihood
########
hy_cut <- c(0, 1, 3, 5, 7, 9, 11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5,
            55, 65, 75, 85, 95, 112.5, 137.5, 175, 225, 275, 325, 375, 450,
            550, 650, 750, 850, 950, 1125, 1375, 1750, 2250, 2750, 3500, 4500)

hy_lab <- c(0, 2, 4, 6, 8, 10, 12, 15, 20, 25, 30, 35, 40, 45, 50,
            60, 70, 80, 90, 100, 125, 150, 200, 250, 300, 350,
            400, 500, 600, 700, 800, 900, 1000, 1250, 1500, 2000,
            2500, 3000, 4000)

# These errors were derived from Ch.1 comparison of HYCOM vs observations
hy_err <- c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5,
            1.4, 1.3, 1.2, 1.1, 1.1, 1.0, 0.9, 0.8, 0.8, 
            0.7, 0.6, 0.6, 0.6, 0.5, 0.4, 0.3, 0.2, 0.2, 
            0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)

hycom_err = data.frame(depth_bin_tdp = factor(hy_lab), err = hy_err)

ggplot(hycom_err, aes(err, forcats::fct_rev(depth_bin_tdp))) + 
  geom_point() + 
  scale_x_continuous(limits = c(0, 1.75), expand = c(0,0)) + 
  labs(x = 'HYCOM Error', y = 'Depth bin') +
  theme_bw() + theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14))

create_z_lik <- function(data, grid_cell_min, grid_cell_max, grid_cell_id, dist_off_bot) {
  lik <- foreach(k = 1:nrow(data), .packages = c("dplyr")) %dopar% {
    if(!is.na(data$max_z[k])) {
      cutoff_a <- data$max_z[k] - data$accuracy[k]
      cutoff_b <- data$max_z[k] + data$accuracy[k] + dist_off_bot # allows for the fish to be up to 100 m off the bottom
    } else {
      cutoff_a <- max(data$max_z, na.rm = T) - max(data$accuracy , na.rm = T)
      cutoff_b <- max(data$max_z, na.rm = T) + max(data$accuracy , na.rm = T) + dist_off_bot
    }
    lik <- ifelse(cutoff_a > grid_cell_max, 0, # sets cells shallower than fish to 0 likelihood
                  ifelse(cutoff_b < grid_cell_min, 0, 1)) # sets cells deeper than fish plus off bottom to 0, else 1
    daily_lik <- bind_cols(grid_cell_id, lik) |> rename(z_lik = max)
  }
}

create_tdp_lik <- function(data, grid_cell_dat, grid_cell_id, version, hy_err, hy_lab) {
  lik_dates <- unique(data$date)  
  lik <- foreach(k = 1:length(lik_dates), .packages = c("dplyr", "sf", "lubridate")) %dopar% {
    day_dat <- data |> filter(date == lik_dates[k])
    day_bins <- unique(day_dat$depth_bin_tdp)
    
    if(!is.na(day_bins[1])) {
      temp_ref <- grid_cell_dat[[k]]
      day_t_z_lik <- data.frame()
      for(i in 1:length(day_bins)) {
        z <- as.numeric(as.character(day_bins[i]))
        mod_err <- hy_err[which(hy_lab == z)]
        t_z <- day_dat |> filter(depth_bin_tdp == z) |> 
          mutate(sd_t = ifelse(is.na(sd_t), 0, sd_t))
        
        if(version == "HYCOM") {
          day_bin_ref <- temp_ref |> filter(depth == z) |> rename(temp = mean_h, err = sd_h) |> 
            mutate(err = ifelse(is.na(err), 0, err)) |> 
            select(date, cell_id, depth, temp, err, N)
        }
        if(version == "GAM") {
          day_bin_ref <- temp_ref |> filter(depth == z) |> rename(temp = mean_g, err = sd_g) |> 
            mutate(err = ifelse(is.na(err), 0, err)) |> 
            select(date, cell_id, depth, temp, err, N)
        }
        if(version == "blend") {
          day_bin_ref <- temp_ref |> filter(depth == z) |> 
            mutate(temp = ifelse(month(date) < 5 | month(date) > 9, mean_h,
                                 ifelse(month(date) == 5 & mday(date) < 5, ((mday(date)/10 * mean_g) + ((10 - mday(date))/10 * mean_h)),
                                        ifelse(month(date) == 9 & mday(date) > 26, (((mday(date) - 21)/10 * mean_h) + ((31 - mday(date))/10 * mean_g)),
                                               (mean_g + mean_h) / 2))),
                   err = ifelse(month(date) < 5 | month(date) > 9, sd_h,
                                ifelse(month(date) == 5 & mday(date) < 5, ((mday(date)/10 * sd_g) + ((10 - mday(date))/10 * sd_h)),
                                       ifelse(month(date) == 9 & mday(date) > 26, (((mday(date) - 21)/10 * sd_h) + ((31 - mday(date))/10 * sd_g)),
                                              (sd_g + sd_h) / 2)))) |> 
            mutate(err = ifelse(is.na(err), 0, err)) |> 
            select(date, cell_id, depth, temp, err, N)
        }
        # t_z_lik <- pnorm((t_z$mean_t + t_z$sd_t) * rep(1, nrow(day_bin_ref)),
        #                  mean = day_bin_ref$temp, sd = day_bin_ref$err + mod_err) -
        #   pnorm((t_z$mean_t - t_z$sd_t) * rep(1, nrow(day_bin_ref)),
        #         mean = day_bin_ref$temp, sd = day_bin_ref$err + mod_err)
        combined_sd <- sqrt(day_bin_ref$err^2 + mod_err^2 + t_z$sd_t^2)
        t_z_lik <- dnorm(t_z$mean_t, mean = day_bin_ref$temp, sd = combined_sd)
        
        z_bin_lik <- data.frame("l" = t_z_lik) |>
          mutate(cell_id = day_bin_ref$cell_id, 
                 zbin = z) 
        
        day_t_z_lik <- bind_rows(z_bin_lik, day_t_z_lik)
      }
      lik <- day_t_z_lik |>
        group_by(cell_id) |>
        summarize(day_t_lik = prod(l, na.rm = T))
      
      daily_lik <- left_join(grid_cell_id, lik) |> 
        mutate(tdp_lik = ifelse(is.na(day_t_lik), 0, day_t_lik )) |> # / max(lik$day_t_lik)
        select(cell_id, tdp_lik)
      
    } else {
      daily_lik <- grid_cell_id |> 
        mutate(tdp_lik = 1)
    }
  }
}
# Custom function for adding legend in multi-panel figure
# https://stackoverflow.com/questions/52975447/reorganize-sf-multi-plot-and-add-a-legend
add_legend <- 
  function( legend, 
            col = sf.colors(),
            legend_x = c(0.9,  1.0),
            legend_y = c(0.05, 0.45),
            text_col = "black",
            ...){
    
    # Get the axis limits and calculate size
    axisLimits <- par()$usr
    xLength <- axisLimits[2] - axisLimits[1]
    yLength <- axisLimits[4] - axisLimits[3]
    
    xl = (1-legend_x[1])*par('usr')[1] + (legend_x[1])*par('usr')[2]
    xr = (1-legend_x[2])*par('usr')[1] + (legend_x[2])*par('usr')[2]
    yb = (1-legend_y[1])*par('usr')[3] + (legend_y[1])*par('usr')[4]
    yt = (1-legend_y[2])*par('usr')[3] + (legend_y[2])*par('usr')[4]
    if( diff(legend_y) > diff(legend_x) ){
      align = c("lt","rb")[2]
      gradient = c("x","y")[2]
    }else{
      align = c("lt","rb")[1]
      gradient = c("x","y")[1]
    }
    
    # Add the legend
    plotrix::color.legend( xl = xl, 
                           xr = xr,
                           yb = yb, 
                           yt = yt,
                           legend = legend, 
                           rect.col = col,
                           gradient="y",
                           col = text_col, 
                           ... )
  }

max_prob_to_1 <- function(prob) {
  for( day in 2:(ncol(prob)-1)) {
    prob[,day] <- ifelse(prob[,day] == max(prob[,day], na.rm = TRUE), 1, 0)
  }
  rownames(prob) <- row(prob)[,1]
  return(prob)
}

create_norm_lt_lik <- function(data, grid_cell_val, min_sd, grid_cell_id) {
  lik <- foreach(k = 2:(nrow(data) - 1), .packages = c("dplyr")) %dopar% {
    day_dat <- data |> filter(date == date[k])
    if(!is.na(data$value[k])) {
      cell_sd <- ifelse(data$err[k] < min_sd, min_sd, data$err[k])
      lik <- dnorm(data$value[k] * rep(1, length(grid_cell_val)),
                   mean = grid_cell_val, sd = rep(cell_sd, length(grid_cell_val)))
      daily_lik <- grid_cell_id |> 
        mutate(l_lik = lik )
    } else {
      daily_lik <- grid_cell_id |> 
        mutate(l_lik = 1)
    }
  }
}

crop_bat_rast <- function(bathy_terra) {
  # Identify contiguous groups, and select only the largest to remove odd pockets inland or anomalies like seamounts when these are beyond the inputs
  bat_patch <- patches(bathy_terra, directions=4)  # 4 directions for connectivity, 'rook'
  # Convert the clumped raster to polygons
  bat_polys <- as.polygons(bat_patch, dissolve=TRUE)
  # Calculate the area of each polygon
  bat_polys$area <- expanse(bat_polys, unit="m")
  # Find the polygon with the largest area
  bat_poly <- bat_polys[which.max(bat_polys$area), ]
  # Crop the data to the largest polygon
  new_bat <- terra::crop(bathy_terra, bat_poly, mask = T)
}

matexp <- function( Mrate,
                    log2steps = 0, # Number of Euler-approximation steps
                    zap_small = FALSE ){
  require(Matrix)
  require(expm)
  if( (log2steps <=0 ) || (log2steps > 100) ){
    # Full version ... note that expm::expm is faster that Matrix::expm
    out = expm(Mrate)
    return( Matrix(out) )
  }else{
    # Euler approximation
    Mrate = Diagonal(nrow(Mrate)) + Mrate / (2^log2steps)
    for(stepI in seq(1,log2steps,length=log2steps)){
      Mrate = Mrate %*% Mrate
    }
    if(zap_small) Mrate = zapsmall(Mrate)
    return( Mrate )
  }
}

# Function to get coefficients for TMB model
get_coef.custom_tmb = function(model, param, ...){
  out = model$parhat[[param]]
  names(out) = rep(param, length(out))
  return(out)
}

# Function to get variance-covariance for TMB model
get_vcov.custom_tmb = function(model, param, ...){
  rows = which( names(model$opt$par) == param )
  array( model$opt$SD$cov.fixed[rows,rows],
         dim = rep(length(rows),2),
         dimnames = list(rep(param,length(rows)),rep(param,length(rows))) )
}

# Function to change coefficients for TMB model
set_coef.custom_tmb = function(model, newpar, param, ...){
  model$parhat[[param]] <- newpar
  return(model)
}

# Function to get predictions when changing coefficients
get_predict.custom_tmb = function(model, newdata, param, center=FALSE, ...){
  # build original model.frame
  frame0 = model.frame( formula=model$formula, data=model$data )
  terms0 = terms( frame0 )
  xlevels = .getXlevels( terms0, frame0 )
  # get new design matrix
  terms1 = delete.response( terms0 )
  frame1 = model.frame( terms1, newdata, xlev=xlevels )
  X_ik = model.matrix( terms1, frame1 )
  gamma_k = get_coef.custom_tmb(model, param)
  # Calculate linear predictor and format output
  yhat_i = X_ik %*% gamma_k
  if(center==TRUE) yhat_i = yhat_i - mean(yhat_i)
  out = data.frame( rowid=seq_along(yhat_i[,1]), estimate=yhat_i )
  return(out)
}

get_day_tdp <- function(day) {
  y <- year(day)
  tdp_y <- readRDS(file = glue("data/tdp_data/tdp_{y}_50k.rds"))
  tdp_dates <- seq(tdp_y[[1]]$date[1], tdp_y[[length(tdp_y)]]$date[1], by="day")
  temps <- tdp_y[which(tdp_dates == day)]
}

# Change marginaleffects options to define `custom_tmb` class
options("marginaleffects_model_classes" = "custom_tmb")

# get_tag_summary <- function(num, info) {
#   tag_info <- info |> filter(tag_num == num)
#   rel_date <- as.Date(tag_info$rel_date, "%m/%d/%Y") 
#   rel_long <- tag_info$rel_lon
#   rel_lat <- tag_info$rel_lat
#   rec_date <- as.Date(tag_info$rec_date, "%m/%d/%Y") 
#   rec_long <- tag_info$rec_lon
#   rec_lat <- tag_info$rec_lat
#   
#   results <- list(
#     summ_dat <- data.frame(num, rel_date, rel_long, rel_lat, rec_date, rec_long, rec_lat),
#     dates <- seq(rel_date, rec_date, by="day")
#   )
# }

# Query parquet dataset with arrow
query_parquet_dataset <- function(out_dir, start_date, end_date,
                                  min_depth = -Inf, max_depth = Inf) {
  # Requires: arrow, dplyr, data.table
  if (!requireNamespace("arrow", quietly = TRUE)) stop("install arrow")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("install dplyr")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("install data.table")
  
  library(arrow)
  library(dplyr)
  library(data.table)
  
  ds <- open_dataset(out_dir)
  
  start <- as.Date(start_date)
  end   <- as.Date(end_date)
  
  # Try to filter directly. If it fails because `date` is not an atomic/date type,
  # catch the error, cast `date` to Date, and try again.
  try_direct <- try({
    tbl <- ds %>%
      filter(date >= start,
             date <= end,
             depth >= min_depth,
             depth <= max_depth) %>%
      collect()
    setDT(tbl)
    tbl
  }, silent = TRUE)
  
  if (!inherits(try_direct, "try-error")) return(try_direct)
  
  # If we get here, the direct filter failed (likely because date is string/other).
  # Cast date to Date and retry. This is lazy in Arrow/dplyr and efficient.
  tbl2 <- ds %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= start,
           date <= end,
           depth >= min_depth,
           depth <= max_depth) %>%
    collect()
  
  setDT(tbl2)
  tbl2
}

calc_edge_length <- function(sf_obj, index_matrix) {
  # index_matrix should be a 2-column matrix where each row contains
  # the indices of two adjacent polygons that share an edge
  
  # Create a vector to store edge lengths
  edge_lengths <- numeric(nrow(index_matrix))
  
  # Loop through each pair of adjacent polygons
  for (i in seq_len(nrow(index_matrix))) {
    # Get the indices of the two polygons
    poly1_idx <- index_matrix[i, 1] 
    poly2_idx <- index_matrix[i, 2] 
    
    # Extract geometries
    poly1 <- sf_obj[poly1_idx, ]
    poly2 <- sf_obj[poly2_idx, ]
    
    # Find the intersection of the two polygons
    # The intersection should be a line (the shared edge)
    shared_edge <- sf::st_intersection(poly1, poly2)
    
    # Calculate the length of the shared edge
    edge_lengths[i] <- sf::st_length(shared_edge) / 1000
  }
  return(edge_lengths)
}

calc_cent_length <- function(sf_obj, index_matrix) {
  # index_matrix should be a 2-column matrix where each row contains
  # the indices of two adjacent polygons that share an edge
  
  # Create a vector to store edge lengths
  cent_lengths <- numeric(nrow(index_matrix))
  
  # Loop through each pair of adjacent polygons
  for (i in seq_len(nrow(index_matrix))) {
    # Get the indices of the two polygons
    poly1_idx <- index_matrix[i, 1] 
    poly2_idx <- index_matrix[i, 2] 
    
    # Extract geometries
    poly1 <- sf_obj[poly1_idx, ]
    poly2 <- sf_obj[poly2_idx, ]
    
    # calculate the distance between the centroids
    cent_lengths[i] <- sf::st_distance(poly1, poly2) / 1000
  }
  return(cent_lengths)
}

create_tdp_lik_renorm <- function(data, grid_cell_dat, grid_cell_id, version, hy_err, hy_lab) {
  lik_dates <- unique(data$date)  
  lik <- foreach(k = 1:length(lik_dates), .packages = c("dplyr", "sf", "lubridate")) %dopar% {
    day_dat <- data |> filter(date == lik_dates[k])
    day_bins <- unique(day_dat$depth_bin_tdp)
    
    if(!is.na(day_bins[1])) {
      temp_ref <- grid_cell_dat[[k]]
      day_t_z_lik <- data.frame()
      for(i in 1:length(day_bins)) {
        z <- as.numeric(as.character(day_bins[i]))
        mod_err <- hy_err[which(hy_lab == z)]
        t_z <- day_dat |> filter(depth_bin_tdp == z) |> 
          mutate(sd_t = ifelse(is.na(sd_t), 0.1, 
                               ifelse(sd_t == 0, 0.1, sd_t)))
        if(version == "HYCOM") {
          day_bin_ref <- temp_ref |> filter(depth == z) |> rename(temp = mean_h, err = sd_h) |> 
            mutate(err = ifelse(is.na(err), 0.1, err)) |> 
            select(date, cell_id, depth, temp, err, N)
        }
        if(version == "GAM") {
          day_bin_ref <- temp_ref |> filter(depth == z) |> rename(temp = mean_g, err = sd_g) |> 
            mutate(err = ifelse(is.na(err), 0.1, err)) |> 
            select(date, cell_id, depth, temp, err, N)
        }
        if(version == "blend") {
          day_bin_ref <- temp_ref |> filter(depth == z) |> 
            mutate(temp = ifelse(month(date) < 5 | month(date) > 9, mean_h,
                                 ifelse(month(date) == 5 & mday(date) < 5, ((mday(date)/10 * mean_g) + ((10 - mday(date))/10 * mean_h)),
                                        ifelse(month(date) == 9 & mday(date) > 26, (((mday(date) - 21)/10 * mean_h) + ((31 - mday(date))/10 * mean_g)),
                                               (mean_g + mean_h) / 2))),
                   err = ifelse(month(date) < 5 | month(date) > 9, sd_h,
                                ifelse(month(date) == 5 & mday(date) < 5, ((mday(date)/10 * sd_g) + ((10 - mday(date))/10 * sd_h)),
                                       ifelse(month(date) == 9 & mday(date) > 26, (((mday(date) - 21)/10 * sd_h) + ((31 - mday(date))/10 * sd_g)),
                                              (sd_g + sd_h) / 2)))) |> 
            mutate(err = ifelse(is.na(err), 0.1, err)) |> 
            select(date, cell_id, depth, temp, err, N)
        }
        t_z_lik <- pnorm((t_z$mean_t + t_z$sd_t) * rep(1, nrow(day_bin_ref)),
                         mean = day_bin_ref$temp, sd = day_bin_ref$err + mod_err) -
          pnorm((t_z$mean_t - t_z$sd_t) * rep(1, nrow(day_bin_ref)),
                mean = day_bin_ref$temp, sd = day_bin_ref$err + mod_err)
        t_z_lik <- t_z_lik / max(t_z_lik)
        
        z_bin_lik <- data.frame("l" = t_z_lik) |>
          mutate(cell_id = day_bin_ref$cell_id, 
                 zbin = z) 
        
        day_t_z_lik <- bind_rows(z_bin_lik, day_t_z_lik)
      }
      lik <- day_t_z_lik |>
        group_by(cell_id) |>
        summarize(day_t_lik = prod(l, na.rm = T))
      
      daily_lik <- left_join(grid_cell_id, lik) |> 
        mutate(tdp_lik = ifelse(is.na(day_t_lik), 0, day_t_lik),
               tdp_lik = tdp_lik / max(tdp_lik)) |> # / max(lik$day_t_lik)
        select(cell_id, tdp_lik)
      
    } else {
      daily_lik <- grid_cell_id |> 
        mutate(tdp_lik = 1)
    }
  }
}
