make_bathy_rast <- function(reference, xmin = -150, xmax = -155, ymin = 50, ymax = 55, out_res = 50000) {
  # create a raster given the bounding coordinates input, in the same CRS as the reference
  bat_ext <- terra::rast(ext = c(xmin, xmax, ymin, ymax), crs = "EPSG:4326") |>
    terra::project(terra::crs(reference))
  # and now set the resolution
  bathy <- terra::rast(ext(bat_ext), out_res, crs = terra::crs(bat_ext))
  
  crp_ref <- terra::crop(reference, bathy) # crop the reference to the desired inputs
  fact = round(out_res / terra::res(crp_ref)[1]) # round for instances where this is not an integer
  gc()
  
  # add info related to the reference bathymetry data at the desired output resolution
  bathy$N <- terra::aggregate(crp_ref, fact = fact, fun = function(x) {length(x[!is.na(x)])})
  bathy[values(bathy$N) < ((out_res / res(reference)[1]) ^ 2) / 5] <- NA  # Set some values to NA
  
  return(bathy)
}

# Remove mask from grid and return unique grid cells
remove_mask_from_grid <- function(grid_sf, mask_sf) {
  stopifnot(inherits(grid_sf, "sf"), inherits(mask_sf, "sf"))
  
  # Ensure projected CRS (area units meaningful)
  if (sf::st_is_longlat(grid_sf)) {
    stop("grid_sf is in lon/lat. Reproject grid_sf to a projected CRS before running.")
  }
  
  # Validity & CRS alignment
  grid_sf <- st_make_valid(grid_sf)
  mask_sf <- st_make_valid(mask_sf)
  
  if (st_crs(grid_sf) != st_crs(mask_sf)) {
    mask_sf <- st_transform(mask_sf, st_crs(grid_sf))
  }
  
  # Crop mask to grid bbox to avoid unnecessary geometry ops
  grid_bbox <- st_as_sfc(st_bbox(grid_sf))
  st_crs(grid_bbox) <- st_crs(grid_sf)
  mask_clip <- st_intersection(mask_sf, grid_bbox)
  if (nrow(mask_clip) == 0 || all(st_is_empty(mask_clip))) {
    out <- grid_sf
    out$part <- 1
    out$new_id <- sprintf("%s_%d", "g", seq_len(nrow(out)))
    out$area <- as.numeric(st_area(out))
    return(out)
  }
  
  # union & simplify mask for speed
  mask_clip <- st_union(mask_clip)
  mask_clip <- st_simplify(mask_clip)
  mask_clip <- st_make_valid(mask_clip)
  
  # Candidate selection: only grid cells that touch mask_clip
  ints <- st_intersects(grid_sf, mask_clip, sparse = TRUE)
  candidate_idx <- which(lengths(ints) > 0)
  noncandidate_idx <- setdiff(seq_len(nrow(grid_sf)), candidate_idx)
  message("Candidates: ", length(candidate_idx), " / ", nrow(grid_sf))
  
  # Subtract mask for each candidate cell
  process_cell <- function(i) {
    cell_row <- grid_sf[i, , drop = FALSE]
    # keep attributes of original cell
    res_geom <- tryCatch(st_difference(st_geometry(cell_row), mask_clip), error = function(e) {
      warning("Difference failed for cell ", i, ": ", conditionMessage(e))
      return(st_geometry(cell_row))
    })
    if (is.null(res_geom) || length(res_geom) == 0 || all(st_is_empty(res_geom))) return(NULL)
    
    # extract polygon pieces (if any)
    pieces <- st_collection_extract(res_geom, "POLYGON")
    if (length(pieces) == 0) return(NULL)
    
    # build sf: replicate attributes for each piece
    n_pieces <- length(pieces)
    attr_row <- st_drop_geometry(cell_row)
    attrs_rep <- attr_row[rep(1, n_pieces), , drop = FALSE]
    piece_sf <- st_as_sf(cbind(attrs_rep, data.frame(orig_index = i, part = seq_len(n_pieces))),
                         geometry = pieces, crs = st_crs(grid_sf))
    piece_sf
  }
  
  res_list <- lapply(candidate_idx, process_cell)
  res_list <- Filter(Negate(is.null), res_list)
  
  # Combine pieces (if any) and untouched non-candidate cells
  if (length(res_list) > 0) {
    pieces_sf <- do.call(rbind, res_list)
    pieces_sf <- st_make_valid(pieces_sf)
    pieces_sf <- st_cast(pieces_sf, "MULTIPOLYGON") |> 
      st_cast("POLYGON")
    # compute area and filter tiny pieces
    pieces_sf$area <- as.numeric(st_area(pieces_sf))
    pieces_sf <- pieces_sf[pieces_sf$area > 0, , drop = FALSE]
  } else {
    pieces_sf <- grid_sf[0, , drop = FALSE]
  }
  
  # Untouched cells: keep their geometry and set part = 1
  if (length(noncandidate_idx) > 0) {
    untouched <- grid_sf[noncandidate_idx, , drop = FALSE]
    # add orig index and part
    untouched$orig_index <- noncandidate_idx
    untouched$part <- 1
    untouched$area <- as.numeric(st_area(untouched))
  } else {
    untouched <- grid_sf[0, , drop = FALSE]
  }
  
  # Combine final set
  combined <- rbind(pieces_sf, untouched) |> mutate(cell_id = row_number())
  
  # Add new unique id and ensure area present
  combined$area <- as.numeric(st_area(combined))
  combined <- combined[, c('cell_id', 'area')]
  return(combined)
}
