#========================================================================
# function for detecting overlapping dates adding a variable, buffer
# in days), for what an acceptable overlap is
#========================================================================
# NB: ridiculously hacky hard coded init value in the reduce statement
date_compare <- function(l) {
  overlapping <-
    cross2(l, l, function(x,y) x==y) %>%
    map(~ c(.[[1]], .[[2]])) %>%
    map(sort) %>%
    unique %>%
    keep(~ int_overlaps(.[1], .[2])) %>%
    reduce(function(x, y) c(x, y[1], y[2]),
           .init = interval(ymd(30000101), ymd(99991231))) %>%
    unique
  
  return(l %in% overlapping)
}

#========================================================================
# function for combining several overlapping geometries 
#========================================================================
geom_combine <- function(l, i) {
  if(!is.list(l)) {
    l <- list(l)
  }
  lp <- keep(l, function(x) length(intersect(x, i)) > 0)
  if(length(lp) == 0) {
    return(c(l, list(i)))
  }
  else {
    ol <- setdiff(l, lp)
    nl <- list(reduce(c(lp, list(i)), union))
    return(c(ol, nl))
  }
}

#=======================================================================
# report a pasted version of unique values in a list
#========================================================================
upaste <- function(l) {
  if(length(unique(l)) == 1) {
    return(l[1])
  }
  else {
    return(paste(l, collapse = "; "))
  }
}

#========================================================================
# shorthand for a function that does name trimming work
#========================================================================
srat <- function(s, r, v = "") {
  return(str_trim(str_replace_all(s, r, v)))
}

#========================================================================
# function for cleaning lessee/operator names using company_names
#========================================================================
clean_names <- function(df, column){
  # read in company names, clean them up as needed, and apply them to the
  # requested data
  column <- enquo(column)

  company_names <-
    file.path(raw, "company_aliases.csv") %>%
    read_csv %>%
    mutate(company = str_to_upper(company)) %>%
    select(company, alias) %>%
    unique  %>% 
    rename(!! column := company)  

  df <-
    df %>%
    mutate_at(vars(!! column), str_to_upper) %>%
    left_join(company_names) 
}

# ========================================================================
# faster st_difference
# ========================================================================
# this version removes the "remove_this" geometries from "trim_me" without
# scanning through all pairs of geometries 
st_left_difference <- function(trim_me, remove_this) {
  suppressMessages({
  
  intersections <-
    st_intersects(trim_me, remove_this) %>%
    as.data.table()
  
  setnames(intersections,
           old = c("row.id", "col.id"),
           new = c("trim_id", "remove_id"))
  
  trim <-
    data.table(trim_geometry = st_geometry(trim_me))[, trim_id := .I]
  remove <-
    data.table(remove_geometry = st_geometry(remove_this))[, remove_id := .I]
  
  setkey(trim, trim_id)
  setkey(remove, remove_id)
  setkey(intersections, remove_id)
  
  difference_this <-
    remove[intersections]
  
  to_remove <-
    difference_this[
    , .(union = st_union(remove_geometry)), keyby = trim_id]
  
  removed <-
    trim[to_remove][
    , .(trim_geometry = st_difference(trim_geometry, union)), by = trim_id]
  
  setcolorder(trim, c("trim_id", "trim_geometry"))
  
  no_int <-
    anti_join(trim, intersections)
  
  class(removed$trim_geometry) <- class(no_int$trim_geometry)
  
  out <-
    rbind(no_int, removed)[
    , trim_id := NULL] %>%
    st_sf()
  
  return(out)})
}

#==============================================================================
# returns a spatial-temporal join of shapes to points, with optional spatial
# and/or temporal buffering
# the whole thing is "tidy" so you can pass plain column names to the variables:
# uid_shape: unique identifier for rows in `shapes`
# uid_point: unique identifier for rows in `points`
# point_time: time variable in the point data.  when the point "occurs"
# shape_start: start time in the shape data.  when the shape "starts"
# shape_end: end time in the shape data.  when the shape "ends"
# the remaining 3 named parameters control various notions of buffering
# min_shape_dist: how far inside a shape's border must a point be before we
# consider it "interior"
# dist_buffer: how far outside a shape's border will we allow a point to match
# to a shape
# time buffer: how much wiggle room on shape_start and shape_end are we going to
# allow
# when a point intersects multiple shapes (possibly w a buffer) and its event 
# occurs after multiple shapes' start times (possibly w a buffer), we have
# to decide which match to keep.  to do this, we order a points matches: first
# by a coarse measure of how far inside or outside the point is relative to a
# shapes border.  next, by whether or not we needed to use the time buffer, to
# keep the point inside the shape's start and end times.  finally, we sort by
# the shape's start dates in descending order.
# the resulting output will have 5 columns: uid_shape, uid_point and 3 flags:
# flag_buffer:
# - 0 if the point is at least min_shape_dist interior of the shape
# - 1 if its between min_shape_dist and 0 interior of the shape
# - 2 if its exterior and we have allowed for a buffer
# flag_late:
# - 0 if the point occurs before shape_end
# - 1 if the point occurs after shape_end but before the time buffer kicks in
# - 2 if the point occurs after shape_end + time_buffer
# n_shapes: is the number of total matches between the point and shape
#==============================================================================
shape_point_time <- function(shapes,
                             points,
                             uid_shape = NULL,
                             uid_point = NULL,
                             point_time = NULL,
                             shape_start = NULL,
                             shape_end = NULL,
                             min_shape_dist = 100,
                             dist_buffer = 0,
                             time_buffer = 0,
                             utm_crs = 32614) {

  uid_shape <- enquo(uid_shape)
  uid_point <- enquo(uid_point)
  point_time <- enquo(point_time)
  shape_start <- enquo(shape_start)
  shape_end <- enquo(shape_end)

  # these two steps assume a utm_crs is currently defined
  shapes <-
    shapes %>%
    st_transform(utm_crs)

  points <-
    points %>%
    st_transform(utm_crs)
  
  intersected_data <-
    st_join(shapes, points, join = st_covers, left = FALSE) %>%
    as.data.frame %>%
    select(-geometry) %>%
    as_tibble %>%
    mutate(Interior = 1)

  if(dist_buffer > 0) {
    print("doing a buffered intersection")
    intersected_data_buffered <-
      shapes %>%
      st_buffer(dist = dist_buffer) %>%
      st_join(points, join = st_covers, left = FALSE) %>%
      as.data.frame %>%
      select(-geometry) %>%
      as_tibble

    intersected_data <-
      intersected_data_buffered %>%
      left_join(intersected_data) %>%
      replace_na(list(Interior = 0))
  }

  intersected_data <-
    intersected_data %>%
    filter(!! point_time >= !! shape_start - time_buffer)

  intersected_data_uid <-
    intersected_data %>%
    select(!! uid_shape, !! uid_point, Interior)

  shapes <-
    shapes %>%
    mutate(geometry = st_cast(geometry, "MULTILINESTRING"))

  # note this is pretty slow
  shape_point_distances <-
    intersected_data_uid %>%
    left_join(shapes) %>%
    rename(shape_geometry = geometry) %>%
    left_join(points) %>%
    rename(point_geometry = geometry) %>%
    select(!! uid_shape, !! uid_point, Interior,
           shape_geometry, point_geometry) %>%
    mutate(dist = as.numeric(st_distance(shape_geometry,
                                         point_geometry,
                                         by_element = TRUE))) %>%
    mutate(dist = if_else(Interior == 1, dist * -1, dist)) %>%
    select(-shape_geometry, -point_geometry)

  # added the replace_na because its better to have an explicit treatment of
  # missing values
  intersected_data <-
    intersected_data %>%
    left_join(shape_point_distances) %>%
    mutate(flag_buffer = case_when(
             dist < -1 * min_shape_dist ~ 0,
             dist < 0 ~ 1,
             TRUE ~ 2),
           flag_late = case_when(
             !! point_time <= !! shape_end ~ 0,
             !! point_time <= !! shape_end + time_buffer ~ 1,
             TRUE ~ 2)) %>%
    replace_na(list(flag_buffer = 3, flag_late = 1)) %>%
    arrange(!! uid_point, flag_buffer, flag_late, desc(!! shape_start)) %>%
    group_by(!! uid_point) %>%
    mutate(n_shapes = n()) %>%
    filter(row_number() == 1) %>%
    ungroup %>%
    select(!! uid_shape, !! uid_point, flag_buffer, flag_late, n_shapes)
    
  return(intersected_data)
}


# ========================================================================
# Multiple Intersections
# ========================================================================
# use graph theory magic - if a intersects b and b intersects c, we want
# a, b, and c to all be in one group
int_bundles <- function(x, y, name){
  pairs <- 
    map2(x, y, c) %>% 
    map(as.character)

  pairs_embed <- do.call("rbind", lapply(pairs, embed, 2))
  pairs_vert <- graph.edgelist(pairs_embed, directed = F)
  pairs_id <- split(V(pairs_vert)$name, clusters(pairs_vert)$membership)

  # collapse list of ids in the same group to a dataframe 
  pairs_id %>%
    map_df(~ cbind(., paste(., collapse = "-")) %>% 
             as_tibble() %>%
             setNames(c(name, "sameid")))
}


# ========================================================================
# character encoding issues
# ========================================================================
fixstrings <- function(s) {
  return(if_else(stri_enc_mark(s) == "UTF-8",
                 stri_encode(s, from = "UTF-8", to = "ASCII"),
                 s))

}

# ========================================================================
# compute number of months that pass between start and end dates
# ========================================================================
makemonths <- function(start, end) {
  as.integer(interval(start, end) / months(1))
}


#===========================================================================
# create function for extracting classification values for each lease
#===========================================================================
st_extract <- function(shape, id, ras){
  shape <- as(st_sfc(shape), "Spatial")
  temp <- raster(shape)
  raster_temp <- crop(ras, extent(temp))
  count <- as.data.frame(table(raster::extract(raster_temp, shape)))

  if(nrow(count) > 0){
    count <- cbind("ID" = id, count)
  } else{
    count <- tibble(ID = id) 
  }

  return(count)   
}

st_extract_group <- function(poly, idname, ras){
  idname <- enquo(idname)
  shape_column <- pull(poly, geometry)
  id_column <- pull(poly, !!idname)

  map2_df(shape_column, id_column, st_extract, ras) %>%
    rename(!!quo_name(idname) := ID)
}


#===========================================================================
# landcover classification dictionary
#===========================================================================
classification <-
  list(
    "Open_Water" = 11, 
    "Perennial_IceSnow" = 12,
    "Developed_OS" = 21, 
    "Developed_LI" = 22, 
    "Developed_MI" = 23, 
    "Developed_HI" = 24, 
    "Barren" = 31, 
    "Deciduous_Forest" = 41, 
    "Evergreen_Forest" = 42, 
    "Mixed_Forest" = 43, 
    "Dwarf_Scrub" = 51, 
    "Shrub_Scrub" = 52, 
    "Grassland" = 71, 
    "Sedge" = 72, 
    "Lichens" = 73,
    "Moss" = 74, 
    "Pasture" = 81, 
    "Cultivated_Crops" = 82,
    "Woody_Wetlands" = 90, 
    "Herbaceous_Wetlands" = 95
  ) 
classification_dict <-  
  bind_cols("Classification" = names(classification), 
            "value" = reduce(classification, c))

#===========================================================================
# flag lessee's who we think are brokers
#===========================================================================
broker_regex <-
  c("propert", " land", "servic", "interest", "assoc", "dwyer",
    "david william", "johnson clay", "clay johnson", "san luis", "larry",
    "mayne", "trimble", "soape", "doug", "hodges", "bellomy", "blair",
    "robbins", "veritas", "horn", "culpepper", "cannon", "wormser", "reeves",
    "basin ventures", "transcendent", "dawson", "weatherl", "san saba",
    "saye", "christensen", "hemus", "kimmeridge", "burleson", "champion",
    "cole stephen", "dixon", "mccabe", "wolcott", "armstrong", "ramsey",
    "alpine", "webb", "huber", "shorthorn", "cervus", "angelle", "pinnacle",
    "mike gaddy", "parke", "leascher", "harris william p", "landsmith",
    "perry") %>%
  paste(collapse = "|") %>%
  regex(ignore_case = T)

# other firms we might want to just write over:
# Pure: pretty sure they are just an LP in HENRY PETROLEUM, see here:
# https://www.oaoa.com/inthepipeline/article_5aac627c-1542-11e4-bc38-001a4bcf6878.html
# other firms to do more cleaning on:
# henry resources vs. henry petroleum

nonbroker_regex <-
  regex("westridge|horizon|neuhaus|alta mesa", ignore_case = T)

#===========================================================================
# function for finding closest shale play
#===========================================================================
closest_shale <- function(df, tolerance = 0){
  start <- Sys.time()

  load(file.path(shape, "shale_plays.Rda"))

  df_crs <- st_crs(df)

  df <-
    df %>%
    st_transform(utm_crs) %>%
    mutate(id = row_number())

  # first find ones that are already on a shale
  onshale <-
    st_join(df, shale_plays, left = FALSE) %>%
    mutate(ShaleDist = 0)

  # now find closet shale
  # use the boundary of the shale and simplify so that there are less points
    # to find distances to
  shale_points <-
    shale_plays %>%
    st_simplify(dTolerance = tolerance) %>% 
    st_boundary() %>% 
    st_cast("POINT") 

  # make a plain shale dataset without geometries to use for closest_shale
  shale_data <-
    shale_points %>%
    as.data.frame() %>%
    select(-geometry) 

  closest_shale_values <-
    df %>%
    anti_join(select(as.data.frame(onshale), id))
  
  if(sum(st_geometry_type(st_geometry(df)) != "POINT") > 0){
    closest_shale_values <-
      closest_shale_values %>%
      st_centroid 
  } 

  closest_shale_values <- st_distance(closest_shale_values, shale_points) 


  # find shale formation that the lease centroid is closest to
  closest_shale_formation <-
    closest_shale_values %>%
    apply(1, which.min) %>%
    map_df(~ shale_data[.x,])

  # find distance to closest shale formation
  closest_shale_dist <-
    closest_shale_values %>%
    apply(1, min) 

  closest_shale <-
    df %>%
    anti_join(select(as.data.frame(onshale), id)) %>%
    bind_cols(tibble(ShaleDist = closest_shale_dist)) %>%
    bind_cols(closest_shale_formation) 

  output <-
    rbind(onshale, closest_shale) %>%
    mutate(OnShale = if_else(ShaleDist == 0, TRUE, FALSE)) %>%
    select(-id) %>%
    st_transform(df_crs) 

  print(paste("Done in", Sys.time() - start))

  return(output)
}


# ===========================================================================
# function for calculating distance of a lease to each infrastructure
# ===========================================================================
infra_dist <- function(df, inf, var_name, df_id){
  df_int <- 
    df %>%
    st_join(inf, left = FALSE) %>%
    st_set_geometry(NULL) %>%
    select(!!df_id) %>%
    as_tibble() %>%
    mutate(!!var_name := 0)

  no_int <-
    df %>%
    anti_join(df_int) %>%
    st_centroid()

  # first find nearest feature to vectorize distance function 
  indices <- st_nearest_feature(no_int, inf)

  nearest <- inf$geometry[indices]
  dist <- st_distance(no_int$geometry, nearest, by_element = TRUE)

  df_no_int <-
    no_int %>%
    st_set_geometry(NULL) %>%
    as_tibble() %>%
    select(!!df_id) %>%
    mutate(!!var_name := as.numeric(dist)) 

  df_dist <-
    rbind(df_int, df_no_int) %>%
    distinct() %>%
    group_by_(df_id) %>%
    arrange_(var_name) %>%
    filter(row_number() == 1)

  return(df_dist)
}
