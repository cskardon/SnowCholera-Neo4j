rotate_coords <- function(e, n, angle_deg) {
  theta <- angle_deg * pi / 180  # convert to radians
  e_rot <- e * cos(theta) - n * sin(theta)
  n_rot <- e * sin(theta) + n * cos(theta)
  return(data.frame(easting = e_rot, northing = n_rot))
}


generate_latlong <- function(df){
  #' Generates latitude and longitude columns from the Easting and Northing columns in the SnowCholera data frames.
  #' 
  #' @description Uses the Easting and Northing and converts them to Lat/Long.
  #' 
  #' NB this does not modify the parameter, it returns an updated version of the data frame.
  #' 
  #' @section What it does
  #' 
  #' Stuff!
  #' 
  #' @param df the data frame to be modified, either the snow_data or pump_data data frames.

  working_df  <- df |> 
    mutate(Easting = as.integer(Easting), Northing = as.integer(Northing)) |>
    mutate(rotated = rotate_coords(Easting, Northing, 0)) |> 
    mutate(sf_points = st_as_sf(rotated, coords = c("easting", "northing"), crs = 27700)) |> 
    mutate(sf_points_wgs84 = st_transform(sf_points, crs = 4326)) |> 
    mutate(
        longitude = st_coordinates(sf_points_wgs84)[,1], 
        latitude = st_coordinates(sf_points_wgs84)[,2]
    ) |> 
    mutate(
        sf_points = NULL,
        sf_points_wgs84 = NULL,
        rotated = NULL
    )
  return(working_df)
}

#' @keywords internal
.neo4j_query_execute <- function(cypher, db_settings){
  # Create https request
  req <- request(paste0(db_settings['schema'], db_settings['server'], "/db/", db_settings['database'], "/query/v2")) |>
    req_auth_basic(db_settings['username'], db_settings['password']) |>
    req_headers("Accept" = "application/json") |>
    req_body_json(list("statement" = cypher))
  
  # Perform request
  resp <- req |> req_perform()
  
  # Extract response and create dataframe
  json <- resp |> resp_body_json(simplifyVector = TRUE)
  names <- json |> pluck("data", 1)
  df <- json |> pluck("data", 2) |> data.frame()

  # Rename columns
  colnames(df) = c(names)
  df
}

neo4j_query <- function(cypher, db_settings){
  if(!is.vector(cypher))
  {
    output <- cypher |> .neo4j_query_execute(db_settings)
    return(output)
  }
  
  results = c()
  for(query in cypher){
    output <- query |> .neo4j_query_execute(db_settings)
    results <- append(results, output)
  }

  return(results)
}