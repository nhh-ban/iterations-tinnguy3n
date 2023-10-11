# Data transformations file - Problem 2

# Unlist function
unlist_safe <-
  function(x) {
    x <- unlist(x)
    if (is.null (x)) {
      return(NA_character_) 
    }else{return(x)}
  }


# Function transforming metadata to dataframe
transform_metadata_to_df <- 
  function(y) {
    {y <- stations_metadata[[1]]}
    {stations_metadata[[1]] %>%
      map(as_tibble) %>% 
      list_rbind() %>% 
      mutate(latestData = map_chr(latestData, unlist_safe)) %>% 
      mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
      unnest_wider(location) %>% 
      unnest_wider(latLon)}
  }
  
transform_metadata_to_df(.)

