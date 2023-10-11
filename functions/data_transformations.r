# Data transformations file

# Problem 2

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
  function(df) {
    df <-
      stations_metadata[[1]] %>%
      map(as_tibble) %>% 
      list_rbind() %>% 
      mutate(latestData = map_chr(latestData, unlist_safe)) %>% 
      mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
      unnest_wider(location) %>% 
      unnest_wider(latLon)
    
    return(df) # Returns the data frame in the function
    }


# Problem 4

# a - time function
to_iso8601 <- function(datetime, offset_days) {
  
  # Converts string to datetime object
  datetime_var <- anytime(datetime)
  
  # Add offset to original time
  adjusted_datetime <- datetime_var + days(offset_days)
  
  # UTC and add iso8601 formatting with Z
  iso8601_z <- format(adjusted_datetime, 
                      format = "%Y-%m-%dT%H:%M:%SZ",
                      tz = "UTC")
  
  return(iso8601_z)
}

# Test the time function
to_iso8601(as_datetime("2016-09-01 10:11:12"), 0)   # Works
to_iso8601(as_datetime("2016-09-01 10:11:12"), -4)  # Works




    
  
