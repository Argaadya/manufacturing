nearest_b <- function(demand, distance, capacity){
  
  visited_spot <- NULL
  vehicle_load <- capacity
  vehicle_num <- 1
  post <- 1:ncol(distance)
  names(demand) <- 1:length(demand)
  
  # Randomly select initial spot
  initial_spot <- sample(2:length(demand), 1)
  
  # nearest neighbour algorithm
  while ( any(demand != 0) ) {
    
    available_spot <- which(demand != 0)
    # Calculate the distance to unvisited spot
    initial_dist <- distance[ c(available_spot), initial_spot] 
    initial_dist <- initial_dist[ which(names(initial_dist) != initial_spot)]
    visited_spot <- c(visited_spot, initial_spot)
    
    # Check if vehicle load is greater than the demand
    if (vehicle_load > demand[ initial_spot ]) {
      
      # continue to travel
      vehicle_load <- vehicle_load - demand[ initial_spot ] 
      demand[ initial_spot ] <- 0
      
      # Is there is more than 1 unvisited spot left
      
      if ( length(initial_dist)>1) {
        
        initial_spot <- which(initial_dist == min(initial_dist)) %>% names() %>% as.numeric()
        
        if (length(initial_spot) > 1) {
          initial_spot <- sample(initial_spot, 1)
        }
        
      } else {
        initial_spot <- which(demand != 0)
      }
      
    } else {
      # return to depot because the vehicle cannot fulfill the demand
      demand[ initial_spot ] <- demand[ initial_spot ] - vehicle_load
      vehicle_num <- vehicle_num + 1
      vehicle_load <- capacity
      initial_spot <- 1
      visited_spot <- c(visited_spot, initial_spot)
      
      # randomly select the next destination
      initial_spot <- sample(available_spot, 1)
      
    }
    
  }
  
  visited_spot <- c(1, visited_spot, 1)
  names(visited_spot) <- NULL
  total_distance <- embed(visited_spot, 2)[ , 2:1] %>% distance[.] %>% sum()
  
  result <- list(vehicle_num = vehicle_num, 
                 route = visited_spot,
                 total_distance = total_distance)
  return(result)
}