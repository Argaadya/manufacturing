# Giant Path -----------

giant_path <- function(distance, demand){
  
  visited_spot <- NULL
  vehicle_num <- 1
  post <- 1:ncol(distance)
  names(demand) <- 1:length(demand)
  
  # Randomly select initial spot
  initial_spot <- sample(2:length(demand), 1)
  
  while (any(demand != 0)) {
    
    available_spot <- which(demand != 0)
    # Calculate the distance to unvisited spot
    initial_dist <- distance[ c(available_spot), initial_spot] 
    initial_dist <- initial_dist[ which(names(initial_dist) != initial_spot)]
    
    visited_spot <- c(visited_spot, initial_spot)
    demand[ initial_spot ] <- 0
    
    if ( length(initial_dist)>1) {
      initial_spot <- which(initial_dist == min(initial_dist)) %>% names() %>% as.numeric()
    } else {
      initial_spot <- which(demand != 0)
    }
  }
  
  visited_spot <- c(1, visited_spot, 1)
  names(visited_spot) <- NULL
  total_distance <- embed(visited_spot, 2)[ , 2:1] %>% distance[.] %>% sum()
  
  result <- list(route = visited_spot,
                 total_distance = total_distance)
  return(result)
}

# Assign Vehicle -----------------

assign_vehicle <- function(x, demand, capacity, distance){
  
  vehicle_load <- capacity
  visited_spot <- NULL
  vehicle_num <- 1
  
  for (i in x) {
    
    initial_spot <- i
    
    if (vehicle_load >= demand[i]) {
      visited_spot <- c(visited_spot, initial_spot)
      vehicle_load <- vehicle_load - demand[ initial_spot ]
      
    } else {
      
      # Go back to depot
      vehicle_num <- vehicle_num + 1
      vehicle_load <- capacity
      visited_spot <- c(visited_spot, 1)
      
      # Revisit the spot 
      visited_spot <- c(visited_spot, initial_spot)
      vehicle_load <- vehicle_load - demand[ initial_spot ]
    }
    
  }
  
  visited_spot <- c(visited_spot)
  total_distance <- embed(visited_spot, 2)[ , 2:1] %>% distance[.] %>% sum()
  
  return(list(distance = total_distance,
              route = visited_spot,
              num_vehicle = vehicle_num))
}

# Nearest Algorithm A -----------

nearest_a <- function(x, demand, capacity, distance, iteration){
  
  best_route <- list(total_distance = Inf)
  
  for (i in 1:iteration) {
    try_route <- giant_path(demand = demand, distance =  distance)
    
    if (best_route$total_distance > try_route$total_distance) {
      best_route <- try_route
    }
  }
  
  travel_route <- assign_vehicle(x = best_route$route, demand = demand, capacity = capacity, distance = distance)
  
return(travel_route)
}