fitness <- function(x, capacity, demand, distance, ...){
  
  vehicle_load <- capacity
  visited_spot <- 1
  vehicle_num <- 1
  
  for (i in x) {
    
    initial_spot <- i
    
    if (vehicle_load > demand[initial_spot]) {
      
      # Go to the spot
      visited_spot <- c(visited_spot, initial_spot)
      vehicle_load <- vehicle_load - demand[ initial_spot ]
      
    } else {
      # Go back to depot
      vehicle_load <- capacity
      visited_spot <- c(visited_spot, 1)
      vehicle_num <- vehicle_num + 1
      
      # Go to the spot 
      visited_spot <- c(visited_spot, initial_spot)
      vehicle_load <- vehicle_load - demand[ initial_spot ]
    }
    
  }
  
  visited_spot <- c(visited_spot, 1)
  total_distance <- embed(visited_spot, 2)[ , 2:1] %>% distance[.] %>% sum()
  
  return(-total_distance)
}


fitness_explain <- function(x, capacity, demand, distance, ...){
  
  vehicle_load <- capacity
  visited_spot <- 1
  vehicle_num <- 1
  total_demand <- NULL
  
  for (i in x) {
    
    initial_spot <- i
    
    if (vehicle_load > demand[initial_spot]) {
      
      # Go to the spot
      visited_spot <- c(visited_spot, initial_spot)
      vehicle_load <- vehicle_load - demand[ initial_spot ]
      
    } else {
      
      total_demand <- c(total_demand, 6000 - vehicle_load)
      
      # Go back to depot
      vehicle_load <- capacity
      visited_spot <- c(visited_spot, 1)
      vehicle_num <- vehicle_num + 1
      
      # Go to the spot 
      visited_spot <- c(visited_spot, initial_spot)
      vehicle_load <- vehicle_load - demand[ initial_spot ]
    }
    
  }
  
  total_demand <- c(total_demand, 6000 - vehicle_load)
  visited_spot <- c(visited_spot, 1)
  total_distance <- embed(visited_spot, 2)[ , 2:1] %>% distance[.] %>% sum()
  
  result <- list(route = visited_spot,
                 total_distance = total_distance,
                 vehicle_num = vehicle_num,
                 total_demand = total_demand)  
  
  return(result)
}

