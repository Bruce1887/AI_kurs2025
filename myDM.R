myFunction <- function(roads, car, packages) { # nolint: object_name_linter.
  
  # if (length(car$mem) == 0) {
  #   car$mem$temp <- 1
  # }
  # else {
  #   car$mem$temp = car$mem$temp + 1
  # }
  # if(car$mem$temp < 20) {
  #   car$nextMove = 5
  #   return(car)
  # }

  dim <- nrow(roads$vroads);
  if(ncol(roads$hroads) != nrow(roads$vroads)) {
    stop(paste("bad dimension",dim))
  }

  if (car$load>0) {
    # FLytta postgubben med A_star algoritmen när du har ett paket
    ret <- a_star(list(x=car$x,y=car$y), list(x=packages[car$load,3],y=packages[car$load,4]),roads,dim)
    car$nextMove = ret$direction 
  }
  else {
    
    notpickedup=which(packages[,5]==0)

    cheapest <- list(step_direction= -1, cost = Inf)

    start <- list(x=car$x,y=car$y)
    # print(paste("start",start$x,start$y))

    for(idx in notpickedup)
    {
      p <- packages[idx, ] # select the full row
      # print(p)

      package_pickup <- list(x=p[1],y=p[2])
      # print(paste("package_pickup:",package_pickup$x,package_pickup$y))
      
      package_target <- list(x=p[3],y=p[4])
      # print(paste("package_target:",package_target$x,package_target$y))

      first_trip = a_star(start,package_pickup,roads,dim)
      # print(paste("first_trip:",first_trip$direction,first_trip$cost))

      second_trip = a_star(package_pickup,package_target,roads,dim)
      # print(paste("second_trip:",second_trip$direction,second_trip$cost))

      # print("Horizontal roads:")
      # print(roads$hroads)
      # print("Vertical roads:")
      # print(roads$vroads)

      # stop("devbug")

      tot_cost <- first_trip$cost + second_trip$cost

      if(tot_cost < cheapest$cost) {
        # välj det paket som har billigast kostnad (utan att väga in hur de andra paketen samspelar)
        cheapest$step_direction = first_trip$direction
        cheapest$cost = tot_cost
      }
    }
    # print(paste("cheapest", cheapest))
    car$nextMove = cheapest$step_direction
  }
  
  car
}

a_star <- function(start, target, roads, dim) {
  # If already at target, stay still
  if (start$x == target$x && start$y == target$y) {
    return(list(direction=5,cost=0))
  }
  
  current = start

  # F cost = G cost (distance from start) + H cost (distance from end)
  g_costs = matrix(rep(-1,dim*dim),nrow=dim)
  h_costs = matrix(rep(-1,dim*dim),nrow=dim)
  
  # visited matriser för att hålla koll på vilka noder som redan är besökta
  # visited <- matrix(FALSE, nrow=dim, ncol=dim)
  
  # Parent tracking for path reconstruction
  parent <- array(list(NULL), dim=c(dim, dim))

  # Sätt g kostnaded på första rutan till 0, behövs för att räkna g_kostnaden till andra rutor.
  g_costs[start$x,start$y] = 0
  h_costs[start$x,start$y] = abs(start$x - target$x) + abs(start$y - target$y)

  manhattan <- function(ax, ay, bx, by){
    return (abs(ax-bx) + abs(ay-by))
  }

  make_node <- function(x, y, next_node) {
    node <- new.env(parent = emptyenv())
    node$x <- x
    node$y <- y
    node$next_node <- next_node
    node
  }

  insert_value_at_correct_place <- function(head, x, y, dim, g_costs, h_costs) {
    new_g <- g_costs[x, y]
    new_h <- h_costs[x, y]
    new_f <- new_g + new_h

    if (is.null(head)) {
      return(make_node(x, y, NULL))
    }

    head_g <- g_costs[head$x, head$y]
    head_h <- h_costs[head$x, head$y]
    head_f <- head_g + head_h
    
    if (new_f < head_f || (new_f == head_f && new_g <= head_g)) {
      new_node <- make_node(x, y, head)
      return(new_node)
    }

    current <- head
    while (!is.null(current$next_node)) {
      next_node <- current$next_node
      next_g <- g_costs[next_node$x, next_node$y]
      next_h <- h_costs[next_node$x, next_node$y]
      next_f <- next_g + next_h

      if (new_f < next_f || (new_f == next_f && new_g <= next_g)) {
        break
      }
      current <- current$next_node
    }

    new_node <- make_node(x, y, current$next_node)
    current$next_node <- new_node
    return(head)
  }

  print_list <- function(head) {
    cat("printing list..\n")
    current <- head
    while (!is.null(current)) {
      cat(current$x, current$y, " -> ")
      current <- current$next_node
    }
    cat("NULL\n")
  }


  already_in_frontier <- function(head, x, y) {
    current <- head
    while (!is.null(current)) {
      if (current$x == x && current$y == y) {
        return(TRUE)
      }
      current <- current$next_node
    }
    return(FALSE)
  }

  pop_front <- function(head_ref) {
    if (is.null(head_ref$head)) return(NULL)
    old_head <- head_ref$head
    head_ref$head <- head_ref$head$next_node
    old_head
  }

  # Create a head reference as environment
  frontier <- new.env()
  frontier$head <- make_node(current$x, current$y, NULL)
  
  iteration_count <- 0
  max_iterations <- 1000

  while (!is.null(frontier$head) && iteration_count < max_iterations) {
    iteration_count <- iteration_count + 1
    
    # print_list(frontier$head)
    
    first <- pop_front(frontier)
    if (is.null(first)) {
      stop("frontier pop gave null. BAD!")
    }

    # Skip if this node was already visited
    # if (visited[first$x, first$y]) {
    #   next
    # }
    # visited[first$x, first$y] <- TRUE

    current <- list(x = first$x, y = first$y)

    
    # Check if we found the target
    if (current$x == target$x && current$y == target$y) {
      # Reconstruct path to get first move
      path_x <- current$x
      path_y <- current$y

      # print(paste("found way from", start$x,start$y, "to", target$x,target$y))

      # Trace back to find the first move
      while (!is.null(parent[[path_x, path_y]])) {
        # print(paste("path",path_x,path_y))
        prev_pos <- parent[[path_x, path_y]]
        if (prev_pos$x == start$x && prev_pos$y == start$y) {
          # This is the first move
          dx <- path_x - start$x
          dy <- path_y - start$y
          
          c <- g_costs[target$x,target$y] 
          if (dx == 1) return(list(direction=6,cost=c))      # Right
          if (dx == -1) return(list(direction=4,cost=c))     # Left  
          if (dy == 1) return(list(direction=8,cost=c))      # Up
          if (dy == -1) return(list(direction=2,cost=c))     # Down
        }
        path_x <- prev_pos$x
        path_y <- prev_pos$y
      }
      stop("no clear path! This is bad!")
      return(list(direction=5,cost=Inf))  # Stay still if no clear path
    }

    hroads <- roads$hroads
    vroads <- roads$vroads

    # Define possible moves: (dx, dy, cost_matrix)
    moves <- list(
      up    = list(dx = 0,  dy = 1, cost_matrix = vroads),
      right = list(dx = 1,  dy = 0, cost_matrix = hroads),
      down  = list(dx = 0,  dy = -1, cost_matrix = vroads),
      left  = list(dx = -1, dy = 0, cost_matrix = hroads)
    )

    for (m in moves) {
      nx <- current$x + m$dx
      ny <- current$y + m$dy

      # bounds check
      if (!(nx >= 1 && nx <= nrow(g_costs) && ny >= 1 && ny <= ncol(g_costs))) {
        next
      }

      # Skip if already visited
      # if (visited[nx, ny]) {
      #   next
      # }

      # set h_cost if unset
      if (h_costs[nx, ny] == -1) {
        h_costs[nx, ny] <- manhattan(nx, ny, target$x, target$y)
      }

      # movement cost from current to neighbor
      road_x = nx
      road_y = ny

      if (m$dx == 1) road_x <- road_x - 1
      if (m$dy == 1) road_y <- road_y - 1

      move_cost <- m$cost_matrix[road_x, road_y]

      new_g <- g_costs[current$x, current$y] + move_cost
      
      # update g_cost if unset or better path found
      if (g_costs[nx, ny] == -1 || g_costs[nx, ny] > new_g) {
        # print(paste("<nx,ny> new_g", nx,ny,new_g))
        g_costs[nx, ny] <- new_g
        parent[[nx, ny]] <- list(x = current$x, y = current$y)
        if (!already_in_frontier(frontier$head, nx, ny)) {
          frontier$head <- insert_value_at_correct_place(frontier$head, nx, ny, dim, g_costs, h_costs)
        }
        # print_list(frontier$head)  # Debug print of the frontier list
      }
    }
  } 

  # No path found, stay still
  stop("no path found! This is bad!")
  return(list(direction=5,cost=Inf))  # Stay still if no clear path
}