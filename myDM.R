myFunction <- function(roads, car, packages) { # nolint: object_name_linter.
  # print(paste("myFunction called. Car at (", car$x, ",", car$y, "), load=", car$load))


  dim <- nrow(roads$vroads);
  if(ncol(roads$hroads) != nrow(roads$vroads)) {
    stop(paste("bad dimension",dim))
  }

  if (car$load>0) {
    # FLytta postgubben med A_star algoritmen när du har ett paket
    car$nextMove = a_star(list(x=car$x,y=car$y), list(x=packages[car$load,3],y=packages[car$load,4]),roads,dim)
  }
  else {
    # TODO: Flytta postgubben automatiskt (inte manuellt) när du inte har ett paket 
    car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
    
    if (car$nextMove=="q") {stop("Game terminated on user request.")}
  }
  
  car
}

a_star <- function(start, target, roads, dim) {
  # If already at target, stay still
  if (start$x == target$x && start$y == target$y) {
    return(5)
  }
  
  current = start

  # F cost = G cost (distance from start) + H cost (distance from end)
  g_costs = matrix(rep(-1,dim*dim),nrow=dim)
  h_costs = matrix(rep(-1,dim*dim),nrow=dim)
  
  # visited matriser för att hålla koll på vilka noder som redan är besökta
  visited <- matrix(FALSE, nrow=dim, ncol=dim)
  
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
    
    first <- pop_front(frontier)
    if (is.null(first)) {
      break
    }

    # Skip if this node was already visited
    if (visited[first$x, first$y]) {
      next
    }
    visited[first$x, first$y] <- TRUE

    current <- list(x = first$x, y = first$y)

    # Check if we found the target
    if (current$x == target$x && current$y == target$y) {
      # Reconstruct path to get first move
      path_x <- current$x
      path_y <- current$y
      
      # Trace back to find the first move
      while (!is.null(parent[[path_x, path_y]])) {
        prev_pos <- parent[[path_x, path_y]]
        if (prev_pos$x == start$x && prev_pos$y == start$y) {
          # This is the first move
          dx <- path_x - start$x
          dy <- path_y - start$y
          
          if (dx == 1) return(6)      # Right
          if (dx == -1) return(4)     # Left  
          if (dy == 1) return(8)      # Up
          if (dy == -1) return(2)     # Down
        }
        path_x <- prev_pos$x
        path_y <- prev_pos$y
      }
      return(5)  # Stay still if no clear path
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
      if (visited[nx, ny]) {
        next
      }

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
  return(5)
}