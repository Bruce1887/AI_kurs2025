myFunction <- function(roads, car, packages) { # nolint: object_name_linter.
  print(paste("car pos: ", car$x,car$y))

  dim <- nrow(roads$vroads);
  if(ncol(roads$hroads) != nrow(roads$vroads)) {
    stop(paste("bad dimension",dim))
  }

  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
    # FLytta postgubben med A_star algoritmen när du har ett paket
    car$nextMove = a_star(list(x=car$x,y=car$y), list(x=packages[car$load,3],y=packages[car$load,4]),roads,dim)
  }
  else {
    # TODO: Flytta postgubben automatiskt (inte manuellt) när du inte har ett paket 
    car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
    
    if (car$nextMove=="q") {stop("Game terminated on user request.")}
    else if (car$nextMove=="p") { 
      # Printar ut grannarna till Car
      print_neighbourhood(car,roads)
    }
  }
  
  car
}

print_neighbourhood <- function(roads,pos){
  nbh <- neigbouring_cost(roads,pos)
  print(paste("neighbour up:",nbh$u))
  print(paste("neighbour right:",nbh$r))
  print(paste("neighbour down:",nbh$d))
  print(paste("neighbour left:",nbh$l))
}

neigbouring_cost <- function(roads, pos) {
  hroads <- roads$hroads
  vroads <- roads$vroads

  neighbourhood <- list(u=-1, r=-1, d=-1, l=-1)

  # fetch up cell (if in bounds)
  if (pos$y + 1 <= ncol(vroads)) {    # check column bounds
    neighbourhood$u <- vroads[pos$x, pos$y + 1]
  }
  # fetch right cell (if in bounds)
  if (pos$x + 1 <= nrow(hroads)) {    # check row bounds
    neighbourhood$r <- hroads[pos$x + 1, pos$y]
  }
  # fetch down cell (if in bounds)
  if (pos$y - 1 >= 1) {
    neighbourhood$d <- vroads[pos$x, pos$y - 1]
  }
  # fetch left cell (if in bounds)
  if (pos$x - 1 >= 1) {
    neighbourhood$l <- hroads[pos$x - 1, pos$y]
  }

  return(neighbourhood)
}

a_star <- function(start,target,roads,dim) {
  print(paste("start:", start, "target:",target))

  target_found <- FALSE
  current = start

  # F cost = G cost (distance from start) + H cost (distance from end)
  g_costs = matrix(rep(-1,dim*dim),nrow=dim)
  h_costs = matrix(rep(-1,dim*dim),nrow=dim)
  # f_costs = <blablabla> # vi behöver inte en f_costs matris, vi deriverar den från g & h
  

  # Sätt g kostnaded på första rutan till 0, behövs för att räkna g_kostnaden till andra rutor.
  g_costs[start$x,start$y] = 0


  manhattan <- function(ax, ay, bx, by){
    return (abs(ax-bx) + abs(ay-by))
  }

  # Node constructor
  make_node <- function(value, next_node = NULL) {
    list(value = value, next_node = next_node)
  }


  # Insert after a specific node
  insert_after <- function(node, value) {
    new_node <- make_node(value, node$next_node)
    node$next_node <- new_node
    invisible(new_node)
  }

  pop_front <- function(head_ref) {
    if (is.null(head_ref$head)) return(NULL)
    value <- head_ref$head$value
    head_ref$head <- head_ref$head$next_node
    value
  }


  # Print the list
  print_list <- function(node) {
    print("printing list..")
    while (!is.null(node)) {
      cat(node$value, " -> ")
      node <- node$next_node
    }
    cat("NULL\n")
  }

  foo <- current$x * dim + current$y

  # Create a head reference as environment
  frontier <- new.env()
  frontier$head <- make_node(foo, NULL)

  print_list(frontier$head)
  
  next_move <- -1

  while((current$x != target$x) && (current$y != target$y)) {
    first <-pop_front(frontier)
    if(is.null(first)) {
      stop("Could not pop first element (got null)")
    }

    print(paste("popped:", first))

    current = list(
      x=first %/% dim,
      y=first%%dim)

    print(paste("current:", current$x,current$y))

    hroads <- roads$hroads
    vroads <- roads$vroads

    # check up cell (if in bounds)
    if (current$y + 1 <= ncol(vroads)) {
      if(h_costs[current$x, current$y + 1] == -1) {
        # h_cost is unset for this cell, set it
        h_costs[current$x, current$y + 1] = manhattan(current$x, current$y + 1, target$x,target$y)
      }
      if ((g_costs[current$x, current$y + 1] == -1) || (g_costs[current$x, current$y + 1] > g_costs[current$x, current$y] + vroads[current$x, current$y + 1])) {
        # value is unset or value is greater than our value, set/update the value 
        g_costs[current$x, current$y + 1] = g_costs[current$x, current$y] + vroads[current$x, current$y + 1]
      }
    }
    # check right cell (if in bounds)
    if (current$x + 1 <= nrow(hroads)) {
      if (h_costs[current$x + 1, current$y] == -1) {
        h_costs[current$x + 1, current$y] = manhattan(current$x + 1, current$y, target$x,target$y)
      }
      if ((g_costs[current$x + 1, current$y] == -1) || (g_costs[current$x + 1, current$y] > g_costs[current$x, current$y] + hroads[current$x + 1, current$y])) {
        # value is unset or value is greater than our value, set/update the value
        g_costs[current$x + 1, current$y] = g_costs[current$x, current$y] + vroads[current$x + 1, current$y]
      }
    }
    # check down cell (if in bounds)
    if (current$y - 1 >= 1) {
      if(h_costs[current$x, current$y - 1] == -1){
        h_costs[current$x, current$y - 1] = manhattan(current$x, current$y - 1, target$x,target$y)
      }
      if ((g_costs[current$x, current$y - 1] == -1) || (g_costs[current$x, current$y - 1] > g_costs[current$x, current$y] + vroads[current$x, current$y - 1])) {
        # value is unset or value is greater than our value, set/update the value 
        g_costs[current$x, current$y - 1] = g_costs[current$x, current$y] + vroads[current$x, current$y - 1]
      }
    }
    # check left cell (if in bounds)
    if (current$x - 1 >= 1) {
      if(h_costs[current$x -1, current$y] == -1){
        h_costs[current$x -1, current$y] = manhattan(current$x -1, current$y, target$x,target$y)
      }
      if ((g_costs[current$x - 1, current$y] == -1) || (g_costs[current$x - 1, current$y] > g_costs[current$x, current$y] + hroads[current$x - 1, current$y])) {
        # value is unset or value is greater than our value, set/update the value
        g_costs[current$x - 1, current$y] = g_costs[current$x, current$y] + vroads[current$x - 1, current$y]
      }
    }
  }

  #### BEGINNING OF NAIVE IMPLEMENTATION #### 
  # NAIVE IMPLEMENTATION: Does not take cost of roads into account 
  #diff_x = target$x - start$x
  #diff_y = target$y - start$y

  #if (diff_x == 0 && diff_y == 0) {
  #  return(0)
  #}

  #if(abs(diff_x) > abs(diff_y)) {
  #  next_move = ifelse(sign(diff_x) == 1, 6,4)
  #}
  #else {
  #  next_move = ifelse(sign(diff_y) == 1, 8,2)
  #}
  ##### END OF NAIVE IMPLEMENTATION ####

  #print(paste("next_move",next_move))

  #next_move
}