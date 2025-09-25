myFunction <- function(roads, car, packages) { # nolint: object_name_linter.
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

a_star <- function(start,target,roads,dim) {
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

  make_node <- function(x,y, next_node) {
    node <- new.env(parent = emptyenv())
    node$x <- x
    node$y <- y
    node$next_node <- next_node
    node
  }

  print_list <- function(head) {
    cat("List: ")
    cur <- head
    while (!is.null(cur)) {
      cat(cur$value, "-> ")
      cur <- cur$next_node
    }
    cat("NULL\n")
  }

  insert_value_at_correct_place <- function(head, x, y, dim, g_costs, h_costs) {
    new_g <- g_costs[x, y]
    new_h <- h_costs[x, y]
    new_f <- new_g + new_h
    print(paste("new",x,y,"[",new_g,",",new_h,",",new_f,"]"))

    if (is.null(head)) {
      message("inserted new head ", x, y, " in empty list")
      return(make_node(x,y,NULL))
    }

    head_g <- g_costs[head$x, head$y]
    head_h <- h_costs[head$x, head$y]
    head_f <- head_g + head_h
    print(paste("head",head$x,head$y,"[",head_g,",",head_h,",",head_f,"]"))
    
    if (new_f < head_f || (new_f == head_f && new_g <= head_g)) {
      new_node <- make_node(x,y, head)
      message("inserted new head ", x, y, " in non-empty list")
      return(new_node)
    }

    current <- head
    while (!is.null(current$next_node)) {
      next_node <- current$next_node
      next_g <- g_costs[next_node$x, next_node$y]
      next_h <- h_costs[next_node$x, next_node$y]
      next_f <- next_g + next_h
      print(paste("next",next_node$x,next_node$y,"[",next_g,",",next_h,",",next_f,"]"))

      if (new_f < next_f || (new_f == next_f && new_g <= next_g)) {
        break
      }
      current <- current$next_node
    }

    message("inserted ", x,y, " after ", current$x,current$y)
    new_node <- make_node(x,y, current$next_node)
    current$next_node <- new_node   # this mutates the original list because nodes are environments
    print_list(head)
    return(head)
  }



  pop_front <- function(head_ref) {
    if (is.null(head_ref$head)) return(NULL)
    old_head <- head_ref$head
    head_ref$head <- head_ref$head$next_node
    old_head
  }

  # Print the list
  print_list <- function(node) {
    print("printing list..")
    while (!is.null(node)) {
      cat(paste(node$x,node$y), " -> ")
      node <- node$next_node
    }
    cat("NULL\n")
  }

  # Create a head reference as environment
  frontier <- new.env()
  frontier$head <- make_node(current$x, current$y, NULL)

  print_list(frontier$head)
  
  next_move <- -1

  print(paste("<current>", current$x,current$y, ", <target>", target$x,target$y))

  temp = 0
  while(!(current$x == target$x && current$y == target$y)) {

    print("")
    print("#### TOP OF LOOP ####")

    first <-pop_front(frontier)
    if(is.null(first)) {
      stop("Could not pop first element (got null)")
    }

    if((current$x == target$x && current$y == target$y))
    {
      print("WE FOUND THE TARGET")
      break
    }


    print(paste("popped:", first$x,first$y))

    current = list(
      x=first$x,
      y=first$y)

    print(paste("current:", current$x,current$y))

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

      print(paste("checking move " ,nx,ny, "from ", current$x,current$y))

      # bounds check
      if (!(nx >= 1 && nx <= nrow(g_costs) && ny >= 1 && ny <= ncol(g_costs))) {
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
      print(paste("move_cost",move_cost))

      new_g <- g_costs[current$x, current$y] + move_cost
      # update g_cost if unset or better path found
      if (g_costs[nx, ny] == -1 || g_costs[nx, ny] > new_g) {
        g_costs[nx, ny] <- new_g
      }

      frontier$head <- insert_value_at_correct_place(frontier$head,nx,ny,dim,g_costs,h_costs)    
    }
    print_list(frontier$head)
    temp = temp+1
    if(temp == 4){
      stop("Det här är bara en stopp för debug-puposes")
    }
  } 

  stop("Lämnade while-loopen")
}