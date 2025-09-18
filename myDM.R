myFunction <- function(roads, car, packages) { # nolint: object_name_linter.
  hroads <- roads$hroads # matrix
  vroads <- roads$vroads # matrix

  print(paste("car pos: ", car$x, " ", car$y))


  nbh <- neigbouring_cost(roads,car$x,car$y)
  print(paste("neighbour up:",nbh$u))
  print(paste("neighbour right:",nbh$r))
  print(paste("neighbour down:",nbh$d))
  print(paste("neighbour left:",nbh$l))


  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
    # FLytta postgubben med A_star algoritmen när du har ett paket
    car$nextMove = a_star(list(x=car$x,y=car$y), list(x=packages[car$load,3],y=packages[car$load,4]),roads)
  }
  else {
    # TODO: Flytta postgubben automatiskt (inte manuellt) när du inte har ett paket 
    car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
    if (car$nextMove=="q") {stop("Game terminated on user request.")}
  }
  
  car
}

neigbouring_cost <- function(roads, x, y) {
  hroads <- roads$hroads
  vroads <- roads$vroads

  neighbourhood <- list(u=-1, r=-1, d=-1, l=-1)

  # fetch up cell (if in bounds)
  if (y + 1 <= ncol(vroads)) {    # check column bounds
    neighbourhood$u <- vroads[x, y + 1]
  }
  # fetch right cell (if in bounds)
  if (x + 1 <= nrow(hroads)) {    # check row bounds
    neighbourhood$r <- hroads[x + 1, y]
  }
  # fetch down cell (if in bounds)
  if (y - 1 >= 1) {
    neighbourhood$d <- vroads[x, y - 1]
  }
  # fetch left cell (if in bounds)
  if (x - 1 >= 1) {
    neighbourhood$l <- hroads[x - 1, y]
  }

  return(neighbourhood)
}

a_star <- function(start,target,roads) {
  # TODO: implement A star here 
  # F cost = G cost (distance from start) + H cost (distance from end)
  
  # Computes the heuristic cost (h)
  manhattan <- function(a, b){
    return (abs(a[1]-b[1]) + abs(a[2]-b[2]))
  } 

  frontier <- list(list(
    x=start[1], y=start[2], g=0,
    h=manhattan(start, target),
    f=manhattan(start, target),
    first_move=NA))
    

  next_move <- -1

  #### BEGINNING OF NAIVE IMPLEMENTATION #### 
  # NAIVE IMPLEMENTATION: Does not take cost of roads into account 
  diff_x = target$x - start$x
  diff_y = target$y - start$y

  if (diff_x == 0 && diff_y == 0) {
    return(0)
  }

  if(abs(diff_x) > abs(diff_y)) {
    next_move = ifelse(sign(diff_x) == 1, 6,4)
  }
  else {
    next_move = ifelse(sign(diff_y) == 1, 8,2)
  }
  #### END OF NAIVE IMPLEMENTATION ####

  print(paste("next_move",next_move))

  next_move
}