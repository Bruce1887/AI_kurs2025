myFunction <- function(roads, car, packages) { # nolint: object_name_linter.
  if (car$load == 0) {
    print("we have not picked up a package")
  } else {
    print("we have picked up a package")
  }
  
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
  }
  car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  
  car
}

neigbouring_cost <- function(roads, x, y) {
  hroads <- roads$hroads
  vroads <- roads$vroads

  neighbourhood <- list(u=-1, r=-1, d=-1, l=-1)

  # fetch up cell (if in bounds)
  if (y + 1 <= ncol(hroads)) {    # check column bounds
    neighbourhood$u <- hroads[x, y + 1]
  }
  # fetch right cell (if in bounds)
  if (x + 1 <= nrow(hroads)) {    # check row bounds
    neighbourhood$r <- hroads[x + 1, y]
  }
  # fetch down cell (if in bounds)
  if (y - 1 >= 1) {
    neighbourhood$d <- hroads[x, y - 1]
  }
  # fetch left cell (if in bounds)
  if (x - 1 >= 1) {
    neighbourhood$l <- hroads[x - 1, y]
  }

  return(neighbourhood)
}

a_star <- function(start,target,roads) {
  # TODO: implement A star here 
  next_move <- 5
  return next_move
}