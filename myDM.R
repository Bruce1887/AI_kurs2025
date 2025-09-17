myFunction <- function(roads, car, packages) { # nolint: object_name_linter.
  print(paste("nrow(roads$hroads):", nrow(roads$hroads), "ncol(roads$hroads):", ncol(roads$hroads)))
  print(paste("nrow(roads$vroads):", nrow(roads$vroads), "ncol(roads$vroads):", ncol(roads$vroads)))

  if (car$load == 0) {
    print("we have not picked up a package")
  } else {
    print("we have picked up a package")
  }
  

  hroads <- roads$hroads   # matrix
  vroads <- roads$vroads   # matrix
  cx = car$x
  cy = car$y



  print(paste("car pos: ", cx, " ", cy))
  print(paste("hroads[", cy, ",", cx, "] :", hroads[cy,cx]))
  print(paste("vroads[", cy, ",", cx, "] :", vroads[cy,cx]))

  # for (r in seq_len(nrow(hroads))) {
  #   for (c in seq_len(ncol(hroads))) {
  #     # Do something with hroads[r, c]
  #     print(paste("c:", c, "r:", r, "cost:", hroads[r, c]))
  #   }
  # }


  # for(c in neigbouring_cost(roads,cx,cy)){
  #   print(c)
  # }

  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
  }
  car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  

  car
}

neigbouring_cost <- function(roads,x,y,dimx,dimy) {
  neighbourhood = list (u=-1,ur=-1,r=-1,dr=-1,d=-1,dl=-1,l=-1,ul=-1)
  # fetch up cell (if in bounds)
  if(y +1 < dimy) {
    neighbourhood$u = roads[x,y+1]
  }# fetch up-right cell (if in bounds)
  if(x -1 > 0) {
    
  }# fetch right cell (if in bounds)
  if(x -1 > 0) {
    
  }# fetch down-right cell (if in bounds)
  if(x -1 > 0) {
    
  }# fetch down cell (if in bounds)
  if(x -1 > 0) {
    
  }# fetch down-left cell (if in bounds)
  if(x -1 > 0) {
    
  }# fetch left cell (if in bounds)
  if(x -1 > 0) {
    
  }
  # fetch up-left cell (if in bounds)
  if(x -1 > 0) {

  }
  neighbourhood
} 