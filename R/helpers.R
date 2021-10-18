#' Function to "get" triangle vertices from vectors of hex coords
get_tri <- function(x, y, cenx, ceny) {
  tx <- c(rep(x, times = c(1, 2, 2, 2, 2, 2)), x[1])
  tx <- matrix(tx, nrow = 2)
  tx <- rbind(tx, cenx)
  ty <- c(rep(y, times = c(1, 2, 2, 2, 2, 2)), y[1])
  ty <- matrix(ty, nrow = 2)
  ty <- rbind(ty, ceny)
  return(data.frame(x = c(tx), y = c(ty)))
}
