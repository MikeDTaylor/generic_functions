# See normalise.R for guide when updating

# Min-max normalise a raster (r), based on the minmum values of a variable in x
mm.rast.normalise <- function(r, x, ...) {
  min_x <- min(x, ...)
  max_x <- max(x, ...)

  return((r - min_x) /(max_x - min_x))
}
