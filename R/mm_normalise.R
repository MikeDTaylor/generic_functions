#' Min-Max Normalisation
#'
#' This function performs a min-max normalisation (feature scaling), transforming
#' a numeric vector \eqn{x} into a \eqn{[0, 1]} range.
#'
#' @param x A numeric vector.
#' @param attr Logical indicating whether to attach \code{"raw.min"} and 
#'   \code{"raw.max"} attributes to the returned object.
#' @param ... Additional arguments passed to \code{\link[base]{min}} and
#'   \code{\link[base]{max}}, e.g., \code{na.rm = TRUE}.
#'
#' @details
#' If \code{x} has all identical values, the function returns a zero vector and
#' issues a warning.
#'
#' When \code{attr} is \code{TRUE}, the returned numeric vector will have 
#' attributes \code{"raw.min"} and \code{"raw.max"} set to the original 
#' minimum and maximum of \code{x}.
#'
#' @return A numeric vector of the same length as \code{x}, scaled to 
#'   \eqn{[0, 1]}. If \code{attr = TRUE}, the result has additional attributes:
#'   \code{"raw.min"} and \code{"raw.max"}.
#'
#' @examples
#' x <- c(10, 20, 30, 40)
#' norm_x <- mm_normalise(x)
#' norm_x
#' attributes(norm_x)
#'
#' # With NA values
#' x_na <- c(10, NA, 30, 40)
#' mm_normalise(x_na, na.rm = TRUE)
#'
#' @export
mm_normalise <- function(x, attr = TRUE, ...) {
  
  # Input validation
  if (!is.numeric(x)) {
    stop("Argument 'x' must be a numeric vector.")
  }
  
  x_min <- min(x, ...)
  x_max <- max(x, ...)
    
  # Handle the case where all values in x are identical
  if (x_max == x_min) {
    warning("All values in 'x' are identical. Returning a vector of 0s.")
    return(rep(0, length(x)))
  }
  
  normalised <- (x - x_min) / (x_max - x_min)
    
  
  # Attach attributes if required
  if (isTRUE(attr)){
    attr(normalised, "raw.min") <- x_min
    attr(normalised, "raw.max") <- x_max
  }
  
  return(normalised)
}

#' Min-Max De-Normalisation
#'
#' This function reverses min-max normalisation, transforming a \eqn{[0, 1]} 
#' scaled numeric vector back into its original range.
#'
#' @param x A numeric vector, presumably one that was originally produced 
#'   by \code{\link{mm_normalise}}. If it has attributes \code{"raw.min"} 
#'   and \code{"raw.max"}, those will be used unless an explicit \code{raw} 
#'   is provided and matches.
#' @param raw Optional. A numeric vector with the same original range as \code{x}
#'   before normalisation. If provided, \code{min(raw)} and \code{max(raw)} 
#'   will be used instead of the attributes in \code{x}, unless they match 
#'   exactly. See Details.
#' @param ... Additional arguments passed to \code{\link[base]{min}} and
#'   \code{\link[base]{max}}, e.g., \code{na.rm = TRUE}.
#'
#' @details
#' If both the attributes \code{"raw.min"}, \code{"raw.max"} are present 
#' in \code{x} and a \code{raw} argument is provided:
#' \itemize{
#'   \item If \code{min(raw)} and \code{max(raw)} match the attributes, 
#'   a warning is issued and the function proceeds using the attributes (i.e., 
#'   \code{raw} is ignored).
#'   \item If they do not match, an error is thrown. 
#' }
#'
#' If the original data all had the same value (i.e., \code{raw.min} 
#' == \code{raw.max}), the function returns a vector of that identical value 
#' and warns.
#'
#' @return A numeric vector with the same length as \code{x}, but mapped back 
#'   to the range \code{c(raw.min, raw.max)} either from attributes or the 
#'   user-supplied \code{raw}.
#'
#' @examples
#' # Normalise and then de-normalise using attributes
#' x <- c(10, 20, 30, 40)
#' norm_x <- mm_normalise(x)
#' denorm_x <- mm_denormalise(norm_x)
#' denorm_x
#'
#' # Normalise and then de-normalise using raw argument
#' norm_x_no_attr <- mm_normalise(x, attr = FALSE)
#' denorm_x2 <- mm_denormalise(norm_x_no_attr, raw = x)
#' denorm_x2
#'
#' @export
mm_denormalise <- function(x, raw = NULL, ...) {
  
  # Confirm either the presence of attributes or a raw vector
  if(any(all(c("raw.min", "raw.max") %in% names(attributes(x))),
         !is.null(raw))) {
    stop("Function requires either the attributes 'raw.min' and 'raw.max' or a numeric vector 'raw'.")
  }
  
  # Detect attributes
  if (all(c("raw.min", "raw.max") %in% names(attributes(x)))) {
    attr.detected <- TRUE
    
    # Check that attributes are numeric
    if (any(!is.numeric(attributes(x)$raw.min),!is.numeric(attributes(x)$raw.min))) {
      stop("Attributes 'raw.min' and 'raw.max' must be numeric vectors")
    }
  }
  
  # Check if raw is numeric
  if (!is.null(raw) && !is.numeric(raw)) {
    stop("Argument 'raw' must be a numeric vector if provided.")
  }
  
  # Check if attributes are detected and raw is provided
  # If yes and the values match make raw <- NULL, otherwise error
  if (isTRUE(attr.detected) && !is.null(raw)) {
    if (all(attributes(x)$raw.min == min(raw, na.rm = TRUE),
            attributes(x)$raw.max == max(raw, na.rm = TRUE))) {
      raw <- NULL
      warning("Argument 'raw' provided and attributes detected (not recommended), defaulting to raw.min and raw.max from attributes")
    } else {
      stop("Argument 'raw' provided and attributes detected (not recommended). Min and max of argument 'raw' are different to raw.min and raw.max in attributes. Either remove 'raw' and use provided attributes (recommended) or remove attributes.")
    }
  }
  
  # Extract min/max value
  if (is.null(raw)) {
    raw_min <- attributes(x)$raw.min
    raw_max <- attributes(x)$raw.max
    attributes(x)$raw.min <- NULL
    attributes(x)$raw.max <- NULL
    
  } else {
    
    raw_min <- min(raw, ...)
    raw_max <- max(raw, ...)
    
  }
  
  # Handle the case where raw_min == raw_max
  if (raw_max == raw_min) {
    warning("Minimum and maximum of 'raw' are identical. Returning a vector of 'raw_min'.")
    return(rep(raw_min, length(x)))
  }
  
  # denormalise data
  denormalised <- (x * (raw_max - raw_min)) + raw_min
  return(denormalised)
  
}

