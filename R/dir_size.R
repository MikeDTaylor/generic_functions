#' Calculate Total Directory Size
#'
#' This function calculates the total size of one or more directories, optionally 
#' including subdirectories, and returns the result in the specified units. A 
#' message indicating which units are used is printed exactly once per function call.
#'
#' @param paths A character vector specifying one or more directory paths.
#' @param units A character string indicating the units for the returned size. Must be 
#'   one of \code{"B"}, \code{"KB"}, \code{"MB"}, \code{"GB"}, or \code{"TB"}. 
#'   Defaults to \code{"B"}.
#' @param recursive A logical value indicating whether files should be listed 
#'   recursively. Defaults to \code{FALSE}.
#' @param verbose A logical value indicating whether a text confirmation of the units 
#' used should be output. Defaults to \code{TRUE}.
#'
#' @details 
#' \itemize{
#'   \item The function uses \code{\link[base]{list.files}} to list files in each directory.
#'   \item Total size is computed by summing the byte sizes of all listed files 
#'     (via \code{\link[base]{file.size}}).
#'   \item The result is converted into the specified \code{units} and rounded to 
#'     two decimal places.
#'   \item A single informational message is printed indicating the units used.
#' }
#'
#' @return A numeric vector of the same length as \code{paths}, where each element 
#'   corresponds to the total size of the matching directory in the chosen units.
#'
#' @note
#' If a directory does not exist, or if it contains no files, a value of \code{0} 
#' (zero) will be returned for that directory.
#'
#' @examples
#' \dontrun{
#' # Calculate size in bytes
#' dir_size(".")
#' 
#' # Calculate size in MB for a single directory
#' dir_size(".", units = "MB")
#'
#' # Calculate size in GB for multiple directories
#' dir_size(c("./A", "./B"), units = "GB")
#' list.dirs(".", recursive = FALSE)
#'
#' # Using dir_size in a dplyr pipeline
#' library(dplyr)
#' library(fs)
#' fs::dir_info(".") %>%
#'   filter(type == "directory") %>%
#'   mutate(size_mb = dir_size(path, units = "MB"))
#' }
#'
#' @export

dir_size <- function(paths, 
                     units = "B", 
                     recursive = FALSE,
                     verbose = TRUE) {
  
  # Ensure `paths` is a character vector.
  stopifnot(is.character(paths))
  
  # Match the user-supplied units to one of the allowed values.
  units <- match.arg(units)
  
  # Determine the divisor based on the chosen unit.
  divisor <- switch(
    units,
    "B" = 1,
    "KB" = 1024^1,
    "MB" = 1024^2,
    "GB" = 1024^3,
    "TB" = 1024^4
  )
  
  # For each path, sum the sizes of all files within that path,
  # then convert to the desired units.
  results <- sapply(paths, function(path) {
    
    # Check if the directory exists
    if (!dir.exists(path)) {
      warning(sprintf("Directory '%s' does not exist. Returning NA.", path),
              call. = FALSE)
      return(NA)
    }
    
    # List all files within the directory (recursively if requested).
    files <- list.files(path, full.names = TRUE, recursive = recursive)
    
    # Sum the size of all files in bytes, ignoring any NA values.
    total_bytes <- sum(file.size(files), na.rm = TRUE)
    
    # Convert from bytes to the chosen unit and round to two decimals.
    round(total_bytes / divisor, 2)
  })
  
  # Print output units when verbose = TRUE.
  if (isTRUE(verbose)) {
    if (units == "B") {
      message("Directory size(s) returned in bytes")
    } else {
      message(sprintf("Directory size(s) returned in %s", units))
    }
  }
  
  # Return the numeric vector of results
  results
}

# Single Directory
dir_size("G:/My Drive", units = "GB")

# Multiple directories
dir_size(list.dirs("G:/My Drive", recursive = FALSE), units = "GB")

# Multiple directories within mutate
dir.check <- fs::dir_info("G:/My Drive",
                          recurse = FALSE)%>%
  # filter(type == "directory")%>%
  mutate(path = as.character(path),
         size = dir_size(path, units = "GB", recursive = TRUE))%>%
  arrange(desc(size))%>%
  dplyr::select(1:3)%>%
  glimpse
