#' Load and/or Install Packages
#'
#' Attempts to load the specified packages; if any are missing either installs or 
#' prompts the user to optionally install them (Windows Only).  If the user chooses
#' "NO" the function will not install missing packages, and the missing packages
#' remain unloaded.
#'
#' @param ... One or more character strings naming the packages to load/install.
#' @param ask Logical; if \code{TRUE}, prompts the user for installation of missing
#'   packages (Windows Only). If \code{FALSE} (default), installs missing packages 
#'   automatically.
#'
#' @note
#' \strong{Important:} Because this function can install packages on-the-fly
#' interactively, it's generally discouraged to use such code in non-interactive 
#' or production contexts. Consider having a structured setup process or a 
#' renv/packrat setup for automated dependency management.
#'
#' @examples
#' \dontrun{
#' # Attempt to load or install dplyr (and possibly ggplot2) automatically
#' lib_loadr("dplyr", "ggplot2")
#'
#' # Prompt the user via a dialog on Windows
#' lib_loadr("readr", ask = TRUE)
#'
#' # Attempt to load a non-existent package "notapkg" and see the warning
#' lib_loadr("dplyr", "notapkg")
#' }
#'
#' @export
lib_loadr <- function(..., ask = FALSE) {
  
  # Gather all package names into a character vector
  libs <- c(...)
  
  # If user supplied no packages, do nothing
  if (length(libs) == 0) {
    stop("No packages specified.")
  }
  
  # Attempt to require() each package, returning TRUE if loaded, FALSE if not
  req <- vapply(libs, require, FUN.VALUE = logical(1), character.only = TRUE, quietly = TRUE)
  # req <- unlist(lapply(libs, require, character.only = TRUE))
  
  # Identify which packages were NOT successfully loaded
  need <- libs[req == FALSE]
  
  # Count how many packages are missing
  n <- length(need)
  
  # Load missing packages or inform user that all packages are installed/loaded
  if (n > 0) {
    
    # If ask == TRUE and OS is not windows change to ask == FALSE 
    if (isTRUE(ask) && .Platform$OS.type != "windows") { 
      ask <- FALSE
      warning("use_lib(.., ask = TRUE) only works on Windows, defaulting to FALSE")
    }
    
    # Check packages are all on CRAN
    # Fetch the names of all available packages on CRAN
    cran_pkgs <- rownames(utils::available.packages())
    
    # Packages that do not exist on CRAN
    na_cran <- need[!need %in% cran_pkgs]
    
    # If any missing packages aren't found on CRAN, warn and remove them from the install list
    if (length(na_cran) > 0) {
      warning(
        "The following package name(s) were not found on CRAN: ",
        toString(na_cran),
        "\nRemoving from install list."
      )
      # Filter those out from the to-install list
      need <- setdiff(need, na_cran)
      
      # Check that we still have packages to install
      if (length(need) == 0) {
        message("No valid packages remain to install (none found on CRAN).")
        return(invisible(NULL))
      }
    }
    
    # Message for Ask dialog
    dialogue <- paste(
      "Missing Packages: ",
      toString(need),
      "\n\n",
      "Install all?",
      sep = ""
    )
    
    # Install missing packages if ask = FALSE otherwise ask whether to install
    if (!ask) {
      # Install the missing packages
      install.packages(need)
      # Then load them again
      lapply(need, require, character.only = TRUE)
      
      # Print message
      message(paste0("Missing packages installed: ", toString(need), "\n",
                     "All packages loaded."))
      return(invisible(NULL))
      
      } else if (winDialog(type = "yesno", dialogue) == "YES") {
        # Install the missing packages
        install.packages(need)
        # Then load them again
        lapply(need, require, character.only = TRUE)
        return(invisible(NULL))
        
        } else {
          message(paste0("Missing packages not installed:\n",
                         toString(need), "\n",
                     "All other packages loaded."))
          return(invisible(NULL))
          }
    
    } else {
    message("All requested packages are already installed and loaded.")
    return(invisible(NULL))
  }
}

