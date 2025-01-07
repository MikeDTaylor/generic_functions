#' Create a Directory with Optional Archive Subdirectory
#'
#' This function checks if a specified directory exists. If it does not, the function
#' creates the directory. Optionally, it can also create an "Archive" subdirectory within
#' the main directory.
#'
#' @param directory Character string specifying the path of the directory to create.
#' @param archive Logical value indicating whether to create an "Archive" subdirectory.
#'                Defaults to \code{FALSE}.
#'
#' @return Invisibly returns \code{TRUE} if the directory (and optional archive subdirectory)
#'         is successfully created or already exists. Throws an error otherwise.
#'
#' @examples
#' \dontrun{
#' # Create a directory named "Data"
#' make_dir("Data")
#'
#' # Create a directory named "Results" with an archive subdirectory
#' make_dir("Results", archive = TRUE)
#' }
#'
#' @export
make_dir <- function(directory, archive = FALSE) {

  # Check if the directory already exists
  if (!dir.exists(directory)) {
    # Attempt to create the directory
    success_main <- dir.create(directory, recursive = TRUE)

    # If directory creation fails, stop and return an error
    if (!success_main) {
      stop(paste("Failed to create directory:", directory))
    }

    # If archive is TRUE, attempt to create the "Archive" subdirectory
    if (archive) {
      archive_dir <- file.path(directory, "Archive")
      success_archive <- dir.create(archive_dir, recursive = TRUE)

      # If archive directory creation fails, stop and return an error
      if (!success_archive) {
        stop(paste("Failed to create archive directory:", archive_dir))
      }
    }
  } else {
    message(paste("Directory already exists:", directory))
    if (archive) {
      archive_dir <- file.path(directory, "Archive")
      if (!dir.exists(archive_dir)) {
        # Attempt to create the archive directory if it doesn't exist
        success_archive <- dir.create(archive_dir, recursive = TRUE)

        if (!success_archive) {
          stop(paste("Failed to create archive directory:", archive_dir))
        }
      } else {
        message(paste("Archive directory already exists:", archive_dir))
      }
    }
  }

  # Invisibly return TRUE to indicate successful execution
  invisible(TRUE)
}

