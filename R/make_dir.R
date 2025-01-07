make_dir <- function(directory, archive = TRUE) {
  if (!file.exists(directory)) {
    dir.create(directory)

    if (archive == TRUE){
      dir.create(paste(directory,"Archive",sep = "/"))

    }
  }
}
