get_col_letters <- function(col_num) {
  letters <- ""
  while (col_num > 0) {
    mod <- (col_num - 1) %% 26
    letters <- paste0(LETTERS[mod + 1], letters)
    col_num <- (col_num - mod - 1) %/% 26
  }
  return(letters)
}