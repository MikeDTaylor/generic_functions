#' Convert continuouse SpatRaster values to Categorical
#'
#' This function takes converts continous values in a SpatialRaster to categorical
#'
#' @param spat A spatRast with continuous values
#'
#' @param df A dataframe with
#'
#' @return A spatRast with categorical values
#'

make_cat <- function(spat, df,
                     name = NULL,
                     verbose = FALSE

) {
  if (ncol(df)!=2) {
    new <- LETTERS[seq(from = 1, to = length(df[1]))]
    df <- cbind(df, "new"=LETTERS[seq(from = 1, to = nrow(df[1]))])
    warning("No 'second' column, defaulting to letters and generic title")
  }

  if (!is.factor(spat)) {
    levels(spat) <- df
    names(spat) <- colnames(df[2])
  }

  if(!is.null(name)) {

    names(spat) <- name

  } else {

    names(spat) <- colnames(df[2])

  }

  if (verbose == TRUE){

    print(cats(spat))

    }

  spat

}
