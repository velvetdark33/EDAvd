#' elimina columnas con datos no numericos
#' install.packages("dplyr")
#' library(dplyr)
#' @param x A vector o dataset
#' @return data con solo numeric
#' @export
#'
#' @examples delcategoric_col(data.frame(x=c(1, 3, 2, 4, 6),y=c("A", "B", "C", "D", "E"),z=c(3, 4, 11, 15, 12)))
delcategoric_col <- function(x){
  if (!require(dplyr)) {
    stop("dplyr not installed")

  } else {
    newdf <- x %>% select(where(is.numeric))
  }

  return(newdf)
}
