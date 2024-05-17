#' elimina columnas con datos no numericos
#'
#' @param x A vector o column of dataset
#' @return data con solo numeric
#' @export
#'
#' @examples removeoutlier_col(data.frame (y = c (5, 8, 8, 12, 14, 15, 16, 19, 20, 22, 24, 25, 25, 26, 30, 48))
removeoutlier_col <- function(x){
      Q <- quantile(column, probs = c(0, 0.25, 0.5, 0.75, 1) ,type =7, na.rm = T)
      x<-x[column>(Q[2]-1.5*(Q[4]-Q[2])) & column < (Q[4]+1.5*(Q[4]-Q[2])) , ]
}
