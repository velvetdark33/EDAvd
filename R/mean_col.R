#' Obtiene las medias por columnas
#' Convertir a data frame si se obtiene variables categoricas falsamente
#' @param x A vector o dataset
#'
#' @return vector medias por columna
#' @export
#'
#' @examples mean_col(data.frame(x=c(1, 3, 2, 4, 6),y=c("A", "B", "C", "D", "E"),z=c(3, 4, 11, 15, 12)))
mean_col <- function(x){
  vec = c()
  for (i in 1:dim(x)[2]) {
    if(is.numeric(x[ , i])){
      md = mean(x[ , i])
      str1 = cat("\nmedia de la columna ",colnames(x[i]))
      str2 = cat(" Es ",md)
      paste0(str1,str2)
      vec[i] = md
    }else{
      cat("\nla variable",colnames(x[i]),"es categorica")
    }
  }
  print("\n")
  return(vec)
}
