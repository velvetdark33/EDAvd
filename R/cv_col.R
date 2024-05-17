#' Obtiene el coeficiente de variacion por columnas
#' Convertir a data frame si se obtiene variables categoricas falsamente
#' @param x A vector o dataset
#'
#' @return vector coef de variacion por columna
#' @export
#'
#' @examples cv_col(data.frame(x=c(1, 3, 2, 4, 6),y=c("A", "B", "C", "D", "E"),z=c(3, 4, 11, 15, 12)))
cv_col <- function(x){
  vec = c()
  for (i in 1:dim(x)[2]) {
    if(is.numeric(x[ , i])){
      desv = sd(x[ , i],na.rm = F)
      md = mean(x[ , i])
      cv <- 100*desv/md
      str1 = cat("\ncoef de variacion de la columna ",colnames(x[i]))
      str2 = cat(" Es ",round(cv,1) ,"%")
      paste0(str1,str2)
      vec[i] = cv
    }else{
      cat("\nla variable",colnames(x[i]),"es categorica")
    }

  }
  print("\n")
  return(vec)
}
