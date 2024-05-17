#' Obtiene las desviaciones estandars por columnas
#' Convertir a data frame si se obtiene variables categoricas falsamente
#' @param x A vector o dataset
#'
#' @return vector desviaciones estandars por columna
#' @export
#'
#' @examples SD_col(data.frame(x=c(1, 3, 2, 4, 6),y=c("A", "B", "C", "D", "E"),z=c(3, 4, 11, 15, 12)))
SD_col <- function(x){
  vec = c()
  for (i in 1:dim(x)[2]) {
    if(is.numeric(x[ , i])){
      desv = sd(x[ , i],na.rm = F)
      str1 = cat("\ndesv estandar de la columna ",colnames(x[i]))
      str2 = cat(" Es ",desv)
      paste0(str1,str2)
      vec[i] = desv
    }else{
      cat("\nla variable",colnames(x[i]),"es categorica")
    }

  }
  print("\n")
  return(vec)
}
