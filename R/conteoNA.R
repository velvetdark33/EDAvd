#' Cuenta los NAS
#'
#' @param x A vector o dataset
#'
#' @return Total NAS por columna
#' @export
#'
#' @examples conteo_NAS(data.frame(x=c(1, 3, NA, NA, 6),y=c("A", NA, "C", "D", "E"),z=c(NA, TRUE, NA, TRUE, NA)))


conteo_NAS <- function(x){
  t_na = sum(is.na(x))
  cat("el total de NAS es ",t_na )
  vec = c()
  for (i in 1:dim(x)[2]) {
    s1= sum(is.na(x[,i]))
    str1 = cat("\nEl numero de NAS de la columna ",colnames(x[i]))
    str2 = cat(" Es ",s1)
    str3 = cat(" corresponde al",round(100*s1/(dim(x)[1]),2),"%")
    #paste0(str1,str2)
    vec[i] = s1
  }
  print("\n")
  return(vec)
}
