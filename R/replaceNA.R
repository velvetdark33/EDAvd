#' reemplaza los NAS
#' si al calcular y como media o mediana obiene error usar por ejemplo:
#' median(as.numeric(unlist(x)) ,na.rm = T)
#' @param x A vector o dataset
#' @param y el valor a reemplazar
#'
#' @return vector con NAS reemplazados
#' @export
#'
#' @examples replaceNA(data.frame(x=c(1, 3, NA, NA, 6),y=c("A", NA, "C", "D", "E"),z=c(NA, TRUE, NA, TRUE, NA)),0)


replaceNA <- function(x,y){
vec_sin_na <- replace(x, is.na(x), y)
return(vec_sin_na)
}
