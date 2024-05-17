#' Obtiene diagramas de caja de cada columna
#'
#' @param x A vector o dataset
#' @param y A logical if is TRUE elimina outliers
#' @return histogramas
#' @export
#'
#' @examples boxplot_col(data.frame(x=c(1, 3, 2, 4, 6),y=c("A", "B", "C", "D", "E"),z=c(3, 4, 11, 15, 12)),F)
boxplot_col <- function(x,y){

  for (i in 1:dim(x)[2]) {
    if(is.numeric(x[ , i])){
      if(y == T){
        #par(mfrow = c(1, 2))
        column <- x[i]
        column <- as.data.frame(column)
        st1 = cat("",colnames(column),"f")
        # Calculamos el diagrama de caja
        g_caja<-boxplot(column ,main = st1, col="skyblue")
        Q <- quantile(column, probs = c(0, 0.25, 0.5, 0.75, 1) ,type =7, na.rm = T)
        x<-x[column>(Q[2]-1.5*(Q[4]-Q[2])) & column < (Q[4]+1.5*(Q[4]-Q[2])) , ]
        boxplot(x[i],main = st1, col="skyblue")

      }else{
        column <- x[i]
        column <- as.data.frame(column)
        st1 = cat("",colnames(column),"f")
        # Calculamos el diagrama de caja
        g_caja<-boxplot(column ,main = st1, col="skyblue")
      }
    }
  }


}
