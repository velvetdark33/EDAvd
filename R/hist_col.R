#' Obtiene los histograma de cada columna
#' Convertir a data frame si se obtiene variables categoricas falsamente
#' @param x A vector o dataset
#' @param y A logical if is TRUE muestra graficos de densidad
#' @return histogramas
#' @export
#'
#' @examples hist_col(data.frame(x=c(1, 3, 2, 4, 6),y=c("A", "B", "C", "D", "E"),z=c(3, 4, 11, 15, 12)),F)
hist_col <- function(x,y){

nam  for (i in 1:dim(x)[2]) {
    if(is.numeric(x[ , i])){

      if(y == T){
        par(mfrow = c(1, 2))

        str1 = cat("\nhistograma de la columna num ",colnames(x[i]))
        hist(x[ , i],main = paste("Histograma de" , colnames(x[i])))
        # Calculamos la densidad
        dx <- density(x[ , i])

        # Añadimos la línea de densidad
        lines(dx, lwd = 2, col = "red")

        # Curva de densidad sin histograma
        plot(dx, lwd = 2, col = "red",
             main = "Densidad")

      }else{
        md = mean(x[ , i])
        str1 = cat("\nhistograma de la columna num ",colnames(x[i]))
        hist(x[ , i],main = paste("Histograma de" , colnames(x[i])))
      }
    }else{
      cat("\nhistograma de la columna categ ",colnames(x[i]),"\n")
      table <- table(x[ , i])
      barplot(table,main = paste("diagrama barras de" , colnames(x[i])))

    }
  }


}
