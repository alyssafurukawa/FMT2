#' Scatterplot Matrix
#'
#' Function for a better looking/more functional scatterplot matrix than default pairs function and quicker than specifying with GGally
#' @param x data frame
#' @param col need to specify col as part of dataframe i.e. penguins$species-- this will provide some interesting feedback
#' @export

scat_matrix <- function(x, col=NULL){
  if (is.null(col)){
    smatrix <- GGally::ggpairs(x) + theme_nate()
    print(smatrix)}

  else{
    smatrix <- GGally::ggpairs(x, ggplot2::aes(color=col)) + theme_nate()
    print(smatrix)
  }
}
