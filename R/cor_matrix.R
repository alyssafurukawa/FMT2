#' Correlation Matrix
#'
#' Quick function for formatted correlation matrix that can be saved as a data frame
#' @param x data frame
#' @export


cor_matrix <- function(x){

  data <- x %>% dplyr::select(where(is.numeric))
  round(cor(data, use= "complete.obs"), 2) %>% as.data.frame()
}
