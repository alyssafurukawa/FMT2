#' Move legend to avoid overlapping data points
#'
#' Used as argument to legend.position to move the legend to bottom when high data values would overlap it.
#' @param x plot data
#' @param m the measure included in the plot i.e. OP-2
#' @param limit the highest data value that won't move the legend
#' @param points the number of points on the x axis. Currently includes options for 2-4.
#' @export

dodge_legend <- function(x, m, limit, points){

  filtered <- x %>%
    dplyr::filter(cah_state != "U.S.", measure==m) %>%
    dplyr::mutate(est= ifelse(is.na(est), 0, est)) #NA values are annoying with T/F

  if (points==4){
    pos <- ifelse(rep(dplyr::filter(filtered, period==3)$est > limit| dplyr::filter(filtered, period==2)$est > limit, 2), c(.5, .1), c(.5, 1))
  }

  else if (points==3){
    pos <- ifelse(rep(dplyr::filter(filtered, period==2)$est > limit, 2), c(.5, .1), c(.5, 1))
  }

  else if (points==2){
    pos <- ifelse(rep(dplyr::filter(filtered, period==2)$est > limit & dplyr::filter(filtered, period==1)$est > limit, 2), c(.5, .1), c(.5, 1))
  }

  else{
    stop("Figure must have 2-4 data points")
  }
  return(pos)
}
