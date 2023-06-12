#' Adjust quarters
#'
#' Used to adjust quarters/periods for MOU lists. Very simple
#' @param period period, i.e. 2021Q1
#' @param move the number of periods back from current
#' @export

move_back <- function(period, move){
  year <- as.numeric(str_sub(period, end=-3))
  q <- as.numeric(str_sub(quarter, 6))
  year_edit <- ifelse(q==1, year-1, year)
  drop <- as.numeric(q)-as.numeric(move)
  temp <- dplyr::case_when(
    drop==0 ~ 4,
    drop==-1 ~ 3,
    drop==-2 ~ 2,
    TRUE ~ drop)

  output <- as.numeric(paste0(year_edit, temp))

  return(output)
}

#move_back <- function(value, move){
#  drop <- as.numeric(value)-as.numeric(move)
#  temp <- dplyr::case_when(
#    drop==0 ~ 4,
#    drop==-1 ~ 3,
#    drop==-2 ~ 2,
#    TRUE ~ drop)
#
#  return(temp)
#}

