#' Round data
#'
#' Round number to 2 digits, does not always round 0.5 up
#' @export

round_2dig <- function(x){
  sprintf("%1.2f", as.numeric(x))
}

#' Round data
#'
#' Round number to 1 digit, does not always round 0.5 up
#' @param x data
#' @export

round_1dig <- function(x){
  sprintf("%1.1f", as.numeric(x))
}

#' Round data
#'
#' Round number to 0 digits, does not always round 0.5 up
#' @param x data
#' @export

round_0dig <- function(x){
  sprintf("%1.0f", as.numeric(x))
}

#' Round data
#'
#' Round decimal point number i.e. 0.9 to percentage i.e. 90%, always round 0.5 up
#' @param x data
#' @export

round_perc <- function(x){
  sprintf("%1.0f%%", as.numeric(x)*100)
}

#' Round data
#'
#' Round number to 0 digits, with min suffix, rounds 0.5 up
#' @param x data
#' @export

round_min <-  function(x){
  val= round_half_up(x, digits = 0)
  paste0(sprintf("%1.0f", as.numeric(val)), " min")
}

#' Round data
#'
#' Round decimal point number i.e. 0.9 to percentage i.e. 90%, always rounds 0.5 up
#' @param x data
#' @param digits desired number of digits, defaults to 0
#' @param dec whether number is a decimal and should be multiplied by 100
#' @export

rnd_perc <- function(x, digits=0, dec=TRUE){
  if (dec==TRUE) {val= x*100} else if (dec==FALSE) {val= x}
  num <- round_half_up(val, digits)
  sprintf("%1.0f%%", num)
}

#' Round data
#'
#' Round number to 1 digit, always rounds 0.5 up
#' @param x data
#' @param digits desired number of digits, defaults to 1
#' @export

rndup1 <- function(x, digits=1){
  num= round_half_up(x, digits)
  #sprintf("%1.1f", as.numeric(num))
  sprintf(paste0("%1.", digits, "f"), as.numeric(num))
}

#' Round data
#'
#' Round number to 0 digits, always rounds 0.5 up
#' @param x data
#' @param digits desired number of digits, defaults to 0
#' @param dec whether number is a decimal and should be multiplied by 100
#' @export

#it would make more sense to have mark= F, but would need to make broader changes to files
rndup <- function(x, digits=0, dec= TRUE, mark= TRUE){
  if (dec==TRUE) {val= x*100} else if (dec==FALSE) {val= x}
  num <- round_half_up(val, digits)
  #sprintf("%1.0f%%", num)
  if (mark==TRUE) {sprintf(paste0("%1.", digits, "f%%"), num)} else if (mark==FALSE){sprintf("%1.1f", num)}
}
