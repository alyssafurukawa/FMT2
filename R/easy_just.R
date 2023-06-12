#' Place Figure Text
#'
#' Used for positioning trend figure data value text appropriately in figures. A simplified version of vjust_right, which doesn't consider whether figure text may be cut off.
#'
#' @param data data frame
#' @param axis_max maximum of fixed Y axis; default is 100; only used if fixed=T
#' @param axis_min minimum of fixed Y axis; default is 100; only used if fixed=T
#' @param invert use this option if plot shows the data in the opposite order as intended
#' @param upper the percentage of the Y axis that will cut off data text if exceeded; defaults to .9
#' @export

easy_just <- function (data, axis_max=100, axis_min=0, invert=TRUE, fixed= TRUE, upper=1){
  data <- data %>%
    {if ("year" %ni% names(data)) dplyr::rename(.data=., year=period) else .}

  var <- ifelse("Your Hospital" %in% data$cah_state, "Your Hospital", st)

  temp <- data %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(total= sum(est),
                  other= total- est[cah_state==var],
                  dif= est[cah_state==var]-other) %>%
    dplyr::select(-c(total, other))

  direction <- c(max(temp$year):min(temp$year)) %>%
    {if (invert==TRUE) rev(x=.) else .} #some figures need the order of this reversed.. like opip q reports

  positions <- c()

  for (yr in direction){
    yrdata <- temp %>%
      dplyr::filter(year==yr,
                    cah_state==var)

    value <- case_when(
      #fixed==FALSE & yrdata$dif < 0 & max(temp$est[temp$cah_state=="Your Hospital"], na.rm=T) >= 500 & yrdata$est/max(temp$est[temp$cah_state=="Your Hospital"], na.rm=T) <= .1 ~ -.75,
      #for cases with very high value, and then low that would otherwise be placed below.. place above instead
      yrdata$dif < 0 ~ 2,
      #no other case is true, state line is below the national line -- place below
      yrdata$dif >= 0 ~ -.75)
    #no other case is true, state line is above the national line -- place above

    positions <- c(positions, value)
  }

  return(positions)
}
