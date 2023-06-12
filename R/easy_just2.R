#' Place Figure Text
#'
#' Used for positioning trend figure data value text appropriately in figures. This takes the state line into account for CAH figures.
#' @param data data frame
#' @param fixed whether the plot's Y axis is fixed; default is FALSE
#' @param axis_max maximum of fixed Y axis; default is 100; only used if fixed=T
#' @param axis_min minimum of fixed Y axis; default is 100; only used if fixed=T
#' @param invert use this option if plot shows the data in the opposite order as intended
#' @export

easy_just2 <- function (data, axis_max=100, axis_min=0, invert=FALSE){

data <- data %>%
  {if ("year" %ni% names(data)) dplyr::rename(.data=., year=period) else .}

temp <- data %>%
  dplyr::mutate(cah_state= dplyr::case_when(
    cah_state=="U.S." ~ "nat",
    cah_state==st ~ "sta",
    cah_state=="Your Hospital" ~ "hosp")) %>%
  tidyr::pivot_wider(id_cols= year, names_from= cah_state, values_from= est) %>%
  dplyr::mutate(ndif= hosp-nat,
         sdif= hosp-sta) %>%
  dplyr::select(-c(nat, sta))

  direction <- c(max(temp$year):min(temp$year)) %>%
    {if (invert==TRUE) rev(x=.) else .} #some figures need the order of this reversed.. like opip q reports

  positions <- c()

  #NEED TO ADD CODE FOR WHEN STATE LINE IS MISSING
  for (yr in direction){
      #for (yr in max(temp$year):min(temp$year)){
      yrdata <- temp %>%
        dplyr::filter(year==yr)

      value <- case_when(
        max(temp$hosp, na.rm = T) > 250 & yrdata$hosp < .25*max(temp$hosp, na.rm = T) ~ -.75, #very high and low value, print above
        yrdata$ndif < 0 & yrdata$sdif < 0 ~ 2, #both lines above hosp -> below
        yrdata$ndif >= 0 & yrdata$sdif >= 0 & yrdata$hosp==499 ~ 2, #both lines below hosp, but outlier obs -> below
        yrdata$ndif >= 0 & yrdata$sdif >= 0 ~ -.75, #both lines below hosp -> above
        yrdata$ndif==0 & yrdata$sdif==0 ~ -.75, #all lines are equal -> above
        #overall logic, place closer to further line and closer to hosp line
        yrdata$ndif >= 0 & yrdata$sdif < 0 & abs(yrdata$ndif) <= abs(yrdata$sdif) ~ -.75, #ndif below/sdif above, bigger space above -> above
        yrdata$ndif < 0 & yrdata$sdif >= 0 & abs(yrdata$ndif) >= abs(yrdata$sdif) ~ -.75, #ndif above/sdif below, bigger space above -> above
        yrdata$ndif >= 0 & yrdata$sdif < 0 & abs(yrdata$ndif) > abs(yrdata$sdif) ~ 2,#ndif below/sdif above, bigger space below -> below
        yrdata$ndif < 0 & yrdata$sdif >= 0 & abs(yrdata$ndif) < abs(yrdata$sdif) ~ 2)#ndif above/sdif below, bigger space below -> below

      positions <- c(positions, value)
    }

  return(positions)
}
