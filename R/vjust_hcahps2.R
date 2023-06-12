#' Place Figure Text
#'
#' Used for positioning trend figure data value text appropriately in figures. This takes the state line into account for CAH figures.
#' @param data data frame
#' @param axis_max maximum of fixed Y axis; default is 100; only used if fixed=T
#' @param axis_min minimum of fixed Y axis; default is 100; only used if fixed=T
#' @param invert use this option if plot shows the data in the opposite order as intended
#' @param upper the value past which all text gets placed below the line
#' @export


vjust_hcahps2 <- function (data, axis_max=100, axis_min=0, invert=FALSE, upper=.925){
  data <- data %>%
    {if ("year" %ni% names(data)) rename(.data=., year=period) else .}

  difs <- tibble()
  for (q in unique(data$question_id)){
    temp <- data %>%
      dplyr::mutate(cah_state= dplyr::case_when(
        cah_state=="U.S." ~ "nat",
        cah_state==st ~ "sta",
        cah_state=="Your Hospital" ~ "hosp")) %>%
      tidyr::pivot_wider(id_cols= c(year, question_id), names_from= cah_state, values_from= est) %>%
      dplyr::mutate(ndif= hosp-nat,
                    sdif= hosp-sta)

    difs <- bind_rows(difs, temp)}

  difs <- difs %>% distinct()

  direction <- c(max(difs$year):min(difs$year)) %>%
    {if (invert==TRUE) rev(x=.) else .} #some figures need the order of this reversed.. like opip q reports

  positions <- c()
  for (yr in direction){
    for (q in unique(difs$question_id)){
      yrdata <- difs %>%
        filter(year==yr,
               question_id==q)

      #for this one, the opposite logic is preferred to the other reports.
      #when between lines, between the print toward the closest one because the lines are typically so close it won't cause an issue
      #because they're so close, the furthest line may sometimes overlap.. so printing toward the closest is preferred
      #could always do an if statement that would change this if the difference is greater than a certain number, but those cases are rare
      #I removed this for now.. still functions like the others at the moment
      value <- case_when(
        yrdata$hosp > upper*axis_max ~ 2, #too high, might get cut off so -> below
        yrdata$hosp < .2*axis_max+axis_min ~ -.75, #too low, might get cut off so -> above; used to be .1, is the new value optimal?
        yrdata$ndif < 0 & yrdata$sdif < 0 ~ 2, #both lines above hosp -> below
        yrdata$ndif >= 0 & yrdata$sdif >= 0 ~ -.75, #both lines below hosp -> above
        yrdata$ndif==0 & yrdata$sdif==0 ~ -.75, #all lines are equal -> above
        yrdata$ndif==0 & yrdata$sdif < 0 ~ 2, #nat=hosp/sdif above -> below
        yrdata$ndif==0 & yrdata$sdif > 0 ~ -.75, #nat=hosp/sdif below -> above
        yrdata$sdif==0 & yrdata$ndif < 0 ~ 2, #st=hosp/ndif above -> below
        yrdata$sdif==0 & yrdata$ndif > 0 ~ -.75, #st=hosp/ndif below -> above
        yrdata$ndif > 0 & yrdata$sdif < 0 & abs(yrdata$ndif) <= abs(yrdata$sdif) ~ -.75, #ndif below/sdif above, bigger space above -> above
        yrdata$ndif < 0 & yrdata$sdif > 0 & abs(yrdata$ndif) >= abs(yrdata$sdif) ~ -.75, #ndif above/sdif below, bigger space above -> above
        yrdata$ndif > 0 & yrdata$sdif < 0 & abs(yrdata$ndif) > abs(yrdata$sdif) ~ 2, #ndif below/sdif above, bigger space below -> below
        yrdata$ndif < 0 & yrdata$sdif > 0 & abs(yrdata$ndif) < abs(yrdata$sdif) ~ 2) #ndif above/sdif below, bigger space below -> below
        #yrdata$ndif > 0 & yrdata$sdif < 0 & abs(yrdata$ndif) <= abs(yrdata$sdif) ~ -.2, #ndif below/sdif above, bigger space above -> below
        #yrdata$ndif < 0 & yrdata$sdif > 0 & abs(yrdata$ndif) >= abs(yrdata$sdif) ~ -.2, #ndif above/sdif below, bigger space above -> below
        #yrdata$ndif > 0 & yrdata$sdif < 0 & abs(yrdata$ndif) > abs(yrdata$sdif) ~ -.75, #ndif below/sdif above, bigger space below -> above
        #yrdata$ndif < 0 & yrdata$sdif > 0 & abs(yrdata$ndif) < abs(yrdata$sdif) ~ -.75) #ndif above/sdif below, bigger space below -> above

      positions <- c(positions, value)

    }
  }

  return(positions)
}
