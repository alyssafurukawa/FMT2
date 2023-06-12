#' Place Figure Text
#'
#' Used for positioning trend figure data value text appropriately in figures
#' @param data data frame
#' @param fixed whether the plot's Y axis is fixed; default is FALSE
#' @param axis_max maximum of fixed Y axis; default is 100; only used if fixed=T
#' @param axis_min minimum of fixed Y axis; default is 100; only used if fixed=T
#' @param invert use this option if plot shows the data in the opposite order as intended
#' @param upper the percentage of the Y axis that will cut off data text if exceeded; defaults to .9
#' @export

vjust_right <- function (data, fixed=FALSE, axis_max=100, axis_min=0, invert=FALSE, upper=.9){
  rtype <- ifelse("year" %in% names(data), "a", "q")
  repyr <- ifelse(rtype=="q", 999, repyr) #not relevant for q reports
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

  if (fixed==FALSE){
    for (yr in direction){
      #for (yr in max(temp$year):min(temp$year)){
      yrdata <- temp %>%
        dplyr::filter(year==yr,
               cah_state==var)

      value <- ifelse(yrdata$dif < 0, 2, -.75)
      value <- ifelse(max(dplyr::filter(temp, cah_state==var)$est, na.rm = T) > 250 & yrdata$est < .25*max(dplyr::filter(temp, cah_state==var)$est, na.rm = T), -.75, value)
         #added 12/10/21
      #when highest value is very high, the bottom value can get cut off otherwise. few CAHs and no states are affected
      #if any in temp are bigger, and this one is a certain fraction

      positions <- c(positions, value)
    }
  } else if (fixed==TRUE){
    for (yr in direction){
      #for (yr in max(temp$year):min(temp$year)){
      yrdata <- temp %>%
        dplyr::filter(year==yr,
               cah_state==var)

      leg_move= ifelse(rtype=="a", #if report is the annual report
                      ifelse((dplyr::filter(data, year==repyr-1 & cah_state==st)$est > 95| dplyr::filter(data, year==repyr-2 & cah_state==st)$est > 95)==TRUE, 1, 0),
                      0)
        #leg_move <- ifelse(
        #  (dplyr::filter(data, year==repyr-1 & cah_state==st)$est > 95| dplyr::filter(data, year==repyr-2 & cah_state==st)$est > 95)==TRUE, 1, 0)

      #added 10/27/21.. indicates whether the middle values are high enough to cause the legend to move to the bottom
      #this affects where the text should go
      #edited 11/18/21 to make this work with quarterly reports

      #considerations for plot text position
      #1) position of state line relative to plot bounds. Don't want the text to be partially cut off if too low or too high
      #2) position of state line relative to legend. Don't want high text values to overlap with the legend; only affects middle 2 values
      #2a) position of the legend. Some high values will cause the legend to move to the bottom. No worry about overlap in this case
      #3) position of state line relative to national line. Position text on other side of the national line to avoid overlap, unless
      #doing so would violate one of the previous rules. In some cases, the best option may be to overlap the national line.

      value <- case_when(
        #10/27.. some of this is inelegant, but works for now
        leg_move==1 & yrdata$dif >= 0 & axis_max-axis_min==50 & yrdata[yrdata$year==yr & yrdata$cah_state==var,]$est <= 98 ~ -.75,
        #legend has moved, greater than nat est, plot is 50-100, estimate is less than  or equal to 98 -- place above
        leg_move==1 & yrdata$dif >= 0 & axis_max-axis_min==100 & yrdata[yrdata$year==yr & yrdata$cah_state==var,]$est <= 97 ~ -.75,
        #legend has moved, greater than nat est, plot is 0-100, estimate is less than or equal to 97 -- place above
        yrdata[yrdata$year==yr & yrdata$cah_state==var,]$est > upper*axis_max & yrdata[yrdata$year==yr & yrdata$cah_state==var,]$est < upper*axis_max+5 & (yr==repyr|yr==repyr-3) ~ -.75,
        #estimate is greater than the normal upper limit, less than extended limit, but not in middle so it won't run over legend -- place above
        yrdata[yrdata$year==yr & yrdata$cah_state==var,]$est > upper*axis_max ~ 2,
        #estimate is greater than normal upper limit, and in the middle -- place below
        yrdata[yrdata$year==yr & yrdata$cah_state==var,]$est < .1*axis_max+axis_min ~ -.75,
        #estimate is less than the lower limit, may get cut off -- place above
        yrdata$dif < 0 ~ 2,
        #no other case is true, state line is below the national line -- place below
        yrdata$dif >= 0 ~ -.75)
      #no other case is true, state line is above the national line -- place above

      positions <- c(positions, value)
    }

  }

  return(positions)
}
