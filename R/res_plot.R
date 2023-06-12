#' Scatterplot Matrix
#'
#' A quicker and better looking residual plot. takes the name of a model, for instance, "advert_model" from "advert_model <- lmSales ~ TV * Radio, data=advertising
#' @param model model
#' @export


res_plot <- function(model){
  points <- moderndive::get_regression_points(model)
  xaxis <- names(points[2])

  rplot <- points %>%
    ggplot(aes_string(x= xaxis, y="residual")) +
    geom_jitter(color= "#1f78b4") +
    #geom_smooth(method= "lm", se = "TRUE", color= "yellow")+
    labs(title = paste("Residual Plot for", xaxis, "Model"),
         y= "Residual Value",
         x= xaxis) +
    theme_nate()

  print(rplot)
}
