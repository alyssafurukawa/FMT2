#' Custom GGPlot Theme
#'
#' My custom GGPlot theme, including the Roboto font which needs to be installed.
#' @export

theme_nate <- function (){
  sysfonts::font_add(family="Roboto Condensed", regular="C:/Users/bean/AppData/Local/Microsoft/Windows/Fonts/RobotoCondensed-Regular.ttf")
  showtext::showtext_auto()
  font <- "Roboto"
  theme_minimal() %+replace%

    theme(
      text= element_text(family= "Roboto Condensed"),
      plot.title = element_text(size=16, vjust=2),
      plot.subtitle = element_text(size = 13, vjust=1),
      plot.caption = element_text(size=10, hjust=1),
      axis.title = element_text(size=13),
      axis.text = element_text(size=11),
      axis.ticks = element_line(colour = "grey20"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color="light grey"),
      legend.background = element_rect(fill="transparent", color=NA),
      legend.key = element_rect(fill="transparent", color=NA),
      axis.line = element_line(colour = "black", size = rel(1)),
      complete=FALSE
    )
}
