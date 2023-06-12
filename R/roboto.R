#' Load Annual Report Font
#'
#' Load the roboto font. It must be installed on computer, and added to R through the extrafont package first.
#'
#' @export

roboto <- function(){
  sysfonts::font_add(family="Roboto Condensed", regular="C:/Users/bean/AppData/Local/Microsoft/Windows/Fonts/RobotoCondensed-Regular.ttf")
  showtext::showtext_auto()
}
