#' Add content to spreadsheets
#'
#' Add column names to spreadsheet. Example: add_colnames 'as_ip_names, 3' will add first 5 vars to row 2, rest to row 3
#' @param names column names
#' @param max_row cell row number
#' @param name_num the number of name columns to add
#' @export

add_colnames <- function(names, max_row, name_num=5){
  for (i in 1:length(names)){
    name <- names[i]
    if (i <= name_num) { #these need to be in higher row, otherwise deleted by merged cells
      add_stuff(row_num = max_row-1, colIndex = i, value= name, style = text_style)}
    else {
      add_stuff(row_num = max_row, colIndex = i, value= name, style = text_style)}
  }
}
