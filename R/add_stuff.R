#' Add content to spreadsheets
#'
#' Used to add content to spreadsheet rows. A slightly edited version of Bert's old function.
#' @param sheet sheet name
#' @param row_num cell row number
#' @param colIndex cell column number
#' @param value value to insert
#' @param style formatting style
#' @export

add_stuff <- function(sheet, row_num,colIndex,value,style){
  sheet_value <- xlsx::createCell(rows[row_num],colIndex = colIndex)
  xlsx::setCellValue(sheet_value[[1,1]],value)
  xlsx::setCellStyle(sheet_value[[1,1]],style)
}
