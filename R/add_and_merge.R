#' Add content to spreadsheets
#'
#' Similar to add_stuff, but can add values to merged rows and columns
#' @param value the value to add
#' @param sheet sheet name
#' @param row_num minimum row number
#' @param col_num minimum column
#' @param height number of merged rows
#' @param width number of merged columns
#' @param style formatting style
#' @export

add_and_merge <- function(value, sheet, row_num, col_num, height=0, width=0, style= column_title){
  sheet_value <- xlsx::createCell(rows[row_num], colIndex = col_num)

  ht <- 2:height
  wt <- 2:width

  #this will create extra cells for formatting. this is necessary because otherwise merged cells will not be completely bordered.
  if (height != 0 & width==0){ #merges across both rows and columns haven't been added yet
    for (h in ht){
      assign(paste0("sv_", h), xlsx::createCell(rows[row_num+h-1], colIndex = col_num))
    }
  } else if (height == 0 & width != 0){
    for (w in wt){
      assign(paste0("sv_", w), xlsx::createCell(rows[row_num], colIndex = col_num+w-1))
    }
  }

  xlsx::setCellValue(sheet_value[[1,1]], value)
  xlsx::setCellStyle(sheet_value[[1,1]], style)

  #now set cell style for those extra cells created above
  if (height == 0 & width != 0){
    list <- rep(paste0('sv_', 1:50)) #possible files
    list <- list[2:width] #the files that were created
    for (file in list){
      temp <- get(file)
      xlsx::setCellStyle(temp[[1,1]], style)
    }
  } else if (height != 0 & width==0){
    list <- rep(paste0('sv_', 1:50))
    list <- list[2:height]
    for (file in list){
      temp <- get(file)
      xlsx::setCellStyle(temp[[1,1]], style)
    }
  }

  xlsx::addMergedRegion(sheet, row_num, ifelse(height==0, row_num, row_num+height-1), col_num, ifelse(width==0, col_num, col_num+width-1))
}

#added height/width 3/23/22.. old function, which doesnt have vertical merging and uses max_col instead of width. also renamed colIndex
#also added a lot of code to make sure that merged cells would be correctly bordered. previously needed to edit this manually

#add_and_merge <- function(value, sheet, row_num, colIndex, max_col, style= column_title, height=0){
#  sheet_value <- xlsx::createCell(rows[row_num], colIndex = colIndex)
#  xlsx::setCellValue(sheet_value[[1,1]], value)
#  xlsx::setCellStyle(sheet_value[[1,1]], style)
#  xlsx::addMergedRegion(sheet, row_num, ifelse(height==0, row_num, row_num+height-1), colIndex, max_col) #start and end row must always be the same
#}
