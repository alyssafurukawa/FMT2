#' Create 2x Tables
#'
#' #This function creates the side-by-side tables used in the annual reports. Access them by subsetting the list
#' @param data data created using FMT::reporting_rank
#' @export

double_table <- function(data){
  rank1 <- data %>% dplyr::slice(1:23) %>% dplyr::mutate(`% of CAHs`= round_1dig(`% of CAHs`)) #otherwise can have error
  rank2 <- data %>% dplyr::slice(24:46)

  #to control table highlights
  index1_state <- which(rank1$State==state_name) #from pdf printing loop
  index2_state <- which(rank2$State==state_name)

  index1_nat <- which(rank1$State=="National")
  index2_nat <- which(rank2$State=="National")

  #make the tables (will be side-by-side)
  rank_table1 <- knitr::kable(rank1, format = "latex", booktabs = TRUE, linesep="", longtable = TRUE,
                       align=c("r","l","c","c")) %>%
    kableExtra::kable_styling(latex_options = c("striped", "scale_down", "repeat_header"), stripe_index = strip.num[1:12],
                  position = "center", font_size = 11.5, stripe_color = "#ddf1f7") %>%
    kableExtra::row_spec(index1_state, background  = "#668cff") %>%
    kableExtra::row_spec(index1_nat, background  = "#60AFFE") %>%
    kableExtra::column_spec(1, bold= TRUE)

  rank_table2 <- knitr::kable(rank2, format = "latex", booktabs = TRUE, linesep="", longtable = TRUE,
                       align=c("r","l","c","c")) %>%
    kableExtra::kable_styling(latex_options = c("striped", "scale_down", "repeat_header"), stripe_index = strip.num[1:12],
                  position = "center", font_size = 11.5, stripe_color = "#ddf1f7") %>%
    kableExtra::row_spec(index2_state, background  = "#668cff") %>%
    kableExtra::row_spec(index2_nat, background  = "#60AFFE") %>%
    kableExtra::column_spec(1, bold = TRUE)#, border_left = TRUE)

  return(list(rank_table1, rank_table2))
}
