#' Rank States
#'
#' This function is used to create the data for the side-by-side tables in the annual report.
#' @param data data file
#' @param domain either IP", "OP", "HCAHPS", or "EDTC"
#' @param report_year report year; removed default year
#' @export

reporting_rank <- function(data, domain, report_year){ #cat should be "IP", "OP", "HCAHPS", "EDTC"
  base_data <- data %>%
    dplyr::filter(measure_cat== domain,
           year==report_year)

  ranked_data <- base_data %>%
    {if (domain=="HCAHPS") dplyr::mutate(.data=., nreport= st_nreport) else dplyr::mutate(.data=., nreport= round(st_cat_report*st_ncah, digits=0))} %>%
    #mutate(nreport= round(st_cat_report*st_ncah, digits=0)) %>% # of cahs reporting domain-- this used to be main argument
    #but changed it to make HCAHPS work correctly 8 20 2021-- check tables for other effects
    #mutate(nreport= st_nreport) %>%
    dplyr::select(st_report_rank, state_name, nreport, st_cat_report) %>%
    dplyr::group_by(state_name) %>%
    dplyr::summarise(across(st_report_rank:st_cat_report, mean, na.rm = TRUE)) %>%
    dplyr::add_row(state_name= "National", st_report_rank= NA, nreport= round(unique(base_data[base_data$year == report_year,]$nat_cat_report)*unique(base_data[base_data$year == report_year,]$nat_ncah), digits = 0),
            st_cat_report= unique(base_data[base_data$year == report_year,]$nat_cat_report)) %>%
    #dplyr::mutate(st_cat_report= as.numeric(round_1dig(st_cat_report * 100))) %>% #added this here, otherwise some errors in arrange
    dplyr::mutate(st_cat_report= as.numeric(rndup1(st_cat_report * 100))) %>% ## 30 nov 22: changed to rndup1 for consistency with the other code/PDFs -af
    dplyr::arrange(desc(st_cat_report), desc(nreport)) %>% #(see AR edits sheet) -- 10/27/21
    dplyr::mutate(#st_cat_report= round_1dig(st_cat_report * 100), 10/27/21
      nreport= prettyNum(nreport, big.mark = ","),
      st_report_rank= ifelse(is.na(st_report_rank), "", as.character(st_report_rank))) %>%
    dplyr::select(st_report_rank, state_name, nreport, st_cat_report) %>%
    magrittr::set_colnames(c("Rank","State","CAHs reporting","% of CAHs"))
}
