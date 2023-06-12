#' Read HCAHPS data
#'
#' Used for reading in HCAHPS data for quarterly reports
#' @param file data location
#' @param joinMOU whether the data should be joined the moulist file. needed for NA measures for non-respondents
#' @export

read_hcahps <- function(file, joinMOU=FALSE){
  hcahps_measures <- c("Composite 1", "Composite 2", "Composite 3", "Composite 5", "Composite 6", "Composite 7",
                       "Q8", "Q9", "Q18", "Q19", "Star Rating") #also updated this for new function

  hcahps_data <- readxl::read_xlsx(paste0(fmt_folder, "Data/MBQIP Data/", file)) %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(sometimes_to_never:star_rating, as.numeric),
           period= 4,
           resp_rate= as.numeric(h_resp_rate_p)) %>%
    dplyr::select(-c(textual_completed_surveys, footnotes:h_resp_rate_p))

  if (joinMOU==TRUE){
    hcahps_frame <- tibble()
    for (m in hcahps_measures){
      temp <- moulist %>%
        dplyr::left_join(
          hcahps_data %>%
            dplyr::filter(question_id==m)) %>%
        dplyr::mutate(question_id= ifelse(is.na(question_id), m, question_id),
                      completed_surveys= ifelse(is.na(completed_surveys), "N/A", completed_surveys))

      hcahps_frame <- dplyr::bind_rows(hcahps_frame, temp)}
  } else {
    hcahps_frame <- hcahps_data %>%
      dplyr::filter(question_id %in% hcahps_measures)}

  return(hcahps_frame)
}
