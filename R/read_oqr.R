#' Read OQR data
#'
#' Used for reading in OQR data in the Annual Reports
#' @param folder data location prior to quarter specification
#' @param quarter data quarter, used in path and elsewhere
#' @param joinMOU whether the data should be joined the moulist file. needed for NA measures for non-respondents
#' @param ps whether the data includes population data; true by default
#' @param impute whether OP-3b observation should be imputed if OP-2 is present; true by default
#' @export

read_oqr <- function(folder, quarter, joinMOU=FALSE, ps= TRUE, impute= TRUE){
  oqr_meas <- c("OP-2", "OP-3b", "OP-18b")
  oqr_data <- read_csv(paste0(fmt_folder, "Data/MBQIP Data/", folder)) %>%
    janitor::clean_names() %>%
    {if (ps==FALSE) dplyr::mutate(.data= ., population_total= NA) else .} %>%
    dplyr::select(provider_id, measure, numerator, denominator, population_total) %>%
    dplyr::mutate(provider_id= as.numeric(provider_id)) %>%
    dplyr::filter(dplyr::case_when(
      quarter==1 ~ provider_id %ni% nomou_p1,
      quarter==2 ~ provider_id %ni% nomou_p2,
      quarter==3 ~ provider_id %ni% nomou_p3,
      quarter==4 ~ provider_id %ni% nomou_p4))
    #{if (quarter==1) dplyr::filter(.data= ., provider_id %ni% nomou_p1) else .} %>%
    #{if (quarter==2) dplyr::filter(.data= ., provider_id %ni% nomou_p2) else .} %>%
    #{if (quarter==3) dplyr::filter(.data= ., provider_id %ni% nomou_p3) else .} %>%
    #{if (quarter==4) dplyr::filter(.data= ., provider_id %ni% nomou_p4) else .}
  #this worked, but caused an error message when the package was loaded

  if (impute==TRUE){
    imputed <- oqr_data %>%
      dplyr::filter(measure=="OP-2"|measure=="OP-3b") %>%
      dplyr::mutate(num= 1) %>%
      dplyr::group_by(provider_id) %>%
      dplyr::summarise(num=sum(num)) %>%
      dplyr::filter(num == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-num) %>%
      dplyr::mutate(measure="OP-3b",
             population_total=0, #will be counted as reporting, * rate
             imputed=1)

    oqr_data <- dplyr::bind_rows(oqr_data, imputed)
  }

  if (joinMOU==TRUE){
    oqr_frame <- dplyr::tibble()
    for (m in oqr_meas){
      temp <- moulist %>%
        dplyr::left_join(
          oqr_data %>%
            dplyr::filter(measure==m),
          by= "provider_id") %>%
        dplyr::mutate(measure= ifelse(is.na(measure), m, measure))

      oqr_frame <- rbind(oqr_frame, temp)}
  } else {
    oqr_frame <- oqr_data %>%
      dplyr::filter(measure %in% oqr_meas)}
  return(oqr_frame)
}
