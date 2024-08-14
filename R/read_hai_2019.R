#' Read HAI data
#'
#' Read and combine HAI data files. Legacy: use read_hai_2020 as the default for this task. That reflects changes in data formatting. Use this function for running old reports.
#' @param folder data location prior to quarter specification
#' @param quarter data quarter, used in path and elsewhere
#' @param year 2020 by default
#' @param joinMOU whether the data should be joined the moulist file. needed for NA measures for non-respondents
#' @export


read_hai_2019 <- function(folder, quarter, year=2020, joinMOU=FALSE) {
  hai_measures <- c("CAUTI", "CDIFF", "CLABSI", "MRSABLD", "SSICOLO", "SSIHYST")
  hai_data <- tibble::tibble()

  for (m in hai_measures){
    file_data <- readr::read_csv(paste0("//files.umn.edu/sph/HPM/Projects/RHRC/RHRC_Flex/Data/MBQIP Data/", year, "/", folder, "/CAH_HAI_", m, "_", year, "Q", quarter,".csv")) %>%
      janitor::clean_names() %>%
      dplyr::select(provider_id, hsp_state, subm_qtr, paste0(tolower(m), "_fac_sir_num"), paste0(tolower(m), "_fac_sir_den"), paste0(tolower(m), "_fac_sir"),
             days_surg= ifelse(str_detect(m, "SSI")==TRUE, paste0(tolower(m), "_fac_surg"), tidyselect::contains("days")), hsp_name, hsp_city, hsp_state) %>%
      dplyr::rename(
        num= paste0(tolower(m), "_fac_sir_num"),
        den= paste0(tolower(m), "_fac_sir_den"),
        sir= paste0(tolower(m), "_fac_sir")) %>%
      dplyr::mutate(measure= m,
             provider_id= as.numeric(provider_id),
             dplyr::across(num:den, ~ifelse(is.na(.x), 0, .x)))
    #need to make these 0 instead of NA as long as CAH is included in data.. see more in OPIP report notes
    #not sure where this original note was from.. but this is done because clabsi and cauti often have blanks but
    #we consider them reporting. every once in a while there will be something similar in another file, so that
    #will also be switched from NA to 0 and counted as reporting.

    if (joinMOU==TRUE){
      file_data <- moulist %>%
        left_join(file_data, by= "provider_id") %>%
        dplyr::mutate(measure= ifelse(is.na(measure)==TRUE, m, measure))
    }

    hai_data <- rbind(hai_data, file_data)
  }
  return(hai_data)
}
