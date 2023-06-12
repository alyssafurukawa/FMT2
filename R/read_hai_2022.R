#' Read HAI data
#'
#' Read and combine HAI data files. An updated version of read_hai_2020 that reflects changes in data formatting.
#' @param folder data location prior to quarter specification
#' @param quarter data quarter, used in path and elsewhere
#' @param year 2022 by default
#' @param joinMOU whether the data should be joined the moulist file. needed for NA measures for non-respondents
#' @export


read_hai_2022 <- function(folder, quarter, year=2022, joinMOU=FALSE) {
  hai_measures <- c("CAUTI", "CDIFF", "CLABSI", "MRSABLD", "SSI_COLO", "SSI_HYST")
  hai_data <- data.frame()
  
  for (m in hai_measures){
    file_data <- read_csv(paste0(fmt_folder, "Data/MBQIP Data/", year, "/", folder, "HRSA_CAH_IQR_HAI_", m, "_", year, "Q", quarter,".csv")) %>%
      clean_names() %>%
      select(provider_id, hsp_state, subm_qtr, sir_num, sir_den, sir,
             days_surg= ifelse(str_detect(m, "SSI")==TRUE, contains("surge"), contains("days")), hsp_name, hsp_city, hsp_state) %>%
      rename(num= sir_num,
             den= sir_den) %>%
      mutate(measure= m,
             provider_id= as.numeric(provider_id),
             across(num:den, ~ifelse(is.na(.x), 0, .x))) %>%
      #need to make these 0 instead of NA as long as CAH is included in data.. see more in OPIP report notes
      dplyr::filter(dplyr::case_when(
        quarter==1 ~ provider_id %ni% nomou_p1,
        quarter==2 ~ provider_id %ni% nomou_p2,
        quarter==3 ~ provider_id %ni% nomou_p3,
        quarter==4 ~ provider_id %ni% nomou_quarterly))
    #{if (quarter==1) filter(.data= ., provider_id %ni% nomou_p1) else .} %>%
    #{if (quarter==2) filter(.data= ., provider_id %ni% nomou_p2) else .} %>%
    #{if (quarter==3) filter(.data= ., provider_id %ni% nomou_p3) else .} %>%
    #{if (quarter==4) filter(.data= ., provider_id %ni% nomou_p4) else .}
    
    if (joinMOU==TRUE){
      file_data <- moulist %>%
        left_join(file_data, by= "provider_id") %>%
        mutate(measure= ifelse(is.na(measure)==TRUE, m, measure))
    }
    
    hai_data <- rbind(hai_data, file_data)
  }
  return(hai_data)
}
