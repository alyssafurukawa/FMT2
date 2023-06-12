#' Read OQR data
#'
#' Used for reading in OQR data in the Annual Reports
#' @param folder data location prior to quarter specification
#' @param quarter data quarter, used in path and elsewhere
#' @param joinMOU whether the data should be joined the moulist file. needed for NA measures for non-respondents
#' @param ps whether the data includes population data; true by default
#' @param impute whether OP-3b observation should be imputed if OP-2 is present; true by default
#' @export

read_oqr_ps <- function(folder, quarter, joinMOU=FALSE, ps= TRUE, impute= TRUE){
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
  
  #if the CAH only has OP-2, this will add a row for OP-3b with 0 population
  #so it will automatically be considered reporting
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
  
  #join data for each measure to MOU list
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
      dplyr::filter(measure %in% oqr_meas)
  }
  
  #use pop data to determine reporting, and add a flag where impacted
  #for all cases when reporting OP-2 but not OP-3b, change OP-3b to reporting
  #if not using PS data (unlikely), will need to create report variable outside of function
  if (ps==TRUE){
    oqr_final_1 <- oqr_frame %>%
      mutate(
        report= case_when(
          !is.na(denominator) ~ 1, #value if > 0 (rows 4-6), * if 0 (rows 7-9)
          is.na(denominator) & population_total==0 ~ 1, #will receive * (row 3), also the imputed CAHs
          TRUE ~ 0),  #will receive NA (rows 1 & 2)
        cah_ps_flag= case_when(
          denominator==0 ~ 1, #rows 7-9 of Telligen table
          is.na(denominator) & population_total==0 ~ 1, #third row of telligen table
          TRUE ~ 0))
    
    op2_report <- oqr_final_1 %>% filter(measure=="OP-2", report==1)
    
    oqr_final <- oqr_final_1 %>%
      mutate(cah_ps_flag= ifelse(measure=="OP-3b" & provider_id %in% op2_report$provider_id & report==0, 1, cah_ps_flag),
             report= ifelse(measure=="OP-3b" & provider_id %in% op2_report$provider_id & report==0, 1, report),
             quarter= quarter)
  } else {
    oqr_final <- oqr_frame %>%
      mutate(quarter=quarter)
  }
  
  return(oqr_final)
}