#' Get domain reporting rates
#'
#' Used in the Annual Reports to get reporting rates for each state at the state, nat, peer, and region levels
#' @param data data file
#' @param domain either IP", "OP", "HCAHPS", or "EDTC"
#' @export

get_report_rate <- function(data, domain){ #domain should be "IP", "OP", "HCAHPS", "EDTC"
  temp <- data

  #nat
  nat_rate <- temp %>%
    dplyr::mutate(nat_ncah= length(unique(provider_id))) %>%
    dplyr::summarise(nat_cat_report= sum(cah_report_measure, na.rm = TRUE)/mean(nat_ncah, na.rm=TRUE)) %>%
    dplyr::mutate(measure_cat= domain)

  #state rate
  st_rate <- temp %>%
    dplyr::group_by(cah_state) %>%
    dplyr::mutate(st_ncah= length(unique(provider_id))) %>%
    dplyr::summarise(st_cat_report= sum(cah_report_measure, na.rm = TRUE)/mean(st_ncah, na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(st_report_rank= rank(-round(st_cat_report, digits = 3), ties.method = "min"),
           measure_cat= domain)

  #peer rate (other states in region)
  peer_rate <- temp %>%
    dplyr::group_by(cah_state) %>%
    dplyr::mutate(st_ncah= length(unique(provider_id)),
           st_report= sum(cah_report_measure)) %>%
    dplyr::group_by(peergroup) %>% #removed pg_name 8/24/21
    dplyr::mutate(peer_ncah= length(unique(provider_id)),
           peer_report= sum(cah_report_measure, na.rm = TRUE),
           compare_ncah= peer_ncah- st_ncah,
           compare_rep= peer_report- st_report) %>%
    dplyr::group_by(cah_state, peergroup, pg_name) %>%
    dplyr::summarise(peer_cat_report= mean(compare_rep, na.rm = TRUE)/mean(compare_ncah, na.rm = TRUE))

  #region rate (other states in region)
  reg_rate <- temp %>%
    dplyr::group_by(cah_state) %>%
    dplyr::mutate(st_ncah= length(unique(provider_id)),
           st_report= sum(cah_report_measure)) %>%
    dplyr::group_by(region) %>% #removed reg_name 8/24/21
    dplyr::mutate(reg_ncah= length(unique(provider_id)),
           reg_report= sum(cah_report_measure, na.rm = TRUE),
           compare_ncah= reg_ncah- st_ncah,
           compare_rep= reg_report- st_report) %>%
    dplyr::group_by(cah_state, region, reg_name) %>%
    dplyr::summarise(reg_cat_report= mean(compare_rep, na.rm = TRUE)/mean(compare_ncah, na.rm = TRUE))

  #combine
  rep_rates <- st_rate %>%
    dplyr::inner_join(nat_rate, by= "measure_cat") %>%
    dplyr::inner_join(peer_rate, by= "cah_state") %>%
    dplyr::inner_join(reg_rate, by= "cah_state")
}
