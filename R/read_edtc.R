#' Read EDTC data
#'
#' Read and format EDTC files. Only used in the annual reports, not quarterly. And will likely need to change with 2020 annual report.
#' @param folder partial file path
#' @param quarter data quarter
#' @param year defaults to 2019
#' @param pivoted TRUE will turn EDTC data into long format
#' @export

read_edtc <- function(folder, quarter, year=2019, pivoted= FALSE){

  edtc_data_raw <- readr::read_csv(paste0(fmt_folder, "Data/Old Data/CAH MBQIP raw data from FORHP/", year, "/",
                                   folder, quarter,"Q19_National_forsas.csv")) %>%
    dplyr::mutate(provider_id= as.numeric(provider_id)) %>%
    dplyr::filter(provider_id %ni% paste0("nomou_p", quarter))

  edtc_data <- moulist %>%
    dplyr::left_join(edtc_data_raw, by="provider_id") %>%
    #filter(hc==1) %>%
    dplyr::mutate(quarter= quarter) %>%
    dplyr::select(provider_id:cahzip, denom:quarter) %>%

    {if (pivoted==TRUE) tidyr::pivot_longer(data= ., cols= edtc1:all_edtc, names_to= "measure", values_to= "num") else .}
}
