#' Catch duplicates within a pipe.
#'
#' Catch duplicates within a pipe.
#' @param var by default, is provider_id
#' @param var2 a 2nd grouping variable, like measure
#' @param var3 3rd grouping variable, like quarter
#' @export

catch_dupes <- function(data, var="provider_id", var2=NULL, var3=NULL){
  id_dupes <- janitor::get_dupes(data, var, var2, var3)

  if (nrow(id_dupes) == 0) {
    print(paste("No Duplicates for", var))
    return(data)}

  else{
    print(paste("Duplicates Found for", var))
    #print("See Pipe Output for Duplicates")
    stop(paste("\nRemove", nrow(id_dupes)/2, "Pair of Duplicates"),
         "\nUse ", quote(data), " %>% get_dupes() to search duplicates")
  }
}
