#' Extract cpcs information as a tibble
#' 
#' 
#' @param pv_res Takes results from get_patents_data 
#' @return A tibble with the cpcs information
#' 
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' get_cpcs_df(pv_res)
#' }
#' 
#' @export
get_cpcs_df <- function(pv_res){
  
  df_cpcs <- pv_res$data$patents$cpcs %>% 
    stats::setNames(pv_res$data$patents$patent_number) %>% 
    dplyr::bind_rows(.id = "patent_number") 
  
  return(df_cpcs)
  
}