#' Extract applications information as a tibble
#' 
#' 
#' @param pv_res Takes results from get_patents_data 
#' @return A tibble with the applications information
#' 
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' get_applications_df(pv_res)
#' }
#' 
#' @export
get_applications_df <- function(pv_res){
  
  df_applications <- pv_res$data$patents$applications %>% 
    stats::setNames(pv_res$data$patents$patent_number) %>% 
    dplyr::bind_rows(.id = "patent_number") 
  
  return(df_applications)
  
}