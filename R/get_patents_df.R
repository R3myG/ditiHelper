#' Extract patents information as a tibble
#' 
#' 
#' @param pv_res Takes results from get_patents_data 
#' @return A tibble with the patents information
#' 
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' get_patents_df(pv_res)
#' }
#' 
#' @export
get_patents_df <- function(pv_res){
  
  df_patents <- pv_res$data$patents %>%
    dplyr::select(dplyr::starts_with("patent_"))
    
  
  return(df_patents)
  
}