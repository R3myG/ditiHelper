#' Extract wipos information as a tibble
#' 
#' 
#' @param pv_res Takes results from get_patents_data 
#' @return A tibble with the wipos information
#' 
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' get_wipos_df(pv_res)
#' }
#' 
#' @export
get_wipos_df <- function(pv_res){
  
  df_wipos <- pv_res$data$patents$wipos %>% 
    stats::setNames(pv_res$data$patents$patent_number) %>% 
    dplyr::bind_rows(.id = "patent_number") 
  
  return(df_wipos)
  
}