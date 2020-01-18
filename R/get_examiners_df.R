#' Extract examiners information as a tibble
#' 
#' 
#' @param pv_res Takes results from get_patents_data 
#' @return A tibble with the examiners information
#' 
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' get_examiners_df(pv_res)
#' }
#' 
#' @export
get_examiners_df <- function(pv_res){
  
  df_examiners <- pv_res$data$patents$examiners %>% 
    stats::setNames(pv_res$data$patents$patent_number) %>% 
    dplyr::bind_rows(.id = "patent_number") 
  
  return(df_examiners)
  
}