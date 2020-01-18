#' Extract lawyers information as a tibble
#' 
#' 
#' @param pv_res Takes results from get_patents_data 
#' @return A tibble with the lawyers information
#' 
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' get_lawyers_df(pv_res)
#' }
#' 
#' @export
get_lawyers_df <- function(pv_res){
  
  df_lawyers <- pv_res$data$patents$lawyers %>% 
    stats::setNames(pv_res$data$patents$patent_number) %>% 
    dplyr::bind_rows(.id = "patent_number") 
  
  return(df_lawyers)
  
}