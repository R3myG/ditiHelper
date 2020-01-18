#' Extract assignees information as a tibble
#' 
#' 
#' @param pv_res Takes results from get_patents_data 
#' @return A tibble with the assignees information
#' 
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' get_assignees_df(pv_res)
#' }
#' 
#' @export
get_assignees_df <- function(pv_res){
  
  df_assignees <- pv_res$data$patents$assignees %>% 
    stats::setNames(pv_res$data$patents$patent_number) %>% 
    dplyr::bind_rows(.id = "patent_number") 
  
  return(df_assignees)
  
}