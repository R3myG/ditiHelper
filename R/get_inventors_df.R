#' Extract inventors information as a tibble
#' 
#' 
#' @param pv_res Takes results from get_patents_data 
#' @return A tibble with the inventors information
#' 
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' get_inventors_df(pv_res)
#' }
#' 
#' @export

get_inventors_df <- function(pv_res){
  
  df_inventors <- pv_res$data$patents$inventors %>% 
    stats::setNames(pv_res$data$patents$patent_number) %>% 
    dplyr::bind_rows(.id = "patent_number") 
  
  return(df_inventors)
  
}