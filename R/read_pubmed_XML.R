#' Parse pubmed XML file with easyPubMed package
#' 
#' 
#' @param xml_file An XML file obtained from pubmed
#' @return A data frame 
#' 
#' @importFrom magrittr %>%
#' 
#' @examples
#' \dontrun{
#' read_pubmed_XML(xml_file)
#' 
#' @export
read_pubmed_XML <- function(xml_file){
  
  Articles_List <- articles_to_list(xml_file)
  
  Articles_DF <- purrr::map_dfr(
    Articles_List, 
    article_to_df, 
    max_chars = 3000, 
    autofill = TRUE, 
    getKeywords = TRUE, 
    getAuthors = TRUE
    ) %>% 
    tibble::as_tibble() %>% 
    tidyr::unite(year, month, day, col = full_date, sep = "-") %>% 
    dplyr::mutate(full_date = lubridate::ymd(full_date) )
  
  return(Articles_DF)
  
}