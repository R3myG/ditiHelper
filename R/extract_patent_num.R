#' Method to get patents data based on a web search
#' 
#' 
#' @param  url_search Url of the search
#' @param nb_pages Number of pages to parse (number of patents found / 50)
#' @return A list with results
#' 
#' @importFrom magrittr %>%
#' 
#' @examples
#' \dontrun{
#' get_pubmed_df_general('query')
#' }
#' 
#' @export

extract_patent_num <- function(url_search, nb_pages){
  
  all_urls <- stringr::str_replace(string = url_search, pattern = "&p=1", replacement = paste0("&p=", 1:nb_pages ) )
  
  all_patNums <- purrr::map(all_urls, parse_patNum) %>% unlist()
  
  print(length(all_patNums))
  
  all_patNums_list <- split(all_patNums, ceiling(seq_along(all_patNums)/10))
  
  all_queries <- list()
  
  for(i in 1:length(all_patNums_list)) {
    new_query <- patentsview::with_qfuns(
      eq(patent_number = all_patNums_list[[i]])
    )
    all_queries <- append(all_queries, list(new_query))
  }
  
  all_dat_list <- purrr::map(all_queries, purrr::possibly(ditiHelper::get_patents_data, FALSE ))  %>% Filter(is.list, .)
  
  return(all_dat_list)
}

#' 
#' 
#' @importFrom magrittr %>%
#' 
parse_patNum <- function(url){
  xml2::read_html(url) %>% 
    rvest::html_text() %>% 
    stringr::str_extract_all("\\n\\d{1,3},\\d{3},?\\d{0,3}\\n", simplify = TRUE) %>% 
    stringr::str_remove_all("\n") %>% 
    stringr::str_remove_all(",") 
  
}