#' Fetch USPTO patents data 
#' 
#' 
#' @param query Takes in a query formated with `patentsview` domain specific language (DSL)
#' @return List containing patents information
#' 
#' @examples
#' \dontrun{
#' query <- with_qfuns( 
#'  or(
#'   text_all(patent_title = "artificial intelligence"),
#'   text_all(patent_abstract = "artificial intelligence")
#'   )
#' )
#' get_patents_data(query)
#' }
#' 
#' @export

get_patents_data <- function(query){
  
  fields <- patentsview::get_fields(
    endpoint = "patents", 
    groups = c("applications", "assignees", "cpcs", "patents", "examiners", "inventors", "lawyers", "wipos") )
  
  pv_res <- patentsview::search_pv(query = query, fields = fields, all_pages = TRUE)
  
  return(pv_res)
  
}