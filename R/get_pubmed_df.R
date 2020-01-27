#' Perform search on pudmed with RISmed and return a tibble with infos
#' 
#' 
#' @param query A pubmed query
#' @return A tibble with scientific publication informations
#' 
#' @examples
#' \dontrun{
#' get_pubmed_df_general('query')
#' }
#' 
#' @export
get_pubmed_df_general <- function(query){
  
  search_query <- RISmed::EUtilsSummary(query, retmax=10000, mindate=1900,maxdate=2021)
  
  records <- RISmed::EUtilsGet(search_query, type = "efetch", db = "pubmed")

  pubmed_data <- tibble::tibble(Title = RISmed::ArticleTitle(records),
                                PMID = RISmed::PMID(records),    
                            AbstractText = RISmed::AbstractText(records),
                            Year = RISmed::YearAccepted(records),
                            Authors = list(RISmed::Author(records)),
                            Journal = RISmed::Title(records),
                            Country = RISmed::Country(records),
                            Language = RISmed::Language(records))
  
  return(pubmed_data)
  
}


#' Perform search on pudmed with RISmed and return a tibble with abstracts
#' 
#' 
#' @param query A pubmed query
#' @return A tibble with scientific publication informations
#' 
#' @examples
#' \dontrun{
#' get_pubmed_df_abstract('query')
#' }
#' 
#' @export
get_pubmed_df_abstract <- function(query){
  
  search_query <- RISmed::EUtilsSummary(query, retmax=10000, mindate=1900,maxdate=2021)
  
  records <- RISmed::EUtilsGet(search_query, type = "efetch", db = "pubmed")
  
  pubmed_data <- tibble::tibble(PMID = RISmed::PMID(records),    
                                AbstractText = RISmed::AbstractText(records),
                                Year = RISmed::YearAccepted(records),
                                Country = RISmed::Country(records))
  
  return(pubmed_data)
  
}