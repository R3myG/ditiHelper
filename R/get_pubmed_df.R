#' Perform search on pudmed with RISmed and return a tibble with infos
#' 
#' 
#' @param query A pubmed query
#' @return A tibble with scientific publication informations
#' 
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' get_examiners_df(pv_res)
#' }
#' 
#' @export
get_pubmed_df <- function(query){
  
  search_query <- RISmed::EUtilsSummary(query, retmax=10000, mindate=1900,maxdate=2021)
  
  records <- RISmed::EUtilsGet(search_query, type = "efetch", db = "pubmed")

  pubmed_data <- tibble::tibble(Title = RISmed::ArticleTitle(records),
                                PMID = RISmed::PMID(records),    
                            AbstractText = RISmed::AbstractText(records),
                            Year = RISmed::YearAccepted(records),
                            Authors = list(RISmed::Author(records)),
                            Journal = RISmed::Title(records),
                            Mesh = RISmed::Mesh(records),
                            PublicationType = RISmed::PublicationType(records),
                            Country = RISmed::Country(records),
                            Language = RISmed::Language(records))
  
  return(pubmed_data)
  
}