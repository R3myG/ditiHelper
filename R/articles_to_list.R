#' Cast PubMed Data into a List of Articles
#'
#' Convert an XML object of PubMed records into a list of strings 
#' (character vector of length 1) corresponding to individual PubMed articles. 
#' PubMed records are identified by a "/PubmedArticle" XML tag. This automatically casts 
#' all the content of each PubMed record to a character-class object without removing XML tags.
#' 
#' 
#' @param pubmed_data String corresponding to the name of an XML file 
#' (typically, the result of a batch_pubmed_download() call). Alternatively, 
#' an XML Object, such as the result of a fetch_pubmed_data() call.
#' 
#' @details The input is an XML object or an XML file, typically the result of a fetch_pubmed_data() 
#' call or a batch_pubmed_download() call. The function returns a list where each element 
#' is a different PubMed record
#' 
#' Character vector listing all the records from the original XML object in text format. Elements in the list are not named and are only accessible via the numeric index.
#' 
#' @author Damiano Fantini <damiano.fantini@@gmail.com>
#' 
#' @references https://www.data-pulse.com/dev_site/easypubmed/
#' 
#' @examples 
#' \dontrun{
#' ## Retrieve PubMed data and return a list ot articles
#' listed_articles <- articles_to_list("pubmed_result.xml")
#' }
#' 
#' @export
articles_to_list <- function(pubmed_data){
    
    # check if it is a XMLAbstractDocument or a file
    options(warn = -1)
    if (sum(class(pubmed_data) == "XMLAbstractDocument") > 0) {
      out <- tryCatch(XML::xpathSApply(pubmed_data, "//PubmedArticle", XML::saveXML), error = function(e) { NULL })
    } else if (class(pubmed_data) == "character") {
      out <- tryCatch(XML::xmlParse(pubmed_data), error = function(e) {NULL})    
      if (!is.null(out))
        out <- tryCatch(XML::xpathApply(out, "//PubmedArticle", XML::saveXML), error = function(e) { NULL })    
    } else {
      out <- NULL
    }
    if (is.null(out)) {
      message("An error occurred")
    }
    options(warn = 0)
    return(out)
  }  
