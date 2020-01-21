#' @title Extract Data from a PubMed Record
#'
#' @description Extract publication-specific information from a PubMed record driven by XML tags. 
#' The input record is a string (character-class vector of length 1) and includes 
#' PubMed-specific XML tags. Data are returned as a data frame where each row corresponds 
#' to one of the authors of the PubMed article.
#'
#' @usage article_to_df(pubmedArticle, autofill = FALSE, 
#'                      max_chars = 500, getKeywords = FALSE, 
#'                      getAuthors = TRUE)
#'                     
#' @param pubmedArticle String including one PubMed record.
#' @param autofill Logical. If TRUE, missing affiliations are automatically imputed based on other non-NA 
#' addresses from the same record.
#' @param max_chars Numeric (integer). Maximum number of characters to be extracted from the Article 
#' Abstract field. Set max_chars to -1 for extracting the full-length abstract. Set max_chars to 0 to 
#' extract no abstract.
#' @param getKeywords Logical. If TRUE, an attempt to extract article Keywords will be made.
#' @param getAuthors Logical. If FALSE, author information won't be extracted. This will considerably 
#' speed up the operation.
#'
#' @details 
#' Given one Pubmed Article record, this function will automatically extract a set of features. 
#' Extracted information include: PMID, DOI, article title, article abstract, publication date (year, month, day), 
#' journal name (title, abbreviation), keywords, and a set of author-specific info (names, affiliation, email address). 
#' Each row of the output data frame corresponds to one of the authors of the PubMed record. Author-independent info 
#' (publication ID, title, journal, date) are identical across all rows. If information about authors are not required, 
#' set 'getAuthors' = TRUE.
#'
#' @return Data frame including the extracted features. Each row correspond a different author.
#'
#' @author Damiano Fantini \email{damiano.fantini@@gmail.com}
#'
#' @references \url{https://www.data-pulse.com/dev_site/easypubmed/}
#'
#' @examples 
#' try({
#'   ## Display some contents
#'   data("EPMsamples")
#'   #display Query String used for collecting the data
#'   print(EPMsamples$NUBL_1618$qry_st)
#'   #Get records
#'   BL_list <- EPMsamples$NUBL_1618$rec_lst
#'   cat(BL_list[[1]])
#'   # cast PM recort to data.frame
#'   BL_df <- article_to_df(BL_list[[1]], max_chars = 0)
#'   print(BL_df)
#' }, silent = TRUE)
#' 
#' \dontrun{
#' ## Query PubMed, retrieve a selected citation and format it as a data frame
#' dami_query <- "Damiano Fantini[AU] AND 2017[PDAT]"
#' dami_on_pubmed <- get_pubmed_ids(dami_query)
#' dami_abstracts_xml <- fetch_pubmed_data(dami_on_pubmed)
#' dami_abstracts_list <- articles_to_list(dami_abstracts_xml)
#' article_to_df(pubmedArticle = dami_abstracts_list[[1]], autofill = FALSE)
#' article_to_df(pubmedArticle = dami_abstracts_list[[2]], autofill = TRUE, max_chars = 300)[1:2,]
#' }
#'
#' @export
article_to_df <-
  function(pubmedArticle, 
           autofill = FALSE, 
           max_chars = 500, 
           getKeywords = FALSE,
           getAuthors = TRUE) 
  {
    #
    currWarn <- options()$warn
    options(warn = -1)
    
    # Nested f(x)
    fix_pdate <- function(date_vec)
    {
      
      month_dict <- data.frame(
        id = c("january", "01", "1", 
               "february", "02", "2", 
               "march", "03", "3", 
               "april", "04", "4", 
               "march", "05", "5", 
               "june", "06", "6", 
               "july", "07", "7", 
               "august", "08", "8", 
               "september", "09", "9", 
               "october", "10",
               "november", "11",
               "december", "12"),
        val = c(rep(1, 3),
                rep(2, 3),
                rep(3, 3),
                rep(4, 3),
                rep(5, 3),
                rep(6, 3),
                rep(7, 3),
                rep(8, 3),
                rep(9, 3),
                rep(0, 2),
                rep(11, 2),
                rep(12, 2)))
      
      if (!is.na(date_vec[[1]])){
        month <- tryCatch({ match.arg(arg = tolower(date_vec[[2]]), 
                                      choices = month_dict$id, 
                                      several.ok = FALSE )}, 
                          error = function(e) {"january"})
        month <- month_dict$val[month_dict$id == month]
      } else {
        month <- NA
      }
      
      if (is.na(date_vec[[3]]) && !is.na(month) && !is.na(date_vec[[1]]) ){
        date_vec[[3]]  <- 1
      }
      
      return( c(Year = as.numeric(date_vec[[1]]), 
                Month = as.numeric(month),
                Day = as.numeric(date_vec[[3]])))
      
    }
    
    # Proceed
    
    # initial check
    # expected cols = 14
    # "pmid", "doi", "title", "abstract", "year", "month", "day", "jabbrv", "journal", 
    # "keywords", "lastname", "firstname", "address", "email" 
    
    # Global Check!
    if (class(pubmedArticle)[1] != "character" ||
        length(pubmedArticle) != 1 ||
        regexpr("(<PubmedArticle)(.+)(\\/PubmedArticle>)", pubmedArticle) < 0 )
    {
      message("An error occurred")
      return(NULL)
    }
    
    # max_chars Check
    if (!is.numeric(max_chars)) {
      max_chars <- 500
    } else if (max_chars < 0) {
      max_chars <- -1  
    }
    
    # Get started
    tryCatch({
      
      # fix &amp;
      pubmedArticle <- gsub("&amp;", "&", pubmedArticle, ignore.case = TRUE)
      
      # proceed
      tmp.article <- custom_grep(xml_data = pubmedArticle, tag = "PubmedArticle", format = "char")
      if (is.null(tmp.article)) 
      {
        message("An error occurred")
        return(NULL)
      }
      
      # Title
      tmp.title <- custom_grep(xml_data = tmp.article, tag = "ArticleTitle", format = "char")
      if (length(tmp.title) > 1){
        tmp.title <- paste(tmp.title, collapse = " ", sep = " ")
      } else if (length(tmp.title) < 1) {
        tmp.title <- NA
      }
      
      # Abstract
      tmp.abstract <- custom_grep(xml_data = tmp.article, tag = "AbstractText", format = "char")
      if (length(tmp.abstract) > 1){
        tmp.abstract <- paste(tmp.abstract, collapse = " ", sep = " ")
        if(max_chars >= 0) {
          tmp.abstract <- gsub("</{0,1}i>", "", tmp.abstract, ignore.case = T)
          tmp.abstract <- gsub("</{0,1}b>", "", tmp.abstract, ignore.case = T)
          tmp.abstract <- gsub("</{0,1}sub>", "", tmp.abstract, ignore.case = T)
          tmp.abstract <- gsub("</{0,1}exp>", "", tmp.abstract, ignore.case = T)
          
          tmp.abstract <- substr(tmp.abstract, 0, max_chars)
        }
      } else if (length(tmp.abstract) < 1) {
        tmp.abstract <- NA
      } else {
        if(max_chars >= 0) {
          tmp.abstract <- substr(tmp.abstract, 0, max_chars)
          tmp.abstract <- gsub("</{0,1}i>", "", tmp.abstract, ignore.case = T)
          tmp.abstract <- gsub("</{0,1}b>", "", tmp.abstract, ignore.case = T)
          tmp.abstract <- gsub("</{0,1}sub>", "", tmp.abstract, ignore.case = T)
          tmp.abstract <- gsub("</{0,1}exp>", "", tmp.abstract, ignore.case = T)
          
        }
      }
      
      # Dates, if any
      my.dateType <- c("DateCompleted",  "DateCreated",  "DateRevised",  "PubDate")
      sel.dateType <-which(sapply(my.dateType, (function(xi) {
        regexpr(xi, tmp.article) > 0
      })))
      if (length(sel.dateType) < 1) {
        tmp.date <- c(Year=NA, Month=NA, Day=NA)
      } else {
        sel.dateType <- sel.dateType[1]
        tmp.date <- custom_grep(xml_data = tmp.article, tag = my.dateType[sel.dateType], format = "char")
        tmp.date <- sapply(c("Year", "Month", "Day"), (function(tt){
          tdat.el <- custom_grep(xml_data = tmp.date, tag = tt, format = "char")
          ifelse(is.null(tdat.el), NA, tdat.el[1])
        }))
      }
      # Fix date
      tmp.date <- fix_pdate(tmp.date)
      
      # Fetch ID string
      tmp.paperID  <- custom_grep(xml_data = tmp.article, tag = "ArticleIdList", format = "char")
      if (is.null(tmp.paperID)) 
      {
        message("An error occurred")
        return(NULL)
      } else {
        tmp.paperID <- gsub("[[:space:]]", "", tmp.paperID[1])
      }
      
      # Get PMID
      tmp.PMID <- gsub("^(.*ArticleIdIdType=\\\"pubmed\\\")([[:space:]]|[[:alnum:]]){0,20}>", "", tmp.paperID)
      tmp.PMID <- gsub("<.*$", "", tmp.PMID)
      
      # Get DOI
      tmp.DOI <- gsub("^(.*ArticleIdIdType=\\\"doi\\\")([[:space:]]|[[:alnum:]]){0,20}>", "", tmp.paperID)
      tmp.DOI <- gsub("<.*$", "", tmp.DOI)
      
      # Get Journal Abbrv
      tmp.jabbrv  <- custom_grep(xml_data = tmp.article, tag = "ISOAbbreviation", format = "char")
      tmp.jabbrv <- ifelse(is.null(tmp.jabbrv), NA, tmp.jabbrv)
      
      # Get Title
      tmp.journal <- custom_grep(xml_data = tmp.article, tag = "Title", format = "char")
      tmp.journal <- ifelse(is.null(tmp.journal), NA, tmp.journal)
      
      # Fetch Keywords ----MeshHeading
      tmp.keys <- tryCatch({
        if (getKeywords) {
          tmp.keys <- custom_grep(xml_data = tmp.article, 
                                  tag = "Keyword", 
                                  format = "char")
          
          tmp.mesh <- custom_grep(xml_data = tmp.article, 
                                  tag = "MeshHeading", 
                                  format = "char")
          
          if (length(tmp.mesh) > 0) {
            tmp.mesh <- sapply(tmp.mesh, function(xxm) {
              custom_grep(xml_data = xxm, 
                          tag = "DescriptorName", 
                          format = "char")
            })
          }
          
          tmp.keys <- c(tmp.keys, tmp.mesh)
          
          if (length(tmp.keys) > 1) {
            tmp.keys <- paste(tmp.keys, collapse = "; ")
          } else if (length(tmp.keys) < 1) {
            tmp.keys <- NA
          }
        } else {
          NA
        }
      }, error = function(e) {NA})
      
      # vector with all unique fields extracted o far
      tmp.resout <- c(pmid=tmp.PMID, 
                      doi=tmp.DOI, 
                      title=tmp.title,
                      abstract=tmp.abstract,
                      year = as.vector(tmp.date[1]),
                      month = as.vector(tmp.date[2]),
                      day = as.vector(tmp.date[3]),
                      jabbrv=tmp.jabbrv,
                      journal=tmp.journal,
                      keywords=tmp.keys)
      
      # Slow part - authors
      tmp.authors <- custom_grep(xml_data = tmp.article, tag = "AuthorList", format = "char")
      
      if (length(tmp.authors) < 1 | !getAuthors) {
        # Set every placeholder with NA
        final.mat <- data.frame(rbind(c(tmp.resout, 
                                        lastname=NA, 
                                        firstname=NA, 
                                        address=NA, 
                                        email=NA)), stringsAsFactors = FALSE)
      } else {
        author.list <- custom_grep(xml_data = tmp.authors, tag = "Author", format = "char")
        final.mat <- do.call(rbind, lapply(author.list, (function(al) {
          tmp.lastnm <- custom_grep(xml_data = al, tag = "LastName", format = "char")
          tmp.firstnm <- custom_grep(xml_data = al, tag = "ForeName", format = "char")
          email.PAT <- "([[:alnum:]]|\\.|\\-\\_){3,200}@([[:alnum:]]|\\.|\\-\\_){3,200}(\\.)([[:alnum:]]){2,6}"
          tmp.email <- regexpr(email.PAT, al)
          if (tmp.email > 0) {
            tmp.email <- substr(al, tmp.email, tmp.email + attributes(tmp.email)$match.length -1 )
            al <- gsub(email.PAT, "", al)
            
          } else {
            tmp.email <- NA
          }
          #
          if (regexpr("Affiliation", al) > 0) {
            tmp.add <- custom_grep(al, "Affiliation", format = "char")[1]
            tmp.add <- trim_address(tmp.add)
          } else {
            tmp.add <- NA
          }
          c(tmp.resout, 
            lastname=tmp.lastnm, 
            firstname=tmp.firstnm, 
            address=tmp.add, 
            email=tmp.email)
          #
        })))
        rownames(final.mat) <- NULL
        
        final.mat <- data.frame(final.mat, stringsAsFactors = FALSE)
        DESELECT <- is.na(final.mat$lastname) | is.na(final.mat$firstname)
        if (length(DESELECT) > 0 & sum(DESELECT) > 0)
          final.mat <- final.mat[!DESELECT, ]
        #
        if (autofill){
          tmp.address <- final.mat[,"address"]
          na.pos <- is.na(tmp.address)
          if (sum(na.pos) != length(tmp.address)) {
            tmp.list <- lapply(tmp.address, function(x) {x} ) 
            cur.add <-  tmp.list[[(which(!na.pos)[1])]]
            for (i in 1:length(na.pos)){
              if(na.pos[i]){
                tmp.list[[i]] <- cur.add
              } else {
                cur.add <- tmp.list[[i]]
              }
            }
            final.mat[,"address"] <- do.call(c, tmp.list)
          }
        }
      }
      
      # Final check and return
      if (ncol(final.mat) != 14) {
        final.mat <- NULL
      }
    }, error = function(e) {NULL}, 
    finally = {
      options(warn = currWarn)
      return(final.mat)
    })
  }