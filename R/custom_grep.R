#' @title Retrieve Text Between XML Tags
#'
#' @description Extract text form a string containing XML or HTML tags. Text 
#' included between tags of interest will be returned. If multiple tagged substrings are found, 
#' they will be returned as different elements of a list or character vector.
#' 
#' @usage custom_grep(xml_data, tag, format = "list")
#' 
#' @param xml_data String (of class character and length 1): corresponds to the PubMed 
#' record or any string including XML/HTML tags.
#' @param tag String (of class character and length 1): the tag of interest (does NOT include < > chars).
#' @param format c("list", "char"): specifies the format for the output.
#' 
#' @details 
#' The input string has to be a character string (length 1) containing tags (HTML or XML format). 
#' If an XML Document is provided as input, the function will rise an error.
#' 
#' @return 
#' List or vector where each element corresponds to an in-tag substring.
#' 
#' @author Damiano Fantini \email{damiano.fantini@@gmail.com}
#'
#' @references \url{https://www.data-pulse.com/dev_site/easypubmed/}
#'
#' @examples 
#' try({
#'   ## extract substrings based on regular expressions
#'   string_01 <- "I can't wait to watch the <strong>Late Night Show with" 
#'   string_01 <- paste(string_01, "Seth Meyers</strong> tonight at <strong>11:30</strong>pm CT!")
#'   print(string_01)
#'   custom_grep(xml_data = string_01, tag = "strong", format = "char")
#'   custom_grep(xml_data = string_01, tag = "strong", format = "list")
#' }, silent = TRUE)
#' 
#' @export
custom_grep <-
  function(xml_data, 
           tag, 
           format = "list")
  {
    x <- xml_data[[1]]
    tag.op <- paste("\\<", tag, "((\\>)|([[:space:]]([^[<]]*)\\>))", sep = "")
    tag.cl <- paste("(<\\/)", tag, "(\\>)", sep = "")
    #
    out.result <- list()
    i = 1
    while (!is.null(x) &&
           !is.na(x) &&
           x != "" &&
           nchar(x) > 0 &&
           regexpr(tag.op, x) > 0 &&
           regexpr(tag.cl, x) > 0){
      tag.op.pos <- regexpr(tag.op, x)
      nu.x <- substr(x, (tag.op.pos - 1), nchar(x))
      inner.trim <- regexpr(">", nu.x, fixed = TRUE)
      nu.x <- substr(nu.x, (inner.trim + 1), nchar(nu.x))
      #
      tag.cl.pos <- regexpr(tag.cl, nu.x)
      tag.cl.full <- tag.cl.pos + attributes(tag.cl.pos)$match.length + 1
      x <- substr(nu.x, tag.cl.full, nchar(x))
      nu.x <- substr(nu.x, 1, (tag.cl.pos - 1))
      #
      out.result[[i]] <- nu.x
      i <- i + 1
    }
    if (format != "list") {
      out.result <- do.call(c, out.result)
    }
    return(out.result)
  }