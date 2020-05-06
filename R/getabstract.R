#' This script is for a function to take a doi input and scrape back an abstract from the
#' publisher's website based on the presence of a subheading and the layout of the text.

#' @description Scrape abstract from publisher's website.
#' @param doi A string of length 1 containing the doi for a journal article
#' @return A string containing the abstract text. May include some other publisher text, including citation information and some background text where abstracts cannot be parsed easily.
#' @export

getabstract <- function(doi = NULL) {
      text <- htm2txt::gettxt(doi)
      lines <- strsplit(text, "\n")[[1]]
      if(any(lines %in% c(" ", ""))){
        lines <- lines[-which(lines %in% c(" ", ""))]
      abstract <- grep("abstract", tolower(lines))
      abstract <- abstract[which.min(nchar(lines[abstract]))]
      if(grepl("Background|Introduction",stringr::str_sub(lines[abstract+1],1,10))) {
        if(grepl("Background",lines[abstract+1])) {
          background <- grep("background", tolower(lines))[2]
          abstract <- paste(lines[abstract:(background-1)], collapse = "// ")
        } else {
          introduction <- grep("introduction", tolower(lines))[2]
          abstract <- paste(lines[abstract:(introduction-1)], collapse = "// ")
        }
      } else {
        abstract <- if(any(grepl("Background|Introduction|References|This is a preview of subscription content", lines[abstract+2:8]))) {
          abstract <- lines[abstract+1]
        } else {
          abstract <- lines[abstract:(abstract+10)]
          abstract <- paste(abstract, collapse = ".")
          if(ngram::wordcount(abstract)<350){
            abstract <- abstract
          } else {
            abstract <- stringr::word(string = abstract, start = 1, end = 350, sep = stringr::fixed(" "))
          }
        }
      }
    }

}
