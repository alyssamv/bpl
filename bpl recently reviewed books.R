library(purrr)
library(stringr)
library(RCurl)
library(plyr); library(dplyr)


get.books <- function(url.bpl = 'https://bpl.bibliocommons.com/'){
  trial.run <- lapply(url.bpl, function(x){
    RCurl::getURL(x, ssl.verifypeer = FALSE);(Sys.sleep(4));RCurl::getURL(x, ssl.verifypeer = FALSE)
  })
  
  ### extraxt list of recently reviewed books  
  datalines <- lapply(1, function(i){
    zz <- textConnection(trial.run[[1]])
    datalines <- readLines(zz)
    close(zz)
    return(datalines)
  })
  
  ### get title and author of listed books
  data.collapse <- paste(datalines[[1]], collapse='')
  books.xml <- unlist(regmatches(data.collapse, gregexpr('class=" icon-book-open">([^*?]+?)<div class="author small">([^*?]+?)</div>', data.collapse))  )
  title.author <- lapply(seq_along(books.xml), function (i){
    title = (stringr::str_match(books.xml[[i]], '<div class="title">(.*?)</div>')[,2])
    title = gsub('&#x27;', "'", title)

    author = (stringr::str_match(books.xml[[i]], '<div class="author small">(.*?)</div>')[,2])
    return(c(title,author))
  })
  
  ### make dataframe
  tt <- as.data.frame(do.call('rbind', title.author)) %>% unique
  colnames(tt) <- c('Title','Author')
  (tt)
}

get.books()
