#' @export
get_all_articles_tbl <- function(journal_id, retmax = 1e6){
  articles <- get_journal_articles(journal_id, retmax)
  meta <- articles %>%
    get_article_meta()
  tbl <- get_article_history(meta)
  mesh <- get_article_mesh(meta)
  tbl$mesh <- mesh
  tbl$uid <- articles
  return(tbl)
}


get_journal_articles <- function(journal_id, retmax){
  paste0("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&retmode=json&retmax=", format(retmax, scientific = FALSE),"&term=", journal_id, "%5BJournal%5D") %>%
    GET() %>%
    content() %>%
    extract2("esearchresult") %>%
    extract2("idlist") %>%
    flatten_chr()
}

get_article_meta <- function(article_id){
  chunks <- split(article_id, ceiling(seq_along(article_id)/100))
  meta <<- map(chunks, function(x){
    Sys.sleep(2)
    uids <- paste(x, collapse = ",")
    paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&retmode=xml&id=", uids) %>%
      read_xml()
  })

  l <- length(meta)
  if (l > 1){
    map(seq.int(2, l), function(i){
      children <- xml_children(meta[[i]])
      map(children, function(x){
        xml_add_child(meta[[1]], x)
      })
    })
  }
    return(meta[[1]]) #couldn't find anything more intuitive
}

get_article_history <- function(meta){
  milestones <- c("received", "accepted", "pubmed")
  history <- html_nodes(meta, "History")
  map(milestones, function(x){
         xpath <- sprintf(".//PubMedPubDate[@PubStatus='%s']", x)
         dates <- xml_find_first(history, xpath)
         Y <- xml_find_first(dates, ".//Year") %>%
           html_text()
         m <- xml_find_first(dates, ".//Month") %>%
           html_text() %>%
           as.numeric() %>%
           formatC(width=2, flag="0")
         d <- xml_find_first(dates, ".//Day") %>%
           html_text() %>%
           as.numeric() %>%
           formatC(width=2, flag="0")
         ifelse(!is.na(Y), paste(Y, m, d, sep = "-"), NA)
  }) %>%
    setNames(milestones) %>%
    dplyr::as_tibble()
}

get_article_mesh <- function(meta){
  article <- html_nodes(meta, "PubmedArticle")
  map(article, function(x){
    mesh_list <- html_node(x, "MeshHeadingList")
    if (is.na(mesh_list)) NULL
    else mesh_list
  })
}


