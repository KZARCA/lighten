
#' @export
extract_from_SIGAPS <- function(x, annee_max = 2016, ...){
  file <- read_html(x, ...)
  big_table <- extract_big_table(file, annee_max)
  disciplines <- get_disciplines(file)
  return(list(big_table = big_table, disciplines = disciplines))
}

extract_big_table <- function(x, annee_max){
  node_table <- html_node(x, "table.analyse_cartouche")

  big_table <- node_table %>%
    html_table(fill = TRUE) %>%
    slice(-(1:2))

  names(big_table) <- c("NLMid", "ISSN", "ESSN", "Titre",
                        paste(rep(c("IF", "Cat"), times =  4),
                              rep((annee_max-4):(annee_max), each=2)), "Disciplines")

  big_table$Disciplines <-str_split(big_table$Disciplines, " ")

  big_table$`Full title` <- node_table %>%
    html_nodes("td") %>%
    html_attr("title") %>%
    discard(is.na)

  return(big_table)
}

get_disciplines <- function(x){
  all_lines <- html_node(x, "table.info") %>%
    str_split("<br>") %>%
    flatten_chr() %>%
    str_replace_all("</?b>", "") %>%
    str_replace_all("&amp;", "&")

  disciplines <- all_lines[str_which(all_lines, "^-")] %>%
    str_replace("^- ", "") %>%
    str_split_fixed(" : ", 2)

}
