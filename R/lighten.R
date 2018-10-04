#' lighten: Lightened choice of journal for publication
#'
#' `lighten` extracts data from medline, with the use IF and SIGAPS score of review
#'
#' @docType package
#' @name lighten
#'
#' @importFrom xml2 read_html
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_add_child
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_find_first
#' @importFrom rvest html_node
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom rvest html_attr
#' @importFrom rvest html_text
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom magrittr "%>%"
#' @importFrom magrittr extract2
#' @importFrom magrittr extract
#' @importFrom dplyr slice
#' @importFrom dplyr as_tibble
#' @importFrom purrr discard
#' @importFrom purrr flatten_chr
#' @importFrom purrr reduce
#' @importFrom purrr map
#' @importFrom stringr str_split
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_which
NULL

globs <- new.env()

if (require("furrr")){
  plan(multiprocess)
  globs$map <- furrr::future_map
  globs$imap <- furrr::future_imap
} else {
  globs$map <- purrr::map
  globs$imap <- purrr::imap
}

