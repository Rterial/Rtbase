# 'This is a neraly perfect css parser.
#'
#'
#'\code{rgx.css}
#'
#'This will extract from a minified or non minified css file and parse into
#'single character vectors each css rule. It is working on '@' media queries as
#'well as most of the other selectors I have checked against.
#'\enumerate{
#'  \item First find anything beginning with a period, open brace follwed by word, @, or word
#'  \item Second capture everything between the opening step and the first open bracket
#'  \item Third Use a trailing capture group between the open brace and first close brace
#'  \item Fourth Be mindful of media queries having two closing braces,so find closing braces that occur
#'  at least once, but could be twice.
#'  }
#'
#'  The pattern finds and group that begins with a:
#'  \itemize{
#'    \item  period "."
#'    \item  open bracket "["
#'    \item  Word
#'  }
#'
#'  Then spans to a boundary with an open brance with a trailing capture group.
#'
#'
#'@aliases \code{\link{parse.css}}
#'
#'
#'
#'@param ... string The raw css to parse. This expects the file has already been
#'  read-in and flattened.
#'
#'
#'@return A character vector with parsed blocks.
#'@examples
#'# Angular material css
#'> ang <- "http://ajax.googleapis.com/ajax/libs/angular_material/1.1.0-rc2/angular-material.css"
#'> Ang <- readLines(ang) %>% paste0(collapse = "")
#'> Ang.parse <- rgx.css(Ang)
#'> grep("checkbox",B,value = T)[8]
#'>  [1] "md-checkbox.md-align-top-left > div._md-container {    top: 12px; }"
#'
#' # from the Materialize css site
#' mtz <- 'https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.6/css/materialize.min.css'
#'
#' > mtrlz <- rgx.css(readLines(mtx) %>% paste0(collapse = "")
#'
#' > grep(".pink",mtrlz,value = T) %>%
#'    chain.df('pink') %>%
#'    mutate(html_output =
#'    sprintf(
#'    paste0('<div style="height:100px;width:375px;%s"><br>',
#'    '<blockquote style="margin-left:50px">%s</blockquote>',
#'    '</div>',collapse = "\n"),
#'    stri_replace_all_regex(pink,"^.*\\{|\\}$",""),
#'    pink)) %>% `$`(html_output) %>%
#'    paste0(collapse = "\n") %>% HTML %>%
#'    html_print
#'
#'  > kable(head(grep(".pink",mtrlz,value = T) %>%
#'          chain.df('pink')),'rst')
#'  ====================================================
#'  pink
#'  ====================================================
#'  .pink{background-color:#e91e63 !important}
#'  .pink-text{color:#e91e63 !important}
#'  .pink.lighten-5{background-color:#fce4ec !important}
#'  .pink-text.text-lighten-5{color:#fce4ec !important}
#'  .pink.lighten-4{background-color:#f8bbd0 !important}
#'  .pink-text.text-lighten-4{color:#f8bbd0 !important}
#'  ====================================================
#'
#'
#'
#'   # To extract chunks and ammend css on the fly
#'tagList(
#'  tags$head(
#'    tags$style(
#'      grep("checkbox", B, value = T) %>%
#'        paste0(collapse = "\n") %>%
#'        HTML)
#'  ),
#'  tags$form(
#'    tags$p(
#'      tags$input(id = "checkit",
#'                 type = "checkbox",
#'                 checked = "", class="filled-in"),
#'      tags$label(`for` = 'checkit'))
#'  )) %>% html_print
#'
#'
#'@author Carl Boneri, \email{carl.boneri@@fas-advisory.com}
#'@family parsers
#' @seealso \code{\link{add}} for binding outputs.
#'@export
rgx.css <- function(...){
  pat <- "(\\.|\\@|\\[|\\w)+(.*?)\\{(.*?)\\}{1,2}"
  stringi::stri_extract_all_regex(...,pat) %>% unlist
}



#' Parse and filter a cascaded stylesheet.
#'
#'
#' \code{css.get_raw_class}
#'
#' This function will take the url or relative path of a
#' css stylesheet and parse it into the classes, ids,
#' selectors and so on. After the stylesheet has been
#' oganized into its groups, the filter applied to the
#' find_group argument will return the body that applies to
#' the query. There are presets available to the function
#' for libraries. To access these simply pass the name of
#' the css library to the pre_set argument.
#'
#'
css.get_raw_class <- function(){

}
