#' Convert a vector or string into pure html encoded output.
#'
#' \code{pure.html}
#'
#' @family Web Helpers
#'
#' @export
pure.html <- function (text, ...) {
  htmlText <- c(text, as.character(list(...)))
  htmlText <- paste(htmlText, collapse = " ")
  attr(htmlText, "html") <- TRUE
  class(htmlText) <- c("html", "character")
  htmlText
}





#' Reference for escape charset tables
#'
#' \url{"http://www.theukwebdesigncompany.com/articles/entity-escape-characters.php}
#'
#' @return
#' dataframe with Symbol - Code - Entity Name
#'
#' data also stored in
#'
#' paste(dirs.r_home('shiny-server/shinyone'),
#' c("/inst/data/escape_char.html",
#' "/inst/data/escape_char.json"),
#' sep = "")
#'
#'
escape.html_tbl <- function(){
  rel_base <-
    paste0('http://www.theukwebdesigncompany.com/articles/',
           'entity-escape-characters.php',
           collapse = "")

  (html(rel_base) %>%
     html_table)[[1]]

}




escape.html <- function(text){
      a <- gsub('&', '&amp;',text)
      b <- gsub('<', '&lt;',a)
      c <- gsub('>', '&gt;',b)
      d <- gsub("'", '&#39;',c)
      e <- gsub('"', '&quot;',d)
      f <- gsub(' ',"&nbsp;")
      e
}
# ---- test-b ----

paste.html <- function(...){
  pure.html(paste0(...,collapse=""))
}

as.html <- pure.html

parse.named <- function(x) {
  if (is.null(names(x))) return(NULL)
  x[names(x) != ""]
}

parse.unnamed <- function(x) {
  if (is.null(names(x))) return(x)
  x[names(x) == ""]
}

mtrl.tag_src <- function(){
  pair_tags <- c("a", "abbr", "address", "article", "aside", "audio", "b", "bdi",
                 "bdo", "blockquote", "body", "button", "canvas", "caption", "cite",
                 "code", "colgroup", "data", "datalist", "dd", "del", "details",
                 "dfn", "div", "dl", "dt", "em", "eventsource", "fieldset", "figcaption",
                 "figure", "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6",
                 "head", "header", "hgroup", "html", "i", "iframe", "ins", "kbd",
                 "label", "legend", "li", "mark", "map", "menu", "meter", "nav",
                 "noscript", "object", "ol", "optgroup", "option", "output", "p",
                 "pre", "progress", "q", "ruby", "rp", "rt", "s", "samp", "script",
                 "section", "select", "small", "span", "strong", "style", "sub",
                 "summary", "sup", "table", "tbody", "td", "textarea", "tfoot",
                 "th", "thead", "time", "title", "tr", "u", "ul", "var", "video")

  solo_tags <- c("area", "base", "br", "col", "command", "embed", "hr",
                 "img", "input", "keygen", "link", "meta", "param",
                 "source", "track", "wbr")

  assign("solo_tags",solo_tags,envir = .GlobalEnv)
  assign("pair_tags",pair_tags,envir = .GlobalEnv)
}


tag.rgx <- function(x){
  if(is.tag_solo(x)){
    sprintf("<%s (\\S+)=[\"']?((?:.(?![\"']?\\s+(?:\\S+)=|[>\"']))+.)[\"']? />",x)
  }else{
    sprintf("<%s\\b[^>]*>(.*?)</%s>",x,x)
  }
}

tag.capture_group <- function(start,end){
  sprintf("<%s\\b[^>]*>(.*?)</%s>",start,end)
}

match2 <- function(var_in,find_in){

  a <- grep(var_in,find_in,value = TRUE)

  if(length(a) > 1){
    a <- unlist(lapply(1:length(a),function(i)
      a[[i]][identical(var_in,a[[i]])]))
  }

  if(length(a) == 0){
    a <- FALSE
  }
  return(a)
}

is.tag_pair <- function(...){
  if(match2(...,pair_tags)!=FALSE){
    TRUE
  }else{
    FALSE
  }
}
is.tag_solo <- function(...){
  if(match2(...,solo_tags)!=FALSE){
    TRUE
  }else{
    FALSE
  }
}

mtrl.valid_tag <- function(tag_in,...){

  if(length(...)!=0){
    inners <- mtrl.tag_inners(...)
  }else{
    inners <- list(attrs = '',contents = '')
  }

  if(is.tag_pair(tag_in) != FALSE){
    pure.html(paste0("<",tag_in,inners$attrs,">",inners$contents,"</",tag_in,">"))
  }else{
    pure.html(paste0("<",tag_in,inners$attrs,"/>"))
  }
}


mtrl.tag_inners <- function(...){
  options("useFancyQuotes" = FALSE)
  A <- c(...)
  B <- A[nchar(names(A))>1]
  C <- A[nchar(names(A))==0]
  list(attrs =
         paste0(lapply(names(B),function(i)
           paste0(" ",i,"=",dQuote(B[[i]]))),
           collapse=""),
       contents = ifelse(length(C)>=1,C,""))
}



#'
#'@examples
#'B <- "https://scontent-atl3-1.xx.fbcdn.net/hprofile-xat1/v/t1.0-1/p160x160/10368257_10205110776266817_7994776168540927627_n.jpg?oh=e15a909f1d078b2925620ab37bbb8684&oe=575E162D"
#'mtrl.valid_tag("a",c(class = "container",href="http://www.facebook.com",
#'                style="height:15px;background-color:black",
#'                mtrl.valid_tag("img",c(src=B))))%>%html_print
#'
mtrl.tag_src()
