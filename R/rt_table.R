#' RT table
#'
#' \code{rt.thead}
#'
#'
rt.thead <- function(data_in){
  if(is.data.frame(data_in)){
    ths <- llply(colnames(data_in),function(i)sprintf('<th>%s</th>',i))
  }else{
    ths <- llply(names(data_in),function(i)sprintf('<th>%s</th>',i))
  }
  paste.html("<thead>\n\t<tr>\n\t\t",
             paste0(ths,
                    collapse = "\n\t\t"),
             "\n\t</tr>\n\t</thead>")
}
#
# rt.body <- function(data_in){
#
#   if(is.data.frame(data_in)){
#     AB <- as.data.frame(
#       apply(data_in, 2,
#             function(i)
#               sprintf('<td>%s</td>',i)
#       ), stringsAsFactors = FALSE
#     )
#   }else if(length(data_in)[[1]] > 2){
#       data_pre <- llply(names(data_in),function(i){
#         a.t <- adf(data_in[[i]])
#         colnames(a.t) <- i
#         a.t
#       }) %>% adf
#
#       AB <- as.data.frame(
#         apply(data_pre, 2,
#               function(i)
#                 sprintf('<td>%s</td>',i)
#         ), stringsAsFactors = FALSE
#       )
#
#     }else {
#       data_pre <- llply(1:length(data_in),function(i)
#         data.frame(named = names(data_in[i]),
#                    datum = data_in[[i]])
#         ) %>% rbind.pages
#
#       AB <- as.data.frame(
#         apply(data_pre, 2,
#               function(i)
#                 sprintf('<td>%s</td>',i)
#         ), stringsAsFactors = FALSE
#       )
#     }
#
#   }
#
#   paste.html(llply(1:nrow(AB),function(i)
#     HTML(paste0("<tr>\n\t",paste0(AB[i,],collapse = "\n\t"),"\n</tr>")))
#     )
# }


rt.table_prep <- function(data_in, add_checks = FALSE){

  if(add_checks){

    if(!exists('icon_master')){
      rt.icon_load()
    }

    trash_icon <- icon_master %>%
      filter(icon_type == 'svg',
             icon_name == 'delete_sweep',
             icon_size == "24") %>%
      `$`(icon_html)

    check_idx <- lapply(1:nrow(data_in),function(i)
      rt.check(id = i,checked = FALSE)) %>% unlist

    set_named <- c(trash_icon,colnames(data_in))

    data_in <- data.frame(check_idx, data_in)

    colnames(data_in) <- set_named
  }

  if(is.list(data_in) & !is.data.frame(data_in)){
    if(length(data_in[[1]])>2){
      dat <- adf(apply(adf(data_in),2,function(i)sprintf('<td>%s</td>',i)))
    }else{
      dat <- llply(1:length(data_in),function(i)
        data.frame(
          named = sprintf('<td>%s</td>',names(data_in[i])),
          datum = sprintf('<td>%s</td>',data_in[[i]])
        )
      ) %>% rbind.pages


    }
  }else{
    dat <- adf(apply(data_in, 2, function(i)sprintf('<td>%s</td>',i)))
  }

  dat <- list(heads = paste0(paste0("<th>", names(dat), "</th>"), collapse = "\n"),
              bods = dat)

  dthead <- paste.html(paste0("<tr>\n\t",paste0(dat$heads,collapse = "\n\t"),"\n</tr>"))


  dtbody <- paste.html(llply(1:nrow(dat$bods),function(i)
    HTML(paste0("<tr>\n\t",paste0(dat$bods[i,],collapse = "\n\t"),"\n</tr>"))
  ))

  tbl.li <- list(heads = dthead,bods = dtbody)
  return(tbl.li)
}


rt.check <- function(id,class= NULL,label = NULL,checked = FALSE){

  blocks <- list(wrap = '<p>\n\t%s\n\t%s\n</p>',
                 check = '<input type="checkbox" id="%s" class="%s" checked="%s" />',
                 label = '<label for="%s">%s</label>')

  vars <- list(id = id,
               class = ifelse(!is.null(class),class,'filled-in'),
               label = ifelse(!is.null(label),label,""),
               status = ifelse(!checked,"","checked")
  )

  make_input <- sprintf(blocks$check,vars$id,vars$class,vars$status)

  make_label <- sprintf(blocks$label, vars$id, vars$label)

  make_box <- sprintf(blocks$wrap,make_input,make_label)

  HTML(make_box)
}




rt.table_print <- function(tbl_li = NULL, id = NULL, class = "highlight"){
  tags$html(
    tags$head(
      rt.make_head('materialize'),
      rt.icon_head('head')
    ),
    tags$div(class="container",
             tags$table(id = id,class = class,
                        tags$thead(tbl_li[[1]]),
                        tags$tbody(tbl_li[[2]])
             ))
  )

}
