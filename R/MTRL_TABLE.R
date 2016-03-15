text.cap <- function(x){
  paste0(toupper(substring(x,1,1)),
         substring(x,2,nchar(x))
  )
}

rt.tbl <- function(df){
  
  nm.in <- colnames(df)
  
  prep.th <- lapply(nm.in,function(i)
    paste("<th>",
          paste0(
            text.cap(
              strsplit(i,"[[:punct:]]")[[1]]),
            collapse = " "), 
          "</th>", 
          sep="")
    ) %>% unlist %>% 
    paste0(collapse="")
  
  theads <- sprintf("<thead>\n<tr>\n%s\n</tr>\n</thead>",prep.th)
  
  prep.tbody <- 
    ldply(1:nrow(df),function(i)paste0("<td>",df[i,],"</td>"))
  
  tmp.dd <- tags$div(class="col s4",
                     tags$a(class='dropdown-button btn',
                            href='#',`data-activates`='dropdown1',"DROP"),
                     tags$ul(id='dropdown1',class='dropdown-content',
                             tags$li(tags$a(href="#!",'one')),
                             tags$li(tags$a(href="#!",'two')),
                             tags$li(tags$a(href="#!",'three')),
                             tags$li(tags$a(href="#!",'four')
                                     )
                             )
  )

  
  tbodied <- 
    paste0("<tbody>",
           unlist(
             llply(1:nrow(prep.tbody),function(xx)
               paste0(c("<tr>",prep.tbody[xx,],"</tr>")
                      )
               )
             ) %>% 
             paste0(collapse = "\n"),
           "</tbody>", 
           sep="")
  
  tfoots <- paste0("<tfoot style='height:600px;overflow:visible;'><tr>",
                   '<td>',rt.sel(main_label= "HI",
                          rt.opt(val = AB$vals,label=AB$labs)),
                   '</td>',"<td>",tmp.dd,"</td>",
                   "</tr></tfoot>")
  
  sprintf('<table class="striped highlight">%s</table>',
          paste0(theads,tfoots,tbodied,collapse = ""))
  
}

rt.opt <- function(label,val){
  sprintf('<option value="%s">%s</option>',val,label)%>%paste0(collapse="\n")
        
}
rt.sel <- function(main_label,...){
  sprintf(
    '<div class="input-field col s3">\n<select>%s</select><label>%s</label></div>',
    ...,main_label)%>%HTML
}
rt.template <- function(...){
  tags$html(
    tags$head(
      tags$link(href="https://fonts.googleapis.com/icon?family=Material+Icons",
                rel="stylesheet"),
      tags$link(href="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/css/materialize.min.css",
                rel = "stylesheet"),
      tags$script(src="https://code.jquery.com/jquery-2.1.1.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/js/materialize.min.js"),
      tags$script(HTML("$(document).ready(function() {
                       $('select').material_select();
                       $('.dropdown-button').dropdown({
                        inDuration: 300,
                        outDuration: 225,
                        constrain_width: true,
                        hover: true, 
                        gutter: 0, 
                        belowOrigin: false, 
                        alignment: 'left'
                      });
                    });"))),
    tags$body(tags$div(class="container",
                       HTML(...))
              )) %>%html_print
}
