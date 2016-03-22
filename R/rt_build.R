#' Universal constants for package development
#'
#' \code{rt.check_scaffold}
#'
rt.scaffold <- function(){

  dirs <-
    list(
      inst = list(
        www = list('style','js','images','templates'),
        shiny = list('templates','ace','fragments'),
        examples = list(''),
        data = list('style_tables','index_tables')),
      revdep = list(
        tests = list('shiny_tests','R test','scraps')
      )
    )

  dirs.structure <- llply(names(dirs),function(i)
    sapply(names(dirs[[i]]),function(x)
      c(i,paste0(i,"/",x),
        paste0(i,"/",x,"/",
               dirs[[i]][[x]]))
    )
  ) %>%
    unlist %>%
    as.character %>%
    unique

  sapply(dirs.structure,function(x)
    suppressWarnings(dir.create(x))
  )

  dir.create('inst/www/templates/todo')
  dir.create('inst/www/templates/datatable')
  invisible()

}



#' Set up material design resources in the new directory
#'
#'
#' \code{rt.setup}
#'
rt.setup <- function(){

  if(!file.exists('inst')){
    rt.scaffold()
  }

  rt_res <- list('mtrl_icons_svg.rds','mtrl_icons.rds','mtrl_colors.rds')
  rds_base <-
    lapply(rt_res,function(i)
      curl::curl_download(
        sprintf("https://github.com/Rterial/Rtbase/blob/master/inst/data/%s?raw=true",i),
        sprintf('inst/data/style_tables/%s',i)
    ))
  # now load into the workspace
  lapply(rt_res,function(i)
    assign(gsub('.rds','',i),
           readRDS(
             sprintf('inst/data/style_tables/%s',i)
             ),
           envir = .GlobalEnv)
  )
  r_rt_res <- list("MTRL_TAGS.R","MTRL_TABLE.R","MTRL_COLORS.R")

  rt_html_r <-
    lapply(r_rt_res,function(i)
           readLines(
             sprintf('https://raw.githubusercontent.com/Rterial/Rtbase/master/R/%s',i)
           ) %>% paste0(collapse = "\n") %>% HTML %>% write(paste0('R/',i))
    )

  # Now source the functions into the environment
  lapply(paste0("R/",r_rt_res),source)

  cp.file_types <- list(".html",".js",".css")
  rt_todo <-
    suppressWarnings(
      lapply(cp.file_types,function(i)
        sprintf("http://codepen.io/CarlBoneri/pen/mVMbEY%s",i) %>%
          readLines %>% paste0(collapse = "\n") %>% HTML %>%
          write(paste0("inst/www/templates/todo/todo_app",i))
      )
    )

  rt_datatable <-
    suppressWarnings(
      lapply(cp.file_types,function(i)
        sprintf("http://codepen.io/CarlBoneri/pen/zqZYqR%s",i) %>%
          readLines %>% paste0(collapse = "\n") %>% HTML %>%
          write(paste0("inst/www/templates/datatable/mtrl_dt",i))
      )
    )
      invisible()



}
