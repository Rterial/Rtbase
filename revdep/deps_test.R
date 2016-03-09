depends_tests <- function (unlist, as.character, .GlobalEnv, row.names) {
  # Test 
  # 
  css_is_live <- function(){
    
    var_paths <- list("css/materialize.min.css", "js/materialize.min.js")
    
    suppressWarnings(
      all(
        sapply(
          sapply(
            var_paths,
            function(i) 
              readLines(
                sprintf("https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/%s",i)
              ) %>% paste0(collapse = "")
          ), function(xx)
            data.frame(
              Nchar = nchar(xx)
            )
        ) > 1)
    )
  }
  
  mtrl_is_live <- function(){
    
    
    srcs_rds <- list("mtrl_icons.rds", "mtrl_colors.rds", 
                     "mtrl_icons_svg.rds")
    
    search_paths <- sapply(repo_is_local(),function(n)
      paste0(Sys.getenv('R_LIBS_USER'), "/",n,collapse = ""))
    
    scrum <- lapply(1:length(srcs_rds),function(n)
      sapply(search_paths,function(i)
        grep(srcs_rds[[n]], 
             list.files(recursive = T,path = i,full.names = T),value = T)
      )) %>% unlist %>% as.character
    
    if(length(scrum[match(srcs_rds,
                          stri_extract_last_boundaries(scrum))
                    ]) == length(srcs_rds)
    ){
      if(!interactive()){
        TRUE
      }else{
        sapply(1:length(scrum),function(xx)
          assign(
            gsub(".rds","",stri_extract_last_boundaries(scrum[[xx]])),
            readRDS(scrum[[xx]]),
            envir = .GlobalEnv)
        )
        invisible()
      }
      
    }
    
  }
  
  repo_is_local <- function(){
    aa <- installed.packages() %>% row.names
    bb <- list.files(Sys.getenv("R_LIBS_USER"))
    bb[is.na(match(bb,aa))]
  }
}

