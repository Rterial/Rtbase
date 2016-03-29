mtrl.colors <- function(){
  
  if(file.exists('data/mtrl_colors.rds')){
    mtrl_colors <- readRDS('data/mtrl_colors.rds')
  }else {
    drop_get('rterial/colors/materialize_color_df.rds','data/mtrl_colors.rds')
    mtrl_colors <- readRDS('data/mtrl_colors.rds')
  }
  
  mtrl_colors <- apply(mtrl_colors,2,stri_trim_both) %>%
    as.data.frame(stringsAsFactors = FALSE)
  
  google_ind <- unique(mtrl_colors$color_mod) %>%
    chain.df('ref') %>%
    mutate(levels =
             c(50,100,200,300,400,600,700,800,900,
               'A100','A200','A400','A700',
               500,50,100,200,300,400,600,700,800,900,
               'A100','A200','A400','A700',
               'black','white','transparent',
               'black','white','transparent')
    )
  
  mtrl_colors <-
    mtrl_colors %>%
    mutate(
      google_colors =
        google_ind[
          match(mtrl_colors$color_mod,google_ind$ref),
          'levels']
    )
  
  assign("mtrl_colors",mtrl_colors,envir = .GlobalEnv)
  
  return(mtrl_colors)
  
}

mtrl.colors_theme <- function(theme_set = 'light',
                              prime_set = 'blue',
                              accent_set = 'pink',
                              warn_set = 'orange'){
  
  if(!exists('mtrl_colors')){
    mtrl.colors()
  }
  
  var_lvl <- list(accent = c("A200","A100","A400","A700"),
                  primary = c('500','300','800','A100'),
                  warn = c('500','300','800','A100'))
  
  
  theme_var <- list(
    dark = c("#000000","#212121","#303030","#424242"),
    light =  c('#E0E0E0','#F5F5F5','#FAFAFA','#FFFFFF'))
  
  
  prime_stroke <-
    mtrl_colors[mtrl_colors$color_name == prime_set,]  %>%
    filter(google_colors == '500' |
             google_colors == '300' |
             google_colors == '800' |
             google_colors == 'A100') %>%
    select(hex) %>% mutate(pal_name = 'primary',hue_lvl = 1:4) %>%
    select(pal_name,hue_lvl,hex)
  
  warn_stroke <-
    mtrl_colors[mtrl_colors$color_name == warn_set,] %>%
    filter(google_colors == '500' |
             google_colors == '300' |
             google_colors == '800' |
             google_colors == 'A100') %>%
    select(hex) %>% mutate(pal_name = 'warn',hue_lvl = 1:4) %>%
    select(pal_name,hue_lvl,hex)
  
  accent_stroke <-
    mtrl_colors[mtrl_colors$color_name == accent_set,] %>%
    filter(google_colors == 'A200' |
             google_colors == 'A100' |
             google_colors == 'A400' |
             google_colors == 'A700') %>%
    select(hex) %>% mutate(pal_name = 'accent',hue_lvl = 1:4) %>%
    select(pal_name,hue_lvl,hex)
  
  pals <- rbind(prime_stroke,warn_stroke)
  
  pals <- rbind(pals,accent_stroke)
  
  var_base <- theme_var[[theme_set]] %>%
    chain.df('hex') %>%
    mutate(pal_name = 'theme_master',
           hue_lvl = 1:4) %>%
    select(pal_name,hue_lvl,hex)
  
  pal_out <- rbind(pals,var_base)
  
  pal_out$color_name <- c(rep(prime_set,4),
                          rep(warn_set,4),
                          rep(accent_set,4),
                          rep(theme_set,4)
  )
  
  return(pal_out)
}


#' Inline css builder function
#'
#'
#' \code{mtrl.build_css}
#'
#' @examples
#' > mtrl.build_css(list(background_color = "blue",width=100))
#' [1] "background-color:blue;width:100"
#'
#'
mtrl.build_css <- function(...){
  
  options('useFancyQuotes' = FALSE)
  
  raw <- HTML(paste0(unlist(lapply(list(...),function(i)
    paste(gsub("_","-",names(i)),i,sep=":",collapse = ";")))))
  
  HTML(paste0("style=",dQuote(raw)))
}


#' Apply a list of named attribs to a tag
#'
#'\code{mtrl.build_attrs}
#'
#'@examples
#' > mtrl.tag_paste(c(class = "container",id = "new_id"))
#'
#' [1] class="container" id="new_id"
mtrl.build_attrs <- function(...){
  
  options('useFancyQuotes' = FALSE)
  
  HTML(sapply(list(...),function(i)
    HTML(paste0(names(i),"=",dQuote(i),sep = " ") %>%
           stri_trim_both))
  )
  
}


#' Generate waves css from R
#'
#' \code{mtrl.css_waves}
#'
mtrl.css_waves <- function(color_name,color_hex){
  HTML(paste0(".waves-effect.waves-",
              color_name," ",".waves-ripple {\n\t\tbackground-color:",
              lapply(color_hex %>% toupper %>% as.character,function(i)
                paste0("rgba(",paste0(col2rgb(i),collapse = ","),
                       ",0.67);\n}")
              ),
              collapse = "\n")
  )
}


#' Set opacity on a color and return either a usable color or pure css
#'
#' \code{mtrl.css_opacity}
#'
#'
#'
#' @examples
#'

mtrl.css_opacity <- function(color_name = NULL,color_value = NULL,opacity = NULL,
                             css_var = c('background-color'),
                             write_css = FALSE){
  
  measure.between <- function(var_obj,small,big){
    all(c(is.smaller = var_obj < big,
          is.bigger = var_obj > small) == TRUE)
    
  }
  
  if(measure.between(opacity,small = 0, big = 1)){
    opace <- opacity
  }else{
    t.opace <- opacity/10
    if(t.opace > 1){
      opace <- opacity/100
    }else{
      opace <- t.opace
    }
  }
  
  if(!is.null(color_value)){
    color_line <-
      lapply(color_value,function(i)
        paste0("rgba(",paste0(col2rgb(i),collapse = ","),
               ",",opace,
               ")")
      )
  }
  
  lum.color <- c(color_line)
  
  if(!is.null(color_name)){
    names(lum.color) <- color_name
  }
  
  if(write_css){
    # To see if we're creating a border or alternative rule or
    # if it's just the color syntax
    # eg. css_var = "border-bottom:1px solid" - vs - "background-color"
    
    css_inner <- ifelse(grepl(":",css_var),css_var,":")
    lum.color <-
      pure.html(
        paste0(".", color_name, " ", "{\n",
               "\t\t",css_inner, color_line,";\n",
               "}\n")
      )
  }
  
  return(lum.color)
  
}



mtrl.hex_to_opace <- function(hex = NULL,opacity = 0.5){
  paste0("rgba(",paste0(col2rgb(hex),collapse = ","),",",opacity,")")
}





#' Display the color pallets at the console
#' 
mtrl.show_colors <- function(){
tagList(tags$link(href = "http://www.google.com/design/spec/static/sites/spec/css/color-palettes.css",rel = "stylesheet"),
HTML(llply(res$content%>%rawToChar %>% paste0(collapse = "") %>% 
stri_replace_all_regex('  |\n','') %>% unlist %>%
as.character %>% paste0(collapse = "") %>% pure.html %>% 
stri_extract_all_regex(tag.rgx('section')),function(i)
i[grep('color-group',i)])[[1]] %>% 
paste0(collapse = "")))%>%html_print

}
