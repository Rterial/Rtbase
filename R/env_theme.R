#' Resource setup for global material design colors
#'
#' \code{rt.deps_colors}
#'
rt.deps_colors <- function(){
  git.src <- 'https://github.com/Rterial/Rtbase/blob/master/inst/data/mtrl_colors.rds?raw=true'
  clr_file <- grep('mtr_colors.rds',
                   list.files(recursive = T,full.names = T),
                   value = T)

  if(length(clr_file) == 1){
    mtrl_colors <- readRDS(clr_file)
  }else{
    mtrl_colors <- readRDS(curl::curl_download(git.src,'mtrl_colors.rds'))
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

  assign("rt_colors",mtrl_colors,envir = .GlobalEnv)
}

#' Global theme configuration
#'
#' \code{rt.set_theme}
#'
#'
#'
rt.set_theme <- function(theme = 'light',
                             primary = 'blue',
                             accent = 'pink',
                             warn = 'orange'){

  theme_set <- theme
  prime_set <- primary
  accent_set <- accent
  warn_set <- warn

  if(!exists('rt_colors')){
    rt.deps_colors()
  }

  var_lvl <- list(accent = c("A200","A100","A400","A700"),
                  primary = c('500','300','800','A100'),
                  warn = c('500','300','800','A100'))


  theme_var <- list(
    dark = c("#000000","#212121","#303030","#424242"),
    light =  c('#E0E0E0','#F5F5F5','#FAFAFA','#FFFFFF'))


  prime_stroke <-
    rt_colors[rt_colors$color_name == prime_set,]  %>%
    filter(google_colors == '500' |
             google_colors == '300' |
             google_colors == '800' |
             google_colors == 'A100') %>%
    select(hex) %>% mutate(pal_name = 'primary',
                           hue_lvl = c(500,300,800,'A100'),
                           color_name = prime_set) %>%
    select(color_name,
           selector = pal_name,
           level = hue_lvl,
           color = hex)

  warn_stroke <-
    rt_colors[rt_colors$color_name == warn_set,] %>%
    filter(google_colors == '500' |
             google_colors == '300' |
             google_colors == '800' |
             google_colors == 'A100') %>%
    select(hex) %>% mutate(pal_name = 'warn',
                           hue_lvl = c(500,300,800,'A100'),
                           color_name = warn_set) %>%
    select(color_name,
           selector = pal_name,
           level = hue_lvl,
           color = hex)

  accent_stroke <-
    rt_colors[rt_colors$color_name == accent_set,] %>%
    filter(google_colors == 'A200' |
             google_colors == 'A100' |
             google_colors == 'A400' |
             google_colors == 'A700') %>%
    select(hex) %>% mutate(pal_name = 'accent',
                           hue_lvl = c('A200','A100','A400','A700'),
                           color_name = accent_set) %>%
    select(color_name,
           selector = pal_name,
           level = hue_lvl,
           color = hex)

  pals <- rbind(prime_stroke,warn_stroke)

  pals <- rbind(pals,accent_stroke)

  var_base <- theme_var[[theme_set]] %>%
    chain.df('hex') %>%
    mutate(pal_name = 'theme_master',
           hue_lvl = 1:4,
           color_name = theme_set) %>%
    select(color_name,
           selector = pal_name,
           level = hue_lvl,
           color = hex)

  pal_out <- rbind(pals,var_base)

  fonts_pal <- function(theme_set = NULL){
    if(theme_set == 'light'){
      text_base <- '#000000'
      theme_scale <- c(0.87, 0.54, 0.38, 0.12)
    }else{
      text_base <- "#FFFFFF"
      theme_scale <- c(1, 0.70, 0.5, 0.12)
    }

    fonts_names <- c('primary','secondary','disabled|hint|icons','dividers')

    llply(1:4,function(i)
      data.frame(color_name = fonts_names[[i]],
                 selector = 'fonts',
                 level = i,
                 color = mtrl.hex_to_opace(text_base,theme_scale[[i]])
      )
    ) %>% rbind.pages
  }

  var_font <- fonts_pal(theme_set = theme_set)

  pal_out <- rbind(pal_out,var_font)

  rt.global <<- new.env()
  assign('theme',function(var_in = 'master', n = 1, font_class = NULL){

    the_vars <- c('primary','warn','accent','theme_master','fonts')
    var <- the_vars[grep(var_in,the_vars)]

    if(is.null(font_class)){
      selected <- (pal_out %>% filter(selector == var) %>% `$`(color))[[n]]
    }else {
      selected <- (pal_out %>% filter(grepl(font_class,color_name)) %>% `$`(color))
    }
    return(selected)
  },
  envir = rt.global
  )

  theme_sprint <- '<div style="background-color:%s;height:%s;width:%s;color:#FFF">%s</div>'
  assign('theme_preview',pal_out %>%
           mutate(html_out = sprintf(theme_sprint,color,"100%","100%",
                                     paste(selector,level,sep = " -- ")
                                     )
                  ),
         envir = rt.global)


}



rt.color_lev <- function(var = NULL){

  lv <- c("50","100","200","300","400",
          "500","600","700","800","900",
          "A100","A200","A400","A700")

  Fun <- stringdist::amatch

  lv[Fun(var,lv, maxDist = Inf)]
}

rt.color_pick <- function(color_pick = NULL,
                          pal_class = c('primary','accent'),
                          var_n = 50){

  color_pal <- mtrl_colors %>% filter(color_name == color_pick)


    color_filt <- switch(pal_class,
                         "primary" = color_pal %>%
                           filter(!grepl('A100|A200|A400|A700',google_colors)),
                         "accent" = color_pal %>% filter(grepl('A100|A200|A400|A700',google_colors))
    )


  color_filt


}


