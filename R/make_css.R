# color scales--------
#
#' load the color keys into the space or build new key
#'
#'
#' Building preformatted color data frame keys for use across applications
#' a great resource for less scales \link{http://colourco.de/monochrome-dark/5/#8b1d1d}
#' for better understanding of pal choices and why they matter
#' \link{https://datavisualization.ch/inside/how-we-created-color-scales/}
#' for google style guidelines \link{https://www.google.com/design/spec/style/color.html#color-color-palette}
#'
#' \code{color_load} a function to load color indexes
#'
#' class opts are [highlight,shades,social,steps_2,steps_3]
#' sub_class opts are ["good_bad,muted,neon,blues,dark_gray,gray,greens,orange,pink,purple,red,teal,
#' yellow,facebook,google_analytics,
#' @export

color_load <- function(){
  look_in <- list.files(recursive = T,full.names = T,pattern = "color_scales")

  if(length(look_in) == 1){

    assign('color_df',readRDS(look_in),
           envir=.GlobalEnv)

  }else{
    drop_get('rterial/colors/color_scales.rds')

    file.copy('color_scales.rds','data/color_scales.rds')

    unlink('color_scales.rds')

    assign('color_df',readRDS('data/color_scales.rds'),
           envir=.GlobalEnv)
  }

}


color_pal_help <- function(filter_class = NULL,filter_choice = NULL){

  if(file.exists('color_scales.rds')){
    color_df <- readRDS('color_scales.rds')
  }else{
    drop_get('master/www/color_scales.rds',overwrite=T)
    color_df <- readRDS('color_scales.rds')
  }

  clr_key <- color_df %>%
    count(color_class,sub_class) %>%
    data.frame(stringsAsFactors=FALSE)

  colnames(clr_key)[[3]] <- 'steps'

  clr_key <-
    cbind(clr_key,hex = lapply(1:nrow(clr_key),function(i)
      paste0(color_df[color_df$color_class == clr_key$color_class[[i]] &
                        color_df$sub_class == clr_key$sub_class[[i]],'hex_val'],
             collapse = ",")
    ) %>% unlist)

  clr_key$V1 <- NULL

  if(!is.null(filter_class)){

    filter_out <- clr_key %>% filter(color_class == as.quoted(filter_class))

    if(is.null(filter_out)){
      filter_out <- clr_key[grep(filter_class,clr_key$color_class),]
    }
    if(!is.null(filter_choice)){
      filter_down <- filter_out %>%
        filter(sub_class == as.quoted(filter_choice)) %>%
        as.data.frame(stringsAsFactors=F) %>% select(hex) %>% as.character

      return(filter_down)
    }
  }
  if(is.null(filter_class) & is.null(filter_choice)){
    clr_list <- llply(unique(color_df$color_class),
                      function(i)
                        color_df[color_df$color_class==i,] %>% unique)

    names(clr_list) <- unique(color_df$color_class)
    return(clr_key)
  }
}


color_gen_shade <- function(base_color = NULL,steps = NULL,print_out = NULL){

  if(!is.null(steps)){
    n_calc <- as.numeric(steps)
  }else{
    n_calc <- 8
  }

  if(stri_detect_regex(base_color,"#")){
    color_in <- col2rgb(base_color) %>% t %>% adf
    col_red <- color_in %>% select(red)
    col_green <- color_in %>% select(green)
    col_blue <- color_in %>% select(blue)
  }else{
    color_in <-
      stri_split_regex(base_color,'[(]|,|[)]') %>% chain.df('rgbs') %>%
      slice(2:4) %>% unlist %>% as.character

    col_red <- color_in[[1]]
    col_green <- color_in[[2]]
    col_blue <- color_in[[3]]
  }

  n_steps <- round((1/steps)*(1:steps),2)


  col_tinted <- function(col_red,col_green,col_blue,n_steps){
    lapply(n_steps,function(i){
      r_set <- round(col_red + (255 - (col_red * (1-i))) * i)
      g_set <- round(col_green + (255 - (col_green * (1-i))) * i)
      b_set <- round(col_blue + (255 - (col_blue * (1-i))) * i)

      RR <- ifelse(r_set > 255,255,r_set)
      GG <- ifelse(g_set > 255,255,g_set)
      BB <- ifelse(b_set > 255,255,b_set)

      sprintf('rgb(%s,%s,%s)',RR,GG,BB)
    })

  }
  lapply(n_steps,
         function(i)
           paste0('rgb(',
                  round(col_red + (255 - (col_red * (1-i))) * i),
                  ",",
                  round(col_green + (255 - (col_green * (1-i))) * i),
                  ",",
                  round(col_blue + (255 - (col_blue * (1-i))) * i),
                  ')')) %>%
    unlist %>% chain.df('colors') %>%
    mutate(
      test =
        paste0('<div class="panel" style="background-color:',colors,
               '">TINTS<span style="float:right;color:#454545;margin-right:5%;">',
               colors,'</span></div>')
    )

  color_built <- rbind(col_scaled,col_tinted)

  if(!is.null(print_out)){
    color_show <- data.frame(scales = col_scaled$test,
                             tints = col_tinted$test,
                             stringsAsFactors = F)

    sample <-
      tags$div(class='container',
               tags$div(class='col-sm-6',
                        style="width:50%;margin-bottom:25px;",
                        color_show$scales %>% unlist %>% HTML),
               tags$div(class='col-sm-6',
                        style="width:50%;",
                        color_show$tints %>% unlist %>% HTML))
    return(html_print(sample))
  }else{
    return(color_built)

  }
}

rgb_to_hsl <- function(r,g,b){
  r1 = r / 255
  g1 = g / 255
  b1 = b / 255

  maxColor = max(r1,g1,b1)
  minColor = min(r1,g1,b1)

  #calculate the luminance
  lume = (maxColor + minColor) / 2
  sat = 0
  hue = 0
  if(maxColor != minColor){
    #calculate saturation
    if(lume < 0.5){
      sat = (maxColor - minColor) / (maxColor + minColor)
    }else{
      sat = (maxColor - minColor) / (2.0 - maxColor - minColor)
    }
    #calculate hue
    if(r1 == maxColor){
      hue <- (g1-b1) / (maxColor - minColor)
    }else if(g1 == maxColor){
      hue <- 2.0 + (b1 - r1) / (maxColor - minColor)
    }else{
      hue = 4.0 + (r1 - g1) / (maxColor - minColor)
    }
  }

  lume_out <- lume * 100
  sat_out <-  sat * 100
  hue_out <- hue * 60

  if(hue_out < 0){
    hue_out <- sum(hue,360)
  }

  hsl_out <- c(H = hue_out,S = sat_out,L = lume_out)

  return(hsl_out)
}

rhsl <- function(r,g,b){


  var_R = (r / 255)
  var_G = (g / 255)
  var_B = (b / 255)

  var_Min = min(var_R,var_G,var_B)
  var_Max = max(var_R,var_G,var_B)
  del_Max = var_Max - var_Min

  L = (var_Max + var_Min) / 2

  if (del_Max == 0){
    H = 0
    S = 0
  }else{
    if (L < 0.5){
      S = del_Max / (var_Max + var_Min)
    }else{
      S = del_Max / (2 - var_Max - var_Min)
    }
  }
  del_R = (((var_Max - var_R) / 6) + (del_Max / 2)) / del_Max
  del_G = (((var_Max - var_G) / 6) + (del_Max / 2)) / del_Max
  del_B = (((var_Max - var_B) / 6) + (del_Max / 2)) / del_Max

  if(var_R == var_Max){
    H = del_B - del_G
  }else {
    if (var_G == var_Max){
      H = ( 1 / 3 ) + del_R - del_B
    }else{
      if (var_B == var_Max){
        H = ( 2 / 3 ) + del_G - del_R
      }
    }
  }
  if(H < 0){
    H <- H + 1
  }
  if(H > 1){
    H <- H - 1
  }

  return(c(H = H,S = S, L = L))
}



Hue_2_RGB <- function(v1,v2,vH){

  if(vH < 0){
    vH <- vH + 1
  }
  if(vH > 1){
    vH <- vH - 1
  }
  if((6 * vH) < 1){
    return(v1 + (v2 - v1) * 6 * vH)
  }
  if((2 * vH) < 1){
    return(v2)
  }
  if((3 * vH) < 2){
    return(v1 + (v2 - v1) * ((2 / 3) - vH) * 6)
  }
  return (v1)
}

hsl_to_rgb <- function(h,s,l){

  if(s == 0){
    R = l * 255
    G = l * 255
    B = l * 255
  }else {
    if(l < 0.5){
      var_2 = l * (1 + s)
    }else {
      var_2 = (l + s) - (s * l)
      var_1 = 2 * l - var_2

      R = 255 * Hue_2_RGB(var_1,var_2,h + (1/3))
      G = 255 * Hue_2_RGB(var_1,var_2,h)
      B = 255 * Hue_2_RGB(var_1,var_2,h - (1/3))
    }
  }
  return(c(R = R,G = G,B = B))
}


rgb_to_hsv <- function(r,g,b){

  #RGB from 0 to 255
  var_R = (r / 255)
  var_G = (g / 255)
  var_B = (b / 255)

  #Min. value of RGB
  var_Min = min(var_R, var_G, var_B)
  #Max. value of RGB
  var_Max = max(var_R, var_G, var_B)
  #Delta RGB value
  del_Max = var_Max - var_Min

  V = var_Max

  #allotment for gray with no luminance
  if (del_Max == 0){
    #HSV results from 0 to 1
    H = 0
    S = 0
    #chromatic elements
  }else {
    S = del_Max / var_Max
    del_R = (((var_Max - var_R) / 6) + (del_Max / 2)) / del_Max
    del_G = (((var_Max - var_G) / 6) + (del_Max / 2)) / del_Max
    del_B = (((var_Max - var_B) / 6) + (del_Max / 2)) / del_Max

    if(var_R == var_Max){
      H = del_B - del_G
    }else {
      if(var_G == var_Max){
        H = (1 / 3) + del_R - del_B
      }else {
        if(var_B == var_Max){
          H = (2 / 3) + del_G - del_R
        }
      }
    }
  }
  return(c(hue = ifelse(H<0,H+1,H),sat = S,val = V))
}





#'
#'
#'> rgb_to_hex("rgba(255,23,200,0.5)")
#'
#' [1] "#FF17C8"
#'
#'> rgb_to_hex("rgba(255,23,200)")
#'
#' [1] "#FF17C8"
#'
#'
rgb_to_hex <- function(...,max_value = 255){

  rgb_in <- extract_rgb(...)

  rgb(rgb_in$red, rgb_in$green, rgb_in$blue,maxColorValue = max_value)

}

#'> extract_rgb("rgb(255,23,200)")
#'  red green blue
#'1 255    23  200
#'
#'
#'> extract_rgb("rgba(255,23,200,0.4)")
#'  red green blue alpha
#'1 255    23  200   0.4
#'
#'
extract_rgb <- function(...){
  txt.extract <- stri_extract_all_regex
  set.names <- c('red','green','blue','alpha')
  a.frame <- txt.extract(...,"[0]+[.]+[1-9]|[0-9]{1,3}",simplify = T) %>% adf

  if(length(a.frame) == 4){
    colnames(a.frame) <- set.names
  }else{
    colnames(a.frame) <- set.names[1:3]
  }
  a.frame
}






#'> rt.color_alpha("rgba(0,0,0,.25)",-4)
#'[1] "rbga(0, 0, 0, 0)"
#'> rt.color_alpha("rgba(0,0,0,.25)",4)
#'[1] "rbga(0, 0, 0, 0.21)"
#'> rt.color_alpha("#FFFFFF",4)
#'[1] "rbga(255, 255, 255, 0.04)"
#'> rt.color_alpha("#FFFFFF",99)
#'[1] "rbga(255, 255, 255, 0.99)"
#'> rt.color_alpha("#FFFFFF",150)
#'[1] "rbga(255, 255, 255, 1)"
#'> rt.color_alpha("#FFFFFF",-55)
#'[1] "rbga(255, 255, 255, 0.45)"
#'> rt.color_alpha("rgb(0,0,0)",-55)
#'[1] "rbga(0, 0, 0, 0.45)"
#'> rt.color_alpha("rgb(0,0,0)",55)
#'[1] "rbga(0, 0, 0, 0.55)"
#'
#'
#'
#'
#'
#'



rt.color_alpha <- function(color_in = '#000000',alpha = 80){

  colr_ready <- switch(substring(color_in,1,1),
                       "#" = toString(as.character(col2rgb(color_in))),
                       "r" = toString(extract_rgb(color_in))
                       )

  # If input is an alpha rgba already need to remove the alpha
  if(stri_count_regex(colr_ready,",") > 2){
    var_in <- extract_rgb(color_in)

    colr_ready <- toString(as.character(var_in[1:3]))


    alpha1 <- as.numeric(var_in[[4]])

    alpha2 <- rt.alpha_prep(alpha) * 100

    if(alpha2 > alpha1){
      al.adj <- 0
    }else{
      al.adj <- (alpha1 - alpha2) / 100
    }

  }else{
    al.adj <- rt.alpha_prep(alpha)
  }


  out <- (sprintf("rbga(%s, %s)", colr_ready, al.adj))

  return(out)
}




rt.alpha_prep <- function(alpha = 80){
  if(alpha > 100){
    alpha <- 1
  }
  # Assume this is if we want to adjust the opacity...in terms of:
  # "take whatever a color is and make it have an opacity Nth lower than current"
  if(alpha < 0){
    al.swap <- alpha * -1
    if(al.swap > 100){
      alpha <- 1
    }else{
      alpha <- 100 - al.swap
    }
  }

  if(alpha == 1){
    al.adj <- alpha
  }else{
    al.adj <- alpha / 100
  }
  al.adj
}







rt.css_inputs <- function(theme_base = c('light','dark'),
                          primary = NULL,accent = NULL,
                          theme_env = NULL){

  light <- list(
    on = rt.global$theme(var_in = "accent",n = 1),
    off = rt.color_alpha("#000000",54),
    disabled = rt.color_alpha("#000000",26)
    )

  dark <- list(
    on = rt.global$theme(var_in="accent",n=1),
    off = rt.color_alpha("#FFFFFF",70),
    disabled = rt.color_alpha("#FFFFFF",30)
  )

  thumb <- list(
    on = list(radio = rt.global$theme(var_in="accent",n=1),
              track =  rt.color_alpha(rt.global$theme(var_in="accent",n=1),50)
              ),
    off =  list(radio = rt.global$theme(var_in="accent",n=1),
                track =  rt.color_alpha(rt.global$theme(var_in="accent",n=1),50)
    ))




}





#' Create a css block-rule
#'
#'  \code{mtrl.css_block}
#'
#'   @examples
#'   mtrl.css_block(css_selector = '.md-input-container','    display: inline-block;
#'    position: relative;
#'    padding: 2px;
#'    margin: 18px 0;
#'    vertical-align: middle;',to_file = 'test.css')
#'> mtrl.css_block(css_selector = '.md-input-container','    display: inline-block;
# +     position: relative;
# +     padding: 2px;
# +     margin: 18px 0;
# +     vertical-align: middle;',for_head = T)
# <style>
# .md-input-container{
# display: inline-block;
# position: relative;
# padding: 2px;
# margin: 18px 0;
# vertical-align: middle;
# }
# </style>
#'
#'
#'
mtrl.css_block <- function(css_selector = NULL,...,
                           for_head = F,to_file = NULL){


  css_inner <- paste0(lapply(strsplit(...,'\n')[[1]],function(i)
    strwrap(stri_trim_both(i),indent = 3)),collapse = "\n")

  new_block <- paste0(css_selector,"{\n",css_inner,"\n}") %>% pure.html

  if(for_head){
    sprintf('<style>\n %s \n </style>',new_block)%>%pure.html

  }else if(!is.null(to_file)){

    if(file.exists(to_file)){
      write(new_block,'try.css')

      file.append(to_file,'try.css')
    }else{
      write(new_block,to_file)
    }

  }
}
