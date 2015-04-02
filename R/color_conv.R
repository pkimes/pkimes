#' RGB triple to HEX string
#'
#' helper function for converting matrix of RGB colors on (0, 255) scale
#' to HEX color strings
#'
#' @param rgb matrix with three columns with entries between (0, 255)
#'
#' @return
#' vector of HEX color strings
#' 
#' @export
#' @author Patrick Kimes
rgb2hex <- function(rgb) {
    if (is.matrix(rgb)) {
        apply(round(rgb), 1,
              function(x) { 
                  paste0("#", paste0(as.hexmode(x), collapse=""))
              })
    } else {
        paste0("#", paste0(as.hexmode(round(rgb)), collapse=""))
    }        
}



#' HEX string to RGB triple
#'
#' helper function for converting HEX colors to RGB colors
#'
#' @param hexcolors vector of HEX color strings
#'
#' @return
#' matrix of RGB colors with three columns
#' 
#' @export
#' @author Patrick Kimes
hex2rgb <- function(hexcolors) {
    t(col2rgb(hexcolors) / 255)
}
