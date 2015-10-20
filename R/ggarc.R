#' data.frame for ggplot2 arc line
#'
#' helper function for adding horizontal arc to ggplot2 object via geom_path()
#' modified from: http://stackoverflow.com/questions/6862742/
#'
#' @param xmin numeric value specifying minimum horizontal value for arc (default = 0)
#' @param xmax numeric value specifying maximum horizontal value for arc (default = 10)
#' @param ymin numeric value specifying minimum vertival value for arc (default = 0)
#' @param height numeric value specifying height of arc. Negative value can be
#'        specified for arc extending down (default = 10)
#' @param n_pt numeric value specifying number of points used to draw arc (default = 100)
#' 
#' @return
#' \code{data.frame} with 2 columns specifyin x and y coordinates of points of arc
#' to be passed to \code{ggplot2::geom_path()}
#' 
#' @export
#' @author Patrick Kimes
gg_arc <- function(xmin = 0, xmax = 10,
                       ymin = 0, height = 10, n_pt = 100) {
    x_c  <- (xmax + xmin) / 2
    x_r <- (xmax - xmin) / 2
    tt <- seq(0, pi, length.out = n_pt)
    
    xx <- x_c + x_r * cos(tt)
    yy <- ymin + height * sin(tt)
    return(data.frame(x = xx, y = yy))
}
