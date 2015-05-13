#' multiple plot function for ggplot2
#'
#' Function is reporduced from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#'
#' @param ... sequence of \code{ggplot} objects
#' @param plotlist list of \code{ggplot} objects if \code{...} not passed (default = NULL)
#' @param cols numeric value specifying number of columns in layout (default = 1)
#' @param layout matrix specifying the layout, if present \code{cols} is
#'        ignored (default = NULL)
#'  
#' @details
#' If the layout is something like matrix(c(1, 2, 3, 3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#'
#' @import grid
#' @export
#' @author Winston Chang (http://www.cookbook-r.com/)
gg_multi <- function(..., plotlist = NULL, cols = 1, layout = NULL) {
    plots <- c(list(...), plotlist)
    numPlots <- length(plots)

    ## If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots == 1) {
        print(plots[[1]])
    } else {
        ## Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        ## Make each plot, in the correct location
        for (i in 1:numPlots) {
            ## Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                  layout.pos.col = matchidx$col))
        }
    }
}
