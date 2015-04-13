#' plot nice heatmap and dendrograms using ggplot2
#'
#' @param data a numeric matrix of expression, genes x samples 
#' @param distance a character string for \code{dist}, or a function which returns
#'        a \code{dist} object
#' @param linkage a character string for \code{hclust} to use for clustering
#' @param labframe a data.frame of column labels (default = NULL)
#' @param rlabframe a data.frame of row labels (default = NULL)
#' @param row_sort a logical whether rows should be reordered based on
#'        clustering (default = TRUE)
#' @param col_sort a logical whether columns should be reordered based on
#'        clustering (default = TRUE)
#' @param colbrew a vector of strings same length as \code{labframe} specifying
#'        the colors to be used for column labels, if length is 1 same palette is
#'        repeated, see Details regarding valid strings (default = "Set1")
#' @param rcolbrew a vector of strings same length as \code{rlabframe} specifying
#'        the colors to be used for row labels, if length is 1 same palette is
#'        repeated, see Details regarding valid strings (default = "Set1")
#' @param rtext a vector of strings with text annotations for rows of heatmap
#'        (default = NULL)
#' 
#' @details
#' Valid choices of color palettes are listed in \code{RColorBrewer::brewer.pal.info}
#' 
#' @return a \code{ggplot} object
#'
#' @import RColorBrewer ggplot2
#' @export
#' @author Patrick Kimes
gg_heatmap <- function(data, distance, linkage, row_sort = TRUE, col_sort = TRUE,
                       labmat = NULL, rlabmat = NULL, colbrew = "Set1", rcolbrew = "Set1",
                       rtext = NULL) {

    if (length(colbrew) == 1) {
        colbrew <- rep(colbrew, length(labmat))
    }
    if (length(rcolbrew) == 1) {
        rcolbrew <- rep(rcolbrew, length(rlabmat))
    }

    .getpal <- function(x) { suppressWarnings(c("#000000", brewer.pal(100, x))) }
    setlist1 <- lapply(colbrew, .getpal)
    setlist2 <- lapply(rcolbrew, .getpal)
    
    p <- nrow(data)
    n <- ncol(data)

    if (!is.null(rtext) & length(rtext) != p) {
        rtext <- NULL
        warning("not using rtext param since length not equal to p")
    }
    
    lab_width <- p / 25
    lab_width2 <- n / 25
    
    if (is.character(distance)) {
        dmat1 <- dist(data, distance)
        dmat2 <- dist(t(data), distance)
    } else { 
        dmat1 <- distance(t(data)) ## rows, genes
        dmat2 <- distance(data) ## columns, samples
    }
    
    hc1 <- hclust(dmat1, method=linkage)
    hc2 <- hclust(dmat2, method=linkage)

    if (col_sort) { data <- data[, hc2$order] }
    if (row_sort) { data <- data[hc1$order, ] }
    gg_data <- melt(data)

    ## basic plot structure
    vmax <- max(abs(gg_data$value))*1.01
    gg_p <- ggplot(gg_data, aes(x=Var2, y=Var1, fill=value)) +
        geom_tile() + theme_bw() +
            theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) +
                scale_fill_gradientn("log2(Expr)",
                    colours=c(rep("#ca0020",2), "#f7f7f7", rep("#0571b0", 2)),
                    rescaler = function(x, ...) x, oob=identity,
                    values=c(-vmax, -vmax/2, 0, vmax/2, vmax))
    ## low="#b2182b", high="#2166ac"
    
    ## add dendrograms to plot
    gg_hc1 <- dendro_data(hc1, hang=.5)
    gg_hc2 <- dendro_data(hc2, hang=.5)

    if (col_sort) {
        gg_p <- gg_p +
            annotate("segment",
                     x = gg_hc2$segments$x,
                     xend = gg_hc2$segments$xend,
                     y = (p+.5) + lab_width/2 + p/6*gg_hc2$segments$y,
                     yend = (p+.5) + lab_width/2 + p/6*gg_hc2$segments$yend,
                     size = .3)
    }
    if (row_sort) {
        gg_p <- gg_p +
            annotate("segment",
                     y = gg_hc1$segments$x,
                     yend = gg_hc1$segments$xend,
                     x = (n+.5) + lab_width2/2 + n/6*gg_hc1$segments$y,
                     xend = (n+.5) + lab_wdith2/2 + n/6*gg_hc1$segments$yend,
                     size = .3)
    }

    ## clean things up
    gg_p <- gg_p + 
        theme(panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              panel.border=element_blank()) +
        xlab("Samples") + ylab("Genes") +
            scale_x_discrete(breaks=NULL) +
                scale_y_continuous(breaks=NULL, expand = c(0,0))

    ## extend range of plot
    if (col_sort) {
        gg_p <- gg_p +
            geom_hline(yintercept = (p+1)*(1.01) + p/5.7*max(gg_hc2$segments$y), color="white")
    }
    if (row_sort) {
        gg_p <- gg_p +
            geom_vline(xintercept = (n+1)*(1.01) + n/5.7*max(gg_hc1$segments$y), color="white")
    }
    if (!is.null(rtext)) {
        gg_p <- gg_p +
            geom_vline(xintercept = -(length(rlabmat)+5)*lab_width2, color="white")
    }

    ## gg_p <- gg_p + geom_hline(yintercept = 0)
    ## gg_p <- gg_p + geom_hline(yintercept = 1)
    
    if (!is.null(labmat)) {
        if (col_sort) { iorder <- hc2$order } else { iorder <- 1:n }
        for (i in 1:length(labmat)) {
            gg_p <- gg_p +
                annotate("rect",
                         xmin = (1:n) - .5,
                         xmax = (1:n) + .5,
                         ymin = rep(.5 - (i-.5)*lab_width, n),
                         ymax = rep(.5 - (i+.5)*lab_width, n), 
                         fill = setlist1[[i]][1 + labmat[iorder, i]],
                         alpha=2/3) #+
                ## annotate("text", x=0, y=-1+lab_width/2-i*lab_width,
                ##          hjust=1, label=names(labmat)[i])
        }
    }

    if (!is.null(rlabmat)) {
        if (row_sort) { iorder <- hc1$order } else { iorder <- 1:p }
        for (i in 1:length(rlabmat)) {
            gg_p <- gg_p +
                annotate("rect",
                         ymin = (1:p) - .5,
                         ymax = (1:p) + .5,
                         xmin = rep(.5 - (i+.5)*lab_width2, p),
                         xmax = rep(.5 - (i-.5)*lab_width2, p), 
                         fill = setlist2[[i]][1 + rlabmat[iorder, i]],
                         alpha=2/3)
        }
    }

    if (!is.null(rtext)) {
        if (row_sort) { iorder <- hc1$order } else { iorder <- 1:p }
        gg_p <- gg_p +
            annotate("text",
                     x = rep(-length(rlabmat)*lab_width2, p),
                     y = 1:p, size = 3, hjust = 1, 
                     label = rtext[iorder])
    }
        
    gg_p
}

