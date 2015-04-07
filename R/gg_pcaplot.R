#' pca scores plot in ggplot2
#' 
#' @param x a matrix of data (n x p)
#' @param npc an integer number of principal components
#' @param labs labels for the different clusters
#' @param idiag a boolean whether to include  
#'
#' @return a \code{ggpairs} plot of the PC scores
#'
#' @import ggplot2
#' @export
#' @author Patrick Kimes
gg_pcaplot <- function(x, npc, labs = NULL, idiag = TRUE) {
    pca <- prcomp(x)
    if (is.null(labs)) {
        ggp <- ggpairs(data.frame(pca$x[, 1:npc]),
                       params=c(alpha=1, color="blue", fill="red"),
                       upper='blank', lower=list(continuous='points'))#,
                       ##diag=list(continuous='blank', discrete='blank'))#ifelse(idiag, 'density', 'blank')))
    } else {
        ggp <- ggpairs(data.frame(pca$x[, 1:npc], labs=as.factor(labs)),
                       params=c(alpha=1), color="labs", columns=1:npc,
                       upper='blank', lower=list(continuous='points'))#,
                       ##diag=list(continuous='blank', discrete='blank'))#ifelse(idiag, 'density', 'blank')))
    }
    for (ip in 1:npc^2) {
        ggp$plots[[ip]] <- paste(ggp$plots[[ip]], "+ theme_bw()")
        ggp$plots[[ip]] <- paste(ggp$plots[[ip]], "+ scale_color_brewer(palette='Set1')")
    }
    ##remove diagonal if not wanted
    if (!idiag) {
        for (ip in 1:npc) {
            ggp$plots[[(ip-1)*(npc+1)+1]] <- "ggally_blank()"
        }
    }
    ggp
}
