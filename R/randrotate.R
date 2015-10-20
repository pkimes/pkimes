#' @title Random rotation matrix
#'
#' Function for generating a random rotation matrix in R^p
#' using the QR decomposition of random normal distribution.
#'
#' @param p dimension of desired random rotation matrix (default = 2)
#'
#' @return
#' p x p random rotation matrix
#' 
#' @examples
#' \dontrun{
#' rotated_x <- tcrossprod(x, randrotate(ncol(x))) #nxp matrix
#' }
#' 
#' @author Patrick Kimes
#' @export

randrotate <- function(p = 2) {
    R <- matrix(rnorm(p*p), nrow=p, ncol=p)
    decomp <- qr(R, LAPACK = TRUE)
    R <- qr.Q(decomp) %*% diag(sign(diag(qr.R(decomp))))
    R
}
