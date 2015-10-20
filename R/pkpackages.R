#' @title Create R Package for Patrick
#'
#' This function is modeled after Hadley's \code{devtools::create} and other similar functions
#' for defining and producing consistent packages using templates. The function is written to several
#' different types of R packages/directories specified in the Details.
#'
#' @param path string specifying location and name of package
#' @param type string specifying type of package to create. Must be one of a set of pre-defined
#'        types else function returns error (default = "analysis")
#' 
#' @details
#' Currently supported valid types include:
#' \itemize{
#' \item "method": for a new method/algorithm, closest to standard R package
#' \item "sim": for method evaluation using simulations
#' \item "data": for method evaluation using real data examples
#' \item "analysis": for analyses run independent of method testing
#' }
#' 
#' @author Patrick Kimes
#' @export

pkpackages <- function(path, type = "method") {

    ## set of supported package types
    valid_types <- c("method", "sim", "data", "analysis")

    ## verify that specified package type is valid
    if (!(type %in% valid_types)) {
        stop(paste0("type must be one of valid types:",
                    paste(valid_types, collapse=", ")))
    }
    
    ## verify that package doesn't already exist in location
    if (file.exists(path)) {
        stop("Directory already exists", call.=FALSE)
    }
    
    dir.create(path, recursive=TRUE)

    write_description(path, type)
    write_namespace(path)
    write_license(path)

    dir.create(file.path(path, "R"))
    dir.create(file.path(path, "man"))
    dir.create(file.path(path, "inst"))
    dir.create(file.path(path, "data"))
    
    ## produce type specific files
    if (type == "method") {
        dir.create(file.path(path, "vignettes"))
        dir.create(file.path(path, "tests"))
        dir.create(file.path(path, "tests", "testthat"))
        file.copy(from=file.path(system.file(package="pkimes"), "testthat.R"),
                  to=file.path(path, "tests", "testthat.R"))
        dir.create(file.path(path, "src"))

    } else if (type == "sim") {
        dir.create(file.path(path, "analysis"))

    } else if (type == "data") {
        dir.create(file.path(path, "analysis"))

    } else if (type == "analysis") {
        dir.create(file.path(path, "analysis"))
    }
}


write_description <- function(path, type) {
    desc <- list(Package = basename(path),
                 Title = "What the Package Does (one line, title case)", 
                 Version = "0.0.0.9000",
                 `Authors@R` = "Patrick K. Kimes", 
                 Description = "What the package does (one paragraph)", 
                 Depends = paste0("R (>= ", as.character(getRversion()), ")"),
                 License = "MIT + file LICENSE",
                 Suggests = "",
                 LazyData = "true")

    if (type == "method") {
        desc <- modifyList(desc, list(Suggests = "testthat"))
    }
    
    desc <- lapply(desc, function(x) paste(x, collapse = ", "))
    text <- paste0(names(desc), ": ", desc, collapse = "\n")

    if (substr(text, nchar(text), 1) != "\n") {
        text <- paste0(text, "\n")
    }

    cat(text, file = file.path(path, "DESCRIPTION"))
}


write_namespace <- function(path) {
    cat('exportPattern("^[^\\\\.]")\n',
        file = file.path(path, "NAMESPACE"))
}


write_license <- function(path) {
    cat(paste0("YEAR: ", format(Sys.Date(), "%Y"), "\n",
               "COPYRIGHT HOLDER: Patrick K. Kimes"),
        file = file.path(path, "LICENSE"))
}

