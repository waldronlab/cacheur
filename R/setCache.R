#' @importFrom BiocFileCache BiocFileCache bfcrpath bfcquery bfccache bfcupdate
#' bfcadd
NULL

#' @name cache-cache
#'
#' @title Helper functions for package caching
#'
#' @description Both setCache and getCache allow maintainers / developers to
#' specify cache locations for the package they wish to implement a caching
#' mechanism. This avoids the maintainer the use of BiocFileCache directly by
#' providing a simple option called `pkgName` which indicates the name of the
#' package being developed.
#'
#' @param directory location on disk intended for caching files
#' @param verbose logical (default FALSE) whether to report procedural steps
#' @param ask logical (default `interactive()``) whether to ask the user to
#' confirm the cache location
#'
#' @examples
#' ## options(pkgName = "cacheur")
#' setCache()
#'
#'
#' @return setCache: Modifies 'pkgName' and <pkg>_cache options and creates
#'   directory if not available
#' @export setCache
setCache <-
    function(directory = rappdirs::user_cache_dir(getOption("pkgName")),
        verbose = TRUE, ask = interactive())
{
    stopifnot(is.character(directory), identical(length(directory), 1L),
        !is.na(directory))

    pkgName <- getOption("pkgName")
    if (!dir.exists(directory)) {
        if (ask) {
            qtxt <- sprintf(
                "Create %s cache at \n    %s? [y/n]: ",
                pkgName, directory
            )
            answer <- .getAnswer(qtxt, allowed = c("y", "Y", "n", "N"))
            if ("n" == answer)
                stop(
                    sprintf("'%s_cache' directory not created. Use 'setCache'",
                        pkgName)
                )
        }
        dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    }
    options(structure(list(directory), .Names = sprintf("%s_cache", pkgName)))

    if (verbose)
        .message(.msg(
            sprintf("%s cache directory set to: %s", pkgName, directory)
        ))

    invisible(directory)
}

#' @rdname cache-cache
#' @return A BiocFileCache instance for the package specified in the 'pkgName'
#'   option
#' @export getCache
getCache <- function() {
    pkgName <- getOption("pkgName")
    cache <- getOption(sprintf("%s_cache", pkgName),
        setCache(directory = rappdirs::user_cache_dir(pkgName))
    )
    BiocFileCache::BiocFileCache(cache)
}
