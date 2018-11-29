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
        .msg(sprintf("%s cache directory set to:\n    ", pkgName, directory))
    invisible(directory)
}

getCache <- function() {
    pkgName <- getOption("pkgName")
    cache <- getOption(sprintf("%s_cache", pkgName),
        setCache(directory = rappdirs::user_cache_dir(pkgName))
    )
    BiocFileCache::BiocFileCache(cache)
}
