.getPkgName <- function() {
    txt <- "What package do you want to set up local caching for? "
    pkgName <- readline(prompt = .msg(fmt = txt))
    stopifnot(is.character(pkgName))
    options(pkgName = pkgName)
}

.onLoad <- function(libname, pkgname)
{
    if (!interactive())
        return()

    if (is.null(getOption("pkgName")))
        .getPkgName()
}
